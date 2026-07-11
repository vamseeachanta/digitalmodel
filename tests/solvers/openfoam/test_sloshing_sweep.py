#!/usr/bin/env python3
"""
ABOUTME: Tests for dm#1528 slice 3 - natural-period calculation and deterministic
sweep generation (fill fraction x conduit capacity x T_roll/T_natural). Oracles
are computed independently of the code under test (analytic tanh dispersion,
plain arithmetic) so the tests remain trustworthy truth sources.
"""

import math

import pytest

from digitalmodel.solvers.openfoam.sloshing_sweep import (
    GRAVITY,
    NaturalPeriodResult,
    SweepCase,
    SweepConfig,
    SweepManifest,
    first_sloshing_natural_period,
    generate_sweep,
    period_ratio,
)


# ---------------------------------------------------------------------------
# Independent analytic oracle for the first sloshing natural period
# ---------------------------------------------------------------------------


def _oracle_natural_period(length, fill_depth, mode=1, gravity=GRAVITY):
    """Linear-potential tanh dispersion, computed from scratch (not the SUT)."""
    k = mode * math.pi / length
    omega = math.sqrt(gravity * k * math.tanh(k * fill_depth))
    return 2.0 * math.pi / omega


# ============================================================================
# Natural period
# ============================================================================


class TestNaturalPeriod:
    def test_matches_analytic_oracle(self):
        length, height, fill = 20.0, 10.0, 0.5
        res = first_sloshing_natural_period(length, height, fill)
        expected = _oracle_natural_period(length, fill * height)
        assert res.natural_period_s == pytest.approx(expected, rel=1e-12)

    def test_frequency_is_inverse_of_period(self):
        res = first_sloshing_natural_period(20.0, 10.0, 0.4)
        assert res.natural_frequency_hz == pytest.approx(
            1.0 / res.natural_period_s, rel=1e-12
        )

    def test_fill_depth_derived_from_fraction(self):
        res = first_sloshing_natural_period(20.0, 10.0, 0.25)
        assert res.fill_depth == pytest.approx(2.5, rel=1e-12)

    def test_provenance_retained(self):
        res = first_sloshing_natural_period(20.0, 10.0, 0.5)
        assert isinstance(res, NaturalPeriodResult)
        assert res.approximation  # non-empty approximation label
        assert "1528" in res.provenance
        assert "tanh" in res.provenance.lower()

    def test_higher_fill_gives_shorter_period(self):
        # tanh(kh) increases with h -> omega increases -> period decreases.
        low = first_sloshing_natural_period(20.0, 10.0, 0.2).natural_period_s
        high = first_sloshing_natural_period(20.0, 10.0, 0.8).natural_period_s
        assert high < low

    @pytest.mark.parametrize("fill", [0.0, 1.0, 1.5, -0.1])
    def test_rejects_invalid_fill_fraction(self, fill):
        with pytest.raises(ValueError, match="fill_fraction"):
            first_sloshing_natural_period(20.0, 10.0, fill)

    def test_rejects_nonpositive_geometry(self):
        with pytest.raises(ValueError):
            first_sloshing_natural_period(0.0, 10.0, 0.5)
        with pytest.raises(ValueError):
            first_sloshing_natural_period(20.0, 0.0, 0.5)


# ============================================================================
# Period ratio
# ============================================================================


class TestPeriodRatio:
    def test_basic_ratio(self):
        assert period_ratio(12.0, 6.0) == pytest.approx(2.0)

    def test_resonance_unity(self):
        assert period_ratio(6.0, 6.0) == pytest.approx(1.0)

    def test_rejects_nonpositive_natural_period(self):
        with pytest.raises(ValueError):
            period_ratio(6.0, 0.0)

    def test_rejects_nonpositive_roll_period(self):
        with pytest.raises(ValueError):
            period_ratio(0.0, 6.0)


# ============================================================================
# Sweep generation
# ============================================================================


def _default_config():
    return SweepConfig(
        length=20.0,
        tank_height=10.0,
        study_name="ref",
    )


class TestSweepGeneration:
    def test_default_fill_matrix_covers_required_fills(self):
        cfg = _default_config()
        fills = sorted({c.fill_fraction for c in generate_sweep(cfg).cases})
        for required in (0.10, 0.25, 0.40, 0.50, 0.65, 0.80):
            assert required in fills

    def test_case_count_is_full_factorial(self):
        cfg = _default_config()
        man = generate_sweep(cfg)
        n_fill = len(cfg.fill_fractions)
        n_cap = len(cfg.conduit_capacities)
        n_ratio = len(cfg.effective_period_ratios())
        assert len(man.cases) == n_fill * n_cap * n_ratio

    def test_roll_period_consistent_with_ratio_and_natural_period(self):
        cfg = _default_config()
        for case in generate_sweep(cfg).cases:
            assert case.roll_period_s == pytest.approx(
                case.period_ratio * case.natural_period_s, rel=1e-12
            )

    def test_natural_period_matches_oracle_per_case(self):
        cfg = _default_config()
        for case in generate_sweep(cfg).cases:
            expected = _oracle_natural_period(cfg.length, case.fill_depth)
            assert case.natural_period_s == pytest.approx(expected, rel=1e-12)

    def test_refinement_adds_points_near_resonance(self):
        cfg = _default_config()
        ratios = cfg.effective_period_ratios()
        # There must be strictly more ratios than the base set (refinement added).
        assert len(ratios) > len(cfg.base_period_ratios)
        # At least three distinct ratios lie strictly inside the resonance band.
        band = [
            r for r in ratios
            if abs(r - cfg.resonance_center) <= cfg.resonance_band
        ]
        assert len(band) >= 3

    def test_near_resonance_flag_matches_band(self):
        cfg = _default_config()
        for case in generate_sweep(cfg).cases:
            expected = abs(case.period_ratio - cfg.resonance_center) <= cfg.resonance_band
            assert case.near_resonance == expected

    def test_control_no_flow_case_present(self):
        cfg = _default_config()
        caps = {c.conduit_capacity for c in generate_sweep(cfg).cases}
        assert 0.0 in caps  # no-flow control coverage

    def test_case_ids_unique(self):
        man = generate_sweep(_default_config())
        ids = [c.case_id for c in man.cases]
        assert len(ids) == len(set(ids))

    def test_rejects_bad_fill_in_config(self):
        with pytest.raises(ValueError):
            generate_sweep(SweepConfig(length=20.0, tank_height=10.0,
                                       fill_fractions=(0.0,)))

    def test_rejects_negative_conduit_capacity(self):
        with pytest.raises(ValueError):
            generate_sweep(SweepConfig(length=20.0, tank_height=10.0,
                                       conduit_capacities=(-0.1,)))

    def test_rejects_nonpositive_ratio(self):
        with pytest.raises(ValueError):
            generate_sweep(SweepConfig(length=20.0, tank_height=10.0,
                                       base_period_ratios=(0.0,)))


# ============================================================================
# Determinism
# ============================================================================


class TestDeterminism:
    def test_regeneration_is_bit_identical(self):
        cfg = _default_config()
        a = generate_sweep(cfg)
        b = generate_sweep(cfg)
        assert [c.case_id for c in a.cases] == [c.case_id for c in b.cases]
        assert a.content_hash == b.content_hash

    def test_case_id_stable_hash_format(self):
        man = generate_sweep(_default_config())
        cid = man.cases[0].case_id
        assert cid.startswith("ref-")
        assert len(cid.split("-")[-1]) == 12

    def test_input_change_changes_case_id(self):
        base = generate_sweep(SweepConfig(length=20.0, tank_height=10.0,
                                          study_name="ref"))
        moved = generate_sweep(SweepConfig(length=21.0, tank_height=10.0,
                                           study_name="ref"))
        assert base.cases[0].case_id != moved.cases[0].case_id

    def test_manifest_is_frozen(self):
        man = generate_sweep(_default_config())
        assert isinstance(man, SweepManifest)
        with pytest.raises(Exception):
            man.content_hash = "x"  # frozen dataclass

    def test_deterministic_snapshot(self):
        """Frozen snapshot of a tiny config: exact IDs must never drift."""
        cfg = SweepConfig(
            length=20.0,
            tank_height=10.0,
            study_name="snap",
            fill_fractions=(0.25, 0.50),
            conduit_capacities=(0.0, 0.05),
            base_period_ratios=(0.8, 1.0, 1.25),
            resonance_center=1.0,
            resonance_band=0.15,
            resonance_points=3,
        )
        man = generate_sweep(cfg)
        # 2 fills x 2 caps x effective ratios
        rows = [
            (c.case_id, c.fill_fraction, c.conduit_capacity,
             round(c.period_ratio, 6))
            for c in man.cases
        ]
        assert rows == SNAPSHOT_ROWS
        assert man.content_hash == SNAPSHOT_CONTENT_HASH


# Frozen after first green run (deterministic algorithm defines the truth).
# Regenerate ONLY on an intentional ID-algorithm change; drift here = bug.
SNAPSHOT_ROWS = [
    ('snap-6eee2f1988fe', 0.25, 0.0, 0.8),
    ('snap-9ed81e7b0e77', 0.25, 0.0, 0.85),
    ('snap-05b0e2f508dc', 0.25, 0.0, 1.0),
    ('snap-569eff5336b1', 0.25, 0.0, 1.15),
    ('snap-9321d1018f93', 0.25, 0.0, 1.25),
    ('snap-2c7968381d03', 0.25, 0.05, 0.8),
    ('snap-f73bdf5e9ddc', 0.25, 0.05, 0.85),
    ('snap-eb5abf43f3f5', 0.25, 0.05, 1.0),
    ('snap-5a4d3cc6044a', 0.25, 0.05, 1.15),
    ('snap-e369dd69a01c', 0.25, 0.05, 1.25),
    ('snap-63ded130401d', 0.5, 0.0, 0.8),
    ('snap-cebf79634685', 0.5, 0.0, 0.85),
    ('snap-e73b10c9475c', 0.5, 0.0, 1.0),
    ('snap-600d66aebbd2', 0.5, 0.0, 1.15),
    ('snap-f82ca54717fc', 0.5, 0.0, 1.25),
    ('snap-ad76d9fd597a', 0.5, 0.05, 0.8),
    ('snap-aaad8952f8d0', 0.5, 0.05, 0.85),
    ('snap-eabb66f7c5de', 0.5, 0.05, 1.0),
    ('snap-7a208c23bc52', 0.5, 0.05, 1.15),
    ('snap-10672c5d73c9', 0.5, 0.05, 1.25),
]
SNAPSHOT_CONTENT_HASH = "da68b2a708b5b941"
