#!/usr/bin/env python3
"""
ABOUTME: Tests for the reduced-order sloshing->roll coupling model (#643, ACMA
B1546). Covers the dm#641 manifest loader (JSON/CSV/dict), omega+fill
interpolation (exact at swept points, bounded between), added-inertia/damping
extraction from in_phase/quad coeffs, the time-domain moment feed
(amplitude/phase for a synthetic single-frequency roll), the anti-roll
fill-tuning helper, and the one-way-coupling escalation-ratio check.

No OpenFOAM installation or CFD data is required -- a synthetic 3x3
{fill x drive frequency} manifest fixture stands in for the sweep output.
"""

import json
import math

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.sloshing_coupling import (
    CouplingStrengthReport,
    MomentCoefficients,
    SloshingCase,
    SloshingCouplingModel,
    TuningReport,
)


# ============================================================================
# Synthetic 3x3 manifest fixture
# ============================================================================
#
# Fills [0.25, 0.50, 0.75] x drive periods [14, 18, 22] s (roll-scale, bracketing
# an ~18 s roll natural period). quad_coeff (anti-roll damping) peaks at fill=0.50
# near the 18 s period, so the tuning helper must select fill 0.50.

FILLS = [0.25, 0.50, 0.75]
PERIODS = [14.0, 18.0, 22.0]

# quad_coeff [N.m/(rad/s)] indexed [fill][period]
QUAD = {
    0.25: {14.0: 1.0, 18.0: 2.0, 22.0: 1.5},
    0.50: {14.0: 2.0, 18.0: 5.0, 22.0: 3.0},   # max damping at 18 s
    0.75: {14.0: 1.5, 18.0: 3.0, 22.0: 2.0},
}
# in_phase_coeff [N.m/rad] indexed [fill][period]
INPHASE = {
    0.25: {14.0: 10.0, 18.0: 20.0, 22.0: 15.0},
    0.50: {14.0: 12.0, 18.0: 25.0, 22.0: 18.0},
    0.75: {14.0: 14.0, 18.0: 30.0, 22.0: 22.0},
}
ROLL_AMP_DEG = 4.0


def _rows():
    rows = []
    for fl in FILLS:
        for T in PERIODS:
            rows.append(
                {
                    "fill_level": fl,
                    "drive_period": T,
                    "drive_freq_hz": 1.0 / T,
                    "roll_amplitude_deg": ROLL_AMP_DEG,
                    "moment_amplitude": 0.0,
                    "moment_phase_rad": 0.0,
                    "in_phase_coeff": INPHASE[fl][T],
                    "quad_coeff": QUAD[fl][T],
                }
            )
    return rows


@pytest.fixture
def manifest_rows():
    return _rows()


@pytest.fixture
def model(manifest_rows):
    return SloshingCouplingModel.from_rows(manifest_rows)


def _omega(T):
    return 2.0 * math.pi / T


# ============================================================================
# Loader
# ============================================================================


class TestLoader:
    def test_from_rows_parses_contract(self, model):
        assert model.n_cases == 9
        assert model.fill_levels == [0.25, 0.50, 0.75]

    def test_case_from_row_reconciles_period_and_freq(self):
        c = SloshingCase.from_row({"fill_level": 0.5, "drive_freq_hz": 0.5})
        assert c.drive_period == pytest.approx(2.0)
        assert c.omega == pytest.approx(2.0 * math.pi / 2.0)

    def test_case_omega_and_amplitude(self):
        c = SloshingCase(fill_level=0.5, drive_period=18.0, roll_amplitude_deg=4.0)
        assert c.omega == pytest.approx(2.0 * math.pi / 18.0)
        assert c.roll_amplitude_rad == pytest.approx(math.radians(4.0))

    def test_invalid_period_rejected(self):
        with pytest.raises(ValueError):
            SloshingCase(fill_level=0.5, drive_period=0.0)

    def test_invalid_fill_rejected(self):
        with pytest.raises(ValueError):
            SloshingCase(fill_level=1.5, drive_period=18.0)

    def test_from_json_manifest_list(self, tmp_path, manifest_rows):
        p = tmp_path / "sweep.json"
        p.write_text(json.dumps(manifest_rows))
        m = SloshingCouplingModel.from_sweep_manifest(p)
        assert m.n_cases == 9

    def test_from_json_manifest_object_with_cases_key(self, tmp_path, manifest_rows):
        p = tmp_path / "sweep.json"
        p.write_text(json.dumps({"cases": manifest_rows}))
        m = SloshingCouplingModel.from_sweep_manifest(p)
        assert m.n_cases == 9

    def test_from_csv_manifest(self, tmp_path, manifest_rows):
        import csv

        p = tmp_path / "sweep.csv"
        with p.open("w", newline="") as fh:
            w = csv.DictWriter(fh, fieldnames=list(manifest_rows[0].keys()))
            w.writeheader()
            w.writerows(manifest_rows)
        m = SloshingCouplingModel.from_sweep_manifest(p)
        assert m.n_cases == 9
        # CSV strings must coerce to float and reproduce the coefficients.
        c = m.moment_coefficients(_omega(18.0), 0.50)
        assert c.quad_coeff == pytest.approx(5.0)

    def test_missing_manifest_raises(self, tmp_path):
        with pytest.raises(FileNotFoundError):
            SloshingCouplingModel.from_sweep_manifest(tmp_path / "nope.json")

    def test_empty_cases_rejected(self):
        with pytest.raises(ValueError):
            SloshingCouplingModel([])


# ============================================================================
# Interpolation
# ============================================================================


class TestInterpolation:
    def test_exact_at_swept_grid_points(self, model):
        for fl in FILLS:
            for T in PERIODS:
                c = model.moment_coefficients(_omega(T), fl)
                assert c.in_phase_coeff == pytest.approx(INPHASE[fl][T])
                assert c.quad_coeff == pytest.approx(QUAD[fl][T])
                assert c.clamped is False

    def test_omega_interp_bounded_between_grid(self, model):
        # Between 18 s (omega .349, quad 5.0) and 14 s (omega .449, quad 2.0) at
        # fill 0.50 -> quad must lie strictly between 2.0 and 5.0.
        w = 0.5 * (_omega(18.0) + _omega(14.0))
        c = model.moment_coefficients(w, 0.50)
        assert 2.0 < c.quad_coeff < 5.0
        assert not c.clamped

    def test_omega_interp_is_linear_midpoint(self, model):
        # np.interp is piecewise linear in omega: midpoint omega -> mean quad.
        w = 0.5 * (_omega(18.0) + _omega(14.0))
        c = model.moment_coefficients(w, 0.50)
        assert c.quad_coeff == pytest.approx(0.5 * (5.0 + 2.0))

    def test_fill_interp_bounded_between(self, model):
        # fill 0.375 between 0.25 (quad 2.0) and 0.50 (quad 5.0) at 18 s.
        c = model.moment_coefficients(_omega(18.0), 0.375)
        assert 2.0 < c.quad_coeff < 5.0
        assert c.quad_coeff == pytest.approx(0.5 * (2.0 + 5.0))

    def test_omega_clamp_high_warns_and_clamps(self, model):
        lo, hi = model.omega_range()
        c = model.moment_coefficients(hi * 2.0, 0.50)
        assert c.clamped is True
        # Clamped to the highest-omega swept point (14 s), quad 2.0.
        assert c.quad_coeff == pytest.approx(2.0)

    def test_omega_clamp_low_clamps(self, model):
        lo, hi = model.omega_range()
        c = model.moment_coefficients(lo * 0.5, 0.50)
        assert c.clamped is True
        # Clamped to the lowest-omega swept point (22 s), quad 3.0.
        assert c.quad_coeff == pytest.approx(3.0)

    def test_fill_clamp_warns(self, model):
        c = model.moment_coefficients(_omega(18.0), 0.95)
        assert c.clamped is True
        # Clamped to highest swept fill 0.75, quad 3.0 at 18 s.
        assert c.quad_coeff == pytest.approx(3.0)

    def test_zero_omega_rejected(self, model):
        with pytest.raises(ValueError):
            model.moment_coefficients(0.0, 0.5)


# ============================================================================
# Added inertia / damping extraction
# ============================================================================


class TestAddedInertiaDamping:
    def test_added_damping_equals_quad(self, model):
        w = _omega(18.0)
        assert model.added_roll_damping(w, 0.50) == pytest.approx(5.0)

    def test_added_inertia_from_in_phase(self, model):
        w = _omega(18.0)
        a44 = model.added_roll_inertia(w, 0.50)
        assert a44 == pytest.approx(-25.0 / (w * w))

    def test_coefficients_object_derived_props(self):
        c = MomentCoefficients(
            omega=0.35, fill_level=0.5, in_phase_coeff=25.0, quad_coeff=5.0
        )
        assert c.added_roll_stiffness == pytest.approx(25.0)
        assert c.added_roll_damping == pytest.approx(5.0)
        assert c.added_roll_inertia == pytest.approx(-25.0 / 0.35**2)

    def test_moment_sign_opposes_motion(self):
        # Positive coeffs -> moment opposes positive roll and positive rate.
        c = MomentCoefficients(
            omega=0.35, fill_level=0.5, in_phase_coeff=25.0, quad_coeff=5.0
        )
        assert c.moment(theta=0.1, theta_dot=0.0) < 0.0
        assert c.moment(theta=0.0, theta_dot=0.1) < 0.0


# ============================================================================
# Time-domain feed
# ============================================================================


class TestTimeDomainFeed:
    def test_single_frequency_moment_matches_closed_form(self, model):
        w = _omega(18.0)
        A = math.radians(ROLL_AMP_DEG)
        t = np.linspace(0.0, 4.0 * 18.0, 4000)
        m = model.moment_from_harmonic(ROLL_AMP_DEG, w, 0.50, t)
        # Coeffs at this swept point: in_phase=25, quad=5.
        expected = -25.0 * (A * np.sin(w * t)) - 5.0 * (A * w * np.cos(w * t))
        assert np.allclose(m, expected)

    def test_single_frequency_amplitude(self, model):
        w = _omega(18.0)
        A = math.radians(ROLL_AMP_DEG)
        t = np.linspace(0.0, 8.0 * 18.0, 8000)
        m = model.moment_from_harmonic(ROLL_AMP_DEG, w, 0.50, t)
        expected_amp = A * math.hypot(25.0, 5.0 * w)
        assert np.max(np.abs(m)) == pytest.approx(expected_amp, rel=1e-3)

    def test_pure_damping_is_90deg_out_of_phase(self):
        # in_phase=0 -> moment tracks -theta_dot = -A*w*cos, i.e. peaks at t=0.
        rows = [
            {"fill_level": 0.5, "drive_period": 18.0, "roll_amplitude_deg": 4.0,
             "in_phase_coeff": 0.0, "quad_coeff": 5.0},
        ]
        m = SloshingCouplingModel.from_rows(rows)
        w = _omega(18.0)
        A = math.radians(4.0)
        t = np.linspace(0.0, 18.0, 2000)
        series = m.moment_from_harmonic(4.0, w, 0.5, t)
        # At t=0: theta=0, theta_dot=A*w -> M = -quad*A*w (max magnitude, negative).
        assert series[0] == pytest.approx(-5.0 * A * w, rel=1e-6)

    def test_moment_time_series_gradient_path(self, model):
        w = _omega(18.0)
        A = math.radians(ROLL_AMP_DEG)
        t = np.linspace(0.0, 4.0 * 18.0, 6000)
        theta = A * np.sin(w * t)
        # No theta_dot supplied -> derived via gradient; should match analytic.
        m_grad = model.moment_time_series(t, theta, omega=w, fill_level=0.50)
        m_exact = model.moment_from_harmonic(ROLL_AMP_DEG, w, 0.50, t)
        # Interior points agree closely (endpoints degrade with one-sided diff).
        assert np.allclose(m_grad[5:-5], m_exact[5:-5], rtol=1e-3, atol=1e-3)

    def test_moment_callable_bound_to_fill(self, model):
        w = _omega(18.0)
        fn = model.moment_callable(fill_level=0.50)
        assert fn(0.0, 0.0, w) == pytest.approx(0.0)
        # Matches the direct coefficient moment.
        c = model.moment_coefficients(w, 0.50)
        assert fn(0.05, 0.01, w) == pytest.approx(c.moment(0.05, 0.01))

    def test_shape_mismatch_rejected(self, model):
        t = np.linspace(0, 1, 10)
        theta = np.zeros(9)
        with pytest.raises(ValueError):
            model.moment_time_series(t, theta, omega=0.35, fill_level=0.5)


# ============================================================================
# Anti-roll tuning
# ============================================================================


class TestTuning:
    def test_best_fill_near_resonance(self, model):
        report = model.best_antiroll_fill(natural_period_s=18.0)
        assert isinstance(report, TuningReport)
        assert report.best_fill == pytest.approx(0.50)
        assert report.best_quad_coeff == pytest.approx(5.0)
        assert len(report.per_fill) == 3

    def test_tuning_by_omega(self, model):
        report = model.best_antiroll_fill(omega_roll=_omega(18.0))
        assert report.best_fill == pytest.approx(0.50)
        assert report.natural_period_s == pytest.approx(18.0)

    def test_tuning_requires_input(self, model):
        with pytest.raises(ValueError):
            model.best_antiroll_fill()

    def test_tuning_summary_string(self, model):
        report = model.best_antiroll_fill(natural_period_s=18.0)
        s = report.summary()
        assert "0.50" in s and "anti-roll" in s


# ============================================================================
# One-way-coupling escalation check
# ============================================================================


class TestCouplingStrength:
    def test_weak_coupling_not_escalated(self, model):
        # Large restoring stiffness -> sloshing moment is a tiny fraction.
        rep = model.coupling_strength(
            amplitude_deg=ROLL_AMP_DEG,
            omega=_omega(18.0),
            fill_level=0.50,
            restoring_stiffness=1.0e6,
        )
        assert isinstance(rep, CouplingStrengthReport)
        assert rep.escalate is False
        assert rep.ratio < 0.15

    def test_strong_coupling_flagged(self):
        # Fabricated strong-coupling case: huge coefficients vs small reference.
        rows = [
            {"fill_level": 0.5, "drive_period": 18.0, "roll_amplitude_deg": 4.0,
             "in_phase_coeff": 5.0e7, "quad_coeff": 1.0e7},
        ]
        m = SloshingCouplingModel.from_rows(rows)
        rep = m.coupling_strength(
            amplitude_deg=4.0,
            omega=_omega(18.0),
            fill_level=0.5,
            reference_moment=1.0e5,
        )
        assert rep.escalate is True
        assert rep.ratio > 1.0

    def test_reference_from_stiffness_matches_explicit(self, model):
        w = _omega(18.0)
        A = math.radians(ROLL_AMP_DEG)
        C44 = 2.0e5
        rep_k = model.coupling_strength(ROLL_AMP_DEG, w, 0.50, restoring_stiffness=C44)
        rep_m = model.coupling_strength(ROLL_AMP_DEG, w, 0.50, reference_moment=C44 * A)
        assert rep_k.ratio == pytest.approx(rep_m.ratio)

    def test_slosh_amplitude_matches_closed_form(self, model):
        w = _omega(18.0)
        A = math.radians(ROLL_AMP_DEG)
        rep = model.coupling_strength(
            ROLL_AMP_DEG, w, 0.50, reference_moment=1.0e5
        )
        expected = A * math.hypot(25.0, 5.0 * w)
        assert rep.slosh_moment_amplitude == pytest.approx(expected)

    def test_missing_reference_rejected(self, model):
        with pytest.raises(ValueError):
            model.coupling_strength(ROLL_AMP_DEG, _omega(18.0), 0.50)


# ============================================================================
# Public exports
# ============================================================================


def test_public_exports():
    from digitalmodel.solvers.openfoam import (  # noqa: F401
        CouplingStrengthReport,
        FillDampingResult,
        MomentCoefficients,
        SloshingCase,
        SloshingCouplingModel,
        TuningReport,
    )
