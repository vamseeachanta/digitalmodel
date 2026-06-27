# ABOUTME: Tests for DNV-RP-F101 corroded-pipe remaining-strength module —
# ABOUTME: hand-calc golden, monotonicity, intact limit, B31G sanity, interaction, validation.
import math

import pytest

from digitalmodel.asset_integrity.corroded_pipe import modified_b31g
from digitalmodel.asset_integrity.dnv_rp_f101 import (
    SMTS_PSI,
    dnv_f101_interacting,
    dnv_f101_psf,
    dnv_f101_single_defect,
    length_correction_factor,
)


# Reference geometry / material for the hand-calc golden.
D, T, DEPTH, LEN = 30.0, 0.375, 0.15, 8.0
SMTS = 66_700.0  # API 5L X52


def test_hand_calc_golden():
    """Single-defect capacity matches an independent hand calculation."""
    Q = math.sqrt(1.0 + 0.31 * (LEN / math.sqrt(D * T)) ** 2)
    dt = DEPTH / T  # 0.4
    p_cap = 2.0 * T * SMTS / (D - T) * (1.0 - dt) / (1.0 - dt / Q)
    # Frozen golden numbers:
    assert Q == pytest.approx(1.6623945246407532, rel=1e-12)
    assert dt == pytest.approx(0.4, rel=1e-12)
    assert p_cap == pytest.approx(1334.1940092246919, rel=1e-12)

    res = dnv_f101_single_defect(D, T, DEPTH, LEN, SMTS)
    assert res.Q == pytest.approx(Q, rel=1e-12)
    assert res.d_over_t == pytest.approx(0.4, rel=1e-12)
    assert res.capacity_pressure_psi == pytest.approx(1334.1940092246919, rel=1e-12)
    assert res.allowable_pressure_psi == pytest.approx(0.72 * 1334.1940092246919, rel=1e-12)
    assert res.method == "DNV-F101-AS"


def test_smts_table_x52():
    assert SMTS_PSI["X52"] == 66_700.0
    assert SMTS_PSI["X70"] == 82_700.0


def test_deeper_defect_lowers_capacity():
    base = dnv_f101_single_defect(D, T, 0.10, LEN, SMTS).capacity_pressure_psi
    for d in (0.15, 0.20, 0.25, 0.30):
        deeper = dnv_f101_single_defect(D, T, d, LEN, SMTS).capacity_pressure_psi
        assert deeper < base
        base = deeper


def test_longer_defect_lowers_capacity():
    base = dnv_f101_single_defect(D, T, DEPTH, 2.0, SMTS).capacity_pressure_psi
    for L in (4.0, 8.0, 16.0, 32.0):
        longer = dnv_f101_single_defect(D, T, DEPTH, L, SMTS).capacity_pressure_psi
        assert longer < base
        base = longer


def test_zero_defect_gives_intact_capacity():
    res = dnv_f101_single_defect(D, T, 0.0, LEN, SMTS)
    intact = 2.0 * T * SMTS / (D - T)
    assert res.d_over_t == pytest.approx(0.0)
    assert res.capacity_pressure_psi == pytest.approx(intact, rel=1e-12)
    assert res.intact_pressure_psi == pytest.approx(intact, rel=1e-12)


def test_q_increases_with_length():
    q_short = length_correction_factor(D, T, 2.0)
    q_long = length_correction_factor(D, T, 20.0)
    assert q_long > q_short > 1.0


def test_usage_factor_scales_allowable_linearly():
    cap = dnv_f101_single_defect(D, T, DEPTH, LEN, SMTS).capacity_pressure_psi
    for f in (0.50, 0.72, 0.90):
        res = dnv_f101_single_defect(D, T, DEPTH, LEN, SMTS, usage_factor=f)
        assert res.allowable_pressure_psi == pytest.approx(f * cap, rel=1e-12)


def test_maop_acceptance_flag():
    res = dnv_f101_single_defect(D, T, DEPTH, LEN, SMTS, maop_psi=500.0)
    assert res.acceptable is True
    res2 = dnv_f101_single_defect(D, T, DEPTH, LEN, SMTS, maop_psi=5000.0)
    assert res2.acceptable is False


def test_cross_method_sanity_vs_b31g():
    """DNV (uses SMTS) capacity exceeds Modified B31G failure pressure (uses SMYS)."""
    smys = 52_000.0  # X52 SMYS for B31G
    dnv = dnv_f101_single_defect(D, T, DEPTH, LEN, SMTS)
    b31 = modified_b31g(D, T, DEPTH, LEN, smys)
    assert math.isfinite(dnv.capacity_pressure_psi) and dnv.capacity_pressure_psi > 0
    assert math.isfinite(b31.failure_pressure_psi) and b31.failure_pressure_psi > 0
    assert dnv.capacity_pressure_psi > b31.failure_pressure_psi
    # Broadly comparable, within a factor of a few.
    assert dnv.capacity_pressure_psi < 5.0 * b31.failure_pressure_psi


def test_psf_format_more_conservative_than_intact():
    res = dnv_f101_psf(D, T, DEPTH, LEN, SMTS)
    assert res.method == "DNV-F101-PSF"
    # PSF allowable is reduced by gamma_m and depth tolerance -> below mean capacity.
    assert 0.0 < res.allowable_pressure_psi < res.capacity_pressure_psi
    assert res.details["d_over_t_star"] > res.d_over_t  # tolerance added


def test_psf_factor_overrides():
    lenient = dnv_f101_psf(D, T, DEPTH, LEN, SMTS, gamma_m=0.90, std_rel_depth=0.0)
    strict = dnv_f101_psf(D, T, DEPTH, LEN, SMTS, gamma_m=0.70, std_rel_depth=0.15)
    assert lenient.allowable_pressure_psi > strict.allowable_pressure_psi


def test_interacting_defects_le_worst_single():
    """Two close defects merge into a worse composite than either alone."""
    # Gap between the two defects well below 2*sqrt(D*t) ~ 6.7 in.
    defects = [(0.0, 4.0, 0.15), (5.0, 4.0, 0.15)]
    inter = dnv_f101_interacting(D, T, defects, SMTS)
    worst_single = min(
        dnv_f101_single_defect(D, T, 0.15, 4.0, SMTS).capacity_pressure_psi,
        dnv_f101_single_defect(D, T, 0.15, 4.0, SMTS).capacity_pressure_psi,
    )
    assert inter.capacity_pressure_psi <= worst_single
    assert inter.details["governing_kind"] == "composite"


def test_far_apart_defects_do_not_interact():
    """Defects spaced beyond the interaction limit are governed by a single defect."""
    # Gap = 20 in >> 2*sqrt(D*t) ~ 6.7 in.
    defects = [(0.0, 4.0, 0.20), (24.0, 4.0, 0.15)]
    inter = dnv_f101_interacting(D, T, defects, SMTS)
    deepest_single = dnv_f101_single_defect(D, T, 0.20, 4.0, SMTS).capacity_pressure_psi
    assert inter.capacity_pressure_psi == pytest.approx(deepest_single, rel=1e-12)
    assert inter.details["governing_kind"] == "single"


def test_input_validation():
    with pytest.raises(ValueError):
        dnv_f101_single_defect(30.0, 0.375, 0.5, 8.0, SMTS)  # d > t
    with pytest.raises(ValueError):
        dnv_f101_single_defect(0.375, 30.0, 0.15, 8.0, SMTS)  # t > D
    with pytest.raises(ValueError):
        dnv_f101_single_defect(30.0, 0.375, -0.1, 8.0, SMTS)  # d < 0
    with pytest.raises(ValueError):
        dnv_f101_single_defect(30.0, 0.375, 0.15, -1.0, SMTS)  # L < 0
    with pytest.raises(ValueError):
        dnv_f101_interacting(30.0, 0.375, [], SMTS)  # empty colony
