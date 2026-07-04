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
    epsilon_d_fractile,
    gamma_d_factor,
    gamma_m_factor,
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
    assert res.allowable_pressure_psi == pytest.approx(
        0.72 * 1334.1940092246919, rel=1e-12
    )
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


# ---------------------------------------------------------------------------
# DNV-RP-F101 Part-A PSF tables (finding #6) — Table 3-2 / 3-7 / 3-8
# ---------------------------------------------------------------------------
def test_gamma_m_table_values():
    """gamma_m by safety class and sizing method (DNV-RP-F101 Table 3-2)."""
    # relative (e.g. MFL)
    assert gamma_m_factor("low", "relative") == pytest.approx(0.90)
    assert gamma_m_factor("medium", "relative") == pytest.approx(0.85)
    assert gamma_m_factor("high", "relative") == pytest.approx(0.80)
    assert gamma_m_factor("very high", "relative") == pytest.approx(0.76)
    # absolute (e.g. UT)
    assert gamma_m_factor("low", "absolute") == pytest.approx(0.94)
    assert gamma_m_factor("medium", "absolute") == pytest.approx(0.88)
    assert gamma_m_factor("high", "absolute") == pytest.approx(0.82)
    assert gamma_m_factor("very high", "absolute") == pytest.approx(0.77)
    # 'normal' is an alias for 'medium'
    assert gamma_m_factor("normal", "absolute") == pytest.approx(0.88)
    # legacy scalar default == Very-High class / absolute sizing
    assert gamma_m_factor() == pytest.approx(0.77)


def test_gamma_m_invalid_inputs():
    with pytest.raises(ValueError):
        gamma_m_factor("extreme", "relative")
    with pytest.raises(ValueError):
        gamma_m_factor("high", "scanned")


def test_epsilon_d_fractile_self_consistency():
    """epsilon_d ~ 0, 1.0, 2.0 at StD[d/t] = 0.04, 0.08, 0.16 (legacy 1.0 == 0.08)."""
    assert epsilon_d_fractile(0.0) == pytest.approx(0.0)
    assert epsilon_d_fractile(0.04) == pytest.approx(0.0, abs=5e-3)
    assert epsilon_d_fractile(0.08) == pytest.approx(1.0, abs=5e-3)
    assert epsilon_d_fractile(0.16) == pytest.approx(2.0, abs=5e-3)
    # above the calibrated range it is clamped at the 0.16 value
    assert epsilon_d_fractile(0.25) == pytest.approx(epsilon_d_fractile(0.16))


def test_gamma_d_table_values_and_monotonicity():
    """gamma_d rises with StD and with safety class (Table 3-7 points)."""
    assert gamma_d_factor(0.0, "medium") == pytest.approx(1.0)
    # Table 3-7 column at StD = 0.08:
    assert gamma_d_factor(0.08, "low") == pytest.approx(1.20)
    assert gamma_d_factor(0.08, "medium") == pytest.approx(1.27904, abs=1e-5)
    assert gamma_d_factor(0.08, "high") == pytest.approx(1.31776, abs=1e-5)
    assert gamma_d_factor(0.08, "very high") == pytest.approx(1.43488, abs=1e-5)
    # Very-High at StD = 0.16 matches the table point 1.84
    assert gamma_d_factor(0.16, "very high") == pytest.approx(1.84352, abs=1e-5)
    # higher safety class -> larger gamma_d (more conservative)
    assert (
        gamma_d_factor(0.08, "low")
        < gamma_d_factor(0.08, "medium")
        < gamma_d_factor(0.08, "high")
        < gamma_d_factor(0.08, "very high")
    )
    # monotone in StD
    assert gamma_d_factor(0.04, "medium") < gamma_d_factor(0.12, "medium")
    # clamped above 0.16
    assert gamma_d_factor(0.30, "very high") == pytest.approx(
        gamma_d_factor(0.16, "very high")
    )


def test_psf_default_uses_tables_golden():
    """Default PSF (Very-High/absolute/StD=0.08) reproduces gamma_m=0.77 and the
    tabulated gamma_d/epsilon_d.  Behaviour change vs the old scalar defaults
    (gamma_d 1.0 -> 1.43488): allowable 950.59 -> 690.45 psi (more conservative);
    0.77 is the Very-High/absolute entry of Table 3-2, not Normal."""
    res = dnv_f101_psf(D, T, DEPTH, LEN, SMTS)
    assert res.details["gamma_m"] == pytest.approx(0.77)
    assert res.details["gamma_d"] == pytest.approx(1.43488, abs=1e-5)
    assert res.details["epsilon_d"] == pytest.approx(1.00312, abs=1e-5)
    assert res.details["safety_class"] == "very high"
    assert res.details["measurement_method"] == "absolute"
    # frozen NEW golden (Very-High tabulated gamma_d):
    assert res.allowable_pressure_psi == pytest.approx(690.4456312148728, rel=1e-10)
    # OLD scalar-default value, for the record (no longer produced):
    assert res.allowable_pressure_psi < 950.5927483767431


def test_psf_higher_safety_class_more_conservative():
    low = dnv_f101_psf(D, T, DEPTH, LEN, SMTS, safety_class="low")
    medium = dnv_f101_psf(D, T, DEPTH, LEN, SMTS, safety_class="medium")
    high = dnv_f101_psf(D, T, DEPTH, LEN, SMTS, safety_class="high")
    vhigh = dnv_f101_psf(D, T, DEPTH, LEN, SMTS, safety_class="very high")
    assert (
        vhigh.allowable_pressure_psi
        < high.allowable_pressure_psi
        < medium.allowable_pressure_psi
        < low.allowable_pressure_psi
    )


def test_psf_relative_vs_absolute_sizing():
    rel = dnv_f101_psf(
        D, T, DEPTH, LEN, SMTS, safety_class="high", measurement_method="relative"
    )
    ab = dnv_f101_psf(
        D, T, DEPTH, LEN, SMTS, safety_class="high", measurement_method="absolute"
    )
    assert rel.details["gamma_m"] == pytest.approx(0.80)
    assert ab.details["gamma_m"] == pytest.approx(0.82)
    assert rel.allowable_pressure_psi < ab.allowable_pressure_psi


def test_psf_explicit_overrides_bypass_tables():
    res = dnv_f101_psf(
        D,
        T,
        DEPTH,
        LEN,
        SMTS,
        gamma_m=0.90,
        gamma_d=1.0,
        epsilon_d=1.0,
        std_rel_depth=0.08,
    )
    assert res.details["gamma_m"] == pytest.approx(0.90)
    assert res.details["gamma_d"] == pytest.approx(1.0)


def test_psf_std_out_of_range_flag():
    res = dnv_f101_psf(D, T, DEPTH, LEN, SMTS, std_rel_depth=0.25)
    assert res.details["within_applicability"] is False
    assert "StD" in res.details["applicability_note"]


def test_psf_deep_defect_applicability_flag():
    # d/t = 0.32/0.375 = 0.853 > 0.85
    res = dnv_f101_psf(D, T, 0.32, LEN, SMTS)
    assert res.details["within_applicability"] is False
    assert "0.85" in res.details["applicability_note"]


# ---------------------------------------------------------------------------
# Interacting-defect area-averaged combined depth (finding #1)
# DNV-RP-F101 Sec. 3.8.2: d_comb = sum(d_i*l_i) / L_comb, L_comb = TOTAL span
# including inter-defect gaps (gaps dilute the average depth).
# ---------------------------------------------------------------------------
def test_interacting_area_averaged_two_defects_golden():
    """Unequal-depth colony: combined depth = area / TOTAL combined length.

    defects [(0,4,0.20),(5,4,0.10)] (gap 1 in < 6.7 in -> interact):
      d_comb = (0.20*4 + 0.10*4) / 9 = 1.2/9 = 0.13333 in, L_comb = 9 in
      OLD (max depth 0.20, L=9) -> governing 1120.40 psi
      NEW (area-avg depth 0.1333) -> governing 1356.49 psi
    """
    defects = [(0.0, 4.0, 0.20), (5.0, 4.0, 0.10)]
    inter = dnv_f101_interacting(D, T, defects, SMTS)
    assert inter.details["governing_kind"] == "composite"
    assert inter.details["comp_depth_in"] == pytest.approx(1.2 / 9.0, rel=1e-12)
    assert inter.details["comp_length_in"] == pytest.approx(9.0, rel=1e-12)
    assert inter.capacity_pressure_psi == pytest.approx(1356.4947867240112, rel=1e-10)
    # strictly less conservative than the old max-depth grouping (1120.40 psi)
    old_max_depth = dnv_f101_single_defect(D, T, 0.20, 9.0, SMTS).capacity_pressure_psi
    assert old_max_depth == pytest.approx(1120.3969410105603, rel=1e-10)
    assert inter.capacity_pressure_psi > old_max_depth


def test_interacting_three_defect_colony_golden():
    """Three interacting defects, area-averaged combined depth.

    defects [(0,4,0.30),(5,4,0.20),(10,4,0.25)] all interact (gaps 1 in):
      governing composite (0..2): area = (0.30+0.20+0.25)*4 = 3.0,
      L_comb = 14 in -> d_comb = 3.0/14 = 0.21429 in -> 934.83 psi.
    """
    defects = [(0.0, 4.0, 0.30), (5.0, 4.0, 0.20), (10.0, 4.0, 0.25)]
    inter = dnv_f101_interacting(D, T, defects, SMTS)
    assert inter.details["governing_kind"] == "composite"
    assert inter.details["governing_members"] == [0, 1, 2]
    assert inter.details["comp_depth_in"] == pytest.approx(3.0 / 14.0, rel=1e-12)
    assert inter.details["comp_length_in"] == pytest.approx(14.0, rel=1e-12)
    assert inter.capacity_pressure_psi == pytest.approx(934.8311372034209, rel=1e-10)


def test_interacting_total_length_denominator_not_member_length():
    """Regression: the denominator is the TOTAL span (incl. gaps), not the sum
    of member defect lengths.  For [(0,4,0.20),(5,4,0.10)] the two differ:
    area/9 (correct) = 0.1333 vs area/8 (wrong) = 0.15."""
    inter = dnv_f101_interacting(D, T, [(0.0, 4.0, 0.20), (5.0, 4.0, 0.10)], SMTS)
    # correct DNV denominator -> 0.1333, NOT the member-length 0.15
    assert inter.details["comp_depth_in"] == pytest.approx(1.2 / 9.0, rel=1e-12)
    assert inter.details["comp_depth_in"] != pytest.approx(0.15, rel=1e-6)


def test_interacting_equal_depth_touching_reduces_to_max():
    """Equal-depth members reduce to the max depth ONLY when touching (gap=0)."""
    # touching: gap = 0 -> L_comb = 8 = sum of lengths -> d_comb = max = 0.15
    touching = dnv_f101_interacting(D, T, [(0.0, 4.0, 0.15), (4.0, 4.0, 0.15)], SMTS)
    assert touching.details["comp_depth_in"] == pytest.approx(0.15, rel=1e-12)
    assert touching.details["comp_length_in"] == pytest.approx(8.0, rel=1e-12)
    # equal-depth single (0.15, 8.0) == hand-calc golden capacity 1334.19
    assert touching.capacity_pressure_psi == pytest.approx(
        1334.1940092246919, rel=1e-10
    )


def test_interacting_equal_depth_with_gap_dilutes():
    """A non-zero gap dilutes even equal-depth members below the member depth."""
    gapped = dnv_f101_interacting(D, T, [(0.0, 4.0, 0.15), (5.0, 4.0, 0.15)], SMTS)
    # 0.15*8 / 9 = 0.13333 < 0.15
    assert gapped.details["comp_depth_in"] == pytest.approx(0.15 * 8 / 9, rel=1e-12)
    assert gapped.details["comp_depth_in"] < 0.15


def test_interacting_applicability_flag_surfaced():
    inter = dnv_f101_interacting(D, T, [(0.0, 4.0, 0.20), (5.0, 4.0, 0.10)], SMTS)
    assert inter.details["within_applicability"] is True


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
