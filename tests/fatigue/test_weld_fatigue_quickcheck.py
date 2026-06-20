"""
Tests for the weld-fatigue S-N quick-check (DNV-RP-C203), issue #818.

Hand-verified anchors (all in the m=3 region, S > SD, so the single-slope
form  N = a * S^(-m)  applies exactly):

  * Class F in air, log10(a1) = 11.855, m = 3 (DNV-RP-C203 Table 2-1):
        N(90 MPa) = 10^(11.855 - 3*log10(90)) = 982_364 cycles.
  * Two-bin Miner sum at 90 MPa (above) is hand-checked below.
"""

import math

import pandas as pd
import pytest

from digitalmodel.fatigue.weld_fatigue_quickcheck import (
    StressBin,
    weld_fatigue_quickcheck,
    run_from_config,
    result_to_csv,
    result_to_html,
)

LOG_A1_F_AIR = 11.855  # DNV-RP-C203 Table 2-1, class F in air
M = 3.0


def _hand_N(stress: float, log_a: float = LOG_A1_F_AIR, m: float = M) -> float:
    """Allowable cycles in the single-slope region: N = a * S^(-m)."""
    return 10 ** (log_a - m * math.log10(stress))


def test_single_stress_range_N_hand_verified():
    """One 90 MPa bin, class F air: allowable N matches the hand value."""
    res = weld_fatigue_quickcheck(
        [StressBin(90.0, 1.0)],
        weld_class="F",
        environment="air",
        design_life_years=1.0,
        block_years=1.0,
    )
    n_engine = float(res.bins["allowable_cycles"].iloc[0])
    assert n_engine == pytest.approx(982_364.07, rel=1e-5)
    assert n_engine == pytest.approx(_hand_N(90.0), rel=1e-9)


def test_clearly_safe_case_damage_far_below_one():
    """Tiny applied cycles at a low stress -> damage << 1, PASS."""
    res = weld_fatigue_quickcheck(
        [StressBin(50.0, 1.0e3)],
        weld_class="D",
        environment="air",
        design_life_years=25.0,
        block_years=1.0,
    )
    assert res.passed
    assert res.total_damage < 1e-2
    assert res.life_factor > 100.0


def test_failing_case_damage_above_one():
    """Heavy high-stress loading -> damage > 1, FAIL."""
    res = weld_fatigue_quickcheck(
        [StressBin(150.0, 5.0e5)],
        weld_class="G",
        environment="seawater_cp",
        design_life_years=25.0,
        block_years=1.0,
    )
    assert not res.passed
    assert res.pass_fail == "FAIL"
    assert res.total_damage > 1.0


def test_two_bin_histogram_miner_sum_hand_verified():
    """Two bins (both above SD, m=3) -> Miner sum matches hand calc.

    Bins: 120 MPa x 4e4, 90 MPa x 2e5, class F air, 1-yr block & life.
        N(120) = a*120^-3,  N(90) = a*90^-3.
        D = 4e4/N(120) + 2e5/N(90).
    """
    res = weld_fatigue_quickcheck(
        [StressBin(120.0, 4.0e4), StressBin(90.0, 2.0e5)],
        weld_class="F",
        environment="air",
        design_life_years=1.0,
        block_years=1.0,
    )
    n120 = _hand_N(120.0)
    n90 = _hand_N(90.0)
    expected = 4.0e4 / n120 + 2.0e5 / n90
    assert res.total_damage == pytest.approx(expected, rel=1e-9)
    # sanity: the per-bin damages add up to the total.
    assert res.bins["damage"].sum() == pytest.approx(res.total_damage, rel=1e-12)


def test_design_life_extrapolation_scales_linearly():
    """Damage over the design life = block damage * (life/block)."""
    bins = [StressBin(80.0, 1.0e5)]
    one_yr = weld_fatigue_quickcheck(
        bins, weld_class="F", design_life_years=1.0, block_years=1.0
    )
    twenty_five = weld_fatigue_quickcheck(
        bins, weld_class="F", design_life_years=25.0, block_years=1.0
    )
    assert twenty_five.total_damage == pytest.approx(
        25.0 * one_yr.total_damage, rel=1e-12
    )


def test_thickness_correction_applied_above_t_ref():
    """t > t_ref increases applied stress by (t/t_ref)^k -> more damage."""
    bins = [StressBin(100.0, 1.0e5)]
    thin = weld_fatigue_quickcheck(
        bins, weld_class="F", design_life_years=1.0, thickness_mm=25.0
    )
    thick = weld_fatigue_quickcheck(
        bins, weld_class="F", design_life_years=1.0, thickness_mm=40.0
    )
    factor = (40.0 / 25.0) ** 0.25  # class F exponent k=0.25
    assert thick.bins["applied_stress_range"].iloc[0] == pytest.approx(
        100.0 * factor, rel=1e-9
    )
    # damage scales with S^m=3 for a fixed N region: ~factor^3 heavier.
    assert thick.total_damage == pytest.approx(
        thin.total_damage * factor ** 3, rel=1e-6
    )


def test_thickness_no_correction_below_t_ref():
    """t <= t_ref -> no correction (factor capped at 1.0)."""
    bins = [StressBin(100.0, 1.0e5)]
    res = weld_fatigue_quickcheck(
        bins, weld_class="F", design_life_years=1.0, thickness_mm=20.0
    )
    assert res.bins["applied_stress_range"].iloc[0] == pytest.approx(100.0)


def test_dataframe_histogram_input():
    """A DataFrame histogram is accepted and matches the StressBin form."""
    hist = pd.DataFrame({"stress_range": [90.0], "cycles": [1.0e5]})
    via_df = weld_fatigue_quickcheck(hist, weld_class="F", design_life_years=1.0)
    via_bins = weld_fatigue_quickcheck(
        [StressBin(90.0, 1.0e5)], weld_class="F", design_life_years=1.0
    )
    assert via_df.total_damage == pytest.approx(via_bins.total_damage, rel=1e-12)


def test_run_from_config_and_artifacts():
    """The YAML-style config path runs and emits non-empty CSV/HTML."""
    config = {
        "weld_fatigue": {
            "weld_class": "F",
            "environment": "seawater_cp",
            "thickness_mm": 40.0,
            "design_life_years": 25.0,
            "block_years": 1.0,
            "histogram": [
                {"stress_range": 28.0, "cycles": 1.0e5},
                {"stress_range": 16.0, "cycles": 1.0e6},
            ],
        }
    }
    res = run_from_config(config)
    assert res.passed
    csv = result_to_csv(res)
    assert "allowable_cycles" in csv and "damage" in csv
    html = result_to_html(res)
    assert "PASS" in html and "DNV-RP-C203" in html


def test_input_validation():
    with pytest.raises(ValueError):
        weld_fatigue_quickcheck([], weld_class="F")
    with pytest.raises(ValueError):
        weld_fatigue_quickcheck([StressBin(-5.0, 1.0)], weld_class="F")
    with pytest.raises(ValueError):
        weld_fatigue_quickcheck(
            [StressBin(90.0, 1.0)], weld_class="F", design_life_years=0.0
        )
