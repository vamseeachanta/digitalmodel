"""
Tests for the SCR/SLWR touchdown-zone fatigue workflow (DNV-RP-C203).

Deterministic, synthetic histograms only — no solver / no network.
"""

import math

import numpy as np
import pytest

from digitalmodel.riser_fatigue import (
    RiserSection,
    TouchdownFatigueInput,
    assess_touchdown_fatigue,
    report_markdown,
)


def _section(wt: float = 25.0) -> RiserSection:
    return RiserSection(outer_diameter_mm=323.9, wall_thickness_mm=wt)


def test_single_bin_hand_verified():
    """One bin, WT=t_ref (no thickness correction), SCF=1, F1 seawater_cp.

    DNV-RP-C203 F1 seawater_cp: log_a1 = 11.299, k1 = 3.
    N(80 MPa) = 10**11.299 * 80**-3 = 388803.3865...
    n = 1e5  ->  D = n/N = 0.257199...
    """
    inp = TouchdownFatigueInput(
        section=_section(25.0),
        stress_ranges_mpa=[80.0],
        cycles=[1.0e5],
        sn_class="F1",
        environment="seawater_cp",
        scf=1.0,
        design_life_years=25.0,
        dff=10.0,
        histogram_period_years=1.0,
    )
    res = assess_touchdown_fatigue(inp)

    N_expected = 10 ** 11.299 * 80.0 ** -3.0
    D_expected = 1.0e5 / N_expected

    assert res.bins[0].allowable_cycles == pytest.approx(N_expected, rel=1e-9)
    assert res.bins[0].damage == pytest.approx(D_expected, rel=1e-9)
    assert res.period_damage == pytest.approx(0.25719940583546, rel=1e-9)
    assert res.annual_damage == pytest.approx(D_expected, rel=1e-9)
    assert res.design_life_damage == pytest.approx(D_expected * 25.0, rel=1e-9)
    assert res.fatigue_life_years == pytest.approx(1.0 / D_expected, rel=1e-9)


def test_safe_case_damage_well_below_one():
    """Low stress ranges over the design life -> tiny damage, PASS."""
    inp = TouchdownFatigueInput(
        section=_section(25.0),
        stress_ranges_mpa=[10.0, 15.0, 20.0],
        cycles=[5.0e5, 1.0e5, 1.0e4],
        sn_class="F1",
        environment="seawater_cp",
        scf=1.0,
        design_life_years=25.0,
        dff=10.0,
    )
    res = assess_touchdown_fatigue(inp)
    assert res.design_life_damage < 0.1   # << allowable 1/DFF
    assert res.usage_factor < 1.0
    assert res.pass_fail == "PASS"
    assert res.fatigue_life_years > 25.0


def test_failing_case_damage_above_one_over_design_life():
    """High stress ranges -> design-life damage > 1, FAIL even without DFF."""
    inp = TouchdownFatigueInput(
        section=_section(25.0),
        stress_ranges_mpa=[120.0, 150.0],
        cycles=[2.0e5, 5.0e4],
        sn_class="F1",
        environment="seawater_cp",
        scf=1.2,
        design_life_years=25.0,
        dff=10.0,
    )
    res = assess_touchdown_fatigue(inp)
    assert res.design_life_damage > 1.0
    assert res.usage_factor > 1.0
    assert res.pass_fail == "FAIL"
    assert res.fatigue_life_years < 25.0


def test_dff_governs_verdict():
    """A case that passes D<1 over life but fails the DFF (usage>1) check.

    With DFF=10 the allowable damage is 0.1.  Pick a damage between 0.1 and
    1.0 over the design life so the verdict is FAIL purely due to the DFF,
    then show DFF=1 flips it to PASS.
    """
    base = dict(
        section=_section(25.0),
        stress_ranges_mpa=[60.0],
        cycles=[1.1e4],
        sn_class="F1",
        environment="seawater_cp",
        scf=1.0,
        design_life_years=25.0,
        histogram_period_years=1.0,
    )
    res_dff10 = assess_touchdown_fatigue(TouchdownFatigueInput(dff=10.0, **base))
    # Confirm the damage sits in the (0.1, 1.0) window so DFF is the governor.
    assert 0.1 < res_dff10.design_life_damage < 1.0
    assert res_dff10.usage_factor > 1.0
    assert res_dff10.pass_fail == "FAIL"

    res_dff1 = assess_touchdown_fatigue(TouchdownFatigueInput(dff=1.0, **base))
    assert res_dff1.design_life_damage == pytest.approx(
        res_dff10.design_life_damage, rel=1e-12
    )
    assert res_dff1.usage_factor <= 1.0
    assert res_dff1.pass_fail == "PASS"


def test_thickness_correction_increases_damage():
    """Thicker wall (> t_ref) raises corrected stress -> more damage."""
    common = dict(
        stress_ranges_mpa=[90.0],
        cycles=[1.0e5],
        sn_class="F1",
        environment="seawater_cp",
    )
    thin = assess_touchdown_fatigue(
        TouchdownFatigueInput(section=_section(25.0), **common)
    )
    thick = assess_touchdown_fatigue(
        TouchdownFatigueInput(section=_section(40.0), **common)
    )
    # k=0.25 default: factor = (40/25)^0.25
    factor = (40.0 / 25.0) ** 0.25
    assert thick.bins[0].stress_corrected_mpa == pytest.approx(
        90.0 * factor, rel=1e-9
    )
    assert thick.period_damage > thin.period_damage


def _e_class_40mm(**overrides):
    kwargs = dict(
        section=_section(40.0),
        stress_ranges_mpa=[100.0],
        cycles=[1.0e5],
        sn_class="E",
        environment="seawater_cp",
    )
    kwargs.update(overrides)
    return TouchdownFatigueInput(**kwargs)


def test_thickness_exponent_defaults_from_the_class():
    """#2165 review finding 3: k comes from the selected class, not 0.25.

    DNV-RP-C203 (2011) Table 2-2 gives k = 0.20 for class E.
    E, t = 40 mm, 100 MPa nominal:
      100 * (40/25)^0.20 = 100 * exp(0.20 * ln 1.6) = 100 * 1.098560 = 109.856 MPa
    The old universal default k = 0.25 gave
      100 * (40/25)^0.25 = 100 * 1.124683 = 112.468 MPa.
    """
    res = assess_touchdown_fatigue(_e_class_40mm())
    assert res.bins[0].stress_corrected_mpa == pytest.approx(109.856, abs=5e-4)
    assert res.bins[0].stress_corrected_mpa == pytest.approx(
        100.0 * 1.6**0.20, rel=1e-12
    )
    assert res.thickness_exponent == pytest.approx(0.20)


def test_thickness_exponent_explicit_override_is_kept():
    """An explicit k still wins: k = 0.25 on class E gives 112.468 MPa."""
    res = assess_touchdown_fatigue(_e_class_40mm(thickness_exponent=0.25))
    assert res.bins[0].stress_corrected_mpa == pytest.approx(112.468, abs=5e-4)
    assert res.thickness_exponent == pytest.approx(0.25)


@pytest.mark.parametrize(
    "sn_class, environment, k",
    [
        ("B1", "air", 0.0),
        ("C", "seawater_cp", 0.15),
        ("D", "free_corrosion", 0.20),
        ("F1", "seawater_cp", 0.25),
    ],
)
def test_thickness_exponent_default_per_class_and_environment(sn_class, environment, k):
    """k per class from DNV-RP-C203 (2011) Tables 2-1 to 2-3, via
    fatigue.c203_sn_tables (the same source as the quick-check)."""
    res = assess_touchdown_fatigue(
        _e_class_40mm(sn_class=sn_class, environment=environment)
    )
    assert res.thickness_exponent == pytest.approx(k)
    assert res.bins[0].stress_corrected_mpa == pytest.approx(100.0 * 1.6**k, rel=1e-12)


def test_lowercase_class_matches_uppercase():
    """#2165 PR #2192 review r2 finding 1: the class is normalised once, as
    get_sn_curve() does (upper case), before both the S-N lookup and the
    class-k lookup. "e" gives the same result as "E" (109.856 MPa, k = 0.20)."""
    lower = assess_touchdown_fatigue(_e_class_40mm(sn_class="e"))
    upper = assess_touchdown_fatigue(_e_class_40mm(sn_class="E"))
    assert lower.thickness_exponent == pytest.approx(0.20)
    assert lower.bins[0].stress_corrected_mpa == pytest.approx(109.856, abs=5e-4)
    assert lower.period_damage == upper.period_damage
    assert lower.bins[0].allowable_cycles == upper.bins[0].allowable_cycles
    assert lower.sn_class == "E"


def test_report_states_the_applied_thickness_exponent():
    inp = _e_class_40mm()
    md = report_markdown(inp, assess_touchdown_fatigue(inp))
    assert "k=0.20" in md


def test_mismatched_lengths_raise():
    with pytest.raises(ValueError):
        assess_touchdown_fatigue(
            TouchdownFatigueInput(
                section=_section(),
                stress_ranges_mpa=[10.0, 20.0],
                cycles=[1.0e5],
            )
        )


def test_report_markdown_contains_key_fields():
    inp = TouchdownFatigueInput(
        section=_section(25.0),
        stress_ranges_mpa=[50.0, 80.0],
        cycles=[2.0e5, 5.0e4],
        sn_class="F1",
        environment="seawater_cp",
        dff=10.0,
    )
    res = assess_touchdown_fatigue(inp)
    md = report_markdown(inp, res)
    assert "DNV-RP-C203" in md
    assert "Touchdown" in md
    assert res.pass_fail in md
    assert "F1" in md
    assert "Usage factor" in md
