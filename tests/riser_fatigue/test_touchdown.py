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
