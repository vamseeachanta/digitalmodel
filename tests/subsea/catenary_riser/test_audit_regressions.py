"""Issue #2751: package calls checked against the cited analytical worksheet."""
from dataclasses import replace

import pytest

from digitalmodel.subsea.catenary_riser import (
    BuoyancyModule, EffectiveWeightCalculator, LazyWaveConfiguration,
    RiserConfiguration, SEAWATER, PRODUCTION_OIL, STEEL_API_5L_X65,
    SimpleCatenaryAnalyzer,
)
from digitalmodel.subsea.catenary_riser.legacy.catenary_equation import CatenaryCalculator
from digitalmodel.subsea.catenary_riser.legacy.catenaryMethods import catenaryEquation
from digitalmodel.subsea.catenary_riser.report import (
    FatigueResult, StrengthResult, CodeCheck, RiserReportData, render_html, render_markdown,
)


@pytest.fixture
def riser():
    return RiserConfiguration("generic", .508, .025, 1500,
                              STEEL_API_5L_X65, PRODUCTION_OIL,
                              water_depth=1000, horizontal_offset=500)


def test_unit_height_ratio(riser):
    # E1-E2 at z/a=1: s=sqrt(3)*a and T=2H. Old model underpredicts.
    r = SimpleCatenaryAnalyzer().analyze_riser(
        replace(riser, water_depth=100), horizontal_tension=100*riser.effective_weight_per_length)
    assert r.arc_length == pytest.approx(173.205080757, abs=1e-9)
    assert r.top_tension == pytest.approx(451404.3943864, abs=1e-6)
    assert r.top_angle == pytest.approx(30, abs=1e-10)


def test_geometry_inverse_and_exhaustion(riser):
    analyzer = SimpleCatenaryAnalyzer()
    r = analyzer.analyze_riser(riser, horizontal_offset=529.9283232381666)
    assert r.converged
    assert r.horizontal_tension == pytest.approx(500000, rel=3e-6)
    _, converged = analyzer._solve_H_from_geometry(1000, 500, 1500, 2257, max_iter=0)
    assert not converged
    with pytest.raises(ValueError, match="available"):
        analyzer.analyze_riser(riser, horizontal_offset=10000)
    too_long = analyzer.analyze_riser(riser, top_tension=5e6)
    assert not too_long.converged


@pytest.mark.parametrize("tension", [0, -1, float("nan"), float("inf"), 2257021])
def test_infeasible_top_tension_raises(riser, tension):
    with pytest.raises(ValueError):
        SimpleCatenaryAnalyzer().analyze_riser(riser, top_tension=tension)


@pytest.mark.parametrize("route", [CatenaryCalculator().calculate, catenaryEquation])
def test_legacy_force_closed_form(route):
    # E1-E2, F=2 MN, w=1000 N/m, d=1000 m, a=1000 m.
    r = route({"F": 2e6, "w": 1000, "d": 1000})
    assert r["S"] == pytest.approx(1732.050807569, abs=1e-9)
    assert r["X"] == pytest.approx(1316.957896925, abs=1e-9)
    assert r["THorizontal"] == 1000000
    assert r["b"] == pytest.approx(.001, abs=1e-15)


@pytest.mark.parametrize("force", [1e6, 0, -1, float("nan"), float("inf")])
def test_legacy_infeasible_force(force):
    with pytest.raises(ValueError):
        CatenaryCalculator().calculate({"F": force, "w": 1000, "d": 1000})


def test_annular_module_and_both_callers(riser):
    module = BuoyancyModule("generic", 100, 1, 500, 100)
    # E4 hand value; issue's one-decimal example differs slightly in rounding.
    assert module.buoyancy_force_per_length(SEAWATER, .3) == pytest.approx(-3680.947171, abs=1e-6)
    coated = replace(riser, outer_diameter=.28, coating_thickness=.01, coating_density=900)
    bare = EffectiveWeightCalculator().calculate(coated).effective_weight
    result = EffectiveWeightCalculator().calculate_with_buoyancy_module(coated, module)
    assert result.effective_weight_with_buoyancy == pytest.approx(bare - 3680.947171, abs=1e-6)
    profile = LazyWaveConfiguration(coated, [module]).get_effective_weight_profile()
    assert profile["generic"] == pytest.approx(bare - 3680.947171, abs=1e-6)
    coated_module = replace(module, outer_coating_thickness=.01, outer_coating_density=1200)
    # Added annulus .01 m: (1200-1025)*9.81*pi/4*(1.02²-1²)=54.472625 N/m.
    assert coated_module.buoyancy_force_per_length(SEAWATER, .3) == pytest.approx(-3626.474546, abs=1e-6)
    with pytest.raises(ValueError):
        module.buoyancy_force_per_length(SEAWATER, 1.1)


@pytest.mark.parametrize("value", [None, 0, -1, float("nan"), float("inf"), -float("inf")])
def test_invalid_report_values_never_pass(value):
    strength = StrengthResult("generic", 0, 1, 1, 1, value)
    check = CodeCheck("generic", "generic", "generic", value)
    fatigue = FatigueResult("generic", 0, value, 25)
    assert strength.status == check.status == fatigue.status == "N/A"
    assert fatigue.fatigue_life_years is None
    report = RiserReportData("generic", "generic", "SCR", 1000,
                             strength_results=[strength], fatigue_results=[fatigue], code_checks=[check])
    assert report.overall_status == "N/A"
    for rendered in (render_html(report), render_markdown(report)):
        assert "N/A" in rendered


def test_fatigue_factor_is_a_cited_input():
    r = FatigueResult("generic", 0, .02, 25)
    assert r.status == "N/A"
    r.design_fatigue_factor = 2
    assert r.status == "N/A"
    r.design_fatigue_factor_basis = "Test input; DNVGL-RP-C203 April 2016 §2.2 eq (2.2.1)"
    assert r.status == "PASS"  # .02 * 25 * 2 = 1
    assert replace(r, design_fatigue_factor=3).status == "FAIL"  # 1.5
    assert replace(r, design_fatigue_factor=float("nan")).status == "N/A"
    assert replace(r, design_life_years=0).status == "N/A"
    for rendered in (render_html(RiserReportData("generic", "generic", "SCR", 1000, fatigue_results=[r])),
                     render_markdown(RiserReportData("generic", "generic", "SCR", 1000, fatigue_results=[r]))):
        assert "DFF" in rendered
        assert r.design_fatigue_factor_basis in rendered
