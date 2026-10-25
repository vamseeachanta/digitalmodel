"""Fail-closed regressions for the high findings in issue 3279.

Synthetic inputs isolate dispatch and acceptance logic; no solver is invoked.
"""

from copy import deepcopy
from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest

from digitalmodel.asset_integrity.common.API579_components import API579_components
from digitalmodel.asset_integrity.common.BS7910_critical_flaw_limits import BS7910_2013
from digitalmodel.asset_integrity.common.data import AttributeDict
from digitalmodel.asset_integrity.custom.MaterialProperties import MaterialProperties
from digitalmodel.asset_integrity.custom.PipeCapacity import API_TR_5C3, PipeCapacity
from digitalmodel.asset_integrity.custom.PipeCapacity import VonMises_Pipe
from digitalmodel.asset_integrity.common.validation import bracketed_interpolate
from digitalmodel.asset_integrity.riser_joint_ffs import fleet_rollup
from digitalmodel.asset_integrity.riser_joint_report import _governing_placements, _scan_result
from digitalmodel.asset_integrity.safety_case.alarp_demonstration import (
    ALARPThresholds, classify_risk, gross_disproportion_check,
    risk_from_likelihood_consequence,
)


@pytest.mark.parametrize("column,bad", [("min_life_years", np.nan), ("joint_id", None)])
def test_A60_unknown_scan_cannot_qualify_joint(column, bad):
    register = pd.DataFrame({"component": ["Main", "Main"],
                             "joint_id": ["joint", "joint"], "min_life_years": [10., 10.]})
    register.loc[0, column] = bad
    with pytest.raises(ValueError, match="every row|identity"):
        fleet_rollup(register, n_campaigns=1)


def test_A61_incomplete_grid_withholds_placement(monkeypatch):
    import digitalmodel.asset_integrity.riser_joint_report as report
    monkeypatch.setattr(report, "_envelopes", lambda *args: {})
    scan = {"joint_id": "joint", "scan_location": "end", "grid_csv": "grid.csv"}
    spec = dict(od_in=21., grade="X80", campaign_water_depth_ft=10.,
                campaign_years=3., differential_head_fraction=1.)
    register = pd.DataFrame({"joint_id": ["joint"], "component": ["Main"],
                             "scan_location": ["end"], "min_life_years": [10.],
                             "corrosion_rate_mm_per_year": [0.]})
    result, placement = _scan_result(scan, spec, pd.DataFrame([[.5, np.nan], [np.nan, np.nan]]), register)
    assert placement["verdict"] == "NOT_EVALUATED"
    assert not placement["eligible_zones"]
    assert "coverage" in placement["reason"]
    assert result["measured_stations_placement"]["verdict"] == "ACCEPT"


def test_A62_tied_depth_retains_worst_status():
    rows = [dict(joint_id="joint", acceptable_depth_ft=5000., verdict=v)
            for v in ["ACCEPT", "RESTRICTED"]]
    assert _governing_placements(rows)[0]["verdict"] == "RESTRICTED"
    assert _governing_placements(rows[::-1])[0]["verdict"] == "RESTRICTED"


@pytest.mark.parametrize("call", [lambda: classify_risk(np.nan),
    lambda: ALARPThresholds(upper_intolerable=np.nan),
    lambda: risk_from_likelihood_consequence(np.inf, 0),
    lambda: classify_risk(1.1)])
def test_A63_unknown_risk_is_not_tolerable(call):
    with pytest.raises(ValueError):
        call()


@pytest.mark.parametrize("likelihood,consequence,expected", [
    (2.0, .1, .2), (.5, 3.0, 1.5), (.5, .25, .125),
])
def test_A63_frequency_and_expected_fatalities_are_not_probabilities(
        likelihood, consequence, expected):
    # Hand products: 2 * .1 = .2; .5 * 3 = 1.5; .5 * .25 = .125.
    assert risk_from_likelihood_consequence(likelihood, consequence) == pytest.approx(expected)


@pytest.mark.parametrize("likelihood,consequence", [
    (None, .1), ("2", .1), (True, .1), (np.nan, .1), (np.inf, .1),
    (-1., .1), (.5, None), (.5, "3"), (.5, True), (.5, np.nan),
    (.5, np.inf), (.5, -1.), (1e308, 2.),
])
def test_A63_invalid_frequency_or_consequence_remains_rejected(likelihood, consequence):
    with pytest.raises(ValueError):
        risk_from_likelihood_consequence(likelihood, consequence)


def test_A63_expected_fatalities_cannot_be_classified_as_a_probability():
    risk = risk_from_likelihood_consequence(.5, 3.)
    with pytest.raises(ValueError, match="risk"):
        classify_risk(risk)


@pytest.mark.parametrize("values", [(np.inf, 100, 3), (100, np.nan, 3), (1, 2, np.inf)])
def test_A64_undefined_economics_cannot_exclude_measure(values):
    with pytest.raises(ValueError, match="finite"):
        gross_disproportion_check(*values)


def test_A65_incomplete_profile_has_no_averaging_length():
    with pytest.raises(ValueError, match="finite|complete"):
        API579_components({}).GMLAssessmentLengthEvaluation(
            [np.nan, .2, .2], dict(tmin=.2, NominalID=11, RSFa=.9))


def test_A66_local_ligament_failure_suppresses_life_and_pressure():
    with pytest.raises(ValueError, match="local wall"):
        API579_components({}).GMLAcceptability(dict(
            tmm=.01, FCAml=0, tmin=.2, NominalWT=.5, tam=.4, Age=10,
            FutureCorrosionRate=.01))


def test_A67_empty_reading_sets_cannot_complete():
    cfg = AttributeDict(default={"Analysis": {"GML": {"data_source": "inline"}}}, ReadingSets=[])
    with pytest.raises(ValueError, match="reading set"):
        API579_components(cfg).gml()


def test_A68_unbracketed_pressure_has_no_allowable_fca(monkeypatch):
    cfg = {"Design": [{"InternalPressure": {"Outer_Pipe": 2000}}],
           "Analysis": {"result_folder": "", "file_name": "synthetic"}}
    component = API579_components(cfg)
    monkeypatch.setattr(component, "read_gml_grids", lambda: setattr(component, "gml_wt_grids", [pd.DataFrame([[.5]])]))
    monkeypatch.setattr(component, "_generate_plots", lambda: False)
    monkeypatch.setattr(component, "API579GML", lambda *args: ({}, [{"FCA": 0, "MAWP": 1000}, {"FCA": .1, "MAWP": 500}]))
    with pytest.raises(ValueError, match="bracket"):
        component.gml()


def test_A69_circumferential_extent_withholds_overall_disposition(monkeypatch):
    component = API579_components({})
    monkeypatch.setattr(component, "LMLAcceptability", lambda *args: [{"c": 100}])
    rows = component.API579LML(pd.DataFrame([[.3, .3], [.3, .3]], columns=[0, 100]), {}, 0)
    assert rows[0]["Overall qualification"] == "NOT_EVALUATED"
    assert "circumferential" in rows[0]["Overall limitation"]


def test_A72_failed_applicability_cannot_continue():
    stub = SimpleNamespace(a=9, B=10, ri=100, c=10, W=20,
                           stress_intensity_annex="Annex_M_7_2_2",
                           flaw=SimpleNamespace(location="internal_surface"))
    with pytest.raises(ValueError, match="a/B"):
        BS7910_2013.perform_flaw_dimension_acceptance_check(stub)


def test_A73_invalid_width_cannot_use_unit_factor():
    stub = SimpleNamespace(a=1, c=10, W=1, B=10, stress_intensity_annex="Annex_M_4_1")
    with pytest.raises(ValueError, match="width|geometry"):
        BS7910_2013.get_finite_width_correction_factor(stub)


@pytest.mark.parametrize("selection", [None, "constant_values", "unknown"])
def test_A74_solution_must_be_explicit_and_qualified(selection):
    component = object.__new__(BS7910_2013)
    component.cfg = AttributeDict(default={"settings": {"stress_intensity_factor_solution": selection}})
    component.assign_key_flaw_dimensions_for_calculations = lambda: None
    component.perform_flaw_dimension_acceptance_check = lambda: None
    component.get_finite_width_correction_factor = lambda: None
    with pytest.raises(ValueError, match="qualified|solution"):
        component.get_stress_intensity_factor_solution_Annex_M_7_2_2()


def _table_flaw_assessment():
    component = object.__new__(BS7910_2013)
    component.cfg = AttributeDict(
        default={"settings": {"stress_intensity_factor_solution": "table",
                              "scf_bending": 1., "ctod": .1}},
        Outer_Pipe={"Geometry": {"Design_WT": 10. / 25.4}},
        pipe_properties={"geometry": {"Nominal_ID": 200. / 25.4}},
        loading={
            "primary_membrane_stress": {
                "value": 1000., "membrane_factor": 1., "bending_factor": .5},
            "secondary_membrane_stress": {
                "stress_to_yield": {"axial": 0.}, "bending_factor": 0.},
        },
    )
    component.flaw = SimpleNamespace(
        dimensions={"a": 2., "c": 4.}, theta=90,
        geometry="thin_pipe", orientation="axial", location="internal_surface")
    component.material_grade_properties = {"SMYS": 100., "SMUS": 150.}
    component.material_properties = {"E": 200000., "Poissionsratio": .3}
    component.delta_P_m, component.delta_P_b = 1., .5
    # Synthetic coefficients bracket the geometry; no source table is qualified.
    component.stress_intensity_solution_tables = {"Table_M_2": pd.DataFrame(
        [[.1, .25, .05, 90, "Depth", 1., .5],
         [.4, .75, .2, 90, "Depth", 1., .5],
         [.1, .25, .05, 0, "Surface", 2., 1.],
         [.4, .75, .2, 0, "Surface", 2., 1.]],
        columns=["a_over_B", "a_over_c", "B_over_ri", "theta",
                 "theta_location", "Mm", "Mb"])}
    return component


@pytest.mark.parametrize("prior_loads", [None, (100., 50.)])
def test_A75_surface_point_can_govern(prior_loads):
    component = _table_flaw_assessment()
    if prior_loads is not None:
        component.P_m, component.P_b = prior_loads
    component.get_flaw_fad_properties()
    # BS 7910:2013, Annex P, Equation P.18: with a/B=.2, c/B=.4,
    # B/ri=.1, Pm=6.894757 MPa and Pb=3.447379 MPa, reference=10.884957 MPa.
    # Lr=10.884957/100=.108849571; the synthetic surface coefficients give
    # KI=(2*6.894757+3.447379)*sqrt(pi*2)=43.206484 MPa sqrt(mm).
    # The existing toughness method gives 1947.852373 MPa sqrt(mm), so
    # Kr=43.206484/1947.852373=.022181601; zero secondary stress gives rho=0.
    assert component.reference_stress == pytest.approx(10.884957072)
    assert component.L_r == pytest.approx(.108849571)
    assert component.K_r == pytest.approx(.022181601)
    assert component.K_I == pytest.approx(43.206483944)
    # Range=(2*1+1*.5)*sqrt(pi*2)=6.266570687 MPa sqrt(mm).
    assert component.delta_K_I == pytest.approx(6.266570687)
    assert component.governing_crack_front == "free_surface"
    assert set(component.crack_front_results) == {"deep", "free_surface"}
    assert component.crack_front_results["deep"]["K_r"] == pytest.approx(.0110908004)


def test_A75_reassessment_uses_current_loads_for_reference_stress():
    component = _table_flaw_assessment()
    # Prime only prior state: 1000 psi and a .5 bending factor, in MPa.
    component.P_m, component.P_b = 6.894757293168, 3.447378646584
    for pressure, reference, kr in [(1000., 10.884957072, .022181601),
                                    (2000., 21.769914144, .044363202)]:
        component.cfg.loading["primary_membrane_stress"]["value"] = pressure
        component.get_flaw_fad_properties()
        # Doubling both primary stresses doubles reference stress and KI/Kmat.
        assert component.reference_stress == pytest.approx(reference)
        assert component.L_r == pytest.approx(reference / 100.)
        assert component.K_r == pytest.approx(kr)
        assert component.governing_crack_front == "free_surface"


def test_A75_fracture_and_growth_can_have_different_governing_points():
    component = _table_flaw_assessment()
    table = component.stress_intensity_solution_tables["Table_M_2"]
    table.loc[table.theta_location == "Depth", "Mb"] = 2.
    component.delta_P_m, component.delta_P_b = 0., 1.
    component.get_flaw_fad_properties()
    # Deep KI=(Pm+2*Pb)*sqrt(pi*a) < surface KI=(2*Pm+Pb)*sqrt(pi*a).
    # Deep range=2*sqrt(pi*2)=5.013256549; surface range is half that.
    assert component.governing_crack_front == "free_surface"
    assert component.K_r == pytest.approx(.022181601)
    assert component.delta_K_I == pytest.approx(5.013256549)
    assert component.crack_front_results["deep"]["delta_K_I"] == pytest.approx(5.013256549)
    assert component.crack_front_results["free_surface"]["delta_K_I"] == pytest.approx(2.506628275)


@pytest.mark.parametrize("kr", [2., np.nan])
def test_A76_no_allowable_flaw_when_first_row_fails(kr):
    component = object.__new__(BS7910_2013)
    columns = ["geometry", "orientation", "location", "theta", "flaw_depth", "c", "K_r", "L_r", "K_r_allowable"]
    component.cfg = AttributeDict(default={"settings": {"theta_array": [0], "fad_properties_columns": columns, "save_detailed_results": False}})
    component.flaw = SimpleNamespace(dimensions={}, geometry="thin_pipe", orientation="axial", location="internal_surface")
    component.critical_allowable_flaw_df = pd.DataFrame(columns=columns)
    component.get_flaw_fad_properties = lambda: (setattr(component, "K_r", kr), setattr(component, "L_r", 0.1))
    component.get_K_r_allowable = lambda: setattr(component, "K_r_allowable", 1.)
    with pytest.raises(ValueError, match="passing|finite"):
        component.iterate_flaw_length_for_unstable_limits_for_single_c_value(3, 1, 1, 1)
    assert component.critical_allowable_flaw_df.empty


def test_A77_unknown_cycles_are_not_zero_demand():
    component = object.__new__(BS7910_2013)
    histogram = pd.DataFrame({"bins": [0., 10.], "component": [np.nan, 0.]})
    with pytest.raises(ValueError, match="histogram|cycles"):
        component.get_approximate_flaw_growth_solution_for_single_flaw_location(1, 1, 1, histogram, "internal_surface", 1, "axial")


def test_A79_temperature_requires_qualified_factor():
    with pytest.raises(ValueError, match="qualified|temperature"):
        MaterialProperties({}).temperatureDerating({"Code": "ASME B31.8", "temperature": 500})


@pytest.mark.parametrize("design", [[], [{"Code": [], "Load Condition": {"Outer_Pipe": None}}],
    [{"Code": [{"Outer_Pipe": "unused"}], "Load Condition": {"Outer_Pipe": None}}]])
def test_A80_unknown_load_does_not_skip_checks(design):
    with pytest.raises(ValueError, match="load|check|Design"):
        PipeCapacity({"Design": design}).evaluate_load_conditions("Outer_Pipe")


@pytest.mark.parametrize("code,helper", [("API RP 16Q-2017", "API_RP_16Q"), ("30 CFR Part 250", "CFR_30_Part_250")])
def test_A81_dispatch_passes_evaluated_wall(monkeypatch, code, helper):
    import importlib
    module = importlib.import_module("digitalmodel.asset_integrity.custom.PipeCapacity")
    seen = []
    class Helper:
        def __init__(self, cfg):
            seen.append(deepcopy(cfg["Outer_Pipe"]["Geometry"]))
        def get_burst_pressure(self, *args):
            return 1
        def get_burst_minimum_thickness(self, *args):
            return .1
    monkeypatch.setattr(module, helper, Helper)
    cfg = {"Outer_Pipe": {"Geometry": {"Nominal_OD": 12, "Nominal_ID": 11, "Design_WT": .5}}}
    PipeCapacity(cfg).internal_pressure_specification_code_based_evaluation(0, "Outer_Pipe", code, .1)
    assert seen[0]["Design_WT"] == .1
    assert seen[0]["Nominal_ID"] == 11.8
    assert cfg["Outer_Pipe"]["Geometry"]["Design_WT"] == .5


@pytest.mark.parametrize("wall,allowance,expected_minimum,expected_capacity", [
    (1., .5, 1.071428571, 164.089401115),
    (.6, .1, .671428571, 94.824464092),
])
def test_A81_rp1111_internal_pressure_design_uses_each_evaluated_wall(
        wall, allowance, expected_minimum, expected_capacity):
    code = "API RP 1111-2009 Pipelines"
    cfg = {
        "Outer_Pipe": {
            "Geometry": {"Nominal_OD": 12., "Design_WT": wall, "Corrosion_Allowance": allowance},
            "Material": {"SMYS": 1000., "SMUS": 1000.},
        },
        "DesignFactors": {code: {
            "D_over_T_Trasition_Ratio": 20., "internal_pressure": {"Fd": 1.},
        }},
        "Design": [{
            "Code": [{"Outer_Pipe": code}],
            "Load Condition": {"Outer_Pipe": "internal_pressure"},
            "InternalPressure": {"Outer_Pipe": 100.},
            "ExternalPressure": {"Outer_Pipe": 10.},
        }],
    }
    PipeCapacity(cfg).evaluate_load_conditions("Outer_Pipe")
    rows = cfg["Result"]["Outer_Pipe"]["internal_pressure"][code]["design_conditions"]
    # Synthetic factors exercise the existing branches, not equation qualification.
    # D/t = 12/1 = 12 or 12/.6 = 20; corroded D/t = 12/.5 = 24.
    assert [row["thickness"] for row in rows] == [wall, .5]
    assert rows[0]["minimum_thickness"] == pytest.approx(.570975492)
    assert rows[1]["minimum_thickness"] == pytest.approx(expected_minimum)
    assert rows[0]["capacity"] == pytest.approx(expected_capacity)
    assert rows[1]["capacity"] == pytest.approx(78.260869565)
    assert cfg["Outer_Pipe"]["Geometry"]["Design_WT"] == wall


def collapse_component():
    cfg = {"Outer_Pipe": {"Geometry": {"Nominal_OD": 12, "Nominal_ID": 11, "Design_WT": .5}},
           "Design": [{"ExternalPressure": {"Outer_Pipe": 100}, "InternalPressure": {"Outer_Pipe": 0}}]}
    return API_TR_5C3(cfg, "Outer_Pipe", 0)


def test_A82_explicit_pressure_override_is_used():
    component = collapse_component()
    low = component.get_collapse_minimum_thickness(100)
    high = component.get_collapse_minimum_thickness(1000)
    assert high > low
    assert component.get_collapse_pressure(high) == pytest.approx(1000, rel=1e-6)


def test_A83_repeated_conditions_retain_governing_result():
    cfg = {}
    component = PipeCapacity(cfg)
    for minimum, pressure in [(1., 1000.), (.1, 5000.)]:
        component.update_pressure_results(minimum, "Outer_Pipe", pressure, "code", .5, "internal_pressure", "tag")
    result = cfg["Result"]["Outer_Pipe"]["internal_pressure"]["code"]
    assert result["minimum_thickness"]["tag"] == 1.
    assert result["Design_WT_Max_Pressure"]["tag"] == 1000.
    assert len(result["design_conditions"]) == 2


def test_A85_unsolved_inverse_is_not_clamped():
    with pytest.raises(ValueError, match="bracket"):
        collapse_component().get_collapse_minimum_thickness(1e9)


@pytest.mark.parametrize("loader", ["yml_utilities", "ymlInput"])
@pytest.mark.parametrize("overlay", [None, "", "[", "[]"])
def test_A87_requested_overlay_failure_is_fatal(tmp_path, loader, overlay):
    from importlib import import_module
    base = tmp_path / "base.yml"
    base.write_text("pressure: 1000\n", encoding="utf-8", newline="\n")
    update = tmp_path / "update.yml"
    if overlay is not None:
        update.write_text(overlay, encoding="utf-8", newline="\n")
    with pytest.raises(ValueError, match="overlay"):
        import_module("digitalmodel.asset_integrity.common." + loader).ymlInput(base, update)


def test_A88_disabled_checks_replace_stale_success():
    from digitalmodel.asset_integrity.API579 import API579
    from digitalmodel.asset_integrity.fracture_mechanics import fracture_mechanics
    cfg = {"default": {"Analysis": {"GML": {"Circumference": False}, "LML": False, "B31G": False,
           "fracture_mechanics": {"flaw_limits": False}}}, "Result": {"acceptable": True}, "input_revision": "synthetic"}
    for entry in [API579, fracture_mechanics]:
        current = deepcopy(cfg)
        if entry is fracture_mechanics:
            with pytest.warns(RuntimeWarning):
                entry(current)
        else:
            entry(current)
        assert "acceptable" not in current["Result"]
        for status in current["Result"]["assessment_status"].values():
            assert status["status"] == "NOT_EVALUATED"
            assert status["reason"]
            assert status["input_revision"] == "synthetic"


@pytest.mark.parametrize("kwargs", [dict(n_campaigns=0), dict(campaign_years=np.nan),
    dict(high_fatigue_margin=0), dict(min_high_fatigue_joints=-1)])
def test_A60_direct_parameters_require_valid_domains(kwargs):
    register = pd.DataFrame(dict(component=["Main"], joint_id=["joint"], min_life_years=[10.]))
    with pytest.raises(ValueError):
        fleet_rollup(register, **kwargs)


def test_A62_raw_capacity_is_retained_and_governs():
    from digitalmodel.asset_integrity.riser_joint_ffs import place_joint, acceptable_water_depth_ft
    raw = acceptable_water_depth_ft(21, .5, "X80")
    placement = place_joint("joint", 10, .5, od_in=21, grade="X80",
                            campaign_water_depth_ft=raw, campaign_years=3)
    assert placement.acceptable_depth_ft == raw
    rows = [dict(joint_id="joint", verdict="ACCEPT", acceptable_depth_ft=value)
            for value in (5000.04, 5000.01)]
    assert _governing_placements(rows)[0]["acceptable_depth_ft"] == 5000.01


def _explorer_builder():
    import importlib.util
    from pathlib import Path

    path = Path(__file__).resolve().parents[2] / "scripts/ffs/build_riser_joint_explorer.py"
    spec = importlib.util.spec_from_file_location("riser_explorer_builder", path)
    builder = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(builder)
    return builder


def test_A62_committed_explorer_preserves_raw_placement_depths():
    import json

    builder = _explorer_builder()
    html = builder._OUT.read_text(encoding="utf-8")
    marker = "const DATA = "
    start = html.index(marker) + len(marker)
    data, end = json.JSONDecoder().raw_decode(html[start:])
    expected = builder.build_tables()
    rebuild = "Regenerate with: PYTHONPATH=src uv run python scripts/ffs/build_riser_joint_explorer.py"
    assert data["placements"].keys() == expected["placements"].keys(), rebuild
    for key, placement in expected["placements"].items():
        stored = data["placements"][key]
        assert stored["acceptable_depth_ft"] == pytest.approx(
            placement["acceptable_depth_ft"], rel=1e-12, abs=1e-8), rebuild
        assert stored["verdict"] == placement["verdict"], rebuild
        assert stored["zones"] == placement["zones"], rebuild
    # The generator may refresh only embedded numbers; page markup stays fixed.
    template = builder._TEMPLATE
    prefix, suffix = template.split("__DATA__")
    before_geom, after_geom = prefix.split("__GEOM__")
    assert html.startswith(before_geom), rebuild
    assert html[:start].endswith(after_geom), rebuild
    assert html[start + end:] == suffix, rebuild


def test_A62_explorer_writes_utf8_lf_and_posix_diagnostics(monkeypatch, capsys):
    from pathlib import PureWindowsPath

    builder = _explorer_builder()
    written = {}

    class Output:
        def write_text(self, text, **kwargs):
            written.update(kwargs)

        def relative_to(self, root):
            return PureWindowsPath("docs/api/ffs/riser-joint-acceptance-explorer.html")

    monkeypatch.setattr(builder, "_OUT", Output())
    builder.main()
    assert written == {"encoding": "utf-8", "newline": "\n"}
    assert "docs/api/ffs/riser-joint-acceptance-explorer.html" in capsys.readouterr().out


@pytest.mark.parametrize("values", [[], [[]]])
def test_A67_empty_grid_is_rejected_without_metadata_shift(values):
    cfg = AttributeDict(default={"Analysis": {"GML": {"data_source": "inline"}}},
                        ReadingSets=[{"grid": {"values": values}, "Label": "empty", "DataCorrectionFactor": 1}])
    with pytest.raises(ValueError, match="reading set"):
        API579_components(cfg).gml()


@pytest.mark.parametrize("samples,values", [([100, np.nan], [0, .1]),
    ([100, 100], [0, .1]), ([200, 100], [0, .1]), ([100, 200], [.1, np.inf])])
def test_A68_allowance_requires_finite_ordered_bracket(samples, values):
    with pytest.raises(ValueError):
        bracketed_interpolate(150, samples, values)


def test_A68_valid_allowance_interpolation_is_unchanged():
    assert bracketed_interpolate(750, [500, 1000], [.1, 0]) == pytest.approx(.05)


@pytest.mark.parametrize("field,value", [("a", np.nan), ("c", 0), ("ri", np.inf), ("W", 0)])
def test_A72_unknown_geometry_cannot_qualify(field, value):
    stub = SimpleNamespace(a=1, B=10, ri=100, c=10, W=200,
                           stress_intensity_annex="Annex_M_7_2_2",
                           flaw=SimpleNamespace(location="internal_surface"))
    setattr(stub, field, value)
    with pytest.raises(ValueError):
        BS7910_2013.perform_flaw_dimension_acceptance_check(stub)


@pytest.mark.parametrize("annex", ["Annex_M_4_3", "Annex_M_7_3_6"])
def test_A72_embedded_applicability_is_enforced_through_alias(annex):
    stub = SimpleNamespace(a=1, B=10, B_dash=.5, ri=100, c=1, W=20,
                           stress_intensity_annex=annex,
                           flaw=SimpleNamespace(location="embedded"))
    with pytest.raises(ValueError, match="B_dash"):
        BS7910_2013.perform_flaw_dimension_acceptance_check(stub)


@pytest.mark.parametrize("annex", ["Annex_M_7_2_4", "Annex_M_7_3_2"])
def test_A74_constant_only_branches_cannot_qualify(annex):
    with pytest.raises(ValueError, match="not qualified"):
        getattr(object.__new__(BS7910_2013), "get_stress_intensity_factor_solution_" + annex)()


@pytest.mark.parametrize("kr,expected", [([.1, .2, .3], .3), ([.1, 1., .2], .1), ([.1], .1)])
def test_A76_last_passing_row_respects_first_failure(kr, expected):
    frame = pd.DataFrame(dict(K_r=kr, L_r=[.1] * len(kr), K_r_allowable=[1.] * len(kr)))
    assert BS7910_2013._last_passing_fad_row(frame).K_r == expected


@pytest.mark.parametrize("bins,counts,factor,life", [([], [], 1, 1), ([0], [0], 1, 1),
    ([10, 0], [1, 0], 1, 1), ([0, 10], [-1, 0], 1, 1), ([0, 10], [1, 2], 1, 1),
    ([0, 10], [1, 0], 0, 1), ([0, 10], [1, 0], 1, 0)])
def test_A77_histogram_coverage_and_factors_are_required(bins, counts, factor, life):
    with pytest.raises(ValueError):
        BS7910_2013._validate_histogram(pd.DataFrame(dict(bins=bins, cycles=counts)), 1, factor, life, 1)


def test_A77_confirmed_zero_demand_is_explicit():
    result = BS7910_2013._validate_histogram(pd.DataFrame(dict(bins=[0, 10], cycles=[0, 0])), 1, 1, 1, 1)
    assert result["status"] == "CONFIRMED_ZERO_DEMAND"
    assert result["criterion"]


def test_A79_explicit_factor_retains_qualified_basis():
    data = dict(Code="ASME B31.8", temperature=50, temperature_derating=.8,
                temperature_derating_basis="synthetic qualified factor at stated temperature")
    assert MaterialProperties({}).temperatureDerating(data) == data


def test_A81_external_dispatch_uses_reduced_wall():
    component = collapse_component()
    component.cfg['Design'][0]['ExternalPressure']['Outer_Pipe'] = 10
    capacity = PipeCapacity(component.cfg)
    _, reduced = capacity.external_pressure_specification_code_based_evaluation(0, "Outer_Pipe", "API TR 5C3-2018", .1)
    assert reduced == component.get_collapse_pressure(.1)
    assert reduced < component.get_collapse_pressure(.5)


def test_A82_failed_override_preserves_demand_basis():
    component = collapse_component()
    before = component.get_collapse_minimum_thickness()
    with pytest.raises(ValueError):
        component.get_collapse_minimum_thickness(1e9)
    assert component.get_collapse_minimum_thickness() == before


@pytest.mark.parametrize("inverse", ["get_burst_minimum_thickness", "get_burst_pressure"])
def test_A85_von_mises_unbracketed_demand_is_rejected(inverse):
    component = object.__new__(VonMises_Pipe)
    component.P_i, component.t = 100, .5
    component.loads = {"allowable_stress_to_yield_ratio": 1}
    component.section_properties = {"SMYS": 1}
    component.get_stress = lambda: 100 + 1 / component.t + component.P_i
    with pytest.raises(ValueError, match="bracket"):
        getattr(component, inverse)()
    assert component.t == .5
    assert component.P_i == 100


@pytest.mark.parametrize("loader", ["yml_utilities", "ymlInput"])
def test_A87_absent_overlay_and_valid_merge_preserve_inputs(tmp_path, loader):
    from importlib import import_module
    load = import_module("digitalmodel.asset_integrity.common." + loader).ymlInput
    base, update = tmp_path / "base.yml", tmp_path / "update.yml"
    base.write_text("pressure: 1000\n", encoding="utf-8", newline="\n")
    update.write_text("pressure: 2000\n", encoding="utf-8", newline="\n")
    assert load(base, None) == {"pressure": 1000}
    assert load(base, update) == {"pressure": 2000}
