# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Fail-closed regressions for the high-severity asset-integrity findings.

Synthetic inputs reproduce unsafe dispositions; they do not qualify a method
against a published engineering example.
"""

import math
from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

from digitalmodel.asset_integrity.applicability import (
    Applicability, collect, from_details, merge,
)
from digitalmodel.asset_integrity.assessment import (
    DecisionBands, FFSComponent, FFSDecision, FFSReport, GridParser,
    Level1Screener, Level2Engine, MeasurementSufficiency, assess_component,
    assess_crack_like_flaw, assess_pitting, assess_pitting_level2_equivalent_lta,
    characterize_pit_field, critical_flaw_depth_mm, decide,
    screen_pitting_level1, reduced_mawp,
)
from digitalmodel.asset_integrity.assessment.level3_escalation import (
    escalate_to_level3, should_escalate_to_level3,
)
from digitalmodel.asset_integrity.dent_assessment import assess_dent
from digitalmodel.asset_integrity.pipeline_defect_screen import assess as assess_pipeline


def component(**kwargs):
    return replace(FFSComponent("component", "B31.8", 12, .5, 1000, 52000,
                                corrosion_rate_in_per_yr=.005), **kwargs)


def report(decision, **kwargs):
    return FFSReport.generate_html(pd.DataFrame([[.4]]), decision, "component",
                                   12, .5, .2, "B31.8", 1000, **kwargs)


@pytest.mark.parametrize("field,bad", [
    ("pressure", 0), ("pressure", math.nan), ("od", math.inf),
    ("strength", math.inf), ("wall", math.inf), ("factor", 0),
    ("factor", 1.1), ("wall", 6),
])
def test_A01_invalid_level1_inputs(field, bad):
    data = dict(pressure=1000, od=12, strength=52000, wall=.5, factor=.72)
    data[field] = bad
    with pytest.raises(ValueError):
        Level1Screener("B31.8", data["od"], data["pressure"],
                       smys_psi=data["strength"], b31_design_factor=data["factor"]
                       ).evaluate(data["wall"])


def test_A02_allowable_stress_requires_material_basis():
    with pytest.raises(ValueError, match="allowable_stress"):
        Level1Screener("ASME_VIII_DIV1", 12, 1000, smys_psi=52000).evaluate(.5)
    assert Level1Screener("ASME_VIII_DIV1", 12, 1000,
                          allowable_stress_psi=10000).evaluate(.5)["verdict"] == "FAIL_LEVEL_1"


@pytest.mark.parametrize("bad", [math.nan, math.inf, -math.inf, 0, -1])
def test_A03_incomplete_or_invalid_measurements(bad):
    grid = pd.DataFrame([[.4, bad], [.4, .4]])
    for operation in (
        lambda: assess_component(component(), grid),
        lambda: Level2Engine("GML", 12, .5, .2).evaluate(grid),
        lambda: GridParser.from_dataframe(grid, drop_all_nan_rows=True),
    ):
        with pytest.raises(ValueError, match="readings|measurements"):
            operation()


@pytest.mark.parametrize("bad", ["0.5", True, False, np.bool_(True)])
@pytest.mark.parametrize("mixed", [False, True])
def test_A03_workflow_rejects_raw_nonnumeric_measurements(bad, mixed):
    from dataclasses import asdict
    from digitalmodel.asset_integrity.assessment.ffs_workflow import FFSWorkflow

    grid = [[.5] * 5 for _ in range(5)] if mixed else [[bad] * 5 for _ in range(5)]
    grid[2][2] = bad
    cfg = {"basename": "ffs", "ffs_assessment": {
        "component": asdict(component()), "grid": grid, "force_type": "GML",
    }}
    with pytest.raises(ValueError, match="reading|measurement"):
        FFSWorkflow().router(cfg)
    assert "ffs" not in cfg


@pytest.mark.parametrize("container", [list, tuple])
def test_A03_workflow_retains_qualified_numeric_measurements(container):
    from dataclasses import asdict
    from digitalmodel.asset_integrity.assessment.ffs_workflow import FFSWorkflow

    grid = container(container([.5] * 5) for _ in range(5))
    cfg = {"basename": "ffs", "ffs_assessment": {
        "component": asdict(component()), "grid": grid, "force_type": "GML",
    }}
    result = FFSWorkflow().router(cfg)["ffs"]
    assert result["verdict"] == "ACCEPT"
    assert result["passes"] is True
    assert result["t_measured_min_in"] == pytest.approx(.5)


@pytest.mark.parametrize("required", [0, -1, math.nan, math.inf])
@pytest.mark.parametrize("kind", ["GML", "LML"])
def test_A05_required_wall_is_positive_finite(required, kind):
    with pytest.raises(ValueError, match="t_min"):
        Level2Engine(kind, 12, .5, required).evaluate(pd.DataFrame([[.01]]))


def test_A06_nonphysical_internal_diameter():
    with pytest.raises(ValueError, match="geometry|diameter|OD"):
        Level2Engine("LML", .1, .5, .2).evaluate(pd.DataFrame([[.1], [.1]]))


@pytest.mark.parametrize("coordinates", [["a", "b"], [0], [0, 0], [2, 1],
                                          [0, 1, 100], [0, math.nan]])
def test_A07_axial_coordinates_require_supported_physical_pitch(coordinates):
    with pytest.raises(ValueError, match="spacing|coordinates|pitch"):
        Level2Engine("LML", 12, .5, .2).evaluate(
            pd.DataFrame(.17, index=coordinates, columns=[0]))


@pytest.mark.parametrize("columns", [[0, 1], [0, 100]])
def test_A08_missing_circumferential_check_withholds_clearance(columns):
    result = Level2Engine("LML", 12, .5, .2).evaluate(
        pd.DataFrame(.17, index=[0, 1, 2], columns=columns))
    assert result["verdict"] != "ACCEPT"
    assert not result["applicability"].ok
    assert any("circumferential" in n.lower() for n in result["applicability"].notes)


def test_A09_future_allowance_consumes_ligament():
    grid = pd.DataFrame(np.full((5, 5), .4))
    assert assess_component(component(), grid).level1["t_mm_in"] == .4
    net = assess_component(component(fca_in=.1), grid)
    assert net.level1["t_mm_in"] == pytest.approx(.3)
    assert net.level2["t_mm_in"] == pytest.approx(.3)
    assert net.remaining_life_yr == pytest.approx((.3 - net.t_min_in) / .005)
    for bad in (.49, -.1, math.nan, math.inf):
        with pytest.raises(ValueError, match="fca|FCA"):
            assess_component(component(fca_in=bad), grid)


def test_A10_unsupported_component_geometry():
    with pytest.raises(ValueError, match="component_type"):
        assess_component(component(component_type="unsupported"), np.full((5, 5), .5))


def test_A11_insufficient_measurements_gate_component_clearance():
    # Degraded wall at the boundary requires the field to establish full extent.
    result = assess_component(component(), np.full((4, 4), .4))
    assert result.sufficiency_status == "TAKE_MORE"
    assert not result.passes
    assert result.verdict == "ESCALATE"
    assert "measurement" in result.decision["governing_criterion"].lower()
    assert result.to_dict()["measurement_actions"]
    assert result.to_dict()["governing_criterion"]


def test_A11_workflow_carries_field_actions_and_matches_golden():
    import json
    from pathlib import Path
    import yaml
    from digitalmodel.asset_integrity.assessment.ffs_workflow import FFSWorkflow
    from tests.workflow_api.golden_helpers import deep_approx

    root = Path(__file__).resolve().parents[2]
    cfg = yaml.safe_load((root / "examples/workflows/ffs-metal-loss/input.yml").read_text(encoding="utf-8"))
    value = FFSWorkflow().router(cfg)["ffs"]
    assert value["verdict"] == "ESCALATE" and value["passes"] is False
    assert value["measurement_actions"][0]["count"] == 3
    golden = json.loads((root / "tests/workflow_api/goldens/ffs_metal_loss.json").read_text(encoding="utf-8"))
    deep_approx(value, golden["result"]["value"], rel=1e-6)


def test_A12_missing_rate_is_unknown_and_zero_requires_basis():
    omitted_rate = FFSComponent("component", "B31.8", 12, .5, 1000, 52000)
    result = assess_component(omitted_rate,
                              np.full((5, 5), .5), force_type="GML")
    assert math.isnan(result.remaining_life_yr)
    assert not result.passes
    assert "rate" in result.decision["governing_criterion"].lower()
    with pytest.raises(ValueError, match="basis"):
        FFSDecision._remaining_life(.4, .2, 0)
    qualified = assess_component(component(corrosion_rate_in_per_yr=0,
                                            corrosion_rate_basis="qualified zero rate"),
                                  np.full((5, 5), .5), force_type="GML")
    assert qualified.remaining_life_yr == math.inf and qualified.passes


def test_A13_screening_flag_cannot_override_failed_strength():
    assert decide(.1, .9, 0, "pressure", screening_pass=True).verdict == "REPLACE"


def test_A14_invalid_decision_inputs_and_unknown_life():
    with pytest.raises(ValueError, match="allowable"):
        decide(1, math.nan, 5, "pressure", screening_pass=True)
    with pytest.raises(ValueError, match="rate"):
        FFSDecision.decide("ACCEPT", "ACCEPT", 1, .9, .4, .2, math.nan)
    for bands in (DecisionBands(monitor_band=math.nan), DecisionBands(derate_floor=1),
                  DecisionBands(repair_life_yr=-1)):
        with pytest.raises(ValueError, match="band|floor|life"):
            decide(1, .9, 5, "pressure", bands=bands)
    assert decide(1, .9, math.nan, "pressure", screening_pass=True).verdict == "ESCALATE"
    assert decide(1, .9, math.inf, "pressure", screening_pass=True).verdict == "ESCALATE"


def test_A14_booleans_and_invalid_rerating_limits_are_not_numeric_evidence():
    from digitalmodel.units import Q_

    for args in ((True, .9, 5), (1, True, 5), (1, .9, True)):
        with pytest.raises((TypeError, ValueError)):
            decide(*args, "pressure")
    for bad in (math.inf, math.nan, 0, -1):
        with pytest.raises(ValueError, match="limit"):
            reduced_mawp(Q_(bad, "psi"))


def test_A15_applicability_preserves_independent_rejection():
    app = Applicability(False, ["scope"], ["different check unavailable"])
    result = decide(.1, .9, 0, "pressure", applicability=app)
    assert result.verdict == "REPLACE"
    assert not result.applicability.ok
    assert "unavailable" in result.governing_criterion


@pytest.mark.parametrize("app", [
    Applicability(True, ["UNSUPPORTED"], ["Criterion is unavailable"]),
    Applicability(1), Applicability("true"), Applicability(np.bool_(True)),
])
def test_A16_inconsistent_applicability_cannot_clear_screening(app):
    result = decide(1, .9, 5, "pressure", applicability=app)
    assert result.verdict == "ESCALATE"
    assert result.applicability.ok is False
    assert result.applicability.flags
    assert "engineering review" in result.governing_criterion


@pytest.mark.parametrize("asset_class", ["mooring_chain", "jacket_member"])
def test_A14_unknown_evidence_preserves_structured_escalation(asset_class):
    app = Applicability(False, ["NOT_EVALUATED"], ["Capacity is not established"])
    result = decide(math.nan, math.nan, math.nan, asset_class, applicability=app)
    assert result.verdict == "ESCALATE"
    assert result.applicability.ok is False
    assert math.isnan(result.margin_allowable)
    assert math.isnan(result.remaining_life_yr)
    assert "Capacity is not established" in result.governing_criterion


def test_A14_unknown_allowable_never_calls_derating():
    def derate(margin, allowable):
        pytest.fail("Unknown strength evidence must not enter a rating calculation")

    result = decide(math.nan, math.nan, math.nan, "pressure", derate=derate,
                    applicability=Applicability(False, ["NOT_EVALUATED"]))
    assert result.verdict == "ESCALATE"
    assert result.derated is None


def test_A15_unknown_allowable_preserves_independent_replacement():
    result = decide(.1, math.nan, math.nan, "pressure",
                    applicability=Applicability(False, ["NOT_EVALUATED"]))
    assert result.verdict == "REPLACE"
    assert math.isnan(result.margin_allowable)


def test_A15_pipeline_retains_supported_axial_failure():
    from pathlib import Path
    import yaml

    source = (Path(__file__).resolve().parents[2] / "examples/workflows/"
              "pipeline-corroded-defect-screen/synthetic.yml")
    inputs = yaml.safe_load(source.read_text(encoding="utf-8"))["pipeline_defect_screen"]
    inputs.update(grid=[[.05] * len(row) for row in inputs["grid"]],
                  axial_stress_psi=1e9, length_confirmed=True)
    result = assess_pipeline(inputs)
    assert not result["applicability"]["ok"]
    assert result["decision"]["verdict"] == "DERATE"
    assert result["decision"]["action"] == "REDUCE AXIAL STRESS"
    assert result["decision"]["demand_limits_psi"]["axial_membrane_stress"] > 0


def test_A16_validity_evidence_is_never_reconstructed_as_clear():
    for app in (merge(Applicability(ok=False)), merge(None), collect([object()]),
                from_details({}, flag="scope")):
        assert not app.ok
        assert app.notes


def test_A17_sufficiency_requires_margin_finite_readings_and_thresholds():
    grid = pd.DataFrame(np.full((5, 5), .5))
    args = dict(nominal_wt_in=.5, assessment_type="GML",
                level1_result={"verdict": "ACCEPT"}, level2_result={"rsf": 1, "rsf_a": .9})
    with pytest.raises(ValueError, match="margin"):
        MeasurementSufficiency().evaluate(grid, **args)
    with pytest.raises(ValueError, match="tolerance"):
        MeasurementSufficiency(ut_tolerance_in=math.nan)
    grid.iloc[0, 0] = math.inf
    args["level1_result"]["margin_in"] = .3
    with pytest.raises(ValueError, match="readings|measurements"):
        MeasurementSufficiency().evaluate(grid, **args)


def test_A17_mutated_configuration_is_validated_at_evaluation():
    engine = MeasurementSufficiency()
    engine.ut_tolerance_in = math.nan
    with pytest.raises(ValueError, match="tolerance"):
        engine.evaluate(pd.DataFrame(np.full((5, 5), .5)), nominal_wt_in=.5,
                        assessment_type="GML", level1_result={"verdict": "ACCEPT", "margin_in": .001},
                        level2_result={"rsf": 1, "rsf_a": .9})


def test_A18_unmeasured_pit_population_is_not_intact_wall():
    for grid in (pd.DataFrame([[math.nan]]), pd.DataFrame()):
        with pytest.raises(ValueError, match="readings|measurements|empty"):
            assess_pitting(grid, nominal_od_in=12, nominal_wt_in=.5, t_min_in=.2)


def test_A18_unknown_population_count_is_not_a_measured_field():
    char = characterize_pit_field(pd.DataFrame(np.full((2, 2), .5)), .5)
    with pytest.raises(ValueError, match="count|population"):
        screen_pitting_level1(replace(char, total_valid_cells=math.nan), t_min_in=.2)


def test_A19_equivalent_mean_cannot_erase_deepest_pit():
    grid = pd.DataFrame([[.01, .4, .4, .4, .4], [.5] * 5])
    result = assess_pitting(grid, nominal_od_in=12, nominal_wt_in=.5, t_min_in=.2)
    assert result["level1"]["verdict"] == "FAIL_LEVEL_1"
    assert result["level2"]["verdict"] != "ACCEPT"
    assert not result["level2"]["applicability"].ok
    assert "ligament" in " ".join(result["level2"]["applicability"].notes)


def test_A20_equivalent_pit_grid_preserves_single_row_extent():
    char = characterize_pit_field(pd.DataFrame([[.17, .5], [.5, .5]]), .5)
    char = replace(char, row_spacing_in=10, pitted_region_length_in=10)
    result = assess_pitting_level2_equivalent_lta(
        char, nominal_od_in=12, nominal_wt_in=.5, t_min_in=.2)
    assert result["flaw_length_in"] == pytest.approx(10)
    assert result["equivalent_lta"]["axial_extent_in"] == 10


def test_A20_single_row_with_explicit_physical_pitch():
    char = characterize_pit_field(pd.DataFrame([[.17, .5]]), .5, row_spacing_in=10)
    result = assess_pitting_level2_equivalent_lta(char, nominal_od_in=12,
                                                 nominal_wt_in=.5, t_min_in=.2)
    assert result["flaw_length_in"] == 10


@pytest.mark.parametrize("l2", [{"verdict": "FAIL_LEVEL_2", "rsf": math.nan, "rsf_a": .9},
                                 {"verdict": "FAIL_LEVEL_2", "rsf": .7},
                                 {"verdict": "ACCEPT", "rsf": 1, "rsf_a": .9,
                                  "applicability": {"ok": False, "notes": ["scope"]}}])
def test_A21_failed_or_unknown_results_require_escalation(l2):
    l1 = {"verdict": "FAIL_LEVEL_1"}
    triggered, reasons = should_escalate_to_level3(l1, l2)
    assert triggered and reasons
    handoff = escalate_to_level3({}, {}, {}, l1, l2)
    assert handoff.triggered and "REQUIRED" in handoff.notes


def test_A21_missing_validity_record_requires_escalation():
    triggered, reasons = should_escalate_to_level3(
        {"verdict": "ACCEPT"}, {"verdict": "ACCEPT", "rsf": 1, "rsf_a": .9})
    assert triggered
    assert any("applicability" in reason.lower() for reason in reasons)


@pytest.mark.parametrize("life", [None, math.nan, -math.inf, math.inf])
def test_A22_unknown_life_is_not_unlimited(life):
    text = report({"verdict": "ACCEPT", "rsf": 1, "remaining_life_yr": life})
    assert "Not evaluated" in text
    assert "Unlimited" not in text and "(no corrosion)" not in text


def test_A22_qualified_zero_rate_can_be_displayed_as_unlimited():
    text = report({"verdict": "ACCEPT", "rsf": 1, "remaining_life_yr": math.inf,
                   "remaining_life_basis": "qualified zero-rate assessment"},
                  corrosion_rate_in_per_yr=0)
    assert "Unlimited (qualified zero corrosion rate)" in text


def test_A23_action_cannot_relabel_replacement():
    with pytest.raises(ValueError, match="action|verdict"):
        report({"verdict": "REPLACE", "action": "ACCEPT", "rsf": .1})


def test_A24_embedded_applicability_and_mechanism_verdicts_control_badges():
    text = report({"verdict": "ESCALATE", "rsf": 1, "rsf_a": .9,
                   "applicability": {"ok": False, "flags": ["scope"], "notes": ["unsupported"]}})
    assert "unsupported" in text and "Not evaluated" in text
    assert "<strong>ACCEPT</strong>" not in text
    text = report({"verdict": "REPAIR", "rsf": 1, "rsf_a": .9,
                   "level1": {"verdict": "FAIL_LEVEL_1", "pitting_screen": {
                       "verdict": "FAIL_LEVEL_1", "deep_pit_criterion_pass": False}}})
    assert "<strong>FAIL_LEVEL_1</strong>" in text


@pytest.mark.parametrize("bad", [math.inf, -1, 0, math.nan])
def test_A26_invalid_toughness_never_passes_fracture(bad):
    args = dict(sigma_m_mpa=100, sigma_y_mpa=350, sigma_u_mpa=500, k_mat=bad)
    with pytest.raises(ValueError, match="k_mat"):
        assess_crack_like_flaw(a_mm=2, c_mm=5, t_mm=10, **args)
    with pytest.raises(ValueError, match="k_mat"):
        critical_flaw_depth_mm(length_mm=10, t_mm=10, **args)


@pytest.mark.parametrize("kwargs", [{"dent_length_axial_in": math.nan},
                                     {"strain_limit": math.nan}, {"wt_in": math.inf},
                                     {"dent_length_circ_in": math.inf}])
def test_A28_nonfinite_dent_geometry_or_strain_limit(kwargs):
    args = dict(od_in=12, wt_in=.5, dent_depth_in=.2,
                dent_length_axial_in=5, dent_length_circ_in=5)
    args.update(kwargs)
    with pytest.raises(ValueError):
        assess_dent(**args)


@pytest.mark.parametrize("kwargs", [{}, {"dent_length_axial_in": 5},
                                     {"dent_length_axial_in": 5, "dent_length_circ_in": 5}])
def test_A29_missing_profile_or_feature_confirmations_withhold_clearance(kwargs):
    result = assess_dent(12, .5, .2, **kwargs)
    assert result.verdict == "NEEDS_ASSESSMENT"
    assert result.validity_notes


def test_qualified_valid_arithmetic_is_unchanged():
    l1 = Level1Screener("B31.8", 12, 1000, smys_psi=52000).evaluate(.4)
    assert l1["t_min_in"] == pytest.approx(12000 / (2 * 52000 * .72))
    assert l1["verdict"] == "ACCEPT"
    l2 = Level2Engine("GML", 12, .5, .2).evaluate(pd.DataFrame([[.3, .4]]))
    assert l2["rsf"] == 1 and l2["verdict"] == "ACCEPT"
    dent = assess_dent(12, .5, .2, 10, 10, on_weld=False, restrained=False,
                       with_gouge_or_metal_loss=False)
    assert dent.verdict == "ACCEPT"
