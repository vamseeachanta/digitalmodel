# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic regressions separating observations, unknowns and routing tokens."""

import math
import json

import numpy as np
import pandas as pd
import pytest

from digitalmodel.asset_integrity.applicability import Applicability
from digitalmodel.asset_integrity.assessment.ffs_coordinator import (
    FFSComponent,
    assess_component,
)
from digitalmodel.asset_integrity.assessment.ffs_decision import (
    FFSDecision,
    Verdict,
    decide,
    reduced_mawp,
)
from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport
from digitalmodel.units import Q_


def _report(decision, rate=None):
    return FFSReport.generate_html(
        pd.DataFrame(np.full((3, 3), 0.42)), decision, "PIPE", 16.0, 0.5,
        0.3, "B31.8", 1000.0, corrosion_rate_in_per_yr=rate,
    )


@pytest.mark.parametrize("life", [None, math.nan, -math.inf, math.inf])
def test_unknown_or_token_life_is_not_reported_as_no_corrosion(life):
    report = _report({"verdict": "ESCALATE", "remaining_life_yr": life})
    assert "no corrosion" not in report
    assert "Unlimited" not in report
    assert report.count("Not evaluated") >= 2


def test_absent_life_is_not_reported_as_no_corrosion():
    report = _report({"verdict": "ESCALATE"})
    assert "no corrosion" not in report
    assert "Unlimited" not in report
    assert report.count("Not evaluated") >= 2


def test_explicit_zero_rate_projection_remains_distinct_from_unknown_life():
    report = _report({"verdict": "ACCEPT", "remaining_life_yr": math.inf}, 0.0)
    assert "no corrosion" in report
    assert "Unlimited (no further corrosion assumed)" in report


@pytest.mark.parametrize("life", [math.nan, -math.inf])
@pytest.mark.parametrize("asset_class", [
    "pressure", "pipeline", "tank", "mooring_chain", "hull_plating", "jacket_member",
])
def test_failed_screen_with_unknown_life_cannot_infer_a_rerating(life, asset_class):
    decision = decide(0.95, 0.9, life, asset_class, screening_pass=False)
    assert decision.verdict == Verdict.ESCALATE
    assert decision.derated is None
    assert "0.950" in decision.governing_criterion
    assert "0.90" in decision.governing_criterion
    assert "not evaluated" in decision.governing_criterion.lower()


def test_explicit_strength_screen_does_not_need_a_life_token():
    decision = decide(0.96, 0.9, math.nan, "pipeline", screening_pass=True)
    assert decision.verdict == Verdict.ACCEPT
    assert math.isnan(decision.remaining_life_yr)
    assert decision.to_dict()["rerated_mawp_psi"] is None


def test_omitted_corrosion_rate_does_not_establish_unlimited_life():
    component = FFSComponent("PIPE", "B31.8", 16.0, 0.5, 1000.0, smys_psi=52000)
    result = assess_component(component, np.full((3, 3), 0.42))
    assert math.isnan(result.remaining_life_yr)
    assert result.to_dict()["remaining_life_yr"] is None
    json.dumps(result.to_dict(), allow_nan=False)
    assert "Unlimited" not in _report(result.decision)


def test_wall_at_required_minimum_has_zero_life_even_without_a_rate():
    decision = FFSDecision.decide(
        "FAIL_LEVEL_1", "ACCEPT", 0.95, 0.9, 0.30, 0.30, None,
    )
    assert decision["remaining_life_yr"] == 0.0
    assert decision["verdict"] == "REPAIR"


def test_legacy_adapter_identifies_measured_wall_comparator():
    decision = FFSDecision.decide(
        "FAIL_LEVEL_1", "ACCEPT", 0.95, 0.9, 0.29, 0.30, 0.01,
    )
    criterion = decision["governing_criterion"]
    assert "t_mm=0.290" in criterion
    assert "t_min=0.300" in criterion
    assert decision["rerated_mawp_psi"] is None


@pytest.mark.parametrize("missing_operand", ["t_mm_in", "t_min_in"])
@pytest.mark.parametrize("missing", [math.nan, math.inf, -math.inf])
@pytest.mark.parametrize("rate", [None, 0.01, 0.0])
@pytest.mark.parametrize("screen", ["FAIL_LEVEL_1", "ACCEPT"])
@pytest.mark.parametrize("strength", [0.30, 0.85, 0.95])
def test_legacy_wall_comparison_requires_two_finite_operands(
    missing_operand, missing, rate, screen, strength,
):
    walls = {"t_mm_in": 0.29, "t_min_in": 0.30, missing_operand: missing}
    decision = FFSDecision.decide(
        screen, "ACCEPT", strength, 0.9, corrosion_rate_in_per_yr=rate, **walls,
        design_pressure_psi=1000.0,
    )
    criterion = decision["governing_criterion"]
    assert decision["verdict"] == "ESCALATE"
    assert math.isnan(decision["remaining_life_yr"])
    assert decision["rerated_mawp_psi"] is None
    assert "Measured wall comparison: Not evaluated" in criterion
    assert " in < " not in criterion
    assert " in >= " not in criterion
    missing_label = "t_mm" if missing_operand == "t_mm_in" else "t_min"
    assert f"{missing_label}=Not evaluated" in criterion
    available = "t_min=0.300 in" if missing_operand == "t_mm_in" else "t_mm=0.290 in"
    assert available in criterion
    assert "Measured wall comparison: Not evaluated" in _report(decision)
    assert "Unlimited" not in _report(decision)


@pytest.mark.parametrize("measured, comparator", [
    (0.29, "<"), (0.30, ">="), (0.42, ">="),
])
def test_finite_wall_comparison_retains_its_ordering(measured, comparator):
    decision = FFSDecision.decide(
        "FAIL_LEVEL_1", "ACCEPT", 0.95, 0.9, measured, 0.30, None,
    )
    assert (
        f"Measured wall t_mm={measured:.3f} in {comparator} t_min=0.300 in"
        in decision["governing_criterion"]
    )


def test_forced_pitting_without_observations_has_no_wall_comparison():
    component = FFSComponent("PIPE", "B31.8", 16.0, 0.5, 1000.0, smys_psi=52000)
    result = assess_component(component, np.full((3, 3), math.nan), force_type="PITTING")
    criterion = result.decision["governing_criterion"]
    assert result.verdict == "ESCALATE"
    assert result.decision["rerated_mawp_psi"] is None
    assert math.isnan(result.t_measured_min_in)
    assert "Measured wall comparison: Not evaluated" in criterion
    assert "t_mm=Not evaluated" in criterion
    assert f"t_min={result.t_min_in:.3f} in" in criterion
    assert " in < " not in criterion
    assert " in >= " not in criterion
    assert "Measured wall comparison: Not evaluated" in _report(result.decision)
    assert result.applicability.to_dict() == result.decision["applicability"]
    assert "ffs.wall_not_evaluated" in result.applicability.flags
    assert "pitting.no_pits" in result.applicability.flags
    assert result.to_dict()["applicability"] == result.decision["applicability"]


@pytest.mark.parametrize("rate", [math.nan, math.inf, -math.inf, -0.01])
@pytest.mark.parametrize("measured", [0.29, 0.42])
def test_invalid_corrosion_rate_cannot_select_an_action_from_life(rate, measured):
    decision = FFSDecision.decide(
        "FAIL_LEVEL_1", "ACCEPT", 0.95, 0.9, measured, 0.30, rate,
        design_pressure_psi=1000.0,
    )
    assert decision["verdict"] == "ESCALATE"
    assert math.isnan(decision["remaining_life_yr"])
    assert decision["rerated_mawp_psi"] is None
    assert "finite and non-negative" in decision["governing_criterion"]
    assert "Unlimited" not in _report(decision)


def test_missing_wall_retains_existing_applicability_evidence():
    original = Applicability(ok=False, flags=["method.limit"], notes=["Synthetic method limit"])
    decision = FFSDecision.decide(
        "ACCEPT", "ACCEPT", 0.95, 0.9, math.nan, 0.30, 0.01,
        applicability=original,
    )
    assert decision["applicability"]["flags"] == ["method.limit", "ffs.wall_not_evaluated"]
    assert decision["applicability"]["notes"][0] == "Synthetic method limit"
    assert original.flags == ["method.limit"]


@pytest.mark.parametrize("rate", [math.nan, math.inf, -math.inf, -0.01])
def test_coordinator_preserves_invalid_rate_escalation_evidence(rate):
    component = FFSComponent(
        "PIPE", "B31.8", 16.0, 0.5, 1000.0, smys_psi=52000,
        corrosion_rate_in_per_yr=rate,
    )
    result = assess_component(component, np.full((3, 3), 0.42), force_type="GML")
    assert result.verdict == "ESCALATE"
    assert math.isnan(result.remaining_life_yr)
    assert math.isnan(result.rerated_pressure_psi)
    assert "ffs.corrosion_rate_not_evaluated" in result.applicability.flags
    assert result.applicability.to_dict() == result.decision["applicability"]
    assert result.to_dict()["applicability"] == result.decision["applicability"]


def test_unknown_life_escalation_suppresses_a_supplied_pressure_scaling():
    decision = decide(
        0.95, 0.9, math.nan, "pressure", screening_pass=False,
        derate=reduced_mawp(Q_(1000, "psi")),
    )
    assert decision.verdict == Verdict.ESCALATE
    assert decision.to_dict()["rerated_mawp_psi"] is None


def test_report_uses_the_rate_basis_that_produced_the_decision():
    decision = FFSDecision.decide(
        "ACCEPT", "ACCEPT", 0.96, 0.9, 0.42, 0.3, 0.0,
    )
    assert "Unlimited (no further corrosion assumed)" in _report(decision)


def test_report_rejects_a_rate_that_conflicts_with_the_decision_basis():
    decision = FFSDecision.decide(
        "ACCEPT", "ACCEPT", 0.96, 0.9, 0.42, 0.3, 0.01,
    )
    with pytest.raises(ValueError, match="corrosion rate"):
        _report(decision, 0.0)


def test_unknown_strength_has_no_numeric_comparison_verdict():
    report = _report({"verdict": "ESCALATE", "rsf": math.nan, "rsf_a": 0.9})
    assert "<strong>NOT_EVALUATED</strong>" in report


def test_nullable_strength_summary_can_be_reported_without_invented_values():
    report = _report({"verdict": "ESCALATE", "rsf": None, "rsf_a": None})
    assert "<strong>NOT_EVALUATED</strong>" in report
    assert "nan" not in report


def test_report_does_not_invent_folias_or_level2_verdict_from_a_final_token():
    report = _report({"verdict": "REPAIR"})
    assert "Factor (M_t)</td><td>Not evaluated</td>" in report
    assert "Level 2 Verdict</td><td colspan='2'><strong>Not evaluated" in report


def test_coordinator_report_retains_calculated_level2_evidence():
    component = FFSComponent("PIPE", "B31.8", 16.0, 0.5, 1000.0, smys_psi=52000)
    grid = np.full((3, 3), 0.5)
    grid[1, 1] = 0.2
    result = assess_component(component, grid, force_type="LML")
    assert result.decision["folias_factor"] == result.level2["folias_factor"]
    assert result.decision["level2_verdict"] == result.level2["verdict"]


def test_forced_pit_free_coordinator_exposes_no_strength_or_rating():
    component = FFSComponent("PIPE", "B31.8", 16.0, 0.5, 1000.0, smys_psi=52000)
    result = assess_component(component, np.full((3, 3), 0.46), force_type="PITTING")
    assert result.verdict == "ESCALATE"
    assert math.isnan(result.rsf)
    assert math.isnan(result.rerated_pressure_psi)
    assert result.to_dict()["rsf"] is None
    assert result.to_dict()["rerated_pressure_psi"] is None
    assert result.repair_recommendation is None
    assert result.level_reached == 0
    assert result.level1["verdict"] == "NOT_EVALUATED"
    assert "Level 1=FAIL_LEVEL_1" not in result.decision["governing_criterion"]


def test_coordinator_accepts_physical_pitch_for_a_single_row():
    component = FFSComponent("PIPE", "B31.8", 16.0, 0.5, 1000.0, smys_psi=52000)
    result = assess_component(
        component, np.array([[0.2, 0.5, 0.2]]), force_type="PITTING",
        row_spacing_in=10.0,
    )
    assert "pitting.row_spacing_assumed" not in result.applicability.flags
    assert result.level2["flaw_length_in"] == pytest.approx(10.0)


def test_pitting_workflow_preserves_explicit_pitch_provenance():
    from digitalmodel.asset_integrity.assessment.pitting_workflow import router

    result = router({
        "basename": "api579_pitting_screen", "pitting_assessment": {
            "grid": [[0.3, 0.5, 0.3]], "row_spacing_in": 10.0,
            "nominal_od_in": 24.0, "nominal_wt_in": 0.5, "t_min_in": 0.35,
        },
    })["api579_pitting_screen"]
    assert result["characterization"]["row_spacing_source"] == "explicit"
    assert result["level2"]["equivalent_lta"]["row_spacing_in"] == 10.0
    assert result["level2"]["equivalent_lta"]["axial_rows"] == 1
    assert "pitting.row_spacing_assumed" not in result["decision"]["applicability"]["flags"]


def test_ffs_workflow_forwards_explicit_pitting_pitch():
    from digitalmodel.asset_integrity.assessment.ffs_workflow import FFSWorkflow

    result = FFSWorkflow().router({
        "basename": "ffs", "ffs_assessment": {
            "component": {
                "component_id": "PIPE", "design_code": "B31.8",
                "nominal_od_in": 16.0, "nominal_wt_in": 0.5,
                "design_pressure_psi": 1000.0, "smys_psi": 52000,
            },
            "grid": [[0.2, 0.5, 0.2]], "force_type": "PITTING",
            "row_spacing_in": 10.0,
        },
    })["ffs"]
    assert result["verdict"] != "ESCALATE"
    assert "applicability" not in result


@pytest.mark.parametrize("rows", [1, 2, 4])
def test_lml_coordinator_preserves_supplied_physical_pitch(rows):
    component = FFSComponent("PIPE", "B31.8", 16.0, 0.5, 1000.0, smys_psi=52000)
    result = assess_component(
        component, np.full((rows, 1), 0.2), force_type="LML", row_spacing_in=10.0,
    )
    assert result.level2["flaw_length_in"] == pytest.approx(rows * 10.0)
    assert result.t_measured_min_in == 0.2


def test_single_row_without_flaw_candidates_retains_a_full_physical_pitch():
    component = FFSComponent("PIPE", "B31.8", 16.0, 0.5, 2300.0, smys_psi=52000)
    single = assess_component(
        component, np.array([[0.46, 0.46]]), force_type="LML", row_spacing_in=10.0,
    )
    reference = assess_component(
        component, np.full((2, 2), 0.46), force_type="LML", row_spacing_in=10.0,
    )
    assert single.level2["flaw_length_in"] == 10.0
    assert single.rsf == pytest.approx(reference.rsf)
    # ASME B31.8-2022, §841.1.1: t_min = P*OD/(2*SMYS*F*E*T).
    # 2300*16/(2*52000*0.72*1*1) = 0.491452991 in; D = 15.017094017 in.
    # API 579-1/ASME FFS-1 (2021), Part 5 §5.4, Eq. (5.13) is the
    # RSF basis; Part 4 §4.4, Table 4.4 is the bulging-factor basis.
    # Hand evaluation of the retained simplified approximation (not full
    # Part 5 qualification): Rt = 0.46/t_min = 0.936;
    # lambda = 1.285*10/sqrt(D*t_min) = 4.730087627;
    # Mt = sqrt(1 + 0.48*lambda**2) = 3.426279309;
    # RSF = 0.936/(1 - (1 - 0.936)/Mt) = 0.953816485.
    assert single.t_min_in == pytest.approx(0.491452991, rel=0, abs=1e-9)
    assert single.level2["rt"] == pytest.approx(0.936, rel=0, abs=1e-9)
    assert single.level2["lambda"] == pytest.approx(4.730087627, rel=0, abs=1e-9)
    assert single.level2["folias_factor"] == pytest.approx(3.426279309, rel=0, abs=1e-9)
    assert single.rsf == pytest.approx(0.953816485, rel=0, abs=1e-9)


@pytest.mark.parametrize("pitch", [0.0, -1.0, math.nan, math.inf, True, "10"])
def test_level2_explicit_pitch_requires_physical_input(pitch):
    from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine

    with pytest.raises(ValueError, match="row_spacing_in"):
        Level2Engine("LML", 16.0, 0.5, 0.3, row_spacing_in=pitch)


def test_single_row_lml_without_pitch_exposes_the_geometry_gap():
    component = FFSComponent("PIPE", "B31.8", 16.0, 0.5, 1000.0, smys_psi=52000)
    result = assess_component(component, np.array([[0.2, 0.5]]), force_type="LML")
    assert result.verdict == "ESCALATE"
    assert "ffs.row_spacing_assumed" in result.applicability.flags
    assert result.decision["rerated_mawp_psi"] is None


def test_gml_folias_placeholder_is_not_reported_as_a_calculation():
    component = FFSComponent("PIPE", "B31.8", 16.0, 0.5, 1000.0, smys_psi=52000)
    result = assess_component(component, np.full((3, 3), 0.42), force_type="GML")
    assert result.decision["folias_factor"] is None
    assert result.to_dict()["folias_factor"] is None
    assert "Factor (M_t)</td><td>Not evaluated</td>" in _report(result.decision)


def test_absent_allowable_is_not_inferred_from_default_policy():
    report = _report({"verdict": "ESCALATE"})
    assert "Allowable RSF (RSFa)</td><td>Not evaluated</td>" in report
    assert "#757575" in report


def test_pit_free_high_allowance_does_not_create_a_deep_pit_finding():
    from digitalmodel.asset_integrity.assessment.pitting_workflow import router

    result = router({
        "basename": "api579_pitting_screen", "pitting_assessment": {
            "grid": [[0.46, 0.46]], "row_spacing_in": 1.0,
            "nominal_od_in": 24.0, "nominal_wt_in": 0.5, "t_min_in": 0.35,
            "fca_in": 0.4,
        },
    })["api579_pitting_screen"]
    assert result["decision"]["verdict"] == "ESCALATE"
    assert "pitting.deep_ligament" not in result["decision"]["applicability"]["flags"]
    assert "deepest ligament after FCA is below" not in result["report_html"]
