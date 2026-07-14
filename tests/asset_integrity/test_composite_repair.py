# ABOUTME: Tests for composite repair candidate screening and FFS repair joins.
# ABOUTME: Covers ISO 24817 / ASME PCC-2 class filtering without design sizing.
"""Composite repair recommendation tests for issue #1296."""

import json

import pandas as pd

from digitalmodel.asset_integrity.assessment.ffs_decision import FFSDecision
from digitalmodel.asset_integrity.assessment.ffs_coordinator import FFSAssessmentResult
from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport
from digitalmodel.asset_integrity.composite_repair import (
    CompositeRepairParameters,
    RepairContext,
    recommend_composite_repair,
    recommend_from_ffs_result,
)


def _repair_context(**overrides):
    base = {
        "active_leak": False,
        "through_wall_now": False,
        "through_wall_within_repair_life": False,
        "defect_type": "external_metal_loss",
        "service": "oil",
        "pressure_psi": 900.0,
        "temperature_f": 120.0,
        "repair_life_yr": 5.0,
        "axial_load": False,
    }
    base.update(overrides)
    return RepairContext(**base)


def _ffs_result(**overrides):
    base = dict(
        component_id="FFS-REPAIR-1",
        assessment_type="GML",
        level_reached=2,
        t_nominal_in=0.875,
        t_min_in=0.761,
        t_measured_min_in=0.720,
        t_measured_avg_in=0.745,
        fca_in=0.0,
        rsf=0.91,
        rsf_a=0.90,
        folias_factor=1.1,
        remaining_life_yr=1.0,
        verdict="REPAIR",
        rerated_pressure_psi=1480.0,
        sufficiency_status="SUFFICIENT",
    )
    base.update(overrides)
    return FFSAssessmentResult(**base)


def _report_inputs():
    df = pd.DataFrame([[0.420] * 5 for _ in range(5)])
    decision = FFSDecision.decide(
        level1_verdict="ACCEPT",
        level2_verdict="ACCEPT",
        rsf=0.93,
        rsf_a=0.9,
        t_mm_in=0.420,
        t_min_in=0.300,
        corrosion_rate_in_per_yr=0.006,
    )
    return df, decision


def test_through_wall_leak_uses_type_b_and_class_a_only():
    recommendation = recommend_composite_repair(
        _repair_context(active_leak=True, through_wall_now=True)
    )

    assert recommendation.iso_defect_type == "Type B"
    assert recommendation.candidate_classes == ["A"]
    assert set(recommendation.excluded) == {"B", "C", "D", "E"}
    assert recommendation.excluded["C"]
    assert recommendation.reinspection_interval_yr == 2.5
    assert recommendation.summary == (
        "REPAIR \u2014 candidate repair class(es): A, "
        "re-inspection interval 2.5 yr per repair-life design"
    )


def test_non_leaking_axial_metal_loss_candidates_b_and_d():
    recommendation = recommend_composite_repair(_repair_context(axial_load=True))

    assert recommendation.iso_defect_type == "Type A"
    assert recommendation.candidate_classes == ["B", "D"]
    assert "C" in recommendation.excluded
    assert "axial" in recommendation.excluded["C"].lower()


def test_structural_only_defect_candidates_class_e():
    recommendation = recommend_composite_repair(
        _repair_context(
            defect_type="structural_only",
            pressure_psi=0.0,
            axial_load=False,
        )
    )

    assert recommendation.candidate_classes == ["E"]
    assert recommendation.iso_defect_type == "Type A"


def test_steam_service_screens_out_by_default():
    recommendation = recommend_composite_repair(_repair_context(service="steam"))

    assert recommendation.candidate_classes == []
    assert set(recommendation.excluded) == {"A", "B", "C", "D", "E"}
    assert "steam" in recommendation.excluded["C"].lower()
    assert "steam" in recommendation.excluded["D"].lower()
    assert "candidate repair class(es): none" in recommendation.summary


def test_to_dict_is_json_friendly_and_contains_document_level_basis():
    recommendation = recommend_composite_repair(
        _repair_context(active_leak=True, through_wall_within_repair_life=True)
    )
    payload = recommendation.to_dict()

    json.dumps(payload)
    assert payload["candidate_classes"] == ["A"]
    assert payload["standards_basis"] == [
        "ISO 24817 (composite repairs for pipework)",
        "ASME PCC-2, non-metallic composite repair articles",
    ]
    assert payload["reinspection_interval_yr"] == 2.5
    assert "excluded" in payload
    assert "basis" not in payload
    assert sorted(payload) == [
        "candidate_classes",
        "excluded",
        "iso_defect_type",
        "reinspection_interval_yr",
        "standards_basis",
        "summary",
    ]


def test_recommend_from_ffs_result_is_fail_closed_to_repair_verdicts():
    params = CompositeRepairParameters(default_temperature_f=100.0)
    accept = recommend_from_ffs_result(
        _ffs_result(verdict="ACCEPT"),
        _repair_context(),
        params=params,
    )
    repair = recommend_from_ffs_result(
        _ffs_result(verdict="REPAIR"),
        _repair_context(active_leak=True, through_wall_now=True),
        params=params,
    )

    assert accept is None
    assert repair is not None
    assert repair.candidate_classes == ["A"]


def test_repair_recommendation_report_section_is_optional():
    df, decision = _report_inputs()
    stale_recommendation = recommend_composite_repair(
        _repair_context(active_leak=True, through_wall_now=True)
    ).to_dict()
    html = FFSReport.generate_html(
        grid_df=df,
        decision=decision,
        component_id="PIPE-001",
        nominal_od_in=16.0,
        nominal_wt_in=0.500,
        t_min_in=0.300,
        design_code="B31.8",
        design_pressure_psi=1000.0,
        repair_recommendation=stale_recommendation,
    )

    assert "Composite Repair Recommendation" not in html
    assert stale_recommendation["summary"] not in html


def test_repair_recommendation_report_section_and_summary_are_rendered():
    df, decision = _report_inputs()
    recommendation = recommend_composite_repair(
        _repair_context(active_leak=True, through_wall_now=True)
    ).to_dict()
    decision = {
        **decision,
        "verdict": "REPAIR",
        "governing_criterion": "Level 1 FAIL; repair recommended.",
    }

    html = FFSReport.generate_html(
        grid_df=df,
        decision=decision,
        component_id="PIPE-001",
        nominal_od_in=16.0,
        nominal_wt_in=0.500,
        t_min_in=0.300,
        design_code="B31.8",
        design_pressure_psi=1000.0,
        repair_recommendation=recommendation,
    )

    assert "Composite Repair Recommendation" in html
    assert recommendation["summary"] in html
    assert (f"Level 1 FAIL; repair recommended.; {recommendation['summary']}") in html
    assert "2.5 yr" in html
    assert "ISO 24817 (composite repairs for pipework)" in html
    assert "ASME PCC-2, non-metallic composite repair articles" in html
