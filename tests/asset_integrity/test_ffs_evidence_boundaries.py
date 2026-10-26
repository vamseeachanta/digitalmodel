# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Evidence-boundary regressions; synthetic inputs, independent arithmetic.

No published-case qualification is claimed. The equations exercised are data
reduction and the existing linear-life/limit-scaling definitions, not new
standard coefficients. All numeric inputs below are deliberately chosen test
inputs, not engineering acceptance limits. The pit detection threshold is the
existing repository heuristic documented under Characterization defaults in
assessment/pitting.py, rather than a new standard acceptance value.
"""
import math

import pandas as pd
import pytest

from digitalmodel.asset_integrity.assessment.ffs_decision import (
    FFSDecision, decide, reduced_mawp,
)
from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport
from digitalmodel.asset_integrity.assessment.pitting import (
    assess_pitting_level2_equivalent_lta, characterize_pit_field,
    screen_pitting_level1,
)
from digitalmodel.units import Q_


def test_pit_free_readings_are_observations_not_nominal_wall():
    """All readings 0.48 >= 0.9*0.5; no pit population has a mean/minimum."""
    char = characterize_pit_field(pd.DataFrame([[0.48, 0.49]]), 0.5)
    assert char.pit_count == 0
    assert char.t_min_pit_in is None
    assert char.t_avg_pit_in is None
    assert char.measured_min_in == 0.48
    assert char.measured_avg_in == pytest.approx((0.48 + 0.49) / 2)
    assert char.background_median_in == pytest.approx((0.48 + 0.49) / 2)
    assert screen_pitting_level1(char, t_min_in=0.49)["verdict"] == "NOT_EVALUATED"
    l2 = assess_pitting_level2_equivalent_lta(
        char, nominal_od_in=16, nominal_wt_in=0.5, t_min_in=0.49)
    assert l2["verdict"] == "NOT_EVALUATED"
    assert math.isnan(l2["rsf"])


def test_no_valid_measurement_or_background_is_not_synthesized():
    """An empty population has no average; all-pit grids have no background."""
    empty = characterize_pit_field(pd.DataFrame([[float("nan")]]), 0.5)
    assert empty.measured_min_in is None
    assert empty.measured_avg_in is None
    assert empty.background_median_in is None
    all_pits = characterize_pit_field(pd.DataFrame([[0.3, 0.3]]), 0.5)
    assert all_pits.background_median_in is None


def test_single_row_explicit_pitch_and_equivalent_extent():
    """Cell-centred length = one row * 0.4 in; two pits are 2*0.4 in apart.

    Splitting the equivalent region into two half-pitch rows preserves its
    length: 2*(0.4/2) = 0.4 in. No Folias coefficient is asserted here.
    """
    char = characterize_pit_field(
        pd.DataFrame([[0.3, 0.5, 0.3]]), 0.5, row_spacing_in=0.4)
    assert char.pitted_region_length_in == pytest.approx(0.4)
    assert char.pit_spacing_min_in == pytest.approx(0.8)
    l2 = assess_pitting_level2_equivalent_lta(
        char, nominal_od_in=16, nominal_wt_in=0.5, t_min_in=0.35)
    assert l2["flaw_length_in"] == pytest.approx(0.4)
    assert l2["equivalent_lta"]["axial_extent_in"] == pytest.approx(0.4)
    assert l2["equivalent_lta"]["row_spacing_in"] == pytest.approx(0.2)
    assert l2["equivalent_lta"]["source_axial_rows"] == 1


def test_uniform_decimal_index_preserves_pitch_and_equivalent_extent():
    """Four cell-centred rows * 0.1 in pitch = 0.4 in axial extent.

    Decimal subtraction has binary roundoff; no spacing variation is intended.
    """
    char = characterize_pit_field(
        pd.DataFrame([[0.3, 0.5]] * 4, index=[0.0, 0.1, 0.2, 0.3]), 0.5)
    assert char.row_spacing_in == pytest.approx(0.1)
    assert char.spacing_basis == "uniform numeric index"
    assert char.pitted_region_length_in == pytest.approx(0.4)
    l2 = assess_pitting_level2_equivalent_lta(
        char, nominal_od_in=16, nominal_wt_in=0.5, t_min_in=0.35)
    assert l2["verdict"] != "NOT_EVALUATED"
    assert math.isfinite(l2["rsf"])
    assert l2["flaw_length_in"] == pytest.approx(0.4)
    assert l2["equivalent_lta"]["axial_extent_in"] == pytest.approx(0.4)


@pytest.mark.parametrize("rows,pitch", [(60, 0.1), (40, 0.2)])
def test_long_decimal_index_preserves_pitch_extent_and_verdict(rows, pitch):
    """Cell-centred lengths are 60*0.1=6 in and 40*0.2=8 in."""
    from digitalmodel.asset_integrity.assessment.ffs_coordinator import (
        FFSComponent, assess_component,
    )

    grid = pd.DataFrame([[0.3, 0.5]] * rows,
                        index=[i * pitch for i in range(rows)])
    char = characterize_pit_field(grid, 0.5)
    assert char.row_spacing_in == pytest.approx(pitch)
    assert char.spacing_basis == "uniform numeric index"
    assert char.pitted_region_length_in == pytest.approx(rows * pitch)
    component = FFSComponent("SYNTHETIC", "B31.8", 16, 0.5, 1000, smys_psi=52000)
    decimal = assess_component(component, grid, force_type="PITTING")
    explicit = assess_component(component, grid.reset_index(drop=True),
                                force_type="PITTING", row_spacing_in=pitch)
    assert decimal.verdict == explicit.verdict
    assert decimal.rsf == pytest.approx(explicit.rsf)


@pytest.mark.parametrize("index", [
    [0], ["row"], [0, 0], [0, 1, 3],
    [0.0, 0.1, 0.2, 0.30000000001],
    [0.0, 0.1, 0.0, 0.1], [0.3, 0.2, 0.1, 0.0],
])
def test_unknown_or_nonuniform_pitch_is_not_one_inch(index):
    """Neither one coordinate nor unequal gaps establish a uniform cell pitch."""
    char = characterize_pit_field(pd.DataFrame([[0.3]] * len(index), index=index), 0.5)
    assert char.row_spacing_in is None
    assert char.pitted_region_length_in is None
    l2 = assess_pitting_level2_equivalent_lta(
        char, nominal_od_in=16, nominal_wt_in=0.5, t_min_in=0.35)
    assert l2["verdict"] == "NOT_EVALUATED"
    assert math.isnan(l2["rsf"])


@pytest.mark.parametrize("pitch", [0, -1, float("nan"), float("inf"), True])
def test_invalid_explicit_pitch_is_rejected(pitch):
    with pytest.raises(ValueError, match="row_spacing_in"):
        characterize_pit_field(pd.DataFrame([[0.3]]), 0.5, row_spacing_in=pitch)


@pytest.mark.parametrize("life", [None, float("nan")])
def test_missing_life_cannot_select_repair_or_rerating(life):
    """Margin 1 >= caller allowable 0.9, but an independent screen failed.

    Life is absent, so the short-life comparator cannot be evaluated.
    """
    result = decide(1, 0.9, life, "pipeline", screening_pass=False).to_dict()
    assert result["verdict"] == "ESCALATE"
    assert result["remaining_life_yr"] is None
    assert result["rerated_mawp_psi"] is None
    assert "remaining life" in result["governing_criterion"].lower()


def test_current_screen_can_pass_without_a_life_projection():
    result = decide(1, 0.9, None, "pipeline", screening_pass=True).to_dict()
    assert result["verdict"] == "ACCEPT"
    assert result["remaining_life_yr"] is None
    assert "not evaluated" in result["governing_criterion"].lower()


@pytest.mark.parametrize("life", [-1.0, float("-inf"), Q_(-1, "yr"), Q_(float("-inf"), "yr")])
@pytest.mark.parametrize("screening_pass", [None, True, False])
def test_negative_life_is_rejected_before_screening(life, screening_pass):
    """An expired or invalid projection cannot become absent passing evidence."""
    with pytest.raises(ValueError, match="remaining_life_yr.*non-negative"):
        decide(1.0, 0.9, life, "pipeline", screening_pass=screening_pass)


@pytest.mark.parametrize("life, verdict", [(0.0, "REPAIR"), (float("inf"), "ACCEPT")])
def test_zero_and_explicit_unlimited_life_keep_their_meaning(life, verdict):
    """Zero life is exhausted; positive infinity is explicit zero degradation."""
    result = decide(1.0, 0.9, life, "pipeline").to_dict()
    assert result["verdict"] == verdict
    assert result["remaining_life_yr"] == life


def test_missing_rating_does_not_claim_rerating():
    result = decide(0.72, 0.9, 5, "pressure").to_dict()
    assert result["verdict"] == "ESCALATE"
    assert result["rerated_mawp_psi"] is None
    assert "not calculated" in result["governing_criterion"].lower()


def test_calculated_rating_keeps_independent_scale():
    """Existing scale definition: 1000 * (0.72 / 0.9) = 800 psi.

    Source: ffs_decision.reduced_mawp, API 579-1/ASME FFS-1 (2021),
    section 2.4.2.2 (existing implementation; no new code qualification).
    """
    result = decide(0.72, 0.9, None, "pressure",
                    derate=reduced_mawp(Q_(1000, "psi"))).to_dict()
    assert result["verdict"] == "DERATE"
    assert result["rerated_mawp_psi"] == pytest.approx(800)
    assert result["remaining_life_yr"] is None


def test_legacy_missing_rate_is_not_zero_rate():
    result = FFSDecision.decide("ACCEPT", "ACCEPT", 1, 0.9, 0.5, 0.3, None)
    assert result["remaining_life_yr"] is None


@pytest.mark.parametrize("life", [None, float("nan"), float("-inf")])
def test_report_does_not_call_missing_life_unlimited(life):
    section = FFSReport._section_remaining_life(0.5, 0.3, life, None)
    assert "Not evaluated" in section
    assert "Unlimited" not in section
    summary = FFSReport._section_executive_summary(
        "SYNTHETIC", "2026-10-20", "ESCALATE", "grey", "Evidence absent",
        life, "B31.8", 1, 0.9)
    assert "Not evaluated" in summary
    assert "no corrosion" not in summary


@pytest.mark.parametrize("rsf_a", [float("nan"), float("inf"), float("-inf")])
def test_legacy_nonfinite_allowable_renders_as_not_evaluated(rsf_a):
    """A serialized absent comparator cannot establish a Level 2 finding."""
    decision = FFSDecision.decide("ACCEPT", "ACCEPT", 1.0, rsf_a, 0.5, 0.3, None)
    assert decision["verdict"] == "ESCALATE"
    assert decision["rsf"] == 1.0
    assert decision["rsf_a"] is None
    _assert_report_allowable_and_comparison(decision, "Not evaluated", "NOT_EVALUATED")


@pytest.mark.parametrize("rsf_a", [0.0, -0.9])
def test_legacy_nonpositive_allowable_renders_as_not_evaluated(rsf_a):
    """A nonpositive comparator cannot establish acceptance or a pressure rating."""
    decision = FFSDecision.decide("ACCEPT", "ACCEPT", 1.0, rsf_a, 0.5, 0.3, None)
    assert decision["verdict"] == "ESCALATE"
    assert decision["rerated_mawp_psi"] is None
    _assert_report_allowable_and_comparison(decision, "Not evaluated", "NOT_EVALUATED")


@pytest.mark.parametrize("rsf_a", [None, float("nan"), float("inf"), float("-inf")])
def test_report_handles_absent_and_raw_nonfinite_allowable(rsf_a):
    """Both report sections also handle payloads before finite normalization."""
    decision = FFSDecision.decide(
        "ACCEPT", "ACCEPT", 1.0, float("nan"), 0.5, 0.3, None)
    decision["rsf_a"] = rsf_a
    _assert_report_allowable_and_comparison(decision, "Not evaluated", "NOT_EVALUATED")


@pytest.mark.parametrize("rsf,comparison", [(0.8, "FAIL"), (0.9, "ACCEPT"), (1.0, "ACCEPT")])
def test_report_finite_allowable_preserves_format_and_comparison(rsf, comparison):
    """Synthetic RSF below, at and above the supplied comparator keeps its status."""
    decision = FFSDecision.decide("ACCEPT", "ACCEPT", rsf, 0.9, 0.5, 0.3, None)
    _assert_report_allowable_and_comparison(decision, "0.90", comparison)


def _assert_report_allowable_and_comparison(decision, allowable_text, comparison):
    report = FFSReport.generate_html(
        pd.DataFrame([[0.5, 0.5], [0.5, 0.5]]), decision, "SYNTHETIC",
        16, 0.5, 0.3, "B31.8", 1000, assessment_date="2026-10-24")
    summary = report.split("<h2>1. Executive Summary</h2>", 1)[1].split("</table>", 1)[0]
    level2 = report.split("<h2>4. Level 2 Assessment", 1)[1].split("</table>", 1)[0]
    allowable_row = f"<td>Allowable RSF (RSFa)</td><td>{allowable_text}</td>"
    assert allowable_row in summary
    assert allowable_row in level2
    rsf_row = level2.split("<td>Remaining Strength Factor (RSF)</td>", 1)[1].split("</tr>", 1)[0]
    assert f"<td>{decision['rsf']:.4f}</td>" in rsf_row
    assert f"<strong>{comparison}</strong>" in rsf_row
    assert report.endswith("</html>")


def test_disposition_translation_contains_no_physical_tokens():
    from digitalmodel.asset_integrity.assessment.ffs_decision import from_screening

    result = from_screening("MONITOR", "pipeline", "Measured dent comparator").to_dict()
    assert result["verdict"] == "MONITOR"
    for key in ("rsf", "rsf_a", "margin", "margin_allowable", "remaining_life_yr",
                "rerated_mawp_psi", "derated"):
        assert result[key] is None


def test_forced_pit_free_coordinator_reports_observed_wall_and_no_strength():
    from digitalmodel.asset_integrity.assessment.ffs_coordinator import (
        FFSComponent, assess_component,
    )

    component = FFSComponent("SYNTHETIC", "B31.8", 16, 0.5, 1000, smys_psi=52000)
    result = assess_component(component, pd.DataFrame([[0.48] * 4] * 4), force_type="PITTING")
    assert result.t_measured_min_in == 0.48
    assert result.t_measured_avg_in == pytest.approx(0.48)
    assert result.verdict == "ESCALATE"
    assert result.remaining_life_yr is None
    assert result.to_dict()["rsf"] is None
    assert result.to_dict()["rerated_pressure_psi"] is None
    assert "NOT_EVALUATED" in result.decision["governing_criterion"]


def test_legacy_report_retains_both_actual_comparators():
    """Failed minimum-wall screen cannot be described as an RSF failure.

    Synthetic min 0.25 < required 0.3; independently supplied RSF 0.95 >= 0.9.
    """
    result = FFSDecision.decide("FAIL_LEVEL_1", "ACCEPT", 0.95, 0.9, 0.25, 0.3, 0.01)
    assert "measured minimum=0.250 in vs required=0.300 in" in result["governing_criterion"]
    assert "RSF=0.950 vs RSFa=0.900 (ACCEPT)" in result["governing_criterion"]


def test_workflow_forwards_explicit_pitch_without_solver_import(monkeypatch):
    import digitalmodel.asset_integrity.assessment.ffs_workflow as workflow

    captured = {}

    class Result:
        def to_dict(self):
            return {"verdict": "ESCALATE"}

    def assess(component, grid, **kwargs):
        captured.update(kwargs)
        return Result()

    monkeypatch.setattr(workflow, "assess_component", assess)
    cfg = {"basename": "ffs", "ffs_assessment": {
        "component": {"component_id": "SYNTHETIC", "design_code": "B31.8",
                      "nominal_od_in": 16, "nominal_wt_in": 0.5,
                      "design_pressure_psi": 1000},
        "grid": [[0.3]], "force_type": "PITTING", "row_spacing_in": 0.4}}
    workflow.FFSWorkflow().router(cfg)
    assert captured["row_spacing_in"] == 0.4


def test_worked_example_answers_follow_independent_arithmetic():
    """For chosen inputs: L=1*0.4, spacing=2*0.4, pressure=1000*0.72/0.9.

    Missing pit population and life have no physical value. No standard
    acceptance coefficient is introduced by these data-reduction identities.
    """
    import runpy
    from pathlib import Path

    namespace = runpy.run_path(str(Path(__file__).resolve().parents[2] /
                                  "examples/asset_integrity/ffs_evidence_boundaries.py"))
    answer = namespace["run_example"]()
    assert answer["pit_free_observed_min_in"] == 0.48
    assert answer["pit_free_pit_min_in"] is None
    assert answer["axial_extent_in"] == pytest.approx(0.4)
    assert answer["pit_spacing_in"] == pytest.approx(0.8)
    assert answer["calculated_rating_psi"] == pytest.approx(800)
    assert answer["remaining_life_yr"] is None
