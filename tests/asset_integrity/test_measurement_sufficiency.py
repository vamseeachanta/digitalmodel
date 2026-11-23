# ABOUTME: Tests for the FFS measurement-sufficiency engine (sufficient / take
# ABOUTME: more / escalate) — one test per decision branch + metrics.
"""Tests for digitalmodel.asset_integrity.assessment.measurement_sufficiency."""

import pandas as pd
import pytest

from digitalmodel.asset_integrity.assessment.measurement_sufficiency import (
    MeasurementSufficiency,
    SufficiencyAction,
    SufficiencyResult,
)

NOMINAL = 0.50  # inches


# --- helpers ---------------------------------------------------------------
def adequate_grid():
    """5x5 grid, interior minimum, not degraded, low scatter (>=15 readings)."""
    df = pd.DataFrame([[0.47] * 5 for _ in range(5)])
    df.iat[2, 2] = 0.46          # interior unique minimum, > 0.45 (not degraded)
    return df


def l1(verdict="ACCEPT", t_min=0.30, margin=0.16):
    return {"verdict": verdict, "t_min_in": t_min, "margin_in": margin}


def l2(rsf=0.95, rsf_a=0.90, t_mm=0.46):
    return {"rsf": rsf, "rsf_a": rsf_a, "t_mm_in": t_mm}


@pytest.fixture
def engine():
    return MeasurementSufficiency()


def _actions(result):
    return {a.action for a in result.actions}


# --- SUFFICIENT ------------------------------------------------------------
def test_sufficient_when_adequate_and_passing(engine):
    res = engine.evaluate(
        adequate_grid(), nominal_wt_in=NOMINAL, assessment_type="GML",
        level1_result=l1(), level2_result=l2(),
    )
    assert isinstance(res, SufficiencyResult)
    assert res.status == "SUFFICIENT"
    assert res.confidence == "high"
    assert res.actions == []


def test_sufficient_medium_confidence_thin_margin(engine):
    # Margin above UT tolerance (no take-more) but below 2x tolerance -> medium.
    res = engine.evaluate(
        adequate_grid(), nominal_wt_in=NOMINAL, assessment_type="GML",
        level1_result=l1(margin=0.03), level2_result=l2(),
    )
    assert res.status == "SUFFICIENT"
    assert res.confidence == "medium"


# --- TAKE_MORE -------------------------------------------------------------
def test_take_more_too_few_readings(engine):
    small = pd.DataFrame([[0.47] * 3 for _ in range(3)])  # 9 < 15
    res = engine.evaluate(
        small, nominal_wt_in=NOMINAL, assessment_type="GML",
        level1_result=l1(), level2_result=l2(),
    )
    assert res.status == "TAKE_MORE"
    assert res.confidence == "low"
    assert "add_readings" in _actions(res)
    add = next(a for a in res.actions if a.action == "add_readings")
    assert add.count == 15 - 9


def test_take_more_high_cov_gml(engine):
    # Interior low-thickness cluster -> high COV, non-uniform GML.
    df = pd.DataFrame([[0.50] * 4 for _ in range(4)])
    for r, c in [(1, 1), (1, 2), (2, 1), (2, 2)]:
        df.iat[r, c] = 0.30
    res = engine.evaluate(
        df, nominal_wt_in=NOMINAL, assessment_type="GML",
        level1_result=l1(), level2_result=l2(),
    )
    assert res.status == "TAKE_MORE"
    assert "densify_readings" in _actions(res)
    assert res.metrics["cov"] > 0.10


def test_take_more_lml_underresolved(engine):
    # LML with only 2 degraded cells -> profile under-sampled.
    df = pd.DataFrame([[0.48] * 4 for _ in range(4)])
    df.iat[1, 1] = 0.40
    df.iat[2, 2] = 0.40
    res = engine.evaluate(
        df, nominal_wt_in=NOMINAL, assessment_type="LML",
        level1_result=l1(), level2_result=l2(),
    )
    assert res.status == "TAKE_MORE"
    assert "densify_readings" in _actions(res)
    assert res.metrics["profile_cells"] == 2


def test_take_more_minimum_on_edge(engine):
    # Degraded minimum on the grid boundary -> extend coverage.
    df = pd.DataFrame([[0.48] * 4 for _ in range(4)])
    df.iat[0, 0] = 0.40   # boundary, degraded (< 0.45)
    res = engine.evaluate(
        df, nominal_wt_in=NOMINAL, assessment_type="GML",
        level1_result=l1(), level2_result=l2(),
    )
    assert res.status == "TAKE_MORE"
    assert "extend_coverage" in _actions(res)
    assert res.metrics["min_on_edge"] is True
    assert res.metrics["min_degraded"] is True


def test_take_more_verdict_within_ut_tolerance(engine):
    # Pass margin smaller than UT tolerance -> remeasure the min cell.
    res = engine.evaluate(
        adequate_grid(), nominal_wt_in=NOMINAL, assessment_type="GML",
        level1_result=l1(margin=0.01), level2_result=l2(),
    )
    assert res.status == "TAKE_MORE"
    assert "remeasure_min_cell" in _actions(res)
    rm = next(a for a in res.actions if a.action == "remeasure_min_cell")
    assert rm.count == 3


def test_uniform_grid_does_not_false_flag_edge(engine):
    # A truly uniform, healthy grid: min is the first (edge) cell but NOT
    # degraded -> must not trigger an extend_coverage take-more.
    df = pd.DataFrame([[0.47] * 5 for _ in range(5)])  # all equal
    res = engine.evaluate(
        df, nominal_wt_in=NOMINAL, assessment_type="GML",
        level1_result=l1(), level2_result=l2(),
    )
    assert res.status == "SUFFICIENT"
    assert "extend_coverage" not in _actions(res)


# --- ESCALATE --------------------------------------------------------------
def test_escalate_severe_rsf(engine):
    res = engine.evaluate(
        adequate_grid(), nominal_wt_in=NOMINAL, assessment_type="LML",
        level1_result=l1(verdict="FAIL_LEVEL_1", margin=-0.05),
        level2_result=l2(rsf=0.40),
    )
    assert res.status == "ESCALATE"
    assert res.confidence == "high"
    assert "escalate_level_3" in _actions(res)


def test_escalate_level1_fail_level2_pass(engine):
    res = engine.evaluate(
        adequate_grid(), nominal_wt_in=NOMINAL, assessment_type="GML",
        level1_result=l1(verdict="FAIL_LEVEL_1", margin=-0.01),
        level2_result=l2(rsf=0.92, rsf_a=0.90),
    )
    assert res.status == "ESCALATE"
    assert "escalate_level_2" in _actions(res)


def test_escalate_rsf_below_rsfa(engine):
    res = engine.evaluate(
        adequate_grid(), nominal_wt_in=NOMINAL, assessment_type="GML",
        level1_result=l1(), level2_result=l2(rsf=0.70, rsf_a=0.90),
    )
    assert res.status == "ESCALATE"
    assert "escalate_level_2" in _actions(res)


# --- metrics + action structure -------------------------------------------
def test_metrics_reported(engine):
    res = engine.evaluate(
        adequate_grid(), nominal_wt_in=NOMINAL, assessment_type="GML",
        level1_result=l1(), level2_result=l2(),
    )
    m = res.metrics
    assert m["n_readings"] == 25
    assert m["min_location"] == (2, 2)
    assert m["min_on_edge"] is False
    assert m["cov"] is not None and m["cov"] >= 0.0


def test_actions_have_location_and_rationale(engine):
    res = engine.evaluate(
        pd.DataFrame([[0.47] * 3 for _ in range(3)]),
        nominal_wt_in=NOMINAL, assessment_type="GML",
        level1_result=l1(), level2_result=l2(),
    )
    assert res.actions
    for a in res.actions:
        assert isinstance(a, SufficiencyAction)
        assert a.location and a.rationale


# --- FFS report integration ------------------------------------------------
def test_ffs_report_includes_sufficiency_section(engine):
    from digitalmodel.asset_integrity.assessment import FFSDecision, FFSReport

    grid = pd.DataFrame([[0.47] * 3 for _ in range(3)])  # sparse -> TAKE_MORE
    suf = engine.evaluate(
        grid, nominal_wt_in=NOMINAL, assessment_type="GML",
        level1_result=l1(), level2_result=l2(),
    )
    decision = FFSDecision.decide(
        level1_verdict="ACCEPT", level2_verdict="ACCEPT",
        rsf=0.95, rsf_a=0.90, t_mm_in=0.46, t_min_in=0.30,
        corrosion_rate_in_per_yr=0.005,
    )
    html_out = FFSReport.generate_html(
        grid_df=grid, decision=decision, component_id="TEST-001",
        nominal_od_in=12.75, nominal_wt_in=0.50, t_min_in=0.30,
        design_code="B31.8", design_pressure_psi=1000,
        sufficiency=suf,
    )
    assert "Field Measurement Sufficiency" in html_out
    assert "TAKE_MORE" in html_out
    assert "add_readings" in html_out


def test_ffs_report_omits_sufficiency_when_absent():
    from digitalmodel.asset_integrity.assessment import FFSDecision, FFSReport

    grid = pd.DataFrame([[0.47] * 3 for _ in range(3)])
    decision = FFSDecision.decide(
        level1_verdict="ACCEPT", level2_verdict="ACCEPT",
        rsf=0.95, rsf_a=0.90, t_mm_in=0.46, t_min_in=0.30,
        corrosion_rate_in_per_yr=0.005,
    )
    html_out = FFSReport.generate_html(
        grid_df=grid, decision=decision, component_id="TEST-001",
        nominal_od_in=12.75, nominal_wt_in=0.50, t_min_in=0.30,
        design_code="B31.8", design_pressure_psi=1000,
    )
    assert "Field Measurement Sufficiency" not in html_out
