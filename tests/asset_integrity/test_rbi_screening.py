# ABOUTME: Tests for the qualitative API 580-style RBI screening layer —
# ABOUTME: PoF bands, risk matrix, risk-scaled intervals, real-register ranking.
"""RBI screening tests (issue #1272).

Covers band-edge behaviour, monotonicity (worse condition -> higher PoF ->
higher risk -> shorter interval), 5x5 matrix corners, interval guardrails
(never exceeds the half-life rule nor the code maximum), and an integration
run over the real anonymized GML results register (regression-anchored).
"""

from pathlib import Path

import pandas as pd
import pytest

from digitalmodel.asset_integrity.assessment.ffs_coordinator import (
    FFSAssessmentResult,
)
from digitalmodel.asset_integrity.inspection_planning import plan_inspection
from digitalmodel.asset_integrity.rbi_screening import (
    NON_RBI_BASELINE_INTERVALS_YR,
    RBI_PHASES,
    RBIParameters,
    RBIScreeningResult,
    cof_category,
    pof_category,
    rbi_rank_register,
    recommend_interval,
    risk_band,
    screen_component,
    screen_ffs_result,
    summarize_ranking,
)

DATA = Path(__file__).parent / "test_data" / "real_inspection"
PARAMS = RBIParameters()

# REGRESSION-ANCHOR: RBI screening of the real GML results register (142 scans)
# with default practice parameters and default consequence class "C".
# Update deliberately only (parameter or register changes).
ANCHOR_BAND_COUNTS_C = {"LOW": 18, "MEDIUM": 79, "MEDIUM-HIGH": 45, "HIGH": 0}
ANCHOR_POF_COUNTS = {1: 18, 2: 66, 3: 13, 4: 36, 5: 9}
ANCHOR_MIN_INTERVAL_YR = 1.4528  # half-life of the shortest-life scan (RJ-127)


# ---------------------------------------------------------------- CoF mapping
def test_cof_class_letters_map_1_to_5():
    assert [cof_category(c)[0] for c in "ABCDE"] == [1, 2, 3, 4, 5]
    assert cof_category("c") == (3, "C")  # case-insensitive


def test_cof_integer_passthrough_and_validation():
    assert cof_category(4) == (4, "D")
    with pytest.raises(ValueError):
        cof_category("F")
    with pytest.raises(ValueError):
        cof_category(0)
    with pytest.raises(ValueError):
        cof_category(6)


# ------------------------------------------------------------------ PoF bands
def test_pof_rsf_margin_band_edges():
    """Values exactly at an edge land in the better (lower) category."""
    rsf_a = 0.90
    expected = {1.10: 1, 1.05: 2, 1.00: 3, 0.90: 4, 0.899: 5}
    for margin, cat in expected.items():
        got, _ = pof_category(rsf=margin * rsf_a, rsf_a=rsf_a, params=PARAMS)
        assert got == cat, f"margin {margin} -> {got}, expected {cat}"


def test_pof_life_ratio_band_edges():
    """Default reference interval 5 yr => life edges 40/20/10/5 yr."""
    expected = {40.0: 1, 20.0: 2, 10.0: 3, 5.0: 4, 4.99: 5}
    for life, cat in expected.items():
        got, _ = pof_category(remaining_life_yr=life, params=PARAMS)
        assert got == cat, f"life {life} -> {got}, expected {cat}"


def test_pof_takes_most_conservative_subscore():
    # good margin, poor life -> life governs
    cat, subs = pof_category(rsf=1.0, rsf_a=0.9, remaining_life_yr=3.0, params=PARAMS)
    assert subs["rsf_margin"] == 1 and subs["life_ratio"] == 5
    assert cat == 5


def test_pof_corrosion_trend_bump_threshold_and_cap():
    base, _ = pof_category(remaining_life_yr=50.0, corrosion_rate_mm_yr=0.24)
    bumped, subs = pof_category(remaining_life_yr=50.0, corrosion_rate_mm_yr=0.25)
    assert base == 1 and bumped == 2 and subs["corrosion_trend_bump"] == 1
    capped, _ = pof_category(remaining_life_yr=1.0, corrosion_rate_mm_yr=0.5)
    assert capped == 5  # bump never exceeds category 5


def test_pof_requires_at_least_one_condition_input():
    with pytest.raises(ValueError):
        pof_category(corrosion_rate_mm_yr=0.1)
    with pytest.raises(ValueError):
        pof_category(rsf=0.95, rsf_a=None)


def test_pof_monotonic_in_rsf_and_life():
    rsf_grid = [1.0, 0.98, 0.95, 0.92, 0.88, 0.80]
    cats = [pof_category(rsf=r, rsf_a=0.9)[0] for r in rsf_grid]
    assert cats == sorted(cats)  # lower RSF -> PoF never decreases
    life_grid = [100.0, 45.0, 25.0, 12.0, 6.0, 2.0]
    cats = [pof_category(remaining_life_yr=y)[0] for y in life_grid]
    assert cats == sorted(cats)  # shorter life -> PoF never decreases


# ---------------------------------------------------------------- risk matrix
def test_matrix_corners_and_center():
    assert risk_band(1, 1) == (1, "LOW")
    assert risk_band(5, 5) == (25, "HIGH")
    assert risk_band(1, 5) == (5, "MEDIUM")
    assert risk_band(5, 1) == (5, "MEDIUM")
    assert risk_band(3, 3) == (9, "MEDIUM")
    assert risk_band(4, 4) == (16, "HIGH")
    assert risk_band(2, 5) == (10, "MEDIUM-HIGH")
    assert risk_band(5, 3) == (15, "MEDIUM-HIGH")


def test_matrix_symmetric_and_monotonic_in_cof():
    for p in range(1, 6):
        for c in range(1, 6):
            assert risk_band(p, c) == risk_band(c, p)  # symmetric banding
        scores = [risk_band(p, c)[0] for c in range(1, 6)]
        assert scores == sorted(scores)  # higher CoF -> risk never decreases


def test_matrix_rejects_out_of_range():
    with pytest.raises(ValueError):
        risk_band(0, 3)
    with pytest.raises(ValueError):
        risk_band(3, 6)


# --------------------------------------------------------------- intervals
def _plan(current=22.0, required=19.0, rate=0.15, code_max=10.0):
    return plan_inspection(
        current_mm=current,
        required_mm=required,
        code_max_interval_years=code_max,
        corrosion_rate=rate,
    )


def test_interval_scaled_down_by_risk_band():
    plan = _plan()  # remaining life 20 yr -> half-life 10 yr -> code max 10 yr
    intervals = [recommend_interval(b, plan)[0] for b in
                 ("LOW", "MEDIUM", "MEDIUM-HIGH", "HIGH")]
    assert intervals == [10.0, 6.0, 4.0, 2.0]
    assert intervals == sorted(intervals, reverse=True)  # higher risk -> shorter


def test_interval_never_exceeds_half_life_nor_code_max():
    for rate in (0.05, 0.15, 0.4, 1.0):
        plan = _plan(rate=rate)
        for band in ("LOW", "MEDIUM", "MEDIUM-HIGH", "HIGH"):
            interval, _ = recommend_interval(band, plan)
            assert interval <= plan.next_inspection_interval_years
            assert interval <= plan.code_max_interval_years
            if plan.half_life_interval_years is not None:
                assert interval <= plan.half_life_interval_years


def test_interval_half_life_governs_when_shorter_than_band_cap():
    plan = _plan(rate=1.0)  # remaining life 3 yr -> half-life 1.5 yr
    interval, governing = recommend_interval("HIGH", plan)
    assert interval == pytest.approx(1.5)
    assert "half-life" in governing


def test_interval_zero_on_failed_screening():
    plan = _plan(current=19.0, required=19.33)  # at/below t_min
    interval, governing = recommend_interval("LOW", plan)
    assert interval == 0.0
    assert "now" in governing


# --------------------------------------------------------- component screening
def test_screen_component_end_to_end_high_risk():
    res = screen_component(
        "J-1", "E", current_mm=20.2, required_mm=19.33, corrosion_rate_mm_yr=0.253
    )
    assert isinstance(res, RBIScreeningResult)
    assert res.pof_category == 5 and res.cof_category == 5
    assert res.risk_band == "HIGH"
    # half-life (~1.72 yr) is shorter than the 2-yr HIGH cap
    assert res.recommended_interval_yr == pytest.approx(0.87 / 0.253 / 2.0, rel=1e-6)
    assert res.recommended_interval_yr <= 2.0
    assert res.inputs["consequence_class"] == "E"
    assert res.basis and any("API RP 581" in note for note in res.basis)
    assert res.phases == RBI_PHASES
    assert res.to_dict()["risk_band"] == "HIGH"


def test_screen_component_low_risk_allows_code_max():
    res = screen_component(
        "J-2", "A", current_mm=25.0, required_mm=19.33, corrosion_rate_mm_yr=0.05
    )
    assert res.risk_band == "LOW"
    assert res.recommended_interval_yr == PARAMS.code_max_interval_yr


def test_screen_component_zero_corrosion_rate():
    res = screen_component(
        "J-3", "B", current_mm=25.0, required_mm=19.33, corrosion_rate_mm_yr=0.0
    )
    assert res.pof_category == 1
    assert res.inputs["remaining_life_yr"] is None  # not corroding
    assert res.recommended_interval_yr == PARAMS.code_max_interval_yr


def test_screen_component_monotonic_interval_vs_cof():
    intervals = []
    for cls in "ABCDE":
        res = screen_component(
            "J", cls, current_mm=21.0, required_mm=19.33, corrosion_rate_mm_yr=0.1
        )
        intervals.append(res.recommended_interval_yr)
    assert intervals == sorted(intervals, reverse=True)


# ------------------------------------------------------------ FFS result hook
def _ffs_result(**over):
    base = dict(
        component_id="FFS-1",
        assessment_type="GML",
        level_reached=2,
        t_nominal_in=0.875,
        t_min_in=0.761,
        t_measured_min_in=0.820,
        t_measured_avg_in=0.845,
        fca_in=0.0,
        rsf=0.96,
        rsf_a=0.90,
        folias_factor=1.1,
        remaining_life_yr=15.0,
        verdict="ACCEPT",
        rerated_pressure_psi=1480.0,
        sufficiency_status="SUFFICIENT",
    )
    base.update(over)
    return FFSAssessmentResult(**base)


def test_screen_ffs_result_reproduces_remaining_life():
    res = screen_ffs_result(_ffs_result(), "C")
    assert res.inputs["remaining_life_yr"] == pytest.approx(15.0)
    assert res.inputs["rsf"] == pytest.approx(0.96)
    assert res.pof_category == 3  # life ratio 15/5 = 3 -> band edge 2.0
    assert res.risk_score == 9 and res.risk_band == "MEDIUM"


def test_screen_ffs_result_low_rsf_raises_pof():
    ok = screen_ffs_result(_ffs_result(), "C")
    bad = screen_ffs_result(_ffs_result(rsf=0.78), "C")
    assert bad.pof_category > ok.pof_category
    assert bad.risk_score > ok.risk_score
    assert bad.recommended_interval_yr <= ok.recommended_interval_yr


def test_screen_ffs_result_explicit_rate_overrides():
    res = screen_ffs_result(_ffs_result(), "C", corrosion_rate_in_per_yr=0.002)
    assert res.inputs["corrosion_rate_mm_yr"] == pytest.approx(0.002 * 25.4)


# --------------------------------------------------- register integration
@pytest.fixture(scope="module")
def register():
    return pd.read_csv(DATA / "gml_results_register.csv")


def test_register_ranking_regression_anchor(register):
    ranked = rbi_rank_register(register)
    assert len(ranked) == 142
    summary = summarize_ranking(ranked)
    # REGRESSION-ANCHOR: band counts on the real register with default
    # parameters and default CoF class "C". Update deliberately only.
    assert summary["band_counts"] == ANCHOR_BAND_COUNTS_C
    assert summary["pof_counts"] == ANCHOR_POF_COUNTS
    assert summary["n_rows"] == 142
    assert summary["min_recommended_interval_yr"] == pytest.approx(
        ANCHOR_MIN_INTERVAL_YR, abs=1e-4
    )
    assert summary["phase"] == "risk prioritization"
    assert summary["rbi_phases"] == list(RBI_PHASES)


def test_register_ranking_sorted_most_critical_first(register):
    ranked = rbi_rank_register(register)
    scores = ranked["risk_score"].tolist()
    assert scores == sorted(scores, reverse=True)
    # the shortest-life joint (RJ-101 box end, ~3.5 yr) ranks in the top rows
    assert "RJ-101" in ranked.head(3)["joint_id"].tolist()


def test_register_intervals_bounded_by_half_life_and_code_max(register):
    ranked = rbi_rank_register(register)
    assert (ranked["recommended_interval_yr"] <= PARAMS.code_max_interval_yr).all()
    assert (
        ranked["recommended_interval_yr"] <= ranked["half_life_interval_yr"] + 1e-12
    ).all()
    assert (ranked["recommended_interval_yr"] > 0.0).all()  # register all passes


def test_register_cof_overrides_shift_bands(register):
    low = summarize_ranking(rbi_rank_register(register, default_cof="A"))
    high = summarize_ranking(rbi_rank_register(register, default_cof="E"))
    assert low["band_counts"]["HIGH"] == 0
    assert high["band_counts"]["HIGH"] >= 1
    assert high["min_recommended_interval_yr"] <= low["min_recommended_interval_yr"]
    # per-joint override: bump one joint to class E, its rows move up
    ranked = rbi_rank_register(register, cof_by_joint={"RJ-103": "E"})
    rj103 = ranked[ranked["joint_id"] == "RJ-103"]
    assert (rj103["cof_category"] == 5).all()


def test_baseline_heuristics_constant_shape():
    assert set(NON_RBI_BASELINE_INTERVALS_YR) == {
        "rigid_riser", "catenary_riser", "touchdown_zone", "flexible_riser"
    }
    for lo, hi in NON_RBI_BASELINE_INTERVALS_YR.values():
        assert 0 < lo <= hi <= 5.0
