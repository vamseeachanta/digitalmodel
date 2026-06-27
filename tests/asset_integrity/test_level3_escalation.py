# ABOUTME: Tests for the FFS Level 3 escalation interface — when to escalate
# ABOUTME: beyond L1/L2 screening and the structured specialist handoff package.
"""Tests for digitalmodel.asset_integrity.assessment.level3_escalation."""

from digitalmodel.asset_integrity.assessment.level3_escalation import (
    Level3Handoff,
    escalate_to_level3,
    should_escalate_to_level3,
)


# --- helpers ---------------------------------------------------------------
def l1(verdict="ACCEPT", t_min=0.30, margin=0.16):
    return {"verdict": verdict, "t_min_in": t_min, "margin_in": margin}


def l2(rsf=0.95, rsf_a=0.90, t_mm=0.46, atype="GML"):
    return {
        "rsf": rsf,
        "rsf_a": rsf_a,
        "t_mm_in": t_mm,
        "assessment_type": atype,
        "folias_factor": 1.2,
    }


def geom():
    return {"od_in": 12.75, "wt_in": 0.50, "flaw_length_in": 6.0, "flaw_depth_in": 0.2}


def loads():
    return {"design_pressure_psi": 1000, "temperature_f": 150, "axial_lbf": 0.0}


def material():
    return {"grade": "API 5L X52", "smys_psi": 52000, "smts_psi": 66000}


# --- should_escalate_to_level3: each trigger -------------------------------
def test_escalate_low_rsf_below_floor():
    triggered, reasons = should_escalate_to_level3(
        l1(), l2(rsf=0.40, rsf_a=0.90), rsf_floor=0.50
    )
    assert triggered is True
    assert any("floor" in r.lower() for r in reasons)


def test_escalate_crack_like_flaw():
    triggered, reasons = should_escalate_to_level3(
        l1(), l2(), has_crack_like_flaw=True
    )
    assert triggered is True
    assert any("crack" in r.lower() for r in reasons)


def test_escalate_interacting_defects():
    triggered, reasons = should_escalate_to_level3(
        l1(), l2(), interacting_defects=True
    )
    assert triggered is True
    assert any("interact" in r.lower() for r in reasons)


def test_escalate_complex_geometry():
    triggered, reasons = should_escalate_to_level3(
        l1(), l2(), complex_geometry=True
    )
    assert triggered is True
    assert any("geometry" in r.lower() for r in reasons)


def test_escalate_level2_fail_rsf_below_rsfa():
    # rsf above the floor but below RSFa -> Level 2 fails, no viable re-rate.
    triggered, reasons = should_escalate_to_level3(
        l1(), l2(rsf=0.70, rsf_a=0.90), rsf_floor=0.50
    )
    assert triggered is True
    assert any("rsfa" in r.lower() for r in reasons)


def test_no_escalation_comfortable_case():
    # RSF well above RSFa, no flags -> no escalation.
    triggered, reasons = should_escalate_to_level3(
        l1(), l2(rsf=0.95, rsf_a=0.90)
    )
    assert triggered is False
    assert reasons == []


def test_returns_python_bool():
    triggered, _ = should_escalate_to_level3(l1(), l2(rsf=0.40))
    assert isinstance(triggered, bool)


# --- escalate_to_level3: handoff package -----------------------------------
def test_handoff_is_dataclass_with_echoed_inputs():
    handoff = escalate_to_level3(
        geom(), loads(), material(), l1(), l2(rsf=0.40, rsf_a=0.90)
    )
    assert isinstance(handoff, Level3Handoff)
    dp = handoff.data_package
    assert dp["geometry"] == geom()
    assert dp["loads"] == loads()
    assert dp["material"] == material()
    assert dp["level1_result"] == l1()
    assert dp["level2_result"] == l2(rsf=0.40, rsf_a=0.90)


def test_handoff_package_completeness_all_five_inputs():
    handoff = escalate_to_level3(
        geom(), loads(), material(), l1(), l2(), has_crack_like_flaw=True
    )
    for key in ("geometry", "loads", "material", "level1_result", "level2_result"):
        assert key in handoff.data_package


def test_handoff_grid_summary_echoed():
    grid = {"n_readings": 25, "min_thickness_in": 0.30, "cov": 0.04}
    handoff = escalate_to_level3(
        geom(), loads(), material(), l1(), l2(), grid_summary=grid,
        interacting_defects=True,
    )
    assert handoff.data_package["grid_summary"] == grid


def test_handoff_triggered_has_analysis_and_questions():
    handoff = escalate_to_level3(
        geom(), loads(), material(), l1(), l2(rsf=0.40, rsf_a=0.90)
    )
    assert handoff.triggered is True
    assert handoff.recommended_analysis  # non-empty
    assert handoff.open_questions        # non-empty


def test_handoff_crack_recommends_bs7910_fad():
    handoff = escalate_to_level3(
        geom(), loads(), material(), l1(), l2(), has_crack_like_flaw=True
    )
    blob = " ".join(handoff.recommended_analysis).lower()
    assert "bs7910" in blob or "fad" in blob or "fracture" in blob


def test_handoff_crack_questions_ask_for_toughness():
    handoff = escalate_to_level3(
        geom(), loads(), material(), l1(), l2(), has_crack_like_flaw=True
    )
    blob = " ".join(handoff.open_questions).lower()
    assert "toughness" in blob or "cvn" in blob or "fad" in blob


def test_handoff_not_triggered_empty_analysis_but_complete_record():
    handoff = escalate_to_level3(
        geom(), loads(), material(), l1(), l2(rsf=0.95, rsf_a=0.90)
    )
    assert handoff.triggered is False
    assert handoff.recommended_analysis == []
    assert handoff.open_questions == []
    # Even when not triggered, the package is a complete record.
    for key in ("geometry", "loads", "material", "level1_result", "level2_result"):
        assert key in handoff.data_package
