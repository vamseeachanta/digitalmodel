# ABOUTME: Tests for the shared FFS applicability layer (#1094): d/t flags on
# ABOUTME: B31G/RSTRENG/DNV, ESCALATE routing in ffs_decision, report footer, goldens.
"""Applicability flags for the FFS strength methods (issue #1094).

Every flag has a positive fixture (flag raised, number still returned) and a
negative fixture (no flag).  The coordinator turns any raised flag into an
ESCALATE verdict, the report lists the flags in its footer, and the validated
golden values from the 2026-06-27/29 validation records are re-asserted here so
the applicability layer cannot move a single number.
"""

import math

import numpy as np
import pandas as pd
import pytest

from digitalmodel.asset_integrity import corroded_pipe as cp
from digitalmodel.asset_integrity import dnv_rp_f101 as dnv
from digitalmodel.asset_integrity.applicability import (
    API579_FOLIAS_LAMBDA_FLAG,
    B31G_RELATIVE_DEPTH_FLAG,
    DNV_F101_RELATIVE_DEPTH_FLAG,
    DNV_F101_SIZING_STD_FLAG,
    Applicability,
    check_upper_limit,
    from_details,
    merge,
)
from digitalmodel.asset_integrity.assessment import (
    FFSComponent,
    FFSDecision,
    FFSReport,
    Level2Engine,
    assess_component,
)
from digitalmodel.asset_integrity.assessment.level2_engine import _LAMBDA_UPPER


# --- the dataclass ---------------------------------------------------------
def test_applicability_default_is_ok_and_empty():
    a = Applicability()
    assert a.ok is True
    assert a.flags == [] and a.notes == []
    assert a.to_dict() == {"ok": True, "flags": [], "notes": []}


def test_check_upper_limit_positive_and_negative():
    bad = check_upper_limit(0.9, 0.80, flag="X_FLAG", quantity="d/t", method="X")
    assert bad.ok is False
    assert bad.flags == ["X_FLAG"]
    assert "0.900" in bad.notes[0] and "0.80" in bad.notes[0]

    good = check_upper_limit(0.8, 0.80, flag="X_FLAG", quantity="d/t", method="X")
    assert good.ok is True and good.flags == []


def test_merge_combines_flags_and_notes_in_order():
    a = Applicability(False, ["A"], ["note a"])
    b = Applicability(True, [], [])
    c = Applicability(False, ["C"], ["note c"])
    m = merge(a, b, c, None)
    assert m.ok is False
    assert m.flags == ["A", "C"]
    assert m.notes == ["note a", "note c"]
    assert merge(b, None).ok is True


def test_from_details_bridges_legacy_dict_keys():
    legacy = {"within_applicability": False, "applicability_note": "too deep"}
    a = from_details(legacy, flag="LEGACY")
    assert a.ok is False and a.flags == ["LEGACY"] and a.notes == ["too deep"]
    assert from_details({"within_applicability": True}, flag="LEGACY").ok is True
    assert from_details({}, flag="LEGACY").ok is True


# --- finding 2: B31G / Modified B31G / RSTRENG d/t > 0.80 -------------------
@pytest.mark.parametrize("fn", [cp.b31g_original, cp.modified_b31g])
def test_b31g_family_flags_deep_defect_but_still_returns_number(fn):
    deep = fn(D=20.0, t=0.5, d=0.45, L=4.0, smys_psi=52_000.0)
    assert deep.applicability.ok is False
    assert deep.applicability.flags == [B31G_RELATIVE_DEPTH_FLAG]
    assert "0.80" in deep.applicability.notes[0]
    # the number is still returned, never suppressed
    assert math.isfinite(deep.failure_pressure_psi) and deep.failure_pressure_psi > 0
    # legacy detail keys stay in step with the dataclass
    assert deep.details["within_applicability"] is False
    assert deep.details["applicability_note"] == deep.applicability.notes[0]


@pytest.mark.parametrize("fn", [cp.b31g_original, cp.modified_b31g])
def test_b31g_family_no_flag_at_or_below_limit(fn):
    at_limit = fn(D=20.0, t=0.5, d=0.40, L=4.0, smys_psi=52_000.0)  # d/t = 0.80
    assert at_limit.applicability.ok is True
    assert at_limit.applicability.flags == []
    assert at_limit.details["within_applicability"] is True


def test_rstreng_flags_on_deepest_profile_point():
    deep = cp.rstreng_effective_area(
        D=20.0,
        t=0.5,
        positions_in=[0.0, 2.0, 4.0],
        depths_in=[0.10, 0.45, 0.10],
        smys_psi=52_000.0,
    )
    assert deep.applicability.ok is False
    assert deep.applicability.flags == [B31G_RELATIVE_DEPTH_FLAG]

    ok = cp.rstreng_effective_area(
        D=20.0,
        t=0.5,
        positions_in=[0.0, 2.0, 4.0],
        depths_in=[0.10, 0.30, 0.10],
        smys_psi=52_000.0,
    )
    assert ok.applicability.ok is True and ok.applicability.flags == []


# --- finding 7: 1.39 / 0.72 are ASME B31.8 class-1 values, documented -------
def test_safety_factor_defaults_are_asme_b318_class1():
    assert cp.ASME_B318_CLASS1_DESIGN_FACTOR == 0.72
    assert cp.ASME_B318_CLASS1_SAFETY_FACTOR == 1.39
    assert cp.ASME_B318_DESIGN_FACTOR_BY_LOCATION_CLASS == {
        1: 0.72,
        2: 0.60,
        3: 0.50,
        4: 0.40,
    }
    # the module default is the class-1 value, by name
    assert cp._DEFAULT_SAFETY_FACTOR == cp.ASME_B318_CLASS1_SAFETY_FACTOR
    assert dnv._DEFAULT_USAGE_FACTOR == cp.ASME_B318_CLASS1_DESIGN_FACTOR
    # location classes 2/3/4 give the 1/F safety factor
    assert cp.safety_factor_for_location_class(1) == pytest.approx(1.39)
    assert cp.safety_factor_for_location_class(2) == pytest.approx(1.0 / 0.60)
    assert cp.safety_factor_for_location_class(3) == pytest.approx(1.0 / 0.50)
    with pytest.raises(ValueError):
        cp.safety_factor_for_location_class(5)
    # the basis is recorded on every result
    r = cp.modified_b31g(D=20.0, t=0.5, d=0.2, L=4.0, smys_psi=52_000.0)
    assert r.details["safety_factor"] == 1.39
    assert "ASME B31.8" in r.details["safety_factor_basis"]


# --- finding 3: DNV-RP-F101 d/t > 0.85 -------------------------------------
def test_dnv_single_defect_flags_deep_defect_but_still_returns_number():
    deep = dnv.dnv_f101_single_defect(
        D=20.0, t=0.5, d=0.45, L=4.0, smts_psi=66_700.0
    )  # d/t = 0.90
    assert deep.applicability.ok is False
    assert deep.applicability.flags == [DNV_F101_RELATIVE_DEPTH_FLAG]
    assert "0.85" in deep.applicability.notes[0]
    assert math.isfinite(deep.capacity_pressure_psi) and deep.capacity_pressure_psi > 0
    assert deep.details["within_applicability"] is False


def test_dnv_single_defect_no_flag_at_limit():
    ok = dnv.dnv_f101_single_defect(
        D=20.0, t=0.5, d=0.425, L=4.0, smts_psi=66_700.0
    )  # d/t = 0.85 exactly
    assert ok.applicability.ok is True and ok.applicability.flags == []
    assert ok.details["within_applicability"] is True


def test_dnv_psf_flags_depth_and_sizing_std_independently():
    both = dnv.dnv_f101_psf(
        D=20.0, t=0.5, d=0.45, L=4.0, smts_psi=66_700.0, std_rel_depth=0.20
    )
    assert both.applicability.ok is False
    assert both.applicability.flags == [
        DNV_F101_RELATIVE_DEPTH_FLAG,
        DNV_F101_SIZING_STD_FLAG,
    ]
    assert both.details["applicability_note"] == "; ".join(both.applicability.notes)

    std_only = dnv.dnv_f101_psf(
        D=20.0, t=0.5, d=0.20, L=4.0, smts_psi=66_700.0, std_rel_depth=0.20
    )
    assert std_only.applicability.flags == [DNV_F101_SIZING_STD_FLAG]

    clean = dnv.dnv_f101_psf(D=20.0, t=0.5, d=0.20, L=4.0, smts_psi=66_700.0)
    assert clean.applicability.ok is True and clean.applicability.flags == []


def test_dnv_interacting_propagates_governing_case_flag():
    # Two touching deep defects -> composite d/t = 0.90 governs -> flagged.
    deep = dnv.dnv_f101_interacting(
        D=20.0, t=0.5, defects=[(0.0, 4.0, 0.45), (4.0, 4.0, 0.45)], smts_psi=66_700.0
    )
    assert deep.applicability.ok is False
    assert DNV_F101_RELATIVE_DEPTH_FLAG in deep.applicability.flags

    ok = dnv.dnv_f101_interacting(
        D=20.0, t=0.5, defects=[(0.0, 4.0, 0.20), (4.0, 4.0, 0.20)], smts_psi=66_700.0
    )
    assert ok.applicability.ok is True


def test_dnv_safety_class_factors_are_explicit_inputs_with_documented_defaults():
    # Defaults reproduce the historical scalar PSFs (Very-High / absolute, StD 0.08).
    r = dnv.dnv_f101_psf(D=20.0, t=0.5, d=0.2, L=4.0, smts_psi=66_700.0)
    assert r.details["safety_class"] == "very high"
    assert r.details["gamma_m"] == pytest.approx(0.77)
    # epsilon_d quadratic at StD 0.08 is 1.0031, i.e. the legacy scalar 1.0
    assert r.details["epsilon_d"] == pytest.approx(1.0, abs=5e-3)
    # ... and an explicit class changes them.
    med = dnv.dnv_f101_psf(
        D=20.0,
        t=0.5,
        d=0.2,
        L=4.0,
        smts_psi=66_700.0,
        safety_class="medium",
        measurement_method="relative",
    )
    assert med.details["gamma_m"] == pytest.approx(0.85)
    assert med.details["safety_class"] == "medium"


# --- findings 5 / 11: Level 2 Folias lambda cap is flagged, not silent ------
def _lml_grid(n_rows: int) -> pd.DataFrame:
    # Uniform 0.42 in against a 0.50 in nominal: every row is a "flaw" row
    # (below 0.9 t_nom), so L_a = n_rows (unit row spacing).
    return pd.DataFrame(np.full((n_rows, 4), 0.42))


def test_level2_lml_flags_lambda_beyond_table_range():
    # t_min 0.45 > t_mm 0.42 -> Rt = 0.933 < 1, so M_t participates in the RSF.
    engine = Level2Engine("LML", nominal_od_in=12.75, nominal_wt_in=0.5, t_min_in=0.45)
    long_flaw = engine.evaluate(_lml_grid(60))
    assert long_flaw["rt"] < 1.0
    assert long_flaw["lambda"] > _LAMBDA_UPPER
    assert long_flaw["applicability"].ok is False
    assert long_flaw["applicability"].flags == [API579_FOLIAS_LAMBDA_FLAG]
    assert "lambda=" in long_flaw["applicability"].notes[0]
    # the frozen table value is still what is reported (finding 11)
    assert long_flaw["folias_factor"] == pytest.approx(
        math.sqrt(1.0 + 0.48 * _LAMBDA_UPPER**2)
    )

    short_flaw = engine.evaluate(_lml_grid(6))
    assert short_flaw["lambda"] <= _LAMBDA_UPPER
    assert short_flaw["applicability"].ok is True


def test_level2_lml_no_lambda_flag_when_folias_does_not_participate():
    # Rt caps at 1.0 (t_mm >= t_min): RSF == 1 whatever M_t is, so nothing was
    # extrapolated and no flag is raised even though lambda > 20.
    engine = Level2Engine("LML", nominal_od_in=12.75, nominal_wt_in=0.5, t_min_in=0.17)
    r = engine.evaluate(_lml_grid(60))
    assert r["lambda"] > _LAMBDA_UPPER and r["rt"] == 1.0
    assert r["rsf"] == 1.0
    assert r["applicability"].ok is True


def test_level2_gml_has_no_applicability_flags():
    engine = Level2Engine("GML", nominal_od_in=12.75, nominal_wt_in=0.5, t_min_in=0.45)
    r = engine.evaluate(_lml_grid(60))
    assert r["applicability"].ok is True and r["applicability"].flags == []


# --- ffs_decision: any raised flag -> ESCALATE -------------------------------
def _decide(applicability=None):
    return FFSDecision.decide(
        level1_verdict="ACCEPT",
        level2_verdict="ACCEPT",
        rsf=0.97,
        rsf_a=0.9,
        t_mm_in=0.42,
        t_min_in=0.30,
        corrosion_rate_in_per_yr=0.005,
        design_pressure_psi=1000.0,
        applicability=applicability,
    )


def test_ffs_decision_escalates_when_any_flag_is_raised():
    flagged = Applicability(False, ["SOME_FLAG"], ["outside the method range"])
    d = _decide(flagged)
    assert d["verdict"] == "ESCALATE"
    assert "SOME_FLAG" in d["governing_criterion"]
    assert "outside the method range" in d["governing_criterion"]
    assert d["applicability"] == flagged.to_dict()
    # the numeric result is still carried for reference, not used as a verdict
    assert d["rsf"] == 0.97
    assert d["rerated_mawp_psi"] == pytest.approx(1000.0)
    assert math.isfinite(d["remaining_life_yr"])


def test_ffs_decision_unchanged_without_flags():
    assert _decide(None)["verdict"] == "ACCEPT"
    clean = _decide(Applicability())
    assert clean["verdict"] == "ACCEPT"
    assert clean["applicability"] == {"ok": True, "flags": [], "notes": []}


# --- coordinator threading ---------------------------------------------------
def _pipe(design_pressure_psi=1000.0):
    return FFSComponent(
        component_id="LINE-1094",
        design_code="B31.8",
        nominal_od_in=12.75,
        nominal_wt_in=0.500,
        design_pressure_psi=design_pressure_psi,
        smys_psi=52_000.0,
        corrosion_rate_in_per_yr=0.005,
        rsf_a=0.90,
    )


# The indexed to_dict surface (#1066) as pinned by the workflow golden.
_INDEXED_KEYS = {
    "component_id",
    "assessment_type",
    "level_reached",
    "t_nominal_in",
    "t_min_in",
    "t_measured_min_in",
    "t_measured_avg_in",
    "fca_in",
    "rsf",
    "rsf_a",
    "folias_factor",
    "remaining_life_yr",
    "verdict",
    "rerated_pressure_psi",
    "sufficiency_status",
    "passes",
    "code_reference",
}


def test_assess_component_escalates_on_flag_and_exposes_applicability():
    # 2650 psi -> B31.8 t_min = 0.453 in > t_mm = 0.42 in, so Rt < 1 and the
    # 60-row flaw (lambda ~ 33) exercises the frozen Folias factor.
    res = assess_component(_pipe(2650.0), _lml_grid(60), force_type="LML")
    assert res.t_min_in > res.t_measured_min_in
    assert res.applicability.ok is False
    assert res.applicability.flags == [API579_FOLIAS_LAMBDA_FLAG]
    assert res.verdict == "ESCALATE"
    assert res.passes is False
    assert res.decision["applicability"] == res.applicability.to_dict()
    payload = res.to_dict()
    assert payload["applicability"]["flags"] == [API579_FOLIAS_LAMBDA_FLAG]
    assert set(payload) == _INDEXED_KEYS | {"applicability"}
    # the same case without the flag would have been a numeric verdict
    numeric = FFSDecision.decide(
        level1_verdict=res.level1["verdict"],
        level2_verdict=res.level2["verdict"],
        rsf=res.rsf,
        rsf_a=res.rsf_a,
        t_mm_in=res.t_measured_min_in,
        t_min_in=res.t_min_in,
        corrosion_rate_in_per_yr=0.005,
        design_pressure_psi=2650.0,
    )
    assert numeric["verdict"] in ("RE_RATE", "REPAIR", "REPLACE", "ACCEPT", "MONITOR")


def test_assess_component_default_applicability_keeps_existing_surface():
    res = assess_component(_pipe(), pd.DataFrame([[0.48] * 6 for _ in range(6)]))
    assert res.applicability == Applicability()
    assert res.verdict in ("ACCEPT", "MONITOR")
    # the indexed to_dict surface (#1066) is untouched when nothing is flagged
    assert set(res.to_dict()) == _INDEXED_KEYS


# --- report footer -------------------------------------------------------------
def _report(applicability=None, decision=None):
    df = pd.DataFrame(np.full((5, 10), 0.42))
    decision = decision or _decide(applicability)
    return FFSReport.generate_html(
        grid_df=df,
        decision=decision,
        component_id="PIPE-1094",
        nominal_od_in=16.0,
        nominal_wt_in=0.5,
        t_min_in=0.3,
        design_code="B31.8",
        design_pressure_psi=1000.0,
        applicability=applicability,
    )


def test_report_footer_lists_raised_flags():
    flagged = Applicability(False, ["SOME_FLAG"], ["d/t=0.900 exceeds limit 0.80"])
    html = _report(flagged)
    assert "Applicability flags raised" in html
    assert "SOME_FLAG" in html
    assert "d/t=0.900 exceeds limit 0.80" in html
    assert "ESCALATE" in html


def test_report_has_no_flag_footer_when_clean():
    assert "Applicability flags raised" not in _report(None)
    assert "Applicability flags raised" not in _report(Applicability())


def test_report_t_am_is_area_average():
    # finding 8 applies to the report's displayed t_am too.
    df = pd.DataFrame({"c0": [0.40, 0.50], "c1": [0.60, np.nan]})
    html = FFSReport.generate_html(
        grid_df=df,
        decision=_decide(None),
        component_id="P",
        nominal_od_in=16.0,
        nominal_wt_in=0.5,
        t_min_in=0.3,
        design_code="B31.8",
        design_pressure_psi=1000.0,
    )
    assert "Area-Averaged Thickness (t_am)</td><td>0.5000 inch" in html
    assert "0.5250 inch" not in html


# --- goldens unchanged (validation records 2026-06-27 / 2026-06-29) -----------
def test_golden_hand_calc_pressures_unchanged():
    # b31g-validation-2026-06-27.md / ffs-validation-record-2026-06-27.md
    kw = dict(D=30.0, t=0.375, d=0.15, L=8.0)
    b = cp.b31g_original(smys_psi=52_000.0, **kw)
    m = cp.modified_b31g(smys_psi=52_000.0, **kw)
    d = dnv.dnv_f101_single_defect(smts_psi=66_700.0, **kw)
    assert b.failure_pressure_psi == pytest.approx(1182.6, rel=2e-3)
    assert m.failure_pressure_psi == pytest.approx(1219.3, rel=2e-3)
    assert m.folias_factor == pytest.approx(2.112, rel=1e-3)
    assert m.area_ratio == pytest.approx(0.340, rel=1e-3)
    assert b.folias_factor == pytest.approx(2.356, rel=1e-3)
    assert b.area_ratio == pytest.approx(0.267, rel=2e-3)
    assert d.Q == pytest.approx(1.6623945246407532, rel=1e-12)
    assert d.capacity_pressure_psi == pytest.approx(1334.1940092246919, rel=1e-12)
    assert d.allowable_pressure_psi == pytest.approx(
        0.72 * 1334.1940092246919, rel=1e-12
    )
    assert b.failure_pressure_psi < m.failure_pressure_psi < d.capacity_pressure_psi
    for r in (b, m, d):
        assert r.applicability.ok is True  # d/t = 0.40, inside every limit


@pytest.mark.parametrize(
    "d, t, expected_L",
    [
        (0.10, 0.500, 9.48),
        (0.15, 0.500, 4.72),
        (0.25, 0.625, 3.76),
        (0.10, 0.219, 1.93),
        (0.05, 0.500, 14.17),
        (0.04, 0.344, 11.75),
        (0.50, 0.625, 1.78),
    ],
)
def test_golden_asme_b31g_allowable_length_table_unchanged(d, t, expected_L):
    assert cp.b31g_original_allowable_length(20.0, t, d) == pytest.approx(
        expected_L, abs=0.02
    )


@pytest.mark.parametrize("lam, mt", [(1.0, 1.217), (2.0, 1.709), (5.0, 3.606)])
def test_golden_folias_table_4_4_unchanged(lam, mt):
    D, t = 20.0, 0.5
    L = lam * math.sqrt(D * t) / 1.285
    assert Level2Engine._folias_factor(L, D, t) == pytest.approx(mt, rel=1e-3)
