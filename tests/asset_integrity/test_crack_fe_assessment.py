# ABOUTME: Tests for the #2157 P3 crack-like-flaw coordinator: shared FFS result Protocol,
# ABOUTME: evidence completeness, and the weldolet benchmark against closed forms.
"""Tests for ``crack_fe_assessment`` (#2157 P3; owner cards G11, G03, B02, S02, B14).

Comparator classes:

- contract: the Protocol, ``passes`` gated on evidence, the required evidence items;
- closed form: Lr and Kr from the receipt values, the limit-load Lr, the flow-rule
  cut-off, the growth life of a piecewise-linear DeltaK(a) (exact integral for m = 3),
  the DeltaK multiplier and threshold margins, the linear-extrapolation sensitivity and
  the screening bounds;
- invariant: the envelope contact point lies on the curve.

The numeric tests use an injected receipt validator so they run in seconds; the
receipt-validation path itself is tested separately against the committed receipts.
"""

from __future__ import annotations

import copy
import json
import math
import re
from pathlib import Path

import pytest
import yaml

from digitalmodel.asset_integrity.assessment import FFSAssessmentResult
from digitalmodel.asset_integrity.assessment import crack_fe_assessment as cfa
from digitalmodel.asset_integrity.assessment.crack_fe_assessment import (
    EVIDENCE_ITEMS,
    CrackAssessmentResult,
    EvidenceItem,
    FFSResultProtocol,
    evidence_status,
    load_case,
    run,
)
from digitalmodel.asset_integrity.assessment.fad_curves import api579_2016_level2

REPO = Path(__file__).resolve().parents[2]
EXAMPLE = REPO / "examples" / "workflows" / "crack-fe-weldolet"
FE_STATES = EXAMPLE / "fe_states"
INPUT = EXAMPLE / "input.yml"

SIGMA_Y, SIGMA_U, KMAT, E_T = 127.0, 385.0, 132.0, 182.5
A_N_MM, M, E_REF, DK_TH, DEMAND = 5.21e-13, 3.0, 200.0, 2.0, 20_000.0


def _receipt(state: str) -> dict:
    return json.loads((FE_STATES / f"{state}.receipt.json").read_text("utf-8"))


def _trust_all(*_args, **_kwargs) -> list[str]:
    return []


@pytest.fixture(scope="module")
def case() -> dict:
    return load_case(INPUT)


@pytest.fixture(scope="module")
def result(case) -> CrackAssessmentResult:
    return cfa._assess(case, _trust_all)


def _depth(result, a):
    return next(d for d in result.depths if d["a_mm"] == pytest.approx(a))


# --------------------------------------------------------------------------- #
# Contract: the shared Protocol and the evidence gate (owner card G11)
# --------------------------------------------------------------------------- #
def _crack(verdict="ACCEPT", status="COMPLETE") -> CrackAssessmentResult:
    items = {n: EvidenceItem(n, status == "COMPLETE", "test") for n in EVIDENCE_ITEMS}
    return CrackAssessmentResult(
        component_id="C-1",
        verdict=verdict,
        evidence_status=status,
        evidence={n: i.to_dict() for n, i in items.items()},
        missing_evidence=[] if status == "COMPLETE" else list(EVIDENCE_ITEMS),
    )


def test_both_result_types_satisfy_the_protocol():
    crack = _crack()
    assert isinstance(crack, FFSResultProtocol)
    ml = FFSAssessmentResult(
        component_id="X", assessment_type="GML", level_reached=1, t_nominal_in=0.5,
        t_min_in=0.2, t_measured_min_in=0.4, t_measured_avg_in=0.45, fca_in=0.0,
        rsf=1.0, rsf_a=0.9, folias_factor=1.0, remaining_life_yr=10.0,
        verdict="ACCEPT", rerated_pressure_psi=1000.0, sufficiency_status="SUFFICIENT",
    )
    assert isinstance(ml, FFSResultProtocol)
    for r in (crack, ml):
        d = r.to_dict()
        for key in ("component_id", "assessment_type", "verdict", "passes",
                    "code_reference"):
            assert key in d


def test_crack_result_is_type_crack():
    crack = _crack()
    assert crack.assessment_type == "CRACK"
    assert crack.to_dict()["assessment_type"] == "CRACK"
    with pytest.raises(ValueError):
        CrackAssessmentResult(
            component_id="C", verdict="ACCEPT", evidence_status="COMPLETE",
            evidence={}, missing_evidence=[], assessment_type="LML",
        )


@pytest.mark.parametrize("verdict", ["ACCEPT", "MONITOR", "REPAIR"])
def test_crack_incomplete_evidence_never_passes(verdict):
    assert _crack(verdict, "INCOMPLETE").passes is False
    assert _crack(verdict, "INCOMPLETE").to_dict()["passes"] is False


def test_passes_needs_accept_or_monitor_and_complete_evidence():
    assert _crack("ACCEPT", "COMPLETE").passes is True
    assert _crack("MONITOR", "COMPLETE").passes is True
    assert _crack("REPAIR", "COMPLETE").passes is False


def test_evidence_status_and_verdict_values_are_closed_sets():
    with pytest.raises(ValueError):
        _crack("ACCEPT", "PARTIAL")
    with pytest.raises(ValueError):
        _crack("SAFE", "COMPLETE")


def test_complete_evidence_requires_all_bases():
    full = {n: EvidenceItem(n, True, "stated") for n in EVIDENCE_ITEMS}
    assert evidence_status(full) == ("COMPLETE", [])
    assert set(EVIDENCE_ITEMS) == {
        "kmat_basis", "lr_max_basis", "residual_stress_basis", "psf_basis",
        "geometry_validity", "fe_receipts",
    }
    for name in EVIDENCE_ITEMS:
        partial = dict(full)
        partial[name] = EvidenceItem(name, False, "absent")
        assert evidence_status(partial) == ("INCOMPLETE", [name])
        dropped = {k: v for k, v in full.items() if k != name}
        assert evidence_status(dropped) == ("INCOMPLETE", [name])


# --------------------------------------------------------------------------- #
# Case loading: every input has a register id or a basis
# --------------------------------------------------------------------------- #
def test_every_input_has_a_register_id_or_basis(case):
    def walk(node, where):
        if isinstance(node, dict):
            if "value" in node or "A" in node or "dk_th" in node:
                assert node.get("register_id") or str(node.get("basis", "")).strip(), where
            for k, v in node.items():
                walk(v, f"{where}.{k}")
        elif isinstance(node, list):
            for i, v in enumerate(node):
                walk(v, f"{where}[{i}]")

    raw = yaml.safe_load(INPUT.read_text("utf-8"))["crack_fe_assessment"]
    walk(raw, "crack_fe_assessment")
    for key in ("sigma_y", "sigma_u", "youngs_modulus", "poisson", "kmat"):
        assert raw["material"][key]["register_id"]


def test_inline_inputs_carry_the_assumed_label(case):
    raw = yaml.safe_load(INPUT.read_text("utf-8"))["crack_fe_assessment"]
    for node in (raw["growth"]["law"], raw["growth"]["threshold"],
                 raw["growth"]["demand_cycles"], raw["checks"]["ssy_max_ratio"],
                 raw["residual_screening"]):
        assert node["status_label"] == "ASSUMED - to be confirmed"
        assert node["basis"].startswith("ASSUMED - to be confirmed:")


# --------------------------------------------------------------------------- #
# Benchmark: FAD at each declared depth (owner cards B02, S02)
# --------------------------------------------------------------------------- #
def _sigma_ref_lin(plane: str) -> tuple[float, int]:
    paths = _receipt("p0b_uncracked")["sigma_ref"]["paths"]
    if plane == "crotch":
        cands = [(p["hoop_sigma_m_mpa"] + p["hoop_sigma_b_mpa"], p["path"])
                 for p in paths if p["path"] in (1, 4)]
    else:
        cands = [(p["sigma_m_mpa"] + p["sigma_b_mpa"], p["path"])
                 for p in paths if p["path"] in (1, 2, 3)]
    return max(cands)


CROTCH = {2.35: "p0b_crotch_a2p35", 2.8: "p0b_crotch_a2p80", 3.2: "p0b_crotch_a3p20"}


def test_depths_and_governing_plane(result):
    assert [d["a_mm"] for d in result.depths] == pytest.approx([2.35, 2.8, 3.2, 3.6, 4.0])
    for a, state in CROTCH.items():
        d = _depth(result, a)
        assert d["status"] == "established"
        assert d["governing_plane"] == "crotch"
        assert d["governing_state"] == state
        k_ff = _receipt(state.replace("crotch", "fullcirc"))["governing"][
            "k_gov_max_mpa_sqrt_m"]
        assert d["k_gov_mpa_sqrt_m"] > k_ff
    assert _depth(result, 3.6)["status"] == "governing_plane_not_established"
    assert _depth(result, 4.0)["status"] == "beyond_limit_state"
    assert "crotch" in _depth(result, 4.0)["reason"]


def test_lr_linearised_is_membrane_plus_bending_over_sigma_y(result):
    sig, path = _sigma_ref_lin("crotch")
    assert path == 4
    for a in CROTCH:
        d = _depth(result, a)
        assert d["sigma_ref_mpa"] == pytest.approx(sig, rel=1e-12)
        assert d["sigma_ref_path"] == 4
        assert d["lr"] == pytest.approx(sig / SIGMA_Y, rel=1e-12)
    assert "no net-section amplification" in result.basis["sigma_ref"]
    assert "sigma_m + sigma_b" in result.basis["sigma_ref"]


def test_kr_is_k_gov_over_kmat(result):
    for a, state in CROTCH.items():
        k = _receipt(state)["governing"]["k_gov_max_mpa_sqrt_m"]
        d = _depth(result, a)
        assert d["k_gov_mpa_sqrt_m"] == pytest.approx(k, rel=1e-12)
        assert d["kr"] == pytest.approx(k / KMAT, rel=1e-12)


def test_lr_max_flow_rule(result):
    assert result.basis["lr_max"]["value"] == pytest.approx((127 + 385) / 254, rel=1e-12)
    assert result.basis["lr_max"]["rule"] == "flow"


def test_envelope_margin_contact_lies_on_the_curve(result):
    lr_cut = (SIGMA_Y + SIGMA_U) / (2 * SIGMA_Y)
    for a in CROTCH:
        d = _depth(result, a)
        m = d["envelope_margin"]
        assert m["factor"] > 1.0
        if m["mode"] == "curve":
            assert api579_2016_level2(m["contact_lr"], lr_cut) == pytest.approx(
                m["contact_kr"], abs=1e-9)
        assert m["contact_lr"] == pytest.approx(m["factor"] * d["lr"], rel=1e-12)
        assert d["fad_inside"] is True


def test_limit_load_lr_sensitivity(result):
    ll = _receipt("p0b_limit_load_crotch_a2p35")["limit_load"]
    s = result.sensitivities["limit_load_lr"]
    assert s["label"] == "SENSITIVITY"
    assert s["a_mm"] == pytest.approx(2.35)
    assert s["lr"] == pytest.approx(4.7 / ll["p_limit_mpa"], rel=1e-12)
    assert round(s["lr"], 3) == 0.384
    assert s["envelope_margin"]["factor"] > _depth(result, 2.35)["envelope_margin"]["factor"]


# --------------------------------------------------------------------------- #
# Benchmark: growth (owner card B14; closed form for piecewise-linear DeltaK, m = 3)
# --------------------------------------------------------------------------- #
def _a_eff() -> float:
    return A_N_MM * 1000.0**1.5 * (E_REF / E_T) ** 3


def _segment_life(a1, a2, k1, k2, a_eff):
    """Exact integral of da / (A (k1 + s (a - a1))^3) over [a1, a2]."""
    s = (k2 - k1) / (a2 - a1)
    return (1.0 / (a_eff * s)) * (0.5 / k1**2 - 0.5 / k2**2)


def _crotch_table():
    a = sorted(CROTCH)
    k = [_receipt(CROTCH[x])["governing"]["k_gov_max_mpa_sqrt_m"] for x in a]
    return a, k


def test_growth_law_conversion_and_e_ratio(result):
    law = result.growth["law"]
    assert law["A_mpa_sqrt_m"] == pytest.approx(_a_eff(), rel=1e-12)
    assert law["m"] == 3.0
    assert "ASSUMED - to be confirmed" in law["status_label"]


def test_life_to_last_fe_state_closed_form(result):
    a, k = _crotch_table()
    n = sum(_segment_life(a[i], a[i + 1], k[i], k[i + 1], _a_eff()) for i in range(2))
    g = result.growth
    assert g["a0_mm"] == pytest.approx(2.35)
    assert g["a_last_fe_mm"] == pytest.approx(3.2)
    assert g["delta_k_basis"].startswith("DeltaK = (1 - R) K_gov")
    for rule in ("none", "e_ratio"):
        life = g["life_to_last_fe_state"][rule]
        assert life["status"] == "GROWS"
        assert life["cycles"] == pytest.approx(n, rel=1e-7)
        assert life["margin_on_demand"] == pytest.approx(n / DEMAND, rel=1e-7)
        # the threshold is not reached, so the multiplier equals the closed form
        assert life["dk_multiplier_to_demand"] == pytest.approx(
            (n / DEMAND) ** (1.0 / 3.0), rel=1e-7)
    assert g["demand_cycles"] == DEMAND


def test_threshold_margins_under_both_rules(result):
    _, k = _crotch_table()
    tm = result.growth["threshold_margin"]
    assert tm["none"]["dk_th_effective"] == pytest.approx(2.0)
    assert tm["e_ratio"]["dk_th_effective"] == pytest.approx(2.0 * E_T / E_REF)
    assert tm["none"]["margin"] == pytest.approx(1.0 - 2.0 / k[0], rel=1e-9)
    assert tm["e_ratio"]["margin"] == pytest.approx(1.0 - 2.0 * E_T / E_REF / k[0],
                                                    rel=1e-9)


def test_extrapolated_life_to_ligament_exhaustion_is_a_labelled_sensitivity(result):
    a, k = _crotch_table()
    s = (k[2] - k[1]) / (a[2] - a[1])
    k_end = k[2] + s * (3.805 - 3.2)
    n = sum(_segment_life(a[i], a[i + 1], k[i], k[i + 1], _a_eff()) for i in range(2))
    n += _segment_life(3.2, 3.805, k[2], k_end, _a_eff())
    sens = result.sensitivities["life_to_ligament_exhaustion"]
    assert sens["label"] == "SENSITIVITY"
    assert sens["a_limit_mm"] == pytest.approx(3.805)
    assert "linear extrapolation" in sens["basis"]
    assert sens["cycles"] == pytest.approx(n, rel=1e-7)
    # the base result never extrapolates
    assert "cycles" not in result.growth.get("life_to_limit_state", {})


def test_fusion_face_plane_is_arrested_below_threshold(result):
    ff = result.growth["non_governing_planes"]["fusion_face"]
    for rule in ("none", "e_ratio"):
        assert ff[rule]["status"] == "ARRESTED"


# --------------------------------------------------------------------------- #
# Benchmark: consistency checks, screening, citations
# --------------------------------------------------------------------------- #
def test_consistency_checks_present_and_stated(result):
    c = result.checks
    for a in CROTCH:
        key = f"{a:.2f}"
        assert "passed" in c["sigma_ref_consistency"][key]
        ssy = c["ssy"][key]
        assert ssy["max_ratio"] == 0.2
        lig = _receipt(CROTCH[a])["crack"]["remaining_ligament_mm"]
        k = _receipt(CROTCH[a])["governing"]["k_gov_max_mpa_sqrt_m"]
        rp = (1 / (2 * math.pi)) * (k / SIGMA_Y) ** 2 * 1000.0
        assert ssy["ratio"] == pytest.approx(rp / lig, rel=1e-12)
        assert ssy["passed"] is (rp / lig <= 0.2)
    sd = c["shakedown"]
    assert sd["status"] in ("EVALUATED", "Not Evaluated")
    if sd["status"] == "EVALUATED":
        peak = max(p["peak_shh_mpa"] for p in _receipt("p0b_uncracked")["sigma_ref"]["paths"]
                   if p["path"] in (1, 4))
        assert sd["ratio"] == pytest.approx(peak / (2 * SIGMA_Y), rel=1e-12)
        assert sd["basis"]
    else:
        assert sd["reason"]
    gv = c["growth_validity"]
    assert gv["status"] in ("VALID", "CONDITIONAL")
    assert gv["lr_max_seen"] == pytest.approx(_sigma_ref_lin("crotch")[0] / SIGMA_Y)


def test_residual_screening_bounds_at_a0(result):
    sb = result.screening["residual_bounds"]
    assert result.screening["rho"] == 0.0
    assert "rho = 0" in result.screening["basis"]
    k_p = _receipt("p0b_crotch_a2p35")["governing"]["k_gov_max_mpa_sqrt_m"]
    root = math.sqrt(math.pi * 2.35 / 1000.0)
    expected = {"relaxed_yield": 0.5 * SIGMA_Y, "yield": SIGMA_Y,
                "flow": 0.5 * (SIGMA_Y + SIGMA_U)}
    assert [b["label"] for b in sb] == list(expected)
    for b in sb:
        assert b["kind"] == "screening"
        ks = 1.12 * expected[b["label"]] * root
        assert b["k_secondary"] == pytest.approx(ks, rel=1e-12)
        assert b["kr"] == pytest.approx((k_p + ks) / KMAT, rel=1e-12)


def test_payload_contains_citations(result):
    cites = result.to_dict()["citations"]
    ids = {c["code_id"]: c for c in cites}
    assert set(ids) == {"api-std-579-asme-ffs-1", "bs-7910"}
    assert ids["api-std-579-asme-ffs-1"]["revision"] == "2016"
    assert ids["bs-7910"]["revision"] == "2013"
    assert all(c["source_sibling"] == "generic" for c in cites)


# --------------------------------------------------------------------------- #
# Benchmark: evidence and verdict (plan P3; owner card G03)
# --------------------------------------------------------------------------- #
def test_benchmark_evidence_is_incomplete_with_named_items(result):
    assert result.evidence_status == "INCOMPLETE"
    assert result.missing_evidence == ["kmat_basis", "residual_stress_basis", "psf_basis"]
    assert "M-06" in result.evidence["kmat_basis"]["basis"]
    assert result.evidence["lr_max_basis"]["established"] is True
    assert result.evidence["geometry_validity"]["established"] is True
    assert result.evidence["fe_receipts"]["established"] is True  # trusted in this test
    assert result.passes is False
    assert result.to_dict()["passes"] is False


def test_verdict_and_findings(result):
    assert result.verdict in ("ACCEPT", "MONITOR")
    assert result.engineering["fad_inside_all_established_depths"] is True
    assert result.engineering["life_exceeds_demand"] is True
    for f in result.findings:
        assert set(f) >= {"id", "criterion", "comparator", "value", "disposition"}
    ids = [f["id"] for f in result.findings]
    assert len(ids) == len(set(ids))
    for name in result.missing_evidence:
        assert f"evidence.{name}" in ids


def test_payload_is_serialisable_and_register_clean(result):
    payload = result.to_dict()
    text = json.dumps(payload, allow_nan=False)
    yaml.safe_dump(payload)
    assert not re.search(r"\b(safe|conservative|acceptable)\b", text, re.IGNORECASE)
    assert payload["evidence_status"] == "INCOMPLETE"
    assert payload["design_basis_status"].startswith("ASSUMED - to be confirmed")


# --------------------------------------------------------------------------- #
# Evidence reaches COMPLETE only when every basis is supplied
# --------------------------------------------------------------------------- #
def _completed_case(case: dict) -> dict:
    c = copy.deepcopy(case)
    c["material"]["kmat"] = {
        "value": 132.0, "unit": "MPa*sqrt(m)", "source_class": "public",
        "basis": "test-only stand-in for a toughness test report",
    }
    c["residual_stress"] = {
        "method": "bs7910_rho", "value": 0.0, "source_class": "public",
        "basis": "test-only stand-in for a residual-stress measurement",
        "profile": {"kind": "uniform", "stress_mpa": 10.0, "y": 1.12,
                    "relaxation": 1.0},
    }
    c["partial_safety_factors"] = {
        "stress": 1.0, "kmat": 1.0, "source_class": "public",
        "basis": "test-only stand-in for a PSF basis",
    }
    return c


def test_complete_case_passes_only_with_every_basis(case):
    res = cfa._assess(_completed_case(case), _trust_all)
    assert res.evidence_status == "COMPLETE"
    assert res.missing_evidence == []
    assert res.passes is (res.verdict in ("ACCEPT", "MONITOR"))
    # residual stress now enters Kr through the named method
    d = next(d for d in res.depths if d["a_mm"] == pytest.approx(2.35))
    ks = 1.12 * 10.0 * math.sqrt(math.pi * 2.35 / 1000.0)
    assert d["kr"] == pytest.approx((d["k_gov_mpa_sqrt_m"] + ks) / KMAT, rel=1e-12)


def test_assumed_basis_does_not_establish_evidence(case):
    c = _completed_case(case)
    c["partial_safety_factors"]["source_class"] = "assumed"
    res = cfa._assess(c, _trust_all)
    assert res.missing_evidence == ["psf_basis"]
    assert res.passes is False


def test_receipt_problem_makes_evidence_incomplete(case):
    def one_bad(receipt, *_a, **_k):
        return ["tampered"] if receipt["state"] == "p0b_crotch_a2p80" else []

    res = cfa._assess(_completed_case(case), one_bad)
    assert res.missing_evidence == ["fe_receipts"]
    assert "p0b_crotch_a2p80" in res.evidence["fe_receipts"]["basis"]
    assert res.passes is False


# --------------------------------------------------------------------------- #
# Receipt validation against the committed receipts
# --------------------------------------------------------------------------- #
def test_receipt_validator_accepts_the_committed_limit_load_receipt():
    r = _receipt("p0b_limit_load_crotch_a2p35")
    assert cfa.receipt_problems(r, FE_STATES, REPO) == []


def test_receipt_validator_rejects_a_failed_gating_guard():
    r = _receipt("p0b_limit_load_crotch_a2p35")
    r["guards"]["a_equilibrium"]["status"] = "fail"
    problems = cfa.receipt_problems(r, FE_STATES, REPO)
    assert any("a_equilibrium" in p for p in problems)


def test_receipt_validator_rejects_a_stale_provenance():
    r = _receipt("p0b_limit_load_crotch_a2p35")
    path = next(iter(r["run"]["generator_files"]))
    r["run"]["generator_files"][path] = "0" * 40
    assert any("recorded blob" in p for p in cfa.receipt_problems(r, FE_STATES, REPO))


def test_missing_declared_receipt_is_a_problem(tmp_path, case):
    (tmp_path / "declared_states.json").write_text(
        json.dumps({"states": [{"state": "not_solved", "kind": "weldolet_crack"}]}),
        "utf-8")
    problems = cfa.declared_receipt_problems(tmp_path, REPO, validator=_trust_all)
    assert problems == {"not_solved": ["declared state not_solved has no receipt"]}


# --------------------------------------------------------------------------- #
# Codex P3 review regressions
# --------------------------------------------------------------------------- #
def test_public_run_cannot_take_a_validator():
    import inspect

    assert list(inspect.signature(run).parameters) == ["case"]


def test_public_run_rejects_a_tampered_receipt(case, tmp_path):
    import shutil

    fe_copy = tmp_path / "fe_states"
    shutil.copytree(FE_STATES, fe_copy)
    target = fe_copy / "p0b_crotch_a2p80.receipt.json"
    rec = json.loads(target.read_text("utf-8"))
    guard = next(iter(rec["guards"]))
    rec["guards"][guard]["status"] = "fail"
    target.write_text(json.dumps(rec, indent=2), encoding="utf-8")
    c = _completed_case(case)
    c["fe_states_dir"] = str(fe_copy)
    res = run(c)
    assert res.evidence_status == "INCOMPLETE"
    assert "fe_receipts" in res.missing_evidence
    assert res.passes is False


def test_base_depth_rows_carry_no_extrapolated_life(result):
    for d in result.depths:
        assert "extrapolated_life_to_limit_cycles" not in d
    sens = result.sensitivities["life_to_ligament_exhaustion"]
    assert sens["label"] == "SENSITIVITY"
    assert [x["a_mm"] for x in sens["remaining_by_depth"]] == pytest.approx([2.35, 2.8, 3.2])
