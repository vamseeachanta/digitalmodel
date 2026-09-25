"""Every declared FE crack state has a valid, provenance-checked receipt (#2157 P0).

Runs in CI without a licence and never skips. For every state declared in
``fe_states/declared_states.json`` (the P0a verification plate and every P0b
weldolet state) it checks:

* the receipt exists (a missing receipt fails) and passes the v2 schema;
* provenance (Codex P0a review, finding 1): ``run.producing_commit`` exists as a
  commit, is an ancestor of HEAD, the generator tree was clean at the solve, and
  every generator/parser file is byte-identical (git blob) between the producing
  commit and the checkout, unless a reviewed post-solve code delta is recorded;
* per-level run metadata (finding 2): argv, MAPDL version and solve seconds for
  every mesh level;
* committed sanitised solver output (finding 3): the digests match, and the
  receipt's reactions, front values and guard verdicts are re-derived from those
  artifacts;
* the deck hash of every mesh level is reproduced by the current generator.

A shallow clone is handled explicitly: a commit that is missing because of clone
depth is fetched by SHA; if it still cannot be reached, the failure message says
"unreachable in a shallow clone", which is distinct from "not a commit" (a fake
or unknown SHA in a full clone). CI checks out with ``fetch-depth: 0``.
"""

from __future__ import annotations

import json
import shutil
from pathlib import Path

import pytest

from digitalmodel.ansys import cint_parser, crack_receipt

REPO = Path(__file__).resolve().parents[2]
FE_STATES = REPO / "examples" / "workflows" / "crack-fe-weldolet" / "fe_states"
FIXTURES = Path(__file__).resolve().parent / "fixtures" / "receipts"


def _declared() -> list[str]:
    manifest = json.loads((FE_STATES / "declared_states.json").read_text("utf-8"))
    return [s["state"] for s in manifest["states"]]


DECLARED = _declared()


def _receipt(state: str) -> dict:
    path = FE_STATES / f"{state}.receipt.json"
    assert path.is_file(), f"declared state has no receipt: {path.name}"
    return json.loads(path.read_text(encoding="utf-8"))


def test_manifest_declares_p0a_and_p0b_states():
    assert "p0a_verification" in DECLARED
    assert len(DECLARED) == len(set(DECLARED))


def test_missing_receipt_fails(tmp_path: Path):
    manifest = {"states": [{"state": "not_solved_yet", "kind": "weldolet_crack"}]}
    (tmp_path / "declared_states.json").write_text(json.dumps(manifest), "utf-8")
    problems = crack_receipt.declared_state_problems(tmp_path)
    assert problems == ["declared state not_solved_yet has no receipt"]


@pytest.mark.parametrize("state", DECLARED)
def test_receipt_schema(state):
    assert cint_parser.validate_receipt_schema(_receipt(state)) == []


@pytest.mark.parametrize("state", DECLARED)
def test_receipt_provenance(state):
    deltas = crack_receipt.load_code_deltas(FE_STATES / "code_deltas.json")
    assert crack_receipt.provenance_problems(_receipt(state), REPO, deltas) == []


@pytest.mark.parametrize("state", DECLARED)
def test_receipt_run_metadata_per_level(state):
    receipt = _receipt(state)
    for mesh in receipt["meshes"]:
        run = mesh["run"]
        assert run["argv"] and run["argv"][0].lower().startswith(("mapdl", "ansys"))
        assert f"_L{mesh['level']}" in " ".join(run["argv"])
        assert run["mapdl_version"]
        assert run["solve_seconds"] > 0.0


@pytest.mark.parametrize("state", DECLARED)
def test_receipt_values_reparsed_from_committed_artifacts(state):
    assert crack_receipt.artifact_problems(_receipt(state), FE_STATES) == []


@pytest.mark.parametrize("state", DECLARED)
def test_receipt_deck_hash_matches_generator(state):
    receipt = _receipt(state)
    for mesh in receipt["meshes"]:
        current = crack_receipt.regenerate_deck_sha256(receipt, mesh["level"])
        assert current == mesh["deck_sha256"], (
            f"stale receipt {state}: level {mesh['level']} deck hash differs"
        )


@pytest.mark.parametrize("state", DECLARED)
def test_receipt_guards(state):
    receipt = _receipt(state)
    allowed_na = crack_receipt.NOT_APPLICABLE_GUARDS.get(receipt["kind"], ())
    recomputed = cint_parser.evaluate_receipt_guards(receipt)
    for name in cint_parser.GUARD_NAMES:
        stored = receipt["guards"][name]["status"]
        expected = "not_applicable" if name in allowed_na else "pass"
        assert stored == expected, (name, receipt["guards"][name])
        assert recomputed[name].status == expected, (name, recomputed[name])


@pytest.mark.parametrize("state", DECLARED)
def test_receipt_and_artifacts_host_free(state):
    path = FE_STATES / f"{state}.receipt.json"
    assert cint_parser.find_host_tokens(path.read_text(encoding="utf-8")) == []
    for mesh in _receipt(state)["meshes"]:
        for art in mesh["artifacts"].values():
            text = (FE_STATES / art["path"]).read_text(encoding="utf-8")
            assert cint_parser.find_host_tokens(text) == [], art["path"]


def _kinds() -> dict[str, str]:
    manifest = json.loads((FE_STATES / "declared_states.json").read_text("utf-8"))
    return {s["state"]: s["kind"] for s in manifest["states"]}


@pytest.mark.parametrize("state", DECLARED)
def test_manifest_kind_matches_receipt(state):
    assert _receipt(state)["kind"] == _kinds()[state]


UNCRACKED = [s for s, k in _kinds().items() if k == "weldolet_uncracked"]


@pytest.mark.parametrize("state", UNCRACKED)
def test_uncracked_far_field_hoop_plausibility(state):
    """Pre-stated comparator (Barlow, ID form, p D_i / 2t) within 2 % at the mean
    radius, away from the fitting (x = 100 mm, psi = 180 deg)."""
    hoop = _receipt(state)["plausibility"]["hoop"]
    assert hoop["comparator"] == "barlow_id_hoop_mpa"
    assert hoop["tolerance"] == 0.02
    assert hoop["radius_offset_from_rm_mm"] == pytest.approx(0.0, abs=1e-6)
    assert abs(hoop["fe_hoop_mpa"] / hoop["comparator_mpa"] - 1.0) <= 0.02
    assert hoop["within_tolerance"] is True


CRACKED_WELDOLET = [s for s, k in _kinds().items() if k == "weldolet_crack"]


@pytest.mark.parametrize("state", CRACKED_WELDOLET)
def test_start_node_correction_is_consistent(state):
    """K at front node 1 comes from the second SIFS definition; both SIFS
    definitions must agree at the reference node after the sign factors."""
    for mesh in _receipt(state)["meshes"]:
        audit = mesh["start_node_audit"]
        assert audit["ref_max_rel_diff"] <= 1e-6
        assert audit["within_tolerance"] is True


@pytest.mark.parametrize("state", UNCRACKED)
def test_uncracked_axial_reaction_equals_end_thrust(state):
    receipt = _receipt(state)
    from digitalmodel.ansys import weldolet_crack

    thrust = weldolet_crack.closed_forms(weldolet_crack.spec_from_receipt(receipt, 0))[
        "end_thrust_n"
    ]
    for mesh in receipt["meshes"]:
        rf = mesh["reactions"]["reaction_sum_n"]
        assert abs(rf + thrust) / thrust <= 0.005


# --------------------------------------------------------------------------- #
# Stop-rule evidence (P0b stopped at a0: guard (c) on the refinement pairs)
# --------------------------------------------------------------------------- #
EVIDENCE = FE_STATES / "stop_rule_evidence"
EVIDENCE_PAIRS = ("L0L1", "L1L2")


@pytest.mark.parametrize("pair", EVIDENCE_PAIRS)
def test_stop_rule_evidence_is_genuine_and_fails_only_guard_c(pair):
    """The undeclared a0 receipts that triggered the stop rule are kept as
    evidence. They predate owner cards G13/G14, so they are checked as
    historical records: schema (apart from the later guard (g)), the producing
    commit and its generator blobs, digests and re-parsed artifacts, the deck
    hashes; under the definitions of their time guard (c) is the only failure."""
    base = EVIDENCE / pair
    receipt = json.loads((base / "p0b_fullcirc_a2p35.receipt.json").read_text("utf-8"))
    problems = cint_parser.validate_receipt_schema(receipt)
    assert problems == ["guards: missing ['g_j_mesh']"]
    assert crack_receipt.provenance_problems(receipt, REPO, [], historical=True) == []
    assert crack_receipt.artifact_problems(receipt, base, historical=True) == []
    for mesh in receipt["meshes"]:
        current = crack_receipt.regenerate_deck_sha256(receipt, mesh["level"])
        assert current == mesh["deck_sha256"]
    failed = sorted(n for n, g in receipt["guards"].items() if g["status"] != "pass")
    assert failed == ["c_contour"]


@pytest.mark.parametrize("pair", EVIDENCE_PAIRS)
def test_stop_rule_evidence_under_owner_g13_g14(pair):
    """Re-evaluated from the committed artifacts under the redefined guard (c)
    and the new guard (g), the same a0 solves pass every gating guard; the
    legacy metric is still the failure it was."""
    base = EVIDENCE / pair
    receipt = json.loads((base / "p0b_fullcirc_a2p35.receipt.json").read_text("utf-8"))
    cint, reac = {}, {}
    for mesh in receipt["meshes"]:
        cint[mesh["level"]] = (base / mesh["artifacts"]["cint"]["path"]).read_text("utf-8")
        reac[mesh["level"]] = (base / mesh["artifacts"]["reac"]["path"]).read_text("utf-8")
    res = cint_parser.evaluate_guards(cint, reac, front_geometry={"type": "polar_z"})
    for name in cint_parser.GUARD_NAMES:
        assert res[name].status == "pass", (name, res[name])
    assert res["c_contour_legacy"].status == "fail"


CROTCH_EVIDENCE = EVIDENCE / "crotch"


def test_crotch_stop_rule_evidence_fails_only_guard_g():
    """Crotch-plane a0 (owner card G15), solved at the current generator: the
    (L1, L2) receipt is genuine and current, and guard (g) is its only failure
    (J at the governing free-surface end node changes by more than 1 %)."""
    receipt = json.loads(
        (CROTCH_EVIDENCE / "p0b_crotch_a2p35.receipt.json").read_text("utf-8")
    )
    assert cint_parser.validate_receipt_schema(receipt) == []
    assert crack_receipt.provenance_problems(receipt, REPO, []) == []
    assert crack_receipt.artifact_problems(receipt, CROTCH_EVIDENCE) == []
    for mesh in receipt["meshes"]:
        assert crack_receipt.regenerate_deck_sha256(receipt, mesh["level"]) == mesh[
            "deck_sha256"
        ]
    assert [m["level"] for m in receipt["meshes"]] == [1, 2]
    failed = sorted(n for n in cint_parser.GUARD_NAMES
                    if receipt["guards"][n]["status"] != "pass")
    assert failed == ["g_j_mesh"]
    assert receipt["guards"]["g_j_mesh"]["value"] > 0.01
    assert "p0b_crotch_a2p35" not in DECLARED


def test_crotch_first_pair_also_fails_only_guard_g():
    """(L0, L1) re-evaluated from the committed artifacts: again only (g)."""
    base = CROTCH_EVIDENCE / "solved" / "p0b_crotch_a2p35"
    receipt = json.loads(
        (CROTCH_EVIDENCE / "p0b_crotch_a2p35.receipt.json").read_text("utf-8")
    )
    cint = {lv: (base / f"weldolet_cint_L{lv}.txt").read_text("utf-8") for lv in (0, 1)}
    reac = {lv: (base / f"weldolet_reac_L{lv}.txt").read_text("utf-8") for lv in (0, 1)}
    res = cint_parser.evaluate_guards(cint, reac, front_geometry=receipt["front_geometry"])
    failed = sorted(n for n in cint_parser.GUARD_NAMES if res[n].status != "pass")
    assert failed == ["g_j_mesh"]
    assert res["g_j_mesh"].value > 0.01


# --------------------------------------------------------------------------- #
# Negative fixtures: each check fails for its own reason
# --------------------------------------------------------------------------- #
def test_fake_commit_receipt_fails_provenance():
    fake = json.loads((FIXTURES / "neg_fake_commit.receipt.json").read_text("utf-8"))
    problems = crack_receipt.provenance_problems(fake, REPO, [])
    joined = " | ".join(problems)
    assert "generator_tree_clean is not true" in joined
    status = crack_receipt.git_commit_status(fake["run"]["producing_commit"], REPO)
    if crack_receipt.is_shallow(REPO):
        assert status in ("absent", "unreachable_shallow")
    else:
        assert status == "absent"
        assert "is not a commit in this repository" in joined
        assert "shallow" not in joined


def test_commit_status_of_head_is_present():
    head = crack_receipt.git(REPO, "rev-parse", "HEAD")
    assert crack_receipt.git_commit_status(head, REPO) == "present"


def test_changed_generator_file_needs_a_recorded_delta():
    receipt = _receipt("p0a_verification")
    doctored = json.loads(json.dumps(receipt))
    path = next(iter(doctored["run"]["generator_files"]))
    doctored["run"]["generator_files"][path] = "0" * 40
    problems = crack_receipt.provenance_problems(doctored, REPO, [])
    assert any("recorded blob" in p for p in problems)


def test_tampered_artifact_detected(tmp_path: Path):
    receipt = _receipt("p0a_verification")
    work = tmp_path / "fe_states"
    for mesh in receipt["meshes"]:
        for art in mesh["artifacts"].values():
            dst = work / art["path"]
            dst.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(FE_STATES / art["path"], dst)
    assert crack_receipt.artifact_problems(receipt, work) == []
    art = receipt["meshes"][0]["artifacts"]["reac"]
    target = work / art["path"]
    text = target.read_text("utf-8")
    lines = text.splitlines(keepends=True)
    idx = next(i for i, ln in enumerate(lines) if ln.split()[:1] == ["fz_ligament"])
    lines[idx] = "fz_ligament -7.990000000000E+04\n"
    target.write_text("".join(lines), "utf-8")
    problems = crack_receipt.artifact_problems(receipt, work)
    assert any("sha256" in p for p in problems)
    # even with the digest updated, the values no longer match the receipt
    doctored = json.loads(json.dumps(receipt))
    doctored["meshes"][0]["artifacts"]["reac"]["sha256"] = crack_receipt.text_sha256(
        target.read_text("utf-8")
    )
    problems = crack_receipt.artifact_problems(doctored, work)
    assert any("reaction" in p for p in problems)
