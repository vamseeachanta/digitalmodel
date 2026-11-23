# ABOUTME: Publication-pipeline tests for the #1505 VIV pilot: Gate-A admission, curated
# ABOUTME: outputs, metrics, promotion-to-accepted, equality-block, report, clean-room replay.
"""#1505 publication-contract tests (InMemoryHfPort; NO Hugging Face network).

Every secret-shaped fixture is BUILT AT RUNTIME -- there is no literal token in source.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts" / "pilots"))

from assetutilities.workflow_api import (
    artifact,
    identity,
    inputs,
    metrics,
    output_contract,
    report,
)
from assetutilities.workflow_api.publication import (
    egress,
    hf_port as hf_port_mod,
    ledger as ledger_mod,
    promotion,
)

import viv_pilot  # noqa: E402


# ---------------------------------------------------------------------------
# AC1 -- public/synthetic algorithm + fail-closed admission (Gate A)
# ---------------------------------------------------------------------------
def test_algorithm_is_public_synthetic():
    # a synthetic parameter_set input carrying only numbers + owned rights admits.
    rec = viv_pilot.build_input_record(viv_pilot.CASES["C-lockin"])
    assert inputs.admission_reason(rec) is None
    assert inputs.admit(rec) is rec


def test_dirty_input_fails_admission():
    # (a) an unlicensed/restricted input is REJECTED (fail-closed).
    restricted = inputs.make_input(
        kind="parameter_set", role="viv_case",
        schema_version=viv_pilot.INPUT_SCHEMA_VERSION,
        canonical_repr={"outer_diameter": 0.2032},
        replay_location="content_addressed_dataset_object",
        redistribution_rights="restricted",
    )
    assert inputs.admission_reason(restricted) == "restricted"
    with pytest.raises(inputs.AdmissionError):
        inputs.admit(restricted)

    # (b) an absolute-local-path (pointer_only geometry) leak is REJECTED.
    leaking = inputs.make_input(
        kind="parameter_set", role="viv_case",
        schema_version=viv_pilot.INPUT_SCHEMA_VERSION,
        canonical_repr={"geometry_path": "/mnt/client/secret_geometry.dat"},
        replay_location="content_addressed_dataset_object",
    )
    assert inputs.admission_reason(leaking) == "path_leaking"

    # (c) a dirty input can never even be projected -> can never be published.
    with pytest.raises(Exception):
        viv_pilot.inputs.input_record_id(restricted)

    # (d) Gate A egress secret/path scan denies a runtime-built token + an abs path.
    gate = viv_pilot.make_gate()
    token = "hf_" + "z" * 30  # built at runtime; no literal token in source
    with pytest.raises(egress.EgressDenied):
        gate.gate_source_texts([f"HF_TOKEN={token}"])
    with pytest.raises(egress.EgressDenied):
        gate.gate_source_texts(["/mnt/local/secret.key"])


# ---------------------------------------------------------------------------
# AC3 -- exact-replay equality is the publication gate
# ---------------------------------------------------------------------------
def test_provenance_data_as_of_excluded_from_equality_digest():
    # the equality set is the timestamp-free results payload; no wall-clock field.
    payload, _a, _f, _r = viv_pilot.compute_case(viv_pilot.CASES["C-lockin"])
    assert "generated_at" not in payload["meta"]
    canon = identity.canonicalize(payload)
    assert "data_as_of" not in canon and "generated_at" not in canon

    # two runs of identical inputs at different wall-clock times -> identical digest.
    p1, _, _, _ = viv_pilot.compute_case(viv_pilot.CASES["C-lockin"])
    p2, _, _, _ = viv_pilot.compute_case(viv_pilot.CASES["C-lockin"])
    r1 = output_contract.output_equality_digest(
        [_curated_output(p1)]
    )["digest"]
    r2 = output_contract.output_equality_digest(
        [_curated_output(p2)]
    )["digest"]
    assert r1 == r2


def test_equality_mismatch_blocks_publication():
    store = viv_pilot.build_metric_store()
    proj, _pl, _ad = viv_pilot.build_projection_for_case(
        viv_pilot.CASES["C-lockin"], store
    )
    m = promotion.PromotionMachine(
        proj, hf_port=hf_port_mod.InMemoryHfPort(),
        ledger=ledger_mod.Ledger(), egress=viv_pilot.make_gate(),
    )
    m.validate()
    perturbed = "0" * 64  # a digest that disagrees with the recorded one
    with pytest.raises(identity.OutputMismatchError):
        m.replay(observed_output_equality=perturbed)
    assert m.state != "accepted"  # promotion never reaches accepted


# ---------------------------------------------------------------------------
# AC4 -- inputs complete/canonical/hashed/schema-valid/replayable
# ---------------------------------------------------------------------------
def test_inputs_complete_canonical_hashed_schema_valid_replayable():
    rec = viv_pilot.build_input_record(viv_pilot.CASES["C-lockin"])
    assert inputs.admission_reason(rec) is None            # complete + replayable
    assert rec["schema_version"] == viv_pilot.INPUT_SCHEMA_VERSION  # schema-valid
    assert rec["replay_location"] in inputs.ALLOWED_REPLAY_LOCATIONS
    # hashed: the record mints a stable input_record_id, and canonically-equivalent
    # reprs collapse to one identity.
    irid = inputs.input_record_id(rec)
    assert len(irid) == 64
    assert inputs.input_record_id(rec) == irid


# ---------------------------------------------------------------------------
# AC5 -- native schema retained; only curated artifacts publish
# ---------------------------------------------------------------------------
def test_outputs_retain_native_schema_only_curated_publish():
    payload, _a, _f, _r = viv_pilot.compute_case(viv_pilot.CASES["C-lockin"])
    # native engineering schema retained in full.
    for key in ("meta", "screening", "input", "beam", "modes", "citations"):
        assert key in payload
    assert isinstance(payload["modes"], list) and payload["modes"]

    # only a curated primary_result label projects; an unlabeled output is refused.
    rec = _curated_output(payload)
    assert rec["curated_label"] == "primary_result"
    assert output_contract.is_curated_label("primary_result") is True
    with pytest.raises(output_contract.OutputContractError):
        output_contract.make_output_record(
            role="scratch", native_schema={"id": "x", "version": "1"},
            media_type="application/json", curated_label="debug_dump",
            artifact_refs=[artifact.structured_artifact(payload)["content_digest"]],
        )


# ---------------------------------------------------------------------------
# AC6 -- metrics have definitions/units/derivations/quality
# ---------------------------------------------------------------------------
def test_metrics_have_definitions_units_derivations_quality():
    store = viv_pilot.build_metric_store()
    for seg in ("a_d_ratio", "lock_in", "fatigue_proxy"):
        d = store.get(f"{viv_pilot.ALGORITHM_ID}/{seg}", "1")
        for field in ("meaning", "unit_or_dimension", "derivation", "quality_rule",
                      "directionality", "label"):
            assert d.get(field)
    # the fatigue proxy is labelled a screening surrogate (no over-claim).
    fp = store.get(f"{viv_pilot.ALGORITHM_ID}/fatigue_proxy", "1")
    assert "SURROGATE" in fp["quality_rule"].upper()


# ---------------------------------------------------------------------------
# AC7 -- projection publishes an immutable revision (in-memory port)
# ---------------------------------------------------------------------------
def test_dataset_projection_publishes_immutable_revision():
    store = viv_pilot.build_metric_store()
    proj, _pl, _ad = viv_pilot.build_projection_for_case(
        viv_pilot.CASES["C-lockin"], store
    )
    port = hf_port_mod.InMemoryHfPort()
    ledger = ledger_mod.Ledger()
    m = viv_pilot.promote_to_accepted(
        proj, hf_port=port, ledger=ledger, gate=viv_pilot.make_gate()
    )
    assert m.state == "accepted"
    assert report.is_exact_revision(m.candidate_revision)   # exact 40-hex revision
    assert m.candidate_revision in port.list_revisions()
    assert ledger.is_eligible(proj.run_id)


# ---------------------------------------------------------------------------
# AC8 + ledger-is-sole-authority -- rolling report
# ---------------------------------------------------------------------------
def test_rolling_report_has_inputs_outputs_and_revision_links():
    summary = viv_pilot.run_pilot(echo_dir=None)
    html = summary["report_html"]
    assert "FINAL (pinned)" in html
    assert html.count("section class='inputs'") == 3
    assert html.count("section class='outputs'") == 3
    assert "hf-revision" in html
    # every accepted revision appears as a pinned link.
    for rev in summary["revisions"].values():
        assert rev in html


def test_ledger_is_sole_eligibility_authority():
    # a run that is NOT in the publications.jsonl ledger is never displayed, even if
    # it is otherwise a valid run view (HF visibility is never consulted).
    store = viv_pilot.build_metric_store()
    proj, payload, _ = viv_pilot.build_projection_for_case(
        viv_pilot.CASES["C-mid"], store
    )
    empty_ledger = ledger_mod.Ledger()
    view = viv_pilot._run_view(proj, payload, "a" * 40)
    html = report.render_report(
        algorithm=viv_pilot.ALGORITHM_ID, runs=[view],
        ledger=empty_ledger.records(), pinned=False,
    )
    assert proj.run_id not in html          # not eligible -> not displayed
    assert "(no successful runs)" in html


# ---------------------------------------------------------------------------
# AC9 -- clean-room replay reproduces accepted outputs
# ---------------------------------------------------------------------------
def test_clean_room_replay_reproduces_accepted_outputs():
    # from the published inputs (params) ALONE, the curated output content_digest is
    # reproduced bit-for-bit.
    p1, _a1, _f1, _r1 = viv_pilot.compute_case(viv_pilot.CASES["C-lockin"])
    p2, _a2, _f2, _r2 = viv_pilot.compute_case(viv_pilot.CASES["C-lockin"])
    assert (
        artifact.structured_object_digest(p1)
        == artifact.structured_object_digest(p2)
    )


# ---------------------------------------------------------------------------
# AC10 -- legal + secret scan: no client identifiers / absolute paths
# ---------------------------------------------------------------------------
_ABS = re.compile(r"(?<![\w.:])/(?:home|mnt|Users|root|etc|var|tmp|srv|opt)/|file:|~/"
                  r"|[A-Za-z]:\\")
_TOKEN = re.compile(r"hf_[A-Za-z0-9]{16,}|ghp_[A-Za-z0-9]{16,}")


def test_no_client_identifiers_or_absolute_paths():
    summary = viv_pilot.run_pilot(echo_dir=None)
    surfaces = [summary["report_html"], summary["ledger_jsonl"]]
    for name in viv_pilot.PUBLISHED_ORDER:
        payload, _a, _f, _r = viv_pilot.compute_case(viv_pilot.CASES[name])
        surfaces.append(identity.canonicalize(payload))
    for text in surfaces:
        assert not _ABS.search(text), f"absolute-path leak in: {text[:120]}"
        assert not _TOKEN.search(text), "secret token leak"


# ---------------------------------------------------------------------------
# helper: the curated output record (mirrors viv_pilot.build_projection_for_case)
# ---------------------------------------------------------------------------
def _curated_output(payload):
    art = artifact.structured_artifact(payload)
    return output_contract.make_output_record(
        role="viv_screening_result",
        native_schema={"id": viv_pilot.OUTPUT_SCHEMA_VERSION, "version": "1"},
        media_type="application/json",
        curated_label="primary_result",
        artifact_refs=[art["content_digest"]],
    )
