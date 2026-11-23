# ABOUTME: Determinism golden harness tests (workspace-hub#3283) — stamp_provenance,
# ABOUTME: KEY-allowlist pruning, golden_workflow_test verdict, + buckling reference golden.
"""Tests for the #3283 determinism golden harness.

The first groups (provenance + KEY-allowlist + golden-diff/verdict) run over
SYNTHETIC envelopes via an injected fake runner -- no heavy engine import. The final
reference test drives the REAL ``run_workflow`` on the BARE in-repo
``buckling-parametric`` id and asserts the emitted ``result_hash`` against #3285's
committed golden (the only test in this file that imports the digitalmodel engine).
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from assetutilities.workflow_api import ResultEnvelope

from digitalmodel.workflow_api.golden import (
    GOLDEN_VOLATILE_KEYS,
    _prune_volatile,
    capture_golden,
    diff_results,
    golden_workflow_test,
)
from digitalmodel.workflow_api.provenance import stamp_provenance

GOLDENS = Path(__file__).resolve().parent / "goldens"


# ── synthetic-envelope helpers ────────────────────────────────────────────────
def _make_envelope(
    *,
    result_hash="hash-abc",
    result=None,
    git_sha="deadbeef",
    package_version="1.2.3",
    data_as_of="2026-06-28T00:00:00Z",
    reproducible=None,
    workflow_id="synthetic",
):
    return ResultEnvelope(
        workflow_id=workflow_id,
        status="ok",
        result=result if result is not None else {"kind": "in_memory", "value": {"x": 1}},
        provenance={
            "code_version": {"package_version": package_version, "git_sha": git_sha},
            "standard_revisions": [],
            "data_as_of": data_as_of,
            "input_hash": "ih-123",
        },
        determinism={"result_hash": result_hash, "reproducible": reproducible},
        confidence=None,
        warnings=[],
    )


def _fake_runner(envelope):
    def _run(workflow_id, params=None, cfg=None):
        return envelope

    return _run


# ── stamp_provenance ──────────────────────────────────────────────────────────
def test_stamp_provenance_shape():
    prov = stamp_provenance("ih")
    assert set(prov) == {"code_version", "standard_revisions", "data_as_of", "input_hash"}
    assert set(prov["code_version"]) == {"package_version", "git_sha"}


def test_stamp_provenance_package_name_parameterized():
    # digitalmodel is an installed package -> its version differs from a hardcoded
    # assetutilities one; the point is the param flows into code_version.
    dm = stamp_provenance("ih", package_name="digitalmodel")
    au = stamp_provenance("ih", package_name="assetutilities")
    assert dm["code_version"]["package_version"] != "<hardcoded>"
    # both resolve their OWN package; git_sha key always present
    assert "git_sha" in dm["code_version"] and "git_sha" in au["code_version"]


def test_stamp_provenance_passes_input_hash_verbatim():
    assert stamp_provenance("sha256:abc")["input_hash"] == "sha256:abc"


def test_stamp_provenance_standard_revisions():
    cite = {"code_id": "DNV-RP-C201", "publisher": "DNV", "revision": "2010-10"}
    prov = stamp_provenance("ih", standard_revisions=[cite])
    assert prov["standard_revisions"] == [cite]


def test_stamp_provenance_none_input_hash_allowed():
    assert stamp_provenance(None)["input_hash"] is None


# ── KEY-ALLOWLIST (never a value heuristic) ───────────────────────────────────
def test_volatile_keys_is_key_allowlist_not_value_heuristic():
    # a RESULT value that string-renders date-like / path-like must SURVIVE pruning.
    env = _make_envelope(
        result={"kind": "in_memory", "value": {"governing": "2024-01-01", "key": "a/b/c"}}
    )
    pruned = _prune_volatile(env.to_dict())
    assert pruned["result"]["value"]["governing"] == "2024-01-01"
    assert pruned["result"]["value"]["key"] == "a/b/c"


def test_prune_volatile_drops_git_sha_by_name():
    a = _make_envelope(git_sha="aaaa").to_dict()
    b = _make_envelope(git_sha="bbbb").to_dict()
    assert _prune_volatile(a) == _prune_volatile(b)
    assert "git_sha" not in _prune_volatile(a)["provenance"]["code_version"]


def test_prune_volatile_drops_package_version_and_data_as_of_and_reproducible():
    env = _make_envelope(package_version="9.9.9", data_as_of="X", reproducible=True)
    pruned = _prune_volatile(env.to_dict())
    assert "package_version" not in pruned["provenance"]["code_version"]
    assert "data_as_of" not in pruned["provenance"]
    assert "reproducible" not in pruned["determinism"]
    # result_hash (non-volatile determinism field) survives
    assert pruned["determinism"]["result_hash"] == "hash-abc"


def test_prune_volatile_keeps_result_payload():
    rich = {"kind": "in_memory", "value": {"a": [1, 2, 3], "b": {"c": "keep/me"}}}
    env = _make_envelope(result=rich)
    assert _prune_volatile(env.to_dict())["result"] == rich


def test_extra_volatile_keys_drop_by_name():
    env = _make_envelope().to_dict()
    pruned = _prune_volatile(env, GOLDEN_VOLATILE_KEYS | {"provenance.input_hash"})
    assert "input_hash" not in pruned["provenance"]


# ── golden_workflow_test verdict (hash equality, not value delta) ─────────────
def test_golden_pass_on_matching_result_hash(tmp_path):
    env = _make_envelope(result_hash="match-1")
    golden = tmp_path / "g.json"
    golden.write_text(json.dumps({"result_hash": "match-1", "result": env.result}))
    out = golden_workflow_test("wf", golden, runner=_fake_runner(env))
    assert out is env


def test_golden_fail_on_result_hash_drift(tmp_path):
    golden_env = _make_envelope(
        result_hash="golden", result={"kind": "in_memory", "value": {"utilization": 0.50}}
    )
    drifted = _make_envelope(
        result_hash="drifted", result={"kind": "in_memory", "value": {"utilization": 0.91}}
    )
    golden = tmp_path / "g.json"
    golden.write_text(json.dumps({"result_hash": "golden", "result": golden_env.result}))
    with pytest.raises(AssertionError) as exc:
        golden_workflow_test("wf", golden, runner=_fake_runner(drifted))
    assert "utilization" in str(exc.value)  # the keyed-delta message names the drift


def test_golden_verdict_is_hash_not_value_delta(tmp_path):
    # identical result payload but DIFFERENT result_hash -> must FAIL (hash is the verdict).
    same_result = {"kind": "in_memory", "value": {"x": 1}}
    golden = tmp_path / "g.json"
    golden.write_text(json.dumps({"result_hash": "h1", "result": same_result}))
    drifted = _make_envelope(result_hash="h2", result=same_result)
    with pytest.raises(AssertionError):
        golden_workflow_test("wf", golden, runner=_fake_runner(drifted))


def test_golden_fails_on_error_envelope(tmp_path):
    err = ResultEnvelope(
        workflow_id="wf", status="error", result={},
        provenance={}, determinism={"result_hash": None}, warnings=["boom"],
    )
    golden = tmp_path / "g.json"
    golden.write_text(json.dumps({"result_hash": "x"}))
    with pytest.raises(AssertionError) as exc:
        golden_workflow_test("wf", golden, runner=_fake_runner(err))
    assert "errored" in str(exc.value)


def test_regen_goldens_env_writes_and_skips(tmp_path, monkeypatch):
    env = _make_envelope(result_hash="fresh-hash")
    golden = tmp_path / "new.json"
    monkeypatch.setenv("REGEN_GOLDENS", "1")
    with pytest.raises(pytest.skip.Exception):
        golden_workflow_test("wf", golden, runner=_fake_runner(env))
    written = json.loads(golden.read_text())
    assert written["result_hash"] == "fresh-hash"


def test_capture_golden_prunes_volatile(tmp_path):
    env = _make_envelope(git_sha="abc123", package_version="7.7.7")
    snap = capture_golden(env, tmp_path / "cap.json")
    assert snap["result_hash"] == "hash-abc"
    assert "git_sha" not in snap["envelope_pruned"]["provenance"]["code_version"]
    assert "package_version" not in snap["envelope_pruned"]["provenance"]["code_version"]


def test_pin_structural_excludes_volatile_by_name(tmp_path):
    env = _make_envelope(result_hash="ph", git_sha="X", package_version="Y")
    golden = tmp_path / "g.json"
    capture_golden(env, golden)
    # a different git_sha/version must still pass the structural pin (volatile by name)
    other = _make_envelope(result_hash="ph", git_sha="DIFFERENT", package_version="DIFF2")
    golden_workflow_test("wf", golden, pin_structural=True, runner=_fake_runner(other))


def test_diff_results_reports_max_abs_delta():
    msg = diff_results({"value": {"a": 1.0}}, {"value": {"a": 1.5}})
    assert "max_abs_delta=0.5" in msg


# ── reference determinism test: REAL emission via run_workflow ────────────────
def test_buckling_reference_golden_via_template():
    # Hashes the REAL artifact emitted by run_workflow on the BARE in-repo id, and
    # asserts it against #3285's committed golden through the #3283 template.
    env = golden_workflow_test(
        "buckling-parametric", GOLDENS / "buckling_parametric.json"
    )
    assert env.determinism["result_hash"] == json.loads(
        (GOLDENS / "buckling_parametric.json").read_text()
    )["result_hash"]
