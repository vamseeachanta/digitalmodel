# ABOUTME: run_workflow envelope + reference golden for the buckling sweep route (#3285-OWNED).
"""Tests for ``run_workflow("buckling-parametric")`` (G2, kind: files).

#3285 OWNS-CREATES this reference golden, runnable via the BARE in-repo id
``run_workflow("buckling-parametric", ...)`` (no #3284). #3283 references it only as
an illustrative consumer example; any cross-repo ``digitalmodel:buckling-parametric``
form is #3284-gated.
"""

from __future__ import annotations

from digitalmodel.workflow_api import ResultEnvelope, run_workflow

from tests.workflow_api.golden_helpers import load_golden


def test_run_workflow_buckling_files():
    env = run_workflow("buckling-parametric")
    assert isinstance(env, ResultEnvelope)
    assert env.status == "ok", env.warnings
    assert env.result["kind"] == "files"
    basenames = {f["basename"] for f in env.result["outputs"]}
    # the save_cfg cfg-dump is EXCLUDED; only the genuine router outputs remain
    assert basenames == {"results.json", "cases.csv"}


def test_buckling_reference_golden_bare_in_repo_id():
    # the #3285-owned reference golden, runnable via the BARE in-repo id (no #3284).
    # results.json is byte-stable (meta.generated_at omitted) + cases.csv is rounded,
    # so the kind:files content hash is exact-stable across runs and machines.
    env = run_workflow("buckling-parametric", verify_reproducible=True)
    assert env.status == "ok", env.warnings
    assert env.determinism["reproducible"] is True
    golden = load_golden("buckling_parametric.json")
    assert env.determinism["result_hash"] == golden["result_hash"]
    # per-file content digests pinned too
    actual = {f["basename"]: f["sha256"] for f in env.result["outputs"]}
    expected = {f["basename"]: f["sha256"] for f in golden["result"]["outputs"]}
    assert actual == expected
