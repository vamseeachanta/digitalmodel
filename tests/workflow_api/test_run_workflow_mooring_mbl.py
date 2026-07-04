# ABOUTME: run_workflow envelope + golden + reserved-basename guard for mooring MBL (#3285).
"""Tests for ``run_workflow("mooring-design-mbl")`` (G3, NEW basename ``mooring_mbl``)."""

from __future__ import annotations

import pytest

from digitalmodel.engine import engine
from digitalmodel.workflow_api import ResultEnvelope, run_workflow

from tests.workflow_api.golden_helpers import deep_approx, load_golden


def test_run_workflow_mooring_mbl_citation_preserved():
    env = run_workflow("mooring-design-mbl")
    assert isinstance(env, ResultEnvelope)
    assert env.status == "ok", env.warnings
    value = env.result["value"]
    assert value["condition"] == "intact"
    assert value["safety_factor"] == pytest.approx(1.67)
    # DNV-OS-E301 citation sidecar preserved on the payload ...
    citations = value["citations"]
    assert len(citations) == 1
    assert citations[0]["code_id"] == "DNV-OS-E301"
    # ... and lifted into provenance.standard_revisions
    revs = env.provenance["standard_revisions"]
    assert any(r["code_id"] == "DNV-OS-E301" for r in revs)


def test_mooring_mbl_golden():
    env = run_workflow("mooring-design-mbl", verify_reproducible=True)
    assert env.status == "ok", env.warnings
    assert env.determinism["reproducible"] is True
    golden = load_golden("mooring_mbl.json")
    # payload is fully rounded -> exact hash AND value are stable
    deep_approx(env.result, golden["result"], rel=1e-9)
    assert env.determinism["result_hash"] == golden["result_hash"]


def test_mooring_basename_still_reserved():
    # the NEW mooring_mbl arm must NOT have disturbed the reserved `mooring` arm,
    # which redirects to subsea/mooring_analysis/ via NotImplementedError.
    with pytest.raises(NotImplementedError):
        engine(cfg={"basename": "mooring"}, config_flag=False)
