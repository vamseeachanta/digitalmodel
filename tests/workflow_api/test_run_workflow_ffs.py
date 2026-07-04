# ABOUTME: run_workflow envelope + determinism golden for the FFS metal-loss route (#3285).
"""Tests for ``run_workflow("ffs-metal-loss")`` (G1, kind: in_memory)."""

from __future__ import annotations

from digitalmodel.workflow_api import ResultEnvelope, run_workflow

from tests.workflow_api.golden_helpers import deep_approx, load_golden

FFS_KEYS = {
    "component_id", "assessment_type", "level_reached", "t_nominal_in", "t_min_in",
    "t_measured_min_in", "t_measured_avg_in", "fca_in", "rsf", "rsf_a",
    "folias_factor", "remaining_life_yr", "verdict", "rerated_pressure_psi",
    "sufficiency_status", "passes", "code_reference",
}


def test_run_workflow_ffs_in_memory():
    env = run_workflow("ffs-metal-loss")
    assert isinstance(env, ResultEnvelope)
    assert env.status == "ok", env.warnings
    assert env.result["kind"] == "in_memory"
    value = env.result["value"]
    # the #1066 indexed 16-key FFSAssessmentResult.to_dict() surface
    assert set(value) == FFS_KEYS
    assert value["assessment_type"] == "LML"
    # the measurement-sufficiency catch: an L1-ACCEPT that is under-measured
    assert value["verdict"] == "ACCEPT"
    assert value["sufficiency_status"] == "TAKE_MORE"
    # digitalmodel stamps its OWN package version, not assetutilities'
    assert "package_version" in env.provenance["code_version"]


def test_ffs_golden():
    # double-run determinism (in-env) + value golden (cross-env-robust: FFS to_dict
    # carries unrounded floats, so VALUE-with-tolerance is pinned, not the exact hash).
    env = run_workflow("ffs-metal-loss", verify_reproducible=True)
    assert env.status == "ok", env.warnings
    assert env.determinism["reproducible"] is True
    golden = load_golden("ffs_metal_loss.json")
    deep_approx(env.result, golden["result"], rel=1e-6)
