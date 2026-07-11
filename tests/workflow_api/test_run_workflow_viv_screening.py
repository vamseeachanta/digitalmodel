# ABOUTME: run_workflow envelope + reference golden + exact-replay-equality for the
# ABOUTME: synthetic VIV parametric screening route (digitalmodel#1505, kind: files).
"""Tests for ``run_workflow("viv-parametric-screening")`` (kind: files) + #1505 replay.

The route wraps the pure, analytical ``orcaflex/viv_screening.py`` (DNV-RP-C205 §9 /
DNV-RP-F105 §4-5) under the NEW basename ``viv_parametric_screening``; results.json is
byte-stable (``meta.generated_at`` omitted) so the content hash is exact-stable across
runs and machines.
"""

from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts" / "pilots"))

from digitalmodel.workflow_api import ResultEnvelope, run_workflow

from tests.workflow_api.golden_helpers import load_golden

import viv_pilot  # noqa: E402


def test_run_workflow_viv_files():
    env = run_workflow("viv-parametric-screening")
    assert isinstance(env, ResultEnvelope)
    assert env.status == "ok", env.warnings
    assert env.result["kind"] == "files"
    basenames = {f["basename"] for f in env.result["outputs"]}
    # the save_cfg cfg-dump is EXCLUDED; only the genuine router outputs remain
    assert basenames == {"results.json", "cases.csv"}


def test_viv_reference_golden_bare_in_repo_id():
    # byte-stable results.json (meta.generated_at omitted) + rounded cases.csv -> the
    # kind:files content hash is exact-stable across runs and machines.
    env = run_workflow("viv-parametric-screening", verify_reproducible=True)
    assert env.status == "ok", env.warnings
    assert env.determinism["reproducible"] is True
    golden = load_golden("viv_parametric_screening.json")
    assert env.determinism["result_hash"] == golden["result_hash"]
    actual = {f["basename"]: f["sha256"] for f in env.result["outputs"]}
    expected = {f["basename"]: f["sha256"] for f in golden["result"]["outputs"]}
    assert actual == expected


def test_at_least_three_variations_plus_one_replay():
    # >=3 distinct-input runs mint distinct run_ids; the pinned regimes are meaningful.
    run_ids = {name: viv_pilot.run_id_for(case) for name, case in viv_pilot.CASES.items()}
    assert len(set(run_ids.values())) == len(viv_pilot.CASES) >= 3

    # C-lockin: cross-flow lock-in, non-zero A/D; C-suppressed: A/D == 0.0.
    _pl_lock, a_d_lock, _fp, res_lock = viv_pilot.compute_case(viv_pilot.CASES["C-lockin"])
    assert res_lock.screening_pass is False        # lock_in == True
    assert a_d_lock > 0.5
    _pl_sup, a_d_sup, _fp2, _res_sup = viv_pilot.compute_case(viv_pilot.CASES["C-suppressed"])
    assert a_d_sup == 0.0

    # exactly one exact replay of C-lockin resolves to the SAME run_id.
    replay_id = viv_pilot.run_id_for(viv_pilot.CASES["C-lockin"])
    assert replay_id == run_ids["C-lockin"]


def test_exact_replay_same_run_id_and_equality():
    store = viv_pilot.build_metric_store()
    base_proj, _pl, _ad = viv_pilot.build_projection_for_case(
        viv_pilot.CASES["C-lockin"], store
    )
    replay_proj, _pl2, _ad2 = viv_pilot.build_projection_for_case(
        viv_pilot.CASES["C-lockin"], store
    )
    # same run_id (identity.derive_run_identity) AND output_equality over the
    # timestamp-free curated set.
    assert replay_proj.run_id == base_proj.run_id
    assert (
        replay_proj.output_equality_digest["digest"]
        == base_proj.output_equality_digest["digest"]
    )
