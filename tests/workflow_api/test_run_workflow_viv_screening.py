# ABOUTME: run_workflow envelope + reference golden + exact-replay-equality for the
# ABOUTME: synthetic VIV parametric screening route (digitalmodel#1505, kind: files).
"""Tests for ``run_workflow("viv-parametric-screening")`` (kind: files) + #1505 replay.

The route wraps the pure, analytical ``orcaflex/viv_screening.py`` (DNV-RP-C205 §9 /
DNV-RP-F105 §4-5) under the NEW basename ``viv_parametric_screening``; results.json is
byte-stable (``meta.generated_at`` omitted) so the content hash is exact-stable across
runs and machines.
"""

from __future__ import annotations

import re
import subprocess
import sys
from pathlib import Path

import yaml

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts" / "pilots"))

from digitalmodel.workflow_api import ResultEnvelope, run_workflow

from tests.workflow_api.golden_helpers import load_golden

import viv_pilot  # noqa: E402

REPO_ROOT = Path(__file__).resolve().parents[2]


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

    # C-lockin: cross-flow lock-in, non-zero A/D (screening FAILS).
    _pl_lock, a_d_lock, _fp, res_lock = viv_pilot.compute_case(viv_pilot.CASES["C-lockin"])
    assert res_lock.screening_pass is False        # lock_in == True
    assert a_d_lock > 0.5

    # C-nolockin: a GENUINE no-lock-in regime -- every mode's Vr is below the in-line
    # onset (Vr >= 1), so screening PASSES, no critical mode, lock_in is False, A/D == 0.
    # (Not the old mislabelled "suppressed" case, which was really in-line lock-in.)
    _pl_no, a_d_no, _fp2, res_no = viv_pilot.compute_case(viv_pilot.CASES["C-nolockin"])
    assert res_no.screening_pass is True
    assert (not res_no.screening_pass) is False    # lock_in is False
    assert res_no.critical_mode is None
    assert a_d_no == 0.0

    # the PRIMARY screening pass/fail (lock_in) genuinely VARIES across the published
    # set -- it is not constant-True across all three cases.
    lock_flags = {
        name: (not viv_pilot.compute_case(viv_pilot.CASES[name])[3].screening_pass)
        for name in viv_pilot.PUBLISHED_ORDER
    }
    assert len(set(lock_flags.values())) > 1, lock_flags   # lock_in NOT constant
    assert lock_flags["C-nolockin"] is False
    assert lock_flags["C-lockin"] is True

    # exactly one exact replay of C-lockin resolves to the SAME run_id.
    replay_id = viv_pilot.run_id_for(viv_pilot.CASES["C-lockin"])
    assert replay_id == run_ids["C-lockin"]


def test_resolved_clean_commit_contains_algorithm_module():
    # MAJOR-1: the identity SHA is resolved at run time (not a hardcoded grandparent
    # commit) and MUST actually contain the algorithm module -- otherwise the identity
    # binds a tree with no algorithm.
    sha = viv_pilot.CLEAN_GIT_COMMIT
    assert re.fullmatch(r"[0-9a-f]{40}", sha), sha
    # HEAD of the algorithm repo, and the module exists at that exact commit.
    head = subprocess.run(
        ["git", "-C", str(viv_pilot._REPO_ROOT), "rev-parse", "HEAD"],
        check=True, capture_output=True, text=True,
    ).stdout.strip()
    assert sha == head
    # `git cat-file -e <sha>:<module>` succeeds (exit 0) iff the blob is present there.
    proc = subprocess.run(
        ["git", "-C", str(viv_pilot._REPO_ROOT), "cat-file", "-e",
         f"{sha}:src/digitalmodel/orcaflex/viv_screening_workflow.py"],
        capture_output=True, text=True,
    )
    assert proc.returncode == 0, proc.stderr


def test_n_modes_is_single_sourced():
    # MINOR: N_MODES is the one source of truth; the derived execution knob, the example
    # input.yml, and the externalized publication YAML all agree with it.
    assert viv_pilot.EXECUTION_PARAMETERS["n_modes"] == viv_pilot.N_MODES

    example = yaml.safe_load(
        (REPO_ROOT / "examples" / "workflows" / "viv-parametric-screening"
         / "input.yml").read_text()
    )
    assert example["viv_parametric_screening"]["n_modes"] == viv_pilot.N_MODES

    pub = yaml.safe_load(
        (REPO_ROOT / "config" / "publication"
         / "viv-parametric-screening.yml").read_text()
    )
    assert pub["identity"]["execution_parameters"]["n_modes"] == viv_pilot.N_MODES


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
