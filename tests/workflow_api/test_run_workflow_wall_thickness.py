# ABOUTME: run_workflow envelope + stable-files golden for wall-thickness quickcheck (#3285).
"""Tests for ``run_workflow("wall-thickness-quickcheck")`` (G4, kind: files).

The quickcheck reads a cached payload that is config-relative; under the #3307 embed
path ``_config_dir_path`` is rebased to a throwaway root, so the cache must be passed
as an ABSOLUTE path via ``params``. Its HTML report embeds the (per-run, abspath)
output path, so the OVERALL kind:files hash is not machine-stable -- only the
sorted-key JSON + the CSV are byte-stable, so the golden pins THOSE per-file digests.
"""

from __future__ import annotations

from pathlib import Path

from digitalmodel.workflow_api import ResultEnvelope, run_workflow

from tests.workflow_api.golden_helpers import load_golden

REPO_ROOT = Path(__file__).resolve().parents[2]
CACHE = (
    REPO_ROOT
    / "examples/workflows/wall-thickness-quickcheck/data/quickcheck_cache.json"
)
PARAMS = {"wall_thickness": {"quickcheck": {"cache": str(CACHE)}}}


def test_run_workflow_wall_thickness_envelope():
    env = run_workflow("wall-thickness-quickcheck", params=PARAMS)
    assert isinstance(env, ResultEnvelope)
    assert env.status == "ok", env.warnings
    assert env.result["kind"] == "files"
    basenames = {f["basename"] for f in env.result["outputs"]}
    # save_cfg dump excluded; the three router outputs present
    assert {
        "wall_thickness_quickcheck.json",
        "wall_thickness_quickcheck.csv",
        "wall_thickness_quickcheck.html",
    } <= basenames


def test_wall_thickness_golden():
    # the byte-stable JSON + CSV per-file digests are pinned (the HTML embeds an
    # abspath so it is intentionally excluded from the golden).
    env = run_workflow("wall-thickness-quickcheck", params=PARAMS)
    assert env.status == "ok", env.warnings
    golden = load_golden("wall_thickness.json")["stable_files"]
    actual = {
        f["basename"]: {"sha256": f["sha256"], "size": f["size"]}
        for f in env.result["outputs"]
        if f["basename"].endswith((".json", ".csv"))
    }
    assert actual == golden
