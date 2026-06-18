"""Regenerate a workflow's parametric atlas from its registry declaration.

Usage:  uv run python scripts/parametric/build_atlas.py mooring-fatigue

Reads the ``parametric:`` block from docs/registry/workflows.yaml, sweeps the
grid, validates against held-out points, stamps provenance (digitalmodel git
sha + cited standards), and writes the atlas under ``atlases/<basename>/``.
This is the manual seed of the refresh job (#799).
"""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path

import yaml

from digitalmodel.parametric.atlas import Axis
from digitalmodel.parametric.generate import generate_atlas

REPO_ROOT = Path(__file__).resolve().parents[2]
REGISTRY = REPO_ROOT / "docs" / "registry" / "workflows.yaml"

# Cited standard editions per workflow (the source of the S-N curves).
STANDARDS = {
    "mooring_fatigue": [{"id": "DNV-RP-C203", "edition": "2021-09"}],
}


def _git_sha() -> str:
    try:
        return subprocess.check_output(
            ["git", "-C", str(REPO_ROOT), "rev-parse", "--short", "HEAD"],
            text=True,
        ).strip()
    except Exception:  # pragma: no cover - provenance best-effort
        return "unknown"


def _row(workflow_id: str) -> dict:
    registry = yaml.safe_load(REGISTRY.read_text())
    for row in registry["workflows"]:
        if row["id"] == workflow_id:
            return row
    raise SystemExit(f"workflow id not found in registry: {workflow_id}")


def main(workflow_id: str) -> None:
    row = _row(workflow_id)
    block = row.get("parametric")
    if not block:
        raise SystemExit(f"{workflow_id} has no parametric: block")
    basename = row["basename"]
    axes = [Axis.from_dict(a) for a in block["axes"]]

    atlas = generate_atlas(
        basename=basename,
        physics=block["physics"],
        response=block["response"],
        axes=axes,
        response_kwargs=block.get("response_kwargs", {}),
        tolerance=float(block.get("tolerance", {}).get("threshold", 0.10)),
        code_version=_git_sha(),
        standards=STANDARDS.get(basename, []),
        input_template=REPO_ROOT / row["input"],
    )
    out = atlas.save(REPO_ROOT / "atlases")
    v = atlas.validation
    print(f"atlas_id={atlas.atlas_id}  rows={len(atlas.grid)}")
    print(f"holdout n={v['n_holdout']}  max_rel_error={v['max_rel_error']:.5f}  "
          f"threshold={v['threshold']}  passes={v['passes']}")
    print(f"written -> {out}")
    if not v["passes"]:
        raise SystemExit("validation FAILED — densify the grid before publishing")


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "mooring-fatigue")
