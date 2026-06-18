"""Build a workflow's parametric atlas from its registry declaration.

Importable core shared by the ``scripts/parametric/build_atlas.py`` CLI and the
``refresh --apply`` path, so a stale atlas can be regenerated programmatically.
"""

from __future__ import annotations

import subprocess
from pathlib import Path

import yaml

from digitalmodel.parametric import refresh
from digitalmodel.parametric.atlas import Atlas, Axis
from digitalmodel.parametric.generate import generate_atlas

REPO_ROOT = Path(__file__).resolve().parents[3]
REGISTRY = REPO_ROOT / "docs" / "registry" / "workflows.yaml"


def _git_sha(repo_root: Path) -> str:
    try:
        return subprocess.check_output(
            ["git", "-C", str(repo_root), "rev-parse", "--short", "HEAD"], text=True
        ).strip()
    except Exception:  # pragma: no cover - provenance best-effort
        return "unknown"


def _row(workflow_id: str) -> dict:
    for row in yaml.safe_load(REGISTRY.read_text())["workflows"]:
        if row["id"] == workflow_id:
            return row
    raise KeyError(f"workflow id not in registry: {workflow_id}")


def build_atlas_from_registry(
    workflow_id: str,
    repo_root: Path = REPO_ROOT,
    atlas_root: Path | None = None,
) -> Atlas:
    row = _row(workflow_id)
    block = row.get("parametric")
    if not block:
        raise ValueError(f"{workflow_id} has no parametric: block")
    basename = row["basename"]
    atlas = generate_atlas(
        basename=basename,
        physics=block["physics"],
        response=block["response"],
        axes=[Axis.from_dict(a) for a in block["axes"]],
        response_kwargs=block.get("response_kwargs", {}),
        tolerance=float(block.get("tolerance", {}).get("threshold", 0.10)),
        code_version=_git_sha(repo_root),
        standards=refresh.STANDARDS.get(basename, []),
        input_template=repo_root / row["input"],
        workflow_id=workflow_id,
        content_fingerprint=refresh.content_fingerprint(workflow_id, repo_root),
    )
    atlas.save(atlas_root or (repo_root / "atlases"))
    return atlas
