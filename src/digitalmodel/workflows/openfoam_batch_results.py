"""Checkpoint and bounded rollup helpers for OpenFOAM batch runs."""

from __future__ import annotations

import json
import os
from datetime import datetime
from pathlib import Path
from typing import Any

import pandas as pd

CHECKPOINT_FILENAME = "_result.json"
RESULTS_ALLOWED_SUFFIXES = {".csv", ".json"}


def load_checkpoint(work_dir: Path) -> dict[str, Any] | None:
    """Return only a completed, structurally valid legacy checkpoint."""
    checkpoint = work_dir / CHECKPOINT_FILENAME
    if not checkpoint.is_file():
        return None
    try:
        row = json.loads(checkpoint.read_text())
    except (OSError, json.JSONDecodeError):
        return None
    if not isinstance(row, dict):
        return None
    return row if row.get("status") == "completed" else None


def write_checkpoint(work_dir: Path, row: dict[str, Any]) -> None:
    """Atomically persist a legacy checkpoint."""
    work_dir.mkdir(parents=True, exist_ok=True)
    target = work_dir / CHECKPOINT_FILENAME
    temporary = work_dir / f"{CHECKPOINT_FILENAME}.tmp"
    temporary.write_text(json.dumps(row, indent=2) + "\n")
    os.replace(temporary, target)


def make_row(
    item: dict[str, Any],
    *,
    status: str,
    case_dir: Path | None = None,
    solver: str | None = None,
    error: str | None = None,
    mock: bool = False,
) -> dict[str, Any]:
    """Build one legacy manifest row without changing key order."""
    return {
        "index": item["index"],
        "name": item["name"],
        **item["case"],
        "status": status,
        "solver": solver,
        "mock": mock,
        "error": error,
        "case_dir": str(case_dir) if case_dir else None,
        "wall_seconds": 0.0,
    }


def write_manifest(rows: list[dict[str, Any]], path: Path) -> None:
    """Write the bounded legacy CSV rollup."""
    path.parent.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(rows).to_csv(path, index=False)


def write_summary(
    rows: list[dict[str, Any]],
    path: Path,
    mode: str,
    workers: int,
    mock: bool,
    timeout_seconds: int,
    started_at: datetime,
    finished_at: datetime,
) -> dict:
    """Write the bounded legacy JSON summary."""
    completed = sum(1 for row in rows if row["status"] == "completed")
    summary = {
        "workflow": "openfoam_run_batch",
        "mode": mode,
        "total_cases": len(rows),
        "completed": completed,
        "failed": len(rows) - completed,
        "workers": workers,
        "host_cpu_count": os.cpu_count(),
        "mock": mock,
        "timeout_seconds": timeout_seconds,
        "started_at_utc": started_at.isoformat(),
        "finished_at_utc": finished_at.isoformat(),
    }
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(summary, indent=2) + "\n")
    return summary
