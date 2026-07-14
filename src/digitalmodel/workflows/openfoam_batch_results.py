"""Checkpoint and bounded rollup helpers for OpenFOAM batch runs."""

from __future__ import annotations

import json
import os
from datetime import datetime
from pathlib import Path
from typing import Any

import pandas as pd

from digitalmodel.workflows.openfoam_batch_config import canonical_json_bytes

CHECKPOINT_FILENAME = "_result.json"
EXTERNAL_CHECKPOINT_FILENAME = ".digitalmodel-checkpoint-v2.json"
RESULTS_ALLOWED_SUFFIXES = {".csv", ".json"}


def load_external_checkpoint(
    layout, case: str, identity: dict, max_row_bytes: int = 65536
) -> dict[str, Any] | None:
    """Read only an exact completed checkpoint while ownership locks hold."""
    layout.require_locks(case)
    data = layout.read_case_file(case, EXTERNAL_CHECKPOINT_FILENAME, 1024 * 1024)
    if data is None:
        return None
    try:
        payload = json.loads(data)
    except (UnicodeDecodeError, json.JSONDecodeError):
        return None
    if not isinstance(payload, dict) or not isinstance(payload.get("result_row"), dict):
        return None
    try:
        exact_identity = isinstance(payload.get("identity"), dict) and (
            canonical_json_bytes(payload["identity"])
            == canonical_json_bytes(identity)
        )
        row_size = len(
            json.dumps(
                payload["result_row"], sort_keys=True, allow_nan=False
            ).encode()
        )
    except (TypeError, ValueError, OverflowError):
        return None
    expected = (
        type(payload.get("schema_version")) is int
        and payload["schema_version"] == 2
        and exact_identity
        and payload.get("owner_token") == layout.owner_token
        and type(payload.get("case")) is str
        and payload["case"] == case
        and payload.get("status") == "completed"
        and payload["result_row"].get("status") == "completed"
        and payload["result_row"].get("name") == case
    )
    return payload["result_row"] if expected and row_size <= max_row_bytes else None


def write_external_checkpoint(
    layout, case: str, identity: dict, row: dict[str, Any], max_row_bytes: int = 65536
) -> None:
    """Atomically persist a bounded checkpoint while ownership locks hold."""
    layout.require_locks(case)
    if len(json.dumps(row, sort_keys=True).encode()) > max_row_bytes:
        raise ValueError("external checkpoint result row exceeds byte bound")
    payload = {
        "schema_version": 2,
        "identity": identity,
        "owner_token": layout.owner_token,
        "case": case,
        "status": row.get("status"),
        "result_row": row,
    }
    data = (json.dumps(payload, indent=2) + "\n").encode()
    layout.write_case_file(case, EXTERNAL_CHECKPOINT_FILENAME, data)


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
    summary = make_summary(
        rows, mode, workers, mock, timeout_seconds, started_at, finished_at
    )
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(summary, indent=2) + "\n")
    return summary


def make_summary(
    rows, mode, workers, mock, timeout_seconds, started_at, finished_at
) -> dict:
    """Build the common batch summary without performing path I/O."""
    completed = sum(1 for row in rows if row["status"] == "completed")
    return {
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


def write_external_results(
    output, rows, mode, workers, mock, timeout_seconds, started_at, finished_at
) -> dict:
    """Publish both external rollups through the retained output descriptor."""
    summary = make_summary(
        rows, mode, workers, mock, timeout_seconds, started_at, finished_at
    )
    manifest = pd.DataFrame(rows).to_csv(index=False).encode()
    output.write("cases.csv", manifest)
    output.write("batch_summary.json", (json.dumps(summary, indent=2) + "\n").encode())
    return summary
