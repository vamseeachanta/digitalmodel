"""Checkpoint and bounded rollup helpers for OpenFOAM batch runs."""

from __future__ import annotations

import json
import os
import re
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from types import MappingProxyType
from typing import Any

import pandas as pd

from digitalmodel.workflows.openfoam_batch_config import canonical_json_bytes

CHECKPOINT_FILENAME = "_result.json"
EXTERNAL_CHECKPOINT_FILENAME = ".digitalmodel-checkpoint-v2.json"
RESULTS_ALLOWED_SUFFIXES = {".csv", ".json"}
RESULT_POLICY_VERSION = "result-policy-v1"
MAX_RESULT_ROW_BYTES = 65536
MAX_CASES_CSV_BYTES = 2 * 1024 * 1024
MAX_SUMMARY_BYTES = 65536
_EXTENSION_ID = re.compile(r"[a-z0-9]+(?:-[a-z0-9]+)*-v[1-9][0-9]*\Z")
_ABSOLUTE_PATH = re.compile(
    r"(?:[A-Za-z]:[\\/]|/)[^\s,;:'\"\]\[()]+"
)
_FULL_REDACTION_KEYS = {
    "case_dir", "command", "cmd", "input_path", "path", "root",
    "stderr", "stdout", "work_dir", "work_root",
}


@dataclass(frozen=True)
class ResultExtension:
    extension_id: str
    basename: str
    schema_id: str
    media_type: str
    max_bytes: int
    policy_version: str
    active: bool


RESULT_EXTENSION_REGISTRY = MappingProxyType({
    "openfoam-artifact-index-v1": ResultExtension(
        extension_id="openfoam-artifact-index-v1",
        basename="artifact_index.json",
        schema_id="openfoam-artifact-index-v1",
        media_type="application/json",
        max_bytes=1048576,
        policy_version=RESULT_POLICY_VERSION,
        active=False,
    ),
})


def active_result_extensions() -> tuple[ResultExtension, ...]:
    return tuple(item for item in RESULT_EXTENSION_REGISTRY.values() if item.active)


def _validate_extension_id(value: object) -> str:
    if not isinstance(value, str):
        raise ValueError("result extension ID must be a string")
    if any(character in value for character in "*?["):
        raise ValueError("result extension glob discovery is forbidden")
    if "/" in value or "\\" in value or ".." in value:
        raise ValueError("result extension traversal is forbidden")
    if not _EXTENSION_ID.fullmatch(value):
        raise ValueError("unknown result extension")
    return value


def select_result_extensions(requested: list[str]) -> tuple[ResultExtension, ...]:
    """Resolve only active code-owned extension IDs; none are active yet."""
    values = [_validate_extension_id(value) for value in requested]
    if len(values) != len(set(values)):
        raise ValueError("duplicate result extension")
    unknown = [value for value in values if value not in RESULT_EXTENSION_REGISTRY]
    if unknown:
        raise ValueError("unknown result extension")
    selected = tuple(RESULT_EXTENSION_REGISTRY[value] for value in values)
    if any(not item.active for item in selected):
        raise ValueError("result extension inactive pending Deckhand #564 approval")
    return selected


def validate_result_basename(basename: str) -> str:
    """Reject every path-like or heavy OpenFOAM result name."""
    if not isinstance(basename, str) or not basename:
        raise ValueError("result basename is not an allowed bounded artifact")
    heavy_names = {
        "U", "p", "T", "k", "omega", "epsilon", "nut", "phi",
        "points", "faces", "owner", "neighbour", "boundary", "mesh",
        "VTK", "polyMesh",
    }
    blocked = (
        basename.startswith((".", "_result", "log.", "processor"))
        or basename in heavy_names
        or basename.startswith("alpha.")
        or basename.endswith(".foam")
    )
    if (
        Path(basename).name != basename
        or "/" in basename
        or "\\" in basename
        or blocked
    ):
        raise ValueError("result basename is not an allowed bounded artifact")
    return basename


def validate_result_policy_config(cfg: dict) -> None:
    run_settings = (cfg.get("openfoam_run_batch") or {}).get("run_batch") or {}
    if "result_extensions" in run_settings:
        raise ValueError("result extensions are code-owned, not YAML-configurable")


def redact_text(value: object) -> str:
    return _ABSOLUTE_PATH.sub("[redacted-path]", str(value))


def _redact_value(key: str, value: object) -> object:
    normalized = key.lower()
    if normalized in _FULL_REDACTION_KEYS or normalized.endswith(("_path", "_root")):
        return "[redacted]"
    if isinstance(value, dict):
        return {name: _redact_value(name, item) for name, item in value.items()}
    if isinstance(value, list):
        return [_redact_value(key, item) for item in value]
    return redact_text(value) if isinstance(value, (str, Path)) else value


def redact_external_row(row: dict[str, Any], _root: Path | None = None) -> dict:
    """Return a path- and stream-safe external result row."""
    return {key: _redact_value(key, value) for key, value in row.items()}


def redact_external_rows(rows: list[dict], root: Path | None = None) -> list[dict]:
    redacted = [redact_external_row(row, root) for row in rows]
    for row in redacted:
        encoded = json.dumps(row, sort_keys=True, allow_nan=False).encode()
        if len(encoded) > MAX_RESULT_ROW_BYTES:
            raise ValueError("external result row exceeds byte bound")
    return redacted


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
    row = {
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
    layout = item.get("layout")
    return redact_external_row(row, layout.root_path) if layout else row


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
    rows = redact_external_rows(rows)
    summary = {
        **make_summary(
            rows, mode, workers, mock, timeout_seconds, started_at, finished_at
        ),
        "result_policy_version": RESULT_POLICY_VERSION,
    }
    manifest = pd.DataFrame(rows).to_csv(index=False).encode()
    summary_bytes = (json.dumps(summary, indent=2) + "\n").encode()
    if len(manifest) > MAX_CASES_CSV_BYTES:
        raise ValueError("cases.csv exceeds result-policy byte bound")
    if len(summary_bytes) > MAX_SUMMARY_BYTES:
        raise ValueError("batch_summary.json exceeds result-policy byte bound")
    output.write("cases.csv", manifest)
    output.write("batch_summary.json", summary_bytes)
    return summary
