"""Bounded public result policy for OpenFOAM batch runs."""

from __future__ import annotations

from copy import deepcopy
from datetime import datetime, timezone
import json
import os
from pathlib import Path, PureWindowsPath
import re
from typing import Any

import pandas as pd


RESULT_POLICY_VERSION = "result-policy-v1"
MAX_RESULT_BYTES = 10 * 1024 * 1024


def row(item: dict[str, Any], *, status: str, case_dir: Path | None = None,
        solver: str | None = None, error: str | None = None,
        mock: bool = False) -> dict[str, Any]:
    return {"index": item["index"], "name": item["name"], **item["case"],
            "status": status, "solver": solver, "mock": mock, "error": error,
            "case_dir": str(case_dir) if case_dir else None, "wall_seconds": 0.0}


def redact_rows(rows: list[dict[str, Any]], external_root: Path | None) -> list[dict[str, Any]]:
    if external_root is None:
        return deepcopy(rows)
    raw_root = str(external_root)
    windows_root = PureWindowsPath(raw_root)
    root = (windows_root.as_posix() if windows_root.is_absolute()
            else Path(raw_root).resolve().as_posix())
    redacted = deepcopy(rows)
    for item in redacted:
        case_dir = item.get("case_dir")
        if isinstance(case_dir, str) and case_dir:
            basename = case_dir.replace("\\", "/").rstrip("/").rsplit("/", 1)[-1]
            item["case_dir"] = f"<external-work>/{basename}"
        error = item.get("error")
        if isinstance(error, str):
            item["error"] = re.sub(re.escape(root), "<external-work>",
                                   error.replace("\\", "/"), flags=re.IGNORECASE)
    return redacted


def write_results(rows: list[dict[str, Any]], output_dir: Path, *, mode: str,
                  workers: int, mock: bool, timeout_seconds: int,
                  started_at: datetime | None = None, finished_at: datetime | None = None,
                  extensions: list[str] | None = None) -> dict[str, str]:
    if extensions:
        raise ValueError("result extension is inactive until separately approved")
    output_dir.mkdir(parents=True, exist_ok=True)
    manifest = output_dir / "cases.csv"
    summary_path = output_dir / "batch_summary.json"
    pd.DataFrame(rows).to_csv(manifest, index=False)
    start = started_at or datetime.now(timezone.utc)
    finish = finished_at or datetime.now(timezone.utc)
    completed = sum(item.get("status") == "completed" for item in rows)
    summary = {"workflow": "openfoam_run_batch", "result_policy_version": RESULT_POLICY_VERSION,
               "mode": mode, "total_cases": len(rows), "completed": completed,
               "failed": len(rows) - completed, "workers": workers,
               "host_cpu_count": os.cpu_count(), "mock": mock,
               "timeout_seconds": timeout_seconds, "started_at_utc": start.isoformat(),
               "finished_at_utc": finish.isoformat()}
    summary_path.write_text(json.dumps(summary, indent=2) + "\n")
    for path in (manifest, summary_path):
        if path.stat().st_size > MAX_RESULT_BYTES:
            path.unlink(missing_ok=True)
            raise ValueError(f"bounded result {path.name} exceeds policy")
    return {"manifest": str(manifest), "summary": str(summary_path)}
