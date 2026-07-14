from __future__ import annotations

import json
from pathlib import Path

import pytest

from digitalmodel.workflows.openfoam_batch_results import redact_rows, write_results


def test_external_paths_are_redacted_from_rows_and_errors(tmp_path: Path) -> None:
    root = tmp_path / "private-root"
    rows = [{
        "name": "case-1", "status": "failed", "case_dir": str(root / "case-1"),
        "error": f"solver failed below {root / 'case-1'}", "wall_seconds": 1.0,
    }]
    redacted = redact_rows(rows, root)
    blob = json.dumps(redacted)
    assert str(root) not in blob
    assert redacted[0]["case_dir"] == "<external-work>/case-1"


def test_result_policy_writes_only_fixed_bounded_files(tmp_path: Path) -> None:
    rows = [{"name": "case-1", "status": "completed", "wall_seconds": 0.0}]
    outputs = write_results(rows, tmp_path, mode="pool", workers=1, mock=True,
                            timeout_seconds=10)
    assert set(outputs) == {"manifest", "summary"}
    assert {p.name for p in tmp_path.iterdir()} == {"cases.csv", "batch_summary.json"}
    with pytest.raises(ValueError, match="extension"):
        write_results(rows, tmp_path, mode="pool", workers=1, mock=True,
                      timeout_seconds=10, extensions=["openfoam-artifact-index-v1"])

