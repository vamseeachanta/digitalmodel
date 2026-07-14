"""Result-policy-v1, redaction, and dormant #1576 extension contract."""

import json
from dataclasses import asdict
from datetime import datetime, timezone
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import Mock

import pytest

from digitalmodel.workflows import openfoam_batch_execution as execution
from digitalmodel.workflows import openfoam_batch_results as results


class RecordingOutput:
    def __init__(self):
        self.files = {}

    def write(self, name, data):
        self.files[name] = data


def _times():
    stamp = datetime(2026, 1, 2, tzinfo=timezone.utc)
    return stamp, stamp


def _row(**changes):
    row = {
        "index": 0,
        "name": "case-a",
        "status": "completed",
        "solver": "simpleFoam",
        "mock": True,
        "error": None,
        "case_dir": "[external]",
        "wall_seconds": 0.1,
    }
    row.update(changes)
    return row


def test_result_policy_v1_mandatory_outputs_are_exact_and_bounded():
    output = RecordingOutput()
    started, finished = _times()
    summary = results.write_external_results(
        output, [_row()], "pool", 2, True, 30, started, finished
    )
    assert set(output.files) == {"cases.csv", "batch_summary.json"}
    assert len(output.files["cases.csv"]) <= results.MAX_CASES_CSV_BYTES
    assert len(output.files["batch_summary.json"]) <= results.MAX_SUMMARY_BYTES
    assert summary["result_policy_version"] == "result-policy-v1"


def test_result_policy_rejects_oversized_row_before_publication():
    output = RecordingOutput()
    started, finished = _times()
    with pytest.raises(ValueError, match="row.*bound"):
        results.write_external_results(
            output,
            [_row(detail="x" * results.MAX_RESULT_ROW_BYTES)],
            "pool", 2, True, 30, started, finished,
        )
    assert output.files == {}


def test_reserved_artifact_index_extension_record_is_exact_and_inactive():
    extension = results.RESULT_EXTENSION_REGISTRY[
        "openfoam-artifact-index-v1"
    ]
    assert asdict(extension) == {
        "extension_id": "openfoam-artifact-index-v1",
        "basename": "artifact_index.json",
        "schema_id": "openfoam-artifact-index-v1",
        "media_type": "application/json",
        "max_bytes": 1048576,
        "policy_version": "result-policy-v1",
        "active": False,
    }
    assert results.active_result_extensions() == ()


@pytest.mark.parametrize(
    "requested, message",
    [
        (["*.json"], "glob"),
        (["unknown-extension-v1"], "unknown"),
        (["../openfoam-artifact-index-v1"], "traversal"),
        (["openfoam/artifact-index-v1"], "traversal"),
        (["openfoam-artifact-index-v1"] * 2, "duplicate"),
        (["openfoam-artifact-index-v1"], "inactive.*Deckhand #564"),
    ],
)
def test_extension_selection_rejects_unsafe_unknown_duplicate_or_inactive(
    requested, message
):
    with pytest.raises(ValueError, match=message):
        results.select_result_extensions(requested)


@pytest.mark.parametrize(
    "basename",
    [
        ".digitalmodel-checkpoint-v2.json",
        "_result.json",
        "log.simpleFoam",
        "processor0",
        "polyMesh",
        "VTK",
        "0/U",
        "constant/p",
        "U",
        "p",
        "points",
        "faces",
        "boundary",
        "mesh",
        "case.foam",
    ],
)
def test_heavy_checkpoint_log_mesh_field_and_processor_names_are_excluded(
    basename
):
    with pytest.raises(ValueError, match="result basename"):
        results.validate_result_basename(basename)


@pytest.mark.parametrize(
    "value",
    [
        ["openfoam-artifact-index-v1"],
        {"id": "openfoam-artifact-index-v1"},
        "*.json",
    ],
)
def test_yaml_cannot_register_or_discover_result_extensions(value):
    cfg = {"openfoam_run_batch": {"run_batch": {"result_extensions": value}}}
    with pytest.raises(ValueError, match="code-owned"):
        results.validate_result_policy_config(cfg)


def test_external_row_redacts_root_path_command_error_stdout_and_stderr():
    root = Path("/private/operator/openfoam")
    row = {
        **_row(case_dir=root / "run/case-a"),
        "work_root": str(root),
        "input_path": str(root / "request.yml"),
        "command": [str(root / "bin/simpleFoam"), "-parallel"],
        "error": f"failed under {root}/run/case-a",
        "stdout": f"banner from {root}",
        "stderr": "private diagnostic",
        "notes": f"inspect {root}/run/case-a/log.simpleFoam",
    }
    redacted = results.redact_external_row(row, root)
    encoded = json.dumps(redacted)
    assert str(root) not in encoded
    assert "private diagnostic" not in encoded
    for key in ("case_dir", "work_root", "input_path", "command", "stdout", "stderr"):
        assert redacted[key] == "[redacted]"
    assert redacted["error"] == "[redacted]"
    assert redacted["notes"] == "inspect [redacted-path]"


def test_external_make_row_is_redacted_but_legacy_row_is_byte_stable():
    root = Path("/private/operator")
    external = {
        "index": 0, "name": "case-a", "case": {},
        "layout": SimpleNamespace(root_path=root),
    }
    redacted = results.make_row(
        external, status="failed", case_dir=root / "run/case-a",
        error=f"failure at {root}/run/case-a",
    )
    assert redacted["case_dir"] == "[redacted]"
    assert str(root) not in redacted["error"]

    legacy = {"index": 0, "name": "case-a", "case": {"wave": 2.0}}
    assert results.make_row(
        legacy, status="completed", case_dir=Path("/legacy/case-a"),
        solver="simpleFoam", mock=True,
    ) == {
        "index": 0, "name": "case-a", "wave": 2.0,
        "status": "completed", "solver": "simpleFoam", "mock": True,
        "error": None, "case_dir": "/legacy/case-a", "wall_seconds": 0.0,
    }


def test_command_failure_log_does_not_emit_absolute_command_or_error_path(
    tmp_path, monkeypatch
):
    root = Path("/private/operator")
    error_log = Mock()
    monkeypatch.setattr(execution.logger, "error", error_log)
    monkeypatch.setattr(
        execution.subprocess,
        "run",
        Mock(side_effect=OSError(f"failed opening {root}/run/case-a")),
    )
    assert execution.run_command(
        [str(root / "bin/simpleFoam")], tmp_path, tmp_path / "log", 3,
        external_root=root,
    ) == 1
    assert str(root) not in repr(error_log.call_args)
    assert "simpleFoam" not in repr(error_log.call_args)
