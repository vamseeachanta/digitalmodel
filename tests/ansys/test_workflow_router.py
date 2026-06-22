"""ANSYS workflow router — engine dispatch + fail-closed (digitalmodel #940)."""

from __future__ import annotations

from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch

import pytest

from digitalmodel.ansys.runner import ANSYSRunStatus
from digitalmodel.ansys.workflow import AnsysWorkflow


def _cfg(tmp_path: Path, *, dry_run: bool = True, operation: str = "run_ansys") -> dict:
    script = tmp_path / "model.inp"
    script.write_text("/PREP7\nFINISH\n")
    return {
        "basename": "ansys",
        "_config_dir_path": str(tmp_path),
        "Analysis": {"result_folder": str(tmp_path / "results")},
        "ansys": {
            "operation": operation,
            "script": "model.inp",
            "output_directory": "out",
            "dry_run": dry_run,
        },
    }


def _fake_result(status: ANSYSRunStatus, tmp_path: Path):
    return SimpleNamespace(
        status=status,
        output_dir=tmp_path / "out",
        log_file=None,
        result_files=[],
        return_code=0,
        error_message="boom",
    )


def test_run_ansys_dry_run_records_status(tmp_path: Path):
    cfg = AnsysWorkflow().router(_cfg(tmp_path, dry_run=True))
    settings = cfg["ansys"]
    assert settings["run_status"] == "dry_run"
    assert "result_files" in settings["outputs"]


def test_unsupported_operation_raises(tmp_path: Path):
    with pytest.raises(ValueError, match="run_ansys"):
        AnsysWorkflow().router(_cfg(tmp_path, operation="bogus"))


def test_missing_script_raises(tmp_path: Path):
    cfg = _cfg(tmp_path)
    cfg["ansys"].pop("script")
    with pytest.raises(ValueError, match="ansys.script is required"):
        AnsysWorkflow().router(cfg)


def test_failed_status_raises(tmp_path: Path):
    with patch(
        "digitalmodel.ansys.runner.run_ansys",
        return_value=_fake_result(ANSYSRunStatus.FAILED, tmp_path),
    ):
        with pytest.raises(RuntimeError, match="ANSYS solve failed"):
            AnsysWorkflow().router(_cfg(tmp_path, dry_run=False))


def test_silent_dry_run_fallback_raises(tmp_path: Path):
    # Solver unavailable: runner returns DRY_RUN although a real solve was requested.
    with patch(
        "digitalmodel.ansys.runner.run_ansys",
        return_value=_fake_result(ANSYSRunStatus.DRY_RUN, tmp_path),
    ):
        with pytest.raises(RuntimeError, match="fell back to dry-run"):
            AnsysWorkflow().router(_cfg(tmp_path, dry_run=False))
