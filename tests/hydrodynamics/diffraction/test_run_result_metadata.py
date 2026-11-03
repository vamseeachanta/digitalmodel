# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Construction-only result metadata contracts for issue #611.

Paths and list entries are synthetic sentinels for the requested independence
contract. A completed status here describes stored metadata, not a native run.
"""

from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
    RunResult,
    RunStatus,
)


@pytest.mark.parametrize(
    "changed_field",
    (
        "input_file",
        "output_dir",
        "log_file",
        "owr_path",
        "data_file",
        "xlsx_path",
        "report_path",
        "validation_report_path",
    ),
)
def test_run_result_output_paths_remain_independent(
    tmp_path: Path, changed_field: str
) -> None:
    """Each explicit path survives construction and unrelated reassignment."""
    paths = {
        "input_file": tmp_path / "input" / "analysis.yml",
        "output_dir": tmp_path / "output",
        "log_file": tmp_path / "logs" / "analysis.log",
        "owr_path": tmp_path / "results" / "analysis.owr",
        "data_file": tmp_path / "data" / "analysis.dat",
        "xlsx_path": tmp_path / "spreadsheets" / "analysis.xlsx",
        "report_path": tmp_path / "reports" / "analysis.html",
        "validation_report_path": tmp_path / "validation" / "analysis.json",
    }
    result = RunResult(status=RunStatus.COMPLETED, **paths)

    assert result.status == "completed"
    assert {name: getattr(result, name) for name in paths} == paths

    replacement = tmp_path / "replacement" / changed_field
    setattr(result, changed_field, replacement)

    expected = paths | {changed_field: replacement}
    assert {name: getattr(result, name) for name in paths} == expected
    assert result.status == "completed"


def test_run_result_dry_run_has_empty_result_paths(tmp_path: Path) -> None:
    """Synthetic dry-run metadata retains input and log without outputs."""
    result = RunResult(
        status=RunStatus.DRY_RUN,
        input_file=tmp_path / "input" / "analysis.yml",
        output_dir=tmp_path / "output",
        log_file=tmp_path / "logs" / "analysis.log",
    )

    assert result.status == "dry_run"
    assert result.input_file == tmp_path / "input" / "analysis.yml"
    assert result.output_dir == tmp_path / "output"
    assert result.log_file == tmp_path / "logs" / "analysis.log"
    assert result.owr_path is None
    assert result.data_file is None
    assert result.xlsx_path is None
    assert result.report_path is None
    assert result.validation_report_path is None
    assert result.validation_verdict == "SKIPPED"


def test_run_result_mutable_metadata_is_per_instance(tmp_path: Path) -> None:
    """Default lists are independent across fields and constructed results."""
    first = RunResult(status=RunStatus.PENDING)
    second = RunResult(status=RunStatus.DRY_RUN)
    entries = {
        "modular_files": tmp_path / "modular" / "section.yml",
        "mesh_files": tmp_path / "meshes" / "surface.gdf",
        "quality_warnings": "synthetic geometry warning",
        "validation_issues": "synthetic validation issue",
    }
    expected = {name: [] for name in entries}

    for name in entries:
        assert getattr(first, name) == []
        assert getattr(second, name) == []
        assert getattr(first, name) is not getattr(second, name)

    for name, entry in entries.items():
        getattr(first, name).append(entry)
        expected[name] = [entry]
        assert {key: getattr(first, key) for key in entries} == expected
        assert {key: getattr(second, key) for key in entries} == {
            key: [] for key in entries
        }

    for name, entry in entries.items():
        getattr(second, name).append(entry)
        setattr(second, name, [])
        assert {key: getattr(first, key) for key in entries} == expected
        assert {key: getattr(second, key) for key in entries} == {
            key: [] for key in entries
        }

    assert first.status == "pending"
    assert second.status == "dry_run"
