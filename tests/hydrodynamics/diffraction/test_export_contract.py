# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Construction-only export dispositions; no solver or export producer is run."""

import sys
from dataclasses import FrozenInstanceError
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.aqwa_runner import (
    AQWARunResult,
    AQWARunStatus,
)
from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
    RunResult,
    RunStatus,
)


@pytest.fixture(autouse=True)
def block_native_api(monkeypatch):
    """An accidental native import during a test fails instead of loading it."""
    monkeypatch.setitem(sys.modules, "OrcFxAPI", None)


@pytest.fixture(params=[(RunResult, RunStatus), (AQWARunResult, AQWARunStatus)])
def result_types(request):
    return request.param


@pytest.mark.parametrize("status", ["PENDING", "DRY_RUN", "FAILED", "COMPLETED"])
def test_missing_paths_are_not_produced(result_types, status):
    result_type, statuses = result_types
    result = result_type(status=getattr(statuses, status))

    assert set(result.export_dispositions) == {"xlsx", "report"}
    for descriptor in result.export_dispositions.values():
        assert descriptor.state == "NOT_PRODUCED"
        assert descriptor.path is None
        assert descriptor.reason == (
            "No export path is recorded; automatic export production is not integrated."
        )
    assert result.xlsx_path is None
    assert result.report_path is None


@pytest.mark.parametrize("status", ["PENDING", "DRY_RUN", "FAILED", "COMPLETED"])
@pytest.mark.parametrize("recorded", [("xlsx",), ("report",), ("xlsx", "report")])
def test_recorded_paths_are_independently_unverified(
    result_types, status, recorded, tmp_path
):
    result_type, statuses = result_types
    paths = {"xlsx": tmp_path / "analysis.xlsx", "report": tmp_path / "analysis.html"}
    result = result_type(
        status=getattr(statuses, status),
        **{f"{name}_path": paths[name] for name in recorded},
    )

    for name, descriptor in result.export_dispositions.items():
        if name in recorded:
            assert descriptor.state == "PATH_RECORDED_UNVERIFIED"
            assert descriptor.path == paths[name]
            assert descriptor.reason == "File existence and contents are unverified."
        else:
            assert descriptor.state == "NOT_PRODUCED"
            assert descriptor.path is None


@pytest.mark.parametrize("name", ["xlsx", "report"])
def test_reassignment_and_reset_recompute_disposition(result_types, name, tmp_path):
    result_type, statuses = result_types
    result = result_type(status=statuses.COMPLETED)
    original = result.export_dispositions[name]
    first = tmp_path / "first"
    second = tmp_path / "second"

    setattr(result, f"{name}_path", first)
    recorded = result.export_dispositions[name]
    assert recorded.path == first
    assert recorded.state == "PATH_RECORDED_UNVERIFIED"
    setattr(result, f"{name}_path", second)
    assert result.export_dispositions[name].path == second
    setattr(result, f"{name}_path", None)
    assert result.export_dispositions[name] == original
    assert recorded.path == first


def test_descriptors_are_immutable_and_instances_independent(result_types, tmp_path):
    result_type, statuses = result_types
    first = result_type(status=statuses.COMPLETED)
    second = result_type(status=statuses.COMPLETED)
    descriptor = first.export_dispositions["xlsx"]

    with pytest.raises(FrozenInstanceError):
        descriptor.path = tmp_path / "changed.xlsx"
    snapshot = first.export_dispositions
    snapshot.clear()
    first.xlsx_path = tmp_path / "first.xlsx"
    assert set(first.export_dispositions) == {"xlsx", "report"}
    assert first.export_dispositions["xlsx"].path == first.xlsx_path
    assert second.export_dispositions["xlsx"].state == "NOT_PRODUCED"
    assert second.xlsx_path is None


def test_disposition_does_not_inspect_files(result_types, tmp_path, monkeypatch):
    result_type, statuses = result_types
    existing = tmp_path / "empty.xlsx"
    existing.touch()
    missing = tmp_path / "missing.html"
    result = result_type(
        status=statuses.COMPLETED, xlsx_path=existing, report_path=missing
    )

    def forbidden(*args, **kwargs):
        pytest.fail("Export disposition must not inspect the filesystem")

    with monkeypatch.context() as guard:
        for method in ("exists", "is_file", "stat", "open"):
            guard.setattr(Path, method, forbidden)
        descriptors = result.export_dispositions
        assert descriptors["xlsx"].path == existing
        assert descriptors["report"].path == missing
        assert {d.state for d in descriptors.values()} == {"PATH_RECORDED_UNVERIFIED"}


@pytest.mark.parametrize("recorded", [(), ("xlsx",), ("report",), ("xlsx", "report")])
def test_cli_prints_disposition_reason_and_path(
    result_types, recorded, tmp_path, capsys
):
    from digitalmodel.hydrodynamics.diffraction.cli import _echo_validation_block

    result_type, statuses = result_types
    paths = {"xlsx": tmp_path / "analysis.xlsx", "report": tmp_path / "analysis.html"}
    result = result_type(
        status=statuses.COMPLETED,
        validation_verdict="WARNING",
        **{f"{name}_path": paths[name] for name in recorded},
    )

    _echo_validation_block(result)

    output = capsys.readouterr().out
    for name, label in (("xlsx", "XLSX       "), ("report", "Report     ")):
        if name in recorded:
            expected = (
                f"{paths[name]} [PATH_RECORDED_UNVERIFIED] "
                "(File existence and contents are unverified.)"
            )
        else:
            expected = (
                "NOT_PRODUCED (No export path is recorded; "
                "automatic export production is not integrated.)"
            )
        assert f"  {label}: {expected}\n" in output
    assert "  Validation : WARN\n" in output
    assert result.validation_verdict == "WARNING"
    if result_type is AQWARunResult:
        assert not hasattr(result, "owr_path")
