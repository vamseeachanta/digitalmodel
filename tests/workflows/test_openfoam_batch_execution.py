from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.workflows.openfoam_batch_execution import (
    checkpoint_matches,
    execute_mpi_plan,
    file_sha256,
    make_checkpoint,
)


def test_checkpoint_v2_requires_identity_owner_case_and_completed() -> None:
    checkpoint = make_checkpoint(
        identity_sha256="a" * 64, owner_token="token", case="case-1",
        row={"name": "case-1", "status": "completed"},
    )
    assert checkpoint_matches(checkpoint, "a" * 64, "token", "case-1")
    assert not checkpoint_matches(checkpoint, "b" * 64, "token", "case-1")
    assert not checkpoint_matches(checkpoint, "a" * 64, "foreign", "case-1")
    assert not checkpoint_matches({**checkpoint, "schema": 1}, "a" * 64, "token", "case-1")
    failed = {**checkpoint, "row": {"name": "case-1", "status": "failed"}}
    assert not checkpoint_matches(failed, "a" * 64, "token", "case-1")
    assert checkpoint["run_identity"]["identity_sha256"] == "a" * 64


def test_checkpoint_contains_no_external_path(tmp_path: Path) -> None:
    checkpoint = make_checkpoint(
        identity_sha256="a" * 64, owner_token="token", case="case-1",
        row={"name": "case-1", "status": "completed", "case_dir": "<external-work>/case-1"},
    )
    assert str(tmp_path) not in str(checkpoint)


def test_mpi_launch_revalidates_bound_executable(tmp_path: Path) -> None:
    tool = tmp_path / "blockMesh"
    tool.write_bytes(b"approved")
    item = {"index": 0, "name": "case", "case": {}}

    def mutate(argv, cwd, log, timeout, expected_executable=None):
        tool.write_bytes(b"changed")
        path, digest = expected_executable
        if file_sha256(path) != digest:
            raise RuntimeError("selected executable changed")
        return 0

    with pytest.raises(RuntimeError, match="changed"):
        execute_mpi_plan(item, tmp_path, [["blockMesh"]], "solver", mutate, 10,
                         tool_bindings={"blockMesh": (tool, file_sha256(tool))})


def test_mpi_executes_verified_absolute_path(tmp_path: Path) -> None:
    tool = tmp_path / "blockMesh"
    tool.write_bytes(b"approved")
    item = {"index": 0, "name": "case", "case": {}}
    launched = []

    def capture(argv, cwd, log, timeout, expected_executable=None):
        launched.append(argv)
        return 0

    execute_mpi_plan(item, tmp_path, [["blockMesh"]], "solver", capture, 10,
                     tool_bindings={"blockMesh": (tool, file_sha256(tool))})
    assert launched == [[str(tool)]]
