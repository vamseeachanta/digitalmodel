"""Regression tests for the Step 4 second adversarial rereview."""

import json
import os
from pathlib import Path
from unittest.mock import Mock

import pytest

from digitalmodel.solvers.openfoam.runner import OpenFOAMRunConfig, OpenFOAMRunner
from digitalmodel.workflows import openfoam_batch_execution as execution
from digitalmodel.workflows import openfoam_batch_layout as layout_module
from digitalmodel.workflows import openfoam_batch_results as results
from digitalmodel.workflows.openfoam_batch_config import ExecutionAuthority
from digitalmodel.workflows.openfoam_batch_executables import ExecutableSet

IDENTITY = {
    "schema_version": 1,
    "identity_kind": "openfoam-run-v1",
    "identity_sha256": "a" * 64,
}
BOOT_A = "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"
BOOT_B = "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb"


def _layout(tmp_path: Path):
    root = tmp_path / "root"
    root.mkdir()
    authority = ExecutionAuthority("trusted-local", root, Path("runs"))
    return layout_module.WorkLayout.create(authority, IDENTITY, "cases")


def _record(**changes):
    record = {
        "schema_version": 1,
        "owner_token": "owner",
        "boot_id": BOOT_A,
        "pid": 12,
        "process_start_token": "100",
        "heartbeat": 1.0,
    }
    record.update(changes)
    return record


def test_reclaim_move_never_strands_replacement_lock(tmp_path, monkeypatch):
    with _layout(tmp_path) as layout:
        locks_fd = os.open(".locks", os.O_RDONLY | os.O_DIRECTORY, dir_fd=layout.run_fd)
        name = "run.lock"
        stale = _record(owner_token=layout.owner_token)
        live = _record(
            owner_token=layout.owner_token,
            boot_id=layout_module._boot_id(),
            pid=os.getpid(),
            process_start_token=layout_module._process_start_token(),
            heartbeat=999.0,
        )
        layout_module._write_new_at(locks_fd, name, stale)

        def replace_before_move(_fd, _name):
            os.unlink(name, dir_fd=locks_fd)
            layout_module._write_new_at(locks_fd, name, live)

        monkeypatch.setattr(
            layout_module, "_before_lock_move", replace_before_move, raising=False
        )
        with pytest.raises(RuntimeError, match="changed"):
            layout._try_reclaim(locks_fd, name, 1000.0, 10.0)
        record = json.loads((layout.run_path / ".locks" / name).read_text())
        assert record["heartbeat"] == 999.0
        assert not list((layout.run_path / ".locks").glob("*.reclaim-*"))
        os.close(locks_fd)


@pytest.mark.parametrize("boot", ["garbage", "", "UNKNOWN", BOOT_A.upper()])
def test_malformed_boot_id_never_reclaims_with_unknown_liveness(boot):
    assert not layout_module.lock_reclaimable(
        _record(boot_id=boot), owner_token="owner", now=1000.0,
        current_boot_id=BOOT_B, process_state="unknown", stale_after=10.0,
    )


def test_unknown_liveness_never_reclaims_even_with_prior_boot():
    assert not layout_module.lock_reclaimable(
        _record(), owner_token="owner", now=1000.0,
        current_boot_id=BOOT_B, process_state="unknown", stale_after=10.0,
    )


def test_locks_directory_swap_invalidates_retained_layout(tmp_path):
    layout = _layout(tmp_path)
    locks = layout.run_path / ".locks"
    locks.rename(layout.run_path / ".locks.saved")
    locks.mkdir()
    try:
        with pytest.raises(RuntimeError, match="owned run"):
            layout.validate_owner()
    finally:
        layout.close()


def _tool(path: Path, name: str) -> Path:
    tool = path / name
    tool.write_text("#!/bin/sh\nexit 0\n")
    tool.chmod(0o700)
    return tool


def test_serial_argv0_is_the_witnessed_absolute_path(tmp_path, monkeypatch):
    case = tmp_path / "case"
    for subdir in ("system", "constant", "0"):
        (case / subdir).mkdir(parents=True, exist_ok=True)
    (case / "system" / "controlDict").write_text("application simpleFoam;\n")
    tools = tmp_path / "tools"
    tools.mkdir()
    mesh = _tool(tools, "blockMesh")
    solver = _tool(tools, "simpleFoam")
    witnesses = ExecutableSet.capture({"blockMesh": mesh, "simpleFoam": solver})
    launched = []
    monkeypatch.setattr("shutil.which", lambda name: f"/alternate/{name}")
    monkeypatch.setattr(
        "subprocess.run",
        lambda argv, **_kwargs: launched.append(argv) or Mock(returncode=0, stdout=""),
    )
    OpenFOAMRunner(
        OpenFOAMRunConfig(solver="simpleFoam", to_vtk=False),
        executable_guard=witnesses.launch,
    ).run(case)
    assert launched == [[str(mesh)], [str(solver)]]


def test_mpi_argv_uses_witnessed_mpirun_and_nested_solver(tmp_path):
    tools = tmp_path / "tools"
    tools.mkdir()
    mpirun = _tool(tools, "mpirun")
    solver = _tool(tools, "interFoam")
    witnesses = ExecutableSet.capture({"mpirun": mpirun, "interFoam": solver})
    launched = []
    item = {
        "executables": witnesses, "index": 0, "name": "case-a", "case": {}
    }
    execution.execute_mpi_plan(
        item, tmp_path,
        [["mpirun", "-np", "2", "interFoam", "-parallel"]],
        "interFoam", lambda argv, *_args: launched.append(argv) or 0, 10,
    )
    assert launched == [[str(mpirun), "-np", "2", str(solver), "-parallel"]]


def test_nonfinite_checkpoint_identity_is_treated_as_corrupt(tmp_path):
    with _layout(tmp_path) as layout:
        case = layout.case_path("case-a")
        case.mkdir()
        payload = {
            "schema_version": 2,
            "identity": {**IDENTITY, "bad": float("nan")},
            "owner_token": layout.owner_token,
            "case": "case-a",
            "status": "completed",
            "result_row": {"name": "case-a", "status": "completed"},
        }
        (case / results.EXTERNAL_CHECKPOINT_FILENAME).write_text(json.dumps(payload))
        with layout.lock("run"), layout.lock("case-a"):
            assert results.load_external_checkpoint(layout, "case-a", IDENTITY) is None
