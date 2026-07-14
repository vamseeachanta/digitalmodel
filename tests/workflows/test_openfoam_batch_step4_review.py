"""Regression tests for the Step 4 adversarial MAJOR findings."""

import contextlib
import json
import math
import os
from pathlib import Path
from unittest.mock import Mock

import pytest

from digitalmodel.solvers.openfoam.runner import OpenFOAMRunConfig, OpenFOAMRunner
from digitalmodel.workflows import openfoam_batch_execution as execution
from digitalmodel.workflows import openfoam_batch_layout as layout_module
from digitalmodel.workflows import openfoam_batch_results as results
from digitalmodel.workflows import openfoam_run_batch as facade
from digitalmodel.workflows.openfoam_batch_config import ExecutionAuthority

IDENTITY = {
    "schema_version": 1,
    "identity_kind": "openfoam-run-v1",
    "identity_sha256": "f" * 64,
}


def _layout(tmp_path: Path):
    root = tmp_path / "root"
    root.mkdir()
    authority = ExecutionAuthority("trusted-local", root, Path("runs"))
    return layout_module.WorkLayout.create(authority, IDENTITY, "cases")


def _record(**changes):
    record = {
        "schema_version": 1,
        "owner_token": "owner",
        "boot_id": "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa",
        "pid": 12,
        "process_start_token": "100",
        "heartbeat": 1.0,
    }
    record.update(changes)
    return record


@pytest.mark.parametrize(
    "changes",
    [
        {"schema_version": 2},
        {"boot_id": None},
        {"boot_id": "unknown"},
        {"pid": None},
        {"pid": True},
        {"process_start_token": ""},
        {"heartbeat": True},
        {"heartbeat": math.nan},
        {"heartbeat": math.inf},
    ],
)
def test_malformed_lock_records_never_reclaim(changes):
    assert not layout_module.lock_reclaimable(
        _record(**changes), owner_token="owner", now=1000.0,
        current_boot_id="bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb",
        process_state="unknown", stale_after=10.0,
    )


def test_unknown_current_boot_never_proves_prior_boot():
    assert not layout_module.lock_reclaimable(
        _record(), owner_token="owner", now=1000.0,
        current_boot_id="unknown", process_state="unknown", stale_after=10.0,
    )


def test_proc_start_token_parses_parenthesized_name_with_spaces(monkeypatch):
    fields = ["R", *[str(value) for value in range(4, 23)]]
    text = f"123 (worker name with spaces) {' '.join(fields)}\n"
    monkeypatch.setattr(Path, "read_text", lambda _path: text)
    assert layout_module._process_start_token(123) == "22"


def test_reclaim_rechecks_source_before_tombstoning_replacement(tmp_path, monkeypatch):
    with _layout(tmp_path) as layout:
        locks_fd = os.open(".locks", os.O_RDONLY | os.O_DIRECTORY, dir_fd=layout.run_fd)
        name = "run.lock"
        stale = _record(owner_token=layout.owner_token, heartbeat=0.0)
        live = _record(owner_token=layout.owner_token, heartbeat=999.0, pid=os.getpid())
        live["boot_id"] = layout_module._boot_id()
        live["process_start_token"] = layout_module._process_start_token()
        layout_module._write_new_at(locks_fd, name, stale)
        renamed = []
        original_rename = layout_module._rename_noreplace

        def replace_source(_fd, _name):
            os.unlink(name, dir_fd=locks_fd)
            layout_module._write_new_at(locks_fd, name, live)

        def observe_rename(*args):
            renamed.append(args)
            return original_rename(*args)

        monkeypatch.setattr(layout_module, "_before_lock_reclaim", replace_source)
        monkeypatch.setattr(layout_module, "_rename_noreplace", observe_rename)
        with pytest.raises(RuntimeError, match="changed"):
            layout._try_reclaim(locks_fd, name, 1000.0, 10.0)
        assert renamed == []
        assert json.loads((layout.run_path / ".locks" / name).read_text())["heartbeat"] == 999.0
        os.close(locks_fd)


def test_checkpoint_rejects_unlinked_or_replaced_lock_leases(tmp_path):
    with _layout(tmp_path) as layout:
        with layout.lock("run"), layout.lock("case-a"):
            locks = layout.run_path / ".locks"
            for key in ("run", "case-a"):
                (locks / layout._lock_name(key)).unlink()
            with pytest.raises(RuntimeError, match="locks"):
                results.load_external_checkpoint(layout, "case-a", IDENTITY)


def test_work_entry_swap_rejects_without_mutating_old_or_new_tree(tmp_path):
    layout = _layout(tmp_path)
    old_case = layout.case_path("case-a")
    (old_case / "old").mkdir(parents=True)
    saved = layout.work_path.with_name("cases.saved")
    layout.work_path.rename(saved)
    layout.work_path.mkdir()
    (layout.work_path / "replacement").mkdir()
    with pytest.raises(RuntimeError, match="owned run"):
        layout.clean_case("case-a")
    assert (saved / "case-a" / "old").is_dir()
    assert (layout.work_path / "replacement").is_dir()
    layout.close()


def _external_mpi_item(tmp_path, layout):
    return {
        "name": "case-a", "work_dir": tmp_path / "case-a", "layout": layout,
        "settings": {"solver": "interFoam", "mesh_utility": "blockMesh"},
    }


def test_external_mpi_resume_uses_only_layout_mutators(tmp_path, monkeypatch):
    layout = Mock()
    layout.has_processor_dirs.return_value = True
    item = _external_mpi_item(tmp_path, layout)
    monkeypatch.setattr(facade, "_has_processor_dirs", Mock(side_effect=AssertionError))
    monkeypatch.setattr(facade, "_set_start_from_latest_time", Mock(side_effect=AssertionError))
    case_dir, _, _, _ = execution._prepare_mpi_case(
        item, {"resume": True}, workers=2, mock=False
    )
    assert case_dir == item["work_dir"]
    layout.has_processor_dirs.assert_called_once_with("case-a")
    layout.set_start_from_latest_time.assert_called_once_with("case-a")


def test_external_mpi_setup_uses_layout_decompose_writer(tmp_path, monkeypatch):
    layout = Mock()
    layout.has_processor_dirs.return_value = False
    item = _external_mpi_item(tmp_path, layout)
    monkeypatch.setattr(facade, "_build_case", lambda _item: item["work_dir"])
    monkeypatch.setattr(facade, "_write_decompose_par_dict", Mock(side_effect=AssertionError))
    execution._prepare_mpi_case(item, {}, workers=3, mock=False)
    layout.write_decompose_par_dict.assert_called_once_with("case-a", 3)


class _LaunchWitness:
    def __init__(self):
        self.events = []

    @contextlib.contextmanager
    def launch(self, name):
        self.events.append(("before", name))
        yield
        self.events.append(("after", name))


def test_mpi_revalidates_executable_before_and_after_every_launch(tmp_path):
    witness = _LaunchWitness()
    item = {"executables": witness, "index": 0, "name": "case-a", "case": {}}
    execution.execute_mpi_plan(
        item, tmp_path, [["blockMesh"], ["mpirun", "solver"]], "solver",
        lambda *_args: 0, 10,
    )
    assert witness.events == [
        ("before", "blockMesh"), ("after", "blockMesh"),
        ("before", "mpirun"), ("after", "mpirun"),
    ]


def test_serial_runner_guards_every_subprocess_launch(tmp_path, monkeypatch):
    for subdir in ("system", "constant", "0"):
        (tmp_path / subdir).mkdir()
    (tmp_path / "system" / "controlDict").write_text("application simpleFoam;\n")
    witness = _LaunchWitness()
    monkeypatch.setattr("shutil.which", lambda name: f"/bin/{name}")
    monkeypatch.setattr("subprocess.run", lambda *_a, **_k: Mock(returncode=0, stdout=""))
    runner = OpenFOAMRunner(
        OpenFOAMRunConfig(solver="simpleFoam", to_vtk=False),
        executable_guard=witness.launch,
    )
    runner.run(tmp_path)
    assert witness.events == [
        ("before", "blockMesh"), ("after", "blockMesh"),
        ("before", "simpleFoam"), ("after", "simpleFoam"),
    ]


def test_checkpoint_is_json_type_exact_case_exact_and_fd_stable(tmp_path):
    with _layout(tmp_path) as layout:
        case = layout.case_path("case-a")
        case.mkdir()
        before = len(list(Path("/proc/self/fd").iterdir()))
        with layout.lock("run"), layout.lock("case-a"):
            for _ in range(30):
                assert results.load_external_checkpoint(layout, "case-a", IDENTITY) is None
        assert len(list(Path("/proc/self/fd").iterdir())) == before
        payload = {
            "schema_version": 2, "identity": {**IDENTITY, "schema_version": True},
            "owner_token": layout.owner_token, "case": "case-a", "status": "completed",
            "result_row": {"name": "other", "status": "completed"},
        }
        (case / results.EXTERNAL_CHECKPOINT_FILENAME).write_text(json.dumps(payload))
        with layout.lock("run"), layout.lock("case-a"):
            assert results.load_external_checkpoint(layout, "case-a", IDENTITY) is None
