"""Final T3 regressions for issue #1565 integration boundaries."""

import subprocess
from unittest.mock import Mock

import pytest

from digitalmodel.workflows import openfoam_batch_execution as execution
from digitalmodel.workflows import openfoam_batch_routing as routing
from digitalmodel.workflows import openfoam_batch_results as results
from digitalmodel.workflows import openfoam_run_batch as facade
from digitalmodel.workflows.openfoam_batch_config import (
    resolve_execution_authority,
    resolve_workers,
)
from digitalmodel.workflows.openfoam_batch_executables import ExecutableSet

IDENTITY = {
    "schema_version": 1,
    "identity_kind": "openfoam-run-v1",
    "identity_sha256": "c" * 64,
}


def _external_cfg(tmp_path, **run_changes):
    cfg_dir = tmp_path / "input"
    root = tmp_path / "operator"
    cfg_dir.mkdir()
    root.mkdir()
    request = cfg_dir / "request.yml"
    request.write_text("basename: openfoam_run_batch\n")
    run = {
        "execution_context": "trusted-local", "work_root": str(root),
        "output_dir": "results", "mock": True, **run_changes,
    }
    cfg = {
        "_config_dir_path": str(cfg_dir), "_config_file_path": str(request),
        "openfoam_run_batch": {
            "base": {"case_type": "current_loading", "solver": "simpleFoam"},
            "cases": [{"name": "case-a"}], "run_batch": run,
        },
    }
    return cfg, root, cfg_dir


def _tree(path):
    return sorted(item.relative_to(path).as_posix() for item in path.rglob("*"))


def _tool(directory, name, text="approved"):
    path = directory / name
    path.write_text(f"#!/bin/sh\necho {text}\n")
    path.chmod(0o700)
    return path


def test_mpi_set_fields_is_selected_and_required(monkeypatch):
    rendered = [{"settings": {
        "mesh_utility": "blockMesh", "solver": "interFoam",
        "run_set_fields": True,
    }}]
    monkeypatch.setattr(routing.shutil, "which", lambda name: f"/tools/{name}")
    selected = routing._selected_tools(
        rendered, {"mode": "mpi", "reconstruct": True}, mock=False
    )
    assert "setFields" in selected
    monkeypatch.setattr(execution.shutil, "which", lambda name: None if name == "setFields" else f"/tools/{name}")
    assert not execution.solver_ready(
        "mpi", "blockMesh", "interFoam", True, run_set_fields=True
    )


def test_launch_argv_rejects_unknown_executable_positions(tmp_path):
    tool = _tool(tmp_path, "mpirun")
    witnesses = ExecutableSet.capture({"mpirun": tool})
    with pytest.raises(RuntimeError, match="uncaptured"):
        with witnesses.launch_argv(["unknown"]):
            pass
    with pytest.raises(RuntimeError, match="uncaptured"):
        with witnesses.launch_argv(
            ["mpirun", "-np", "2", "unknownSolver"],
            executable_names=["mpirun", "unknownSolver"],
        ):
            pass


def test_retained_descriptor_binds_bytes_across_swap_execute_restore(tmp_path):
    approved = _tool(tmp_path, "solver", "approved")
    replacement = _tool(tmp_path, "replacement", "unapproved")
    saved = tmp_path / "saved"
    witnesses = ExecutableSet.capture({"solver": approved})
    with witnesses.launch("solver") as bound:
        approved.rename(saved)
        replacement.rename(approved)
        try:
            run = subprocess.run(
                [bound], pass_fds=(bound.pass_fd,), capture_output=True,
                text=True, check=True,
            )
        finally:
            approved.unlink()
            saved.rename(approved)
    assert run.stdout.strip() == "approved"


@pytest.mark.parametrize("value", [True, 1.5, "2", 0, -1])
def test_workers_require_exact_positive_integer(value):
    with pytest.raises(ValueError, match="workers"):
        resolve_workers({"workers": value})


def test_workers_above_visible_limit_reject_before_mutation(tmp_path, monkeypatch):
    cfg, root, cfg_dir = _external_cfg(tmp_path, workers=3)
    monkeypatch.setattr(routing.os, "cpu_count", lambda: 2)
    with pytest.raises(ValueError, match="visible"):
        facade.router(cfg)
    assert _tree(root) == []
    assert _tree(cfg_dir) == ["request.yml"]


@pytest.mark.parametrize("git_marker", ["directory", "file"])
def test_namespace_ancestry_rejects_nested_git_boundary(tmp_path, git_marker):
    root = tmp_path / "operator"
    nested = root / "team"
    nested.mkdir(parents=True)
    marker = nested / ".git"
    marker.mkdir() if git_marker == "directory" else marker.write_text("gitdir: elsewhere\n")
    with pytest.raises(ValueError, match="Git"):
        resolve_execution_authority({
            "execution_context": "trusted-local", "work_root": str(root),
            "work_root_namespace": "team/lane",
        }, tmp_path, {})


@pytest.mark.parametrize("invalid", [
    {"result_extensions": ["openfoam-artifact-index-v1"]},
    {"mode": "mpi"},
])
def test_external_config_rejects_before_layout_or_output_mutation(
    tmp_path, monkeypatch, invalid
):
    cfg, root, cfg_dir = _external_cfg(tmp_path, **invalid)
    if invalid.get("mode") == "mpi":
        cfg["openfoam_run_batch"]["cases"].append({"name": "case-b"})
    monkeypatch.setattr(facade, "_build_run_identity", lambda **_kw: IDENTITY)
    with pytest.raises(ValueError):
        facade.router(cfg)
    assert _tree(root) == []
    assert _tree(cfg_dir) == ["request.yml"]


def test_external_outputs_and_failure_log_use_relative_locators(
    tmp_path, monkeypatch
):
    cfg, root, cfg_dir = _external_cfg(tmp_path)
    monkeypatch.setattr(facade, "_build_run_identity", lambda **_kw: IDENTITY)
    monkeypatch.setattr(facade, "_run_pool", lambda *_a: [{
        "index": 0, "name": "case-a", "status": "failed",
        "error": "private", "wall_seconds": 0.0,
    }])
    warning = Mock()
    monkeypatch.setattr(facade.logger, "warning", warning)
    returned = facade.router(cfg)
    assert returned["openfoam_run_batch"]["outputs"] == {
        "manifest": "results/cases.csv",
        "summary": "results/batch_summary.json",
    }
    exposed = repr((returned["openfoam_run_batch"]["outputs"], warning.call_args))
    assert str(root) not in exposed
    assert str(cfg_dir) not in exposed


@pytest.mark.parametrize(
    "key", ["failure_reason", "exception", "credential", "api_token", "secret"]
)
def test_external_scalar_diagnostic_and_credential_aliases_are_closed(key):
    projected = results.redact_external_row({
        "index": 0, "name": "case-a", key: "privateToken",
    })
    assert projected[key] == "<redacted>"
