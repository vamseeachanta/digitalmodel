"""Offline runner regressions; no solver executable is discovered or launched."""

from pathlib import Path
from types import SimpleNamespace

import pytest

from digitalmodel.ansys.runner import ANSYSRunConfig, ANSYSRunner, ANSYSRunStatus, run_ansys


def test_single_node_defaults_to_smp(tmp_path, monkeypatch):
    """Issue 2196 requires SMP by default; the expected flag is literal policy."""
    script = tmp_path / "model.inp"
    script.write_text("FINISH\n", encoding="utf-8")
    monkeypatch.setattr(ANSYSRunner, "_detect_executable", lambda self: Path("fake"))

    def simulate(argv, **kwargs):
        assert "-smp" in argv
        assert "-dis" not in argv
        (Path(kwargs["cwd"]) / "model.out").write_text("completed", encoding="utf-8")
        return SimpleNamespace(returncode=0, stdout="", stderr="")

    monkeypatch.setattr("digitalmodel.ansys.runner._run_solver_process", simulate)
    assert run_ansys(script, output_dir=tmp_path / "out").status == ANSYSRunStatus.COMPLETED


@pytest.mark.parametrize("mode", ["-smp", "-dis", "-SMP", "-DIS", "-mpi", "-machines"])
def test_explicit_mode_is_preserved(tmp_path, monkeypatch, mode):
    """Explicit MAPDL mode flags are passed exactly once, without a conflicting flag."""
    script = tmp_path / "model.inp"
    script.write_text("FINISH\n", encoding="utf-8")
    monkeypatch.setattr(ANSYSRunner, "_detect_executable", lambda self: Path("fake"))

    def simulate(argv, **kwargs):
        assert argv.count(mode) == 1
        assert ("-dis" if mode == "-smp" else "-smp") not in argv
        (Path(kwargs["cwd"]) / "model.out").write_text("completed", encoding="utf-8")
        return SimpleNamespace(returncode=0, stdout="", stderr="")

    monkeypatch.setattr("digitalmodel.ansys.runner._run_solver_process", simulate)
    result = run_ansys(script, output_dir=tmp_path / "out", extra_args=[mode])
    assert result.status == ANSYSRunStatus.COMPLETED


@pytest.mark.parametrize("timeout", [0, -1, float("nan"), float("inf"), True, None, "1"])
def test_unbounded_or_invalid_timeout_is_rejected(timeout):
    """A finite positive wall-clock budget is required by the hang-prevention contract."""
    with pytest.raises(ValueError, match="finite and positive"):
        ANSYSRunConfig(timeout_seconds=timeout)


def test_conflicting_modes_are_rejected():
    with pytest.raises(ValueError, match="either"):
        ANSYSRunConfig(extra_args=["-smp", "-dis"])


@pytest.mark.parametrize("entrypoint", ["config", "function", "workflow"])
@pytest.mark.parametrize("extra_args", ["-dis", ("-dis",), {"-dis"}, [1], [None], False])
def test_invalid_extra_args_are_rejected(tmp_path, extra_args, entrypoint):
    """Scalar YAML arguments must never be expanded into individual characters."""
    with pytest.raises(ValueError, match="list of str"):
        if entrypoint == "config":
            ANSYSRunConfig(extra_args=extra_args)
        elif entrypoint == "function":
            run_ansys(tmp_path / "model.inp", dry_run=True, extra_args=extra_args)
        else:
            from digitalmodel.ansys.workflow import AnsysWorkflow

            AnsysWorkflow().router({"ansys": {
                "script": str(tmp_path / "model.inp"), "dry_run": True,
                "extra_args": extra_args,
            }})


@pytest.mark.parametrize("distributed", ["-DIS", "-mpi", "-MPI", "-machines", "-MACHINES"])
def test_distributed_alias_conflicts_with_smp(distributed):
    with pytest.raises(ValueError, match="either"):
        ANSYSRunConfig(extra_args=["-SMP", distributed])
