"""Public smoke workflow/CLI guards; all probes are injected, never licensed."""

import importlib.util
import json
from pathlib import Path

import pytest

from digitalmodel.solvers.smoke import probes
from digitalmodel.solvers.smoke.workflow import SolverSmokeTestWorkflow


@pytest.mark.parametrize("require_all", [True, False])
@pytest.mark.parametrize("keep", [True, False])
def test_failure_report_precedes_raise_and_preserves_scratch_policy(
    monkeypatch, tmp_path, require_all, keep
):
    observed = []

    def fake_run(solvers, scratch):
        observed.append(scratch)
        (scratch / "owned.txt").write_text("fixture")
        return {"ok": False, "results": [{"solver": "orcaflex", "ok": False}]}

    monkeypatch.setattr(probes, "run_probes", fake_run)
    cfg = {"Analysis": {"result_folder": str(tmp_path)}, "solver_smoke_test": {
        "solvers": ["orcaflex"], "require_all": require_all, "keep_scratch": keep
    }}
    try:
        if require_all:
            with pytest.raises(RuntimeError, match="FAILED"):
                SolverSmokeTestWorkflow().router(cfg)
        else:
            assert SolverSmokeTestWorkflow().router(cfg) is cfg
        report = json.loads((tmp_path / "solver_smoke_test.json").read_text())
        assert report["ok"] is False
        assert observed[0].exists() is keep
        assert not observed[0].is_relative_to(tmp_path)
        if keep:
            assert cfg["solver_smoke_test"]["scratch_dir"] == str(observed[0])
    finally:
        if observed and observed[0].exists():
            (observed[0] / "owned.txt").unlink()
            observed[0].rmdir()


def test_workflow_cleans_scratch_when_injected_probe_raises(monkeypatch, tmp_path):
    observed = []

    def fail(solvers, scratch):
        observed.append(scratch)
        raise RuntimeError("injected")

    monkeypatch.setattr(probes, "run_probes", fail)
    with pytest.raises(RuntimeError, match="injected"):
        SolverSmokeTestWorkflow().router({"Analysis": {"result_folder": str(tmp_path)}})
    assert not observed[0].exists()


def test_mixed_dispatch_and_hostname_remain_explicit(monkeypatch, tmp_path):
    called = []

    def fake(name, ok):
        def run(path):
            called.append((name, path))
            return {"solver": name, "ok": ok}
        return run

    monkeypatch.setattr(probes, "CHECKS", {
        "orcaflex": fake("orcaflex", True), "aqwa": fake("aqwa", False)
    })
    monkeypatch.setattr(probes.platform, "node", lambda: "fixture-host")
    result = probes.run_probes(["orcaflex", "aqwa"], tmp_path)
    assert not result["ok"] and "host" not in result
    assert called == [("orcaflex", tmp_path / "orcaflex"), ("aqwa", tmp_path / "aqwa")]
    assert probes.run_probes(["orcaflex"], tmp_path, True)["host"] == "fixture-host"


@pytest.mark.parametrize("ok", [True, False])
def test_cli_json_and_exit_code(monkeypatch, tmp_path, capsys, ok):
    script = Path(__file__).resolve().parents[3] / "scripts/solver_smoke_test.py"
    spec = importlib.util.spec_from_file_location("smoke_cli_contract", script)
    cli = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(cli)
    observed = []

    def fake(solvers, scratch, include_host=False):
        print("injected probe chatter")
        observed.append((solvers, include_host))
        return {"ok": ok, "results": [{"solver": "orcaflex", "ok": ok}]}

    monkeypatch.setattr(cli, "run_probes", fake)
    code = cli.main(["--solver", "orcaflex", "--json", "--output-dir", str(tmp_path)])
    assert code == (0 if ok else 1)
    assert json.loads(capsys.readouterr().out)["ok"] is ok
    assert observed == [(["orcaflex"], False)]


def test_engine_dispatch_uses_real_smoke_configuration(monkeypatch, tmp_path):
    from digitalmodel.engine import engine

    def fake(solvers, scratch):
        assert solvers == ["orcaflex"]
        return {"ok": True, "results": [{"solver": "orcaflex", "ok": True}]}

    monkeypatch.setattr(probes, "run_probes", fake)
    input_file = tmp_path / "smoke.yml"
    input_file.write_text("basename: solver_smoke_test\nsolver_smoke_test:\n"
                          "  solvers: [orcaflex]\n", encoding="utf-8")
    result = engine(inputfile=str(input_file), root_folder=str(tmp_path))
    assert result["solver_smoke_test"]["report"]["ok"] is True
    path = Path(result["solver_smoke_test"]["report_path"])
    assert path.is_relative_to(tmp_path) and path.is_file()
