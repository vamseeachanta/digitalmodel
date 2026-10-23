"""Keep this new test directory in the explicit solver-smoke CI invocation."""

import importlib.util
from pathlib import Path
import shlex
import sys

import pytest
import yaml


ROOT = Path(__file__).resolve().parents[3]


def test_smoke_regressions_are_in_metadata_and_executed_arguments():
    config = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text())
    gate = config["gates"]["tests-solver-smoke"]
    assert "tests/solvers/smoke/" in gate["test_roots"]
    assert "tests/solvers/smoke/" in shlex.split(gate["command"])
    assert "`tests/solvers/smoke/`" in (ROOT / "tests/DOMAINS.md").read_text()


def test_touched_source_and_new_test_paths_select_smoke(monkeypatch):
    path = ROOT / "scripts/ci/detect_touched_domains.py"
    spec = importlib.util.spec_from_file_location("smoke_domain_detector", path)
    detector = importlib.util.module_from_spec(spec)
    monkeypatch.setitem(sys.modules, spec.name, detector)
    spec.loader.exec_module(detector)
    domains = detector.parse_domains(ROOT / "tests/DOMAINS.md")
    for changed in ["src/digitalmodel/solvers/smoke/probes.py",
                    "tests/solvers/smoke/test_orcaflex_probe_contract.py"]:
        selected = detector.touched_domains([changed], domains)
        assert "solver-smoke" in {domain.name for domain in selected}


@pytest.mark.parametrize("event", ["push", "pull_request"])
def test_public_mesh_changes_trigger_gmsh_workflow(event):
    workflow_path = ".github/workflows/gmsh-meshing-tests.yml"
    workflow = yaml.safe_load((ROOT / workflow_path).read_text())
    # PyYAML's YAML 1.1 loader treats the Actions key "on" as True.
    events = workflow.get("on", workflow.get(True))
    mesh_test = "tests/hydrodynamics/test_oc4_public_mesh.py"
    registration_test = "tests/solvers/smoke/test_ci_registration.py"
    example = "examples/hydrodynamics/oc4-semisub/**"
    assert {mesh_test, registration_test, example} <= set(events[event]["paths"])


def test_public_mesh_tests_execute_with_locked_gmsh_and_runtime():
    workflow = yaml.safe_load(
        (ROOT / ".github/workflows/gmsh-meshing-tests.yml").read_text()
    )
    mesh_test = "tests/hydrodynamics/test_oc4_public_mesh.py"
    registration_test = "tests/solvers/smoke/test_ci_registration.py"
    steps = workflow["jobs"]["focused-tests"]["steps"]
    commands = [
        shlex.split(step["run"].replace("\\\n", " ")) for step in steps if "run" in step
    ]
    test_commands = [args for args in commands if "pytest" in args]
    assert any({mesh_test, registration_test} <= set(args) for args in test_commands)
    assert any("libglu1-mesa" in args for args in commands)
    assert any(
        args[:2] == ["uv", "sync"]
        and "--frozen" in args
        and "cfd" in args
        and "test" in args
        for args in commands
    )
    assert any(
        "--frozen" in args and "cfd" in args and "test" in args and mesh_test in args
        for args in test_commands
    )
