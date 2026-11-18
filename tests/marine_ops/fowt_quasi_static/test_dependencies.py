# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Optional solver installation and required CI enrolment contracts."""
import json
import os
from pathlib import Path
import shlex
import subprocess
import sys
import tomllib

import yaml

ROOT = Path(__file__).resolve().parents[3]


def test_solver_extra_declares_the_executed_versions():
    project = tomllib.loads((ROOT / "pyproject.toml").read_text())
    # Pins are the retained calculation environment, not the test interpreter.
    # Source: pyproject.toml fowt extra and the committed report manifest.
    versions = {"capytaine": "3.0.0", "moorpy": "1.3.0",
                "shapely": "2.1.2", "matplotlib": "3.10.9"}
    assert set(project["project"]["optional-dependencies"]["fowt"]) == {
        f"{name}=={release}" for name, release in versions.items()}
    manifest_path, = (ROOT / "docs/reports").glob("fowt-*-mooring-analysis.manifest.json")
    retained = json.loads(manifest_path.read_text())["results"]
    assert {name.lower(): value for name, value in retained["versions"].items()} == {
        name: versions[name] for name in ("capytaine", "moorpy")}
    for run in retained["bem_provenance"].values():
        executed = run["inputs"]["versions"]
        assert all(executed[name] == versions[name] for name in ("capytaine", "shapely"))


def test_base_shapely_requirement_covers_the_imported_api():
    # Shapely 2.1.0 API docs: constrained_delaunay_triangles was added in 2.1.0.
    from packaging.requirements import Requirement

    project = tomllib.loads((ROOT / "pyproject.toml").read_text())
    requirement, = (Requirement(value) for value in project["project"]["dependencies"]
                    if Requirement(value).name == "shapely")
    assert "2.0.7" not in requirement.specifier
    assert "2.1.0" in requirement.specifier


def test_ci_domain_enrols_the_suite_and_installs_the_solver_extra():
    path = "tests/marine_ops/fowt_quasi_static/"
    domain = next(line for line in (ROOT / "tests/DOMAINS.md").read_text().splitlines()
                  if f"`{path}`" in line)
    name = domain.split("|")[1].strip()
    gate = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text())["gates"][f"tests-{name}"]
    assert gate["enabled"] and gate["failure_action"] == "block"
    assert path in gate["test_roots"]
    command = shlex.split(gate["command"])
    assert path in command
    assert command[command.index("--with-editable") + 1] == ".[test,fowt]"


def test_workflow_import_and_non_solver_functions_with_solvers_absent():
    environment = os.environ.copy()
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        environment.pop(name, None)
    code = """
import importlib.abc
import sys
class AbsentSolvers(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] in {'capytaine', 'moorpy'}:
            raise ModuleNotFoundError(fullname)
sys.meta_path.insert(0, AbsentSolvers())
sys.path.insert(0, sys.argv[1])
from digitalmodel.marine_ops.fowt_quasi_static import workflow
from digitalmodel.marine_ops.fowt_quasi_static.hydrodynamics import low_frequency_solver
from digitalmodel.marine_ops.fowt_quasi_static.mooring import grounded_horizontal_tension, mooring_system
assert grounded_horizontal_tension(779.6, 186., 850., 5846.) > 0
assert {'capytaine', 'moorpy'}.isdisjoint(sys.modules)
for function, args in ((low_frequency_solver, ()), (mooring_system, ({},))):
    try:
        function(*args)
    except ModuleNotFoundError:
        continue
    raise AssertionError('Solver execution requires its optional dependency')
"""
    result = subprocess.run(
        [sys.executable, "-I", "-B", "-c", code, str(ROOT / "src")],
        env=environment, capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
