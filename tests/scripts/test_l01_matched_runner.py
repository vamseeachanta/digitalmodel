# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic orchestration checks; no licensed library is imported."""
import ast
import importlib.util
import sys
from pathlib import Path
from types import ModuleType

import pytest

ROOT = Path(__file__).resolve().parents[2]


@pytest.fixture(autouse=True)
def forbid_licensed_api(monkeypatch):
    sentinel = ModuleType("OrcFxAPI")

    def refuse_access(name):
        raise AssertionError("licensed API access is forbidden in offline tests")

    sentinel.__getattr__ = refuse_access
    monkeypatch.setitem(sys.modules, "OrcFxAPI", sentinel)


def assert_no_module_scope_licensed_import(tree):
    for statement in tree.body:
        if isinstance(statement, (ast.FunctionDef, ast.ClassDef)):
            continue
        assert not any(
            (isinstance(node, ast.Import)
             and any(alias.name == "OrcFxAPI" for alias in node.names))
            or (isinstance(node, ast.ImportFrom) and node.module == "OrcFxAPI")
            for node in ast.walk(statement)
        ), "runner must not import the licensed API at module scope"


@pytest.mark.parametrize("source", [
    "import OrcFxAPI", "from OrcFxAPI import Diffraction",
])
def test_module_scope_guard_rejects_both_import_forms(source):
    with pytest.raises(AssertionError, match="module scope"):
        assert_no_module_scope_licensed_import(ast.parse(source))


@pytest.fixture
def runner():
    path = ROOT / "docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_matched_api.py"
    tree = ast.parse(path.read_text(encoding="utf-8"))
    assert_no_module_scope_licensed_import(tree)
    spec = importlib.util.spec_from_file_location("matched_runner", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class SyntheticDiffraction:
    # Synthetic cardinalities only; not engineering input.
    PeriodOrFrequency = [1]
    WaveHeading = [0]
    Bodies = []

    def __init__(self, error=None):
        self.error = error
        self.saved = False

    def Calculate(self):
        if self.error:
            raise self.error

    def SaveResults(self, path):
        self.saved = True


@pytest.mark.parametrize("error", [RuntimeError("calculation failed"),
                                  AttributeError("missing calculation method")])
def test_failed_calculation_never_saves_or_reports_success(runner, tmp_path, error):
    fake = SyntheticDiffraction(error)
    assert not runner.run_orcawave_analysis(
        tmp_path / "input.yml", tmp_path / "output.owr",
        diffraction_factory=lambda _: fake,
    )
    assert not fake.saved


def test_completed_calculation_saves_but_is_not_qualification(runner, tmp_path):
    fake = SyntheticDiffraction()
    assert runner.run_orcawave_analysis(
        tmp_path / "input.yml", tmp_path / "output.owr",
        diffraction_factory=lambda _: fake,
    )
    assert fake.saved


def test_save_failure_propagates(runner, tmp_path):
    fake = SyntheticDiffraction()

    def fail(_):
        raise OSError("save failed")

    fake.SaveResults = fail
    assert not runner.run_orcawave_analysis(
        tmp_path / "input.yml", tmp_path / "output.owr",
        diffraction_factory=lambda _: fake,
    )


def test_construction_failure_returns_false(runner):
    def fail(_):
        raise RuntimeError("synthetic load failure")

    assert not runner.run_orcawave_analysis(
        Path("input.yml"), Path("output.owr"), diffraction_factory=fail,
    )


def test_missing_configuration_returns_false(runner):
    assert not runner.run_orcawave_analysis(
        Path("input.yml"), Path("output.owr"), diffraction_factory=lambda _: object(),
    )


def test_entry_point_propagates_failure(runner, monkeypatch, tmp_path):
    calls = []
    monkeypatch.setattr(runner, "__file__", str(tmp_path / "runner.py"))
    input_path = tmp_path / "orcawave_001_ship_raos_rev2_matched.yml"
    input_path.write_text("# synthetic configuration; no solver input\n", encoding="utf-8")

    def fail(*args):
        calls.append(args)
        return False

    monkeypatch.setattr(runner, "run_orcawave_analysis", fail)
    assert runner.main() == 1
    assert calls == [(input_path, input_path.with_suffix(".owr"))]


def test_default_factory_cannot_access_licensed_api(runner, tmp_path):
    with pytest.raises(AssertionError, match="licensed API access is forbidden"):
        runner.run_orcawave_analysis(tmp_path / "input.yml", tmp_path / "output.owr")


def test_call_order(runner):
    calls = []
    fake = SyntheticDiffraction()
    fake.Calculate = lambda: calls.append("calculate")
    fake.SaveResults = lambda _: calls.append("save")

    def factory(_):
        calls.append("load")
        return fake

    assert runner.run_orcawave_analysis(
        Path("input.yml"), Path("output.owr"), diffraction_factory=factory,
    )
    assert calls == ["load", "calculate", "save"]
