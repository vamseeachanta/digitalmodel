"""Synthetic orchestration checks; no licensed library is imported."""
import ast
import importlib.util
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]


@pytest.fixture
def runner():
    path = ROOT / "docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_matched_api.py"
    tree = ast.parse(path.read_text(encoding="utf-8"))
    for statement in tree.body:
        if isinstance(statement, (ast.FunctionDef, ast.ClassDef)):
            continue
        assert not any(
            isinstance(node, ast.Import)
            and any(alias.name == "OrcFxAPI" for alias in node.names)
            for node in ast.walk(statement)
        ), "runner must not import the licensed API at module scope"
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


def test_entry_point_propagates_failure(runner, monkeypatch):
    monkeypatch.setattr(runner, "run_orcawave_analysis", lambda *args: False)
    assert runner.main() == 1


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
