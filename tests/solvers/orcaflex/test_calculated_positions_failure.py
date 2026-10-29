"""Regression contract for issue #2548, without importing a solver binding.

Expected outcomes follow an independent call trace: a first successful solve
allows calculated positions to be saved; a failed second solve means False,
one recorded failure, and no dynamics or final result saves. Two successful
solves mean True. These are event counts, not engineering acceptance limits.
"""

import importlib.util
import logging
from pathlib import Path
import subprocess
import sys
import types

import pytest

from digitalmodel.solvers.orcaflex.run_state import SimulationStateError

SOURCE = (
    Path(__file__).resolve().parents[3]
    / "src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py"
)


def load_analysis(monkeypatch):
    """Load the complete module with import-only dependencies replaced.

    Only the configuration and statics workflow is exercised. Scientific and
    file utility stand-ins avoid optional installations; no stub supplies the
    control flow or state gate under test. The solver binding is always a stub.
    """
    for name in (
        "OrcFxAPI",
        "pandas",
        "numpy",
        "scipy",
        "scipy.interpolate",
        "assetutilities",
        "assetutilities.common",
        "assetutilities.common.utilities",
        "assetutilities.common.file_management",
        "assetutilities.common.saveData",
        "assetutilities.common.ymlInput",
    ):
        monkeypatch.setitem(sys.modules, name, types.ModuleType(name))
    utilities = sys.modules["assetutilities.common.utilities"]
    utilities.is_file_valid_func = lambda name, root: (True, name)
    files = sys.modules["assetutilities.common.file_management"]
    files.get_filename_without_extension = lambda name: {
        "with_path": str(Path(name).with_suffix(""))
    }
    sys.modules["assetutilities.common.saveData"].saveDataYaml = lambda *args: None
    sys.modules["assetutilities.common.ymlInput"].ymlInput = lambda name: {}
    spec = importlib.util.spec_from_file_location("calculated_positions_under_test", SOURCE)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class SolverStub:
    """A scripted sequence of solve results and an ordered event record."""

    def __init__(self, outcomes):
        self.outcomes = iter(outcomes)
        self.events = []
        self.state = "Reset"

    def CalculateStatics(self):
        self.events.append("statics")
        outcome = next(self.outcomes)
        if isinstance(outcome, BaseException):
            raise outcome
        self.state = outcome

    def UseCalculatedPositions(self, **kwargs):
        self.events.append(("positions", kwargs))

    def SaveData(self, name):
        self.events.append(("save_data", name))

    def LoadData(self, name):
        self.events.append(("load", name))

    def RunSimulation(self):
        self.events.append("dynamics")
        self.state = "SimulationStopped"

    def SaveSimulation(self, name):
        self.events.append(("save_simulation", name))


def configured_analysis(module, model):
    analysis = module.OrcaFlexCustomAnalysis()
    analysis.cfg = {
        "file_management": {"input_files": {"yml": ["case.yml"]}},
        "orcaflex": {
            "analysis": {
                "static": True,
                "simulation": True,
                "save_sim": True,
                "save_dat": True,
                "iterate": {"flag": False},
                "UseCalculatedPositions_cfg": {
                    "flag": False,
                    "clean_StaleCalculatedPositions": False,
                },
            },
            "iterate": {
                "overwrite_data": False,
                "UseCalculatedPositions": {
                    "SetLinesToUserSpecifiedStartingShape": False,
                    "UseStaticLineEndOrientations": False,
                },
            },
        },
    }
    module.OrcFxAPI.Model = lambda: model
    return analysis


@pytest.fixture
def module(monkeypatch):
    return load_analysis(monkeypatch)


@pytest.mark.parametrize("outcome", [RuntimeError("second solve failed"), "Reset"])
def test_second_solve_failure_propagates(module, outcome):
    """A failed call or rejected state cannot return a model as a success."""
    model = SolverStub([outcome])
    analysis = configured_analysis(module, model)
    expected = RuntimeError if isinstance(outcome, Exception) else SimulationStateError
    with pytest.raises(expected):
        analysis.analysis_with_calculated_positions(model)


@pytest.mark.parametrize(
    "outcomes, message, expected_events",
    [
        ([RuntimeError("first solve failed")], "first solve failed", ["statics"]),
        (["Reset"], "Reset", ["statics"]),
        (
            ["InStaticState", RuntimeError("second solve failed")],
            "second solve failed",
            ["statics", ("positions", {}), ("save_data", "case_calculated_all.yml"), "statics"],
        ),
        (
            ["InStaticState", "Reset"],
            "Reset",
            ["statics", ("positions", {}), ("save_data", "case_calculated_all.yml"), "statics"],
        ),
    ],
)
def test_static_failure_returns_false_and_records_error(
    module, outcomes, message, expected_events, caplog
):
    """Hand trace: first failure stops immediately; second follows a position save."""
    model = SolverStub(outcomes)
    analysis = configured_analysis(module, model)
    with caplog.at_level(logging.ERROR):
        returned, success = analysis.run_static_analysis("case.yml", model)
    assert returned is model
    assert success is False
    assert model.events == expected_events
    assert len(caplog.records) == 1
    assert "case.yml" in caplog.text
    assert message in caplog.text
    assert caplog.records[0].exc_info is not None


@pytest.mark.parametrize("overwrite", [False, True])
def test_success_requires_both_solves(module, overwrite, caplog):
    """Two successes give True; the sole save uses the configured overwrite rule."""
    model = SolverStub(["InStaticState", "InStaticState"])
    analysis = configured_analysis(module, model)
    analysis.cfg["orcaflex"]["iterate"]["overwrite_data"] = overwrite
    returned, success = analysis.run_static_analysis("case.yml", model)
    assert returned is model
    assert success is True
    destination = "case.yml" if overwrite else "case_calculated_all.yml"
    assert model.events == [
        "statics", ("positions", {}), ("save_data", destination), "statics"
    ]
    assert not caplog.records


@pytest.mark.parametrize("failure", [RuntimeError("second solve failed"), "Reset"])
def test_workflow_failure_stops_before_dynamics_and_final_saves(module, failure, caplog):
    """Failure has no dynamic solve, final saves, iteration, or aggregate save."""
    model = SolverStub(["InStaticState", failure])
    analysis = configured_analysis(module, model)
    analysis.cfg["file_management"]["input_files"]["yml"].append("later.yml")
    analysis.cfg["orcaflex"]["analysis"]["iterate"] = {
        "flag": True, "to_target_value": True
    }
    analysis.iterate_to_target_value = lambda *args: pytest.fail("iteration after failure")
    analysis.save_data = lambda: pytest.fail("aggregate save after failure")
    with pytest.raises(RuntimeError, match="Static analysis failed.*case.yml"):
        analysis.perform_simulations()
    assert model.events == [
        ("load", "case.yml"), "statics", ("positions", {}),
        ("save_data", "case_calculated_all.yml"), "statics"
    ]
    assert len(caplog.records) == 1
    assert "Run statics  ....  SUCCESS" not in caplog.text


def test_workflow_success_keeps_dynamics_and_saves(module):
    """Hand trace: load, solve, positions/save, solve, dynamics, final saves."""
    model = SolverStub(["InStaticState", "InStaticState"])
    analysis = configured_analysis(module, model)
    analysis.process_fea()
    assert model.events == [
        ("load", "case.yml"), "statics", ("positions", {}),
        ("save_data", "case_calculated_all.yml"), "statics", "dynamics",
        ("save_simulation", "case.sim"), ("save_data", "case.dat")
    ]


def test_position_save_failure_is_also_recorded(module, monkeypatch, caplog):
    """A save failure between solves prevents the second solve and returns False."""
    model = SolverStub(["InStaticState"])
    analysis = configured_analysis(module, model)

    def fail_save(name):
        raise OSError("position save failed")

    monkeypatch.setattr(model, "SaveData", fail_save)
    assert analysis.run_static_analysis("case.yml", model) == (model, False)
    assert model.events == ["statics", ("positions", {})]
    assert "position save failed" in caplog.text


@pytest.mark.parametrize("interrupt", [KeyboardInterrupt, SystemExit])
def test_process_control_exceptions_are_not_swallowed(module, interrupt):
    """Interrupts leave the workflow rather than becoming ordinary failure flags."""
    model = SolverStub(["InStaticState", interrupt()])
    analysis = configured_analysis(module, model)
    with pytest.raises(interrupt):
        analysis.run_static_analysis("case.yml", model)


def test_unhandled_workflow_failure_has_nonzero_exit():
    """An uncaught workflow exception gives a non-zero interpreter status.

    Independent expectation: successful first call, failed second call, then
    exception escape. The interpreter must not reach normal termination.
    """
    script = """
import runpy
import pytest
import sys
from pathlib import Path
sys.path.insert(0, str(Path(sys.argv[1]).resolve().parents[3] / 'src'))
helpers = runpy.run_path(sys.argv[1])
with pytest.MonkeyPatch.context() as patch:
    module = helpers['load_analysis'](patch)
    model = helpers['SolverStub'](['InStaticState', RuntimeError('second solve failed')])
    analysis = helpers['configured_analysis'](module, model)
    analysis.perform_simulations()
"""
    result = subprocess.run(
        [sys.executable, "-c", script, str(Path(__file__).resolve())],
        capture_output=True, text=True, check=False, timeout=30,
    )
    assert result.returncode != 0
    assert "second solve failed" in result.stderr
    assert "Static analysis failed for case.yml" in result.stderr
