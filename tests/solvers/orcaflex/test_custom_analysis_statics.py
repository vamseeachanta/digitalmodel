# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Solver-free regressions for the failure contract in issue #2548.

Expected results follow an independent event trace: load, first solve, use
positions, save the calculated input, second solve, then optional simulation
and result saves. An exception or rejected state ends that trace. Thus a
failed first solve permits no save; a failed second solve permits only the
intermediate input save, never simulation or final result saves. There are no
engineering coefficients, acceptance limits or numerical solver results here.
"""

import importlib.util
import logging
from pathlib import Path
import sys
from types import ModuleType
from unittest.mock import Mock, call

import pytest

SOURCE_DIR = (
    Path(__file__).resolve().parents[3] / "src/digitalmodel/solvers/orcaflex"
)
state_spec = importlib.util.spec_from_file_location(
    "run_state_under_test", SOURCE_DIR / "run_state.py"
)
run_state = importlib.util.module_from_spec(state_spec)
state_spec.loader.exec_module(run_state)
SimulationStateError = run_state.SimulationStateError


@pytest.fixture
def analysis_module(monkeypatch):
    """Load the real implementation with unavailable dependencies replaced.

    The solver binding is replaced before module execution, so even a machine
    with a native installation cannot import or query the licensed solver.
    Unused scientific and file utilities are isolated for this control-flow test.
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
        monkeypatch.setitem(sys.modules, name, ModuleType(name))

    utilities = sys.modules["assetutilities.common.utilities"]
    utilities.is_file_valid_func = Mock()
    files = sys.modules["assetutilities.common.file_management"]
    files.get_filename_without_extension = lambda filename: {
        "with_path": str(Path(filename).with_suffix(""))
    }
    sys.modules["assetutilities.common.saveData"].saveDataYaml = Mock()
    sys.modules["assetutilities.common.ymlInput"].ymlInput = Mock()
    monkeypatch.setitem(
        sys.modules, "digitalmodel.solvers.orcaflex.run_state", run_state
    )

    source = SOURCE_DIR / "orcaflex_custom_analysis.py"
    spec = importlib.util.spec_from_file_location("custom_analysis_under_test", source)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def analysis(analysis_module):
    instance = analysis_module.OrcaFlexCustomAnalysis()
    instance.cfg = {
        "file_management": {"input_files": {"yml": ["case.yml"]}},
        "orcaflex": {
            "analysis": {
                "static": True,
                "simulation": True,
                "save_sim": True,
                "save_dat": True,
                "iterate": {"flag": False},
                "UseCalculatedPositions_cfg": {"flag": False},
            },
            "iterate": {
                "UseCalculatedPositions": {
                    "SetLinesToUserSpecifiedStartingShape": False,
                    "UseStaticLineEndOrientations": False,
                },
                "overwrite_data": False,
            },
        },
    }
    return instance


def model_for(*outcomes):
    """Each supplied outcome represents one solve in the event trace above."""
    model = Mock(state="Reset")
    pending = iter(outcomes)

    def calculate():
        outcome = next(pending)
        if isinstance(outcome, BaseException):
            raise outcome
        model.state = outcome

    def simulate():
        model.state = "SimulationStopped"

    model.CalculateStatics.side_effect = calculate
    model.RunSimulation.side_effect = simulate
    return model


@pytest.mark.parametrize(
    "outcome",
    [RuntimeError("second solve failed"), "Reset", "CalculatingStatics"],
)
def test_second_solve_failure_is_reported(analysis, caplog, outcome):
    """Issue #2548: first solve + second failure must yield false, not success."""
    model = model_for("InStaticState", outcome)
    with caplog.at_level(logging.ERROR):
        returned, success = analysis.run_static_analysis("case.yml", model)

    assert returned is model
    assert success is False
    assert model.CalculateStatics.call_args_list == [call(), call()]
    model.SaveData.assert_called_once_with("case_calculated_all.yml")
    assert "case.yml" in caplog.text
    assert caplog.records[-1].exc_info is not None
    if isinstance(outcome, Exception):
        assert caplog.records[-1].exc_info[1] is outcome
    else:
        assert isinstance(caplog.records[-1].exc_info[1], SimulationStateError)


@pytest.mark.parametrize("outcome", [RuntimeError("first solve failed"), "Reset"])
def test_first_solve_failure_stops_before_saving(analysis, caplog, outcome):
    """A failed first event cannot reach positions, saves or the second event."""
    model = model_for(outcome)
    _, success = analysis.run_static_analysis("case.yml", model)

    assert success is False
    model.CalculateStatics.assert_called_once_with()
    model.UseCalculatedPositions.assert_not_called()
    model.SaveData.assert_not_called()
    assert caplog.records[-1].exc_info is not None


def test_success_requires_both_solves(analysis, caplog):
    """Two completed solve events permit the existing successful return."""
    model = model_for("InStaticState", "InStaticState")
    returned, success = analysis.run_static_analysis("case.yml", model)

    assert returned is model
    assert success is True
    assert model.CalculateStatics.call_args_list == [call(), call()]
    model.UseCalculatedPositions.assert_called_once_with()
    model.SaveData.assert_called_once_with("case_calculated_all.yml")
    assert not caplog.records


@pytest.mark.parametrize("outcome", [RuntimeError("second solve failed"), "Reset"])
def test_helper_propagates_failure(analysis, outcome):
    """The helper must raise so every caller can distinguish a failed solve."""
    model = model_for(outcome)
    expected_type = RuntimeError if isinstance(outcome, Exception) else SimulationStateError
    with pytest.raises(expected_type):
        analysis.analysis_with_calculated_positions(model)


def test_helper_returns_successful_model(analysis):
    model = model_for("InStaticState")
    assert analysis.analysis_with_calculated_positions(model) is model


def test_intermediate_save_failure_is_reported(analysis, caplog):
    """An input-save exception stops the trace before the second solve."""
    model = model_for("InStaticState")
    error = OSError("input save failed")
    model.SaveData.side_effect = error
    _, success = analysis.run_static_analysis("case.yml", model)

    assert success is False
    model.CalculateStatics.assert_called_once_with()
    assert caplog.records[-1].exc_info[1] is error


def test_interrupt_is_not_reported_as_a_completed_failure(analysis):
    model = model_for(KeyboardInterrupt())
    with pytest.raises(KeyboardInterrupt):
        analysis.run_static_analysis("case.yml", model)


@pytest.mark.parametrize("outcome", [RuntimeError("second solve failed"), "Reset"])
def test_failed_second_solve_suppresses_downstream_work(
    analysis, analysis_module, caplog, outcome
):
    """Only the intermediate input save precedes the failed second event."""
    model = model_for("InStaticState", outcome)
    analysis_module.OrcFxAPI.Model = Mock(return_value=model)
    with caplog.at_level(logging.INFO):
        analysis.process_fea()

    model.LoadData.assert_called_once_with("case.yml")
    model.RunSimulation.assert_not_called()
    model.SaveSimulation.assert_not_called()
    model.SaveData.assert_called_once_with("case_calculated_all.yml")
    assert model.method_calls == [
        call.LoadData("case.yml"),
        call.CalculateStatics(),
        call.UseCalculatedPositions(),
        call.SaveData("case_calculated_all.yml"),
        call.CalculateStatics(),
    ]
    assert "Run statics  ....  FAIL" in caplog.text
    assert "Run statics  ....  SUCCESS" not in caplog.text


def test_success_keeps_downstream_simulation_and_saves(analysis, analysis_module, caplog):
    """Completing both solve events permits simulation and both result saves."""
    model = model_for("InStaticState", "InStaticState")
    analysis_module.OrcFxAPI.Model = Mock(return_value=model)
    with caplog.at_level(logging.INFO):
        analysis.process_fea()

    model.RunSimulation.assert_called_once_with()
    model.SaveSimulation.assert_called_once_with("case.sim")
    assert model.method_calls == [
        call.LoadData("case.yml"),
        call.CalculateStatics(),
        call.UseCalculatedPositions(),
        call.SaveData("case_calculated_all.yml"),
        call.CalculateStatics(),
        call.RunSimulation(),
        call.SaveSimulation("case.sim"),
        call.SaveData("case.dat"),
    ]
    assert "Run statics  ....  SUCCESS" in caplog.text
