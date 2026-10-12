"""Defect 3 of workspace-hub#3838 -- an unstable OrcaFlex run reports success.

``RunSimulation`` returns ``stOK`` while setting the model to
``SimulationStoppedUnstable``, so exception-only handling ships a diverged run as
good. The remedy under test is
``digitalmodel.solvers.orcaflex.run_state``: one gate the run paths call after a
solve completes.

Every case here drives a stubbed model state, so none of them needs an OrcaFlex
licence. The two cases that read the vendor enum need only the import.
"""

from __future__ import annotations

import importlib.util
import os.path

import pytest

from digitalmodel.solvers.orcaflex import run_state

ORCFXAPI_INSTALLED = importlib.util.find_spec("OrcFxAPI") is not None


class _EnumLikeState:
    """Stands in for ``OrcFxAPI.ModelState``: carries a name, prints as an int.

    The vendor enum is an ``IntEnum``, so ``str()`` of a member is the integer
    value, not the member name. A gate that compares ``str(model.state)`` to a
    list of names therefore never matches anything.
    """

    def __init__(self, name: str, value: int):
        self.name = name
        self._name_ = name
        self.value = value

    def __str__(self) -> str:
        return str(self.value)


class _StubModel:
    def __init__(self, state):
        self.state = state


def _model(state_name: str) -> _StubModel:
    values = {
        "Reset": 0,
        "CalculatingStatics": 1,
        "InStaticState": 2,
        "RunningSimulation": 3,
        "SimulationStopped": 4,
        "SimulationStoppedUnstable": 5,
    }
    return _StubModel(_EnumLikeState(state_name, values[state_name]))


# ---------------------------------------------------------------------------
# Test 13 -- a run completing in SimulationStoppedUnstable is rejected
# ---------------------------------------------------------------------------


def test_unstable_simulation_is_rejected():
    with pytest.raises(run_state.SimulationStateError) as excinfo:
        run_state.check_simulation(_model("SimulationStoppedUnstable"), context="run.sim")

    message = str(excinfo.value)
    assert "SimulationStoppedUnstable" in message
    assert "run.sim" in message


def test_unstable_statics_is_rejected():
    with pytest.raises(run_state.SimulationStateError):
        run_state.check_statics(_model("SimulationStoppedUnstable"), context="statics")


# ---------------------------------------------------------------------------
# Test 14 -- a run completing in SimulationStopped is accepted
# ---------------------------------------------------------------------------


def test_completed_simulation_is_accepted():
    assert run_state.check_simulation(_model("SimulationStopped")) == "SimulationStopped"


def test_completed_statics_is_accepted():
    assert run_state.check_statics(_model("InStaticState")) == "InStaticState"


def test_simulation_stopped_is_accepted_by_the_statics_gate():
    assert run_state.check_statics(_model("SimulationStopped")) == "SimulationStopped"


# ---------------------------------------------------------------------------
# Incomplete runs are rejected too -- the gate is about the state, not only
# about instability.
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("name", ["Reset", "InStaticState", "RunningSimulation"])
def test_simulation_gate_rejects_states_that_are_not_a_finished_dynamic_run(name):
    with pytest.raises(run_state.SimulationStateError):
        run_state.check_simulation(_model(name))


@pytest.mark.parametrize("name", ["Reset", "CalculatingStatics"])
def test_statics_gate_rejects_states_that_are_not_a_finished_solve(name):
    with pytest.raises(run_state.SimulationStateError):
        run_state.check_statics(_model(name))


# ---------------------------------------------------------------------------
# The name resolution the gate depends on
# ---------------------------------------------------------------------------


def test_state_name_prefers_the_member_name_over_str():
    """``str(state)`` is the integer, so the name must come from ``.name``."""
    model = _model("SimulationStopped")
    assert str(model.state) == "4"
    assert run_state.state_name(model) == "SimulationStopped"


def test_an_unresolvable_state_is_indeterminate_and_does_not_raise():
    """A test double whose state is not an OrcaFlex state must pass through.

    The gate is added to paths that existing tests already drive with mocks. It
    must reject a state it recognises as bad, never a state it cannot read.
    """

    class _Opaque:
        pass

    assert run_state.state_name(_StubModel(_Opaque())) is None
    assert run_state.check_simulation(_StubModel(_Opaque())) is None
    assert run_state.check_statics(_StubModel(_Opaque())) is None


def test_a_model_without_a_state_attribute_is_indeterminate():
    class _NoState:
        pass

    assert run_state.state_name(_NoState()) is None
    assert run_state.check_simulation(_NoState()) is None


def test_an_undefined_vendor_state_is_rejected():
    """``IntEnumAllowsUndefined`` fabricates ``UNDEFINED_<n>`` members. An
    unrecognised state coming back from the solver is not a pass."""
    with pytest.raises(run_state.SimulationStateError):
        run_state.check_simulation(_StubModel(_EnumLikeState("UNDEFINED_9", 9)))


@pytest.mark.solver
@pytest.mark.skipif(not ORCFXAPI_INSTALLED, reason="OrcFxAPI is not installed")
def test_vendor_enum_members_resolve_by_name():
    """Importing OrcFxAPI claims no licence; only constructing a Model does."""
    import OrcFxAPI

    assert str(OrcFxAPI.ModelState.SimulationStopped) != "SimulationStopped", (
        "the premise of this defect: str() of the vendor enum is not its name"
    )
    assert (
        run_state.state_name(_StubModel(OrcFxAPI.ModelState.SimulationStopped))
        == "SimulationStopped"
    )
    assert (
        run_state.state_name(_StubModel(OrcFxAPI.ModelState.SimulationStoppedUnstable))
        == "SimulationStoppedUnstable"
    )
    with pytest.raises(run_state.SimulationStateError):
        run_state.check_simulation(
            _StubModel(OrcFxAPI.ModelState.SimulationStoppedUnstable)
        )


@pytest.mark.solver
@pytest.mark.skipif(not ORCFXAPI_INSTALLED, reason="OrcFxAPI is not installed")
def test_the_gate_covers_every_state_the_vendor_enum_defines():
    """A state the gate does not know is a state it cannot judge, so the known
    set must track the vendor enum rather than a hand-copied list."""
    import OrcFxAPI

    vendor = {
        name
        for name in dir(OrcFxAPI.ModelState)
        if not name.startswith("_") and isinstance(
            getattr(OrcFxAPI.ModelState, name), OrcFxAPI.ModelState
        )
    }
    assert vendor == set(run_state.KNOWN_STATES)


# ---------------------------------------------------------------------------
# The load-time gate in orcaflex_analysis_components
# ---------------------------------------------------------------------------


def test_loaded_simulation_in_an_unstable_state_is_not_returned(monkeypatch):
    from digitalmodel.solvers.orcaflex import orcaflex_analysis_components as oac

    analysis = oac.OrcaFlexAnalysis.__new__(oac.OrcaFlexAnalysis)
    unstable = _model("SimulationStoppedUnstable")

    monkeypatch.setattr(os.path, "isfile", lambda _path: True)
    monkeypatch.setattr(
        oac.OrcaFlexAnalysis, "loadSimulation", lambda self, name: unstable
    )
    monkeypatch.setattr(
        oac.OrcaFlexAnalysis, "get_SimulationFileName", lambda self, name: "x.sim"
    )

    assert analysis.get_model_from_filename("x.yml") is None


def test_loaded_simulation_in_a_stopped_state_is_returned(monkeypatch):
    from digitalmodel.solvers.orcaflex import orcaflex_analysis_components as oac

    analysis = oac.OrcaFlexAnalysis.__new__(oac.OrcaFlexAnalysis)
    stopped = _model("SimulationStopped")

    monkeypatch.setattr(os.path, "isfile", lambda _path: True)
    monkeypatch.setattr(
        oac.OrcaFlexAnalysis, "loadSimulation", lambda self, name: stopped
    )
    monkeypatch.setattr(
        oac.OrcaFlexAnalysis, "get_SimulationFileName", lambda self, name: "x.sim"
    )

    assert analysis.get_model_from_filename("x.yml") is stopped
