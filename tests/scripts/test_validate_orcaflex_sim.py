"""scripts/validate_orcaflex_sim.py accepts only a genuinely completed simulation.

OrcFxAPI is mocked so the test runs without a licensed install.
"""

from __future__ import annotations

import enum
import importlib.util
import sys
import types
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[2]
SCRIPT = REPO / "scripts" / "validate_orcaflex_sim.py"


class ModelState(enum.IntEnum):
    Reset = 0
    CalculatingStatics = 1
    InStaticState = 2
    RunningSimulation = 3
    SimulationStopped = 4
    SimulationStoppedUnstable = 5
    SimulationPaused = 6


class _RangeGraph:
    Min = [-1.0, -2.0]
    Max = [3.0, 4.0]


class _Line:
    typeName = "Line"

    def __init__(self, name: str, missing: tuple[str, ...] = ()):
        self.name = name
        self._missing = missing

    def RangeGraph(self, var, period):
        if var in self._missing:
            raise ValueError(f"no {var}")
        return _RangeGraph()


_ABSENT = object()


def _fake_api(state, *, complete=True, lines=None):
    api = types.ModuleType("OrcFxAPI")
    api.ModelState = ModelState
    api.pnWholeSimulation = 0
    api.DLLVersion = lambda: "mock"
    api.Period = lambda p: p

    class Model:
        def __init__(self, path):
            self.state = state
            if complete is not _ABSENT:
                self.simulationComplete = complete
            self.objects = [_Line("L1")] if lines is None else lines

    api.Model = Model
    return api


def _run(monkeypatch, capsys, api, argv=("x", "case.sim")):
    monkeypatch.setitem(sys.modules, "OrcFxAPI", api)
    spec = importlib.util.spec_from_file_location("_validate_sim_under_test", SCRIPT)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    rc = mod.main(list(argv))
    return rc, capsys.readouterr().out


def test_completed_simulation_passes(monkeypatch, capsys):
    rc, out = _run(monkeypatch, capsys, _fake_api(ModelState.SimulationStopped))
    assert rc == 0
    assert "VALIDATE: PASS" in out


@pytest.mark.parametrize(
    "state, phrase",
    [
        (ModelState.SimulationPaused, "paused"),
        (ModelState.SimulationStoppedUnstable, "unstable"),
        (ModelState.RunningSimulation, "still running"),
        (ModelState.Reset, "reset"),
        (ModelState.CalculatingStatics, "statics"),
        (ModelState.InStaticState, "only statics"),
    ],
)
def test_non_completed_states_fail(monkeypatch, capsys, state, phrase):
    rc, out = _run(monkeypatch, capsys, _fake_api(state))
    assert rc == 1
    assert "VALIDATE: FAIL" in out
    assert state.name in out
    assert phrase in out
    assert "PASS" not in out


def test_stopped_but_not_complete_fails(monkeypatch, capsys):
    rc, out = _run(monkeypatch, capsys, _fake_api(ModelState.SimulationStopped, complete=False))
    assert rc == 1
    assert "simulationComplete" in out


def test_stopped_without_completion_flag_fails(monkeypatch, capsys):
    rc, out = _run(monkeypatch, capsys, _fake_api(ModelState.SimulationStopped, complete=_ABSENT))
    assert rc == 1
    assert "VALIDATE: FAIL" in out
    assert "simulationComplete is not exposed" in out
    assert "PASS" not in out


class _CountingLine(_Line):
    def __init__(self, name, missing=()):
        super().__init__(name, missing)
        self.calls = 0

    def RangeGraph(self, var, period):
        self.calls += 1
        return super().RangeGraph(var, period)


def test_every_line_is_sampled_when_all_pass(monkeypatch, capsys):
    lines = [_CountingLine("Riser"), _CountingLine("Mooring")]
    rc, out = _run(monkeypatch, capsys, _fake_api(ModelState.SimulationStopped, lines=lines))
    assert rc == 0
    assert "VALIDATE: PASS" in out
    assert [ln.calls for ln in lines] == [5, 5]
    assert "range-graph sample for Riser" in out
    assert "range-graph sample for Mooring" in out


def test_failing_first_line_fails_run(monkeypatch, capsys):
    lines = [_CountingLine("Hawser", missing=("Bend Moment",)), _CountingLine("Riser")]
    rc, out = _run(monkeypatch, capsys, _fake_api(ModelState.SimulationStopped, lines=lines))
    assert rc == 1
    assert "Hawser: Bend Moment" in out
    assert "PASS" not in out
    assert lines[1].calls == 5


def test_failing_later_line_fails_run(monkeypatch, capsys):
    lines = [_CountingLine("Riser"), _CountingLine("Hawser", missing=("Curvature",))]
    rc, out = _run(monkeypatch, capsys, _fake_api(ModelState.SimulationStopped, lines=lines))
    assert rc == 1
    assert "Hawser: Curvature" in out
    assert "PASS" not in out
    assert [ln.calls for ln in lines] == [5, 5]


def test_no_line_fails(monkeypatch, capsys):
    rc, out = _run(monkeypatch, capsys, _fake_api(ModelState.SimulationStopped, lines=[]))
    assert rc == 1
    assert "no Line object" in out


def test_missing_path_exits_2(monkeypatch, capsys):
    rc, out = _run(monkeypatch, capsys, _fake_api(ModelState.SimulationStopped), argv=("x",))
    assert rc == 2
