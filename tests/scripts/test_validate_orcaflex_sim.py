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


def _fake_api(state, *, complete=True, lines=None, frequency_domain=False):
    api = types.ModuleType("OrcFxAPI")
    api.ModelState = ModelState
    api.pnWholeSimulation = 0
    api.DLLVersion = lambda: "mock"
    api.Period = lambda p: p

    class Model:
        def __init__(self, path):
            self.state = state
            if frequency_domain is not _ABSENT:
                self.isFrequencyDomainDynamics = frequency_domain
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


class _FrequencyDomainLine(_CountingLine):
    """Whole-simulation fields per Orcina's OrcaFlexLineObject.RangeGraph reference.

    Public online reference, accessed 2026-10-19:
    https://www.orcina.com/webhelp/OrcFxAPI/Content/html/Pythonreference,OrcaFlexLineObject.htm
    Empty arrays suffice: no physical values are evaluated in the deferred check.
    """

    def RangeGraph(self, var, period):
        self.calls += 1
        return types.SimpleNamespace(X=[], StaticValue=[], StdDev=[], Upper=[], Lower=[])


def test_frequency_domain_is_not_evaluated(monkeypatch, capsys):
    """Issue #2325 permits deferral, never PASS or missing-variable FAIL.

    Exit 3 is the application's distinct deferred status, not a solver value.
    The documented whole-simulation fields lack Min/Max, so no extrema query
    should occur; the hand-counted expected number of queries is zero.
    """
    lines = [_FrequencyDomainLine("LineA"), _FrequencyDomainLine("LineB")]
    api = _fake_api(ModelState.SimulationStopped, lines=lines, frequency_domain=True)
    rc, out = _run(monkeypatch, capsys, api)
    assert rc == 3
    assert "VALIDATE: NOT_EVALUATED" in out
    assert "not evaluated (frequency domain)" in out
    assert "LineA" in out and "LineB" in out
    assert [line.calls for line in lines] == [0, 0]
    assert "VALIDATE: PASS" not in out
    assert "VALIDATE: FAIL" not in out
    assert "missing strength variables" not in out


@pytest.mark.parametrize(
    "state, complete",
    [
        (ModelState.SimulationPaused, True),
        (ModelState.SimulationStoppedUnstable, True),
        (ModelState.RunningSimulation, True),
        (ModelState.Reset, True),
        (ModelState.CalculatingStatics, True),
        (ModelState.InStaticState, True),
        (ModelState.SimulationStopped, False),
        (ModelState.SimulationStopped, _ABSENT),
    ],
)
def test_frequency_domain_does_not_bypass_completion(monkeypatch, capsys, state, complete):
    """The existing completion contract takes precedence over domain deferral."""
    line = _FrequencyDomainLine("LineA")
    api = _fake_api(state, complete=complete, lines=[line], frequency_domain=True)
    rc, out = _run(monkeypatch, capsys, api)
    assert rc == 1
    assert "VALIDATE: FAIL" in out
    assert "NOT_EVALUATED" not in out
    assert line.calls == 0


def test_frequency_domain_without_lines_fails(monkeypatch, capsys):
    """The existing requirement for at least one Line remains in force."""
    api = _fake_api(ModelState.SimulationStopped, lines=[], frequency_domain=True)
    rc, out = _run(monkeypatch, capsys, api)
    assert rc == 1
    assert "no Line object" in out
    assert "NOT_EVALUATED" not in out


@pytest.mark.parametrize("domain", [_ABSENT, None, "False"])
def test_unavailable_domain_is_not_assumed_time_domain(monkeypatch, capsys, domain):
    """Only the documented boolean False establishes time-domain applicability.

    Orcina Model.isFrequencyDomainDynamics reference, accessed 2026-10-19:
    https://www.orcina.com/webhelp/OrcFxAPI/Content/html/Pythonreference,Model.htm
    No numerical acceptance criterion is applied to missing or malformed metadata.
    """
    line = _CountingLine("LineA")
    api = _fake_api(ModelState.SimulationStopped, lines=[line], frequency_domain=domain)
    rc, out = _run(monkeypatch, capsys, api)
    assert rc == 3
    assert "VALIDATE: NOT_EVALUATED" in out
    assert "dynamics domain unavailable" in out
    assert "VALIDATE: PASS" not in out
    assert "VALIDATE: FAIL" not in out
    assert line.calls == 0


def test_time_domain_extrema_are_unchanged(monkeypatch, capsys):
    """Independent hand calculation using the existing synthetic time-domain data.

    Min contains [-1, -2], whose least value is -2; Max contains [3, 4], whose
    greatest value is 4. These exact integers need no rounding or tolerance.
    This is an extrema-retrieval check, not a physical strength acceptance limit.
    """
    rc, out = _run(monkeypatch, capsys, _fake_api(ModelState.SimulationStopped))
    assert rc == 0
    assert "(-2.0, 4.0)" in out
