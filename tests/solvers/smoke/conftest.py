"""Isolated OrcFxAPI double; never import or allocate the native API."""

from pathlib import Path
import sys
from types import SimpleNamespace
import weakref

import numpy as np
import pytest


class FakeLine:
    def __init__(self, model):
        self.model = model
        self.Length = [0.0]
        self.TargetSegmentLength = [0.0]

    def StaticResult(self, *args):
        return self.model.api.static

    def TimeHistory(self, *args):
        key = "reader_history" if self.model.role == "simulation_reader" else "history"
        return np.asarray(getattr(self.model.api, key), dtype=float)


class FakeModel:
    def __init__(self, api, filename=None, threadCount=None, **kwargs):
        self.api = api
        api.constructors.append({"filename": filename, "threadCount": threadCount})
        if api.fail_constructor == len(api.constructors):
            raise RuntimeError("fake licence/constructor failure")
        api.references.append(weakref.ref(self))
        self.role = "solve" if len(api.constructors) == 1 else "simulation_reader"
        self.threadCount = 1 if threadCount is None else threadCount
        self.state = "Reset"
        self.simulationComplete = False
        self.general = SimpleNamespace()
        self.environment = SimpleNamespace()
        if filename is not None:
            self.LoadData(filename)

    def CreateObject(self, *args):
        line = FakeLine(self)
        self.api.references.append(weakref.ref(line))
        return line

    def __getitem__(self, name):
        if self.api.missing_line:
            raise KeyError(name)
        assert name == "SmokeTestLine"
        return self.CreateObject()

    def CalculateStatics(self):
        self.api.fail_if("statics")
        self.state = "InStaticState"

    def RunSimulation(self):
        self.api.fail_if("dynamics")
        self.state = self.api.state
        self.simulationComplete = self.api.complete
        self.threadCount = self.api.observed_threads.get("solve", self.threadCount)

    def SaveData(self, path):
        self.api.fail_if("save_data")
        Path(path).write_text("fake data", encoding="utf-8")

    def LoadData(self, path):
        self.api.fail_if("load_data")
        self.role = "data_reader"
        self.threadCount = self.api.observed_threads.get(self.role, self.threadCount)
        self.api.loads.append(("data", Path(path).name))

    def SaveSimulation(self, path):
        self.api.fail_if("save_sim")
        if self.api.sim_file != "missing":
            content = "" if self.api.sim_file == "empty" else self.api.sim_file
            Path(path).write_text(content, encoding="utf-8")

    def LoadSimulation(self, path):
        self.api.loads.append(("simulation", Path(path).name))
        self.api.fail_if("load_sim")
        if Path(path).read_text() == "corrupt":
            raise RuntimeError("fake corrupt simulation")
        self.role = "simulation_reader"
        self.state = self.api.reader_state
        self.simulationComplete = self.api.reader_complete
        self.threadCount = self.api.observed_threads.get(self.role, self.threadCount)


class FakeAPI:
    otLine = 1
    oeEndA = 1
    ModelState = SimpleNamespace(
        InStaticState="InStaticState", SimulationStopped="SimulationStopped",
        RunningSimulation="RunningSimulation", SimulationPaused="SimulationPaused",
    )

    def __init__(self):
        self.static = 10.0
        self.history = [1.0, 2.0, 3.0]
        self.reader_history = [1.0, 2.0, 3.0]
        self.state = self.reader_state = "SimulationStopped"
        self.complete = self.reader_complete = True
        self.sim_file = "valid fake simulation"
        self.missing_line = False
        self.fail_operation = None
        self.fail_constructor = None
        self.observed_threads = {}
        self.constructors = []
        self.loads = []
        self.references = []

    def Model(self, filename=None, threadCount=None, **kwargs):
        return FakeModel(self, filename, threadCount, **kwargs)

    def fail_if(self, operation):
        if self.fail_operation == operation:
            raise RuntimeError("fake " + operation + " failure")

    @staticmethod
    def DLLVersion():
        return "FAKE-NO-LICENCE"

    @staticmethod
    def Period(value):
        return value


@pytest.fixture
def fake_api(monkeypatch):
    api = FakeAPI()
    monkeypatch.setitem(sys.modules, "OrcFxAPI", api)
    return api
