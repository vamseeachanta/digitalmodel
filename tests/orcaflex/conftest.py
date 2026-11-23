"""Shared OrcaFlex test fixtures — mock OrcFxAPI objects.

ABOUTME: Provides fake OrcFxAPI types and a fake Model so tests run
without an OrcaFlex license.  Re-used across all orcaflex test files.
"""
from __future__ import annotations

import sys
import types

import pytest


# ---------------------------------------------------------------------------
# Fake OrcFxAPI types
# ---------------------------------------------------------------------------

class _RangeGraph:
    """Fake range graph result with arclength data."""
    def __init__(self):
        self.X = [0.0, 25.0, 50.0, 75.0, 100.0]
        self.Min = [10.0, 12.0, 11.0, 13.0, 10.5]
        self.Max = [20.0, 22.0, 21.0, 23.0, 20.5]
        self.Mean = [15.0, 17.0, 16.0, 18.0, 15.5]


class FakeLine:
    """Fake OrcFxAPI.Line object."""
    def __init__(self, name: str = "Line1"):
        self.name = name

    def StaticResult(self, variable: str, *args):
        """Return fake static result values."""
        return 100.0

    def TimeHistory(self, *args):
        """Return fake time history list."""
        return [100.0, 110.0, 90.0, 105.0, 95.0]

    def RangeGraph(self, *args):
        """Return fake range graph result."""
        return _RangeGraph()


class FakeVessel:
    """Fake OrcFxAPI.Vessel object."""
    def __init__(self, name: str = "Vessel1"):
        self.name = name

    def StaticResult(self, var: str, *args):
        results = {"X": 0.0, "Y": 0.0, "Z": -8.0}
        return results.get(var, 0.0)

    def TimeHistory(self, dof: str, *args):
        return [0.0, 0.1, -0.1, 0.05, -0.05]


class FakeBuoy:
    """Fake OrcFxAPI.Buoy base class."""
    pass


class FakeBuoy3D(FakeBuoy):
    """Fake 3D buoy — name contains '3D' for isinstance checks."""
    pass


class FakeBuoy6D(FakeBuoy):
    """Fake 6D buoy — name contains '6D' for isinstance checks."""
    pass


class FakeGeneral:
    """Fake model.general object."""
    OrcaFlexVersion = "11.6"
    UnitsSystem = "SI"
    NumberOfStages = 2


class FakeEnvironment:
    """Fake model.environment object."""
    WaterDepth = 100.0
    Density = 1.025
    WaveType = "JONSWAP"
    HsAtOrigin = 4.0
    WaveDirection = 180.0


class FakeModel:
    """Fake OrcFxAPI.Model with all attributes sections expect."""
    def __init__(self):
        self.simulationFileName = "test_model.sim"
        self.general = FakeGeneral()
        self.environment = FakeEnvironment()
        self.objects = [
            FakeLine("Line1"),
            FakeLine("Line2"),
            FakeVessel("FPSO1"),
            FakeBuoy3D(),
            FakeBuoy6D(),
        ]
        self.codeChecks = [
            types.SimpleNamespace(Name="API-2A-WSD", Utilisation=0.72),
            types.SimpleNamespace(Name="ISO-19902", Utilisation=1.05),
        ]
        self.ModalAnalysisResults = [
            types.SimpleNamespace(Description="1st lateral mode", NaturalFrequency=0.15),
            types.SimpleNamespace(Description="1st vertical mode", NaturalFrequency=0.22),
            types.SimpleNamespace(Description="2nd lateral mode", NaturalFrequency=0.38),
        ]

    def LoadSimulation(self, path: str):
        return None

    def SampleTimes(self):
        return [0.0, 1.0, 2.0, 3.0, 4.0]


def _build_fake_orcfxapi():
    """Build a fake OrcFxAPI module namespace."""
    return types.SimpleNamespace(
        Line=FakeLine,
        Vessel=FakeVessel,
        Buoy=FakeBuoy,
        Model=FakeModel,
        oeEndA=1,
        oeEndB=2,
    )


@pytest.fixture
def fake_orcfxapi(monkeypatch):
    """Install fake OrcFxAPI module and return it."""
    fake = _build_fake_orcfxapi()
    monkeypatch.setitem(sys.modules, "OrcFxAPI", fake)
    return fake


@pytest.fixture
def fake_model():
    """Return a FakeModel instance."""
    return FakeModel()
