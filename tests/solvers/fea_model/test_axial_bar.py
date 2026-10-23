# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Axial chain checks in N, mm and MPa through the public model entry."""

import subprocess
import sys

import pytest

from digitalmodel.solvers.fea_model.fea_model import fea_model


def element(**overrides):
    """Synthetic prismatic bar: E in MPa, A in mm² and L in mm."""
    return dict(youngs_modulus=200000.0, area=100.0, length=1000.0, **overrides)


def test_one_element_stiffness_and_displacement():
    """MIT 2.080J (2013), Lecture 4 Eq. 4.25 and Lecture 2 Eq. 2.5.

    Synthetic N/mm case: N=EA*epsilon, epsilon=delta/L, so k=EA/L.
    By hand: k=200000*100/1000=20000 N/mm. With P=10000 N,
    delta=P*L/(EA)=10000*1000/(200000*100)=0.5 mm.
    Equilibrium and end displacement difference give K=k*[[1,-1],[-1,1]].
    """
    cfg = {"axial_bar": {"elements": [element()], "tip_load": 10000.0}}
    assert fea_model(cfg) is cfg
    model = cfg["axial_bar"]["result"]
    assert model["element_stiffnesses"] == [20000.0]
    assert model["stiffness_matrix"] == [[20000.0, -20000.0], [-20000.0, 20000.0]]
    assert model["equivalent_stiffness"] == 20000.0
    assert model["displacements"] == [0.0, 0.5]
    assert model["reaction"] == -10000.0


def test_two_elements_in_series():
    """Same MIT equations, synthetic N/mm inputs; force common to both bars.

    k1=200000*100/1000=20000 N/mm; k2=200000*100/2000=10000 N/mm.
    k_eq=k1*k2/(k1+k2)=200000000/30000=6666.666666666667 N/mm.
    P=10000 N -> u=[0,0.5,1.5] mm; K assembled by shared-node addition.
    """
    second = element()
    second["length"] = 2000.0
    model = fea_model(
        {"axial_bar": {"elements": [element(), second], "tip_load": 10000}}
    )["axial_bar"]["result"]
    assert model["element_stiffnesses"] == [20000.0, 10000.0]
    assert model["equivalent_stiffness"] == pytest.approx(6666.666666666667)
    assert model["displacements"] == pytest.approx([0, 0.5, 1.5])
    assert model["stiffness_matrix"] == [
        [20000, -20000, 0],
        [-20000, 30000, -10000],
        [0, -10000, 10000],
    ]
    # Independent nodal equilibrium K*u=[-P,0,P], all forces in N.
    forces = [
        sum(k * u for k, u in zip(row, model["displacements"]))
        for row in model["stiffness_matrix"]
    ]
    assert forces == pytest.approx([-10000, 0, 10000])


@pytest.mark.parametrize("load,expected", [(0, 0), (-10000, -0.5)])
def test_zero_and_compressive_load(load, expected):
    """Same hand k=20000 N/mm gives delta=P/k in mm, signed load in N."""
    model = fea_model({"axial_bar": {"elements": [element()], "tip_load": load}})[
        "axial_bar"
    ]["result"]
    assert model["displacements"] == [0, expected]


@pytest.mark.parametrize("field", ["youngs_modulus", "area", "length"])
@pytest.mark.parametrize("invalid", [0, -1, float("nan"), float("inf"), -float("inf")])
def test_invalid_element_inputs(field, invalid):
    """Positive finite E (MPa), A (mm²), L (mm) required for EA/L."""
    bar = element()
    bar[field] = invalid
    with pytest.raises(ValueError, match=field):
        fea_model({"axial_bar": {"elements": [bar], "tip_load": 10000}})


@pytest.mark.parametrize("load", [float("nan"), float("inf"), -float("inf")])
def test_nonfinite_load(load):
    """The applied end force must be finite, in N."""
    with pytest.raises(ValueError, match="tip_load"):
        fea_model({"axial_bar": {"elements": [element()], "tip_load": load}})


def test_empty_chain():
    """No stiffness or deflection (N/mm, mm) can be assigned to an empty chain."""
    with pytest.raises(ValueError, match="elements"):
        fea_model({"axial_bar": {"elements": [], "tip_load": 10000}})


def test_entry_import_avoids_legacy_dependencies():
    """Fresh public import and N/mm fixture need only standard Python modules."""
    code = """
import sys
from digitalmodel.solvers.fea_model.fea_model import fea_model
fea_model({"axial_bar": {"elements": [{"youngs_modulus": 200000, "area": 100, "length": 1000}], "tip_load": 10000}})
assert "digitalmodel.infrastructure.base_solvers.structural.fea.FEAComponents" not in sys.modules
assert "OrcFxAPI" not in sys.modules
assert "ansys.mapdl.core" not in sys.modules
"""
    subprocess.run([sys.executable, "-c", code], check=True)


@pytest.mark.parametrize(
    "bar,load",
    [
        ({"youngs_modulus": 1e308, "area": 100, "length": 1}, 1),
        ({"youngs_modulus": 1e-308, "area": 1e-308, "length": 1}, 1),
        ({"youngs_modulus": 1e-308, "area": 1, "length": 1}, 1e308),
    ],
)
def test_unrepresentable_bar_result(bar, load):
    """Synthetic N/mm stress cases refuse overflow and stiffness underflow."""
    with pytest.raises(ValueError, match="floating-point range"):
        fea_model({"axial_bar": {"elements": [bar], "tip_load": load}})


def test_legacy_writer_call_sequence(monkeypatch):
    """A stub preserves the writer call sequence; its input units are delegated."""
    import types

    calls = []

    class Writer:
        def __init__(self, cfg):
            self.cfg = cfg

        def get_raw_data(self):
            calls.append("get_raw_data")

        def prepare_FEAModel(self):
            calls.append("prepare_FEAModel")

        def save_output(self):
            calls.append("save_output")

        def load_variations(self):
            calls.append("load_variations")

    key = "digitalmodel.infrastructure.base_solvers.structural.fea.FEAComponents"
    monkeypatch.setitem(sys.modules, key, types.SimpleNamespace(FEAComponents=Writer))
    cfg = {}
    assert fea_model(cfg) is cfg
    assert calls == [
        "get_raw_data",
        "prepare_FEAModel",
        "save_output",
        "load_variations",
    ]
