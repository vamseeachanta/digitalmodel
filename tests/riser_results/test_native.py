# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic native-shaped objects; no solver module is imported."""

from types import SimpleNamespace

import pytest

from digitalmodel.riser_results.native import NativeAdapter, final_touchdown


def test_final_grounded_tail_ignores_earlier_contact():
    assert final_touchdown([0, 10, 20, 30, 40], [5, 0, 5, 0, 0], 0) == (30, [10])
    with pytest.raises(ValueError, match="grounded tail"):
        final_touchdown([0, 10], [0, 2], 0)


def fake_binding():
    calls = []

    class Line:
        LineType = ["pipe"]

        def RangeGraph(self, variable, period):
            calls.append(("RangeGraph", variable, period))
            return SimpleNamespace(X=[0, 10, 20, 30], Min=[1, 2, 3, 4], Max=[2, 3, 4, 5])

        def StaticResult(self, variable, extra):
            calls.append(("StaticResult", variable, extra))
            return 0 if variable == "clearance" and extra >= 20 else 1

        def TimeHistory(self, variable, period, objectExtra):
            calls.append(("TimeHistory", variable, objectExtra))
            if variable == "clearance":
                return [0, 0, 0] if objectExtra >= 20 else [1, 1, 1]
            return [1, 2, 1]

    line = Line()
    model = SimpleNamespace(
        general=SimpleNamespace(UnitsSystem="SI"),
        state=SimpleNamespace(name="SimulationStopped"),
        simulationTimeStatus=SimpleNamespace(CurrentTime=3750),
        SampleTimes=lambda period: [150, 1950, 3750],
    )
    objects = {"riser": line, "pipe": SimpleNamespace(StressOD=0.2731, StressID=0.2343)}

    class Model:
        def __getattr__(self, name):
            return getattr(model, name)

        def __getitem__(self, name):
            return objects[name]

    binding = SimpleNamespace(
        Model=lambda filename: Model(), DLLVersion=lambda: "synthetic",
        SpecifiedPeriod=lambda start, end: (start, end), pnStaticState="static",
        oeArcLength=lambda arc: arc,
        oeLine=lambda **kwargs: (kwargs["ArcLength"], kwargs["Theta"]),
    )
    options = {
        "line": "riser", "analysis_type": "dynamic", "period_s": [150, 3750],
        "fibres_deg": [0, 180], "contact_tolerance_m": 0,
        "variables": {name: name for name in (
            "effective_tension", "bending_moment", "von_mises_stress", "top_angle",
            "longitudinal_stress", "clearance")},
    }
    return binding, options, objects, calls


def test_adapter_reads_graphs_fixed_fibres_and_static_values():
    binding, options, objects, calls = fake_binding()
    snapshot = NativeAdapter(binding).read("synthetic.sim", {}, options)
    assert snapshot["touchdown_arc_length"] == [20, 20, 20]
    assert snapshot["units"]["stress"] == "kPa"
    assert snapshot["stress_geometry"]["checked_wall_m"] == pytest.approx(0.0194)
    assert {(h["name"], h["arc_length"], h["fibre_deg"]) for h in snapshot["stress_histories"]} == {
        ("touchdown", 20, 0), ("touchdown", 20, 180),
        ("hang_off", 0, 0), ("hang_off", 0, 180),
    }
    assert ("TimeHistory", "longitudinal_stress", (20, 180)) in calls
    assert ("StaticResult", "top_angle", 0) in calls


def test_native_unit_readback_rejects_unsupported_system():
    binding, options, _, _ = fake_binding()
    original = binding.Model
    model = original("synthetic.sim")
    model.general.UnitsSystem = "US"
    binding.Model = lambda filename: model
    with pytest.raises(ValueError, match="SI"):
        NativeAdapter(binding).read("synthetic.sim", {}, options)


def test_native_grid_mismatch_fails_without_interpolation():
    binding, options, objects, _ = fake_binding()
    original = objects["riser"].RangeGraph

    def graph(variable, period):
        result = original(variable, period)
        if variable == "bending_moment":
            result.X[1] = 11
        return result

    objects["riser"].RangeGraph = graph
    with pytest.raises(ValueError, match="grid"):
        NativeAdapter(binding).read("synthetic.sim", {}, options)
