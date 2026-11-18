# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Native adapter; a binding is supplied only by the explicit runtime factory.

Calls follow existing solvers/orcaflex/orcaflex_analysis_components.py extraction
and oeLine usage. No solver is imported, started, calculated or queried at import.
SI native output units are m, kN, kN.m, kPa and degrees; other systems fail closed.
Result variable names are supplied by a version-checked extraction manifest.
"""

from __future__ import annotations

import importlib
import math
from pathlib import Path
from typing import Any

from .schema import Document, number, vector


def final_touchdown(arcs: list[float], clearance: list[float],
                    tolerance: float) -> tuple[float, list[float]]:
    """Find the start of the final contiguous grounded tail, retaining earlier contact."""
    arcs = vector(arcs, "contact arcs")
    clearance = vector(clearance, "clearance", len(arcs))
    number(tolerance, "contact tolerance", 0)
    if clearance[-1] > tolerance:
        raise ValueError("final grounded tail is absent")
    index = len(arcs) - 1
    while index > 0 and clearance[index - 1] <= tolerance:
        index -= 1
    if index == 0:
        raise ValueError("entire line is grounded; final touchdown is unresolved")
    earlier = [arc for arc, gap in zip(arcs[:index], clearance[:index]) if gap <= tolerance]
    return arcs[index], earlier


class NativeAdapter:
    """Read completed simulations using an injected binding; synthetic bindings work in tests."""

    def __init__(self, binding: Any) -> None:
        """Store the binding without opening any simulation."""
        self.api = binding

    def read(self, result_file: Path, case_row: Document, options: Document) -> Document:
        """Extract profiles and physical stress histories without rerunning results."""
        api = self.api
        model = api.Model(str(result_file))
        if model.general.UnitsSystem != "SI":
            raise ValueError("native adapter requires a read-back SI unit system")
        dynamic = options["analysis_type"] == "dynamic"
        state = model.state.name
        if state not in ({"SimulationStopped"} if dynamic else {"InStaticState", "SimulationStopped"}):
            raise ValueError("model is not in a completed result state")
        if dynamic:
            start, end = vector(options["period_s"], "period_s", 2)
            if end <= start or model.simulationTimeStatus.CurrentTime < end:
                raise ValueError("requested stationary interval is incomplete")
            period = api.SpecifiedPeriod(start, end)
            times = list(model.SampleTimes(period))
        else:
            period = api.pnStaticState
            times = []
        line = model[options["line"]]
        variables = options["variables"]
        profiles = {}
        arcs = None
        for name in ("effective_tension", "bending_moment", "von_mises_stress"):
            graph = line.RangeGraph(variables[name], period)
            grid = list(graph.X)
            if arcs is None:
                arcs = grid
            elif grid != arcs:
                raise ValueError("native range graph grid mismatch; interpolation is prohibited")
            static = [float(line.StaticResult(variables[name], api.oeArcLength(s))) for s in arcs]
            profiles[name] = {"static": static,
                              "min": list(graph.Min) if dynamic else None,
                              "max": list(graph.Max) if dynamic else None}
        if not arcs:
            raise ValueError("native arc grid is empty")

        def history(variable: str, arc: float) -> list[float]:
            return list(line.TimeHistory(variables[variable], period, objectExtra=api.oeArcLength(arc)))

        clearances = [history("clearance", arc) if dynamic else
                      [float(line.StaticResult(variables["clearance"], api.oeArcLength(arc)))]
                      for arc in arcs]
        size = len(times) if dynamic else 1
        for values in clearances:
            vector(values, "clearance history", size)
        touchdowns, earlier = [], []
        for sample in range(size):
            td, contacts = final_touchdown(arcs, [row[sample] for row in clearances],
                                          options["contact_tolerance_m"])
            touchdowns.append(td)
            earlier.append(contacts)
        histories = []
        if dynamic:
            zone = [s for s in arcs if min(touchdowns) <= s <= max(touchdowns)]
            stations = [("hang_off", arcs[0])] + [("touchdown", s) for s in zone]
            for name, arc in stations:
                for fibre in options["fibres_deg"]:
                    extra = api.oeLine(ArcLength=arc, RadialPos=1, Theta=fibre)
                    values = list(line.TimeHistory(variables["longitudinal_stress"], period,
                                                   objectExtra=extra))
                    histories.append({"name": name, "arc_length": arc, "fibre_deg": fibre, "stress": values})
        sections = []
        for name in set(line.LineType):
            section = model[name]
            od = number(section.StressOD, "StressOD", 0)
            inner = number(section.StressID, "StressID", 0)
            if not 0 < inner < od:
                raise ValueError("native stress section requires explicit valid StressOD and StressID")
            sections.append((od, (od - inner) / 2))
        if not sections or any(not all(math.isclose(a, b, rel_tol=1e-9, abs_tol=1e-12)
                                       for a, b in zip(section, sections[0])) for section in sections):
            raise ValueError("native stress sections differ along the riser")
        top = {"static": float(line.StaticResult(variables["top_angle"], api.oeArcLength(arcs[0]))),
               "history": history("top_angle", arcs[0]) if dynamic else []}
        hang = {"static": float(line.StaticResult(variables["effective_tension"], api.oeArcLength(arcs[0]))),
                "history": history("effective_tension", arcs[0]) if dynamic else []}
        return {"data_basis": "native; engineering qualification pending readback",
                "software": {"name": "native time-domain line solver", "version": str(api.DLLVersion())},
                "units": {"length": "m", "force": "kN", "moment": "kN.m", "stress": "kPa", "angle": "deg"},
                "arc_length": arcs, "profiles": profiles, "times_s": times,
                "touchdown_arc_length": touchdowns, "earlier_contacts": earlier,
                "top_angle": top, "hang_off_tension": hang, "stress_histories": histories,
                "stress_geometry": {"outside_diameter_m": sections[0][0], "checked_wall_m": sections[0][1]}}


def load_adapter() -> NativeAdapter:
    """Load the licensed binding only when explicitly exporting in its interactive process."""
    return NativeAdapter(importlib.import_module("OrcFxAPI"))
