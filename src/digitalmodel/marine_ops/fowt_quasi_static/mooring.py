# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Elastic quasi-static catenaries and constrained horizontal equilibrium.

The grounded inextensible relation follows the catenary equations in Irvine
(1981), Cable Structures. MoorPy supplies the elastic/seabed solution. The mean
load is applied at hub height; rotations and draft are held for static offsets,
with reaction forces/moments reported rather than concealed.
"""
from __future__ import annotations

import math
from typing import TYPE_CHECKING

import numpy as np
from scipy.optimize import brentq

if TYPE_CHECKING:
    import moorpy as mp


def grounded_horizontal_tension(distance: float, rise: float, length: float,
                                 weight: float) -> float:
    """Solve L-X=s-a acosh(1+z/a), s=sqrt(z²+2az), H=wa."""
    def residual(a: float) -> float:
        suspended = math.sqrt(rise**2 + 2*a*rise)
        return suspended-a*math.acosh(1+rise/a)-(length-distance)
    root: float = brentq(residual, 1e-6, 1e7)
    return weight * root


def mooring_system(basis: dict, removed: int | None = None,
                    length_change: float = 0.0) -> mp.System:
    """Construct the attributed three-chain system, optionally without one line."""
    import moorpy as mp
    values = basis["mooring"]
    environment = basis["environment"]
    rho, gravity = environment["water_density"]["value"], environment["gravity"]["value"]
    system = mp.System(depth=environment["water_depth"]["value"], rho=rho, g=gravity)
    body = system.addBody(0, np.zeros(6), DOFs=[0, 1])
    diameter = values["volume_equivalent_diameter"]["value"]
    mass = values["mass_per_length"]["value"]
    system.lineTypes["chain"] = {
        "name": "chain", "d_vol": diameter,
        "d_nom": values["nominal_diameter"]["value"], "m": mass,
        "EA": values["axial_stiffness"]["value"],
        "w": (mass-rho*math.pi*diameter**2/4)*gravity,
    }
    for index, (anchor, fairlead) in enumerate(zip(values["anchors"]["value"],
                                                   values["fairleads"]["value"])):
        if index != removed:
            a = system.addPoint(1, np.array(anchor))
            f = system.addPoint(1, np.array(fairlead))
            body.attachPoint(f.number, np.array(fairlead))
            line = system.addLine(values["unstretched_length"]["value"]+length_change,
                                  "chain", pointA=a.number, pointB=f.number)
            line.reference_index = index
    system.initialize()
    return system


def forces(system: mp.System, position: np.ndarray) -> np.ndarray:
    """Return six mooring forces at a prescribed still-water-origin position."""
    body = system.bodyList[0]
    body.setPosition(np.asarray(position))
    for line in system.lineList:
        line.staticSolve(reset=True, tol=1e-10)
    return body.getForces(lines_only=True, all_DOFs=True)


def stiffness(system: mp.System, position: np.ndarray) -> np.ndarray:
    """Return centred finite-difference six-degree mooring restoring stiffness."""
    matrix = np.zeros((6, 6))
    for axis in range(6):
        step = 0.01 if axis < 3 else 1e-5
        delta = np.zeros(6)
        delta[axis] = step
        matrix[:, axis] = -(forces(system, position+delta)-forces(system, position-delta))/(2*step)
    forces(system, position)
    return matrix


def equilibrate(system: mp.System, basis: dict, heading: float) -> dict:
    """Solve surge/sway equilibrium at fixed draft and attitude under rated thrust."""
    angle = math.radians(heading)
    thrust = basis["turbine"]["rated_rotor_thrust"]["value"]
    horizontal = thrust*np.array([math.cos(angle), math.sin(angle), 0])
    external = np.r_[horizontal, np.cross([0, 0, basis["turbine"]["hub_height"]["value"]], horizontal)]
    body = system.bodyList[0]
    body.f6Ext = external
    system.solveEquilibrium(DOFtype="free", tol=1e-5, maxIter=500)
    position = body.r6.copy()
    residual = forces(system, position)+external
    if np.linalg.norm(residual[:2]) > 100:
        raise ValueError("Horizontal equilibrium residual exceeds 100 N")
    return {"offset": position, "residual": residual,
            "stiffness": stiffness(system, position)}


def line_results(system: mp.System, position: np.ndarray, basis: dict,
                 damaged: bool) -> list[dict]:
    """Evaluate tensions, grounded length, anchor angle and factored utilisation."""
    forces(system, position)
    key = "damaged_tension_factor" if damaged else "intact_tension_factor"
    factor = basis["analysis"][key]["value"]
    breaking = basis["mooring"]["minimum_breaking_load"]["value"]
    rows = []
    for line in system.lineList:
        tension = max(float(line.TA), float(line.TB))
        angle = math.degrees(math.atan2(max(0.0, float(line.fA[2])),
                                       math.hypot(line.fA[0], line.fA[1])))
        rows.append({"line": line.reference_index+1, "tension": tension,
                     "utilisation": tension*factor/breaking,
                     "grounded_length": float(line.LBot), "anchor_angle": angle})
    return rows
