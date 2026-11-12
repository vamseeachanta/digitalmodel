"""Licence-free checks of a generated floating wind (fowt) model.

The checks read the generator's ``fowt`` spec (OrcaFlex units: te, kN, m),
convert to SI and compute, without a licensed solver:

* static equilibrium in still water (heave, pitch and surge) with elastic
  catenary mooring lines on a frictionless flat seabed;
* fairlead tensions at equilibrium;
* the 6x6 mooring stiffness at equilibrium by central differences;
* rigid-body natural periods: coupled surge-heave-pitch and uncoupled yaw,
  with added mass iterated at each mode's own frequency from the WAMIT
  radiation table;
* the mean offset under a steady rotor thrust.

The elastic catenary with seabed contact follows the closed-form equations
used in quasi-static mooring codes (e.g. Jonkman 2007, NREL/TP-500-41958,
section 4.2; zero seabed friction). Hydrostatics are linear about the
undisplaced position, using the generator's full OrcaFlex matrix, which holds
both buoyancy and weight moment-arm terms.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

import numpy as np
from scipy.linalg import eig
from scipy.optimize import fsolve

KILO = 1000.0


# --------------------------------------------------------------- catenary
def catenary_endpoint(h: float, v: float, length: float, w: float, ea: float) -> tuple[float, float]:
    """Horizontal and vertical span of a line with fairlead forces ``h``, ``v``.

    ``w`` is the submerged weight per unit length (N/m). If ``v < w L`` part
    of the line rests on the seabed (zero friction).
    """
    if v < w * length:
        grounded = length - v / w
        x = grounded + (h / w) * math.asinh(v / h) + h * length / ea
        z = (h / w) * (math.sqrt(1.0 + (v / h) ** 2) - 1.0) + v ** 2 / (2.0 * ea * w)
    else:
        va = v - w * length
        x = (h / w) * (math.asinh(v / h) - math.asinh(va / h)) + h * length / ea
        z = ((h / w) * (math.sqrt(1.0 + (v / h) ** 2) - math.sqrt(1.0 + (va / h) ** 2))
             + (v * length - 0.5 * w * length ** 2) / ea)
    return x, z


def solve_catenary(span: float, rise: float, length: float, w: float, ea: float,
                   guess: tuple[float, float] | None = None) -> tuple[float, float]:
    """Return fairlead (H, V) in N for a horizontal span and vertical rise (m)."""
    if guess is None:
        guess = (max(w * span * 0.5, 1.0e3), w * min(length, rise * 2.0))

    def residual(p):
        h, v = abs(p[0]) + 1e-6, abs(p[1])
        x, z = catenary_endpoint(h, v, length, w, ea)
        return [x - span, z - rise]

    sol, info, ier, msg = fsolve(residual, guess, full_output=True, xtol=1e-12)
    if ier != 1:
        raise RuntimeError(f"catenary did not converge: {msg}")
    return abs(sol[0]), abs(sol[1])


# --------------------------------------------------------------- the model
@dataclass
class Model:
    mass: float                 # kg
    cog: np.ndarray             # m, hull axes
    inertia_cg: np.ndarray      # kg m2
    volume: float               # m3
    rho: float                  # kg/m3
    g: float
    k_hydro: np.ndarray         # 3x3 heave/roll/pitch, SI, full OrcaFlex convention
    fairleads: np.ndarray       # (n, 3) hull axes
    anchors: np.ndarray         # (n, 3) global
    length: float
    w: float                    # submerged weight per length N/m
    ea: float                   # N
    mbl: float | None           # N; None where not published


def model_from_spec(spec: dict) -> Model:
    """Build the check model from a ProjectInputSpec dict of model type fowt."""
    from digitalmodel.solvers.orcaflex.modular_generator.routers.fowt_router import (
        full_hydrostatic_stiffness,
    )
    from digitalmodel.solvers.orcaflex.modular_generator.schema.fowt import FowtSpec

    fowt = FowtSpec(**spec["fowt"])
    hull = fowt.hull
    rho = spec["environment"]["water"]["density"] * KILO
    depth = spec["environment"]["water"]["depth"]
    k = np.array(full_hydrostatic_stiffness(fowt)) * KILO
    if len(fowt.mooring_line_types) != 1:
        raise NotImplementedError("one mooring line type is supported by the checks")
    lt = fowt.mooring_line_types[0]
    w = (lt.mass_per_length * KILO - rho * math.pi * lt.outer_diameter ** 2 / 4.0) * fowt.gravity
    lengths = {line.length for line in fowt.mooring_lines}
    if len(lengths) != 1:
        raise NotImplementedError("equal line lengths are assumed by the checks")
    return Model(
        mass=hull.mass * KILO, cog=np.array(hull.centre_of_mass),
        inertia_cg=np.array(hull.inertia_about_centre_of_mass) * KILO,
        volume=hull.displaced_volume, rho=rho, g=fowt.gravity, k_hydro=k,
        fairleads=np.array([line.fairlead for line in fowt.mooring_lines]),
        anchors=np.array([[*line.anchor_xy, -depth + line.anchor_height_above_seabed]
                          for line in fowt.mooring_lines]),
        length=lengths.pop(), w=w, ea=lt.axial_stiffness * KILO,
        mbl=None if lt.minimum_breaking_load is None else lt.minimum_breaking_load * KILO)


def rotation(rx: float, ry: float, rz: float) -> np.ndarray:
    cx, sx, cy, sy, cz, sz = (math.cos(rx), math.sin(rx), math.cos(ry), math.sin(ry),
                              math.cos(rz), math.sin(rz))
    rot_x = np.array([[1, 0, 0], [0, cx, -sx], [0, sx, cx]])
    rot_y = np.array([[cy, 0, sy], [0, 1, 0], [-sy, 0, cy]])
    rot_z = np.array([[cz, -sz, 0], [sz, cz, 0], [0, 0, 1]])
    return rot_z @ rot_y @ rot_x


def mooring_loads(model: Model, q: np.ndarray) -> tuple[np.ndarray, list[dict]]:
    """Mooring force and moment about the hull origin (global axes), per-line results."""
    r = rotation(q[3], q[4], q[5])
    total = np.zeros(6)
    lines = []
    for fair_local, anchor in zip(model.fairleads, model.anchors):
        fair = q[:3] + r @ fair_local
        horizontal = anchor[:2] - fair[:2]
        span = float(np.linalg.norm(horizontal))
        rise = float(fair[2] - anchor[2])
        h, v = solve_catenary(span, rise, model.length, model.w, model.ea)
        force = np.array([*(h * horizontal / span), -v])
        arm = fair - q[:3]
        total[:3] += force
        total[3:] += np.cross(arm, force)
        tension = math.hypot(h, v)
        lines.append({"horizontal": h, "vertical": v, "tension": tension,
                      "span": span, "grounded_length": max(model.length - v / model.w, 0.0),
                      "utilisation": None if model.mbl is None else tension / model.mbl})
    return total, lines


def hydro_and_weight(model: Model, q: np.ndarray) -> np.ndarray:
    """Linear hydrostatic plus weight load about the origin (global axes)."""
    load = np.zeros(6)
    load[2] = model.rho * model.g * model.volume - model.mass * model.g
    load[4] = model.mass * model.g * model.cog[0]  # weight lever arm at zero pitch
    load[3] = -model.mass * model.g * model.cog[1]
    restoring = model.k_hydro @ np.array([q[2], q[3], q[4]])
    load[2] -= restoring[0]
    load[3] -= restoring[1]
    load[4] -= restoring[2]
    return load


def external(thrust: float, hub: np.ndarray, tilt_deg: float, q: np.ndarray) -> np.ndarray:
    tilt = math.radians(tilt_deg)
    r = rotation(q[3], q[4], q[5])
    force = r @ np.array([thrust * math.cos(tilt), 0.0, -thrust * math.sin(tilt)])
    arm = r @ hub
    return np.concatenate([force, np.cross(arm, force)])


def equilibrium(model: Model, thrust: float = 0.0, hub: np.ndarray | None = None,
                tilt_deg: float = 0.0, free: tuple[int, ...] = (0, 2, 4),
                steps: int = 20) -> dict:
    """Solve static equilibrium in the listed degrees of freedom (others held at zero).

    The thrust is applied in ``steps`` equal increments, each solve starting
    from the previous solution.
    """
    hub = np.zeros(3) if hub is None else hub
    x = np.zeros(len(free))
    for fraction in np.linspace(0.0, 1.0, steps + 1)[1:] if thrust else [0.0]:
        result = _equilibrium_step(model, thrust * fraction, hub, tilt_deg, free, x)
        x = result["position"][list(free)]
    return result


def _equilibrium_step(model: Model, thrust: float, hub: np.ndarray, tilt_deg: float,
                      free: tuple[int, ...], start: np.ndarray) -> dict:

    def residual(x):
        q = np.zeros(6)
        q[list(free)] = x
        total = mooring_loads(model, q)[0] + hydro_and_weight(model, q)
        if thrust:
            total = total + external(thrust, hub, tilt_deg, q)
        return total[list(free)]

    sol, info, ier, msg = fsolve(residual, start, full_output=True, xtol=1e-10)
    if ier != 1 and np.max(np.abs(residual(sol))) > 1.0:
        raise RuntimeError(f"equilibrium did not converge: {msg}")
    q = np.zeros(6)
    q[list(free)] = sol
    loads, lines = mooring_loads(model, q)
    return {"position": q, "lines": lines, "mooring_load": loads,
            "residual": residual(sol)}


def mooring_stiffness(model: Model, q0: np.ndarray, step: float = 0.01,
                      angle: float = 1e-4) -> np.ndarray:
    k = np.zeros((6, 6))
    for j in range(6):
        d = step if j < 3 else angle
        plus, minus = q0.copy(), q0.copy()
        plus[j] += d
        minus[j] -= d
        k[:, j] = -(mooring_loads(model, plus)[0] - mooring_loads(model, minus)[0]) / (2 * d)
    return k


# --------------------------------------------------------- natural periods
def added_mass_function(data: dict, rho: float):
    """Return A(omega) -> 6x6 (SI) from generator_data.yml, linear in omega.

    Rows hold 11, 33, 55, 15 and 66; 22, 44 and 24 follow three-fold symmetry.
    The zero-frequency matrix closes the table below its lowest frequency.
    """
    from digitalmodel.floating_wind.iea15_orcaflex_spec import _expand

    hydro = data["hydrodynamics"]
    rows = hydro["radiation_by_period_nondimensional"]["value"]
    zero = np.array(hydro["added_mass_zero_frequency_nondimensional"]["value"])
    omegas = [0.0] + [2 * math.pi / r[0] for r in rows]
    mats = [zero] + [np.array(_expand(r)[0]) for r in rows]
    order = np.argsort(omegas)
    omegas = np.array(omegas)[order]
    mats = np.array(mats)[order]

    def at(omega: float) -> np.ndarray:
        out = np.empty((6, 6))
        for i in range(6):
            for j in range(6):
                out[i, j] = np.interp(omega, omegas, mats[:, i, j])
        return rho * out

    return at


def rigid_mass_matrix(model: Model) -> np.ndarray:
    m, c = model.mass, model.cog
    skew = np.array([[0, -c[2], c[1]], [c[2], 0, -c[0]], [-c[1], c[0], 0]])
    mass = np.zeros((6, 6))
    mass[:3, :3] = m * np.eye(3)
    mass[:3, 3:] = -m * skew
    mass[3:, :3] = m * skew
    mass[3:, 3:] = model.inertia_cg + m * (np.dot(c, c) * np.eye(3) - np.outer(c, c))
    return mass


def natural_periods(model: Model, data: dict, q0: np.ndarray, iterations: int = 50,
                    infinite_frequency_added_mass: bool = False) -> dict:
    """Coupled surge-heave-pitch and uncoupled yaw periods (s).

    By default the added mass is taken at each mode's own frequency. With
    ``infinite_frequency_added_mass`` the infinite-frequency matrix is used
    for every mode, which reproduces time-domain models whose radiation
    memory does not recover the low-frequency added mass.
    """
    if infinite_frequency_added_mass:
        a_inf = model.rho * np.array(
            data["hydrodynamics"]["added_mass_infinite_frequency_nondimensional"]["value"])

        def added(omega: float) -> np.ndarray:
            return a_inf
    else:
        added = added_mass_function(data, model.rho)
    stiffness = mooring_stiffness(model, q0)
    stiffness[2:5, 2:5] += model.k_hydro
    mass = rigid_mass_matrix(model)
    dofs = [0, 2, 4]
    sub = np.ix_(dofs, dofs)
    periods = {}
    guesses = {"surge": 140.0, "heave": 20.0, "pitch": 28.0}
    for name, guess in guesses.items():
        period = guess
        for _ in range(iterations):
            omega = 2 * math.pi / period
            values, vectors = eig(stiffness[sub], (mass + added(omega))[sub])
            candidates = 2 * math.pi / np.sqrt(np.real(values))
            new = float(candidates[np.argmin(abs(candidates - period))])
            if abs(new - period) < 1e-9:
                break
            period = new
        periods[name] = period
    period = 90.0
    for _ in range(iterations):
        omega = 2 * math.pi / period
        new = 2 * math.pi * math.sqrt((mass[5, 5] + added(omega)[5, 5]) / stiffness[5, 5])
        if abs(new - period) < 1e-9:
            break
        period = new
    periods["yaw"] = period
    return {"periods": periods, "mooring_stiffness": stiffness - _hydro6(model)}


def _hydro6(model: Model) -> np.ndarray:
    out = np.zeros((6, 6))
    out[2:5, 2:5] = model.k_hydro
    return out


def run_checks(spec: dict, data: dict, thrust: float | None, hub: list[float] | None,
               tilt_deg: float = 0.0) -> dict:
    """All licence-free checks for one fowt spec; SI units in the result.

    With ``thrust`` None the thrust-loaded equilibria are not computed.
    """
    model = model_from_spec(spec)
    still = equilibrium(model)
    periods = natural_periods(model, data, still["position"])
    loaded = held = None
    if thrust is not None:
        loaded = equilibrium(model, thrust, np.array(hub), tilt_deg)
        held = equilibrium(model, thrust, np.array(hub), 0.0, free=(0,))
    return {
        "still_water": still,
        "natural": periods,
        "thrust": thrust,
        "loaded_free": loaded,
        "loaded_surge_only": held,
        "net_buoyancy": model.rho * model.g * model.volume - model.mass * model.g,
    }
