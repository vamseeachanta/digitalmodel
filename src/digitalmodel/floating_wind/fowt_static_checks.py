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
    # (period s, 6x6 added mass SI) of every spec radiation entry, as validated;
    # period 0 is infinite frequency. Checked against the data by natural_periods.
    radiation_added_mass: tuple = ()


def combine_mass_properties(parts: list[tuple[float, np.ndarray, np.ndarray]]
                            ) -> tuple[float, np.ndarray, np.ndarray]:
    """Mass, centre and inertia about that centre of (mass, centre, inertia) parts."""
    mass = sum(m for m, _, _ in parts)
    centre = sum(m * np.asarray(c, dtype=float) for m, c, _ in parts) / mass
    inertia = np.zeros((3, 3))
    for m, c, i in parts:
        d = np.asarray(c, dtype=float) - centre
        inertia += np.asarray(i, dtype=float) + m * (d @ d * np.eye(3) - np.outer(d, d))
    return mass, centre, inertia


def flexible_mass_properties(fowt, principal_rna_inertia: bool = False
                             ) -> tuple[float, np.ndarray, np.ndarray]:
    """Rigid-equivalent mass (kg), centre (m) and inertia (kg m2) of hull, tower and RNA.

    Tower sections are thin-walled tubes on the hull z axis. With
    ``principal_rna_inertia`` the RNA products of inertia are dropped, as in
    the generated lumped buoy.
    """
    hull = fowt.hull
    parts = [(hull.mass * KILO, hull.centre_of_mass,
              np.array(hull.inertia_about_centre_of_mass) * KILO)]
    z = fowt.tower.base_elevation
    for section in fowt.tower.sections:
        m = section.mass_per_length * KILO * section.length
        radial = m * section.outer_diameter ** 2 / 8.0
        axial = m * section.length ** 2 / 12.0
        parts.append((m, [0.0, 0.0, z + section.length / 2.0],
                      np.diag([radial + axial, radial + axial, 2.0 * radial])))
        z += section.length
    rna = fowt.rna
    inertia = np.array(rna.inertia_about_centre_of_mass) * KILO
    if principal_rna_inertia:
        inertia = np.diag(np.diag(inertia))
    parts.append((rna.mass * KILO, np.array(rna.centre_of_mass_from_tower_top) + [0.0, 0.0, z],
                  inertia))
    return combine_mass_properties(parts)


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
    mass, cog = hull.mass * KILO, np.array(hull.centre_of_mass)
    inertia = np.array(hull.inertia_about_centre_of_mass) * KILO
    if fowt.representation == "flexible":
        # Rigid-equivalent body: tower and RNA lumped with the hull, and the
        # weight moment-arm term taken for the whole assembly.
        mass, cog, inertia = flexible_mass_properties(fowt)
        k = np.array(hull.hydrostatic_stiffness_excluding_gravity) * KILO
        k[1, 1] += mass * fowt.gravity * (-cog[2])
        k[2, 2] += mass * fowt.gravity * (-cog[2])
    return Model(
        mass=mass, cog=cog, inertia_cg=inertia,
        volume=hull.displaced_volume, rho=rho, g=fowt.gravity, k_hydro=k,
        fairleads=np.array([line.fairlead for line in fowt.mooring_lines]),
        anchors=np.array([[*line.anchor_xy, -depth + line.anchor_height_above_seabed]
                          for line in fowt.mooring_lines]),
        length=lengths.pop(), w=w, ea=lt.axial_stiffness * KILO,
        mbl=None if lt.minimum_breaking_load is None else lt.minimum_breaking_load * KILO,
        radiation_added_mass=tuple((entry.period, np.array(entry.added_mass) * KILO)
                                   for entry in hull.radiation))


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


def check_spec_radiation(model: Model, data: dict, rtol: float = 1e-9) -> None:
    """Require the spec's added mass to be the data's, entry by entry.

    The periods are computed from the ``generator_data.yml`` table, which also
    holds the zero-frequency matrix that closes it below its lowest frequency
    (the spec has no zero-frequency entry). The generator emits the spec's
    radiation list, so the two must hold the same periods and added-mass
    matrices; otherwise the periods would not describe the generated model.
    Comparison is to ``rtol`` of each matrix's largest entry (conversion
    round-off only). Damping is not used by the period calculation and is not
    compared.
    """
    from digitalmodel.floating_wind.iea15_orcaflex_spec import _expand

    hydro = data["hydrodynamics"]
    expected = {0.0: model.rho * np.array(
        hydro["added_mass_infinite_frequency_nondimensional"]["value"])}
    for row in hydro["radiation_by_period_nondimensional"]["value"]:
        expected[row[0]] = model.rho * np.array(_expand(row)[0])
    actual = list(model.radiation_added_mass)
    periods = sorted(expected)
    if len(actual) != len(periods) or any(
            not math.isclose(p, q, rel_tol=rtol, abs_tol=0.0)
            for (p, _), q in zip(actual, periods)):
        raise ValueError("spec radiation periods differ from the generator data's "
                         f"({len(actual)} spec entries, {len(periods)} in the data)")
    for (period, matrix), key in zip(actual, periods):
        reference = expected[key]
        scale = float(np.max(np.abs(reference)))
        if not np.allclose(matrix, reference, rtol=rtol, atol=rtol * scale):
            raise ValueError(f"spec added mass differs from the generator data at period "
                             f"{period:g} s (0 = infinite frequency)")


def natural_periods(model: Model, data: dict, q0: np.ndarray, iterations: int = 50,
                    infinite_frequency_added_mass: bool = False) -> dict:
    """Coupled surge-heave-pitch and uncoupled yaw periods (s).

    By default the added mass is taken at each mode's own frequency. With
    ``infinite_frequency_added_mass`` the infinite-frequency matrix is used
    for every mode, which reproduces time-domain models whose radiation
    memory does not recover the low-frequency added mass. In both cases the
    spec radiation is first checked against the data (``check_spec_radiation``).
    """
    check_spec_radiation(model, data)
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


def tower_top_deflection(fowt, hull_rotation: tuple[float, float, float] = (0.0, 0.0, 0.0),
                         steps_per_section: int = 50) -> dict:
    """First-order static deflection of the flexible tower in hull axes.

    The tower is a cantilever fixed to the hull at ``base_elevation``. Its own
    weight and the RNA weight, resolved in hull axes for the given hull
    rotation (rad), load it laterally; the RNA weight also acts at its offset
    from the tower top. Euler-Bernoulli bending, linear, P-delta excluded.
    Returns the tower-top displacement (m) and rotation (rad) in hull x and y.
    """
    g_hull = rotation(*hull_rotation).T @ np.array([0.0, 0.0, -fowt.gravity])
    base = fowt.tower.base_elevation
    z_nodes, ei_nodes, w_nodes = [base], [], []
    for section in fowt.tower.sections:
        dz = section.length / steps_per_section
        for _ in range(steps_per_section):
            z_nodes.append(z_nodes[-1] + dz)
            ei_nodes.append(section.bending_stiffness * KILO)
            w_nodes.append(section.mass_per_length * KILO * dz)
    z = np.array(z_nodes)
    mid = (z[:-1] + z[1:]) / 2.0
    top = z[-1]
    rna = fowt.rna
    rna_force = rna.mass * KILO * g_hull
    rna_point = np.array(rna.centre_of_mass_from_tower_top) + [0.0, 0.0, top]
    result = {}
    for axis, (lateral, sign) in {"x": (0, 1.0), "y": (1, -1.0)}.items():
        # Bending moment about the axis normal to the deflection plane, at each
        # element mid-point, from all loads above it (positive bends toward +lateral).
        moments = []
        for zm in mid:
            above = mid > zm
            m = np.sum(np.array(w_nodes)[above] * g_hull[lateral] * (mid[above] - zm))
            m += rna_force[lateral] * (rna_point[2] - zm)
            m -= rna_force[2] * (rna_point[lateral] - 0.0)
            moments.append(m)
        curvature = np.array(moments) / np.array(ei_nodes)
        dz = np.diff(z)
        slope = np.concatenate([[0.0], np.cumsum(curvature * dz)])
        deflection = np.concatenate([[0.0], np.cumsum((slope[:-1] + slope[1:]) / 2.0 * dz)])
        result[f"top_displacement_{axis}"] = float(deflection[-1])
        result[f"top_rotation_about_{'y' if axis == 'x' else 'x'}"] = float(sign * slope[-1])
    return result
