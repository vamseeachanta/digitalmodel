# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
# ABOUTME: Smith axial pile drivability with an independently evaluated energy ledger.
# ABOUTME: Single-blow motion and stress histories in SI units; no solver dependency.
"""One-dimensional hammer–pile–soil dynamics.

The physical basis is FHWA GEC 12, Volume II, Section 12.3. The numerical
scheme uses midpoint momentum and exact piecewise-linear displacement-path
force averages. See docs/domains/geotechnical/pile-drivability.md for equations,
sources, verification and limitations. Positive motion and stress are downward
and compressive respectively. Resistance to driving is supplied independently
of static pile capacity. Only a settled blow has a reported permanent set.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Sequence


def _positive(name: str, value: float, *, zero: bool = False) -> None:
    if not math.isfinite(value) or (value < 0 if zero else value <= 0):
        qualifier = "nonnegative" if zero else "positive"
        raise ValueError(f"{name} must be finite and {qualifier}")


@dataclass(frozen=True)
class LumpedPile:
    """N springs with N+1 nodal masses; SI units, ordered head to toe.

    General masses allow published lumped models to be entered directly.
    Areas and lengths describe spring segments, not mass points.
    """

    masses_kg: tuple[float, ...]
    stiffnesses_n_m: tuple[float, ...]
    areas_m2: tuple[float, ...]
    lengths_m: tuple[float, ...]

    def __post_init__(self) -> None:
        """Reject incomplete or nonphysical meshes and freeze input sequences."""
        count = len(self.stiffnesses_n_m)
        if (count < 1 or len(self.masses_kg) != count + 1
                or len(self.areas_m2) != count or len(self.lengths_m) != count):
            raise ValueError("N segments require N+1 masses and N areas and lengths")
        for name in ("masses_kg", "stiffnesses_n_m", "areas_m2", "lengths_m"):
            values = tuple(getattr(self, name))
            for value in values:
                _positive(name, value)
            object.__setattr__(self, name, values)

    @property
    def node_depths_m(self) -> tuple[float, ...]:
        """Return node positions relative to the head."""
        depths = [0.0]
        for length in self.lengths_m:
            depths.append(depths[-1] + length)
        return tuple(depths)

    @property
    def recommended_time_step_s(self) -> float:
        """Return a temporal resolution bound, not an implicit stability limit."""
        return 0.1 * min(
            math.sqrt(min(a, b) / k)
            for a, b, k in zip(self.masses_kg, self.masses_kg[1:], self.stiffnesses_n_m)
        )

    @property
    def settling_window_s(self) -> float:
        """Return a dwell interval at least as long as a uniform rod round trip."""
        return 2 * sum(
            math.sqrt((a + b) / k)
            for a, b, k in zip(self.masses_kg, self.masses_kg[1:], self.stiffnesses_n_m)
        )


def uniform_pile(
    length_m: float,
    area_m2: float,
    youngs_modulus_pa: float,
    density_kg_m3: float,
    segments: int,
) -> LumpedPile:
    """Discretise a uniform rod with half end masses and springs EA/dx."""
    for name, value in (("length", length_m), ("area", area_m2),
                        ("modulus", youngs_modulus_pa), ("density", density_kg_m3)):
        _positive(name, value)
    if isinstance(segments, bool) or not isinstance(segments, int) or segments < 1:
        raise ValueError("segments must be a positive integer")
    dx = length_m / segments
    mass = density_kg_m3 * area_m2 * dx
    return LumpedPile(
        (mass / 2,) + (mass,) * (segments - 1) + (mass / 2,),
        (youngs_modulus_pa * area_m2 / dx,) * segments,
        (area_m2,) * segments,
        (dx,) * segments,
    )


@dataclass(frozen=True)
class Hammer:
    """Ram mass (kg), delivered impact energy (J), elastic cushion (N/m)."""

    mass_kg: float
    energy_j: float
    cushion_stiffness_n_m: float

    def __post_init__(self) -> None:
        """Validate the impact and elastic contact inputs."""
        _positive("hammer mass", self.mass_kg)
        _positive("delivered energy", self.energy_j, zero=True)
        _positive("cushion stiffness", self.cushion_stiffness_n_m)


@dataclass(frozen=True)
class SoilResistance:
    """Resistance (N), explicit quake (m), damping (s/m); no default stiffness."""

    ultimate_resistance_n: float = 0.0
    quake_m: float | None = None
    damping_s_m: float = 0.0

    def __post_init__(self) -> None:
        """Require an explicit positive quake whenever resistance is positive."""
        _positive("ultimate resistance", self.ultimate_resistance_n, zero=True)
        if self.quake_m is None:
            if self.ultimate_resistance_n > 0:
                raise ValueError("positive resistance requires an explicit quake_m")
        else:
            _positive("quake", self.quake_m)
        _positive("Smith damping", self.damping_s_m, zero=True)

    @property
    def stiffness_n_m(self) -> float:
        """Return zero for absent soil, otherwise the explicitly specified Ru/q."""
        return self.ultimate_resistance_n / self.quake_m if self.quake_m is not None else 0.0


@dataclass(frozen=True)
class EnergyBalance:
    """Instantaneous stored energy and cumulative dissipation, all in joules."""

    kinetic_j: float
    pile_elastic_j: float
    cushion_elastic_j: float
    soil_elastic_j: float
    soil_plastic_work_j: float
    damping_work_j: float
    residual_j: float


@dataclass(frozen=True)
class BlowResult:
    """Full-resolution records; stress columns are segments, motion columns nodes.

    Every history's outer dimension is time. Stress is signed in Pa; both
    envelopes are positive magnitudes. The histories can feed fatigue counting
    with a separately determined number of representative hammer blows.
    Stress column i belongs to the interval between node depths i and i+1.
    None denotes an unavailable settled result, not zero penetration.
    """

    time_s: tuple[float, ...]
    node_depths_m: tuple[float, ...]
    displacement_m: tuple[tuple[float, ...], ...]
    velocity_m_s: tuple[tuple[float, ...], ...]
    stress_pa: tuple[tuple[float, ...], ...]
    hammer_displacement_m: tuple[float, ...]
    hammer_velocity_m_s: tuple[float, ...]
    cushion_force_n: tuple[float, ...]
    energy: tuple[EnergyBalance, ...]
    initial_energy_j: float
    max_compressive_stress_pa: tuple[float, ...]
    max_tensile_stress_pa: tuple[float, ...]
    shaft_plastic_displacement_m: tuple[float, ...]
    toe_plastic_displacement_m: float
    settled: bool
    permanent_set_m: float | None
    blows_per_m: float | None
    smith_set_estimate_m: float | None
    smith_blows_per_m_estimate: float | None


def _path_force(
    start: float, increment: float, stiffness: float,
    limit: float, unilateral: bool,
) -> tuple[float, float, float, float]:
    """Integrate clipped Hooke force and its magnitude over a linear path.

    Return mean force, its derivative with respect to increment, mean magnitude
    and its derivative. Split at every yield point and at zero; trapezoids are
    exact on each piece. Infinite limit denotes an elastic separating cushion.
    """
    if stiffness == 0:
        return 0.0, 0.0, 0.0, 0.0
    quake = limit / stiffness
    cuts = [0.0, 1.0]
    if increment != 0:
        for boundary in (0.0, quake) if unilateral else (-quake, 0.0, quake):
            fraction = (boundary - start) / increment
            if 0 < fraction < 1:
                cuts.append(fraction)
    cuts.sort()
    lower = 0.0 if unilateral else -limit
    mean = tangent = magnitude = magnitude_tangent = 0.0
    for a, b in zip(cuts, cuts[1:]):
        z_a, z_b = start + a * increment, start + b * increment
        f_a = min(limit, max(lower, stiffness * z_a))
        f_b = min(limit, max(lower, stiffness * z_b))
        mean += 0.5 * (f_a + f_b) * (b - a)
        magnitude += 0.5 * (abs(f_a) + abs(f_b)) * (b - a)
        midpoint = start + 0.5 * (a + b) * increment
        elastic_region = (0 < midpoint < quake) if unilateral else (-quake < midpoint < quake)
        if elastic_region:
            derivative = 0.5 * stiffness * (b * b - a * a)
            tangent += derivative
            if midpoint != 0:
                magnitude_tangent += math.copysign(derivative, midpoint)
    return mean, tangent, magnitude, magnitude_tangent


def _soil_step(
    soil: SoilResistance, start: float, increment: float,
    plastic: float, dt: float, unilateral: bool,
) -> tuple[float, float, float, float, float, float, float]:
    """Return static/dynamic forces, tangents, plastic state, energy and work."""
    resistance = soil.ultimate_resistance_n
    quake = soil.quake_m
    if resistance == 0 or quake is None:
        return 0.0, 0.0, 0.0, 0.0, plastic, 0.0, 0.0
    stiffness = soil.stiffness_n_m
    force, tangent, magnitude, magnitude_tangent = _path_force(
        start - plastic, increment, stiffness, resistance, unilateral,
    )
    damping = soil.damping_s_m * magnitude * increment / dt
    damping_tangent = soil.damping_s_m * (magnitude + magnitude_tangent * increment) / dt
    # A toe dashpot may dissipate rebound energy, but cannot pull on the pile.
    if unilateral and damping < -force:
        damping, damping_tangent = -force, -tangent
    end = start + increment
    if unilateral:
        new_plastic = max(plastic, end - quake)
        elastic = max(0.0, end - new_plastic)
    else:
        elastic = min(quake, max(-quake, end - plastic))
        new_plastic = end - elastic if abs(end - plastic) > quake else plastic
    return (force, tangent, damping, damping_tangent, new_plastic,
            0.5 * stiffness * elastic**2, resistance * abs(new_plastic - plastic))


def _tridiagonal(diagonal: list[float], off: list[float], rhs: list[float]) -> list[float]:
    """Solve a symmetric tridiagonal system without modifying its inputs."""
    d, x = diagonal.copy(), rhs.copy()
    for i in range(1, len(d)):
        ratio = off[i - 1] / d[i - 1]
        d[i] -= ratio * off[i - 1]
        x[i] -= ratio * x[i - 1]
    x[-1] /= d[-1]
    for i in range(len(d) - 2, -1, -1):
        x[i] = (x[i] - off[i] * x[i + 1]) / d[i]
    return x


def _advance(
    pile: LumpedPile, hammer: Hammer | None, soils: tuple[SoilResistance, ...],
    masses: tuple[float, ...], u: list[float], v: list[float],
    plastic: list[float], dt: float,
) -> tuple[list[float], list[float], list[float], float, float]:
    """Advance one energy-consistent step; fail explicitly on nonconvergence."""
    _positive("time increment", dt)
    offset = int(hammer is not None)
    increment = [dt * velocity for velocity in v]
    inertia = [2 * mass / dt**2 for mass in masses]
    scale = max(1e-12, max(abs(x) for x in u), max(abs(x) for x in increment))
    for _iteration in range(30):
        residual = [a * (d - dt * speed) for a, d, speed in zip(inertia, increment, v)]
        diagonal = inertia.copy()
        off = [0.0] * (len(u) - 1)
        # All spring forces are positive compression between upper/lower nodes.
        for i, stiffness in enumerate(pile.stiffnesses_n_m):
            left = offset + i
            force = stiffness * (u[left] - u[left + 1]
                                 + 0.5 * (increment[left] - increment[left + 1]))
            tangent = 0.5 * stiffness
            residual[left] += force
            residual[left + 1] -= force
            diagonal[left] += tangent
            diagonal[left + 1] += tangent
            off[left] -= tangent
        if hammer is not None:
            force, tangent, _magnitude, _magnitude_tangent = _path_force(
                u[0] - u[1], increment[0] - increment[1],
                hammer.cushion_stiffness_n_m, math.inf, True,
            )
            residual[0] += force
            residual[1] -= force
            diagonal[0] += tangent
            diagonal[1] += tangent
            off[0] -= tangent
        new_plastic = []
        plastic_work = damping_work = 0.0
        for i, soil in enumerate(soils):
            toe = i == len(soils) - 1
            left = len(u) - 1 if toe else offset + i
            start = u[left] if toe else 0.5 * (u[left] + u[left + 1])
            delta = increment[left] if toe else 0.5 * (increment[left] + increment[left + 1])
            f, k, damp, kd, p, _elastic_energy, work = _soil_step(
                soil, start, delta, plastic[i], dt, toe,
            )
            new_plastic.append(p)
            plastic_work += work
            damping_work += damp * delta
            if toe:
                residual[left] += f + damp
                diagonal[left] += k + kd
            else:
                for node in (left, left + 1):
                    residual[node] += 0.5 * (f + damp)
                    diagonal[node] += 0.25 * (k + kd)
                off[left] += 0.25 * (k + kd)
        correction = _tridiagonal(diagonal, off, [-r for r in residual])
        if max(abs(x) for x in correction) <= 8 * math.ulp(scale):
            # Use this evaluated increment and its associated plastic state/work.
            return ([x + d for x, d in zip(u, increment)],
                    [2 * d / dt - speed for d, speed in zip(increment, v)],
                    new_plastic, plastic_work, damping_work)
        increment = [d + change for d, change in zip(increment, correction)]
    raise RuntimeError("nonlinear time step did not converge; reduce time_step_s")


def _stored_energy(
    pile: LumpedPile, hammer: Hammer | None, soils: tuple[SoilResistance, ...],
    masses: tuple[float, ...], u: list[float], v: list[float], plastic: list[float],
) -> tuple[float, float, float, float]:
    """Evaluate energy directly from current motion and internal variables."""
    offset = int(hammer is not None)
    kinetic = 0.5 * math.fsum(m * speed**2 for m, speed in zip(masses, v))
    pile_elastic = 0.5 * math.fsum(
        k * (u[offset + i] - u[offset + i + 1])**2
        for i, k in enumerate(pile.stiffnesses_n_m)
    )
    cushion_elastic = (0.5 * hammer.cushion_stiffness_n_m * max(0.0, u[0] - u[1])**2
                       if hammer is not None else 0.0)
    soil_elastic = 0.0
    for i, soil in enumerate(soils):
        toe = i == len(soils) - 1
        displacement = u[-1] if toe else 0.5 * (u[offset + i] + u[offset + i + 1])
        elastic = displacement - plastic[i]
        if toe:
            elastic = max(0.0, elastic)
        soil_elastic += 0.5 * soil.stiffness_n_m * elastic**2
    return kinetic, pile_elastic, cushion_elastic, soil_elastic


def _settlement_equilibrium(
    pile: LumpedPile, stiffness: list[float], plastic: list[float],
) -> list[float] | None:
    """Find an elastic reference, or return None when the solve is unresolved."""
    count = len(pile.masses_kg)
    if not any(stiffness[:-1]):
        return [plastic[-1]] * count
    diagonal, off, rhs = [0.0] * count, [0.0] * (count - 1), [0.0] * count
    for i, spring in enumerate(pile.stiffnesses_n_m):
        soil = stiffness[i]
        for node in (i, i + 1):
            diagonal[node] += spring + 0.25 * soil
            rhs[node] += 0.5 * soil * plastic[i]
        off[i] = -spring + 0.25 * soil
    try:
        equilibrium = _tridiagonal(diagonal, off, rhs)
        if stiffness[-1] > 0 and equilibrium[-1] > plastic[-1]:
            diagonal[-1] += stiffness[-1]
            rhs[-1] += stiffness[-1] * plastic[-1]
            equilibrium = _tridiagonal(diagonal, off, rhs)
    except (ZeroDivisionError, OverflowError):
        return None  # No remaining-motion certificate from an unresolved solve.
    return equilibrium if all(math.isfinite(x) for x in equilibrium) else None


def _equilibrium_residual(
    pile: LumpedPile, stiffness: list[float], plastic: list[float],
    equilibrium: list[float],
) -> list[float]:
    """Evaluate physical reference forces without assembled-matrix cancellation."""
    residual = [0.0] * len(equilibrium)
    for i, spring in enumerate(pile.stiffnesses_n_m):
        force = spring * (equilibrium[i] - equilibrium[i + 1])
        soil_force = stiffness[i] * (0.5 * (equilibrium[i] + equilibrium[i + 1]) - plastic[i])
        residual[i] += force + 0.5 * soil_force
        residual[i + 1] += -force + 0.5 * soil_force
    residual[-1] += stiffness[-1] * max(0.0, equilibrium[-1] - plastic[-1])
    return residual


def _remaining_pile_energy(
    pile: LumpedPile, soils: tuple[SoilResistance, ...],
    u: list[float], v: list[float], plastic: list[float],
) -> float:
    """Bound future pile kinetic energy; infinity means settlement is uncertified.

    Use relative elastic energy about an admissible equilibrium, retaining a
    compliance bound on solver residuals. The derivation and applicability are
    recorded in the calculation document's numerical settlement section.
    """
    stiffness = [soil.stiffness_n_m for soil in soils]
    equilibrium = _settlement_equilibrium(pile, stiffness, plastic)
    if equilibrium is None:
        return math.inf
    residual = _equilibrium_residual(pile, stiffness, plastic, equilibrium)
    norm_squared = math.fsum(force**2 for force in residual)
    compliance = math.fsum(1 / spring for spring in pile.stiffnesses_n_m)
    bound = min((1 / soil + compliance - 0.75 / pile.stiffnesses_n_m[i]
                 for i, soil in enumerate(stiffness[:-1]) if soil > 0), default=0.0)
    error = bound * math.sqrt(len(u) * norm_squared)
    if not math.isfinite(error):
        return math.inf
    extensions = [0.5 * (a + b) - p for a, b, p in
                  zip(equilibrium, equilibrium[1:], plastic)]
    extensions.append(max(0.0, equilibrium[-1] - plastic[-1] + error))
    if any(soil.ultimate_resistance_n > 0 and soil.quake_m is not None
           and abs(extension) + (error if i < len(u) - 1 else 0)
           > soil.quake_m for i, (soil, extension) in enumerate(zip(soils, extensions))):
        return math.inf
    delta = [a - b for a, b in zip(u, equilibrium)]
    energy = 0.5 * math.fsum(mass * speed**2 for mass, speed in zip(pile.masses_kg, v))
    energy += 0.5 * math.fsum(spring * (a - b)**2 for spring, a, b in
                              zip(pile.stiffnesses_n_m, delta, delta[1:]))
    energy += 0.5 * math.fsum(soil * (0.5 * (a + b))**2 for soil, a, b in
                              zip(stiffness[:-1], delta, delta[1:]))
    start, reference = u[-1] - plastic[-1], equilibrium[-1] - plastic[-1]
    toe_relative = (max(0.0, start)**2 if reference <= 0 else
                    ((start - reference)**2 if start >= 0 else reference**2 - 2 * reference * start))
    energy += 0.5 * stiffness[-1] * toe_relative
    energy += math.fsum(abs(force * change) for force, change in zip(residual, delta))
    energy += 0.5 * len(u) * bound * norm_squared
    return energy if math.isfinite(energy) else math.inf


def _initial_motion(values: Sequence[float] | None, count: int, name: str) -> list[float]:
    result = [0.0] * count if values is None else list(values)
    if len(result) != count or not all(math.isfinite(x) for x in result):
        raise ValueError(f"{name} requires one finite value per pile node")
    return result


def simulate(
    pile: LumpedPile, *, time_step_s: float, duration_s: float,
    hammer: Hammer | None = None,
    shaft: Sequence[SoilResistance] | None = None,
    toe: SoilResistance | None = None,
    initial_displacement_m: Sequence[float] | None = None,
    initial_velocity_m_s: Sequence[float] | None = None,
    settlement_velocity_m_s: float = 1e-3,
) -> BlowResult:
    """Simulate one impact, or a free rod when no hammer or soil is supplied.

    Args:
        pile: Lumped masses and spring geometry, ordered head to toe.
        time_step_s: Maximum step, limited to the mesh/contact resolution bound.
        duration_s: End time; the last step is shortened to reach it exactly.
        hammer: Ram initially touching the head with downward impact velocity.
        shaft: One total shaft resistance per segment, applied at its midpoint.
        toe: Separate unilateral resistance at the last pile node.
        initial_displacement_m: Optional initial pile displacement at each node.
        initial_velocity_m_s: Optional initial pile velocity at each node.
        settlement_velocity_m_s: Maximum pile speed throughout the settling dwell.

    Returns:
        Motion, stress and energy records; settled and estimated set are distinct.

    Raises:
        ValueError: If inputs are nonphysical, inconsistent or under-resolved.
        RuntimeError: If a nonlinear step cannot converge.
    """
    _positive("time step", time_step_s)
    _positive("duration", duration_s)
    _positive("settlement velocity", settlement_velocity_m_s)
    step_limit = pile.recommended_time_step_s
    if hammer is not None:
        step_limit = min(step_limit, 0.1 * math.sqrt(
            min(hammer.mass_kg, pile.masses_kg[0]) / hammer.cushion_stiffness_n_m))
    count = len(pile.stiffnesses_n_m)
    shaft_soils = (SoilResistance(),) * count if shaft is None else tuple(shaft)
    if len(shaft_soils) != count:
        raise ValueError("shaft requires one SoilResistance per segment")
    soils = shaft_soils + (toe if toe is not None else SoilResistance(),)
    # Resolve elastic soil response as well as rod and cushion response.
    for i, soil in enumerate(soils):
        if soil.ultimate_resistance_n > 0:
            mass = (pile.masses_kg[-1] if i == count
                    else min(pile.masses_kg[i], pile.masses_kg[i + 1]))
            step_limit = min(step_limit, 0.1 * math.sqrt(mass / soil.stiffness_n_m))
    if time_step_s > step_limit * (1 + 1e-12):
        raise ValueError(f"time_step_s exceeds the resolution bound {step_limit:.6g} s")
    u = _initial_motion(initial_displacement_m, len(pile.masses_kg), "displacement")
    v = _initial_motion(initial_velocity_m_s, len(pile.masses_kg), "velocity")
    offset = int(hammer is not None)
    masses = pile.masses_kg
    if hammer is not None:
        masses = (hammer.mass_kg,) + masses
        u.insert(0, u[0])
        v.insert(0, math.sqrt(2 * hammer.energy_j / hammer.mass_kg))
    plastic = []
    for i, soil in enumerate(soils):
        start = u[-1] if i == count else 0.5 * (u[offset + i] + u[offset + i + 1])
        plastic.append(_soil_step(soil, start, 0.0, 0.0, time_step_s, i == count)[4])
    initial_energy = math.fsum(_stored_energy(pile, hammer, soils, masses, u, v, plastic))
    times: list[float] = []
    displacements: list[tuple[float, ...]] = []
    velocities: list[tuple[float, ...]] = []
    stresses: list[tuple[float, ...]] = []
    ram_u: list[float] = []
    ram_v: list[float] = []
    cushion_forces: list[float] = []
    energies: list[EnergyBalance] = []
    plastic_work = damping_work = quiet_duration = 0.0
    initial_toe = u[-1]
    steps = math.ceil(duration_s / time_step_s)
    for step in range(steps + 1):
        if times and times[-1] >= duration_s:
            break
        time = min(step * time_step_s, duration_s)
        if step:
            dt = time - times[-1]
            u, v, plastic, work, damping = _advance(
                pile, hammer, soils, masses, u, v, plastic, dt,
            )
            plastic_work += work
            damping_work += damping
            detached = hammer is None or (u[0] <= u[1] and v[0] <= v[1])
            if detached and max(abs(speed) for speed in v[offset:]) <= settlement_velocity_m_s:
                quiet_duration += dt
            else:
                quiet_duration = 0.0
        times.append(time)
        displacements.append(tuple(u[offset:]))
        velocities.append(tuple(v[offset:]))
        stresses.append(tuple(
            k * (u[offset + i] - u[offset + i + 1]) / pile.areas_m2[i]
            for i, k in enumerate(pile.stiffnesses_n_m)
        ))
        if hammer is not None:
            ram_u.append(u[0])
            ram_v.append(v[0])
        cushion_forces.append(hammer.cushion_stiffness_n_m * max(0.0, u[0] - u[1])
                              if hammer is not None else 0.0)
        stored = _stored_energy(pile, hammer, soils, masses, u, v, plastic)
        residual = initial_energy - math.fsum((*stored, plastic_work, damping_work))
        energies.append(EnergyBalance(*stored, plastic_work, damping_work, residual))
    total_resistance = sum(soil.ultimate_resistance_n for soil in soils)
    settled = total_resistance > 0 and quiet_duration >= pile.settling_window_s
    if settled:
        remaining = _remaining_pile_energy(pile, soils, u[offset:], v[offset:], plastic)
        settled = remaining <= 0.5 * min(pile.masses_kg) * settlement_velocity_m_s**2
        if hammer is not None:
            # A separated ram must remain behind even the fastest possible rebound.
            settled = settled and u[0] <= u[1] and v[0] <= -math.sqrt(
                2 * remaining / pile.masses_kg[0])
    permanent_set = max(0.0, u[-1] - initial_toe) if settled else None
    smith_set = None
    if total_resistance > 0 and any(row[-1] < 0 for row in velocities):
        quake = sum(soil.ultimate_resistance_n * soil.quake_m for soil in soils
                    if soil.quake_m is not None) / total_resistance
        smith_set = max(0.0, max(row[-1] for row in displacements) - initial_toe - quake)

    def reciprocal(value: float | None) -> float | None:
        return None if value is None else (1 / value if value > 0 else math.inf)

    return BlowResult(
        tuple(times), pile.node_depths_m, tuple(displacements), tuple(velocities), tuple(stresses),
        tuple(ram_u), tuple(ram_v), tuple(cushion_forces), tuple(energies), initial_energy,
        tuple(max(0.0, max(row[i] for row in stresses)) for i in range(count)),
        tuple(max(0.0, -min(row[i] for row in stresses)) for i in range(count)),
        tuple(plastic[:-1]), plastic[-1], settled, permanent_set, reciprocal(permanent_set),
        smith_set, reciprocal(smith_set),
    )
