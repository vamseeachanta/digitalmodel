# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Illustrative spudcan screening using open research, with SI inputs.

Hu et al. (2015), equations 4–14:
https://jack-up.citystgeorges.ac.uk/wp-content/uploads/2025/09/Jackup-2015-11.pdf

Only the analytical peak and underlying-clay branches are implemented. Straight
connecting segments are illustrative assumptions, not Hu's post-peak solution.
Depth refers to the lowest level of the widest spudcan cross-section. Results
exclude dynamics, buoyancy, cyclic degradation, scour and structural interaction.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, fields


def finite(name: str, value: float, *, minimum: float = 0.0, strict: bool = False) -> None:
    """Reject Boolean, nonfinite and out-of-domain numerical inputs."""
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f'{name} must be a finite number')
    if not math.isfinite(value) or value < minimum or (strict and value == minimum):
        raise ValueError(f'{name} is outside its finite admissible range')


@dataclass(frozen=True)
class SandOverClay:
    """Two layers; clay strength is defined at the sand–clay interface.

    The Hu conical-footing calibration requires 0.16 <= H/D <= 1.0 and a base
    cone angle greater than 7 degrees. ID is a fraction. Q is the natural log
    of crushing strength expressed in kPa, not a dimensional crushing strength.
    Area may retain a published value rounded separately from equivalent D.
    """

    diameter_m: float
    area_m2: float
    sand_thickness_m: float
    interface_su_kpa: float
    clay_gradient_kpa_m: float
    sand_submerged_weight_kn_m3: float
    clay_submerged_weight_kn_m3: float
    relative_density: float
    critical_state_friction_deg: float = 31.0
    log_crushing_strength_kpa: float = 10.0
    base_cone_angle_deg: float = 13.0

    def validate(self) -> None:
        """Enforce geometry, positive strengths and published method bounds."""
        for item in fields(self):
            finite(item.name, getattr(self, item.name),
                   strict=item.name != 'clay_gradient_kpa_m')
        if not 0.16 <= self.sand_thickness_m / self.diameter_m <= 1.0:
            raise ValueError('Hu conical-footing calibration requires 0.16 <= H/D <= 1.0')
        if self.relative_density > 1.0:
            raise ValueError('relative density must be a fraction in (0, 1]')
        if not 7.0 < self.base_cone_angle_deg < 90.0:
            raise ValueError('base cone angle must be greater than 7 and less than 90 degrees')
        if not 20 <= self.critical_state_friction_deg <= 40:
            raise ValueError('critical-state friction angle must lie in [20, 40] degrees')
        equivalent_area = math.pi * self.diameter_m**2 / 4
        if abs(self.area_m2 / equivalent_area - 1) > 0.02:
            raise ValueError('area and equivalent diameter differ by more than 2 percent')


def clay_resistance_kpa(su_kpa: float, gradient_kpa_m: float, diameter_m: float) -> float:
    """Circular reference pressure, Hu 2015 equation 5 at zero sand thickness.

    q = 6.34 su + 0.56 k D. This is a reference half-space solution, without
    embedment or remoulding corrections; it is not a full deep-clay installation
    model. For uniform clay (k=0) q=6.34 su.
    """
    finite('su', su_kpa, strict=True)
    finite('gradient', gradient_kpa_m)
    finite('diameter', diameter_m, strict=True)
    return 6.34 * su_kpa + 0.56 * gradient_kpa_m * diameter_m


@dataclass(frozen=True)
class UniformSand:
    """Drained circular flat-base reference, separate from the Hu calibration.

    No clay interface, conical-base correction or stress-dependent dilatancy is
    implied. This is an explicit homogeneous-soil branch, not an extrapolation
    of the bounded layered model to infinite sand thickness.
    """

    diameter_m: float
    submerged_weight_kn_m3: float
    friction_deg: float


def sand_resistance_kpa(diameter_m: float, depth_m: float, gamma_kn_m3: float,
                        friction_deg: float) -> float:
    """Circular sand pressure using the circular-plate research expression.

    The Bearing Capacity of Sand, tested by Loading Circular Plates (1961),
    equations 3 and 5, printed page 662:
    https://www.issmge.org/uploads/publications/1/40/1961_01_0110.pdf
    q = 0.3 gamma B Ngamma + 1.2 gamma d (1 + 0.1 d/B) Nq, B = diameter.
    Thus sgamma=0.6, sq=1.2, dgamma=1 and dq=1+0.1 d/B. Apparent cohesion,
    extra surface loading and settlement correction are zero here. Nq and
    Ngamma use equations 2 and 4 of the 1970 research bulletin:
    https://www.geo.dk/media/1431/geodgibulletinno28.pdf

    Restricted here to shallow embedment d/B <= 1 and 0 < phi < 50 degrees.
    The 1961 paper reports measured capacities exceeding theoretical factors;
    this arithmetic reference does not claim to reproduce those measurements.
    """
    finite('diameter', diameter_m, strict=True)
    finite('depth', depth_m)
    finite('submerged unit weight', gamma_kn_m3, strict=True)
    finite('friction angle', friction_deg, strict=True)
    if friction_deg >= 50 or depth_m > diameter_m:
        raise ValueError('circular sand reference requires phi < 50 degrees and depth/diameter <= 1')
    phi = math.radians(friction_deg)
    nq = math.exp(math.pi * math.tan(phi)) * math.tan(math.pi / 4 + phi / 2)**2
    ngamma = 1.5 * (nq - 1) * math.tan(phi)
    dq = 1 + 0.1 * depth_m / diameter_m
    return 0.3 * gamma_kn_m3 * diameter_m * ngamma + 1.2 * gamma_kn_m3 * depth_m * dq * nq


def _peak_for_pressure(profile: SandOverClay, pressure_kpa: float) -> float:
    """Evaluate Hu equations 4–12 at a trial pressure for the implicit solve."""
    p = profile
    ir = p.relative_density * (p.log_crushing_strength_kpa - math.log(pressure_kpa)) - 1
    phi = math.radians(p.critical_state_friction_deg + 2.65 * ir)
    psi = math.radians(2.65 * ir / 0.8)
    tangent = math.tan(psi)
    reduced_tangent = math.sin(phi) * math.cos(psi) / (1 - math.sin(phi) * math.sin(psi))
    distribution = 0.642 * (p.sand_thickness_m / p.diameter_m)**(-0.576)
    exponent = 2 * (1 + distribution * (reduced_tangent / tangent - 1))
    spread = 1.76 * p.sand_thickness_m / p.diameter_m * tangent
    nc = 6.34 + 0.56 * p.clay_gradient_kpa_m * (
        p.diameter_m + 1.76 * p.sand_thickness_m * tangent) / p.interface_su_kpa
    multiplier = math.exp(exponent * math.log1p(spread))
    return (nc * p.interface_su_kpa + 0.12 * p.sand_submerged_weight_kn_m3 * p.sand_thickness_m) * multiplier + (
        p.sand_submerged_weight_kn_m3 * p.diameter_m / (2 * tangent * (exponent + 1))
        * (1 - (1 - exponent * spread) * multiplier))


def hu_peak_resistance_kpa(profile: SandOverClay) -> float:
    """Solve Hu 2015 equations 4–12, requiring the stated 0 < IR < 4 bounds.

    Bisection stays within the admissible dilatancy interval. No friction angle,
    pressure or relative density is silently clipped to force convergence.
    """
    profile.validate()
    try:
        low = math.exp(profile.log_crushing_strength_kpa - (5 - 1e-5) / profile.relative_density)
        high = math.exp(profile.log_crushing_strength_kpa - (1 + 1e-5) / profile.relative_density)
        if low == 0 or not math.isfinite(high):
            raise ValueError('pressure bracket is outside numerical range')
        if _peak_for_pressure(profile, low) < low or _peak_for_pressure(profile, high) > high:
            raise ValueError('peak has no root within the published dilatancy bounds')
        for _ in range(100):
            midpoint = (low + high) / 2
            residual = _peak_for_pressure(profile, midpoint) - midpoint
            if abs(residual) <= 1e-8 * max(1.0, midpoint):
                return midpoint
            if residual > 0:
                low = midpoint
            else:
                high = midpoint
    except (OverflowError, ZeroDivisionError) as exc:
        raise ValueError('profile exceeds the numerical range of the peak solution') from exc
    raise ValueError('peak resistance did not converge')


def hu_clay_resistance_kpa(profile: SandOverClay, depth_m: float) -> float:
    """Hu 2015 equations 13–14; depth and interface share a mudline datum."""
    profile.validate()
    finite('depth', depth_m, minimum=profile.sand_thickness_m)
    nc = 11 * profile.sand_thickness_m / profile.diameter_m + 10.5
    su = profile.interface_su_kpa + profile.clay_gradient_kpa_m * (depth_m - profile.sand_thickness_m)
    return nc * su + 0.9 * profile.sand_thickness_m * profile.clay_submerged_weight_kn_m3


@dataclass(frozen=True)
class PenetrationPoint:
    """Force and pressure; branch labels distinguish assumed interpolation."""

    depth_m: float
    pressure_kpa: float
    load_kn: float
    branch: str


def penetration_curve(profile: SandOverClay | UniformSand,
                      depths_m: list[float]) -> list[PenetrationPoint]:
    """Sample the curve, adding the exact peak and interface to prevent aliasing.

    Linear mobilization from zero and linear peak-to-interface interpolation are
    illustrative assumptions. Only peak and clay pressures are research equations.
    The caller must constrain depths to the qualified extent of its soil profile.
    """
    if not depths_m:
        raise ValueError('depth grid must not be empty')
    for depth in depths_m:
        finite('depth', depth)
    if isinstance(profile, UniformSand):
        result = []
        for depth in sorted(set(depths_m)):
            pressure = sand_resistance_kpa(profile.diameter_m, depth,
                                           profile.submerged_weight_kn_m3, profile.friction_deg)
            result.append(PenetrationPoint(depth, pressure,
                          pressure * math.pi * profile.diameter_m**2 / 4,
                          'uniform_sand_circular'))
        return result
    peak = hu_peak_resistance_kpa(profile)
    h = profile.sand_thickness_m
    peak_depth = 0.12 * h
    interface = hu_clay_resistance_kpa(profile, h)
    grid = sorted(set(depths_m) | {peak_depth, h})
    result = []
    for depth in grid:
        if depth < peak_depth:
            pressure, branch = peak * depth / peak_depth, 'illustrative_mobilization'
        elif depth == peak_depth:
            pressure, branch = peak, 'peak'
        elif depth < h:
            pressure = peak + (interface - peak) * (depth - peak_depth) / (h - peak_depth)
            branch = 'illustrative_post_peak'
        else:
            pressure, branch = hu_clay_resistance_kpa(profile, depth), 'clay'
        result.append(PenetrationPoint(depth, pressure, pressure * profile.area_m2, branch))
    return result


@dataclass(frozen=True)
class PunchThroughScreen:
    """Potential resistance loss; recovery depths are static, not dynamic runs."""

    peak_load_kn: float
    interface_load_kn: float
    peak_depth_m: float
    capacity_drop: bool
    preload_reaches_peak: bool
    peak_recovery_depth_m: float | None
    preload_equilibrium_depth_m: float | None


def punch_through_screen(profile: SandOverClay, preload_kn: float) -> PunchThroughScreen:
    """Identify resistance loss and whether a supplied preload reaches the peak.

    Recovery is the depth at which the clay branch again equals a load. None
    denotes no finite recovery for a zero-gradient clay, not a safe outcome.
    Preload below the peak does not rule out hazards from input uncertainty.
    """
    finite('preload', preload_kn, strict=True)
    peak = hu_peak_resistance_kpa(profile)
    h = profile.sand_thickness_m
    interface = hu_clay_resistance_kpa(profile, h)
    slope = (11 * h / profile.diameter_m + 10.5) * profile.clay_gradient_kpa_m
    drop = peak > interface
    reaches = preload_kn >= peak * profile.area_m2

    def recovery(pressure: float) -> float | None:
        if pressure <= interface:
            return h
        return h + (pressure - interface) / slope if slope > 0 else None

    pressure = preload_kn / profile.area_m2
    if not reaches:
        equilibrium = pressure / peak * 0.12 * h
    elif not drop and pressure < interface:
        equilibrium = 0.12 * h + (pressure - peak) / (interface - peak) * 0.88 * h
    else:
        equilibrium = recovery(pressure)
    return PunchThroughScreen(
        peak * profile.area_m2, interface * profile.area_m2, 0.12 * h,
        drop, reaches, recovery(peak) if drop else h, equilibrium,
    )
