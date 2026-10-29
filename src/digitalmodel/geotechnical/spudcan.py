# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Illustrative spudcan screening using open research, with SI inputs.

Hu et al. (2015), equations 4–14:
https://jack-up.citystgeorges.ac.uk/wp-content/uploads/2025/09/Jackup-2015-11.pdf

The original two-argument interface retains the analytical peak and underlying-
clay branches. Straight connecting segments in that interface are illustrative
assumptions, not the research post-peak solution. LayeredProfile selects the
separate bottom-up envelope documented in the layered method note.
In the original interface, depth is the lowest level of the widest cross-section.
The layered interface instead publishes explicitly converted tip depths. Results
exclude dynamics, buoyancy, cyclic degradation, scour and structural interaction.

Clause context: RP 6.2.1-6.2.3, 6.2.6.1-6.2.6.4, 8.3.5;
C6.2.1-C6.2.3, C6.2.6.3, C8.3, first edition. RP document identity and
OPEN items are recorded in docs/domains/geotechnical/jackup-screening.md.
Only the cited research equations execute; unresolved OCR expressions have no
execution path. Clause references do not qualify these research calculations.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, fields
from typing import Callable, overload


def finite(name: str, value: object, *, minimum: float = 0.0, strict: bool = False) -> None:
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
    The [20, 40] critical-state friction interval is an illustrative silica-sand
    restriction assumed, based on previous experience, not a research calibration
    bound. Area may retain a value rounded separately from equivalent D; 2% is
    an input reconciliation tolerance, not a physical or calibration limit.
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
        """Enforce geometry, positive strengths and published method bounds.

        RP 6.2.1-6.2.3; C6.2.1-C6.2.3: SI inputs and full circular-area
        reconciliation only; partial engagement and standard corrections OPEN.
        """
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
    RP 6.2.1-6.2.3; C6.2.1-C6.2.3: research factor family only, without
    standard backflow or deep-embedment corrections.
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

    The restrictions d/B <= 1 and 0 < phi < 50 degrees are illustrative shallow
    reference limits assumed, based on previous experience; neither is a
    published calibration bound of these papers. They exclude deeper embedment
    and high-friction extrapolation without qualifying the retained range.
    The 1961 paper reports measured capacities exceeding theoretical factors;
    this arithmetic reference does not claim to reproduce those measurements.
    RP 6.2.1-6.2.3; C6.2.1-C6.2.3: one research factor family and one
    friction-angle input; standard sand-angle and backflow corrections OPEN.
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

    The IR endpoint margin 1e-5 avoids the singular zero-dilatancy boundary;
    1e-8 relative pressure (1e-8 kPa below 1 kPa) and 100 iterations are numerical
    solve tolerances, not research calibration limits.
    Bisection stays within the admissible dilatancy interval. No friction angle,
    pressure or relative density is silently clipped to force convergence.
    RP 6.2.1-6.2.3; C6.2.1-C6.2.3: research sand-angle adjustment occurs
    once in each residual evaluation, without standard factor substitution.
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
    """Hu 2015 equations 13–14; depth and interface share a mudline datum.

    RP 6.2.1-6.2.3; C6.2.1-C6.2.3: depth is the bearing plane, not the tip;
    the sand-plug term is research-specific, not a standard backflow correction.
    """
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


def _reference_penetration_curve(profile: SandOverClay | UniformSand,
                      depths_m: list[float]) -> list[PenetrationPoint]:
    """Sample the curve, adding the exact peak and interface to prevent aliasing.

    Linear mobilization from zero and linear peak-to-interface interpolation are
    illustrative assumptions. Only peak and clay pressures are research equations.
    The caller must constrain depths to the qualified extent of its soil profile.
    RP 6.2.1-6.2.3; C6.2.1-C6.2.3: kPa times full circular area gives kN;
    bearing-plane depths must share a datum. Partial engagement is OPEN.
    RP 6.2.6.1-6.2.6.4, 8.3.5: retain descending branches for hazard screening.
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
    RP 6.2.6.1-6.2.6.4, 8.3.5; C6.2.6.3, C8.3: reports peak depth, peak
    load and reduced interface load on a descending branch. The interface depth
    is profile.sand_thickness_m. Missing finite recovery is explicit; a supplied
    margin/plunge criterion and qualified lower support remain OPEN.
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


@dataclass(frozen=True)
class SoilLayer:
    """Constant properties within a contiguous mudline-referenced interval.

    Units: m, kN/m3, kPa, degrees. Clay is undrained and sand drained. Layers
    carry effective unit weights; strengths are effective-stress independent.
    No averaging across interfaces or implicit strength gradient is performed.
    """

    kind: str
    top_m: float
    bottom_m: float
    submerged_weight_kn_m3: float
    su_kpa: float | None = None
    friction_deg: float | None = None


@dataclass(frozen=True)
class Spudcan:
    """Equivalent circular bearing area and an explicit tip-to-plane offset.

    Tip depth = bearing-plane depth + tip_to_bearing_m. A zero offset explicitly
    represents a flat base. Partial area engagement is outside this model.
    """

    diameter_m: float
    tip_to_bearing_m: float = 0.0

    @property
    def area_m2(self) -> float:
        """Full circular projected area used to convert pressure to leg reaction."""
        return (math.pi / 4 * self.diameter_m) * self.diameter_m


@dataclass(frozen=True)
class LayeredProfile:
    """Bottom-up, wished-in-place mechanism envelope; not a dynamic solution.

    Any number of constant-strength clay layers may underlie one surface sand
    layer. Sand spread ratios are horizontal:vertical = 1:spread_ratio; the
    selected range 3–5 follows the open 2015 sand paper. Punching coefficients
    must be independently supplied; no standard chart is inferred or digitized.
    A coefficient provided for load spread is rejected to prevent inactive input.

    Clay general shear reuses q=6.34 su from the circular reference above. This
    differs from the 6.05 surface factor in the open 2015 weak-layer paper.
    Equation 6 retains that explicitly selected general-shear factor. Equation 2
    retains its published 5, 0.33 and 1.2 coefficients and is bounded by the same
    circular general-shear references in both layers. These choices are a
    research screening envelope, without standard-conformance qualification.

    SNAME T&RB 5-5A Rev 3 (2008), clause: TO CONFIRM;
    ISO 19905-1:2012, clause: TO CONFIRM. Equation sources and limitations:
    docs/domains/geotechnical/jackup-layered-penetration.md.
    """

    layers: tuple[SoilLayer, ...]
    sand_method: str = 'load_spread'
    spread_ratio: float = 3.0
    punching_coefficient: float | None = None


@dataclass(frozen=True)
class CapacityDrop:
    """One peak-to-trough loss and static recovery within the evaluated interval.

    Depths refer to the tip. Recovery is a force equilibrium, not a prediction
    of dynamic plunge or arrest. None means no recovery in the evaluated extent.
    """

    peak: PenetrationPoint
    trough: PenetrationPoint
    drop_kn: float
    drop_percent: float
    recovery_depth_m: float | None
    preload_reaches_peak: bool | None
    preload_recovery_depth_m: float | None


@dataclass(frozen=True)
class LayeredPenetrationCurve:
    """Sampled leg reactions with exact mechanism transitions added to the grid.

    Peak identifies the first descending branch, or the interval maximum if no
    loss occurs. Maximum separately retains a deeper, higher recovered capacity.
    The flag is None without a preload; False only describes this finite curve.
    """

    points: tuple[PenetrationPoint, ...]
    peak: PenetrationPoint
    maximum: PenetrationPoint
    post_peak_drop_kn: float
    events: tuple[CapacityDrop, ...]
    preload_kn: float | None
    punch_through_flag: bool | None
    preload_support_depth_m: float | None
    evaluated_tip_interval_m: tuple[float, float]


def _validate_layered(profile: LayeredProfile, spudcan: Spudcan) -> None:
    """Reject gaps, incompatible parameters and unsupported sand arrangements."""
    finite('diameter', spudcan.diameter_m, strict=True)
    finite('tip-to-bearing offset', spudcan.tip_to_bearing_m)
    finite('circular area', spudcan.area_m2, strict=True)
    if not profile.layers:
        raise ValueError('layered profile must contain at least one clay layer')
    if profile.sand_method not in ('load_spread', 'punching_shear'):
        raise ValueError('unknown sand method')
    finite('spread ratio', profile.spread_ratio, strict=True)
    if not 3 <= profile.spread_ratio <= 5:
        raise ValueError('spread ratio must be in [3, 5]')
    has_sand = profile.layers[0].kind == 'sand'
    if profile.punching_coefficient is not None:
        finite('punching coefficient', profile.punching_coefficient, strict=True)
        if profile.sand_method != 'punching_shear' or not has_sand:
            raise ValueError('punching coefficient requires a surface sand punching method')
    if has_sand and profile.sand_method == 'punching_shear' and profile.punching_coefficient is None:
        raise ValueError('sand punching shear requires an explicit punching coefficient')
    bottom = 0.0
    for index, layer in enumerate(profile.layers):
        finite('layer top', layer.top_m)
        finite('layer bottom', layer.bottom_m, minimum=layer.top_m, strict=True)
        finite('submerged unit weight', layer.submerged_weight_kn_m3)
        if layer.top_m != bottom:
            raise ValueError('soil layers must be contiguous from mudline zero')
        bottom = layer.bottom_m
        if layer.kind == 'clay':
            finite('clay strength', layer.su_kpa, strict=True)
            if layer.friction_deg is not None:
                raise ValueError('clay does not accept a sand friction angle')
        elif layer.kind == 'sand':
            if index != 0 or len(profile.layers) < 2:
                raise ValueError('only surface sand over clay is supported')
            finite('sand unit weight', layer.submerged_weight_kn_m3, strict=True)
            finite('sand friction angle', layer.friction_deg, strict=True)
            if layer.friction_deg is None or not 20 <= layer.friction_deg <= 40 or bottom > spudcan.diameter_m:
                raise ValueError('sand screening requires phi in [20, 40] and H/D <= 1')
            if layer.su_kpa is not None:
                raise ValueError('sand does not accept an undrained clay strength')
        else:
            raise ValueError('layer kind must be clay or sand')


def _bisect_depth(function: Callable[[float], float], low: float, high: float) -> float:
    """Solve a bracketed continuous root to a relative depth tolerance of 1e-12."""
    f_low, f_high = function(low), function(high)
    if f_low == 0:
        return low
    if f_high == 0:
        return high
    if (f_low > 0) == (f_high > 0):
        raise ValueError('depth root is not bracketed')
    for _ in range(100):
        middle = (low + high) / 2
        f_middle = function(middle)
        if high - low <= 1e-12 * max(1.0, abs(middle)):
            return middle
        if (f_middle > 0) == (f_low > 0):
            low, f_low = middle, f_middle
        else:
            high = middle
    raise ValueError('depth root did not converge')


def _required_parameter(name: str, value: float | None) -> float:
    """Keep missing soil parameters outside every arithmetic execution path."""
    if value is None:
        raise ValueError(f'{name} is required for the selected mechanism')
    return value


class _LayeredResistance:
    """Evaluate the declared bottom-up envelope without duplicating references."""

    def __init__(self, profile: LayeredProfile, spudcan: Spudcan) -> None:
        """Prepare net interface supports from the deepest clay upward."""
        self.profile = profile
        self.spudcan = spudcan
        self.lower_net = [0.0] * len(profile.layers)
        # Each interface support includes all deeper layers. Effective stress
        # is added once at the current bearing plane, never once per recursion.
        for index in range(len(profile.layers) - 2, -1, -1):
            self.lower_net[index] = self.net(index + 1, profile.layers[index].bottom_m)[0]

    def overburden(self, depth: float) -> float:
        """Effective stress from the actual piecewise unit-weight profile."""
        return sum(layer.submerged_weight_kn_m3 * max(0.0, min(depth, layer.bottom_m) - layer.top_m)
                   for layer in self.profile.layers)

    def general_clay(self, layer: SoilLayer) -> float:
        """Reuse the existing constant-strength circular clay reference."""
        strength = _required_parameter('clay strength', layer.su_kpa)
        return clay_resistance_kpa(strength, 0.0, self.spudcan.diameter_m)

    def squeeze(self, index: int, depth: float) -> float:
        """Open weak-layer paper (2015), equation 2; T is remaining weak clay."""
        layer = self.profile.layers[index]
        remaining = layer.bottom_m - depth
        if remaining == 0:
            return math.inf  # analytical limit, always bounded by lower support
        diameter = self.spudcan.diameter_m
        strength = _required_parameter('clay strength', layer.su_kpa)
        return strength * (5 + 0.33 * diameter / remaining + 1.2 * depth / diameter)

    def sand_layered(self, depth: float) -> float:
        """Open sand paper (2015), equations 1–2, applied at successive depths."""
        layer = self.profile.layers[0]
        remaining = layer.bottom_m - depth
        diameter = self.spudcan.diameter_m
        q0 = self.overburden(depth)
        if self.profile.sand_method == 'load_spread':
            spread = 1 + 2 * remaining / (diameter * self.profile.spread_ratio)
            return spread**2 * (self.lower_net[0] + q0)
        coefficient = _required_parameter('punching coefficient', self.profile.punching_coefficient)
        friction = _required_parameter('sand friction angle', layer.friction_deg)
        shear = coefficient * math.tan(math.radians(friction))
        return self.lower_net[0] + 2 * remaining / diameter * (
            layer.submerged_weight_kn_m3 * remaining + 2 * q0) * shear + q0

    def sand_general(self, depth: float) -> float:
        """Reuse the existing circular sand branch as the homogeneous upper bound."""
        layer = self.profile.layers[0]
        friction = _required_parameter('sand friction angle', layer.friction_deg)
        return sand_resistance_kpa(self.spudcan.diameter_m, depth,
                                   layer.submerged_weight_kn_m3, friction)

    def net(self, index: int, depth: float) -> tuple[float, str]:
        """Apply general shear, cylindrical clay punching or bounded squeezing."""
        layer = self.profile.layers[index]
        if layer.kind == 'sand':
            general, layered = self.sand_general(depth), self.sand_layered(depth)
            pressure = min(general, layered)
            branch = 'sand_general_shear' if general <= layered else self.profile.sand_method
            return pressure - self.overburden(depth), branch
        general = self.general_clay(layer)
        strength = _required_parameter('clay strength', layer.su_kpa)
        if index == len(self.profile.layers) - 1:
            return general, 'clay_general_shear'
        lower = self.lower_net[index]
        if lower < general:
            # Weak-layer paper, equation 6: alpha=0.75, 4*alpha=3.
            punching = lower + 3 * strength * (layer.bottom_m - depth) / self.spudcan.diameter_m
            return min(general, punching), 'clay_punching' if punching < general else 'clay_general_shear'
        squeezing = self.squeeze(index, depth)
        return min(lower, max(general, squeezing)), (
            'clay_squeezing' if squeezing > general and lower > general else 'clay_general_shear')

    def point(self, depth: float) -> PenetrationPoint:
        """Evaluate at a bearing-plane depth and publish a tip-referenced point."""
        layers = self.profile.layers
        index = next((i for i, layer in enumerate(layers) if depth < layer.bottom_m), len(layers) - 1)
        net, branch = self.net(index, depth)
        pressure = net + self.overburden(depth)
        load = pressure * self.spudcan.area_m2
        finite('layered pressure', pressure, strict=True)
        finite('leg reaction', load, strict=True)
        return PenetrationPoint(depth + self.spudcan.tip_to_bearing_m, pressure, load, branch)

    def anchors(self) -> list[float]:
        """Include all interfaces, cap switches and internal sand stationary points.

        The supported sand range makes the homogeneous-minus-layered capacity
        strictly increasing, giving at most one cap intersection. Clay punching
        is linear and squeezing is monotone, so their switches can be bracketed.
        """
        result = [0.0]
        diameter = self.spudcan.diameter_m
        for index, layer in enumerate(self.profile.layers):
            top, bottom = layer.top_m, layer.bottom_m
            result.extend((top, bottom))
            if layer.kind == 'sand':
                if self.profile.sand_method == 'load_spread':
                    b = 2 / (diameter * self.profile.spread_ratio)
                    a = 1 + b * bottom
                    gamma = layer.submerged_weight_kn_m3
                    turning = (a * gamma - 2 * b * self.lower_net[0]) / (3 * b * gamma)
                else:
                    coefficient = _required_parameter('punching coefficient', self.profile.punching_coefficient)
                    friction = _required_parameter('sand friction angle', layer.friction_deg)
                    shear = coefficient * math.tan(math.radians(friction))
                    turning = diameter / (4 * shear)
                if top < turning < bottom:
                    result.append(turning)
                def difference(depth: float) -> float:
                    return self.sand_general(depth) - self.sand_layered(depth)
                if difference(top) * difference(bottom) < 0:
                    result.append(_bisect_depth(difference, top, bottom))
            elif index < len(self.profile.layers) - 1:
                strength = _required_parameter('clay strength', layer.su_kpa)
                general, lower = self.general_clay(layer), self.lower_net[index]
                if lower < general:
                    switch = bottom - diameter * (general - lower) / (3 * strength)
                    if top < switch < bottom:
                        result.append(switch)
                elif lower > general:
                    for capacity in (general, lower):
                        if self.squeeze(index, top) < capacity:
                            result.append(_bisect_depth(lambda z: self.squeeze(index, z) - capacity, top, bottom))
        return result


def _support_depth(points: tuple[PenetrationPoint, ...], model: _LayeredResistance,
                   target: float) -> float | None:
    """First upward resistance crossing, excluding a peak followed by a loss.

    A surface capacity exceeding the load supports it at the first evaluated
    depth. A discontinuous upward transition crosses at its interface. None
    means no support within the evaluated interval; extrapolation is prohibited.
    """
    if target < points[0].load_kn:
        return points[0].depth_m

    def descends_after(index: int) -> bool:
        while index + 1 < len(points) and points[index + 1].load_kn == target:
            index += 1
        return index + 1 < len(points) and points[index + 1].load_kn < target

    offset = model.spudcan.tip_to_bearing_m
    for index, (a, b) in enumerate(zip(points, points[1:])):
        if a.load_kn <= target <= b.load_kn:
            endpoint = index if target == a.load_kn else index + 1
            if target in (a.load_kn, b.load_kn) and descends_after(endpoint):
                continue
            if a.load_kn == target:
                return a.depth_m
            return _bisect_depth(lambda z: model.point(z).load_kn - target,
                                 a.depth_m - offset, b.depth_m - offset) + offset
    if len(points) == 1 and target == points[0].load_kn:
        return points[0].depth_m
    return None


def _capacity_events(points: tuple[PenetrationPoint, ...], model: _LayeredResistance,
                     preload_kn: float | None) -> tuple[CapacityDrop, ...]:
    """Retain each descending branch; solve recovery on the actual envelope."""
    events = []
    def recovery(start: int, target: float) -> float | None:
        return _support_depth(points[start:], model, target)

    index = 0
    while index < len(points) - 1:
        if points[index + 1].load_kn >= points[index].load_kn:
            index += 1
            continue
        peak_index = index
        trough_index = index + 1
        index += 1
        while index + 1 < len(points) and points[index + 1].load_kn <= points[index].load_kn:
            index += 1
            if points[index].load_kn < points[trough_index].load_kn:
                trough_index = index
        peak, trough = points[peak_index], points[trough_index]
        reached = None if preload_kn is None else preload_kn >= max(p.load_kn for p in points[:peak_index + 1])
        drop = peak.load_kn - trough.load_kn
        events.append(CapacityDrop(peak, trough, drop, 100 * drop / peak.load_kn,
                                  recovery(trough_index, peak.load_kn), reached,
                                  recovery(trough_index, preload_kn) if reached and preload_kn is not None else None))
    return tuple(events)


@overload
def penetration_curve(profile: SandOverClay | UniformSand,
                      spudcan: list[float] | None = None, depths: None = None, *,
                      depths_m: list[float] | None = None,
                      preload_kn: None = None) -> list[PenetrationPoint]: ...


@overload
def penetration_curve(profile: LayeredProfile, spudcan: Spudcan,
                      depths: list[float], *, depths_m: None = None,
                      preload_kn: float | None = None) -> LayeredPenetrationCurve: ...


def penetration_curve(profile: SandOverClay | UniformSand | LayeredProfile,
                      spudcan: Spudcan | list[float] | None = None,
                      depths: list[float] | None = None, *,
                      depths_m: list[float] | None = None,
                      preload_kn: float | None = None) -> list[PenetrationPoint] | LayeredPenetrationCurve:
    """Return reactions, capacity losses and a preload flag for a layered profile.

    New interface: penetration_curve(profile, spudcan, depths, preload_kn=...).
    Depths are TIP penetrations below mudline; supply the surveyed tip-to-bearing
    offset. Requests above full bearing-plane engagement or below the profile
    are rejected. Exact mechanism transitions within the requested interval are
    added, so a coarse grid cannot hide the selected envelope's capacity loss.
    Post-peak loss reports the first event; all events are retained separately.
    Without preload the hazard flag is unresolved (None). With preload it means
    the load reaches a descending peak without being supported by an earlier,
    higher capacity. It is not an installation acceptance criterion.

    The original two-argument and depths_m keyword forms retain their original
    research branches, return type and BEARING-PLANE depth datum. They do not
    accept new geometry or preload arguments. No existing Hu curve is changed.
    SNAME T&RB 5-5A Rev 3 (2008), clause: TO CONFIRM;
    ISO 19905-1:2012, clause: TO CONFIRM. See the layered method note for sources.
    """
    if not isinstance(profile, LayeredProfile):
        if depths is not None or preload_kn is not None or isinstance(spudcan, Spudcan):
            raise ValueError('reference profiles require the original two-argument interface')
        if depths_m is not None and spudcan is not None:
            raise ValueError('depth grid was supplied twice')
        grid = depths_m if depths_m is not None else spudcan
        if not isinstance(grid, (list, tuple)):
            raise ValueError('reference depth grid is required')
        return _reference_penetration_curve(profile, grid)
    if not isinstance(spudcan, Spudcan) or depths is None or depths_m is not None:
        raise ValueError('layered profiles require spudcan geometry and tip depths')
    _validate_layered(profile, spudcan)
    if preload_kn is not None:
        finite('preload', preload_kn, strict=True)
    if not depths:
        raise ValueError('depth grid must not be empty')
    for depth in depths:
        finite('tip depth above bearing engagement', depth, minimum=spudcan.tip_to_bearing_m)
        if depth > profile.layers[-1].bottom_m:
            raise ValueError('tip depth extends below the investigated profile')
    model = _LayeredResistance(profile, spudcan)
    offset = spudcan.tip_to_bearing_m
    bearing_depths = {depth - offset for depth in depths}
    low, high = min(bearing_depths), max(bearing_depths)
    bearing_depths.update(z for z in model.anchors() if low <= z <= high)
    points = tuple(model.point(z) for z in sorted(bearing_depths))
    events = _capacity_events(points, model, preload_kn)
    maximum = max(points, key=lambda p: p.load_kn)
    peak = events[0].peak if events else maximum
    flag = None if preload_kn is None else any(event.preload_reaches_peak for event in events)
    support = None if preload_kn is None else _support_depth(points, model, preload_kn)
    return LayeredPenetrationCurve(points, peak, maximum,
                                   events[0].drop_kn if events else 0.0, events,
                                   preload_kn, flag, support,
                                   (points[0].depth_m, points[-1].depth_m))
