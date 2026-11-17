"""Explicit rigid-pile resistance components in m, kN, kPa and kN/m.

Method recovered from a legacy engineering calculation: sand lateral resistance
is min((C1*z+C2*D)*sigma', C3*D*sigma'); shaft friction uses supplied curves or
the existing beta method. Discrete nodal forces define moments about a load
and an explicitly supplied pivot. Axial components expose full-base/annulus
bearing, internal friction, uplift, compression and installation pressure.

Source references: API RP 2A; the help text states nineteenth edition and also
uses the spelling API RP2A (19th edition). Clay lateral resistance is labelled
"eqn. 6.7.2-1" and bearing factors "table 6.4.3-1 API RP 2A". For sand lateral,
shaft friction, rigid-pile recurrence and axial assembly: clause not stated in
source. Existing functions carry their own references; no compliance with a
particular revision is established by reusing their algebra.

Input gaps: C1/C2/C3, shaft-friction knots/limits, bearing factors and their
upstream citations are caller responsibilities; no source tables are included.
The pivot-selection rule is unresolved. Supplied forces require independent
soil-profile integration and quadrature weights. Public centroids and bending
moments follow statics; separate legacy fields retain the initial half-depth
centroid convention. Exposed residuals can be nonzero; the component is not an
equilibrium solver. Open-end bearing is capped at full-base bearing; a separate
legacy field retains the discontinuous switch. Negative suction differential
denotes a self-weight surplus, not a qualified installation design.

Left out: automatic pivot/plug selection, layered-profile/control-flow assembly,
calibration fits, stress interactions, plotting, and installation qualifications
such as hydraulic failure, cavitation and structural buckling. Applicability
requires a separately established short, rigid pile criterion. Caller-supplied
coefficients use upstream-wired citations; no tabulated constant is selected here.
"""

from __future__ import annotations

from dataclasses import dataclass, fields
import math

import numpy as np

from digitalmodel.geotechnical.piles import skin_friction_sand
from digitalmodel.structural.stress.vm_stress import PipeGeometry


def _finite(value: float, name: str, *, nonnegative: bool = True) -> float:
    if isinstance(value, bool) or not isinstance(value, (float, int)):
        raise ValueError(f'{name} must be a finite number')
    if not math.isfinite(value) or (nonnegative and value < 0):
        raise ValueError(f'{name} must be finite and nonnegative')
    return float(value)


def _check_fields(record: object) -> None:
    for field in fields(record):
        _finite(getattr(record, field.name), field.name)


@dataclass(frozen=True)
class SandPoint:
    """Local SI state and caller-qualified dimensionless lateral coefficients."""

    depth_m: float
    diameter_m: float
    effective_stress_kpa: float
    c1_ratio: float
    c2_ratio: float
    c3_ratio: float

    def __post_init__(self) -> None:
        _check_fields(self)
        if self.diameter_m == 0:
            raise ValueError('diameter_m must be positive')


@dataclass(frozen=True)
class SandLateralResistance:
    shallow_kn_m: float
    deep_kn_m: float
    ultimate_kn_m: float


def sand_lateral_resistance(point: SandPoint) -> SandLateralResistance:
    """Return shallow/deep sand resistance; API RP 2A, clause not stated in source."""
    shallow = (point.c1_ratio*point.depth_m + point.c2_ratio*point.diameter_m)*point.effective_stress_kpa
    deep = point.c3_ratio*point.diameter_m*point.effective_stress_kpa
    return SandLateralResistance(shallow, deep, min(shallow, deep))


@dataclass(frozen=True)
class FrictionCurve:
    """Caller-supplied strength/friction knots in kPa, starting at the origin.

    Flat right extrapolation preserves the friction <= strength bound.
    """

    points_kpa: tuple[tuple[float, float], ...]

    def __post_init__(self) -> None:
        try:
            points = tuple(tuple(pair) for pair in self.points_kpa)
        except TypeError as error:
            raise ValueError('curve requires strength/friction pairs') from error
        if len(points) < 2 or any(len(pair) != 2 for pair in points):
            raise ValueError('curve requires at least two strength/friction pairs')
        if points[0] != (0., 0.):
            raise ValueError('curve must start at the origin (0, 0)')
        for strength, friction in points:
            _finite(strength, 'strength_kpa')
            _finite(friction, 'friction_kpa')
            if friction > strength:
                raise ValueError('friction cannot exceed strength')
        if any(b[0] <= a[0] or b[1] < a[1] for a, b in zip(points, points[1:])):
            raise ValueError('strength must increase and friction must not decrease')
        object.__setattr__(self, 'points_kpa', points)


def cohesive_friction_from_curve(strength_kpa: float, curve: FrictionCurve) -> float:
    """Interpolate explicit friction knots; API RP 2A, clause not stated in source.

    No empirical breakpoints or friction cap are selected. Curve qualification
    and its citation belong to the caller. This can represent the older method.
    """
    strength = _finite(strength_kpa, 'strength_kpa')
    x, y = zip(*curve.points_kpa)
    return float(np.interp(strength, x, y))


def capped_sand_friction(effective_stress_kpa: float, interface_angle_deg: float,
                         earth_pressure_ratio: float, limit_kpa: float) -> float:
    """Return min(K*sigma'*tan(delta), limit), with explicit caller inputs.

    API RP 2A, clause not stated in source. The existing beta-method API
    subtracts five degrees internally; adding those five here adapts an explicit
    interface angle without implementing a second beta method. No soil-angle
    reduction, coefficient, or stress limit is inferred. The caller qualifies
    delta (interface_angle_deg) for the soil and interface conditions.
    """
    for value, name in ((effective_stress_kpa, 'effective_stress_kpa'),
                        (interface_angle_deg, 'interface_angle_deg'),
                        (earth_pressure_ratio, 'earth_pressure_ratio'), (limit_kpa, 'limit_kpa')):
        _finite(value, name)
    if interface_angle_deg >= 90:
        raise ValueError('interface angle must be below 90 degrees')
    friction = skin_friction_sand(effective_stress_kpa, interface_angle_deg + 5,
                                  earth_pressure_ratio).unit_friction_kpa
    return min(friction, limit_kpa)


@dataclass(frozen=True)
class ResistanceNode:
    """Depth below seabed and nonnegative integrated nodal resistance.

    The caller supplies all quadrature, endpoint and calibration weights.
    """

    depth_m: float
    element_force_kn: float

    def __post_init__(self) -> None:
        _check_fields(self)


@dataclass(frozen=True)
class RigidPileResponse:
    capacity_kn: float
    forward_moment_kn_m: tuple[float, ...]
    reverse_moment_kn_m: tuple[float, ...]
    reversal_flags: tuple[int, ...]
    cumulative_force_kn: tuple[float, ...]
    centroid_depth_m: tuple[float | None, ...]
    shear_kn: tuple[float, ...]
    bending_moment_kn_m: tuple[float, ...]
    force_residual_kn: float
    moment_residual_kn_m: float
    legacy_centroid_depth_m: tuple[float | None, ...]
    legacy_bending_moment_kn_m: tuple[float, ...]


def rigid_pile_response(nodes: tuple[ResistanceNode, ...], *, top_depth_m: float,
                        load_depth_m: float, pivot_depth_m: float) -> RigidPileResponse:
    """Evaluate the explicit-pivot discrete recurrence, without a pivot search.

    Depth is positive down from seabed. H=sum(F*abs(z-pivot))/(pivot-load).
    Forward moments sum F*(z-load); reverse moments sum backwards, reset at
    zero forward moment within an absolute tolerance of 1e-12 kN m. A negative
    reverse-minus-forward moment gives a reversed reaction. Such flags need
    not be sorted. The pivot must be a supplied node within an absolute
    tolerance of 1e-12 m, strictly below the load and at/below the pile top.

    Centroids are sum(signed F*z)/sum(signed F) through each node. Shear is
    sum(signed F) minus H below the load. Static bending moments are
    sum(signed F*(z_i-z)) minus H*max(z_i-load, 0). A zero signed resultant
    has no centroid (None), but its force couple remains in the static moment.
    The legacy_centroid_depth_m and legacy_bending_moment_kn_m fields reproduce
    a source convention, including a half-depth second centroid and zero
    centroid moment at zero shear/resultant; they are not static moments.
    Force residual is sum(signed F)-H; moment residual is sum(signed F*(z-load)).
    These residuals are diagnostics, not an acceptance check or a solved balance.
    API RP 2A, clause not stated in source.
    """
    top = _finite(top_depth_m, 'top_depth_m', nonnegative=False)
    load = _finite(load_depth_m, 'load_depth_m', nonnegative=False)
    pivot = _finite(pivot_depth_m, 'pivot_depth_m')
    nodes = tuple(nodes)
    z = tuple(node.depth_m for node in nodes)
    if len(z) < 2 or any(b <= a for a, b in zip(z, z[1:])):
        raise ValueError('at least two strictly increasing nodes are required')
    if (not any(math.isclose(pivot, depth, rel_tol=0., abs_tol=1e-12) for depth in z)
            or pivot <= load or pivot < top):
        raise ValueError('pivot must be on the grid, at/below top and below load')
    force = tuple(node.element_force_kn if node.depth_m >= top else 0. for node in nodes)
    if sum(force) <= 0:
        raise ValueError('nonzero total resistance is required')
    forward, total = [], 0.
    increments = tuple(f*(depth-load) for f, depth in zip(force, z))
    for moment in increments:
        total += moment
        forward.append(total)
    reverse, total = [0.]*len(z), 0.
    for i in range(len(z)-1, -1, -1):
        total = (total + increments[i]
                 if not math.isclose(forward[i], 0., rel_tol=0., abs_tol=1e-12) else 0.)
        reverse[i] = total
    flags = tuple(int(b-a < 0) for a, b in zip(forward, reverse))
    capacity = sum(f*abs(pivot-depth) for f, depth in zip(force, z))/(pivot-load)
    signed = tuple(-f if flag else f for f, flag in zip(force, flags))
    cumulative, centroids, shear, bending = [], [], [], []
    legacy_centroids, legacy_bending = [], []
    total, first_moment = 0., 0.
    for i, (f, depth) in enumerate(zip(signed, z)):
        previous = total
        total += f
        first_moment += f*depth
        applied = capacity if depth > load else 0.
        s = total-applied
        centroid = first_moment/total if total != 0 else None
        moment = depth*total-first_moment-applied*max(depth-load, 0.)
        if total == 0:
            legacy_centroid = None
        elif i == 0:
            legacy_centroid = depth
        elif i == 1:
            legacy_centroid = depth/2 if s != 0 else 0.
        else:
            prior_centroid = legacy_centroids[-1] if legacy_centroids[-1] is not None else 0.
            legacy_centroid = (f*depth + previous*prior_centroid)/total
        legacy_moment = ((depth-legacy_centroid)*total
                         if legacy_centroid is not None and s != 0 else 0.)
        legacy_moment -= applied*(depth-load)
        cumulative.append(total)
        centroids.append(centroid)
        shear.append(s)
        bending.append(moment)
        legacy_centroids.append(legacy_centroid)
        legacy_bending.append(legacy_moment)
    return RigidPileResponse(capacity, tuple(forward), tuple(reverse), flags,
                             tuple(cumulative), tuple(centroids), tuple(shear), tuple(bending),
                             sum(signed)-capacity, sum(f*(depth-load) for f, depth in zip(signed, z)),
                             legacy_centroid_depth_m=tuple(legacy_centroids),
                             legacy_bending_moment_kn_m=tuple(legacy_bending))


@dataclass(frozen=True)
class PileSection:
    """Tube dimensions and nonoverlapping internal bulkhead area/perimeter."""

    diameter_m: float
    wall_thickness_m: float
    bulkhead_area_m2: float
    internal_extra_perimeter_m: float

    def __post_init__(self) -> None:
        _check_fields(self)
        geometry = PipeGeometry(self.diameter_m, self.wall_thickness_m)
        if self.bulkhead_area_m2 >= geometry.inner_area:
            raise ValueError('bulkhead area must be smaller than internal disk area')


@dataclass(frozen=True)
class AxialInputs:
    """Explicit friction, bearing pressures, submerged weight and plug weight."""

    outer_friction_kn: float
    full_base_bearing_kpa: float
    annulus_bearing_kpa: float
    pile_submerged_weight_kn: float
    plug_weight_kn: float

    def __post_init__(self) -> None:
        _check_fields(self)


@dataclass(frozen=True)
class AxialComponents:
    internal_friction_kn: float
    full_base_bearing_kn: float
    annulus_bearing_kn: float
    open_end_bearing_kn: float
    closed_compression_kn: float
    open_compression_kn: float
    uplift_kn: float
    installation_resistance_kn: float
    suction_differential_kpa: float
    legacy_open_end_bearing_kn: float


def axial_components(section: PileSection, inputs: AxialInputs) -> AxialComponents:
    """Assemble explicit legacy axial components; no plugging switch is inferred.

    Internal friction scales external friction by inner/outer circumference plus
    caller-supplied additional internal perimeter. Full bearing uses the outer
    disk; annulus bearing uses steel annulus plus bulkhead area. Open bearing
    is min(internal friction + annulus bearing, full bearing). The separate
    legacy_open_end_bearing_kn field retains the discontinuous source convention:
    internal friction + annulus bearing when internal friction < full bearing,
    otherwise full bearing. Open compression uses the capped bearing value.
    Compression subtracts submerged pile weight to reproduce the legacy load
    convention. Suction uses the full inner disk area, not the steel annulus.
    Component outputs do not qualify
    automatic plugged/unplugged design. API RP 2A, clause not stated in source;
    caller bearing factors may refer to table 6.4.3-1 API RP 2A.
    """
    geometry = PipeGeometry(section.diameter_m, section.wall_thickness_m)
    internal = inputs.outer_friction_kn * (
        geometry.inner_diameter/section.diameter_m
        + section.internal_extra_perimeter_m/(math.pi*section.diameter_m))
    full = inputs.full_base_bearing_kpa*(geometry.inner_area+geometry.cross_sectional_area)
    annulus = inputs.annulus_bearing_kpa*(geometry.cross_sectional_area+section.bulkhead_area_m2)
    open_bearing = min(internal+annulus, full)
    legacy_open_bearing = internal+annulus if internal < full else full
    installation = inputs.outer_friction_kn+internal+annulus
    return AxialComponents(internal, full, annulus, open_bearing,
                            inputs.outer_friction_kn+full-inputs.pile_submerged_weight_kn,
                            inputs.outer_friction_kn+open_bearing-inputs.pile_submerged_weight_kn,
                            inputs.outer_friction_kn+inputs.pile_submerged_weight_kn+min(inputs.plug_weight_kn, internal),
                            installation, (installation-inputs.pile_submerged_weight_kn)/geometry.inner_area,
                            legacy_open_end_bearing_kn=legacy_open_bearing)
