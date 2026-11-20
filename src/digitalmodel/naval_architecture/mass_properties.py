"""SI rigid-body mass properties generalized from a legacy engineering workbook.

Uniform axis-aligned cuboids only; diagonal mass moments are returned about
axes parallel to x (length), y (breadth), z (height). Products of inertia,
rotated components, fluid free-surface effects and added mass are excluded.
All empirical ratios and reference coordinates are caller inputs. See
``docs/mass-properties-and-tank-response.md`` for method limits and Input gaps.
"""
from __future__ import annotations

from dataclasses import dataclass
import math

from .gyradius import gyradius_from_inertia, gyradius_from_point_masses
from .tank_gauging.calibration import finite, positive

Vector3 = tuple[float, float, float]


def _vector(values: Vector3, label: str) -> Vector3:
    try:
        result = tuple(finite(value, label) for value in values)
    except TypeError as exc:
        raise ValueError(f"{label} must have three finite coordinates") from exc
    if len(result) != 3:
        raise ValueError(f"{label} must have three finite coordinates")
    return result


def _nonnegative(value: float, label: str) -> float:
    result = finite(value, label)
    if result < 0:
        raise ValueError(f"{label} must be nonnegative")
    return result


@dataclass(frozen=True)
class BodyInput:
    """Mass in kg, centre and dimensions in m; zero-mass items are permitted."""
    mass_kg: float
    centre_m: Vector3
    length_m: float
    breadth_m: float
    height_m: float

    def __post_init__(self) -> None:
        for name in ("mass_kg", "length_m", "breadth_m", "height_m"):
            object.__setattr__(self, name, _nonnegative(getattr(self, name), name))
        object.__setattr__(self, "centre_m", _vector(self.centre_m, "centre_m"))


@dataclass(frozen=True)
class MassGroupInput:
    bodies: tuple[BodyInput, ...]

    def __post_init__(self) -> None:
        object.__setattr__(self, "bodies", tuple(self.bodies))
        if not self.bodies or any(not isinstance(b, BodyInput) for b in self.bodies):
            raise ValueError("at least one BodyInput is required")
        try:
            total_mass = math.fsum(b.mass_kg for b in self.bodies)
        except OverflowError as exc:
            raise ValueError("total mass_kg exceeds numeric range") from exc
        positive(total_mass, "total mass_kg")


@dataclass(frozen=True)
class MassCaseInput:
    """Reference is required, even when the desired axis passes through the CG."""
    group: MassGroupInput
    reference_m: Vector3


@dataclass(frozen=True)
class OffsetInput:
    point_m: Vector3
    reference_m: Vector3


@dataclass(frozen=True)
class ParallelAxisInput:
    mass_kg: float
    offset_m: Vector3


@dataclass(frozen=True)
class InertiaInput:
    mass_kg: float
    inertia_kg_m2: Vector3


@dataclass(frozen=True)
class GyradiusEstimateInput:
    length_m: float
    beam_m: float
    roll_beam_ratio_dimensionless: float
    pitch_length_ratio_dimensionless: float
    yaw_length_ratio_dimensionless: float


@dataclass(frozen=True)
class MassProperties:
    mass_kg: float
    centre_m: Vector3
    reference_m: Vector3
    offsets_m: tuple[Vector3, ...]
    parallel_inertias_kg_m2: tuple[Vector3, ...]
    self_inertias_kg_m2: tuple[Vector3, ...]
    parallel_totals_kg_m2: Vector3
    total_inertia_kg_m2: Vector3
    gyradii_m: Vector3


def combined_centre_of_gravity(inputs: MassGroupInput) -> Vector3:
    """Identity c = sum(m*r)/sum(m), m in kg and c in m, common datum.

    Reuse the existing point-mass balance; intrinsic inertia does not change CG.
    The total mass must be positive; individual zero-mass items contribute zero.
    """
    result = gyradius_from_point_masses(
        (b.mass_kg for b in inputs.bodies), (b.centre_m for b in inputs.bodies))
    return result.centroid_m


def coordinate_offset(inputs: OffsetInput) -> Vector3:
    """Identity d = point - reference, both in metres in the same frame."""
    point = _vector(inputs.point_m, "point_m")
    reference = _vector(inputs.reference_m, "reference_m")
    return _vector(tuple(a - b for a, b in zip(point, reference)), "offset_m")


def parallel_axis_inertia(inputs: ParallelAxisInput) -> Vector3:
    """Parallel-axis identity: add m*(dy²+dz²) to Ixx, cyclically, kg m².

    This is the transfer contribution alone; the original axis passes through
    the body's centroid. Axes are parallel; offsets are in metres.
    The result is the diagonal of the full shift tensor returned by
    ``digitalmodel.marine_ops.fowt_quasi_static.basis.parallel_axis``.
    """
    mass = _nonnegative(inputs.mass_kg, "mass_kg")
    x, y, z = _vector(inputs.offset_m, "offset_m")
    if mass == 0:
        return (0.0, 0.0, 0.0)
    try:
        values = (mass * (y*y + z*z), mass * (x*x + z*z), mass * (x*x + y*y))
    except OverflowError as exc:
        raise ValueError("parallel-axis inertia exceeds numeric range") from exc
    return _vector(values, "parallel-axis inertia_kg_m2")


def cuboid_centroidal_inertia(inputs: BodyInput) -> Vector3:
    """Uniform cuboid identity Ixx = m*(breadth²+height²)/12, cyclically.

    The divisor follows from integral(x² dx)/length = length²/12 over a
    centred uniform interval; it is not an empirical input. Return kg m².
    Zero dimensions describe the limiting lamina, rod or point.
    """
    if inputs.mass_kg == 0:
        return (0.0, 0.0, 0.0)
    l, b, h = inputs.length_m, inputs.breadth_m, inputs.height_m
    try:
        values = tuple(inputs.mass_kg * (a*a + c*c) / 12
                       for a, c in ((b, h), (l, h), (l, b)))
    except OverflowError as exc:
        raise ValueError("cuboid inertia exceeds numeric range") from exc
    return _vector(values, "cuboid inertia_kg_m2")


def radius_of_gyration(inputs: InertiaInput) -> Vector3:
    """Identity k = sqrt(I/m), in m, about the supplied inertia axes.

    Reuse only the existing helper's rotational radii; translational estimates
    are not part of this result. Mass must be positive, inertia nonnegative.
    """
    mass = positive(inputs.mass_kg, "mass_kg")
    inertias = _vector(inputs.inertia_kg_m2, "inertia_kg_m2")
    for value in inertias:
        _nonnegative(value, "inertia_kg_m2")
    result = gyradius_from_inertia(mass, *inertias)
    return _vector((result.roll, result.pitch, result.yaw), "gyradii_m")


def estimate_gyradii(inputs: GyradiusEstimateInput) -> Vector3:
    """Caller-selected kx=r_x*beam, ky=r_y*length, kz=r_z*length, in m.

    This is a scaling estimate, not a physical identity or qualified model.
    All dimensionless ratios require an external engineering basis.
    """
    length = _nonnegative(inputs.length_m, "length_m")
    beam = _nonnegative(inputs.beam_m, "beam_m")
    ratios = (inputs.roll_beam_ratio_dimensionless,
              inputs.pitch_length_ratio_dimensionless,
              inputs.yaw_length_ratio_dimensionless)
    values = tuple(_nonnegative(r, "gyradius ratio") * span
                   for r, span in zip(ratios, (beam, length, length)))
    return _vector(values, "estimated gyradii_m")


def calculate_mass_properties(inputs: MassCaseInput) -> MassProperties:
    """Sum intrinsic cuboid and parallel-axis inertias about an explicit datum.

    CG is reported independently of that datum. Radii correspond to the
    supplied axes, which need not pass through the combined CG. No scenario
    inheritance, ingress mass or reference-selection policy is inferred.
    """
    bodies = inputs.group.bodies
    centre = combined_centre_of_gravity(inputs.group)
    reference = _vector(inputs.reference_m, "reference_m")
    offsets = tuple(coordinate_offset(OffsetInput(b.centre_m, reference)) for b in bodies)
    parallel = tuple(parallel_axis_inertia(ParallelAxisInput(b.mass_kg, d))
                     for b, d in zip(bodies, offsets))
    intrinsic = tuple(cuboid_centroidal_inertia(b) for b in bodies)
    try:
        transfers = tuple(math.fsum(row[i] for row in parallel) for i in range(3))
        totals = tuple(math.fsum(row[i] for row in parallel + intrinsic) for i in range(3))
        mass = math.fsum(b.mass_kg for b in bodies)
    except OverflowError as exc:
        raise ValueError("mass properties exceed numeric range") from exc
    radii = radius_of_gyration(InertiaInput(mass, totals))
    return MassProperties(mass, centre, reference, offsets, parallel, intrinsic,
                          _vector(transfers, "parallel totals"), totals, radii)
