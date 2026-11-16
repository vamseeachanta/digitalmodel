"""Axial rope response, tensile-test inference and equal-strain length changes.

Inputs and outputs use SI: lengths m, forces and axial rigidity EA N, area m²,
modulus Pa, and strain as a dimensionless ratio. Inputs are frozen dataclasses.
Engineering strain and force/strain rigidity reuse existing repository functions;
exact boundary conversions use the shared unit registry. No material defaults apply.

Method
------
Elastic extension is T L / EA; line stiffness is EA/L. A test point gives
measured strain = extension/reference length, corrected strain = measured minus
supplied residual strain, EA = T/corrected strain, and E = EA/area. An interval
gives EA = delta(T) gauge_length/delta(extension). The caller chooses whether
the gauge length is the original or current length; those conventions differ.

For a leg with vertical V, horizontal H and lateral Y offsets, L = norm(V,H,Y)
and displaced length Lp = norm(V+z,H,Y). To match reference strain e = z/Vref,
the adjusted rest length is Lp/(1+e); shortening is L - adjusted rest length.
Alternatively, keeping the inclined leg unchanged, its strain q = (Lp-L)/L
requires reference extension zref = (Vref+z) q/(1+q), and reference added length
z-zref. A negative shortening or added length denotes the opposite adjustment.

Standards context as stated in the source
-----------------------------------------
"ABS Guidance Notes, THE APPLICATION OF FIBER ROPE FOR OFFSHORE MOORING (2014)"
and "ISO 18692": clause not stated in source.
Neither standard is applied by the equations in this module.
No standard clause is attached to the geometric derivation or
test-point equations. These calculations do not establish standards compliance.

Limits and Input gaps
---------------------
Reference length, effective load-bearing area, residual strain and EA require
qualified caller inputs without defaults. Missing standard clauses and the
reference-length convention remain input gaps. Only positive tensile rigidity
is inferred; unloading intervals require force and extension changes of the
same sign. Zero elastic load and zero geometric displacement are supported.

Geometry assumes fixed horizontal offsets and a prescribed vertical relative
displacement. It does not solve force equilibrium, rotations, slack legs,
dynamics or manufacturing tolerances. Equal strain means equal tension only
for identical EA. Creep, rupture life, catalogue ratings, empirical material
fits and tenacity conversions are left outside this module. No acceptance
criterion or material coefficient is inferred from the source.

Inputs and intermediate quantities, including unit conversions, must be
representable as finite floats. Extreme scales may raise ValueError even
where an algebraically simplified final result would be representable.
"""

from __future__ import annotations

from dataclasses import dataclass
import math
from numbers import Real
from types import SimpleNamespace

from digitalmodel.marine_ops.installation.jumper_lift import (
    GRAVITY_M_S2,
    compute_sling_stiffness,
)
from digitalmodel.marine_ops.marine_analysis.catenary.utils import calculate_elongation
from digitalmodel.naval_architecture.fundamentals import engineering_strain
from digitalmodel.units import Q_


def _number(name: str, value: float, *, positive: bool = False,
            nonnegative: bool = False) -> float:
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{name} must be a real finite scalar")
    try:
        result = float(value)
    except OverflowError as exc:
        raise ValueError(f"{name} is not representable") from exc
    if not math.isfinite(result):
        raise ValueError(f"{name} must be finite")
    if positive and result <= 0:
        raise ValueError(f"{name} must be positive")
    if nonnegative and result < 0:
        raise ValueError(f"{name} must be nonnegative")
    return result


def strain_from_extension(reference_length_m: float, extension_m: float) -> float:
    """Adapt SI lengths to the existing engineering-strain function (dL/L)."""
    length = _number("reference_length_m", reference_length_m, positive=True)
    extension = _number("extension_m", extension_m)
    length_ft = Q_(length, "m").to("ft").magnitude
    extension_ft = Q_(extension, "m").to("ft").magnitude
    return _number("strain_ratio", engineering_strain(length_ft, extension_ft))


def rigidity_from_force_strain(force_n: float, strain_ratio: float) -> float:
    """Adapt EA = force/strain to existing sling stiffness, with no defaults.

    The existing routine expects tonne-force and returns kN. Its own exported
    gravity is used solely to reverse that boundary convention; gravity cancels
    algebraically. No mass or site gravity is a physical input to this function.
    If compute_sling_stiffness ever applies its own defaults or validation to
    MBL or elongation, this adapter must be revisited.
    """
    force = _number("force_n", force_n, positive=True)
    strain = _number("strain_ratio", strain_ratio, positive=True)
    force_kn = Q_(force, "N").to("kN").magnitude
    equivalent_mass = _number("equivalent_mass_t", force_kn / GRAVITY_M_S2, positive=True)
    data = SimpleNamespace(sling_mbl_te=equivalent_mass, sling_elongation=strain)
    rigidity_kn = compute_sling_stiffness(data)["sling_ea_kn"]
    return _number("axial_rigidity_n", Q_(rigidity_kn, "kN").to("N").magnitude,
                   positive=True)


@dataclass(frozen=True, slots=True)
class ElasticRope:
    """Uniform linear elastic rope under nonnegative tension (SI)."""

    length_m: float
    axial_rigidity_n: float
    tension_n: float

    def __post_init__(self) -> None:
        _number("length_m", self.length_m, positive=True)
        _number("axial_rigidity_n", self.axial_rigidity_n, positive=True)
        _number("tension_n", self.tension_n, nonnegative=True)


@dataclass(frozen=True, slots=True)
class ElasticResponse:
    """Line stiffness, strain and extension, all in SI."""

    line_stiffness_n_per_m: float
    strain_ratio: float
    extension_m: float


def elastic_response(rope: ElasticRope) -> ElasticResponse:
    """Compute k=EA/L, strain=T/EA and dL=T L/EA without material factors."""
    stiffness = _number("line_stiffness_n_per_m", rope.axial_rigidity_n / rope.length_m,
                        positive=True)
    strain = _number("strain_ratio", rope.tension_n / rope.axial_rigidity_n,
                     positive=rope.tension_n > 0)
    extension = _number(
        "extension_m",
        calculate_elongation(rope.tension_n, rope.length_m, rope.axial_rigidity_n),
        positive=rope.tension_n > 0,
    )
    return ElasticResponse(stiffness, strain, extension)


@dataclass(frozen=True, slots=True)
class TensilePoint:
    """Load/extension reading with an explicit reference and residual strain."""

    reference_length_m: float
    extension_m: float
    tension_n: float
    area_m2: float
    residual_strain_ratio: float

    def __post_init__(self) -> None:
        for name in ("reference_length_m", "extension_m", "tension_n", "area_m2"):
            _number(name, getattr(self, name), positive=True)
        _number("residual_strain_ratio", self.residual_strain_ratio, nonnegative=True)


@dataclass(frozen=True, slots=True)
class TensileProperties:
    """Measured/corrected strain, EA in N and apparent modulus in Pa."""

    measured_strain_ratio: float
    elastic_strain_ratio: float
    axial_rigidity_n: float
    modulus_pa: float


def tensile_properties(point: TensilePoint) -> TensileProperties:
    """Infer EA=T/(dL/L-residual), E=EA/A; the area basis is caller-supplied."""
    if point.extension_m <= point.residual_strain_ratio * point.reference_length_m:
        raise ValueError("elastic extension must exceed residual extension")
    measured = strain_from_extension(point.reference_length_m, point.extension_m)
    corrected = _number("elastic_strain_ratio", measured - point.residual_strain_ratio,
                        positive=True)
    rigidity = rigidity_from_force_strain(point.tension_n, corrected)
    modulus = _number("modulus_pa", rigidity / point.area_m2, positive=True)
    return TensileProperties(measured, corrected, rigidity, modulus)


def incremental_rigidity(force_increment_n: float, extension_increment_m: float,
                         gauge_length_m: float) -> float:
    """Compute EA=delta(T) Lg/delta(L) on loading or unloading intervals."""
    force = _number("force_increment_n", force_increment_n)
    extension = _number("extension_increment_m", extension_increment_m)
    length = _number("gauge_length_m", gauge_length_m, positive=True)
    if force == 0 or extension == 0 or (force > 0) != (extension > 0):
        raise ValueError("force and extension increments must have the same nonzero sign")
    strain = strain_from_extension(length, abs(extension))
    return rigidity_from_force_strain(abs(force), strain)


@dataclass(frozen=True, slots=True)
class SlingGeometry:
    """Unloaded endpoint offsets in m; horizontal and lateral offsets are signed."""

    vertical_m: float
    horizontal_m: float
    lateral_m: float

    def __post_init__(self) -> None:
        _number("vertical_m", self.vertical_m, positive=True)
        _number("horizontal_m", self.horizontal_m)
        _number("lateral_m", self.lateral_m)


@dataclass(frozen=True, slots=True)
class EqualStrainInputs:
    """Reference vertical length, imposed displacement and one inclined leg."""

    reference_vertical_m: float
    vertical_displacement_m: float
    leg: SlingGeometry

    def __post_init__(self) -> None:
        _number("reference_vertical_m", self.reference_vertical_m, positive=True)
        _number("vertical_displacement_m", self.vertical_displacement_m, nonnegative=True)
        if not isinstance(self.leg, SlingGeometry):
            raise ValueError("leg must be SlingGeometry")


@dataclass(frozen=True, slots=True)
class EqualStrainAdjustment:
    """Two alternative length adjustments at the same prescribed displacement."""

    original_length_m: float
    displaced_length_m: float
    unadjusted_extension_m: float
    unadjusted_strain_ratio: float
    adjusted_length_m: float
    shortening_m: float
    adjusted_extension_m: float
    adjusted_strain_ratio: float
    reference_added_length_m: float
    reference_extension_m: float


def equal_strain_adjustment(data: EqualStrainInputs) -> EqualStrainAdjustment:
    """Match reference strain by shortening a leg, or by lengthening reference.

    Uses a rationalized length difference to retain extension for very small z.
    The two alternatives are separate configurations, not simultaneous edits.
    """
    leg = data.leg
    z = data.vertical_displacement_m
    moved_vertical = _number("displaced_vertical_m", leg.vertical_m + z, positive=True)
    original = _number("original_length_m", math.hypot(
        leg.vertical_m, leg.horizontal_m, leg.lateral_m), positive=True)
    displaced = _number("displaced_length_m", math.hypot(
        moved_vertical, leg.horizontal_m, leg.lateral_m), positive=True)
    extension = z * (leg.vertical_m / displaced + moved_vertical / displaced) / (
        1 + original / displaced)
    extension = _number("unadjusted_extension_m", extension, positive=z > 0)
    unadjusted_strain = strain_from_extension(original, extension)
    target = strain_from_extension(data.reference_vertical_m, z)
    adjusted = _number("adjusted_length_m", displaced / (1 + target), positive=True)
    shortening = _number("shortening_m", (target * original - extension) / (1 + target))
    adjusted_extension = _number("adjusted_extension_m", target * adjusted, positive=z > 0)
    reference_extension = _number("reference_extension_m",
        (data.reference_vertical_m + z) * (unadjusted_strain / (1 + unadjusted_strain)),
        positive=z > 0)
    reference_added = _number("reference_added_length_m", z - reference_extension)
    return EqualStrainAdjustment(
        original, displaced, extension, unadjusted_strain, adjusted, shortening,
        adjusted_extension, target, reference_added, reference_extension,
    )
