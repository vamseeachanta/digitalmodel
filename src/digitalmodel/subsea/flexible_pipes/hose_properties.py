"""Effective hose geometry, reference-property transfer and hydrostatic sizing.

These methods use SI throughout: lengths in m, linear masses in kg/m, total
masses in kg, density in kg/m³, forces in N and rotational rigidities in N·m².
Records are frozen; empirical inputs have no defaults. Existing PipeSection
geometry supplies annular area and second moment.

The equivalent buoyancy diameter preserves volume, using a length-weighted mean
of squared diameters. It does not establish an equivalent drag diameter. Property
transfer assumes equal effective material response: force scales with annular
area, while rotational rigidity uses an explicitly selected geometric basis.
Selecting area for rotational rigidity is a simplifying assumption, not
demonstrated for bending or torsion of a different hose.

Buoyancy reserve is additional displaced mass divided by loaded mass. Circular
diameter sizing uses pi*D²/4. Inverse component buoyancy assumes a uniform flooded
annulus, with total displaced mass rho*pi*(OD²-ID²)*L/4. These definitions retain
the circular-area factor and component length.
Apparent submerged mass means signed submerged force divided by gravity.

Standards named in the source: ISO 13628-11; OCIMF – Guide to Manufacturing and
Purchasing Hoses for Offshore Moorings, GMPHOM 2009. For both, clause not stated
in source. Ancillary procurement references are ASME B1.1, ASME B18.2.2 and
ASME B16.5; clause not stated in source. No equation is attributed to a clause
and no compliance verdict is computed here.

Input gaps: independently sourced density, reserve requirement, measured force
and rigidity references, pressure state and justification for transferring
properties between constructions. The geometric basis for rotational scaling
requires an explicit choice. Standard clauses and the physical justification
for treating a component as a uniform flooded annulus are not established.

Limits: positive finite geometry and references; a nonzero bore strictly below
OD; nonnegative linear masses/reserve and positive loaded mass. Unrepresentable
areas, moments or calculated results raise ValueError. Construction mechanics,
strength acceptance, drag, catenary response, connection layout, solver exports
and bills of material remain outside these methods.
"""

from __future__ import annotations

from dataclasses import dataclass
import math
from numbers import Real
from typing import Iterable, Literal

from digitalmodel.subsea.connectors.section_properties import PipeSection


def _number(value: float, name: str, *, positive: bool = False) -> None:
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{name} must be a finite number")
    try:
        valid = math.isfinite(value)
    except OverflowError:
        valid = False
    if not valid or (positive and value <= 0):
        raise ValueError(f"{name} must be finite" + (" and positive" if positive else ""))


def _positive_result(value: float) -> float:
    _number(value, "calculated result", positive=True)
    return value


@dataclass(frozen=True)
class HoseSegment:
    """Uniform cylindrical envelope; positive length and diameter in metres."""

    length_m: float
    outer_diameter_m: float

    def __post_init__(self) -> None:
        _number(self.length_m, "length_m", positive=True)
        _number(self.outer_diameter_m, "outer_diameter_m", positive=True)


@dataclass(frozen=True)
class AnnularSection:
    """Unit-labelled adapter to the existing PipeSection geometry implementation."""

    outer_diameter_m: float
    inner_diameter_m: float

    def __post_init__(self) -> None:
        _number(self.outer_diameter_m, "outer_diameter_m", positive=True)
        _number(self.inner_diameter_m, "inner_diameter_m", positive=True)
        if self.inner_diameter_m >= self.outer_diameter_m:
            raise ValueError("inner diameter must be below outer diameter")
        try:
            _positive_result(self.pipe_section.area)
            _positive_result(self.pipe_section.moment_of_inertia)
        except OverflowError as exc:
            raise ValueError("section properties exceed representable range") from exc

    @property
    def pipe_section(self) -> PipeSection:
        """Return the existing geometry API; dimensions remain in metres."""
        return PipeSection(
            self.outer_diameter_m,
            (self.outer_diameter_m - self.inner_diameter_m) / 2,
        )


@dataclass(frozen=True)
class ForceReference:
    """Measured axial rigidity or breaking force in N at a reference section."""

    value_n: float
    section: AnnularSection

    def __post_init__(self) -> None:
        _number(self.value_n, "value_n", positive=True)
        if not isinstance(self.section, AnnularSection):
            raise ValueError("section must be an AnnularSection")


@dataclass(frozen=True)
class RigidityReference:
    """Measured bending or torsional rigidity in N·m² at a reference section."""

    value_n_m2: float
    section: AnnularSection

    def __post_init__(self) -> None:
        _number(self.value_n_m2, "value_n_m2", positive=True)
        if not isinstance(self.section, AnnularSection):
            raise ValueError("section must be an AnnularSection")


def equivalent_buoyancy_diameter_m(segments: Iterable[HoseSegment]) -> float:
    """Return sqrt(sum(L*D²)/sum(L)); preserves cylindrical displacement volume.

    Blank portions and zero-length placeholders must be removed by the caller;
    HoseSegment rejects nonpositive lengths. This is volume averaging, not a
    taper model or a hydrodynamic drag calculation. The iterable is read once.
    """
    segments = tuple(segments)
    if not segments or any(not isinstance(s, HoseSegment) for s in segments):
        raise ValueError("at least one HoseSegment is required")
    try:
        length = math.fsum(s.length_m for s in segments)
        squared = math.fsum(s.length_m * s.outer_diameter_m**2 for s in segments)
        _positive_result(length)
        return _positive_result(math.sqrt(_positive_result(squared) / length))
    except OverflowError as exc:
        raise ValueError("equivalent diameter exceeds representable range") from exc


def scale_force_property_n(reference: ForceReference, target: AnnularSection) -> float:
    """Return reference force * A_target/A_reference, using existing annular areas.

    This transfers either EA or a breaking-force reference; it does not compute
    a strength allowable or establish equivalent construction behavior.
    """
    if not isinstance(reference, ForceReference) or not isinstance(target, AnnularSection):
        raise ValueError("reference and target must be ForceReference and AnnularSection")
    ratio = target.pipe_section.area / reference.section.pipe_section.area
    return _positive_result(reference.value_n * ratio)


def scale_rotational_rigidity_n_m2(
    reference: RigidityReference,
    target: AnnularSection,
    *,
    basis: Literal["area", "second_moment"],
) -> float:
    """Scale a reference EI or GJ by A/A_ref or I/I_ref, as explicitly requested.

    The second-moment ratio is (OD⁴-ID⁴)/(OD_ref⁴-ID_ref⁴); the polar-moment
    multiplier cancels for circular torsional sections. Area scales with the
    squared diameters instead. No basis is inferred from the property name.
    """
    if not isinstance(reference, RigidityReference) or not isinstance(target, AnnularSection):
        raise ValueError("reference and target must be RigidityReference and AnnularSection")
    if basis not in ("area", "second_moment"):
        raise ValueError("basis must be 'area' or 'second_moment'")
    attribute = "area" if basis == "area" else "moment_of_inertia"
    ratio = getattr(target.pipe_section, attribute) / getattr(reference.section.pipe_section, attribute)
    return _positive_result(reference.value_n_m2 * ratio)


@dataclass(frozen=True)
class BuoyancyRequirement:
    """Linear loaded masses, external fluid density and reserve/loaded-mass ratio."""

    dry_mass_per_length_kg_m: float
    content_mass_per_length_kg_m: float
    fluid_density_kg_m3: float
    reserve_ratio_1: float

    def __post_init__(self) -> None:
        for name in ("dry_mass_per_length_kg_m", "content_mass_per_length_kg_m", "reserve_ratio_1"):
            value = getattr(self, name)
            _number(value, name)
            if value < 0:
                raise ValueError(f"{name} must be nonnegative")
        _number(self.fluid_density_kg_m3, "fluid_density_kg_m3", positive=True)
        if self.dry_mass_per_length_kg_m == self.content_mass_per_length_kg_m == 0:
            raise ValueError("loaded mass must be positive")


def required_displaced_mass_kg_m(inputs: BuoyancyRequirement) -> float:
    """Return (1+reserve_ratio)*(dry mass + content mass) in kg/m."""
    loaded = inputs.dry_mass_per_length_kg_m + inputs.content_mass_per_length_kg_m
    return _positive_result((1 + inputs.reserve_ratio_1) * loaded)


def required_buoyancy_diameter_m(inputs: BuoyancyRequirement) -> float:
    """Solve rho*pi*D²/4 = required displaced mass per length for circular D.

    Delegate to hydrodynamic_od_m after validating positive finite volume;
    its nonpositive-volume clamp is excluded by this adapter.
    """
    from digitalmodel.drilling_riser.global_model.build import hydrodynamic_od_m

    mass = required_displaced_mass_kg_m(inputs)
    volume = _positive_result(mass / inputs.fluid_density_kg_m3)
    return _positive_result(hydrodynamic_od_m(volume))


@dataclass(frozen=True)
class ImmersedComponent:
    """Uniform flooded annulus: total masses and external fluid density, in SI.

    Apparent mass is signed submerged force divided by gravity. The annulus
    assumption excludes closed voids, attached floats and varying cross-sections.
    """

    outer_diameter_m: float
    length_m: float
    mass_in_air_kg: float
    apparent_mass_in_fluid_kg: float
    fluid_density_kg_m3: float

    def __post_init__(self) -> None:
        for name in ("outer_diameter_m", "length_m", "mass_in_air_kg", "fluid_density_kg_m3"):
            _number(getattr(self, name), name, positive=True)
        _number(self.apparent_mass_in_fluid_kg, "apparent_mass_in_fluid_kg")
        if self.apparent_mass_in_fluid_kg >= self.mass_in_air_kg:
            raise ValueError("displaced mass must be positive")


def equivalent_annular_bore_m(inputs: ImmersedComponent) -> float:
    """Solve ID² = OD² - 4*(mass_air-mass_submerged)/(rho*pi*L).

    Length is required because the masses are totals. A solved bore outside
    (0, OD) is incompatible with the assumed flooded annular geometry.
    """
    try:
        displaced_mass = inputs.mass_in_air_kg - inputs.apparent_mass_in_fluid_kg
        _positive_result(displaced_mass)
        outer_squared = _positive_result(inputs.outer_diameter_m**2)
        denominator = _positive_result(inputs.fluid_density_kg_m3 * math.pi * inputs.length_m)
        bore_squared = outer_squared - 4 * displaced_mass / denominator
        _positive_result(bore_squared)
        # Unreachable in exact arithmetic for positive displaced mass; floating-point rounding can reach it.
        if bore_squared >= outer_squared:
            raise ValueError("displacement cannot be resolved within outer diameter")
        bore = _positive_result(math.sqrt(bore_squared))
        # Unreachable in exact arithmetic; the square root can round up to the outer diameter.
        if bore >= inputs.outer_diameter_m:
            raise ValueError("bore cannot be resolved below outer diameter")
        return bore
    except OverflowError as exc:
        raise ValueError("equivalent bore exceeds representable range") from exc
