"""Equivalent hose geometry, reference-property scaling and buoyancy masses.

Method reconstructed from a legacy hose-property calculation. Inputs and outputs
are SI: lengths/diameters in m, masses in kg, density in kg/m^3, axial
stiffness/strength in N, and bending/torsional stiffness in N m^2.
``feet_to_metres`` uses the exact international foot definition, 0.3048 m;
``millimetres_to_metres`` uses 1000 mm/m. Convert kN to N and kN m^2 to
N m^2 by multiplying by 1000 at the caller boundary.

The equivalent buoyancy diameter is sqrt(sum(L_i D_i^2) / sum(L_i)).
Reference bending/torsional properties scale with (D^4-d^4), and axial
properties/breaking strength scale with (D^2-d^2). The ratios presume the
reference effective material behavior is transferable. They do not derive
constitutive properties from construction or establish a strength criterion.
Fluid and displaced masses are rho pi D^2 L / 4. Submerged mass is
dry mass + contained-fluid mass - displaced-fluid mass. Float dry mass is
displaced-fluid mass - net buoyancy expressed as a mass-equivalent.

References as stated in the source:
* ISO 13628-11 – Flexible Pipe Systems for Subsea and Marine Applications;
  clause not stated in source.
* OCIMF – Guide to Manufacturing and Purchasing Hoses for Offshore Moorings,
  GMPHOM 2009; clause not stated in source.
These are source reference labels, not verified formula attributions or
claims of compliance. No standard-derived acceptance constants are embedded.

Limits and Input gaps:
* Reference properties, bore/outer diameters, fluid densities, float volume,
  net buoyancy and bend-radius ratio require independently qualified inputs.
  All are required arguments; no empirical or material defaults are supplied.
* Source diameter choices and some length/radius unit labels are inconsistent.
  Callers must resolve their basis before engineering use.
* Volume-equivalent diameter conserves displacement for piecewise cylinders;
  its use as an equivalent drag diameter is not established.
* Buoyancy is a mass-equivalent, not a force; no gravity is assumed. Negative
  submerged mass denotes net uplift, not an error.
* The source does not establish stiffness scaling validity for different hose
  constructions, pressure states or reinforcement. Each reference property
  must already represent the intended state.
* Uniform contained-fluid and external-fluid densities and full submergence
  are assumed. End hardware must be included in the supplied dry mass/basis.
* Mooring equilibrium, anchor layout, model commands, pressure/collapse
  checks, fatigue and dynamic response are left outside this module.
"""

from __future__ import annotations

from dataclasses import dataclass, fields
from math import fsum, isfinite, sqrt
from numbers import Real
from typing import Sequence

from digitalmodel.cad.units import METRES_PER_UNIT
from digitalmodel.installation.pipelay.integrity import displaced_weight_per_length


def _finite(value: float, name: str) -> None:
    if isinstance(value, bool) or not isinstance(value, Real) or not isfinite(value):
        raise ValueError(f"{name} must be a finite real number")


def _nonnegative(value: float, name: str) -> None:
    _finite(value, name)
    if value < 0:
        raise ValueError(f"{name} must be nonnegative")


def _positive(value: float, name: str) -> None:
    _finite(value, name)
    if value <= 0:
        raise ValueError(f"{name} must be positive")


@dataclass(frozen=True)
class DiameterSegment:
    """One constant-diameter portion of a buoyancy profile, in metres."""

    length_m: float
    outer_diameter_m: float

    def __post_init__(self) -> None:
        _positive(self.length_m, "length_m")
        _nonnegative(self.outer_diameter_m, "outer_diameter_m")


@dataclass(frozen=True)
class AnnularGeometry:
    """Circular outer and bore diameters in m; a zero bore is permitted."""

    outer_diameter_m: float
    inner_diameter_m: float

    def __post_init__(self) -> None:
        _positive(self.outer_diameter_m, "outer_diameter_m")
        _nonnegative(self.inner_diameter_m, "inner_diameter_m")
        if self.inner_diameter_m >= self.outer_diameter_m:
            raise ValueError("inner_diameter_m must be smaller than outer_diameter_m")


@dataclass(frozen=True)
class HoseReferenceProperties:
    """Effective properties for a specified geometry and supplied pressure states."""

    geometry: AnnularGeometry
    bending_empty_n_m2: float
    bending_pressurized_n_m2: float
    torsional_n_m2: float
    axial_empty_n: float
    axial_pressurized_n: float
    breaking_strength_n: float

    def __post_init__(self) -> None:
        if not isinstance(self.geometry, AnnularGeometry):
            raise ValueError("geometry must be AnnularGeometry")
        for field in fields(self):
            if field.name != "geometry":
                _positive(getattr(self, field.name), field.name)


@dataclass(frozen=True)
class BendRadiusInput:
    """Selected diameter in m and a required, dimensionless radius multiplier."""

    diameter_m: float
    radius_to_diameter_dimensionless: float

    def __post_init__(self) -> None:
        _positive(self.diameter_m, "diameter_m")
        _positive(self.radius_to_diameter_dimensionless, "radius_to_diameter_dimensionless")


@dataclass(frozen=True)
class SubmergedHoseInput:
    """Dry assembly mass, full length, effective geometry and two fluid densities."""

    length_m: float
    dry_mass_kg: float
    geometry: AnnularGeometry
    internal_density_kg_m3: float
    external_density_kg_m3: float

    def __post_init__(self) -> None:
        _positive(self.length_m, "length_m")
        _nonnegative(self.dry_mass_kg, "dry_mass_kg")
        if not isinstance(self.geometry, AnnularGeometry):
            raise ValueError("geometry must be AnnularGeometry")
        _nonnegative(self.internal_density_kg_m3, "internal_density_kg_m3")
        _nonnegative(self.external_density_kg_m3, "external_density_kg_m3")


@dataclass(frozen=True)
class SubmergedHoseMass:
    """Mass quantities in kg, except dry mass per unit length in kg/m."""

    dry_mass_per_length_kg_m: float
    internal_fluid_mass_kg: float
    displaced_fluid_mass_kg: float
    submerged_mass_kg: float


@dataclass(frozen=True)
class FloatBuoyancyInput:
    """Displaced volume, signed net buoyancy mass-equivalent and water density."""

    volume_m3: float
    net_buoyancy_mass_kg: float
    water_density_kg_m3: float

    def __post_init__(self) -> None:
        _positive(self.volume_m3, "volume_m3")
        _finite(self.net_buoyancy_mass_kg, "net_buoyancy_mass_kg")
        _positive(self.water_density_kg_m3, "water_density_kg_m3")
        displaced = self.volume_m3 * self.water_density_kg_m3
        _finite(displaced, "displaced_fluid_mass_kg")
        if self.net_buoyancy_mass_kg > displaced:
            raise ValueError("net buoyancy would imply a negative dry mass")


@dataclass(frozen=True)
class FloatMass:
    """Displaced-fluid and inferred dry masses in kg."""

    displaced_fluid_mass_kg: float
    dry_mass_kg: float


def feet_to_metres(length_ft: float) -> float:
    """Convert a nonnegative length using exactly 0.3048 m per foot."""
    _nonnegative(length_ft, "length_ft")
    return length_ft * METRES_PER_UNIT["ft"]


def millimetres_to_metres(diameter_mm: float) -> float:
    """Convert a nonnegative diameter using exactly 1000 mm per metre."""
    _nonnegative(diameter_mm, "diameter_mm")
    return diameter_mm * METRES_PER_UNIT["mm"]


def total_hose_length_m(segments: Sequence[DiameterSegment]) -> float:
    """Return sum(L_i) in m for a nonempty piecewise profile."""
    if not segments:
        raise ValueError("at least one diameter segment is required")
    if any(not isinstance(segment, DiameterSegment) for segment in segments):
        raise ValueError("segments must contain DiameterSegment records")
    total = fsum(segment.length_m for segment in segments)
    _positive(total, "total_hose_length_m")
    return total


def equivalent_buoyancy_diameter_m(segments: Sequence[DiameterSegment]) -> float:
    """Return sqrt(sum(L_i D_i^2)/sum(L_i)) in m, conserving cylinder volume."""
    total = total_hose_length_m(segments)
    diameter = sqrt(fsum(s.length_m / total * s.outer_diameter_m**2 for s in segments))
    _nonnegative(diameter, "equivalent_buoyancy_diameter_m")
    return diameter


def _annular_power(geometry: AnnularGeometry, power: int) -> float:
    if power not in (2, 4):
        raise ValueError("power must be 2 or 4")
    outer, inner = geometry.outer_diameter_m, geometry.inner_diameter_m
    # Factoring avoids subtracting nearly equal squared/fourth-power terms.
    area_term = (outer - inner) * (outer + inner)
    return area_term if power == 2 else area_term * (outer**2 + inner**2)


def scale_hose_properties(
    reference: HoseReferenceProperties, geometry: AnnularGeometry
) -> HoseReferenceProperties:
    """Scale bending/torsion by annular fourth powers; axial/strength by squares.

    The common pi/64, pi/32 and pi/4 factors cancel in the respective
    section-property ratios. This is a reference scaling assumption, not
    a new material model or a standard acceptance check.
    """
    if not isinstance(geometry, AnnularGeometry):
        raise ValueError("geometry must be AnnularGeometry")
    area_ratio = _annular_power(geometry, 2) / _annular_power(reference.geometry, 2)
    inertia_ratio = _annular_power(geometry, 4) / _annular_power(reference.geometry, 4)
    return HoseReferenceProperties(
        geometry=geometry,
        bending_empty_n_m2=reference.bending_empty_n_m2 * inertia_ratio,
        bending_pressurized_n_m2=reference.bending_pressurized_n_m2 * inertia_ratio,
        torsional_n_m2=reference.torsional_n_m2 * inertia_ratio,
        axial_empty_n=reference.axial_empty_n * area_ratio,
        axial_pressurized_n=reference.axial_pressurized_n * area_ratio,
        breaking_strength_n=reference.breaking_strength_n * area_ratio,
    )


def minimum_bend_radius_m(inputs: BendRadiusInput) -> float:
    """Return selected diameter times the supplied radius/diameter ratio, in m."""
    radius = inputs.diameter_m * inputs.radius_to_diameter_dimensionless
    _positive(radius, "minimum_bend_radius_m")
    return radius


def compute_submerged_hose_mass(inputs: SubmergedHoseInput) -> SubmergedHoseMass:
    """Adapt circular displacement to contents/external masses and their balance."""
    geometry = inputs.geometry
    # A unit gravity factor returns mass per length for either fluid envelope.
    fluid = displaced_weight_per_length(
        geometry.inner_diameter_m, inputs.internal_density_kg_m3, gravity=1.0
    ) * inputs.length_m
    displaced = displaced_weight_per_length(
        geometry.outer_diameter_m, inputs.external_density_kg_m3, gravity=1.0
    ) * inputs.length_m
    result = SubmergedHoseMass(
        dry_mass_per_length_kg_m=inputs.dry_mass_kg / inputs.length_m,
        internal_fluid_mass_kg=fluid,
        displaced_fluid_mass_kg=displaced,
        submerged_mass_kg=inputs.dry_mass_kg + fluid - displaced,
    )
    for field in fields(result):
        _finite(getattr(result, field.name), field.name)
    return result


def compute_float_mass(inputs: FloatBuoyancyInput) -> FloatMass:
    """Return rho*V and rho*V - net buoyancy mass-equivalent, both in kg."""
    displaced = inputs.water_density_kg_m3 * inputs.volume_m3
    dry = displaced - inputs.net_buoyancy_mass_kg
    _finite(dry, "dry_mass_kg")
    return FloatMass(displaced, dry)
