"""Submerged hose mass, equivalent buoyancy diameter and fitting transitions.

The method in a legacy engineering workbook is expressed in SI units:

* D_eq = sqrt(sum(L_i D_i**2) / sum(L_i)), preserving displaced volume.
* m_contents = rho_contents * bore_area * L; m_displaced = rho_water *
  envelope_area * L; m_submerged = m_dry + m_contents - m_displaced.
  These are masses in kg, including signed effective submerged mass, not forces.
* The fitting/body factor is I_fitting / I_body for annuli with a common bore.
  Each supplied stiffness is linearly scaled from one to that factor over a
  caller-specified transition fraction. Existing geometry, fluid-mass and
  interpolation implementations are reused.

Inputs: lengths/diameters [m], dry mass [kg], densities [kg/m3], EI and GJ
[N m2], EA [N], and explicitly dimensionless factors/fractions. Convert other
units at the boundary with the repository's existing converters.

Source references: ISO 13628-11, Flexible Pipe Systems for Subsea and Marine
Applications; OCIMF, Guide to Manufacturing and Purchasing Hoses for Offshore
Moorings, GMPHOM 2009. For each: clause not stated in source.
The references are named in the source; formula-specific attribution is not
established. No standard compliance or strength acceptance is calculated.

Limits and Input gaps: the equivalent envelope does not determine drag or
local bend stiffness. Dry mass and both fluid densities must be supplied;
material composition, fluid state and pressure-dependent stiffness remain
external inputs. Uniform scaling of EA and GJ by the bending-inertia ratio
is a source modelling assumption, not a derived constitutive law. Transition
locations must be supplied; their physical basis is not established. Buoy
inertia/added mass, wind factors, layout stationing, model-export commands,
strength checks and dynamic mooring response are left to separate methods.
"""

from __future__ import annotations

from dataclasses import dataclass, fields
import math
import numbers

import numpy as np

from digitalmodel.drilling_riser.section import moment_of_inertia
from digitalmodel.subsea.flexible_pipes.cross_section import (
    FlexiblePipeSection,
    Layer,
    LayerType,
)


def _finite(value: float, name: str, *, positive: bool = False) -> None:
    if isinstance(value, bool) or not isinstance(value, numbers.Real):
        raise ValueError(f"{name} must be a finite number")
    if not math.isfinite(value) or (value <= 0 if positive else value < 0):
        bound = "positive" if positive else "nonnegative"
        raise ValueError(f"{name} must be finite and {bound}")


@dataclass(frozen=True)
class DiameterSegment:
    """One constant equivalent-envelope diameter over a positive length [m]."""

    length_m: float
    buoyancy_diameter_m: float

    def __post_init__(self) -> None:
        for field in fields(self):
            _finite(getattr(self, field.name), field.name, positive=True)


def equivalent_buoyancy_diameter_m(segments: tuple[DiameterSegment, ...]) -> float:
    """Return sqrt(sum(L D**2)/sum(L)) [m]; preserve displaced volume.

    Source standard references and unresolved clause attribution are stated in
    the module docstring. Positive segment lengths and diameters are required.
    """
    if not segments:
        raise ValueError("at least one diameter segment is required")
    # Normalise before squaring to reduce overflow for disparate diameters.
    total_length = math.fsum(segment.length_m for segment in segments)
    largest = max(segment.buoyancy_diameter_m for segment in segments)
    mean_square = math.fsum(
        (segment.length_m / total_length) * (segment.buoyancy_diameter_m / largest)**2
        for segment in segments
    )
    return largest * math.sqrt(mean_square)


@dataclass(frozen=True)
class HoseMassInput:
    """Supplied hose mass/envelope and explicit fluid states, entirely in SI."""

    length_m: float
    bore_diameter_m: float
    buoyancy_diameter_m: float
    dry_mass_kg: float
    water_density_kg_m3: float
    contents_density_kg_m3: float

    def __post_init__(self) -> None:
        for field in fields(self):
            _finite(getattr(self, field.name), field.name,
                    positive=field.name not in {"dry_mass_kg", "contents_density_kg_m3"})
        if self.buoyancy_diameter_m <= self.bore_diameter_m:
            raise ValueError("buoyancy_diameter_m must exceed bore_diameter_m")


@dataclass(frozen=True)
class HoseMassResult:
    """Mass outputs; a negative submerged mass denotes net buoyancy."""

    dry_mass_per_length_kg_m: float
    contents_mass_kg: float
    displaced_mass_kg: float
    submerged_mass_kg: float
    empty_submerged_mass_kg: float


def hose_mass_properties(inputs: HoseMassInput) -> HoseMassResult:
    """Adapt a supplied dry mass to existing pipe content/displacement kernels.

    A zero-density surrogate layer provides only the bore and equivalent
    buoyancy envelope; it does not infer material density or structural layers.
    Fluid masses use the existing cross-section implementation. Effective mass
    is m_dry + m_contents - m_displaced; no gravity is applied.
    """
    envelope = FlexiblePipeSection([Layer(
        layer_type=LayerType.OUTER_SHEATH,
        inner_diameter=inputs.bore_diameter_m,
        thickness=(inputs.buoyancy_diameter_m - inputs.bore_diameter_m) / 2,
        density=0.0,
    )])
    contents = envelope.content_mass_per_length(inputs.contents_density_kg_m3) * inputs.length_m
    displaced = envelope.displaced_mass_per_length(inputs.water_density_kg_m3) * inputs.length_m
    empty = inputs.dry_mass_kg - displaced
    return HoseMassResult(
        dry_mass_per_length_kg_m=inputs.dry_mass_kg / inputs.length_m,
        contents_mass_kg=contents,
        displaced_mass_kg=displaced,
        submerged_mass_kg=inputs.dry_mass_kg + contents - displaced,
        empty_submerged_mass_kg=empty,
    )


@dataclass(frozen=True)
class HoseStiffness:
    """Supplied nonnegative EI, EA and GJ; pressure state is caller-selected."""

    bending_stiffness_n_m2: float
    axial_stiffness_n: float
    torsional_stiffness_n_m2: float

    def __post_init__(self) -> None:
        for field in fields(self):
            _finite(getattr(self, field.name), field.name)


def flange_stiffness_ratio(
    flange_diameter_m: float, body_diameter_m: float, bore_diameter_m: float,
) -> float:
    """Return (D_flange**4 - d**4)/(D_body**4 - d**4) via existing annular I.

    A common bore and unchanged effective modulus are assumed. This factor
    determines bending scaling only; extending it to EA/GJ requires the source
    modelling assumption described in the module Input gaps.
    """
    for name, value in (("flange_diameter_m", flange_diameter_m),
                        ("body_diameter_m", body_diameter_m),
                        ("bore_diameter_m", bore_diameter_m)):
        _finite(value, name, positive=True)
    if min(flange_diameter_m, body_diameter_m) <= bore_diameter_m:
        raise ValueError("body and flange diameters must exceed bore_diameter_m")
    return (moment_of_inertia(flange_diameter_m, bore_diameter_m)
            / moment_of_inertia(body_diameter_m, bore_diameter_m))


def flange_transition_stiffness(
    base: HoseStiffness, ratio_dimensionless: float,
    station_fraction_dimensionless: float,
) -> HoseStiffness:
    """Interpolate each stiffness from base to base*ratio without extrapolation.

    fraction=0 returns the body; fraction=1 returns the fitting. The caller
    supplies each transition station. Uniform EA/GJ scaling is an unqualified
    source assumption, as documented under Input gaps; it is not a strength check.
    """
    _finite(ratio_dimensionless, "ratio_dimensionless", positive=True)
    _finite(station_fraction_dimensionless, "station_fraction_dimensionless")
    if station_fraction_dimensionless > 1:
        raise ValueError("station_fraction_dimensionless must be in [0, 1]")
    return HoseStiffness(*(
        float(np.interp(station_fraction_dimensionless, (0.0, 1.0),
                        (getattr(base, field.name), getattr(base, field.name) * ratio_dimensionless)))
        for field in fields(base)
    ))
