# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Umbilical build-up and supplied-rating screening (SI units).

API 17E / ISO 13628-5 scope: cross-section and pressure performance.
Clause-level mapping is NOT_EVALUATED: no accessible, verified clause text
was available. This module does not implement a standard pressure-rating rule
or establish compliance. Supplier-qualified differential limits, including any
temperature, ageing and safety allowances, must be supplied for each element.
No steel formula is applied to thermoplastic hose.

Equation basis (public textbook, not standard acceptance criteria):
OpenStax, University Physics Volume 1 (2016), sections 14.1 and 14.4:
https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure
https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy

For straight elements and concentric layers, circle geometry gives
A = pi*D**2/4, A_wall = pi*(Do**2-Di**2)/4, D_next = D+2*t.
The factors 2 and 4 are geometric identities, not empirical coefficients.
Mass per axial metre is sum(rho*A); contents and core filler are included.
Armour occupies a supplied fraction of its annulus; its remaining area is a
sealed massless void. Air buoyancy is neglected. With a sealed outer sheath,
W_air=m*g; W_water=(m-rho_water*pi*OD**2/4)*g (Archimedes).
External gauge pressure is rho_water*g*depth. All supplied internal pressures
use the same surface gauge datum at the element's assessed depth.
Burst demand=max(p_internal-p_external, 0); collapse reverses the difference.
Demand/allowable <= 1 is comparison to the supplied limit, not an API factor.

The core diameter is a supplied manufacturing envelope. Area occupancy is
necessary but does not establish circular-element packing or manufacturing fit.
Helical lay, ovality, tolerances, flooding and coupled loads are excluded.
"""

from __future__ import annotations

import math
from collections.abc import Mapping
from dataclasses import dataclass
from typing import Literal

Status = Literal["PASS", "FAIL", "NOT_EVALUATED"]


def _number(name: str, value: float, *, positive: bool = False) -> None:
    if not math.isfinite(value) or (value <= 0 if positive else value < 0):
        bound = "positive" if positive else "nonnegative"
        raise ValueError(f"{name} must be finite and {bound}")


@dataclass(frozen=True)
class QualifiedLimit:
    """Supplied positive allowable and its evidence reference; SI units.

    The reference records the caller's basis; its validity is not authenticated.
    """

    value: float
    basis: str

    def __post_init__(self) -> None:
        _number("limit", self.value, positive=True)
        if not self.basis.strip():
            raise ValueError("limit basis must be nonblank")


@dataclass(frozen=True)
class CheckResult:
    """An individual screening check; missing evidence never yields PASS."""

    status: Status
    demand: float | None
    allowable: float | None
    utilisation: float | None
    basis: str


def _unevaluated(reason: str, demand: float | None = None) -> CheckResult:
    return CheckResult("NOT_EVALUATED", demand, None, None, reason)


def _check(
    demand: float, limit: QualifiedLimit | None, *, minimum: bool = False
) -> CheckResult:
    if limit is None:
        return _unevaluated("A qualified limit and its basis are required", demand)
    passed = demand >= limit.value if minimum else demand <= limit.value
    utilisation = (
        (limit.value / demand if demand > 0 else None)
        if minimum
        else demand / limit.value
    )
    return CheckResult(
        "PASS" if passed else "FAIL", demand, limit.value, utilisation, limit.basis
    )


def _standard_gap() -> CheckResult:
    return _unevaluated(
        "API 17E / ISO 13628-5 clause mapping and qualification require "
        "verified standard text and design evidence"
    )


@dataclass(frozen=True)
class FunctionalElement:
    """One named group of straight, identical tubes/hoses; m and kg/m³.

    Wall density is the effective density of the whole wall for reinforced hose.
    Limits are differential pressures in Pa, independently qualified for burst
    and external collapse. Contents density may be zero for an empty element.
    """

    name: str
    kind: Literal["thermoplastic_hose", "steel_tube"]
    count: int
    outer_diameter: float
    inner_diameter: float
    wall_density: float
    contents_density: float
    burst_limit: QualifiedLimit | None = None
    collapse_limit: QualifiedLimit | None = None

    def __post_init__(self) -> None:
        if not self.name.strip():
            raise ValueError("element name must be nonblank")
        if self.kind not in ("thermoplastic_hose", "steel_tube"):
            raise ValueError("unsupported element kind")
        if type(self.count) is not int or self.count < 1:
            raise ValueError("element count must be a positive integer")
        for name in ("outer_diameter", "inner_diameter", "wall_density"):
            _number(name, getattr(self, name), positive=True)
        _number("contents_density", self.contents_density)
        if self.inner_diameter >= self.outer_diameter:
            raise ValueError("inner diameter must be smaller than outer diameter")


@dataclass(frozen=True)
class RadialBuildLayer:
    """Concentric layer: thickness [m], density [kg/m³], occupied fraction.

    Any unoccupied area is sealed void; use fraction=1 for a solid sheath.
    All three inputs are required; no armour fill fraction is assumed.
    """

    thickness: float
    density: float
    material_fraction: float

    def __post_init__(self) -> None:
        for name in ("thickness", "density", "material_fraction"):
            _number(name, getattr(self, name), positive=True)
        if self.material_fraction > 1:
            raise ValueError("material_fraction must not exceed unity")


def _elements_valid(elements: tuple[FunctionalElement, ...]) -> None:
    if not elements or len({e.name for e in elements}) != len(elements):
        raise ValueError("elements must be nonempty with unique names")


@dataclass(frozen=True)
class UmbilicalSection:
    """Supplied core envelope, functional elements, filler, armour and sheath."""

    elements: tuple[FunctionalElement, ...]
    core_diameter: float
    filler_density: float
    armour: tuple[RadialBuildLayer, ...]
    sheath: RadialBuildLayer

    def __post_init__(self) -> None:
        _elements_valid(self.elements)
        _number("core_diameter", self.core_diameter, positive=True)
        _number("filler_density", self.filler_density)
        if any(e.outer_diameter > self.core_diameter for e in self.elements):
            raise ValueError("an element exceeds the supplied core envelope")
        if self.sheath.material_fraction != 1:
            raise ValueError("the sealed sheath must be solid (fraction=1)")


@dataclass(frozen=True)
class SizingResult:
    """Derived OD [m], areas [m²], mass [kg/m] and signed weights [N/m]."""

    outer_diameter: float
    functional_element_count: int
    filler_area: float
    mass_per_length: float
    weight_in_air: float
    weight_in_water: float
    packing: CheckResult
    standard_qualification: CheckResult


def size_umbilical(
    section: UmbilicalSection, *, water_density: float, gravity: float
) -> SizingResult:
    """Calculate straight-element mass balance and sealed-envelope buoyancy."""
    _number("water_density", water_density, positive=True)
    _number("gravity", gravity, positive=True)
    # Circle geometry and mass balance: OpenStax (2016), §14.1; see module basis.
    occupied = sum(
        e.count * math.pi * e.outer_diameter**2 / 4 for e in section.elements
    )
    filler_area = math.pi * section.core_diameter**2 / 4 - occupied
    if filler_area < 0:
        raise ValueError("element areas exceed the supplied core area")
    mass = filler_area * section.filler_density
    for e in section.elements:
        mass += (
            e.count
            * math.pi
            / 4
            * (
                (e.outer_diameter**2 - e.inner_diameter**2) * e.wall_density
                + e.inner_diameter**2 * e.contents_density
            )
        )
    diameter = section.core_diameter
    for layer in (*section.armour, section.sheath):
        outer = diameter + 2 * layer.thickness
        mass += (
            math.pi
            / 4
            * (outer**2 - diameter**2)
            * layer.density
            * layer.material_fraction
        )
        diameter = outer
    # Archimedes: OpenStax (2016), §14.4; gravity is a required input.
    wet = (mass - water_density * math.pi * diameter**2 / 4) * gravity
    if not all(math.isfinite(v) for v in (diameter, mass, mass * gravity, wet)):
        raise ValueError("section exceeds the supported numerical range")
    return SizingResult(
        diameter,
        sum(e.count for e in section.elements),
        filler_area,
        mass,
        mass * gravity,
        wet,
        _unevaluated("Area occupancy alone does not establish manufacturing fit"),
        _standard_gap(),
    )


@dataclass(frozen=True)
class ElementPressureResult:
    """Independent burst and collapse screening for a named group."""

    name: str
    burst: CheckResult
    collapse: CheckResult


@dataclass(frozen=True)
class PressureResult:
    """Hydrostatic pressure [Pa] and individual supplied-limit checks."""

    external_pressure: float
    elements: tuple[ElementPressureResult, ...]
    standard_qualification: CheckResult

    @property
    def status(self) -> Status:
        """Return FAIL for any exceeded limit; qualification gaps prevent PASS."""
        statuses = [c.status for e in self.elements for c in (e.burst, e.collapse)]
        statuses.append(self.standard_qualification.status)
        if "FAIL" in statuses:
            return "FAIL"
        return "NOT_EVALUATED" if "NOT_EVALUATED" in statuses else "PASS"


def check_pressure(
    elements: tuple[FunctionalElement, ...],
    internal_pressures: Mapping[str, float],
    *,
    water_depth: float,
    water_density: float,
    gravity: float,
) -> PressureResult:
    """Screen both differential directions at one depth and pressure datum.

    Repeated calls are needed for operating, shutdown and installation cases;
    this routine does not select the governing internal-pressure envelope.
    """
    _elements_valid(elements)
    if set(internal_pressures) != {e.name for e in elements}:
        raise ValueError("internal pressures must match every element name exactly")
    _number("water_depth", water_depth)
    _number("water_density", water_density, positive=True)
    _number("gravity", gravity, positive=True)
    external = water_density * gravity * water_depth
    _number("external pressure", external)
    results = []
    for e in elements:
        internal = internal_pressures[e.name]
        _number("internal pressure", internal)
        results.append(
            ElementPressureResult(
                e.name,
                _check(max(internal - external, 0), e.burst_limit),
                _check(max(external - internal, 0), e.collapse_limit),
            )
        )
    return PressureResult(external, tuple(results), _standard_gap())
