# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Sourced SI design-basis template and uniform lift-string static adapter.

This is a screening data contract, not a class-standard design specification.
Field scope, equations and assumptions: docs/domains/risers/lift-riser-analysis.md.
No empirical value, material property or acceptance criterion is defaulted.
"""

from __future__ import annotations

from dataclasses import dataclass
from math import isfinite
from numbers import Real
from types import MappingProxyType
from typing import Mapping
from urllib.parse import urlsplit

from digitalmodel.subsea.mining_riser import (
    BundleSection,
    HangingResult,
    SlurryLayer,
    StringWeightInventory,
    Tube,
    free_hanging_screen,
    string_weight_inventory,
)


def finite_number(value: float) -> bool:
    """Reject booleans, text and nonfinite quantities without echoing inputs."""
    return isinstance(value, Real) and not isinstance(value, bool) and isfinite(value)


@dataclass(frozen=True)
class Source:
    """Caller-attested provenance and applicability; not source authentication.

    Assumed quantities must be identified explicitly, including manufactured
    test inputs. A citation does not prove that its data apply to this system.
    """

    reference: str
    locator: str
    conditions: str
    assumed: bool

    def __post_init__(self) -> None:
        for name in ("reference", "locator", "conditions"):
            value = getattr(self, name)
            if not isinstance(value, str) or not value.strip():
                raise ValueError(
                    "source reference, locator and conditions are required"
                )
        try:
            parsed = urlsplit(self.reference)
            valid = (
                parsed.scheme == "https"
                and parsed.hostname
                and parsed.username is None
                and parsed.password is None
            )
        except ValueError:
            valid = False
        if not valid:
            raise ValueError("source requires a credential-free public HTTPS reference")
        if not isinstance(self.assumed, bool):
            raise ValueError("source assumed flag must be boolean")


@dataclass(frozen=True)
class SourcedQuantity:
    """Finite scalar, explicit SI unit and required provenance."""

    value: float
    unit: str
    source: Source

    def __post_init__(self) -> None:
        if not finite_number(self.value):
            raise ValueError("quantity must be a finite real number")
        if not isinstance(self.unit, str) or not self.unit.strip():
            raise ValueError("quantity unit is required")
        if not isinstance(self.source, Source):
            raise ValueError("quantity source is required")


@dataclass(frozen=True)
class FieldSpec:
    """Unit and mathematical sign domain; no engineering acceptance range."""

    unit: str
    domain: str


# Domains below are mathematical requirements of D1/D2 and R1, not design
# limits. Fractions [0,1] follow the definition of a fraction (R1 Eq. 6.3).
BASIS_FIELDS = MappingProxyType(
    {
        "water_depth_m": FieldSpec("m", "positive"),
        "gravity_m_s2": FieldSpec("m/s2", "positive"),
        "external_density_kg_m3": FieldSpec("kg/m3", "positive"),
        "outer_diameter_m": FieldSpec("m", "positive"),
        "wall_thickness_m": FieldSpec("m", "positive"),
        "material_density_kg_m3": FieldSpec("kg/m3", "positive"),
        "youngs_modulus_pa": FieldSpec("Pa", "positive"),
        "contents_inertia_fraction": FieldSpec("1", "fraction"),
        "additional_mass_kg_m": FieldSpec("kg/m", "nonnegative"),
        "additional_displaced_area_m2": FieldSpec("m2", "nonnegative"),
        "axial_added_mass_kg_m": FieldSpec("kg/m", "nonnegative"),
    }
)
CASE_FIELDS = MappingProxyType(
    {
        "liquid_density_kg_m3": FieldSpec("kg/m3", "positive"),
        "solids_density_kg_m3": FieldSpec("kg/m3", "positive"),
        "solids_volume_fraction": FieldSpec("1", "fraction"),
        "mixture_velocity_m_s": FieldSpec("m/s", "nonnegative"),
        "significant_wave_height_m": FieldSpec("m", "nonnegative"),
        "peak_wave_period_s": FieldSpec("s", "positive"),
        "current_speed_m_s": FieldSpec("m/s", "nonnegative"),
        "top_heave_amplitude_m": FieldSpec("m", "nonnegative"),
        "top_heave_period_s": FieldSpec("s", "positive"),
        "lower_end_force_n": FieldSpec("N", "signed"),
        "lower_end_mass_kg": FieldSpec("kg", "nonnegative"),
        "pump_differential_pressure_pa": FieldSpec("Pa", "nonnegative"),
    }
)


def _quantity_matches(name: str, quantity: SourcedQuantity, spec: FieldSpec) -> None:
    if not isinstance(quantity, SourcedQuantity) or quantity.unit != spec.unit:
        raise ValueError(f"{name} requires its specified SI unit and source")
    v = quantity.value
    if (
        (spec.domain == "positive" and v <= 0)
        or (spec.domain == "nonnegative" and v < 0)
        or (spec.domain == "fraction" and not 0 <= v <= 1)
    ):
        raise ValueError(f"{name} is outside its mathematical domain")


def _fields(values: Mapping, specs: Mapping) -> Mapping:
    if not isinstance(values, Mapping) or set(values) != set(specs):
        raise ValueError("fields must match the required template exactly")
    for name, spec in specs.items():
        _quantity_matches(name, values[name], spec)
    return MappingProxyType(dict(values))


@dataclass(frozen=True)
class LoadCase:
    """Named operating or survival snapshot; all CASE_FIELDS are required.

    Wave, current, motion, flow and pump fields record the basis but do not enter
    the gravity-only static adapter. They require later response calculations.
    """

    name: str
    kind: str
    values: Mapping[str, SourcedQuantity]

    def __post_init__(self) -> None:
        if not isinstance(self.name, str) or not self.name.strip():
            raise ValueError("case name is required")
        if self.kind not in ("operating", "survival"):
            raise ValueError("case kind must be operating or survival")
        object.__setattr__(self, "values", _fields(self.values, CASE_FIELDS))


@dataclass(frozen=True)
class LiftDesignBasis:
    """Uniform single-bore basis with explicit optional top-tension criterion.

    Both operating and survival snapshots are required by this template's scope,
    not by a claimed design standard. More cases may be supplied. None for the
    limit means NOT_EVALUATED, never acceptance. Use mining_riser directly for
    bundles and piecewise profiles; this adapter assumes one full-depth tube.
    """

    values: Mapping[str, SourcedQuantity]
    cases: tuple[LoadCase, ...]
    top_tension_limit: SourcedQuantity | None

    def __post_init__(self) -> None:
        object.__setattr__(self, "values", _fields(self.values, BASIS_FIELDS))
        cases = tuple(self.cases)
        if any(not isinstance(c, LoadCase) for c in cases):
            raise ValueError("cases must be LoadCase records")
        if {c.kind for c in cases} != {"operating", "survival"}:
            raise ValueError("both operating and survival cases are required")
        if len({c.name for c in cases}) != len(cases):
            raise ValueError("case names must be unique")
        object.__setattr__(self, "cases", cases)
        if self.top_tension_limit is not None:
            _quantity_matches(
                "top tension limit", self.top_tension_limit, FieldSpec("N", "positive")
            )
        # Geometry identity D1: d=D-2t must be positive.
        if (
            2 * self.values["wall_thickness_m"].value
            >= self.values["outer_diameter_m"].value
        ):
            raise ValueError("wall thickness must leave a positive bore")

    def case(self, name: str) -> LoadCase:
        for case in self.cases:
            if case.name == name:
                return case
        raise ValueError("unknown case")

    def section(self, name: str) -> BundleSection:
        """Map SI quantities to the existing D1/D2 and R1 calculation inputs."""
        v = {k: q.value for k, q in self.values.items()}
        c = {k: q.value for k, q in self.case(name).values.items()}
        layer = SlurryLayer(
            v["water_depth_m"],
            c["liquid_density_kg_m3"],
            c["solids_density_kg_m3"],
            c["solids_volume_fraction"],
        )
        tube = Tube(
            v["outer_diameter_m"],
            v["wall_thickness_m"],
            v["material_density_kg_m3"],
            v["youngs_modulus_pa"],
            layer.density_kg_m3,
            v["contents_inertia_fraction"],
            True,
        )
        return BundleSection(
            layer.length_m,
            (tube,),
            v["external_density_kg_m3"],
            v["additional_mass_kg_m"],
            v["additional_displaced_area_m2"],
            v["axial_added_mass_kg_m"],
        )


@dataclass(frozen=True)
class CaseScreen:
    """Gravity-only results and a caller-criterion comparison; not compliance."""

    static: HangingResult
    weights: StringWeightInventory
    tension_status: str


def screen_case(basis: LiftDesignBasis, name: str) -> CaseScreen:
    """Compare D2 top effective tension against the explicit supplied limit.

    Any effective compression fails the taut-string assumption independently
    of the top-force comparison. No pressure-wall strength check is implied.
    """
    section = basis.section(name)
    case = basis.case(name)
    gravity = basis.values["gravity_m_s2"].value
    static = free_hanging_screen(
        [section],
        gravity_m_s2=gravity,
        lower_end_force_n=case.values["lower_end_force_n"].value,
    )
    status = "NOT_EVALUATED"
    if static.has_compression:
        status = "FAIL"
    elif basis.top_tension_limit is not None:
        status = (
            "PASS"
            if static.effective_tension_n[0] <= basis.top_tension_limit.value
            else "FAIL"
        )
    return CaseScreen(
        static, string_weight_inventory([section], gravity_m_s2=gravity), status
    )
