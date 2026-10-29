"""Digest-bound, solver-free riser model snapshot tables (SI units).

This module consumes explicit snapshots; it does not extract or authenticate
solved-model data. One continuous, top-to-bottom line is supported. Properties
are retained as supplied, rather than reconstructed from design-basis geometry.
Missing properties remain None and NOT_EVALUATED. No design defaults are used.

Loading source: Orcina online help, "Morison's equation", retrieved 2026-10-19,
reproducing Morison et al. (1950): added mass = Ca * displaced-fluid mass;
drag factor = rho * Cd * projected width / 2. The circular area pi*D^2/4
is the elementary disk-area identity. These are normal-direction factors for
a submerged circular line, not a general OrcaFlex hydrodynamic-load evaluator.
"""

from __future__ import annotations

import hashlib
import math
from collections.abc import Mapping, Sequence
from dataclasses import dataclass


MORISON_SOURCE = (
    "https://www.orcina.com/webhelp/OrcaFlex/Content/html/Morison%27sequation.htm"
)

# SI unit declarations; all engineering coefficients and screening limits are
# required snapshot/caller inputs, never inferred from a design standard.
PROPERTY_UNITS = {
    "stress_od_m": "m", "stress_id_m": "m", "section_modulus_m3": "m^3",
    "hydrodynamic_diameter_m": "m", "added_mass_diameter_m": "m",
    "buoyancy_od_m": "m", "dry_mass_kg_m": "kg/m", "wet_mass_kg_m": "kg/m",
    "contents_mass_kg_m": "kg/m", "ea_n": "N", "ei_n_m2": "N m^2",
    "gj_n_m2": "N m^2", "cd_normal": "1", "ca_normal": "1",
    "cd_axial": "1", "ca_axial": "1", "internal_fluid_density_kg_m3": "kg/m^3",
    "marine_growth_thickness_m": "m", "segment_length_m": "m", "length_m": "m",
}
DERIVED_UNITS = {
    "added_mass_kg_m": "kg/m", "total_inertial_mass_kg_m": "kg/m",
    "normal_drag_factor_kg_m2": "kg/m^2", "ca_d2_m2": "m^2",
}
TRANSITION_FIELDS = (
    "stress_od_m", "stress_id_m", "section_modulus_m3", "ei_n_m2", "ea_n",
    "hydrodynamic_diameter_m", "dry_mass_kg_m", "added_mass_kg_m", "ca_d2_m2",
)


@dataclass(frozen=True)
class ComponentSnapshot:
    """Explicit SI model properties with per-field source references.

    The digest must match the bytes supplied to build_appendix. This establishes
    binding, not proof that the snapshot was extracted from those bytes.
    Wet mass means buoyancy-adjusted mass per length and can be negative;
    contents mass is separate from dry structural mass. Diameters and Ca must
    include the intended growth/buoyancy effects already: no correction is made.
    """

    component_id: str
    top_elevation_m: float
    bottom_elevation_m: float
    model_sha256: str
    source: str
    properties: Mapping[str, float | None]
    sources: Mapping[str, str]


def _text(value: str, name: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} requires a non-empty source or identifier")


def _number(value: float, name: str, *, signed: bool = False) -> None:
    try:
        finite = isinstance(value, (int, float)) and math.isfinite(value)
    except OverflowError:
        finite = False
    if (isinstance(value, bool) or not finite or (not signed and value < 0)):
        raise ValueError(f"{name} must be finite and have a valid sign")


def _section_row(item: ComponentSnapshot, digest: str) -> dict:
    _text(item.component_id, "component_id")
    _text(item.source, "elevation source")
    if item.model_sha256 != digest:
        raise ValueError("component model digest differs from supplied model bytes")
    for value in (item.top_elevation_m, item.bottom_elevation_m):
        _number(value, "elevation", signed=True)
    if item.top_elevation_m <= item.bottom_elevation_m:
        raise ValueError("top elevation must exceed bottom elevation")
    if set(item.properties) - PROPERTY_UNITS.keys():
        raise ValueError("unknown property name")
    if set(item.sources) - item.properties.keys():
        raise ValueError("source references an absent property")
    for name, source in item.sources.items():
        _text(source, f"{name} source")
    properties = {name: item.properties.get(name) for name in PROPERTY_UNITS}
    for name, value in properties.items():
        if value is not None:
            _number(value, name, signed=name == "wet_mass_kg_m")
            _text(item.sources.get(name), f"{name} source")
    od, inner = properties["stress_od_m"], properties["stress_id_m"]
    if od is not None and inner is not None and inner >= od:
        raise ValueError("stress_id_m must be less than stress_od_m")
    return {
        "component_id": item.component_id,
        "top_elevation_m": item.top_elevation_m,
        "bottom_elevation_m": item.bottom_elevation_m,
        "source": item.source, "sources": dict(item.sources),
        "properties": properties,
        "status": {name: "SUPPLIED" if value is not None else "NOT_EVALUATED"
                   for name, value in properties.items()},
    }


def _loading_row(section: dict, density: float) -> dict:
    props = section["properties"]
    dependencies = {
        "ca_d2_m2": ("ca_normal", "added_mass_diameter_m"),
        "added_mass_kg_m": ("ca_normal", "added_mass_diameter_m"),
        "normal_drag_factor_kg_m2": ("cd_normal", "hydrodynamic_diameter_m"),
        "total_inertial_mass_kg_m": (
            "ca_normal", "added_mass_diameter_m", "dry_mass_kg_m",
            "contents_mass_kg_m",
        ),
    }
    # Orcina, Morison's equation (online, retrieved 2026-10-19), extended form
    # and accelerating-body equation: Ca*Delta, (m+Ca*Delta), rho*Cd*A/2.
    # pi/4 is disk geometry, not an empirical added-mass coefficient.
    formulas = {
        "ca_d2_m2": lambda: props["ca_normal"] * props["added_mass_diameter_m"]**2,
        "added_mass_kg_m": lambda: (
            props["ca_normal"] * density * math.pi
            * props["added_mass_diameter_m"]**2 / 4
        ),
        "normal_drag_factor_kg_m2": lambda: (
            density * props["cd_normal"] * props["hydrodynamic_diameter_m"] / 2
        ),
        "total_inertial_mass_kg_m": lambda: (
            row["added_mass_kg_m"] + props["dry_mass_kg_m"]
            + props["contents_mass_kg_m"]
        ),
    }
    row = {"component_id": section["component_id"], "status": {},
           "missing_inputs": {}}
    for name, needed in dependencies.items():
        missing = [field for field in needed if props[field] is None]
        try:
            value = None if missing else formulas[name]()
        except OverflowError as exc:
            raise ValueError(f"{name} must be finite") from exc
        if value is not None:
            _number(value, name)
        row[name] = value
        row["status"][name] = "NOT_EVALUATED" if missing else "CALCULATED"
        row["missing_inputs"][name] = missing
    return row


def _transition(upper: dict, lower: dict, limits: Mapping[str, float]) -> dict:
    metrics = {}
    for name in TRANSITION_FIELDS:
        a, b = upper[name], lower[name]
        # Issue 2153 (2026), C.2: adjacent ratios. Direction is lower/upper;
        # max(b/a,a/b) is an explicitly symmetric screen, not a code criterion.
        valid = a is not None and b is not None and a > 0 and b > 0
        ratio, factor = (b / a, max(b / a, a / b)) if valid else (None, None)
        if valid:
            _number(ratio, "transition ratio")
            _number(factor, "step factor")
        limit = limits.get(name)
        screened = valid and limit is not None
        metrics[name] = {
            "ratio_lower_over_upper": ratio, "step_factor": factor,
            "ratio_status": "CALCULATED" if valid else "NOT_EVALUATED",
            "limit": limit, "flag": factor > limit if screened else None,
            "screening_status": "EVALUATED" if screened else "NOT_EVALUATED",
        }
    return {"upper_component_id": upper["component_id"],
            "lower_component_id": lower["component_id"],
            "elevation_m": upper["bottom_elevation_m"], "metrics": metrics}


def _validate_inputs(model_bytes, density, density_source, limits):
    if not isinstance(model_bytes, bytes) or not model_bytes:
        raise ValueError("non-empty model bytes are required")
    _number(density, "water density")
    if density == 0:
        raise ValueError("water density must be positive")
    _text(density_source, "water density source")
    for name, limit in limits.items():
        if name not in TRANSITION_FIELDS:
            raise ValueError("unknown screening metric")
        _number(limit, "screening limit")
        if limit <= 1:  # A symmetric step factor has its mathematical minimum at 1.
            raise ValueError("screening limits must exceed unity")


def _ordered_sections(components, digest):
    sections = [_section_row(item, digest) for item in components]
    sections.sort(key=lambda row: row["top_elevation_m"], reverse=True)
    if not sections or len({row["component_id"] for row in sections}) != len(sections):
        raise ValueError("a non-empty set of unique component_id values is required")
    for upper, lower in zip(sections, sections[1:]):
        if upper["bottom_elevation_m"] != lower["top_elevation_m"]:
            raise ValueError("component elevations must be continuous without overlap")
    return sections


def _tables(sections, transitions, loading, digest):
    return {
        "C.1": {"caption": f"Section property snapshot; model SHA-256 {digest}",
                "units": {**PROPERTY_UNITS, "top_elevation_m": "m",
                          "bottom_elevation_m": "m"}, "rows": sections},
        "C.2": {"caption": f"Adjacent component screens; model SHA-256 {digest}",
                "units": {"elevation_m": "m", "ratio": "1"},
                "rows": transitions},
        "C.3": {"caption": f"Circular line normal loading; model SHA-256 {digest}",
                "units": DERIVED_UNITS.copy(), "rows": loading},
    }


def _validated_basis(design_basis):
    if not isinstance(design_basis, Mapping):
        raise ValueError("design basis must be a component/property mapping")
    basis = {}
    for component_id, properties in design_basis.items():
        _text(component_id, "basis component_id")
        if not isinstance(properties, Mapping):
            raise ValueError("basis properties must be a mapping")
        basis[component_id] = {}
        for name, entry in properties.items():
            if name not in PROPERTY_UNITS:
                raise ValueError("unknown basis property identity")
            if (not isinstance(entry, Mapping)
                    or set(entry) != {"value", "units", "source"}):
                raise ValueError("basis entry requires value, units and source")
            if entry["units"] != PROPERTY_UNITS[name]:
                raise ValueError(f"basis units differ for {name}; no conversion is applied")
            _text(entry["source"], f"basis {name} source")
            value = entry["value"]
            if value is not None:
                _number(value, f"basis {name}", signed=name == "wet_mass_kg_m")
            basis[component_id][name] = dict(entry)
    return basis


def _reconciliation_table(sections, design_basis, digest):
    basis = _validated_basis(design_basis)
    model = {section["component_id"]: section for section in sections}
    identities = list(model) + sorted(basis.keys() - model.keys())
    rows = []
    for component_id in identities:
        section = model.get(component_id)
        properties = basis.get(component_id, {})
        for name, units in PROPERTY_UNITS.items():
            # Model components expose all snapshot properties, including unknowns;
            # basis-only components expose only the explicitly supplied identities.
            if section is None and name not in properties:
                continue
            entry = properties.get(name)
            model_value = section["properties"][name] if section else None
            basis_value = entry["value"] if entry else None
            missing = []
            if section is None:
                missing.append("model_component")
            elif model_value is None:
                missing.append("model_value")
            if component_id not in basis:
                missing.append("basis_component")
            elif entry is None:
                missing.append("basis_property")
            elif basis_value is None:
                missing.append("basis_value")
            # C.6 subtraction convention: model minus basis; absolute difference
            # is its magnitude. No division, tolerance or design limit is used.
            difference = None if missing else model_value - basis_value
            if difference is not None:
                _number(difference, "reconciliation difference", signed=True)
            rows.append({
                "component_id": component_id, "property_id": name,
                "model_value": model_value, "basis_value": basis_value,
                "units": units,
                "model_source": section["sources"].get(name) if section else None,
                "basis_source": entry["source"] if entry else None,
                "signed_difference": difference,
                "absolute_difference": abs(difference) if difference is not None else None,
                "status": "NOT_EVALUATED" if missing else "CALCULATED",
                "missing_evidence": missing,
            })
    return {
        "caption": f"Model-to-basis reconciliation; model SHA-256 {digest}",
        "difference_convention": "model minus basis",
        "rows": rows,
    }


def build_appendix(
    model_bytes: bytes,
    components: Sequence[ComponentSnapshot],
    *,
    water_density_kg_m3: float,
    water_density_source: str,
    screening_limits: Mapping[str, float] | None = None,
    design_basis: Mapping[str, Mapping[str, Mapping[str, float | str | None]]] | None = None,
) -> dict:
    """Return C.1-C.3 and optional C.6 tables for strict JSON serialization.

    Screening limits apply to max(lower/upper, upper/lower), flagging strict
    exceedance only. Both properties must be positive for a ratio. No limit
    means no screening verdict. Missing properties mean NOT_EVALUATED.
    Continuity uses exact snapshot elevations; no hidden tolerance is applied.

    Optional design_basis maps component_id to property_id to an entry containing
    value (finite SI number or None), units and source (non-empty label). Units
    must exactly match PROPERTY_UNITS; no relabelling or conversion is performed.
    C.6 joins exact identities only, retaining unmatched evidence with None
    differences and NOT_EVALUATED status. Signed difference is model minus basis;
    absolute difference is its magnitude, including when the basis is zero.
    Passing None omits C.6; an empty mapping reports missing basis evidence.
    This comparison does not establish extraction authenticity or acceptability.
    """
    limits = dict(screening_limits or {})
    _validate_inputs(model_bytes, water_density_kg_m3, water_density_source, limits)
    digest = hashlib.sha256(model_bytes).hexdigest()
    sections = _ordered_sections(components, digest)
    loading = [_loading_row(row, water_density_kg_m3) for row in sections]
    merged = [{**section, **section["properties"], **loads}
              for section, loads in zip(sections, loading)]
    transitions = [_transition(a, b, limits) for a, b in zip(merged, merged[1:])]
    tables = _tables(sections, transitions, loading, digest)
    if design_basis is not None:
        tables["C.6"] = _reconciliation_table(sections, design_basis, digest)
    return {
        "model_sha256": digest, "model_extraction_status": "NOT_EVALUATED",
        "screening_is_code_criterion": False,
        "water_density_kg_m3": water_density_kg_m3,
        "water_density_source": water_density_source,
        "equation_sources": [MORISON_SOURCE],
        "tables": tables,
        "not_evaluated": [
            "Solved-model extraction and snapshot authenticity",
            "Axial loading, non-circular or partially submerged components",
            "Lumped bodies (C.3) and special components (C.4)",
            "Node-level discretisation (C.5)",
            *(["Shared model-to-basis reconciliation (C.6)"]
              if design_basis is None else []),
            "Elevation step-profile figure",
        ],
    }
