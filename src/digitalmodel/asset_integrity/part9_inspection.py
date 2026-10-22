# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Inspection intake for API 579 Part 9; no engineering acceptance is inferred.

Field coverage follows the public practitioner checklist (undated; accessed
2026-10-20), questions 1-33:
https://www.inspection-for-industry.com/api-579-part-9-crack-like-flaws-assessment.html
This is an intake reference, not a source of standard-derived coefficients.
API 579-1/ASME FFS-1 (2016), Part 9 and (2007), Annexes C/D/E remain
qualification requirements. The annex crosswalk to later editions is unverified.
No licensed tables, default factors or derived limits ship here.
"""

from __future__ import annotations

from collections.abc import Mapping
from datetime import date, datetime, timezone
import math


# Kinds express data representation and geometric signs, not code acceptance.
# Units are explicit; no pressure-to-stress or toughness correlation is assumed.
FIELD_SPECS = {
    "component_id": "text",
    "standard_edition": "text",
    "units": ("mm-MPa-C",),
    "inspection.date": "date",
    "inspection.method": "text",
    "inspection.sizing_basis": "text",
    "geometry.shape": ("cylinder", "sphere", "plate", "other"),
    "geometry.thickness_mm": "positive",
    "geometry.location": ("base_metal", "weld", "haz", "nozzle"),
    "flaw.depth_mm": "positive",
    "flaw.length_mm": "positive",
    "flaw.orientation": ("longitudinal", "circumferential", "other"),
    "flaw.position": ("internal_surface", "external_surface", "embedded", "through_wall"),
    "flaw.sizing_uncertainty_mm": "nonnegative",
    "loads.pressure_mpa": "nonnegative",
    "loads.temperature_c": "finite",
    "loads.cyclic": "bool",
    "loads.membrane_mpa": "finite",
    "loads.bending_mpa": "finite",
    "loads.secondary_mpa": "finite",
    "loads.stress_basis": "text",
    "loads.governing_case": "text",
    "material.yield_mpa": "positive",
    "material.ultimate_mpa": "positive",
    "material.toughness_mpa_sqrt_m": "positive",
    "material.toughness_temperature_c": "finite",
    "material.toughness_zone": ("base_metal", "weld", "haz"),
    "material.toughness_basis": "text",
    "fabrication.pwht": "bool",
    "fabrication.pressure_test_after_weld": "bool",
    "environment.hydrogen_service": "bool",
    "growth.mechanism": ("none", "fatigue", "environmental", "combined", "unknown"),
    "growth.basis": "text",
    "growth.assessment_period_years": "positive",
}

# References are outstanding requirements, not resolved Citation sidecars.
# They shall not be lifted into provenance as qualified calculation sources.
METHOD_REQUIREMENTS = (
    ("fad", "Part 9: Level 2 failure assessment diagram", "2016",
     "The shared crack_fad.fad_curve_api579_level2 exists; its edition, cutoff "
     "and applicability require source qualification for this workflow."),
    ("partial_safety_factors", "Part 9: partial safety factors", "2016",
     "Category-keyed factors and category selection require a verified table; "
     "no unity factor or category is assumed."),
    ("annex_c_k", "Annex C: stress intensity factor solutions", "2007",
     "Cylinder longitudinal/circumferential and sphere influence coefficients, "
     "crack-front locations, interpolation and validity ranges are unqualified."),
    ("annex_d_reference_stress", "Annex D: reference stress solutions", "2007",
     "A geometry-matched reference-stress solution is required; plate stress "
     "and uncracked pressure stress shall not substitute for a cylinder solution."),
    ("annex_e_residual", "Annex E: residual stress distributions", "2007",
     "Weld type, PWHT and pressure-test effects require a qualified profile "
     "and plasticity-interaction method; absent stress is not zero stress."),
    ("growth", "Part 9: crack growth and assessment period", "2016",
     "Growth parameters, threshold, environment and assessment period require "
     "qualification; remaining life and leak-before-break are not calculated."),
    ("published_example", "Part 9: worked examples", "2016",
     "No published-example benchmark has been qualified for this workflow."),
)


def _get(data, path):
    value = data
    for key in path.split("."):
        if not isinstance(value, Mapping) or key not in value:
            return None
        value = value[key]
    return value


def _valid(value, kind):
    if isinstance(kind, tuple):
        return isinstance(value, str) and value in kind
    if kind == "bool":
        return isinstance(value, bool)
    if kind == "text":
        return isinstance(value, str) and bool(value.strip())
    if kind == "date":
        if not isinstance(value, str):
            return False
        try:
            return date.fromisoformat(value).isoformat() == value
        except ValueError:
            return False
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        return False
    try:
        finite = math.isfinite(value)
    except OverflowError:
        return False
    return finite and (kind == "finite" or
                       (value > 0 if kind == "positive" else value >= 0))


def _require(data, observed, issues, path, kind):
    value = _get(data, path)
    if value is None or not _valid(value, kind):
        issues.append({"field": path, "kind": "missing" if value is None else "invalid",
                       "reason": f"Required {kind} value was not established."})
    else:
        observed[path] = value


def _conditional_fields(data, observed, issues):
    def require(path, kind):
        _require(data, observed, issues, path, kind)

    if observed.get("geometry.shape") in ("cylinder", "sphere"):
        require("geometry.inside_radius_mm", "positive")
    if observed.get("flaw.position") == "embedded":
        require("flaw.ligament_mm", "positive")
    if observed.get("geometry.location") in ("weld", "haz", "nozzle"):
        require("fabrication.weld_type", "text")
        require("fabrication.distance_to_weld_mm", "nonnegative")
    if observed.get("environment.hydrogen_service") is True:
        require("environment.hydrogen_basis", "text")
    active_growth = observed.get("growth.mechanism") not in (None, "none")
    if observed.get("loads.cyclic") is True or active_growth:
        parameters = _get(data, "growth.parameters")
        if not _growth_parameters_valid(parameters):
            issues.append({"field": "growth.parameters", "kind": "missing_or_invalid",
                           "reason": "Supply model, basis, units and finite named values."})
        else:
            for key in ("model", "basis", "units"):
                observed[f"growth.parameters.{key}"] = parameters[key]
            for key, value in parameters["values"].items():
                observed[f"growth.parameters.values.{key}"] = value


def _growth_parameters_valid(parameters):
    if not isinstance(parameters, Mapping):
        return False
    if not all(_valid(parameters.get(k), "text") for k in ("model", "basis", "units")):
        return False
    values = parameters.get("values")
    return isinstance(values, Mapping) and bool(values) and all(
        _valid(k, "text") and _valid(v, "finite") for k, v in values.items())


def _consistency_issues(observed, issues, as_of):
    def issue(path, reason):
        issues.append({"field": path, "kind": "inconsistent", "reason": reason})

    inspected = observed.get("inspection.date")
    if inspected is not None and date.fromisoformat(inspected) > as_of:
        issue("inspection.date", "Inspection date is after the assessment date.")
    a = observed.get("flaw.depth_mm")
    t = observed.get("geometry.thickness_mm")
    uncertainty = observed.get("flaw.sizing_uncertainty_mm")
    position = observed.get("flaw.position")
    # Geometric identities, not standard-derived applicability limits.
    if a is not None and t is not None and position == "through_wall" and a != t:
        issue("flaw.depth_mm", "A through-wall extent must equal measured wall thickness.")
    if a is not None and t is not None and position != "through_wall":
        if a >= t:
            issue("flaw.depth_mm", "A sub-wall flaw requires depth less than wall thickness.")
        elif uncertainty is not None and a + uncertainty >= t:
            issue("flaw.sizing_uncertainty_mm", "Depth plus sizing uncertainty reaches the wall.")
        ligament = observed.get("flaw.ligament_mm")
        if position == "embedded" and ligament is not None:
            if a + ligament + (uncertainty or 0) >= t:
                issue("flaw.ligament_mm", "Embedded flaw requires a positive far ligament.")
    sy, su = observed.get("material.yield_mpa"), observed.get("material.ultimate_mpa")
    if sy is not None and su is not None and su < sy:
        issue("material.ultimate_mpa", "Ultimate strength is below yield strength.")
    location = observed.get("geometry.location")
    zone = observed.get("material.toughness_zone")
    if location in ("base_metal", "weld", "haz") and zone is not None and location != zone:
        issue("material.toughness_zone", "Document controlling toughness for the flaw zone.")
    temperatures = (observed.get("loads.temperature_c"),
                    observed.get("material.toughness_temperature_c"))
    # The Celsius scale places absolute zero at -273.15 C (SI Brochure, 9th ed.,
    # section 2.3.1, "The kelvin", p. 129); this is not an FFS code limit.
    for field in ("loads.temperature_c", "material.toughness_temperature_c"):
        value = observed.get(field)
        if value is not None and value < -273.15:
            issue(field, "Temperature is below absolute zero (-273.15 C).")
    if None not in temperatures and temperatures[0] != temperatures[1]:
        # Neither a temperature correction nor its acceptance is inferred.
        issue("material.temperature_basis", "Toughness temperature differs from the load case; "
              "a temperature-qualified material assessment is required.")
    if observed.get("loads.cyclic") is True and observed.get("growth.mechanism") == "none":
        issue("growth.mechanism", "Cyclic loading requires a documented growth assessment.")
    if observed.get("growth.mechanism") == "unknown":
        issue("growth.mechanism", "The growth mechanism is unresolved; qualify it before review.")


def _applicability_flags(observed):
    # The requirements mix 2007 annexes and 2016 Part 9; no edition mapping is qualified.
    flags = ["STANDARD_METHODS_UNQUALIFIED", "ANNEX_C_SOLUTION_UNQUALIFIED",
             "EDITION_CROSSWALK_UNQUALIFIED"]
    if observed.get("geometry.location") in ("weld", "haz", "nozzle"):
        flags.append("WELD_OR_DISCONTINUITY_REVIEW")
    if observed.get("environment.hydrogen_service") is True:
        flags.append("HYDROGEN_REVIEW")
    if observed.get("loads.cyclic") is True or observed.get("growth.mechanism") != "none":
        flags.append("GROWTH_REVIEW")
    if observed.get("flaw.position") in ("embedded", "through_wall"):
        flags.append("FLAW_POSITION_REQUIRES_SPECIALIST_REVIEW")
    return flags


def assess_inspection(data: Mapping, *, as_of: date | None = None) -> dict:
    """Return missing measurements and named calculation blockers, never a pass.

    COMPLETE_FOR_REVIEW means only that the declared intake fields are present
    and internally consistent. It does not establish data authenticity, standard
    applicability, material suitability, engineering sufficiency or service life.
    The result contains no unresolved standard-derived numeric constants.
    Future inspections are checked against as_of, defaulting to the current UTC
    date. Supply a calendar date to reproduce a historical intake decision.
    A future inspection can become eligible for intake when that date is reached.
    """
    if not isinstance(data, Mapping):
        raise ValueError("part9_inspection must be a mapping")
    if as_of is None:
        as_of = datetime.now(timezone.utc).date()
    if type(as_of) is not date:
        raise ValueError("as_of must be a calendar date")
    observed, issues = {}, []
    for path, kind in FIELD_SPECS.items():
        _require(data, observed, issues, path, kind)
    _conditional_fields(data, observed, issues)
    _consistency_issues(observed, issues, as_of)
    checks = [{"id": key, "status": "NOT_EVALUATED", "source_required": section,
               "standard": "API 579-1/ASME FFS-1",
               "edition": edition,
               "reason": reason} for key, section, edition, reason in METHOD_REQUIREMENTS]
    return {
        "component_id": observed.get("component_id", "unspecified"),
        "assessment_type": "API579_PART9_INSPECTION_READINESS",
        "standard_edition_requested": observed.get("standard_edition"),
        "verdict": "NOT_EVALUATED", "acceptable": None, "kr": None, "lr": None,
        "measurement_sufficiency": {
            "status": "INSUFFICIENT_DATA" if issues else "COMPLETE_FOR_REVIEW",
            "issues": issues,
            "criterion": "Intake completeness only; engineering sufficiency is not established.",
        },
        "observed_fields": observed,
        "checks": checks,
        "applicability_flags": _applicability_flags(observed),
        "next_action": "Resolve intake issues, qualify standard methods and a published "
                       "benchmark, then obtain a reviewed Part 9 assessment.",
    }
