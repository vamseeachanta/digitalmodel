# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Part 9 intake tests; all numerical inputs are synthetic, not code limits.

The public practitioner checklist (undated, accessed 2026-10-20), questions
1-33, supplies field coverage only, not numerical acceptance criteria:
https://www.inspection-for-industry.com/api-579-part-9-crack-like-flaws-assessment.html
"""

from copy import deepcopy
from datetime import date, datetime, timedelta
import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.asset_integrity.part9_inspection import assess_inspection

REQUIRED_FIELDS = (
    "component_id", "standard_edition", "units", "inspection.date",
    "inspection.method", "inspection.sizing_basis", "geometry.shape",
    "geometry.thickness_mm", "geometry.location", "flaw.depth_mm",
    "flaw.length_mm", "flaw.orientation", "flaw.position",
    "flaw.sizing_uncertainty_mm", "loads.pressure_mpa", "loads.temperature_c",
    "loads.cyclic", "loads.membrane_mpa", "loads.bending_mpa",
    "loads.secondary_mpa", "loads.stress_basis", "loads.governing_case",
    "material.yield_mpa", "material.ultimate_mpa", "material.toughness_mpa_sqrt_m",
    "material.toughness_temperature_c", "material.toughness_zone",
    "material.toughness_basis", "fabrication.pwht",
    "fabrication.pressure_test_after_weld", "environment.hydrogen_service",
    "growth.mechanism", "growth.basis", "growth.assessment_period_years",
)


@pytest.fixture
def inspection():
    """Invented surface flaw: 5 mm depth in a 20 mm wall; no measured data."""
    return {
        "component_id": "synthetic-component",
        "standard_edition": "2016",
        "units": "mm-MPa-C",
        "inspection": {"date": "2026-10-20", "method": "synthetic UT",
                       "sizing_basis": "Synthetic demonstration only"},
        "geometry": {"shape": "cylinder", "thickness_mm": 20,
                     "inside_radius_mm": 500, "location": "base_metal"},
        "flaw": {"depth_mm": 5, "length_mm": 30, "orientation": "longitudinal",
                 "position": "external_surface", "sizing_uncertainty_mm": 1},
        "loads": {"pressure_mpa": 2, "temperature_c": -10, "cyclic": False,
                  "membrane_mpa": 50, "bending_mpa": -5, "secondary_mpa": 0,
                  "stress_basis": "Synthetic specified stresses; no pressure conversion",
                  "governing_case": "synthetic steady state"},
        "material": {"yield_mpa": 250, "ultimate_mpa": 400,
                     "toughness_mpa_sqrt_m": 60, "toughness_temperature_c": -10,
                     "toughness_zone": "base_metal", "toughness_basis": "Synthetic input"},
        "fabrication": {"pwht": False, "pressure_test_after_weld": False},
        "environment": {"hydrogen_service": False},
        "growth": {"mechanism": "none", "basis": "Synthetic non-growing case",
                   "assessment_period_years": 1},
    }


def test_complete_intake_does_not_establish_acceptance(inspection):
    result = assess_inspection(inspection)
    assert result["measurement_sufficiency"]["status"] == "COMPLETE_FOR_REVIEW"
    assert result["measurement_sufficiency"]["issues"] == []
    assert result["verdict"] == "NOT_EVALUATED"
    assert result["acceptable"] is None
    assert result["kr"] is None and result["lr"] is None
    assert {c["id"] for c in result["checks"]} == {
        "fad", "partial_safety_factors", "annex_c_k", "annex_d_reference_stress",
        "annex_e_residual", "growth", "published_example",
    }
    assert all(c["status"] == "NOT_EVALUATED" for c in result["checks"])
    # Hand check of reported measurements: 5 + 1 = 6 mm, below the 20 mm wall.
    assert result["observed_fields"]["flaw.depth_mm"] == 5
    assert result["observed_fields"]["flaw.sizing_uncertainty_mm"] == 1
    assert result["observed_fields"]["geometry.thickness_mm"] == 20
    json.dumps(result, allow_nan=False)


@pytest.mark.parametrize("field", REQUIRED_FIELDS)
def test_every_required_field_is_reported_when_missing(inspection, field):
    keys = field.split(".")
    parent = inspection
    for key in keys[:-1]:
        parent = parent[key]
    del parent[keys[-1]]
    result = assess_inspection(inspection)
    issues = result["measurement_sufficiency"]["issues"]
    assert any(i["field"] == field and i["kind"] == "missing" for i in issues)
    assert result["verdict"] == "NOT_EVALUATED"


@pytest.mark.parametrize("value", [True, "5", float("nan"), float("inf"), -1, 0, 10**1000])
def test_invalid_depth_never_counts_as_a_measurement(inspection, value):
    inspection["flaw"]["depth_mm"] = value
    result = assess_inspection(inspection)
    assert result["measurement_sufficiency"]["status"] == "INSUFFICIENT_DATA"
    assert any(i["field"] == "flaw.depth_mm" for i in
               result["measurement_sufficiency"]["issues"])
    json.dumps(result, allow_nan=False)


@pytest.mark.parametrize("group,key,value", [
    ("loads", "cyclic", 0), ("fabrication", "pwht", "false"),
    ("inspection", "date", "2026-02-30"), ("flaw", "position", "unknown"),
    ("material", "toughness_basis", "  "), ("geometry", "thickness_mm", None),
])
def test_malformed_field_types(inspection, group, key, value):
    inspection[group][key] = value
    assert assess_inspection(inspection)["measurement_sufficiency"]["status"] == (
        "INSUFFICIENT_DATA")


def test_empty_and_malformed_groups_are_reported():
    issues = assess_inspection({})["measurement_sufficiency"]["issues"]
    assert {item["field"] for item in issues} == set(REQUIRED_FIELDS)
    assert assess_inspection({"geometry": []})["verdict"] == "NOT_EVALUATED"
    with pytest.raises(ValueError, match="mapping"):
        assess_inspection([])


@pytest.mark.parametrize("position,depth,ligament,issue", [
    ("external_surface", 20, None, "flaw.depth_mm"),
    ("external_surface", 19, None, "flaw.sizing_uncertainty_mm"),
    ("embedded", 5, None, "flaw.ligament_mm"),
    ("embedded", 5, 15, "flaw.ligament_mm"),
])
def test_geometry_is_checked_without_an_assumed_k_solution(
    inspection, position, depth, ligament, issue,
):
    """Hand check: 19 + 1 = 20 wall; embedded 5 + 15 + 1 = 21 mm.

    An uncertain surface depth reaching the wall and an embedded crack with
    no remaining far ligament cannot be treated as strictly sub-wall flaws.
    These are geometric identities, not standard applicability thresholds.
    """
    inspection["flaw"].update(position=position, depth_mm=depth)
    if ligament is not None:
        inspection["flaw"]["ligament_mm"] = ligament
    result = assess_inspection(inspection)
    assert any(i["field"] == issue for i in result["measurement_sufficiency"]["issues"])


def test_weld_hydrogen_and_growth_require_their_own_basis(inspection):
    inspection["geometry"]["location"] = "haz"
    inspection["environment"]["hydrogen_service"] = True
    inspection["loads"]["cyclic"] = True
    inspection["growth"]["mechanism"] = "fatigue"
    result = assess_inspection(inspection)
    fields = {i["field"] for i in result["measurement_sufficiency"]["issues"]}
    assert {"fabrication.weld_type", "fabrication.distance_to_weld_mm",
            "environment.hydrogen_basis", "growth.parameters",
            "material.toughness_zone"} <= fields
    assert {"WELD_OR_DISCONTINUITY_REVIEW", "HYDROGEN_REVIEW", "GROWTH_REVIEW"} <= (
        set(result["applicability_flags"]))


def test_conditional_bases_can_complete_intake_but_not_calculation(inspection):
    inspection["geometry"]["location"] = "weld"
    inspection["material"]["toughness_zone"] = "weld"
    inspection["fabrication"].update(weld_type="butt", distance_to_weld_mm=0)
    inspection["loads"]["cyclic"] = True
    inspection["growth"].update(mechanism="fatigue", parameters={
        "basis": "Synthetic growth-law parameters withheld",
        "model": "not qualified", "values": {"synthetic_parameter": 1},
        "units": "explicitly synthetic",
    })
    result = assess_inspection(inspection)
    assert result["measurement_sufficiency"]["status"] == "COMPLETE_FOR_REVIEW"
    assert result["verdict"] == "NOT_EVALUATED"


def test_strength_and_temperature_inconsistency_are_flagged(inspection):
    inspection["material"].update(ultimate_mpa=200, toughness_temperature_c=20)
    issues = assess_inspection(inspection)["measurement_sufficiency"]["issues"]
    assert {"material.ultimate_mpa", "material.temperature_basis"} <= {
        i["field"] for i in issues}


def test_supported_shapes_and_orientations_are_not_silently_qualified(inspection):
    for shape in ("cylinder", "sphere", "plate", "other"):
        inspection["geometry"]["shape"] = shape
        for orientation in ("longitudinal", "circumferential", "other"):
            inspection["flaw"]["orientation"] = orientation
            result = assess_inspection(inspection)
            assert result["checks"][2]["status"] == "NOT_EVALUATED"
            assert "ANNEX_C_SOLUTION_UNQUALIFIED" in result["applicability_flags"]


def test_input_is_preserved_and_output_is_deterministic(inspection):
    before = deepcopy(inspection)
    assert assess_inspection(inspection) == assess_inspection(inspection)
    assert inspection == before


def test_documented_example_has_the_stated_answer():
    root = Path(__file__).resolve().parents[2]
    cfg = yaml.safe_load((root / "examples/workflows/api579-part9-crack-fad/input.yml").read_text(
        encoding="utf-8"))
    result = assess_inspection(cfg["part9_inspection"])
    assert result["measurement_sufficiency"]["status"] == "COMPLETE_FOR_REVIEW"
    assert len(result["checks"]) == 7  # Named readiness checks, not engineering results.
    assert result["acceptable"] is None


def test_through_wall_extent_must_match_measured_wall(inspection):
    """Geometric identity: a through-wall extent equals the measured wall."""
    inspection["flaw"]["position"] = "through_wall"
    result = assess_inspection(inspection)
    assert any(i["field"] == "flaw.depth_mm" for i in
               result["measurement_sufficiency"]["issues"])


@pytest.mark.parametrize("edition", ["2007", "2016", "2021", "unverified"])
def test_an_edition_label_cannot_qualify_methods(inspection, edition):
    inspection["standard_edition"] = edition
    result = assess_inspection(inspection)
    assert "EDITION_CROSSWALK_UNQUALIFIED" in result["applicability_flags"]
    assert all(c["status"] == "NOT_EVALUATED" for c in result["checks"])


def test_annex_references_use_the_edition_of_the_lettered_annexes(inspection):
    result = assess_inspection(inspection)
    editions = {check["id"]: check["edition"] for check in result["checks"]}
    assert editions == {
        "fad": "2016", "partial_safety_factors": "2016", "annex_c_k": "2007",
        "annex_d_reference_stress": "2007", "annex_e_residual": "2007",
        "growth": "2016", "published_example": "2016",
    }
    inspection["standard_edition"] = "2007"
    result = assess_inspection(inspection)
    assert "EDITION_CROSSWALK_UNQUALIFIED" in result["applicability_flags"]
    assert result["verdict"] == "NOT_EVALUATED"


@pytest.mark.parametrize("ligament,uncertainty,complete", [
    (15, 0, False), (14.999, 0, True), (14, 1, False), (13.999, 1, True),
])
def test_embedded_far_ligament_boundary(inspection, ligament, uncertainty, complete):
    # Hand sums: 5 + 15 + 0 = 20; 5 + 14.999 + 0 = 19.999 mm.
    # With 1 mm uncertainty: 5 + 14 + 1 = 20; 5 + 13.999 + 1 = 19.999 mm.
    inspection["flaw"].update(position="embedded", ligament_mm=ligament,
                              sizing_uncertainty_mm=uncertainty)
    result = assess_inspection(inspection)
    assert (result["measurement_sufficiency"]["status"] == "COMPLETE_FOR_REVIEW") == complete
    issues = result["measurement_sufficiency"]["issues"]
    assert any(item["field"] == "flaw.ligament_mm" for item in issues) == (not complete)


@pytest.mark.parametrize("shape", ["cylinder", "sphere"])
@pytest.mark.parametrize("radius", [None, 0, -1, float("nan"), float("inf"), 500])
def test_curved_geometry_requires_a_positive_finite_radius(inspection, shape, radius):
    inspection["geometry"].update(shape=shape, inside_radius_mm=radius)
    result = assess_inspection(inspection)
    complete = radius == 500
    assert (result["measurement_sufficiency"]["status"] == "COMPLETE_FOR_REVIEW") == complete
    assert any(item["field"] == "geometry.inside_radius_mm" for item in
               result["measurement_sufficiency"]["issues"]) == (not complete)


def test_cyclic_loading_cannot_claim_no_growth_even_with_parameters(inspection):
    inspection["loads"]["cyclic"] = True
    inspection["growth"]["parameters"] = {
        "model": "synthetic", "basis": "synthetic", "units": "synthetic",
        "values": {"parameter": 1},
    }
    result = assess_inspection(inspection)
    assert result["measurement_sufficiency"]["status"] == "INSUFFICIENT_DATA"
    assert any(item["field"] == "growth.mechanism" for item in
               result["measurement_sufficiency"]["issues"])


@pytest.mark.parametrize("mechanism", ["fatigue", "environmental", "combined", "unknown"])
@pytest.mark.parametrize("value", [float("nan"), float("inf"), float("-inf"), True, "1", 1])
def test_growth_parameters_require_finite_numbers_and_a_known_mechanism(
    inspection, mechanism, value,
):
    inspection["growth"].update(mechanism=mechanism, parameters={
        "model": "synthetic", "basis": "synthetic", "units": "synthetic",
        "values": {"parameter": value},
    })
    result = assess_inspection(inspection)
    complete = type(value) is int and mechanism != "unknown"
    assert (result["measurement_sufficiency"]["status"] == "COMPLETE_FOR_REVIEW") == complete
    if mechanism == "unknown":
        assert any(item["field"] == "growth.mechanism" for item in
                   result["measurement_sufficiency"]["issues"])
    assert "GROWTH_REVIEW" in result["applicability_flags"]
    json.dumps(result, allow_nan=False)


@pytest.mark.parametrize("position,depth,specialist", [
    ("external_surface", 5, False), ("internal_surface", 5, False),
    ("embedded", 5, True), ("through_wall", 20, True),
])
def test_flaw_position_controls_specialist_review(inspection, position, depth, specialist):
    inspection["flaw"].update(position=position, depth_mm=depth, ligament_mm=2)
    result = assess_inspection(inspection)
    assert result["measurement_sufficiency"]["status"] == "COMPLETE_FOR_REVIEW"
    assert ("FLAW_POSITION_REQUIRES_SPECIALIST_REVIEW" in result["applicability_flags"]) == (
        specialist)


@pytest.mark.parametrize("temperature,complete", [(-273.16, False), (-273.15, True), (0, True)])
def test_temperatures_cannot_be_below_absolute_zero(inspection, temperature, complete):
    inspection["loads"]["temperature_c"] = temperature
    inspection["material"]["toughness_temperature_c"] = temperature
    result = assess_inspection(inspection)
    assert (result["measurement_sufficiency"]["status"] == "COMPLETE_FOR_REVIEW") == complete
    if not complete:
        assert {"loads.temperature_c", "material.toughness_temperature_c"} <= {
            item["field"] for item in result["measurement_sufficiency"]["issues"]}


@pytest.mark.parametrize("days,complete", [(-1, True), (0, True), (1, False)])
def test_inspection_date_cannot_be_in_the_future(inspection, days, complete):
    as_of = date(2026, 10, 22)
    inspection["inspection"]["date"] = (as_of + timedelta(days=days)).isoformat()
    result = assess_inspection(inspection, as_of=as_of)
    assert (result["measurement_sufficiency"]["status"] == "COMPLETE_FOR_REVIEW") == complete
    if not complete:
        assert any(item["field"] == "inspection.date" for item in
                   result["measurement_sufficiency"]["issues"])


@pytest.mark.parametrize("as_of", [True, "2026-10-22", datetime(2026, 10, 22)])
def test_assessment_date_requires_an_explicit_calendar_date(inspection, as_of):
    with pytest.raises(ValueError, match="as_of"):
        assess_inspection(inspection, as_of=as_of)


def test_future_inspection_rejection_is_stable_before_the_inspection_date(inspection):
    inspection["inspection"]["date"] = "2026-10-24"
    first = assess_inspection(inspection, as_of=date(2026, 10, 22))
    second = assess_inspection(inspection, as_of=date(2026, 10, 23))
    assert first["measurement_sufficiency"]["status"] == "INSUFFICIENT_DATA"
    assert first == second
