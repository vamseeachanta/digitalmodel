# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Part 9 intake tests; all numerical inputs are synthetic, not code limits.

The public practitioner checklist (undated, accessed 2026-10-20), questions
1-33, supplies field coverage only, not numerical acceptance criteria:
https://www.inspection-for-industry.com/api-579-part-9-crack-like-flaws-assessment.html
"""

from copy import deepcopy
import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.asset_integrity.part9_inspection import (
    FIELD_SPECS,
    assess_inspection,
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
    json.dumps(result, allow_nan=False)


@pytest.mark.parametrize("field", list(FIELD_SPECS))
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
    assert len(assess_inspection({})["measurement_sufficiency"]["issues"]) == len(FIELD_SPECS)
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
    """Hand check: 19 + 1 = 20 wall; embedded 5 + 15 = 20 wall.

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
    cfg = yaml.safe_load((root / "examples/workflows/api579-part9-crack-fad/input.yml").read_text())
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


@pytest.mark.parametrize("edition", ["2007", "2021", "unverified"])
def test_an_edition_label_cannot_qualify_methods(inspection, edition):
    inspection["standard_edition"] = edition
    result = assess_inspection(inspection)
    assert "EDITION_CROSSWALK_UNQUALIFIED" in result["applicability_flags"]
    assert all(c["status"] == "NOT_EVALUATED" for c in result["checks"])
