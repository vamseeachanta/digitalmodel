# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline acceptance checks for the research-only mechanics boundary."""

from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[3]
SPEC = ROOT / "docs/subsea/flexible_pipe_mechanics_boundary.yaml"
NOTE = SPEC.with_suffix(".md")
ROLES = {
    "carcass",
    "pressure_sheath",
    "pressure_armour",
    "tensile_armour",
    "outer_sheath",
    "anti_wear",
    "insulation",
}
UNKNOWN = {
    "operating_mbr",
    "storage_mbr",
    "internal_pressure_rating",
    "collapse_rating",
    "rating_conditions",
    "torsional_stiffness",
    "qualification_basis",
}


@pytest.fixture
def spec():
    return yaml.safe_load(SPEC.read_text(encoding="utf-8"))


def test_taxonomy_is_unique_and_maps_existing_roles(spec):
    roles = spec["layers"]
    assert len(roles) == len(ROLES)
    assert {row["role"] for row in roles} == ROLES
    from digitalmodel.subsea.flexible_pipes.cross_section import LayerType

    for row in roles:
        assert row["description"] and row["classification"] == "metadata"
        assert row["code_role"] in {role.value for role in LayerType} | {None}
        assert row["code_role"] == row["role"] or row["role"] == "insulation"
    assert next(r for r in roles if r["role"] == "insulation")["code_role"] is None


def test_source_references_resolve_with_locators(spec):
    catalog = yaml.safe_load((ROOT / spec["source_catalog"]).read_text())
    sources = {row["id"]: row for row in catalog["sources"]}
    assert set(spec["evidence"]) == {"construction", "polymers", "inspection"}
    for key, row in spec["evidence"].items():
        assert row["source_id"] == ("S3" if key == "inspection" else "S1")
        assert sources[row["source_id"]]["urls"][0].startswith("https://")
        assert row["locator"] and row["use"] and row["verified_on"]
    for row in spec["layers"]:
        assert row["evidence"] in {"construction", "polymers"}
    assert "12-14" in spec["evidence"]["construction"]["locator"]
    assert "37" in spec["evidence"]["polymers"]["locator"]


def test_metadata_and_future_mechanics_have_separate_boundaries(spec):
    assert {"layer", "material", "duty", "geometry"} <= set(spec["metadata"])
    assert set(spec["future_mechanics"]) == {
        "friction",
        "slip_contact",
        "hysteresis",
        "pressure_dependent_stiffness",
        "torsion",
        "annulus_state",
        "fatigue",
    }
    assert all(spec["future_mechanics"].values())
    assert spec["status"] == "research_only"
    assert spec["specimen"]["synthetic"] is True
    assert spec["specimen"]["engineering_use"] is False
    assert spec["specimen"]["export_ready"] is False
    assert not {"capacity", "design_pass", "results"} & spec["specimen"].keys()


@pytest.mark.parametrize("field", sorted(UNKNOWN))
def test_unavailable_qualification_has_no_numeric_or_standard_default(spec, field):
    assert set(spec["specimen"]["qualification"]) == UNKNOWN
    entry = spec["specimen"]["qualification"][field]
    assert entry["value"] is None
    assert entry["reason"] and entry["required_evidence"] and entry["unit"]
    assert spec["standards"]["calculation_criteria"] is None
    assert spec["standards"]["status"] == "unresolved"


def test_export_contract_units_and_missing_properties(spec):
    expected = {
        "OD": ("m", "m", 1),
        "ID": ("m", "m", 1),
        "MassPerUnitLength": ("kg/m", "te/m", 0.001),
        "EA": ("N", "kN", 0.001),
        "EI": ("N.m2", "kN.m2", 0.001),
        "GJ": (None, "kN.m2", None),
        "MinRadius": (None, "m", None),
        "TensionTorqueCoupling": (None, "not_established", None),
    }
    assert set(spec["export_requirements"]) == set(expected)
    for key, units in expected.items():
        row = spec["export_requirements"][key]
        assert (row["from_unit"], row["to_unit"], row["scale"]) == units
        assert row["status"] == ("indicative" if units[0] else "missing")
    for path in spec["code_contracts"].values():
        assert (ROOT / path).is_file()


def test_readable_roadmap_retains_stop_conditions(spec):
    text = NOTE.read_text(encoding="utf-8")
    for heading in (
        "Summary",
        "Revision history",
        "Boundary",
        "Evidence",
        "Export requirements",
        "Follow-up plan",
        "Limitations",
    ):
        assert f"## {heading}\n" in text
    assert (
        text.index("## Summary")
        < text.index("## Revision history")
        < text.index("## Boundary")
    )
    for marker in (
        "research_only",
        "missing versus zero",
        "operating versus storage",
        "incompatible pressure",
        "absent GJ",
        "BASIS-UNRESOLVED",
    ):
        assert marker in text
    assert SPEC.name in text and spec["source_catalog"] in text
