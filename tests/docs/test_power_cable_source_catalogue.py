"""Offline validation of the research note's bounded first slice."""

import copy
import json
import math
import re
from pathlib import Path
from urllib.parse import urlsplit

import jsonschema
import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2] / "data/cross_sections"
CATALOGUE = ROOT / "power_cable_source_catalogue.yaml"
SCHEMA = ROOT / "power_cable_source_catalogue.schema.json"
PROPERTIES = {
    "outer_diameter": "mm",
    "weight_in_air": "kg/m",
    "weight_in_seawater": "kg/m",
    "axial_stiffness": "MN",
    "terminated_axial_working_load_limit": "kN",
}


def load_catalogue():
    assert CATALOGUE.is_file(), "catalogue is absent"
    assert SCHEMA.is_file(), "schema is absent"
    return yaml.safe_load(CATALOGUE.read_text(encoding="utf-8"))


def validate_references(node, source_ids):
    if isinstance(node, dict):
        for key, value in node.items():
            if re.search(r"unverified|not read", key, re.I):
                raise ValueError("excluded status in key")
            if key in {"access", "status"} and re.search(
                r"unverified|not read", str(value), re.I
            ):
                raise ValueError("excluded source status")
            if key == "source_id" and value not in source_ids:
                raise ValueError("unknown source_id")
            if key == "source_ids" and not set(value) <= source_ids:
                raise ValueError("unknown source_ids")
            if key == "locator" and not value.strip():
                raise ValueError("empty locator")
            validate_references(value, source_ids)
    elif isinstance(node, list):
        for item in node:
            validate_references(item, source_ids)


def validate_sources(sources):
    ids = [source["id"] for source in sources]
    if len(set(ids)) != len(ids) or not set(ids) <= set(range(1, 17)):
        raise ValueError("duplicate or out-of-range source id")
    for source in sources:
        url = source["url"]
        if url is not None:
            parsed = urlsplit(url)
            if (parsed.scheme not in {"http", "https"} or not parsed.hostname
                    or parsed.username or parsed.password
                    or re.search(r"[\s<>]", url)):
                raise ValueError("invalid URL")
        if "doi" in source and not re.fullmatch(
            r"10\.[0-9]{4,9}/\S+", source["doi"]
        ):
            raise ValueError("invalid DOI")
    return set(ids)


def validate_numbers(data):
    rows = data["static_array_cable_33kv"]["rows"]
    sizes = [row["conductor_mm2"] for row in rows]
    if len(rows) != 11 or any(a >= b for a, b in zip(sizes, sizes[1:])):
        raise ValueError("static sizes must strictly increase across eleven rows")
    sets = data["dynamic_cable_66kv"]["property_sets"]
    if len(sets) != 2:
        raise ValueError("expected two dynamic property sets")
    for entry in sets:
        properties = entry["properties"]
        if set(properties) != set(PROPERTIES):
            raise ValueError("expected five dynamic properties")
        if any(properties[key]["unit"] != unit for key, unit in PROPERTIES.items()):
            raise ValueError("incorrect property unit")
        if properties["weight_in_seawater"]["value"] >= properties["weight_in_air"]["value"]:
            raise ValueError("submerged weight must be below air weight")
    for name, count in (("three_core_array_cable", 9), ("bonded_hose", 10)):
        orders = [layer["order"] for layer in data["layer_buildups"][name]["layers"]]
        if orders != list(range(1, count + 1)):
            raise ValueError("layer orders must be contiguous")


def validate_catalogue(data):
    schema = json.loads(SCHEMA.read_text(encoding="utf-8"))
    jsonschema.Draft202012Validator.check_schema(schema)
    jsonschema.Draft202012Validator(schema).validate(data)
    ids = validate_sources(data["sources"])
    validate_references(data, ids)
    validate_numbers(data)
    validate_finite(data)


def validate_finite(node):
    if isinstance(node, dict):
        for value in node.values():
            validate_finite(value)
    elif isinstance(node, list):
        for value in node:
            validate_finite(value)
    elif isinstance(node, (int, float)) and not isinstance(node, bool):
        if not math.isfinite(node):
            raise ValueError("non-finite number")


def test_catalogue_satisfies_schema_and_content_constraints():
    validate_catalogue(load_catalogue())


def test_validator_rejects_row_without_source_id():
    data = copy.deepcopy(load_catalogue())
    del data["static_array_cable_33kv"]["rows"][0]["source_id"]
    with pytest.raises(jsonschema.ValidationError, match="source_id"):
        validate_catalogue(data)


@pytest.mark.parametrize("defect", [
    "unknown_source", "wrong_unit", "submerged_weight", "layer_gap",
    "duplicate_source", "excluded_status", "extra_key", "bad_url", "bad_doi",
])
def test_validator_rejects_content_defects(defect):
    data = copy.deepcopy(load_catalogue())
    properties = data["dynamic_cable_66kv"]["property_sets"][0]["properties"]
    if defect == "unknown_source":
        data["static_array_cable_33kv"]["rows"][0]["source_id"] = 16
    elif defect == "wrong_unit":
        properties["axial_stiffness"]["unit"] = "mm"
    elif defect == "submerged_weight":
        properties["weight_in_seawater"]["value"] = properties["weight_in_air"]["value"]
    elif defect == "layer_gap":
        data["layer_buildups"]["bonded_hose"]["layers"][1]["order"] = 3
    elif defect == "duplicate_source":
        data["sources"][1]["id"] = data["sources"][0]["id"]
    elif defect == "excluded_status":
        data["sources"][0]["access"] = "unverified"
    elif defect == "extra_key":
        data["extra"] = None
    elif defect == "bad_url":
        data["sources"][0]["url"] = "https:///"
    else:
        data["sources"][0]["doi"] = "broken"
    with pytest.raises((ValueError, jsonschema.ValidationError)):
        validate_catalogue(data)
