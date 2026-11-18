# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check chapter-5 extraction metadata; no production formula is qualified."""

import math
from pathlib import Path
import re

import pytest
import yaml


CASE_PAGES = (
    ("en400_2020_ch5_ex5_1", "5-3", 194, "5.2.1", "5.1"),
    ("en400_2020_ch5_ex5_2", "5-4", 195, "5.2.2", "5.2"),
)
SOURCE_SHA256 = "f8f4807492bffcc61cfe1b03bc7a0f457d7ab5209bb0404bba44a4e363be7cec"
FIXTURE_PATH = (
    Path(__file__).resolve().parents[1]
    / "fixtures/test_vectors/naval_architecture/en400_materials.yaml"
)


@pytest.fixture(scope="module")
def material_vectors():
    """Require the source fixture and both records, without empty-case success."""
    data = yaml.safe_load(FIXTURE_PATH.read_text(encoding="utf-8"))
    assert isinstance(data, dict)
    assert data["source_type"] == "textbook"
    assert data["source_description"].strip()
    assert data["source_version"] == "Summer 2020"
    assert data["source_url"] == (
        "https://www.usna.edu/NAOE/_files/documents/Courses/EN400/"
        "EN400_Course_Notes,_Summer_2020.pdf"
    )
    assert re.fullmatch(r"[0-9a-f]{64}", data["source_sha256"])
    assert data["source_sha256"] == SOURCE_SHA256
    for field in ("equations", "inputs", "outputs", "references"):
        assert isinstance(data[field], list) and data[field]
    examples = data["worked_examples"]
    assert isinstance(examples, list)
    assert len(examples) == len(CASE_PAGES)
    assert {example["id"] for example in examples} == {
        case[0] for case in CASE_PAGES
    }
    return data


def assert_numeric_fields(values, units):
    """Require finite numbers and an explicit unit for each field."""
    assert isinstance(values, dict) and values
    assert isinstance(units, dict) and units.keys() == values.keys()
    for name, value in values.items():
        assert type(value) in (int, float), name
        assert math.isfinite(value), name
        assert isinstance(units[name], str) and units[name].strip(), name


@pytest.mark.parametrize(
    "case_id,printed_page,pdf_page,section,example_number",
    CASE_PAGES,
    ids=[case[0] for case in CASE_PAGES],
)
def test_en400_material_vector(
    material_vectors, case_id, printed_page, pdf_page, section, example_number
):
    """Require source locators, typed values, units and absolute tolerances."""
    example = next(
        row for row in material_vectors["worked_examples"] if row["id"] == case_id
    )
    assert isinstance(example["description"], str) and example["description"].strip()
    assert example["use_as_test"] is True
    for field in ("source_url", "source_version", "source_sha256"):
        assert example[field] == material_vectors[field]
    assert example["printed_page"] == printed_page
    assert type(example["pdf_page"]) is int and example["pdf_page"] == pdf_page
    assert example["source_section"] == section
    assert example["source_example"] == example_number
    assert_numeric_fields(example["inputs"], example["input_units"])
    assert_numeric_fields(example["outputs"], example["output_units"])
    tolerances = example["absolute_tolerance"]
    assert isinstance(tolerances, dict)
    assert tolerances.keys() == example["outputs"].keys()
    for name, tolerance in tolerances.items():
        assert type(tolerance) in (int, float), name
        assert math.isfinite(tolerance) and tolerance > 0, name
    assert isinstance(example["tolerance_basis"], str)
    assert example["tolerance_basis"].strip()
    if "intermediate_outputs" in example:
        assert_numeric_fields(
            example["intermediate_outputs"], example["intermediate_units"]
        )
