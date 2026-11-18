# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Qualify engineering strain against EN400 and explicit input boundaries."""

from fractions import Fraction
from pathlib import Path

import pytest
import yaml

from digitalmodel.naval_architecture import fundamentals


@pytest.fixture(scope="module")
def strain_case():
    """Read the unchanged, source-pinned chapter-5 worked answer."""
    path = (
        Path(__file__).resolve().parents[1]
        / "fixtures/test_vectors/naval_architecture/en400_materials.yaml"
    )
    data = yaml.safe_load(path.read_text(encoding="utf-8"))
    cases = [
        row for row in data["worked_examples"]
        if row["id"] == "en400_2020_ch5_ex5_2"
    ]
    assert len(cases) == 1
    case = cases[0]
    for record in (data, case):
        assert record["source_version"] == "Summer 2020"
        assert record["source_sha256"] == (
            "f8f4807492bffcc61cfe1b03bc7a0f457d7ab5209bb0404bba44a4e363be7cec"
        )
        assert record["source_url"] == (
            "https://www.usna.edu/NAOE/_files/documents/Courses/EN400/"
            "EN400_Course_Notes,_Summer_2020.pdf"
        )
    assert (case["printed_page"], case["pdf_page"]) == ("5-4", 195)
    assert (case["source_section"], case["source_example"]) == ("5.2.2", "5.2")
    assert case["input_units"] == {"original_length_ft": "ft", "elongation_in": "in"}
    assert case["output_units"] == {"engineering_strain": "ft/ft"}
    assert case["inputs"] == {"original_length_ft": 75.0, "elongation_in": 1.0}
    assert case["outputs"] == {"engineering_strain": 0.00111}
    assert case["absolute_tolerance"] == {"engineering_strain": 0.000005}
    assert case["use_as_test"] is True
    return case


def test_en400_example_5_2(strain_case):
    """Normalize the source's inches explicitly; retain its printed answer."""
    inputs = strain_case["inputs"]
    result = fundamentals.engineering_strain(
        inputs["original_length_ft"], inputs["elongation_in"] / 12.0
    )
    assert result == pytest.approx(
        strain_case["outputs"]["engineering_strain"],
        rel=0.0,
        abs=strain_case["absolute_tolerance"]["engineering_strain"],
    )


@pytest.mark.parametrize(
    "elongation,expected", [(0.0, 0.0), (-0.75, -0.01), (0.75, 0.01)]
)
def test_signed_elongation(elongation, expected):
    """Zero, shortening and extension preserve their dimensionless sign."""
    assert fundamentals.engineering_strain(75, elongation) == pytest.approx(expected)


@pytest.mark.parametrize("scale", [0.125, 1e-6, 1e6])
def test_uniform_scaling_preserves_strain(scale):
    """Multiplying both lengths by the same factor preserves the ratio."""
    result = fundamentals.engineering_strain(75 * scale, 0.75 * scale)
    assert result == pytest.approx(0.01)


def test_real_scalar_inputs_return_float():
    """Rational scalar lengths are accepted without introducing dependencies."""
    result = fundamentals.engineering_strain(Fraction(3, 2), Fraction(3, 200))
    assert isinstance(result, float)
    assert result == pytest.approx(0.01)


@pytest.mark.parametrize("length", [0.0, -1.0])
def test_nonpositive_original_length_rejected(length):
    """Original length must be positive before division."""
    with pytest.raises(ValueError, match="original_length_ft"):
        fundamentals.engineering_strain(length, 1.0)


@pytest.mark.parametrize("argument", ["original_length_ft", "elongation_ft"])
@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf")])
def test_nonfinite_lengths_rejected(argument, value):
    """Neither argument may supply NaN or either infinity."""
    lengths = {"original_length_ft": 75.0, "elongation_ft": 1.0}
    lengths[argument] = value
    with pytest.raises(ValueError, match=argument):
        fundamentals.engineering_strain(**lengths)


@pytest.mark.parametrize("argument", ["original_length_ft", "elongation_ft"])
@pytest.mark.parametrize("value", [True, False, None, "75", [], {}, 1j])
def test_nonreal_lengths_rejected(argument, value):
    """Reject booleans, text, containers and complex values without coercion."""
    lengths = {"original_length_ft": 75.0, "elongation_ft": 1.0}
    lengths[argument] = value
    with pytest.raises(TypeError, match=argument):
        fundamentals.engineering_strain(**lengths)
