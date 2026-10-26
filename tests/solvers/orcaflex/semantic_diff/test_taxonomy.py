# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic policy tests, with literal expectations independent of the classifier.

Basis: docs/domains/orcaflex/SEMANTIC_DIFF_COMPARISON_POLICY.md, Examples.
The depth change is the hand calculation 100 + 1 = 101; view values are
arbitrary display settings. These numbers are test inputs, not solver defaults.
"""

import runpy

import pytest

from digitalmodel.solvers.orcaflex.semantic_diff import (
    MISSING,
    DiffCategory,
    classify_diff,
    is_semantically_equivalent,
)


@pytest.mark.parametrize(
    "section,key,before,after,expected",
    [
        ("Environment", "WaterDepth", 100, 101, "physics-significant"),
        ("Environment", "WaterDepth", 100, "deep", "loadability-significant"),
        (
            "Groups", "Structure", {"Child": "Parent", "Parent": "Model"},
            {"Child": "Absent", "Parent": "Model"},
            "reference-resolution-significant",
        ),
        ("Environment", "WaterDepth", 100, 100.0, "normalization-only"),
        ("Environment", "MultipleCurrentDataCanBeDefined", "No", False,
         "normalization-only"),
        ("General", "DefaultViewAzimuth", 270, 0, "ui-cosmetic-only"),
        ("General", "DefaultViewSize", 400, MISSING, "intentional-omission"),
    ],
)
def test_categories(section, key, before, after, expected):
    assert classify_diff(section, key, before, after).value == expected


@pytest.mark.parametrize("after", [101, 100.0, MISSING, None])
def test_unknown_change_is_significant(after):
    assert classify_diff("Environment", "Unregistered", 100, after) == (
        DiffCategory.PHYSICS_SIGNIFICANT
    )


@pytest.mark.parametrize(
    "section,key,before",
    [
        ("General", "DefaultViewAzimuth", 270),
        ("Environment", "WaterDepth", 100),
        ("Environment", "ImplicitVariableMaxTimeStep", 100),
    ],
)
def test_omission_requires_explicit_policy(section, key, before):
    assert classify_diff(section, key, before, MISSING) != (
        DiffCategory.INTENTIONAL_OMISSION
    )
    assert classify_diff("General", "DefaultViewSize", 400, MISSING) == (
        DiffCategory.INTENTIONAL_OMISSION
    )


@pytest.mark.parametrize("before,after", [("No", True), (False, "Yes")])
def test_changed_boolean_is_physics_significant(before, after):
    assert classify_diff(
        "Environment", "MultipleCurrentDataCanBeDefined", before, after
    ) == DiffCategory.PHYSICS_SIGNIFICANT


@pytest.mark.parametrize("before,after", [(100, True), (100, "100"), (100, None)])
def test_invalid_scalar_is_loadability_significant(before, after):
    assert classify_diff("Environment", "WaterDepth", before, after) == (
        DiffCategory.LOADABILITY_SIGNIFICANT
    )


@pytest.mark.parametrize("invalid", [float("nan"), float("inf"), -float("inf")])
def test_nonfinite_values_are_not_normalized(invalid):
    assert classify_diff("Environment", "WaterDepth", 100, invalid) == (
        DiffCategory.LOADABILITY_SIGNIFICANT
    )


@pytest.mark.parametrize("after", [MISSING, None, {}, {"Child": "Absent"}])
def test_missing_references_are_never_cosmetic(after):
    assert classify_diff("Groups", "Structure", {"Child": "Model"}, after) == (
        DiffCategory.REFERENCE_RESOLUTION_SIGNIFICANT
    )


@pytest.mark.parametrize("key", ["WaterDepth", "Density", "DefaultViewAzimuth"])
def test_registry_does_not_match_keys_across_sections(key):
    assert classify_diff("Unknown", key, 100, 101) == (
        DiffCategory.PHYSICS_SIGNIFICANT
    )


def test_unknown_boolean_numeric_equality_is_not_silently_accepted():
    equivalent, diffs = is_semantically_equivalent(
        {"Unknown": {"Flag": True}}, {"Unknown": {"Flag": 1}}
    )
    assert not equivalent
    assert [diff.category for diff in diffs] == [DiffCategory.PHYSICS_SIGNIFICANT]


def test_nested_change_and_list_order_are_significant():
    equivalent, diffs = is_semantically_equivalent(
        {"Unknown": {"Nested": {"Depths": [100, 101]}}},
        {"Unknown": {"Nested": {"Depths": [101, 100]}}},
    )
    assert not equivalent
    assert [diff.key for diff in diffs] == ["Nested"]


def test_omission_is_directional_and_null_is_present():
    for before, after in [(MISSING, 400), (400, None)]:
        assert classify_diff("General", "DefaultViewSize", before, after) == (
            DiffCategory.LOADABILITY_SIGNIFICANT
        )


def test_diffs_keep_omission_separate_from_accidental_loss():
    equivalent, diffs = is_semantically_equivalent(
        {"General": {"DefaultViewSize": 400, "DefaultViewAzimuth": 270}},
        {"General": {}},
    )
    assert not equivalent
    categories = {diff.key: diff.category for diff in diffs}
    assert categories == {
        "DefaultViewAzimuth": DiffCategory.LOADABILITY_SIGNIFICANT,
        "DefaultViewSize": DiffCategory.INTENTIONAL_OMISSION,
    }
    omitted = next(diff for diff in diffs if diff.key == "DefaultViewSize")
    assert omitted.generated_value is MISSING
    assert omitted.omission_reason


def test_safe_differences_are_all_retained():
    equivalent, diffs = is_semantically_equivalent(
        {"General": {"DefaultViewAzimuth": 270, "DefaultViewSize": 400},
         "Environment": {"WaterDepth": 100}},
        {"General": {"DefaultViewAzimuth": 0},
         "Environment": {"WaterDepth": 100.0}},
    )
    assert equivalent
    assert {diff.category.value for diff in diffs} == {
        "ui-cosmetic-only", "intentional-omission", "normalization-only"
    }


@pytest.mark.parametrize("section", ["General", "Environment", "Groups", "Unknown"])
def test_section_loss_and_addition_are_significant(section):
    document = {section: {"DefaultViewSize": 400}}
    assert not is_semantically_equivalent(document, {})[0]
    assert not is_semantically_equivalent({}, document)[0]


def test_equal_documents_have_no_diffs():
    document = {"Environment": {"WaterDepth": 100}, "Unknown": {"Value": None}}
    assert is_semantically_equivalent(document, document) == (True, [])


def test_equal_values_are_not_a_diff():
    with pytest.raises(ValueError, match="different"):
        classify_diff("Environment", "WaterDepth", 100, 100)


def test_offline_mapping_example(capsys):
    runpy.run_module(
        "digitalmodel.solvers.orcaflex.semantic_diff.example", run_name="__main__"
    )
    output = capsys.readouterr().out
    assert "policy_match=True" in output
    assert "intentional-omission" in output
    assert "normalization-only" in output


def test_nested_key_type_changes_are_unknown_and_significant():
    equivalent, diffs = is_semantically_equivalent(
        {"Unknown": {"Nested": {True: "value"}}},
        {"Unknown": {"Nested": {1: "value"}}},
    )
    assert not equivalent
    assert [diff.category for diff in diffs] == [DiffCategory.PHYSICS_SIGNIFICANT]


@pytest.mark.parametrize("key", ["WaterDepth", "SeabedOriginDepth", "Density"])
def test_changed_physical_scalar_is_never_cosmetic(key):
    assert classify_diff("Environment", key, 100, 101) == (
        DiffCategory.PHYSICS_SIGNIFICANT
    )


def test_unknown_view_like_key_is_significant():
    assert classify_diff("General", "DefaultViewUnregistered", 270, 0) == (
        DiffCategory.PHYSICS_SIGNIFICANT
    )
