# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Literal source-occurrence regressions for the offline YAML validator.

Position basis: PyYAML documentation, Mark and its error-position example
(https://pyyaml.org/wiki/PyYAMLDocumentation). Marks are zero-based; displayed
positions add one. Counts below enumerate literal key starts independently of
the YAML loader. Fixture scalar values are synthetic, not engineering inputs.
"""

import pytest

from digitalmodel.solvers.orcaflex.yaml_validator import (
    Severity,
    ValidationIssue,
    validate_orcaflex_yaml,
)


FORBIDDEN = "NumMooringLines"
MAXIMUM = "ImplicitVariableMaxTimeStep"


def validate(tmp_path, text):
    path = tmp_path / "model.yml"
    path.write_text(text, encoding="utf-8")
    return validate_orcaflex_yaml(path)


@pytest.mark.parametrize("text", [
    "General: {NumMooringLines: x, NumMooringLines: y}\n",
    "General: [{NumMooringLines: x}, {NumMooringLines: y}]\n",
])
def test_distinct_flow_keys_keep_their_columns(tmp_path, text):
    """Count each key start plus one; the first fixture has columns 11 and 31.

    The nested fixture adds sequence and mapping edges; identical messages must
    still represent both written keys. YAML 1.2.2 (2021), sections 3.2.1.1 and
    3.3: collection nodes and loading failures, including non-unique keys.
    """
    expected_columns = [
        offset + 1 for offset in range(len(text)) if text.startswith(FORBIDDEN, offset)
    ]
    result = validate(tmp_path, text)
    issues = [issue for issue in result.errors if issue.property == FORBIDDEN]
    assert len(issues) == len(expected_columns)
    assert [(issue.line, issue.column) for issue in issues] == [
        (1, column) for column in expected_columns
    ]
    assert all(issue.file == "model.yml" for issue in issues)
    assert not result.valid


def test_duplicate_flow_setters_keep_both_ambiguous_occurrences(tmp_path):
    """Two literal setter keys imply two warnings, at their counted columns.

    No final-dictionary value can resolve the duplicate setter context; the
    existing #716 contract requires warnings for both written occurrences.
    """
    text = f"General: {{{MAXIMUM}: 1, {MAXIMUM}: 1}}\n"
    result = validate(tmp_path, text)
    issues = [issue for issue in result.warnings if issue.property == MAXIMUM]
    assert not result.errors
    assert [(issue.line, issue.column) for issue in issues] == [
        (1, text.index(MAXIMUM) + 1), (1, text.rindex(MAXIMUM) + 1)
    ]
    assert all("ambiguous" in issue.message for issue in issues)


def test_alias_revisit_keeps_one_written_forbidden_key(tmp_path):
    """One anchored key has one source location despite two graph visits.

    YAML 1.2.2 (2021), section 3.2.2.2: aliases refer to an existing node.
    PyYAML composition does not provide separate alias-use marks for that key.
    """
    text = "Defaults: &settings {NumMooringLines: x}\nGeneral: *settings\n"
    result = validate(tmp_path, text)
    issues = [issue for issue in result.errors if issue.property == FORBIDDEN]
    assert [(issue.line, issue.column) for issue in issues] == [
        (1, text.index(FORBIDDEN) + 1)
    ]


@pytest.mark.parametrize("text", [
    "General: [\n",
    "General: *undefined\n",
    "General: {}\n---\nGeneral: {}\n",
])
def test_composition_failure_is_identified(tmp_path, text):
    """Malformed streams have no complete graph: YAML 1.2.2 (2021), 3.3.1."""
    result = validate(tmp_path, text)
    assert not result.valid
    assert any("YAML composition error" in issue.message for issue in result.errors)


@pytest.mark.parametrize("bad_value", [
    "!unsupported value",
    "{[unhashable]: value}",
    "!!bool invalid",
    "!!int ''",
    "!!float ''",
])
def test_construction_failure_retains_each_source_finding(tmp_path, bad_value):
    """Two written keys survive a later unavailable constructor or mapping key.

    YAML 1.2.2 (2021), 3.3.3-3.3.4 and PyYAML safe_load: composition can succeed
    while object construction fails. The two key starts are counted literally.
    """
    text = ("General: {NumMooringLines: x, NumMooringLines: y, "
            f"Custom: {bad_value}}}\n")
    result = validate(tmp_path, text)
    assert not result.valid
    assert any("YAML construction error" in issue.message for issue in result.errors)
    issues = [issue for issue in result.errors if issue.property == FORBIDDEN]
    assert [(issue.line, issue.column) for issue in issues] == [
        (1, text.index(FORBIDDEN) + 1), (1, text.rindex(FORBIDDEN) + 1)
    ]


@pytest.mark.parametrize("bad_value", [
    "!!int invalid", "!!float invalid", "!!timestamp 2000-99-99",
    "!!timestamp invalid",
    "!!bool invalid", "!!int ''", "!!float ''",
])
def test_scalar_constructor_conversion_failure_becomes_diagnostic(tmp_path, bad_value):
    """Invalid tagged lexemes cannot erase a preceding inherited-context warning.

    YAML 1.2.2 (2021), 3.3.3: a tag can be recognized without valid content.
    The fixture writes the setter on line two, after two spaces: column three.
    The deliberately impossible date is a synthetic invalid lexeme.
    """
    text = f"General:\n  {MAXIMUM}: 1\n  Custom: {bad_value}\n"
    result = validate(tmp_path, text)
    assert not result.valid
    assert any("YAML construction error" in issue.message for issue in result.errors)
    warnings = [issue for issue in result.warnings if issue.property == MAXIMUM]
    assert [(issue.line, issue.column) for issue in warnings] == [(2, 3)]
    assert "inherited context" in warnings[0].message


@pytest.mark.parametrize("bad_value", ["!!float invalid", "!!int ''", "!!float ''"])
def test_malformed_maximum_preserves_its_source_error(tmp_path, bad_value):
    """A malformed tagged setter yields both a source and construction finding."""
    result = validate(tmp_path, f"General:\n  {MAXIMUM}: {bad_value}\n")
    assert not result.valid
    assert any("YAML construction error" in issue.message for issue in result.errors)
    issues = [issue for issue in result.errors if issue.property == MAXIMUM]
    assert [(issue.line, issue.column) for issue in issues] == [(2, 3)]
    assert "finite positive number" in issues[0].message


@pytest.mark.parametrize("exception", [KeyError, IndexError])
def test_validation_errors_remain_outside_construction_boundary(
    tmp_path, monkeypatch, exception,
):
    """Validator programming errors must still propagate after loading succeeds."""
    from digitalmodel.solvers.orcaflex.yaml_validator import OrcaFlexYAMLValidator

    def fail_validation(*args, **kwargs):
        raise exception("synthetic validator failure")

    monkeypatch.setattr(OrcaFlexYAMLValidator, "_validate_flat_format", fail_validation)
    with pytest.raises(exception, match="synthetic validator failure"):
        validate(tmp_path, "General: {}\n")


def test_issue_positional_arguments_remain_compatible():
    """The optional column follows all existing positional fields."""
    issue = ValidationIssue(Severity.WARNING, "message", "model.yml", 2, MAXIMUM)
    assert issue.property == MAXIMUM
    assert issue.line == 2
    assert issue.column is None
