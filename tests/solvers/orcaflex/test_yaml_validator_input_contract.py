"""Offline input controls for issue #2519.

Duplicate expectations follow YAML 1.2.2 (2021), section 3.2.1.3,
Node Comparison: equal string keys in one mapping violate uniqueness.
Expected source lines are counted directly in each literal, independently
of the validator. Fixture scalars are synthetic labels, not physical data.
"""

import pytest

from digitalmodel.solvers.orcaflex.yaml_validator import validate_orcaflex_yaml


def validate(tmp_path, text):
    path = tmp_path / "model.yml"
    path.write_text(text, encoding="utf-8")
    return validate_orcaflex_yaml(path, strict_profile=True)


@pytest.mark.parametrize(("text", "key", "first", "repeated"), [
    ("General: {}\nGeneral: {}\n", "General", 1, 2),
    ("Environment:\n  WaterDepth: a\n  WaterDepth: b\n", "WaterDepth", 2, 3),
    ("Lines:\n- Name: a\n  Name: b\n", "Name", 2, 3),
    ("StageDuration: []\nStageDuration: []\n", "StageDuration", 1, 2),
    ("General: {UnitsSystem: SI, UnitsSystem: SI}\n", "UnitsSystem", 1, 1),
    ('General:\n  UnitsSystem: SI\n  "UnitsSystem": SI\n', "UnitsSystem", 2, 3),
    ("- includefile: a.yml\n  includefile: b.yml\n", "includefile", 1, 2),
])
def test_duplicate_keys_are_errors_before_construction(tmp_path, text, key, first, repeated):
    result = validate(tmp_path, text)
    assert not result.valid
    matches = [issue for issue in result.errors if "Duplicate mapping key" in issue.message]
    assert len(matches) == 1
    assert matches[0].property == key
    assert matches[0].file == "model.yml"
    assert matches[0].line == repeated
    assert f"first declared on line {first}" in matches[0].message


def test_each_repeated_assignment_is_reported(tmp_path):
    """Hand count: the first key is line 2; duplicates are lines 3 and 4."""
    result = validate(tmp_path, "General:\n  Custom: a\n  Custom: b\n  Custom: c\n")
    assert not result.valid
    assert {issue.line for issue in result.errors} == {3, 4}


def test_identical_keys_in_distinct_mappings_are_allowed(tmp_path):
    """Uniqueness is local: each of the two object maps has one Name key."""
    result = validate(tmp_path, "Lines:\n- Name: first\n- Name: second\n")
    assert result.valid


def test_shared_alias_does_not_duplicate_source_diagnostics(tmp_path):
    """Only line 3 repeats Custom; the alias reuses that same source node."""
    result = validate(tmp_path, "Defaults: &d\n  Custom: a\n  Custom: b\nGeneral: *d\n")
    assert not result.valid
    duplicates = [issue for issue in result.errors if "Duplicate mapping key" in issue.message]
    assert [issue.line for issue in duplicates] == [3]


def test_duplicate_in_included_file_is_rejected_by_directory_check(tmp_path):
    from digitalmodel.solvers.orcaflex.yaml_validator import OrcaFlexYAMLValidator

    (tmp_path / "master.yml").write_text("- includefile: general.yml\n", encoding="utf-8")
    (tmp_path / "general.yml").write_text("UnitsSystem: SI\nUnitsSystem: SI\n", encoding="utf-8")
    result = OrcaFlexYAMLValidator(strict_profile=True).validate_directory(tmp_path)
    assert not result.valid
    assert any(issue.file == "general.yml" and issue.line == 2 for issue in result.errors)


@pytest.mark.parametrize("key", ["true", "1", "1.0", "null", "2026-01-01", "[a, b]", "{a: b}"])
def test_property_keys_must_be_strings(tmp_path, key):
    """The property-name profile rejects typed keys, including Python collisions.

    YAML 1.2.2 section 3.2.1.1 permits arbitrary keys in general YAML; this
    validator's property-name profile is intentionally narrower. No conversion
    of a boolean, number, null, timestamp or collection can prove a property name.
    """
    result = validate(tmp_path, f"General:\n  {key}: ignored\n")
    assert not result.valid
    assert any(issue.line == 2 and "Mapping keys must be strings" in issue.message
               for issue in result.errors)


def test_boolean_and_integer_keys_cannot_collapse_to_one_key(tmp_path):
    """Independent Python rule: True == 1; both keys are invalid property names."""
    result = validate(tmp_path, "General:\n  true: first\n  1: second\n")
    assert not result.valid
    assert {issue.line for issue in result.errors} == {2, 3}


def test_quoted_typed_text_remains_a_string(tmp_path):
    result = validate(tmp_path, 'General:\n  "true": first\n  "1": second\n')
    assert result.valid


def test_yaml_merge_override_is_not_a_duplicate_assignment(tmp_path):
    """A merge and one local assignment are distinct source keys, not repeats.

    Retains the existing SafeLoader merge behavior; this is not proof that a
    native loader accepts merges or that inherited controls have been resolved.
    """
    result = validate(tmp_path, "Defaults: &d {UnitsSystem: SI}\nGeneral:\n  <<: *d\n  UnitsSystem: SI\n")
    assert result.valid


def test_duplicate_merge_directive_is_rejected(tmp_path):
    result = validate(tmp_path, "Defaults: &d {}\nGeneral:\n  <<: *d\n  <<: *d\n")
    assert not result.valid
    assert any(issue.property == "<<" and issue.line == 4 for issue in result.errors)


@pytest.mark.parametrize("text", ["null\n", "~\n", "---\n", "# comment only\n"])
def test_null_document_does_not_prove_an_input_contract(tmp_path, text):
    """YAML 1.2.2 section 7.2 Empty Nodes permits null; preflight needs content."""
    result = validate(tmp_path, text)
    assert not result.valid
    assert any("Document contains no model or fragment" in issue.message
               for issue in result.errors)


@pytest.mark.parametrize("section", ["General", "Environment"])
@pytest.mark.parametrize("value", ["", "null", "~", "true", "1", "wrong", "[]", "[null]", "[{Custom: wrong}]"])
def test_malformed_property_sections_cannot_be_skipped(tmp_path, section, value):
    """Orcina Text data files, Indentation: sections contain name/value pairs.

    A mapping or the repository's include-list convention is required. Scalars,
    nulls and other sequences provide neither; no default is inferred for them.
    """
    result = validate(tmp_path, f"{section}: {value}\n")
    assert not result.valid
    assert any(issue.property == section and issue.line == 1
               and "mapping or a nonempty include list" in issue.message
               for issue in result.errors)


@pytest.mark.parametrize("section", ["General", "Environment"])
def test_section_include_list_and_empty_mapping_remain_supported(tmp_path, section):
    """Repository format findings distinguish property maps from include lists."""
    (tmp_path / "fragment.yml").write_text("UnitsSystem: SI\n", encoding="utf-8")
    assert validate(tmp_path, f"{section}:\n- includefile: fragment.yml\n").valid
    assert validate(tmp_path, f"{section}: {{}}\n").valid


def test_property_default_is_not_confused_with_a_null_section(tmp_path):
    """Retain property null tokens; no physical value is inferred from them.

    The existing yaml_utils.OrcaFlexLoader contract distinguishes property
    defaults from script text. This structural slice does not resolve defaults.
    """
    assert validate(tmp_path, "Environment:\n  WaterDepth: ~\n").valid


@pytest.mark.parametrize("text", [
    "6DBuoys:\n  1: {}\n  2: {}\n  3: {}\n",
    "6DBuoys:\n  1.5: {}\n  2.5: {}\n",
    "includefile: first.yml\nincludefile: second.yml\n",
    "- includefile: first.yml\n  includefile: second.yml\n",
    "New: first\nfirst: {}\nNew: second\nsecond: {}\n",
])
def test_native_patterns_remain_accepted_by_default(tmp_path, text):
    """Numeric object names and repeated actions occur in tracked model inputs.

    Synthetic names preserve those source patterns without copying model data.
    Default acceptance does not prove that includes or actions were resolved.
    """
    for name in ("first.yml", "second.yml"):
        (tmp_path / name).write_text("UnitsSystem: SI\n", encoding="utf-8")
    path = tmp_path / "model.yml"
    path.write_text(text, encoding="utf-8")
    assert validate_orcaflex_yaml(path).valid
    assert not validate_orcaflex_yaml(path, strict_profile=True).valid


def test_strict_profile_is_explicit_for_file_and_directory(tmp_path):
    from digitalmodel.solvers.orcaflex.yaml_validator import OrcaFlexYAMLValidator

    path = tmp_path / "model.yml"
    path.write_text("6DBuoys:\n  1: {}\n  2: {}\n", encoding="utf-8")
    assert validate_orcaflex_yaml(path).valid
    assert not validate_orcaflex_yaml(path, strict_profile=True).valid
    assert not validate_orcaflex_yaml(tmp_path, strict_profile=True).valid
    assert OrcaFlexYAMLValidator().validate_directory(tmp_path).valid
    assert not OrcaFlexYAMLValidator(strict_profile=True).validate_file(path).valid


def test_custom_key_tag_diagnostic_names_the_tag(tmp_path):
    result = validate(tmp_path, "General:\n  !fixture Custom: ignored\n")
    assert not result.valid
    assert any(issue.line == 2 and "!fixture" in issue.message
               and "Mapping keys must be strings" in issue.message
               for issue in result.errors)


def test_comment_only_template_is_outside_strict_input_scope(tmp_path):
    """The defaults template is comments, not an automatically loaded model."""
    path = tmp_path / "model.yml"
    path.write_text("# General:\n#   UnitsSystem: SI\n", encoding="utf-8")
    default = validate_orcaflex_yaml(path)
    assert default.valid
    assert any("null/empty" in issue.message for issue in default.warnings)
    strict = validate_orcaflex_yaml(path, strict_profile=True)
    assert not strict.valid
    assert any("Document contains no model or fragment" in issue.message
               for issue in strict.errors)


@pytest.mark.parametrize("section", ["General", "Environment"])
@pytest.mark.parametrize("value", ["null", "wrong", "[]", "[null]"])
def test_malformed_merged_sections_retain_source_locations(tmp_path, section, value):
    """Inherited containers require the same shape as literal root sections."""
    result = validate(tmp_path, f"<<:\n  {section}: {value}\n")
    assert not result.valid
    assert any(issue.property == section and issue.line == 2
               and "mapping or a nonempty include list" in issue.message
               for issue in result.errors)


@pytest.mark.parametrize("section", ["General", "Environment"])
def test_nested_root_merges_validate_the_effective_section(tmp_path, section):
    result = validate(tmp_path, f"Defaults: &d\n  {section}: null\n<<:\n  <<: *d\n")
    assert not result.valid
    assert any(issue.property == section and issue.line == 2
               and "mapping or a nonempty include list" in issue.message
               for issue in result.errors)


@pytest.mark.parametrize("section", ["General", "Environment"])
def test_first_merge_source_takes_precedence_for_section_shape(tmp_path, section):
    result = validate(tmp_path, f"<<:\n- {section}: wrong\n- {section}: {{}}\n")
    assert not result.valid
    assert any(issue.property == section and issue.line == 2
               and "mapping or a nonempty include list" in issue.message
               for issue in result.errors)


@pytest.mark.parametrize("section", ["General", "Environment"])
@pytest.mark.parametrize("text", [
    "<<: {{{section}: {{}}}}\n",
    "Defaults: &d {{{section}: {{}}}}\n<<:\n  <<: *d\n",
    "<<: {{{section}: null}}\n{section}: {{}}\n",
    "{section}: {{}}\n<<: {{{section}: wrong}}\n",
    "<<: [{{{section}: {{}}}}, {{{section}: null}}]\n",
])
def test_valid_merged_sections_and_local_overrides_remain_supported(tmp_path, section, text):
    """Local sections override merges regardless of their source order."""
    assert validate(tmp_path, text.format(section=section)).valid


@pytest.mark.parametrize("section", ["General", "Environment"])
def test_merged_section_include_lists_remain_supported(tmp_path, section):
    (tmp_path / "fragment.yml").write_text("UnitsSystem: SI\n", encoding="utf-8")
    text = f"<<:\n  {section}:\n  - includefile: fragment.yml\n"
    assert validate(tmp_path, text).valid


@pytest.mark.parametrize("strict_profile", [False, True])
def test_root_merge_graph_limits_remain_enforced(tmp_path, strict_profile):
    path = tmp_path / "model.yml"
    path.write_text("&root {<<: *root}\n", encoding="utf-8")
    result = validate_orcaflex_yaml(path, strict_profile=strict_profile)
    assert not result.valid
    assert any("cyclic YAML alias graph" in issue.message for issue in result.errors)
