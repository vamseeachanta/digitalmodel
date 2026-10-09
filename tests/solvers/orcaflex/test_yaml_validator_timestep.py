"""Issue 716: assignment-time context, not final mapping truthiness."""

import textwrap

import pytest
import yaml

from digitalmodel.solvers.orcaflex.yaml_validator import validate_orcaflex_yaml


MAXIMUM = "ImplicitVariableMaxTimeStep"
MODE = "ImplicitUseVariableTimeStep"
METHOD = "DynamicsSolutionMethod"
SECTIONS = [
    "ExpansionTables", "LineContactData", "MultibodyGroups", "PyModels",
    "RayleighDampingCoefficients", "StiffenerTypes",
]


def check(tmp_path, content):
    path = tmp_path / "model.yml"
    path.write_text(textwrap.dedent(content), encoding="utf-8")
    return validate_orcaflex_yaml(path)


def general(*rows):
    return "General:\n" + "".join(f"  {row}\n" for row in rows)


def assert_unresolved(result):
    assert not result.errors, [str(i) for i in result.errors]
    assert result.warnings
    assert any(
        any(word in i.message.lower() for word in
            ("context", "ambig", "unresolved", "inherit", "unsupported"))
        for i in result.warnings
    ), [str(i) for i in result.warnings]


@pytest.mark.parametrize("mode", ["true", "Yes", "'Yes'"])
def test_known_enabled_context_accepts_positive_maximum(tmp_path, mode):
    result = check(tmp_path, general(
        f"{METHOD}: Implicit time domain", f"{MODE}: {mode}", f"{MAXIMUM}: 0.125"))
    assert not result.errors
    assert not result.warnings


@pytest.mark.parametrize("mode", ["false", "No", "'No'"])
def test_explicit_disabled_prefix_is_error(tmp_path, mode):
    result = check(tmp_path, general(
        f"{METHOD}: Implicit time domain", f"{MODE}: {mode}", f"{MAXIMUM}: 0.125"))
    assert result.errors
    assert not any("does not exist" in i.message for i in result.errors)


@pytest.mark.parametrize("mode", ["'enabled'", "'anything'", "1", "null"])
def test_unsupported_mode_cannot_authorize_maximum(tmp_path, mode):
    result = check(tmp_path, general(
        f"{METHOD}: Implicit time domain", f"{MODE}: {mode}", f"{MAXIMUM}: 0.125"))
    assert result.errors or result.warnings
    assert not any("does not exist" in i.message for i in result.issues)


@pytest.mark.parametrize("value", ["true", "false", "'0.1'", "bad", "~",
                                      ".inf", "-.inf", ".nan", "0", "-0.2"])
def test_bad_maximum_is_not_a_supported_numeric_value(tmp_path, value):
    result = check(tmp_path, general(
        f"{METHOD}: Implicit time domain", f"{MODE}: true", f"{MAXIMUM}: {value}"))
    assert result.errors
    assert not any("does not exist" in i.message for i in result.errors)


@pytest.mark.parametrize("rows", [
    [f"{MAXIMUM}: 0.125"],
    [f"{MODE}: true", f"{MAXIMUM}: 0.125"],
    [f"{METHOD}: Implicit time domain", f"{MAXIMUM}: 0.125"],
    [f"{MAXIMUM}: 0.125", f"{MODE}: true"],
    [f"{MAXIMUM}: 0.125", f"{MODE}: false"],
    [f"{MAXIMUM}: 0.125", f"{METHOD}: Explicit time domain"],
    [f"{MODE}: true", f"{MAXIMUM}: 0.125", f"{METHOD}: Implicit time domain"],
])
def test_missing_or_only_later_context_warns(tmp_path, rows):
    assert_unresolved(check(tmp_path, general(*rows)))


@pytest.mark.parametrize("method", ["Explicit time domain", "Frequency domain"])
def test_incompatible_preceding_solution_method_errors(tmp_path, method):
    result = check(tmp_path, general(
        f"{METHOD}: {method}", f"{MODE}: true", f"{MAXIMUM}: 0.125"))
    assert result.errors
    assert not any("does not exist" in i.message for i in result.errors)


def test_later_disable_does_not_retroactively_change_assignment(tmp_path):
    # No duplicate keys: variable mode is inherited, therefore unresolved.
    result = check(tmp_path, general(
        f"{METHOD}: Implicit time domain", f"{MAXIMUM}: 0.125", f"{MODE}: false"))
    assert_unresolved(result)


def test_bare_fragment_requires_parent_section_context(tmp_path):
    assert_unresolved(check(tmp_path,
        f"{METHOD}: Implicit time domain\n{MODE}: true\n{MAXIMUM}: 0.125\n"))


@pytest.mark.parametrize("prefix", ["  - ", "  - - "])
def test_general_sequence_does_not_prove_mapping_context(tmp_path, prefix):
    indent = " " * len(prefix)
    result = check(tmp_path, f"General:\n{prefix}{METHOD}: Implicit time domain\n"
                   f"{indent}{MODE}: true\n{indent}{MAXIMUM}: 0.125\n")
    assert result.errors or result.warnings


def test_duplicate_maxima_retain_each_source_line(tmp_path):
    result = check(tmp_path, general(f"{MAXIMUM}: 0.125", f"{MAXIMUM}: 0.25"))
    assert {i.line for i in result.warnings if i.property == MAXIMUM} == {2, 3}


def test_constructor_error_preserves_available_source_findings(tmp_path):
    result = check(tmp_path, general(f"{MAXIMUM}: 0.125", "Custom: !unknown value"))
    assert result.errors
    assert any(i.property == MAXIMUM for i in result.warnings)


@pytest.mark.timeout(5)
def test_deep_yaml_returns_diagnostic_before_recursive_parser_failure_escapes(tmp_path):
    result = check(tmp_path, "General: " + "[" * 2000 + "0" + "]" * 2000)
    assert result.errors


def test_large_flat_table_is_not_an_alias_expansion(tmp_path):
    result = check(tmp_path, "ExpansionTables: [" + ",".join(["0"] * 100010) + "]")
    assert not result.errors


def test_compact_alias_expansion_remains_bounded(tmp_path):
    rows = ["ExpansionTables: &a0 [0]"]
    for i in range(1, 8):
        rows.append(f"Alias{i}: &a{i} [" + ",".join([f"*a{i-1}"] * 10) + "]")
    result = check(tmp_path, "\n".join(rows))
    assert any("traversal limit" in i.message for i in result.errors)


def test_nested_unrelated_mode_does_not_enable_general(tmp_path):
    result = check(tmp_path, general(
        f"{METHOD}: Implicit time domain", "Unrelated:", f"  {MODE}: true",
        f"{MAXIMUM}: 0.125"))
    assert_unresolved(result)


@pytest.mark.parametrize("section", ["Environment", "LineTypes", "Vessels"])
def test_known_wrong_section_errors(tmp_path, section):
    result = check(tmp_path, f"{section}:\n  {MAXIMUM}: 0.125\n")
    assert any(MAXIMUM in i.message or i.property == MAXIMUM for i in result.errors)
    assert not any("does not exist" in i.message for i in result.errors)


def test_intervening_include_invalidates_known_prefix(tmp_path):
    (tmp_path / "overlay.yml").write_text(f"{MODE}: false\n", encoding="utf-8")
    result = check(tmp_path, general(
        f"{METHOD}: Implicit time domain", f"{MODE}: true",
        "includefile: overlay.yml", f"{MAXIMUM}: 0.125"))
    assert_unresolved(result)


@pytest.mark.parametrize("rows", [
    [f"{MODE}: false", f"{MAXIMUM}: 0.125", f"{MODE}: true"],
    [f"{MODE}: true", f"{MAXIMUM}: 0.125", f"{MODE}: false"],
    [f"{MODE}: true", f"{MAXIMUM}: 0.125", f"{MAXIMUM}: 0.25"],
    [f"{METHOD}: Explicit time domain", f"{MODE}: true",
     f"{MAXIMUM}: 0.125", f"{METHOD}: Implicit time domain"],
])
def test_duplicate_relevant_keys_warn_before_dictionary_collapse(tmp_path, rows):
    assert_unresolved(check(tmp_path, general(*rows)))


def test_duplicate_general_mappings_warn(tmp_path):
    content = general(f"{MODE}: false") + general(
        f"{METHOD}: Implicit time domain", f"{MODE}: true", f"{MAXIMUM}: 0.125")
    assert_unresolved(check(tmp_path, content))


def test_merge_alias_cannot_silently_supply_context(tmp_path):
    content = f"""\
    Defaults: &settings
      {METHOD}: Implicit time domain
      {MODE}: true
    General:
      <<: *settings
      {MAXIMUM}: 0.125
    """
    assert_unresolved(check(tmp_path, content))


def test_shared_general_alias_warns(tmp_path):
    content = f"""\
    Defaults: &settings
      {METHOD}: Implicit time domain
      {MODE}: true
      {MAXIMUM}: 0.125
    General: *settings
    """
    assert_unresolved(check(tmp_path, content))


@pytest.mark.timeout(5)
def test_cyclic_alias_returns_diagnostic_instead_of_recursing(tmp_path):
    result = check(tmp_path, f"General: &cycle\n  Loop: *cycle\n  {MAXIMUM}: 0.125\n")
    assert any(any(word in i.message.lower() for word in
                   ("cyclic", "cycle", "alias", "ambig", "unsupported"))
               for i in result.issues)


@pytest.mark.parametrize("section", SECTIONS)
def test_documented_section_name_is_recognized(tmp_path, section):
    result = check(tmp_path, f"General:\n  UnitsSystem: SI\n{section}: []\n")
    assert not result.errors
    assert not any(section in i.message for i in result.warnings)


@pytest.mark.parametrize("section", SECTIONS)
def test_section_recognition_does_not_hide_nested_invalid_property(tmp_path, section):
    result = check(tmp_path,
        f"General:\n  UnitsSystem: SI\n{section}:\n  - Name: fixture\n    NumMooringLines: 2\n")
    assert any("NumMooringLines" in i.message for i in result.errors)


def test_unknown_section_warning_survives(tmp_path):
    result = check(tmp_path, "General:\n  UnitsSystem: SI\nMultibodyGroupz: []\n")
    assert any("MultibodyGroupz" in i.message for i in result.warnings)


def test_generator_preserves_and_reorders_maximum_then_validator_recognizes(tmp_path):
    from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
    from digitalmodel.solvers.orcaflex.modular_generator.builders.generic_builder import GenericModelBuilder
    from digitalmodel.solvers.orcaflex.modular_generator.schema import GenericModel, ProjectInputSpec

    properties = {MAXIMUM: 0.125, MODE: True, METHOD: "Implicit time domain"}
    spec = ProjectInputSpec(
        metadata={"name": "fixture", "description": "fixture", "structure": "generic",
                  "operation": "generic", "project": "fixture"},
        environment={"water": {"depth": 100, "density": 1.025},
                     "seabed": {"stiffness": {"normal": 100, "shear": 100}}},
        generic=GenericModel(general_properties=properties),
    )
    output = GenericModelBuilder(spec, BuilderContext()).build()
    settings = output["General"]
    assert settings[MAXIMUM] == properties[MAXIMUM]
    assert list(settings).index(MODE) < list(settings).index(MAXIMUM)
    assert spec.generic.general_properties == properties
    # The method arrives from the earlier base include in real generation.
    overlay = check(tmp_path, yaml.safe_dump(output, sort_keys=False))
    assert_unresolved(overlay)
    combined = {METHOD: "Implicit time domain", **settings}
    result = check(tmp_path, yaml.safe_dump({"General": combined}, sort_keys=False))
    assert not result.errors
    assert not result.warnings
