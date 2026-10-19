# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Structural regressions for property-only variation files; no solver import.

Source: Orcina, OrcaFlex online help, "Variation models", "Text data format"
and "Change tracking", accessed 2026-10-19:
https://www.orcina.com/webhelp/OrcaFlex/Content/html/Variationmodels.htm

Expected documents are literal applications of the documented name-keyed
modification syntax. Synthetic property values are opaque input sentinels,
not engineering coefficients or acceptance limits. No numerical equation or
physical validity is evaluated here. Input preservation is checked by exact
structural equality and byte equality, without a floating-point tolerance.
"""

from copy import deepcopy

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
    _OBJECT_SECTIONS,
)
from digitalmodel.solvers.orcaflex.modular_generator.writers.basefile import (
    build_variation_document,
    write_variation_model,
)


@pytest.mark.parametrize("section", _OBJECT_SECTIONS)
def test_named_lists_become_modification_mappings(tmp_path, section):
    """Hand construction: remove Name, use its text as the mapping key."""
    overrides = {section: [{"Name": "Existing", "SelectedField": "changed"}]}
    original = deepcopy(overrides)

    document = build_variation_document(
        tmp_path / "master.yml", overrides, tmp_path / "changes" / "case.yml"
    )

    assert document == {
        "BaseFile": "../master.yml",
        section: {"Existing": {"SelectedField": "changed"}},
    }
    assert overrides == original
    document[section]["Existing"]["SelectedField"] = "later edit"
    assert overrides == original


def test_existing_mapping_and_property_tables_are_preserved(tmp_path):
    """Only the object collection changes syntax; property tables stay lists."""
    properties = {"SelectedTable": [["first", "second"]], "OptionalField": None}
    overrides = {"6DBuoys": {"Body": properties}}
    original = deepcopy(overrides)
    document = build_variation_document(
        tmp_path / "master.dat", overrides, tmp_path / "case.yml"
    )
    assert document["6DBuoys"] == {"Body": properties}
    document["6DBuoys"]["Body"]["SelectedTable"][0].append("later edit")
    assert overrides == original


def test_attached_object_fixture_emits_only_selected_fields(tmp_path):
    """Hand oracle: a Body field edit emits no line/shape connection edits.

    The fixture includes both a line endpoint and an attached shape. Byte
    equality proves source preservation only; native resolved-model equality
    remains NOT_EVALUATED until checked with the solver separately.
    """
    master = tmp_path / "master.yml"
    model = {
        "6DBuoys": [{"Name": "Body", "SelectedField": "original"}],
        "Lines": [{"Name": "Sling", "EndAConnection": "Body"}],
        "Shapes": [{"Name": "Attachment", "Connection": "Body"}],
    }
    master.write_text(yaml.safe_dump(model), encoding="utf-8")
    before = master.read_bytes()
    overrides = {"6DBuoys": [{"Name": "Body", "SelectedField": "changed"}]}
    out = tmp_path / "changes" / "case.yml"

    write_variation_model(master, overrides, out)

    assert yaml.safe_load(out.read_text(encoding="utf-8")) == {
        "BaseFile": "../master.yml",
        "6DBuoys": {"Body": {"SelectedField": "changed"}},
    }
    assert master.read_bytes() == before
    assert yaml.safe_load(master.read_text(encoding="utf-8")) == model


@pytest.mark.parametrize(
    "objects",
    [
        [{"SelectedField": "changed"}],
        [None],
        ["Body"],
        [{"Name": ""}],
        [{"Name": "   "}],
        [{"Name": None}],
        [{"Name": True}],
        [{"Name": ["Body"]}],
        [{"Name": "Body"}, {"Name": "Body"}],
        {"": {}},
        {"   ": {}},
        {None: {}},
        {"Body": None},
        {"Body": []},
        {"Body": {"Name": "Renamed"}},
        {"Body": {"Name*": "Renamed"}},
        "Body",
        None,
    ],
)
def test_ambiguous_object_overrides_fail_before_output_creation(tmp_path, objects):
    out = tmp_path / "not-created" / "case.yml"
    with pytest.raises(ValueError, match="6DBuoys"):
        write_variation_model(tmp_path / "master.yml", {"6DBuoys": objects}, out)
    assert not out.parent.exists()


@pytest.mark.parametrize("directive", ["New", "Deleted", "Order"])
@pytest.mark.parametrize("as_list", [False, True])
def test_structural_directives_require_a_separate_creation_workflow(
    tmp_path, directive, as_list
):
    """Orcina's Text data format reserves these keys for structural operations."""
    objects = [{"Name": directive}] if as_list else {directive: {}}
    with pytest.raises(ValueError, match="6DBuoys"):
        build_variation_document(
            tmp_path / "master.yml", {"6DBuoys": objects}, tmp_path / "case.yml"
        )


@pytest.mark.parametrize("section", ["BaseFile", "BaseFileNoTracking", "IncludeFile", "includefile"])
def test_overrides_cannot_replace_or_extend_the_base_reference(tmp_path, section):
    with pytest.raises(ValueError, match="composition"):
        build_variation_document(
            tmp_path / "master.yml", {section: "other.yml"}, tmp_path / "case.yml"
        )


@pytest.mark.parametrize("objects", [[], {}])
def test_empty_object_override_is_an_empty_mapping(tmp_path, objects):
    document = build_variation_document(
        tmp_path / "master.yml", {"Lines": objects}, tmp_path / "case.yml"
    )
    assert document["Lines"] == {}


@pytest.mark.parametrize("alias", ["same", "symlink", "hardlink"])
def test_source_master_cannot_be_overwritten_through_an_alias(tmp_path, alias):
    master = tmp_path / "master.yml"
    master.write_text("General: {}\n", encoding="utf-8")
    before = master.read_bytes()
    out = master
    if alias == "symlink":
        out = tmp_path / "alias.yml"
        out.symlink_to(master)
    elif alias == "hardlink":
        out = tmp_path / "alias.yml"
        out.hardlink_to(master)

    with pytest.raises(ValueError, match="source model"):
        write_variation_model(master, {}, out)
    assert master.read_bytes() == before


def test_invalid_override_leaves_existing_destination_unchanged(tmp_path):
    out = tmp_path / "case.yml"
    out.write_text("previous content\n", encoding="utf-8")
    with pytest.raises(ValueError, match="Lines"):
        write_variation_model(tmp_path / "master.yml", {"Lines": [{"Length": []}]}, out)
    assert out.read_text(encoding="utf-8") == "previous content\n"


def test_artifact_guard_accepts_written_modifications_without_mutation(tmp_path):
    from digitalmodel.solvers.orcaflex.modular_generator.writers.basefile import (
        validate_variation_document,
    )

    path = tmp_path / "case.yml"
    write_variation_model(
        tmp_path / "master.yml", {"Lines": [{"Name": "Sling", "SelectedField": []}]}, path
    )
    document = yaml.safe_load(path.read_text(encoding="utf-8"))
    before = deepcopy(document)
    assert validate_variation_document(document) is None
    assert document == before


@pytest.mark.parametrize("section", _OBJECT_SECTIONS)
def test_artifact_guard_rejects_creation_lists_instead_of_normalizing(section):
    """An artifact list is evidence of creation syntax, even if a merge is clean."""
    from digitalmodel.solvers.orcaflex.modular_generator.writers.basefile import (
        validate_variation_document,
    )

    with pytest.raises(ValueError, match=section):
        validate_variation_document(
            {"BaseFile": "master.yml", section: [{"Name": "Existing"}]}
        )


@pytest.mark.parametrize(
    "document",
    [
        None,
        [],
        {},
        {"BaseFile": None},
        {"BaseFile": " "},
        {"BaseFile": ["master.yml"]},
        {"Lines": {}, "BaseFile": "master.yml"},
        {"BaseFile": "master.yml", "IncludeFile": "other.yml"},
        {"BaseFile": "master.yml", "includefile": "other.yml"},
        {"BaseFile": "master.yml", "BaseFileNoTracking": "other.yml"},
        {"BaseFile": "master.yml", "basefile": "other.yml"},
        {"BaseFile": "master.yml", "Lines": {"New": {}}},
        {"BaseFile": "master.yml", "Lines": {"Deleted": "Sling"}},
        {"BaseFile": "master.yml", "Lines": {"Order": ["Sling"]}},
        {"BaseFile": "master.yml", "Lines": {"Sling": {"Name": "Renamed"}}},
        {"BaseFile": "master.yml", "Lines": {"Sling": {"Name*": "Renamed"}}},
        {"BaseFile": "master.yml", None: {}},
        {"BaseFile": "master.yml", "Delete": "Body"},
        {"BaseFile": "master.yml", "Line": {"Name": "NewLine"}},
        {"BaseFile": "master.yml", "UnknownObjects": [{"Name": "Body"}]},
    ],
)
def test_artifact_guard_rejects_invalid_modification_documents(document):
    from digitalmodel.solvers.orcaflex.modular_generator.writers.basefile import (
        validate_variation_document,
    )

    with pytest.raises(ValueError):
        validate_variation_document(document)


@pytest.mark.parametrize("overrides", [[], "Lines", {None: {}}, {True: {}}])
def test_invalid_override_container_or_section_name_has_a_clear_error(tmp_path, overrides):
    with pytest.raises(ValueError, match="[Oo]verrides|section"):
        build_variation_document(tmp_path / "master.yml", overrides, tmp_path / "case.yml")


def test_unknown_section_cannot_bypass_the_writer_guard(tmp_path):
    with pytest.raises(ValueError, match="section"):
        build_variation_document(
            tmp_path / "master.yml", {"Line": {"Name": "NewLine"}}, tmp_path / "case.yml"
        )


def test_artifact_guard_is_exported_with_the_writers():
    from digitalmodel.solvers.orcaflex.modular_generator.writers import (
        validate_variation_document,
    )
    from digitalmodel.solvers.orcaflex.modular_generator.writers import basefile

    assert validate_variation_document is basefile.validate_variation_document


@pytest.mark.parametrize("overrides", [None, {}])
def test_no_overrides_emits_only_the_base_reference(tmp_path, overrides):
    assert build_variation_document(
        tmp_path / "master.dat", overrides, tmp_path / "case.yml"
    ) == {"BaseFile": "master.dat"}
