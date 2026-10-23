# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Oracles derived by hand from Orcina, Variation models (accessed 2026-10-22).

https://www.orcina.com/webhelp/OrcaFlex/Content/html/Variationmodels.htm
The Text data format example addresses VesselTypes/type/Draughts by name;
the opening example addresses Environment/WaveTrains by name. Synthetic
strings are opaque sentinels, with no engineering constants or tolerances.
Snapshot comparisons exercise an audit utility, never native solver behavior.
"""

from copy import deepcopy

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.writers.basefile import (
    build_variation_document,
    validate_variation_document,
)


@pytest.mark.parametrize(
    "section,value,expected",
    [
        (
            "Environment",
            {"WaveTrains": [{"Name": "Wave", "WaveHeight": "edit"}]},
            {"WaveTrains": {"Wave": {"WaveHeight": "edit"}}},
        ),
        (
            "VesselTypes",
            [{"Name": "Type", "Draughts": [{"Name": "Draught", "Mass": "edit"}]}],
            {"Type": {"Draughts": {"Draught": {"Mass": "edit"}}}},
        ),
        (
            "6DBuoys",
            [{"Name": "Body", "Wings": [{"Name": "Wing", "WingType": "edit"}]}],
            {"Body": {"Wings": {"Wing": {"WingType": "edit"}}}},
        ),
    ],
)
def test_nested_named_lists_become_modifications(tmp_path, section, value, expected):
    """Hand oracle: replace each Name-bearing list layer with its name key."""
    before = deepcopy(value)
    document = build_variation_document(
        tmp_path / "master.yml", {section: value}, tmp_path / "case.yml"
    )
    assert document[section] == expected
    assert value == before


@pytest.mark.parametrize(
    "objects", [[], [{"Name": "Wave"}], {"New": {}}, {"Wave": {"Name*": "other"}}]
)
def test_nested_artifact_creation_and_structural_operations_are_rejected(objects):
    with pytest.raises(ValueError, match="WaveTrains"):
        validate_variation_document(
            {"BaseFile": "master.yml", "Environment": {"WaveTrains": objects}}
        )


def test_nested_named_mapping_cannot_hide_list_creation_in_an_unknown_collection():
    with pytest.raises(ValueError, match="Nested"):
        validate_variation_document(
            {
                "BaseFile": "master.yml",
                "6DBuoys": {"Body": {"Nested": [{"Name": "Attachment"}]}},
            }
        )


@pytest.mark.parametrize(
    "objects", [[{"Name": "Wave"}, {}], [{"Name": "Wave"}, {"Name": "wave"}]]
)
def test_invalid_nested_writer_input_is_rejected(tmp_path, objects):
    with pytest.raises(ValueError, match="WaveTrains"):
        build_variation_document(
            tmp_path / "master.yml",
            {"Environment": {"WaveTrains": objects}},
            tmp_path / "case.yml",
        )


def test_empty_nested_collections_and_property_tables(tmp_path):
    document = build_variation_document(
        tmp_path / "master.yml",
        {
            "Environment": {"WaveTrains": [], "CurrentProfile": [["depth", "speed"]]},
        },
        tmp_path / "case.yml",
    )
    assert document["Environment"] == {
        "WaveTrains": {},
        "CurrentProfile": [["depth", "speed"]],
    }


@pytest.mark.parametrize(
    "text",
    [
        "BaseFile: master.yml\nLines: {}\nLines: {}\n",
        "BaseFile: master.yml\nLines:\n  Sling: {}\n  Sling: {}\n",
        "BaseFile: master.yml\nLines:\n  Sling:\n    Length: old\n    Length: edit\n",
        "BaseFile: master.yml\nLines: [{Name: Sling}]\n",
        "BaseFile: master.yml\nEnvironment:\n  WaveTrains: [{Name: Wave}]\n",
    ],
)
def test_artifact_loader_rejects_hidden_duplicates_and_creation(tmp_path, text):
    from digitalmodel.solvers.orcaflex.modular_generator.writers import (
        load_variation_document,
    )

    artifact = tmp_path / "case.yml"
    artifact.write_text(text, encoding="utf-8")
    with pytest.raises(ValueError):
        load_variation_document(artifact)


def test_artifact_loader_preserves_native_blank_and_default(tmp_path):
    from digitalmodel.solvers.orcaflex.modular_generator.writers import (
        load_variation_document,
    )

    artifact = tmp_path / "case.yml"
    artifact.write_text(
        "BaseFile: master.yml\nGeneral:\n  Script: \n  Default: ~\n", encoding="utf-8"
    )
    assert load_variation_document(artifact) == {
        "BaseFile": "master.yml",
        "General": {"Script": "", "Default": None},
    }


@pytest.mark.parametrize(
    "text",
    [
        "BaseFile: [unterminated\n",
        "BaseFile: master.yml\nGeneral: &same {}\nEnvironment: *same\n",
        "BaseFile: master.yml\nGeneral: &cycle {Self: *cycle}\n",
        "BaseFile: master.yml\n---\nBaseFile: other.yml\n",
    ],
)
def test_artifact_loader_rejects_invalid_or_aliased_yaml(tmp_path, text):
    from digitalmodel.solvers.orcaflex.modular_generator.writers import (
        load_variation_document,
    )

    artifact = tmp_path / "case.yml"
    artifact.write_text(text, encoding="utf-8")
    with pytest.raises(ValueError):
        load_variation_document(artifact)


def test_nested_collection_name_can_also_be_an_object_name(tmp_path):
    document = build_variation_document(
        tmp_path / "master.yml",
        {
            "6DBuoys": [{"Name": "Draughts", "SelectedField": "edit"}],
        },
        tmp_path / "case.yml",
    )
    assert document["6DBuoys"] == {"Draughts": {"SelectedField": "edit"}}


@pytest.fixture
def snapshots():
    """Hand oracle: copy the master and change only the selected scalar.

    The sling and shape stay attached to Body. The rejected snapshot is made
    independently by setting one connection to Fixed, reproducing the defect
    class in issue #2137 without pretending to resolve a native model.
    """
    reference = {
        "6DBuoys": [{"Name": "Body", "SelectedField": "old", "OtherField": "retain"}],
        "Lines": [
            {"Name": "Sling", "EndAConnection": "Body", "EndBConnection": "Fixed"}
        ],
        "Shapes": [{"Name": "Attachment", "Connection": "Body"}],
    }
    resolved = deepcopy(reference)
    resolved["6DBuoys"][0]["SelectedField"] = "edit"
    return reference, resolved, {("6DBuoys", 0, "SelectedField"): "edit"}


def test_resolved_snapshot_accepts_only_the_expected_change(snapshots):
    from digitalmodel.solvers.orcaflex.modular_generator.writers import (
        validate_resolved_variation,
    )

    reference, resolved, selected = snapshots
    before = deepcopy(snapshots)
    assert validate_resolved_variation(reference, resolved, selected) is None
    assert snapshots == before


@pytest.mark.parametrize(
    "defect", ["line", "shape", "other", "missing", "extra", "selected", "name"]
)
def test_resolved_snapshot_rejects_all_unselected_drift(snapshots, defect):
    from digitalmodel.solvers.orcaflex.modular_generator.writers import (
        validate_resolved_variation,
    )

    reference, resolved, selected = snapshots
    if defect == "line":
        resolved["Lines"][0]["EndAConnection"] = "Fixed"
    elif defect == "shape":
        resolved["Shapes"][0]["Connection"] = "Fixed"
    elif defect == "other":
        resolved["6DBuoys"][0]["OtherField"] = "drift"
    elif defect == "missing":
        del resolved["Shapes"]
    elif defect == "extra":
        resolved["Extra"] = "drift"
    elif defect == "name":
        resolved["6DBuoys"][0]["Name"] = "Recreated"
    else:
        resolved["6DBuoys"][0]["SelectedField"] = "wrong"
    with pytest.raises(ValueError):
        validate_resolved_variation(reference, resolved, selected)


@pytest.mark.parametrize(
    "invalid", ["reference", "resolved", "collection", "missing", "name", "bool_index"]
)
def test_snapshot_audit_rejects_unresolved_or_broad_selections(snapshots, invalid):
    from digitalmodel.solvers.orcaflex.modular_generator.writers import (
        validate_resolved_variation,
    )

    reference, resolved, selected = snapshots
    if invalid in {"reference", "resolved"}:
        (reference if invalid == "reference" else resolved)["BaseFile"] = "other.yml"
    elif invalid == "collection":
        selected = {("6DBuoys",): resolved["6DBuoys"]}
    elif invalid == "name":
        selected = {("6DBuoys", 0, "Name"): "Body"}
    elif invalid == "bool_index":
        selected = {("6DBuoys", False, "SelectedField"): "edit"}
    else:
        selected = {("6DBuoys", 0, "Missing"): "edit"}
    with pytest.raises(ValueError):
        validate_resolved_variation(reference, resolved, selected)


def test_snapshot_audit_covers_tables_order_and_types(snapshots):
    """Hand oracle: a table replacement permits no change to an adjacent flag."""
    from digitalmodel.solvers.orcaflex.modular_generator.writers import (
        validate_resolved_variation,
    )

    reference, _, _ = snapshots
    reference["General"] = {"Table": [["old"]], "Flag": True}
    resolved = deepcopy(reference)
    resolved["General"]["Table"] = [["edit"]]
    selected = {("General", "Table"): [["edit"]]}
    validate_resolved_variation(reference, resolved, selected)
    resolved["General"]["Flag"] = 1
    with pytest.raises(ValueError, match="Unselected"):
        validate_resolved_variation(reference, resolved, selected)
    resolved["General"]["Flag"] = True
    with pytest.raises(ValueError, match="overlap"):
        validate_resolved_variation(
            reference,
            resolved,
            {
                **selected,
                ("General", "Table", 0, 0): "edit",
            },
        )


def test_snapshot_audit_preserves_object_order(snapshots):
    from digitalmodel.solvers.orcaflex.modular_generator.writers import (
        validate_resolved_variation,
    )

    reference, _, _ = snapshots
    reference["Lines"].append({"Name": "Other", "EndAConnection": "Body"})
    resolved = deepcopy(reference)
    resolved["Lines"].reverse()
    with pytest.raises(ValueError, match="Unselected"):
        validate_resolved_variation(reference, resolved, {})


def test_worked_example(tmp_path, monkeypatch, capsys):
    """Execute the documented inputs, hand-built oracle and rejection example."""
    from pathlib import Path

    root = Path(__file__).resolve().parents[4]
    text = (root / "docs/domains/orcaflex/variation-override-safety.md").read_text(
        encoding="utf-8"
    )
    code = text.split("```python\n", 1)[1].split("```", 1)[0]
    monkeypatch.setenv("EXAMPLE_OUTPUT", str(tmp_path))
    exec(compile(code, "variation-override-safety.md", "exec"), {})
    assert capsys.readouterr().out.splitlines() == [
        "Detached connection: rejected",
        "Syntax: PASS; snapshot audit: PASS; native preservation: NOT_EVALUATED",
    ]


def test_template_reference_mode_uses_nested_modifications(tmp_path):
    """Orcina's opening example: select an existing wave train by name."""
    from digitalmodel.solvers.orcaflex.template_generator import TemplateGenerator

    master = tmp_path / "master.yml"
    source = tmp_path / "variation.yml"
    master.write_text("General: {}\n", encoding="utf-8")
    source.write_text(
        "Environment:\n  WaveTrains: [{Name: Wave, WaveHeight: edit}]\n",
        encoding="utf-8",
    )
    output = tmp_path / "out" / "case.yml"
    before = master.read_bytes(), source.read_bytes()
    result = TemplateGenerator().generate(master, source, output, as_reference=True)
    assert result["success"]
    from digitalmodel.solvers.orcaflex.modular_generator.writers import (
        load_variation_document,
    )

    assert load_variation_document(output) == {
        "BaseFile": "../master.yml",
        "Environment": {"WaveTrains": {"Wave": {"WaveHeight": "edit"}}},
    }
    assert (master.read_bytes(), source.read_bytes()) == before


@pytest.mark.parametrize("defect", ["master", "variation", "duplicate", "directive"])
def test_template_reference_mode_rejects_unsafe_output(tmp_path, defect):
    from digitalmodel.solvers.orcaflex.template_generator import TemplateGenerator

    master = tmp_path / "master.yml"
    source = tmp_path / "variation.yml"
    master.write_text("General: {}\n", encoding="utf-8")
    source.write_text("Lines: [{Name: Sling}]\n", encoding="utf-8")
    output = tmp_path / "out" / "case.yml"
    if defect == "master":
        output = master
    elif defect == "variation":
        output = source
    elif defect == "duplicate":
        source.write_text("Lines: {}\nLines: {}\n", encoding="utf-8")
    else:
        source.write_text("IncludeFile: other.yml\n", encoding="utf-8")
    before = master.read_bytes(), source.read_bytes()
    result = TemplateGenerator().generate(master, source, output, as_reference=True)
    assert result["success"] is False
    assert (master.read_bytes(), source.read_bytes()) == before
    if defect in {"duplicate", "directive"}:
        assert not output.exists()
