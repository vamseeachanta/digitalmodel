# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Source audit boundaries for the five unmeasured pipe properties."""

import copy
import json

import pytest

from tests.installation.test_shore_pull_in import CAMPAIGN, driver


LABEL = "assumed, based on previous experience"
PIPE_ASSUMPTIONS = {
    "pipeline.dimensions.wall_thickness": ("m", "Diameter-to-wall class of the generic pipe"),
    "pipeline.coatings.corrosion.thickness": ("m", "Generic polymer corrosion coating"),
    "pipeline.coatings.corrosion.density": ("te/m3", "Generic polymer corrosion coating"),
    "pipeline.coatings.weight.0.thickness": ("m", "Generic concrete weight coating"),
    "pipeline.coatings.weight.0.density": ("te/m3", "Generic concrete weight coating"),
}
ORIGINAL = json.loads((CAMPAIGN / "case-table.json").read_text())


@pytest.fixture
def traced_table():
    """Build an allowed input table independently from the production audit."""
    table = copy.deepcopy(ORIGINAL)
    rows = []
    for path, (unit, basis) in PIPE_ASSUMPTIONS.items():
        value = table["basis"]
        for part in path.split("."):
            value = value[int(part)] if isinstance(value, list) else value[part]
        table["input_sources"][path] = {"label": LABEL, "basis": basis}
        rows.append({"input": path, "value": value, "unit": unit, "label": LABEL, "basis": basis})
    table["assumed_inputs"] = rows
    return table


def test_source_audit_accepts_all_public_inputs_and_only_five_labelled_properties(traced_table):
    rows = driver().audit_input_sources(traced_table)
    assert rows == traced_table["assumed_inputs"]
    assert {row["input"] for row in rows} == set(PIPE_ASSUMPTIONS)


@pytest.mark.parametrize("path", ["pipeline.dimensions.wall_thickness", "pipeline.dimensions.outer_diameter", "cases.0.friction"])
def test_source_audit_rejects_input_without_public_source_or_label(traced_table, path):
    traced_table["input_sources"][path] = {}
    with pytest.raises(ValueError, match="source|label"):
        driver().audit_input_sources(traced_table)


@pytest.mark.parametrize("path", sorted(set(ORIGINAL["input_sources"]) - set(PIPE_ASSUMPTIONS)))
def test_source_audit_rejects_assumed_input_of_every_other_kind(traced_table, path):
    traced_table["input_sources"][path] = {"label": LABEL, "basis": "Generic numerical scenario"}
    with pytest.raises(ValueError, match="Only pipe wall thickness or coating"):
        driver().audit_input_sources(traced_table)


@pytest.mark.parametrize("path", PIPE_ASSUMPTIONS)
def test_source_audit_rejects_assumed_input_without_basis(traced_table, path):
    traced_table["input_sources"][path].pop("basis")
    with pytest.raises(ValueError, match="basis"):
        driver().audit_input_sources(traced_table)


@pytest.mark.parametrize("basis", ["", "  ", None])
def test_source_audit_rejects_empty_basis(traced_table, basis):
    traced_table["input_sources"][next(iter(PIPE_ASSUMPTIONS))]["basis"] = basis
    with pytest.raises(ValueError, match="basis"):
        driver().audit_input_sources(traced_table)


@pytest.mark.parametrize("field", ["url", "retrieval_date", "record_count", "sha256"])
def test_source_audit_keeps_public_metadata_required(traced_table, field):
    traced_table["sources"]["friction"].pop(field)
    with pytest.raises(ValueError, match="provenance"):
        driver().audit_input_sources(traced_table)


@pytest.mark.parametrize("path", ["pipeline.dimensions.outer_diameter", "equipment.vessel.properties.loa", "equipment.vessel.properties.beam", "equipment.vessel.properties.draft"])
def test_source_audit_keeps_public_dataset_for_other_physical_anchors(traced_table, path):
    traced_table["input_sources"][path]["source"] = "defaults"
    with pytest.raises(ValueError, match="public dataset"):
        driver().audit_input_sources(traced_table)


@pytest.mark.parametrize("path", PIPE_ASSUMPTIONS)
def test_unlabelled_pipe_property_cannot_substitute_a_repository_source(traced_table, path):
    traced_table["input_sources"][path] = {"source": "defaults"}
    traced_table["assumed_inputs"] = [row for row in traced_table["assumed_inputs"] if row["input"] != path]
    with pytest.raises(ValueError, match="public dataset"):
        driver().audit_input_sources(traced_table)


@pytest.mark.parametrize("path", PIPE_ASSUMPTIONS)
def test_absent_pipe_measurement_cannot_claim_the_public_dataset(traced_table, path):
    traced_table["input_sources"][path] = {"source": "dataset"}
    traced_table["assumed_inputs"] = [row for row in traced_table["assumed_inputs"] if row["input"] != path]
    with pytest.raises(ValueError, match="public dataset.*measurement"):
        driver().audit_input_sources(traced_table)


@pytest.mark.parametrize("field,value", [("value", -1), ("unit", "unknown"), ("label", "scenario assumption"), ("basis", "different basis")])
def test_source_audit_rejects_inconsistent_basis_table(traced_table, field, value):
    traced_table["assumed_inputs"][0][field] = value
    with pytest.raises(ValueError, match="basis table"):
        driver().audit_input_sources(traced_table)


def test_case_table_records_exact_label_and_basis_for_five_properties():
    table = json.loads((CAMPAIGN / "case-table.json").read_text())
    assert len(table["assumed_inputs"]) == 5
    assert driver().audit_input_sources(table) == table["assumed_inputs"]
    for row in table["assumed_inputs"]:
        assert row["label"] == LABEL
        assert row["basis"].strip()
        assert "source" not in table["input_sources"][row["input"]]


def test_basis_table_is_retained_in_models_manifest_and_analysis(traced_table, tmp_path):
    campaign = tmp_path / "campaign"
    campaign.mkdir()
    (campaign / "case-table.json").write_text(json.dumps(traced_table))
    (campaign / "criteria.json").write_bytes((CAMPAIGN / "criteria.json").read_bytes())
    output = tmp_path / "output"
    driver().prepare(campaign, output)
    for name in ("models.json", "manifest.template.json", "preparation.json"):
        record = json.loads((output / name).read_text())
        assert record["provenance"]["assumed_inputs"] == traced_table["assumed_inputs"]
        assert record["provenance"]["public_dataset"] == traced_table["public_dataset"]
        assert record["provenance"]["input_sources"] == traced_table["input_sources"]
    page = (output / "analysis.html").read_text()
    assert "<th>Input</th><th>Value with unit</th><th>Label</th><th>Basis</th>" in page
    for row in traced_table["assumed_inputs"]:
        assert row["label"] in page
        assert row["input"] in page
        assert row["basis"] in page
        assert f'{row["value"]} {row["unit"]}' in page


def test_preparation_refuses_missing_source_before_writing_models(traced_table, tmp_path):
    traced_table["input_sources"]["cases.0.friction"] = {}
    (tmp_path / "case-table.json").write_text(json.dumps(traced_table))
    (tmp_path / "criteria.json").write_bytes((CAMPAIGN / "criteria.json").read_bytes())
    with pytest.raises(ValueError, match="source|label"):
        driver().prepare(tmp_path, tmp_path / "output")
    assert not (tmp_path / "output").exists()
