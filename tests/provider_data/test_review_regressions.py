"""Review regressions through the public provenance and YAML interfaces."""

from datetime import date

import pytest
import yaml

from digitalmodel.provider_data import ProviderDataSource, SpecValue, load_provider_yaml


@pytest.mark.parametrize(
    "field,value",
    [
        ("revision_date", date(2023, 11, 17)),
        ("revision_date", ""),
        ("revision_date", "  "),
        ("revision_date", None),
        ("revision_date", 2023),
        ("sha256", None),
        ("sha256", 123),
        ("sha256", []),
        ("note", None),
        ("note", 123),
        ("note", []),
    ],
)
def test_provenance_rejects_invalid_text_fields(
    source_fields, parameters_payload, tmp_path, field, value
):
    source_fields[field] = value
    with pytest.raises(ValueError, match=field):
        ProviderDataSource(**source_fields)
    path = tmp_path / "invalid-provenance.yaml"
    path.write_text(yaml.safe_dump(parameters_payload), encoding="utf-8")
    with pytest.raises(ValueError, match=field):
        load_provider_yaml(path)


def test_provenance_preserves_vendor_revision_labels_and_optional_empty_text(
    source_fields,
):
    source_fields["revision_date"] = "Vendor revision B"
    source = ProviderDataSource(**source_fields, sha256="", note="")
    assert source.revision_date == "Vendor revision B"
    assert source.sha256 == source.note == ""


@pytest.mark.parametrize("note", [None, 123, []])
def test_specification_note_rejects_non_text(note, parameters_payload, tmp_path):
    with pytest.raises(ValueError, match="note"):
        SpecValue(value=1.0, unit="m", note=note)
    parameters_payload["specifications"]["mwd_m"]["note"] = note
    path = tmp_path / "invalid-note.yaml"
    path.write_text(yaml.safe_dump(parameters_payload), encoding="utf-8")
    with pytest.raises(ValueError, match="note"):
        load_provider_yaml(path)


@pytest.mark.parametrize(
    "mapping",
    [
        {"mwd_m": 123},
        {"mwd_m": None},
        {5: None},
        {None: "Environment.WaterDepth"},
        {"unknown_spec": "Environment.WaterDepth"},
        {"": "Environment.WaterDepth"},
        {"mwd_m": ""},
        {"mwd_m": "  "},
    ],
)
def test_mapping_rejects_invalid_entries(mapping, parameters_payload, tmp_path):
    path = tmp_path / "mapping.yaml"
    path.write_text(yaml.safe_dump(parameters_payload), encoding="utf-8")
    record = load_provider_yaml(path)
    record.orcaflex_mapping = mapping
    with pytest.raises(ValueError, match="orcaflex_mapping"):
        record.validate()
    parameters_payload["orcaflex_mapping"] = mapping
    path.write_text(yaml.safe_dump(parameters_payload), encoding="utf-8")
    with pytest.raises(ValueError, match="orcaflex_mapping"):
        load_provider_yaml(path)


def test_mapping_accepts_subset_including_list_specs(parameters_payload, tmp_path):
    mapping = {"bore_variants": "Bore variants: metadata only"}
    parameters_payload["orcaflex_mapping"] = mapping
    path = tmp_path / "mapping.yaml"
    path.write_text(yaml.safe_dump(parameters_payload), encoding="utf-8")
    record = load_provider_yaml(path)
    assert record.orcaflex_mapping == mapping
    record.validate()
