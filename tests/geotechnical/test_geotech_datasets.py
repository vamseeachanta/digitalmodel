"""Offline contracts for source-linked dataset discovery metadata."""

from dataclasses import FrozenInstanceError
import re
import socket

import pytest

from digitalmodel.geotechnical.database import Database, load_database


def legacy_data():
    """Return a database with no optional dataset metadata."""
    return {"meta": {}, "sources": [], "parameters": [], "gaps": [], "excluded": []}


def test_legacy_database_has_empty_immutable_datasets():
    """Preserve older files and positional construction."""
    for db in (load_database(legacy_data()), Database({}, {}, (), {}, (), (), {})):
        assert dict(db.datasets) == {}
        with pytest.raises(TypeError):
            db.datasets["new"] = None


@pytest.fixture
def data():
    """Use synthetic discovery metadata without numerical design inputs."""
    value = legacy_data()
    value["sources"] = [{
        "source_id": "source-1", "citation": "Synthetic source reference",
        "access": "doi:synthetic", "reuse_terms": "Terms not stated",
        "redistribution": "not-stated", "read_status": "unverified",
    }]
    value["datasets"] = [{
        "dataset_id": "test-cpt", "source_id": "source-1",
        "content": "CPT sounding records and index", "format": "CSV",
        "access_route": "doi:synthetic", "readiness": "metadata_only",
    }]
    return value


def test_dataset_lookup_retains_source_and_detaches_input(data):
    """Return immutable metadata and the original source by stable identity."""
    db = load_database(data)
    record = db.datasets["test-cpt"]
    assert record.dataset_id == "test-cpt"
    assert record.source_id == "source-1"
    assert record.source is db.sources["source-1"]
    assert record.source.citation == "Synthetic source reference"
    assert record.content == "CPT sounding records and index"
    assert record.format == "CSV"
    assert record.access_route == "doi:synthetic"
    assert record.size_description is None
    assert record.readiness == "metadata_only"
    with pytest.raises(KeyError):
        db.datasets["absent"]
    with pytest.raises(TypeError):
        db.datasets["new"] = record
    with pytest.raises(FrozenInstanceError):
        record.readiness = "ready"
    with pytest.raises(FrozenInstanceError):
        record.source.reuse_terms = "changed"
    data["datasets"][0]["content"] = "changed"
    data["sources"][0]["citation"] = "changed"
    assert record.content == "CPT sounding records and index"
    assert record.source.citation == "Synthetic source reference"


@pytest.mark.parametrize("status", ["yes", "no", "cite-only", "not-stated"])
def test_linking_source_never_promotes_rights_or_readiness(data, status):
    """Discovery alone cannot grant reuse or engineering qualification."""
    data["sources"][0]["redistribution"] = status
    record = load_database(data).datasets["test-cpt"]
    assert record.source.redistribution == status
    assert record.source.reuse_terms == "Terms not stated"
    assert record.source.read_status == "unverified"
    assert record.readiness == "metadata_only"


@pytest.mark.parametrize("field", [
    "dataset_id", "source_id", "content", "format", "access_route", "readiness",
])
@pytest.mark.parametrize("bad_value", [None, "", " \t", 0, [], {}])
def test_reject_malformed_dataset_cells(data, field, bad_value):
    """Reject malformed known fields without coercion or fallback."""
    data["datasets"][0][field] = bad_value
    with pytest.raises(ValueError, match=field):
        load_database(data)


@pytest.mark.parametrize("field", [
    "dataset_id", "source_id", "content", "format", "access_route", "readiness",
])
def test_reject_missing_dataset_cells(data, field):
    del data["datasets"][0][field]
    with pytest.raises(ValueError, match=field):
        load_database(data)


@pytest.mark.parametrize("section", [None, {}, "", (), 0])
def test_reject_invalid_datasets_section(data, section):
    data["datasets"] = section
    with pytest.raises(ValueError, match="datasets.*list"):
        load_database(data)


@pytest.mark.parametrize("entry", [None, "text", [], 0])
def test_reject_non_mapping_dataset(data, entry):
    data["datasets"] = [entry]
    with pytest.raises(ValueError, match="dataset.*mapping"):
        load_database(data)


@pytest.mark.parametrize("field,value,message", [
    ("source_id", "absent", "unknown source"),
    ("readiness", "ready", "readiness"),
])
def test_reject_unknown_reference_or_readiness(data, field, value, message):
    data["datasets"][0][field] = value
    with pytest.raises(ValueError, match=message):
        load_database(data)


@pytest.mark.parametrize("field", ["qualified", "source", "size_description(bytes)", 42])
def test_reject_unknown_dataset_field(data, field):
    data["datasets"][0][field] = True
    with pytest.raises(ValueError, match=re.escape(f"unknown dataset field {field!r}")):
        load_database(data)


def test_reject_duplicate_dataset_ids(data):
    data["datasets"].append(dict(data["datasets"][0]))
    with pytest.raises(ValueError, match="duplicate id"):
        load_database(data)


def test_explicit_empty_datasets_section(data):
    data["datasets"] = []
    assert dict(load_database(data).datasets) == {}


def test_optional_size_description_preserves_text(data):
    data["datasets"][0]["size_description"] = "not stated"
    assert load_database(data).datasets["test-cpt"].size_description == "not stated"


@pytest.mark.parametrize("value", [None, "", " ", 0, [], {}])
def test_reject_malformed_size_description(data, value):
    data["datasets"][0]["size_description"] = value
    with pytest.raises(ValueError, match="size_description"):
        load_database(data)


def test_seed_metadata_is_source_linked_and_offline(monkeypatch):
    """Expose the historical research-note entry without fetching source files."""
    def no_network(*args, **kwargs):
        raise AssertionError("dataset lookup must be offline")

    monkeypatch.setattr(socket, "socket", no_network)
    db = load_database()
    assert set(db.datasets) == {"usgs-cpt"}
    record = db.datasets["usgs-cpt"]
    assert record.source_id == "S11"
    assert record.source is db.sources["S11"]
    assert record.source.reuse_terms == "Marked CC0 1.0 Universal"
    assert record.source.redistribution == "yes"
    assert record.content == (
        "CPT soundings and index with position, elevation and water table"
    )
    assert record.format == "CSV per sounding plus index CSV; zipped legacy text files"
    assert record.access_route == "https://doi.org/10.5066/P13M3WVY"
    assert record.size_description == "not stated"
    assert record.readiness == "metadata_only"
