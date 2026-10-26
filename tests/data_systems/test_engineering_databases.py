# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise catalogue contracts using invented values, never design inputs."""

import copy
import json
import shutil
from pathlib import Path

import pytest
import yaml
from jsonschema import Draft202012Validator

from digitalmodel.data_systems.engineering_databases import (
    CatalogueValidationError,
    load_database,
    load_flexible_pipe,
    load_line_pipe,
    load_soil_stiffness,
    load_structure_properties,
)

ROOT = Path(__file__).resolve().parents[2]
DIRECTORIES = {
    "line_pipe": ROOT / "data/line_pipe",
    "flexible_pipe": ROOT / "data/flexible_pipe",
    "structure_properties": ROOT / "data/riser_database/structure_properties",
    "soil_stiffness": ROOT / "data/soil_stiffness",
}
LOADERS = {
    "line_pipe": load_line_pipe,
    "flexible_pipe": load_flexible_pipe,
    "structure_properties": load_structure_properties,
    "soil_stiffness": load_soil_stiffness,
}
ROWS = {
    "line_pipe": {
        "od_m": 0.4, "wt_m": 0.02, "grade": "synthetic grade",
        "smys_mpa": 200.0, "smts_mpa": 300.0,
    },
    "flexible_pipe": {
        "id_m": 0.1, "od_m": 0.2, "axial_stiffness_n": 100000.0,
        "bending_stiffness_n_m2": 1000.0, "torsional_stiffness_n_m2": 2000.0,
        "dry_mass_kg_per_m": 20.0, "minimum_bend_radius_m": 2.0,
        "design_pressure_pa": 100000.0, "temperature_deg_c": 20.0,
        "stiffness_basis": "initial_tangent", "test_conditions": "invented fixture",
    },
    "structure_properties": {
        "section_type": "custom", "section_label": "synthetic section",
        "area_m2": 0.01, "iy_m4": 0.0001, "iz_m4": 0.0002,
        "torsion_constant_m4": 0.00005, "elastic_modulus_pa": 1000000000.0,
        "density_kg_per_m3": 1000.0, "dry_mass_kg_per_m": 10.0,
    },
    "soil_stiffness": {
        "soil_type": "clay", "drainage": "undrained", "loading": "monotonic",
        "direction": "vertical", "pipe_od_m": 0.4, "embedment_m": 0.1,
        "depth_top_m": 0.0, "depth_bottom_m": 1.0,
        "displacement_m": [0.0, 0.01, 0.02],
        "resistance_n_per_m": [0.0, 10.0, 15.0],
        "initial_secant_stiffness_n_per_m2": 1000.0,
        "test_conditions": "invented fixture; positive resistance magnitudes",
    },
}
CITATION = {
    "title": "Invented test fixture", "publisher": "Synthetic fixture",
    "url": None, "catalogue_number": "synthetic-fixture",
    "revision": "synthetic", "date": "2026-10-26", "page": "fixture 1",
    "table": None, "source_class": "synthetic_fixture",
    "reuse_terms": "Invented test data only; unsuitable for engineering use",
}


@pytest.fixture(params=DIRECTORIES)
def catalogue(request, tmp_path):
    """Copy an empty catalogue and add a clearly marked synthetic row."""
    database = request.param
    directory = tmp_path / database
    shutil.copytree(DIRECTORIES[database], directory)
    data = json.loads((directory / "data.json").read_text())
    row = dict(id="fixture-1", status="synthetic", citation_id="fixture-source")
    row.update(copy.deepcopy(ROWS[database]))
    data["records"] = [row]
    sources = yaml.safe_load((directory / "sources.yml").read_text())
    sources["citations"] = {"fixture-source": copy.deepcopy(CITATION)}
    return database, directory, data, sources


def save_and_load(catalogue, **kwargs):
    """Write test mutations and exercise the public loader."""
    database, directory, data, sources = catalogue
    (directory / "data.json").write_text(json.dumps(data))
    (directory / "sources.yml").write_text(yaml.safe_dump(sources))
    return load_database(database, directory, **kwargs)


@pytest.mark.parametrize("database", DIRECTORIES)
def test_empty_catalogue_and_schema(database):
    directory = DIRECTORIES[database]
    schema = json.loads((directory / "schema.json").read_text())
    Draft202012Validator.check_schema(schema)
    assert LOADERS[database](directory) == []
    assert json.loads((directory / "data.json").read_text())["records"] == []


def test_synthetic_requires_explicit_opt_in(catalogue):
    with pytest.raises(CatalogueValidationError, match="synthetic"):
        save_and_load(catalogue)
    records = save_and_load(catalogue, allow_synthetic=True)
    assert records[0]["id"] == "fixture-1"
    assert records[0]["citation"] == CITATION


@pytest.mark.parametrize("source_class", [
    "mill_catalogue", "manufacturer_catalogue", "public_paper", "public_thesis",
    "public_agency_report", "own_design_basis",
])
def test_clean_source_class_contract(catalogue, source_class):
    # Invented metadata tests admission syntax, not source authenticity or rights.
    catalogue[2]["records"][0]["status"] = "cited"
    citation = catalogue[3]["citations"]["fixture-source"]
    citation["source_class"] = source_class
    citation["url"] = "https://example.invalid/synthetic-fixture"
    citation["catalogue_number"] = None
    citation["page"] = None
    citation["table"] = "fixture table"
    assert save_and_load(catalogue)[0]["citation"]["source_class"] == source_class


@pytest.mark.parametrize("field", list(CITATION))
def test_every_citation_field_is_required(catalogue, field):
    del catalogue[3]["citations"]["fixture-source"][field]
    with pytest.raises(CatalogueValidationError):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("mutation", ["missing", "unresolved", "empty", "orphan"])
def test_row_citation_join_is_required(catalogue, mutation):
    row = catalogue[2]["records"][0]
    citations = catalogue[3]["citations"]
    if mutation == "missing":
        del row["citation_id"]
    elif mutation == "unresolved":
        row["citation_id"] = "absent"
    elif mutation == "empty":
        citations["fixture-source"] = {}
    else:
        citations["orphan"] = copy.deepcopy(CITATION)
    with pytest.raises(CatalogueValidationError):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("field,value", [
    ("date", "2026-02-30"), ("title", " "), ("revision", ""),
    ("source_class", "standard"), ("url", "file:fixture"),
    ("url", "https://"),
])
def test_invalid_citation(catalogue, field, value):
    catalogue[3]["citations"]["fixture-source"][field] = value
    with pytest.raises(CatalogueValidationError):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("fields", [("url", "catalogue_number"), ("page", "table")])
def test_citation_requires_address_and_locator(catalogue, fields):
    for field in fields:
        catalogue[3]["citations"]["fixture-source"][field] = None
    with pytest.raises(CatalogueValidationError):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("mutation", ["wrong", "missing", "extra"])
def test_units_are_exact(catalogue, mutation):
    units = catalogue[2]["units"]
    field = next(iter(units))
    if mutation == "wrong":
        units[field] = "ft"
    elif mutation == "missing":
        del units[field]
    else:
        units["extra"] = "m"
    with pytest.raises(CatalogueValidationError):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("mutation", ["duplicate", "unknown", "version", "database"])
def test_document_contract(catalogue, mutation):
    data = catalogue[2]
    if mutation == "duplicate":
        data["records"].append(copy.deepcopy(data["records"][0]))
    elif mutation == "unknown":
        data["records"][0]["unrecognised"] = 1
    elif mutation == "version":
        data["schema_version"] = 2
    else:
        data["database"] = "other"
    with pytest.raises(CatalogueValidationError):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("value", [float("nan"), float("inf"), -1.0, True, "1.0"])
def test_numeric_values_are_finite_and_typed(catalogue, value):
    field = next(key for key, item in ROWS[catalogue[0]].items() if isinstance(item, float))
    catalogue[2]["records"][0][field] = value
    with pytest.raises(CatalogueValidationError):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("status,source_class", [
    ("cited", "synthetic_fixture"), ("synthetic", "manufacturer_catalogue"),
])
def test_synthetic_status_cannot_be_disguised(catalogue, status, source_class):
    catalogue[2]["records"][0]["status"] = status
    catalogue[3]["citations"]["fixture-source"]["source_class"] = source_class
    with pytest.raises(CatalogueValidationError):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("database,changes", [
    ("line_pipe", {"wt_m": 0.2}),
    ("line_pipe", {"smts_mpa": 100.0}),
    ("flexible_pipe", {"id_m": 0.3}),
    ("structure_properties", {"dry_mass_kg_per_m": 11.0}),
    ("soil_stiffness", {"depth_bottom_m": -1.0}),
    ("soil_stiffness", {"depth_top_m": 2.0}),
    ("soil_stiffness", {"displacement_m": [0.0, 0.02, 0.01]}),
    ("soil_stiffness", {"resistance_n_per_m": [0.0, 10.0]}),
    ("soil_stiffness", {"resistance_n_per_m": [1.0, 10.0, 15.0]}),
    ("soil_stiffness", {"initial_secant_stiffness_n_per_m2": 500.0}),
])
def test_engineering_relationships(tmp_path, database, changes):
    directory = tmp_path / "catalogue"
    shutil.copytree(DIRECTORIES[database], directory)
    data = json.loads((directory / "data.json").read_text())
    row = dict(id="fixture-1", status="synthetic", citation_id="fixture-source")
    row.update(copy.deepcopy(ROWS[database]))
    row.update(changes)
    data["records"] = [row]
    sources = yaml.safe_load((directory / "sources.yml").read_text())
    sources["citations"] = {"fixture-source": copy.deepcopy(CITATION)}
    with pytest.raises(CatalogueValidationError):
        save_and_load((database, directory, data, sources), allow_synthetic=True)


def test_unknown_database(tmp_path):
    with pytest.raises(CatalogueValidationError, match="database"):
        load_database("unknown", tmp_path)


def test_missing_catalogue_files(tmp_path):
    with pytest.raises(CatalogueValidationError, match="cannot read"):
        load_line_pipe(tmp_path)


@pytest.mark.parametrize("filename,content", [
    ("data.json", "["), ("data.json", "null"), ("sources.yml", "["),
    ("sources.yml", "null"), ("schema.json", "{}"),
])
def test_malformed_documents(catalogue, filename, content):
    save_and_load(catalogue, allow_synthetic=True)
    (catalogue[1] / filename).write_text(content)
    with pytest.raises(CatalogueValidationError):
        load_database(catalogue[0], catalogue[1], allow_synthetic=True)


@pytest.mark.parametrize("filename", ["data.json", "sources.yml"])
def test_duplicate_mapping_keys_are_rejected(catalogue, filename):
    _, directory, _, _ = catalogue
    save_and_load(catalogue, allow_synthetic=True)
    path = directory / filename
    if filename.endswith("json"):
        path.write_text('{"schema_version": 1, "schema_version": 1}')
    else:
        path.write_text("schema_version: 1\nschema_version: 1\n")
    with pytest.raises(CatalogueValidationError, match="duplicate"):
        load_database(catalogue[0], directory, allow_synthetic=True)
