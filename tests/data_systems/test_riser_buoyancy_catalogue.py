# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Validate buoyancy catalogue structure using invented inputs only."""

import copy
import json
import shutil
from pathlib import Path

import pytest
import yaml

from digitalmodel.data_systems import engineering_databases as catalogues

ROOT = Path(__file__).resolve().parents[2]
DIRECTORY = ROOT / "data/riser_buoyancy"
UNITS = {
    "module_od_m": "m", "module_id_m": "m", "length_m": "m",
    "foam_density_kg_m3": "kg/m3", "depth_rating_m": "m",
    "mass_in_air_kg": "kg", "net_buoyancy_n": "N",
}
ROW = {
    "id": "fixture-1", "status": "synthetic", "citation_id": "fixture-source",
    "module_od_m": 0.8, "module_id_m": 0.4, "length_m": 2.0,
    "foam_density_kg_m3": 300.0, "depth_rating_m": 100.0,
    "mass_in_air_kg": 200.0, "net_buoyancy_n": 1000.0,
    "application": "drilling_riser",
}
CITATION = {
    "title": "Invented buoyancy fixture", "publisher": "Synthetic fixture",
    "url": None, "catalogue_number": "synthetic-fixture",
    "revision": "synthetic", "date": "2026-11-13", "page": "fixture 1",
    "table": None, "source_class": "synthetic_fixture",
    "reuse_terms": "Invented test data only; unsuitable for engineering use",
}


@pytest.fixture
def catalogue(tmp_path):
    directory = tmp_path / "catalogue"
    shutil.copytree(DIRECTORY, directory)
    data = json.loads((directory / "data.json").read_text(encoding="utf-8"))
    data["records"] = [copy.deepcopy(ROW)]
    sources = yaml.safe_load((directory / "sources.yml").read_text(encoding="utf-8"))
    sources["citations"] = {"fixture-source": copy.deepcopy(CITATION)}
    return directory, data, sources


def save_and_load(catalogue, *, allow_synthetic=True):
    directory, data, sources = catalogue
    (directory / "data.json").write_text(json.dumps(data), encoding="utf-8")
    (directory / "sources.yml").write_text(yaml.safe_dump(sources), encoding="utf-8")
    return catalogues.load_riser_buoyancy(directory, allow_synthetic=allow_synthetic)


def test_empty_production_catalogue():
    assert catalogues.load_riser_buoyancy(DIRECTORY) == []
    data = json.loads((DIRECTORY / "data.json").read_text(encoding="utf-8"))
    assert data["units"] == UNITS
    assert data["records"] == []


def test_synthetic_requires_opt_in_and_preserves_values(catalogue):
    with pytest.raises(catalogues.CatalogueValidationError, match="synthetic"):
        save_and_load(catalogue, allow_synthetic=False)
    with pytest.raises(catalogues.CatalogueValidationError, match="synthetic"):
        catalogues.load_riser_buoyancy(catalogue[0])
    assert save_and_load(catalogue) == [{**ROW, "citation": CITATION}]


@pytest.mark.parametrize("field", UNITS)
@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf"), True, "1", None])
def test_numbers_are_finite_and_typed(catalogue, field, value):
    catalogue[1]["records"][0][field] = value
    with pytest.raises(catalogues.CatalogueValidationError):
        save_and_load(catalogue)


@pytest.mark.parametrize("field", list(UNITS)[:-1])
@pytest.mark.parametrize("value", [-1.0, 0.0])
def test_constrained_numeric_bounds(catalogue, field, value):
    catalogue[1]["records"][0][field] = value
    if value == 0 and field in {"module_id_m", "mass_in_air_kg"}:
        assert save_and_load(catalogue)[0][field] == value
    else:
        with pytest.raises(catalogues.CatalogueValidationError):
            save_and_load(catalogue)


@pytest.mark.parametrize("force", [-1000.0, 0.0, 1000.0])
@pytest.mark.parametrize("application", ["drilling_riser", "distributed_lazy_wave", "buoyancy_can"])
def test_signed_force_and_applications(catalogue, force, application):
    catalogue[1]["records"][0].update(net_buoyancy_n=force, application=application)
    assert save_and_load(catalogue) == [{**ROW, "net_buoyancy_n": force,
                                       "application": application, "citation": CITATION}]


@pytest.mark.parametrize("field", ROW)
def test_every_row_field_is_required(catalogue, field):
    del catalogue[1]["records"][0][field]
    with pytest.raises(catalogues.CatalogueValidationError):
        save_and_load(catalogue)


@pytest.mark.parametrize("field", UNITS)
@pytest.mark.parametrize("mutation", ["wrong", "missing", "extra"])
def test_exact_units(catalogue, field, mutation):
    units = catalogue[1]["units"]
    if mutation == "missing":
        del units[field]
    else:
        units[field if mutation == "wrong" else "extra"] = "wrong"
    with pytest.raises(catalogues.CatalogueValidationError):
        save_and_load(catalogue)


@pytest.mark.parametrize("mutation", ["unknown", "duplicate", "unresolved", "orphan", "application"])
def test_row_and_citation_contract(catalogue, mutation):
    row = catalogue[1]["records"][0]
    if mutation == "unknown":
        row["extra"] = 1
    elif mutation == "duplicate":
        catalogue[1]["records"].append(copy.deepcopy(row))
    elif mutation == "unresolved":
        row["citation_id"] = "absent"
    elif mutation == "orphan":
        catalogue[2]["citations"]["orphan"] = copy.deepcopy(CITATION)
    else:
        row["application"] = "unknown"
    with pytest.raises(catalogues.CatalogueValidationError):
        save_and_load(catalogue)


@pytest.mark.parametrize("inner", [0.8, 1.0])
def test_inner_diameter_must_be_less_than_outer(catalogue, inner):
    catalogue[1]["records"][0]["module_id_m"] = inner
    with pytest.raises(catalogues.CatalogueValidationError, match="ID must be less than OD"):
        save_and_load(catalogue)


def test_shared_citation_definitions_do_not_drift():
    reference = json.loads((ROOT / "data/line_pipe/schema.json").read_text(encoding="utf-8"))
    schema = json.loads((DIRECTORY / "schema.json").read_text(encoding="utf-8"))
    for name in ("citation", "sources"):
        assert schema["$defs"][name] == reference["$defs"][name]


def test_registered_identifier_needs_explicit_engineering_checks(tmp_path, monkeypatch):
    schema = json.loads((ROOT / "data/line_pipe/schema.json").read_text(encoding="utf-8"))
    schema["properties"]["database"]["const"] = "future_catalogue"
    schema["properties"]["units"]["const"] = {}
    schema["properties"]["records"]["items"] = {"type": "object"}
    (tmp_path / "schema.json").write_text(json.dumps(schema), encoding="utf-8")
    data = {"schema_version": 1, "database": "future_catalogue", "units": {},
            "records": [{key: ROW[key] for key in ("id", "status", "citation_id")}]}
    sources = {"schema_version": 1, "citation_schema": "schema.json#/$defs/citation",
               "citations": {"fixture-source": CITATION}}
    (tmp_path / "data.json").write_text(json.dumps(data), encoding="utf-8")
    (tmp_path / "sources.yml").write_text(yaml.safe_dump(sources), encoding="utf-8")
    monkeypatch.setattr(catalogues, "DATABASES", catalogues.DATABASES | {"future_catalogue"})
    with pytest.raises(catalogues.CatalogueValidationError, match="no engineering checks for database"):
        catalogues.load_database("future_catalogue", tmp_path, allow_synthetic=True)
