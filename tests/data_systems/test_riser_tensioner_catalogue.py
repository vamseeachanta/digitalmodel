# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise the tensioner contract with invented values only."""
import copy
import json
import shutil
from pathlib import Path

import pytest
import yaml

from digitalmodel.data_systems import engineering_databases as catalogues

ROOT = Path(__file__).resolve().parents[2]
DIRECTORY = ROOT / "data/riser_tensioners"
UNITS = {"capacity_n": "N", "stroke_m": "m", "count_typical": "1"}
ROW = {"id": "fixture-1", "status": "synthetic", "citation_id": "fixture-source",
       "kind": "wireline", "capacity_n": 1000.0, "stroke_m": 2.0, "count_typical": None}
CITATION = {
    "title": "Invented tensioner fixture", "publisher": "Synthetic fixture",
    "url": None, "catalogue_number": "synthetic-fixture", "revision": "synthetic",
    "date": "2026-11-16", "page": "fixture 1", "table": None,
    "source_class": "synthetic_fixture", "reuse_terms": "Invented test data only",
}


@pytest.fixture
def catalogue(tmp_path):
    directory = tmp_path / "catalogue"
    shutil.copytree(DIRECTORY, directory)
    data = json.loads((directory / "data.json").read_text(encoding="utf-8"))
    data["records"] = [copy.deepcopy(ROW)]
    sources = {"schema_version": 1, "citation_schema": "schema.json#/$defs/citation",
               "citations": {"fixture-source": copy.deepcopy(CITATION)}}
    return directory, data, sources


def load(catalogue, **kwargs):
    directory, data, sources = catalogue
    (directory / "data.json").write_text(json.dumps(data), encoding="utf-8")
    (directory / "sources.yml").write_text(yaml.safe_dump(sources), encoding="utf-8")
    return catalogues.load_riser_tensioners(directory, **kwargs)


def test_empty_production_and_shared_definitions():
    assert catalogues.load_riser_tensioners(DIRECTORY) == []
    data = json.loads((DIRECTORY / "data.json").read_text(encoding="utf-8"))
    sources = yaml.safe_load((DIRECTORY / "sources.yml").read_text(encoding="utf-8"))
    assert data["units"] == UNITS and data["records"] == [] and sources["citations"] == {}
    schema = json.loads((DIRECTORY / "schema.json").read_text(encoding="utf-8"))
    reference = json.loads((ROOT / "data/line_pipe/schema.json").read_text(encoding="utf-8"))
    for name in ("citation", "sources"):
        assert schema["$defs"][name] == reference["$defs"][name]


@pytest.mark.parametrize("kind", ["wireline", "direct_acting", "telescopic_joint"])
@pytest.mark.parametrize("count", [None, 1, 4])
def test_opt_in_and_exact_round_trip(catalogue, kind, count):
    catalogue[1]["records"][0].update(kind=kind, count_typical=count)
    with pytest.raises(catalogues.CatalogueValidationError, match="synthetic"):
        load(catalogue)
    assert load(catalogue, allow_synthetic=True) == [
        {**ROW, "kind": kind, "count_typical": count, "citation": CITATION}]


@pytest.mark.parametrize("field", ROW)
def test_required_fields(catalogue, field):
    del catalogue[1]["records"][0][field]
    with pytest.raises(catalogues.CatalogueValidationError):
        load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("field", UNITS)
@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf"), True, "1", 0, -1])
def test_numeric_rejections(catalogue, field, value):
    catalogue[1]["records"][0][field] = value
    with pytest.raises(catalogues.CatalogueValidationError):
        load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("mutation", ["unknown", "kind", "fractional", "null_capacity",
                                     "null_stroke", "duplicate", "unresolved", "orphan",
                                     "wrong_units", "missing_units", "extra_units"])
def test_contract_rejections(catalogue, mutation):
    row, units = catalogue[1]["records"][0], catalogue[1]["units"]
    if mutation in {"unknown", "kind", "fractional", "null_capacity", "null_stroke"}:
        key, value = {"unknown": ("extra", 1), "kind": ("kind", "unknown"),
                      "fractional": ("count_typical", 1.5), "null_capacity": ("capacity_n", None),
                      "null_stroke": ("stroke_m", None)}[mutation]
        row[key] = value
    elif mutation == "duplicate":
        catalogue[1]["records"].append(copy.deepcopy(row))
    elif mutation == "unresolved":
        row["citation_id"] = "absent"
    elif mutation == "orphan":
        catalogue[2]["citations"]["orphan"] = copy.deepcopy(CITATION)
    elif mutation == "missing_units":
        del units["capacity_n"]
    else:
        units["extra" if mutation == "extra_units" else "capacity_n"] = "wrong"
    with pytest.raises(catalogues.CatalogueValidationError):
        load(catalogue, allow_synthetic=True)
