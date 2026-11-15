"""Public fender data files agree with the module; the vendor-table importer maps
rows to the schema (fictional input rows)."""

from __future__ import annotations

import csv
import importlib.util
from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics import fender_design as fd
from digitalmodel.hydrodynamics.berthing_energy import load_fender_catalogue

ROOT = Path(__file__).resolve().parents[3]
DATA = ROOT / "data" / "fender_design"


def _rows(path):
    with path.open(newline="", encoding="utf-8") as fh:
        return list(csv.DictReader(fh))


def test_sources_csv_matches_register():
    rows = _rows(DATA / "sources.csv")
    assert [r["source_id"] for r in rows] == list(fd.SOURCES)
    for r in rows:
        s = fd.SOURCES[r["source_id"]]
        assert (r["title"], r["edition"], r["verification"]) == (s.title, s.edition, s.verification)


def test_method_matrix_covers_methods():
    assert {r["method"] for r in _rows(DATA / "method_matrix.csv")} == set(fd.METHODS)


def test_catalogue_index_has_no_values_and_counts():
    rows = _rows(DATA / "catalogue_index.csv")
    assert rows
    for r in rows:
        assert int(r["rows"]) > 0
        assert r["values_held"] == "private vendor table"
        assert not any(k.endswith(("_knm", "_kn", "_kpa")) for k in r)


def test_illustrative_rows_are_fictional_and_load():
    cat = load_fender_catalogue(DATA / "illustrative_fenders.csv")
    assert all(f.fender_id.startswith("GEN-") and f.source == "fictional test row" for f in cat)


def test_schema_lists_importer_columns():
    spec = importlib.util.spec_from_file_location("imp", ROOT / "scripts" / "fender_design" / "import_vendor_tables.py")
    imp = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(imp)
    schema = yaml.safe_load((DATA / "database_schema.yaml").read_text(encoding="utf-8"))
    assert list(schema["columns"]) == list(imp.DATABASE_COLUMNS)


SOLID_HDR = ("record_id,vendor,product_line,fender_type,size_designation,height_mm,length_mm,compound_grade,"
             "rated_deflection_pct,rated_energy_knm,rated_reaction_kn,basis,perf_basis,tolerance_pct,weight_kg,"
             "test_standard,chart_end_deflection_pct,chart_end_reaction_kn,chart_end_energy_knm,source_id,"
             "page_or_table,value_method,conversion,units,accessed_on,notes")
FLOAT_HDR = ("record_id,vendor,product_line,fender_type,size_designation,diameter_mm,length_mm,initial_pressure_kpa,"
             "rated_deflection_pct,gea_knm,reaction_kn,hull_pressure_kpa,weight_kg,test_standard,source_id,"
             "page_or_table,value_method,conversion,units,accessed_on,notes")


def test_importer_maps_and_filters(tmp_path):
    src = tmp_path / "in"
    src.mkdir()
    (src / "sources.csv").write_text(
        "source_id,vendor,document_title,edition_or_date\nSRC-x,Example Co,Example catalogue,2026\n", encoding="utf-8")
    (src / "solid_rubber.csv").write_text(
        SOLID_HDR + "\n"
        "S1,Example Co,Cone,cone,C1000,1000,,G1,72,500,900,per-unit,RPD,10,1000,PIANC 2002,,,,SRC-x,p.1,table,,,2026-11-15,\n"
        "S2,Example Co,Arch,arch,A500,500,1000,G1,50,60,300,per-m,RPD,10,,PIANC 2002,,,,SRC-x,p.2,table,,,2026-11-15,\n"
        "S3,Example Co,Cone,cone,C800,800,,G1,72,,,per-unit,RPD,10,,,72,500,300,SRC-x,p.3,read from chart,,,2026-11-15,\n",
        encoding="utf-8")
    (src / "floating.csv").write_text(
        FLOAT_HDR + "\nF1,Example Co,Pneumatic,pneumatic,2000 x 3500,2000,3500,50,60,300,850,130,500,ISO 17357-1:2014,"
        "SRC-x,p.4,table,,,2026-11-15,\n", encoding="utf-8")
    spec = importlib.util.spec_from_file_location("imp", ROOT / "scripts" / "fender_design" / "import_vendor_tables.py")
    imp = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(imp)
    out = tmp_path / "out"
    assert imp.main(["--in", str(src), "--out", str(out)]) == 0
    db = _rows(out / "fender_database.csv")
    assert [r["record_id"] for r in db] == ["S1", "S2", "S3", "F1"]
    assert db[3]["internal_pressure_kpa"] == "50" and db[3]["perf_basis"] == "GEA"
    # chart end points are never imported as ratings
    assert db[2]["rated_energy_knm"] == "" and db[2]["rated_reaction_kn"] == ""
    cat = load_fender_catalogue(out / "fender_catalogue.csv")
    # per-metre arch and unrated chart row left out of the selection catalogue
    assert [f.fender_id for f in cat] == ["S1", "F1"]
    assert "Example catalogue (2026), p.4; read 2026-11-15" in cat[1].source
