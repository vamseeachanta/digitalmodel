"""Public fender data files agree with the module; the vendor-table importer maps
rows to the schema (fictional input rows)."""

from __future__ import annotations

import csv
import dataclasses
import importlib.util
import re
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
    names = [f.name for f in dataclasses.fields(fd.Source)]
    assert list(rows[0]) == names
    for r in rows:
        s = fd.SOURCES[r["source_id"]]
        assert [r[n] for n in names] == [getattr(s, n) for n in names], r["source_id"]
        # date read: an ISO date for every source
        assert re.fullmatch(r"\d{4}-\d{2}-\d{2}", r["accessed_on"]), r["source_id"]


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
    assert [r["perf_basis"] for r in _rows(out / "fender_catalogue.csv")] == ["RPD", "GEA"]


def _importer():
    spec = importlib.util.spec_from_file_location("imp", ROOT / "scripts" / "fender_design" / "import_vendor_tables.py")
    imp = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(imp)
    return imp


def _two_bases(tmp_path):
    """The same fictional cone size rated on CV and on RPD, plus one pneumatic row."""
    src = tmp_path / "in"
    src.mkdir()
    (src / "sources.csv").write_text(
        "source_id,vendor,document_title,edition_or_date\nSRC-x,Example Co,Example catalogue,2026\n", encoding="utf-8")
    (src / "solid_rubber.csv").write_text(
        SOLID_HDR + "\n"
        "S1,Example Co,Cone,cone,C1000,1000,,G1,72,500,900,per-unit,RPD,10,1000,PIANC 2002,,,,SRC-x,p.1,table,,,2026-11-15,\n"
        "S1-CV,Example Co,Cone,cone,C1000,1000,,G1,72,450,850,per-unit,CV,10,1000,PIANC 2002,,,,SRC-x,p.1,table,,,2026-11-15,\n",
        encoding="utf-8")
    (src / "floating.csv").write_text(
        FLOAT_HDR + "\nF1,Example Co,Pneumatic,pneumatic,2000 x 3500,2000,3500,50,60,300,850,130,500,ISO 17357-1:2014,"
        "SRC-x,p.4,table,,,2026-11-15,\n", encoding="utf-8")
    return src


def test_importer_refuses_mixed_bases_and_exports_one(tmp_path):
    imp = _importer()
    src = _two_bases(tmp_path)
    out = tmp_path / "out"
    with pytest.raises(ValueError, match="cone: CV, RPD"):
        imp.main(["--in", str(src), "--out", str(out)])
    assert not (out / "fender_catalogue.csv").exists()
    assert imp.main(["--in", str(src), "--out", str(out), "--perf-basis", "CV"]) == 0
    rows = _rows(out / "fender_catalogue.csv")
    assert [(r["fender_id"], r["perf_basis"]) for r in rows] == [("S1-CV", "CV"), ("F1", "GEA")]
    assert "basis CV" in rows[0]["source"]
    with pytest.raises(ValueError):
        imp.catalogue_rows(imp.convert(src), "XYZ")


def test_case_loader_refuses_mixed_bases(tmp_path):
    import sys
    sys.path.insert(0, str(ROOT / "examples" / "hydrodynamics" / "fender_design"))
    from fender_case import catalogue_bases, check_one_basis, compute, load_case

    cat = tmp_path / "cat.csv"
    rows = _rows(DATA / "illustrative_fenders.csv")
    cols = list(rows[0]) + ["perf_basis"]
    for r in rows:
        r["perf_basis"] = "GEA" if r["fender_type"] == "pneumatic" else "CV"
    rows[1]["perf_basis"] = "RPD"           # a pneumatic row on another basis
    with cat.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=cols)
        w.writeheader()
        w.writerows(rows)
    case = load_case(ROOT / "examples" / "hydrodynamics" / "fender_design" / "generic_case.yaml")
    with pytest.raises(ValueError, match="pneumatic: GEA, RPD"):
        compute(case, cat, citations="record")
    # one basis per type (pneumatic GEA, foam CV) is accepted
    rows[1]["perf_basis"] = "GEA"
    with cat.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=cols)
        w.writeheader()
        w.writerows(rows)
    res = compute(case, cat, citations="record")
    assert res.perf_basis["GEN-PN-4"] == "GEA"
    assert catalogue_bases(DATA / "illustrative_fenders.csv") == {}   # no column: nothing to check
    check_one_basis(res.catalogue, {})


def test_size_summary_agrees_with_the_catalogue_index():
    """Guide revision 6: the database by vendor, then type, then size. The public summary holds
    no rated value, and its row counts are those of the catalogue index, type by type."""
    import csv
    from collections import Counter
    data = Path(__file__).resolve().parents[3] / "data" / "fender_design"
    with (data / "catalogue_size_summary.csv").open(newline="", encoding="utf-8") as fh:
        summary = list(csv.DictReader(fh))
    assert set(summary[0]) == {"vendor", "fender_type", "rows", "size_measure", "rows_with_size",
                               "smallest_size_mm", "largest_size_mm", "source_ids", "accessed_on"}
    index: Counter = Counter()
    with (data / "catalogue_index.csv").open(newline="", encoding="utf-8") as fh:
        for r in csv.DictReader(fh):
            for part in r["fender_types"].split(";"):
                name, _, n = part.strip().rpartition(" ")
                index[(r["vendor"], name)] += int(n)
    assert Counter({(r["vendor"], r["fender_type"]): int(r["rows"]) for r in summary}) == index
    # hand count of the index rows column: 286 + 286 + 195 + 18 + 68 + 52 + 36 + 1542 + 1920 + 32 + 113
    # + 43 + 1 + 60 = 4652
    assert sum(int(r["rows"]) for r in summary) == 4652
    for r in summary:
        assert r["size_measure"] in ("height", "diameter")
        if r["largest_size_mm"]:
            assert int(r["smallest_size_mm"]) <= int(r["largest_size_mm"])


def test_size_summary_from_vendor_tables(tmp_path):
    """The importer groups by vendor, type and size measure: two cone rows of 300 and 500 mm and
    one pneumatic row of 1,000 mm give two groups (hand values)."""
    import importlib.util
    spec = importlib.util.spec_from_file_location(
        "import_vendor_tables", Path(__file__).resolve().parents[3] / "scripts" / "fender_design" / "import_vendor_tables.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    (tmp_path / "solid_rubber.csv").write_text(
        "vendor,fender_type,height_mm,source_id,accessed_on\nExample Co,cone,300,SRC-x,2026-01-01\n"
        "Example Co,cone,500,SRC-x,2026-01-02\n", encoding="utf-8", newline="\n")
    (tmp_path / "floating.csv").write_text(
        "vendor,fender_type,diameter_mm,source_id,accessed_on\nExample Co,pneumatic,1000,SRC-y,2026-01-01\n",
        encoding="utf-8", newline="\n")
    rows = mod.size_summary(tmp_path)
    assert [(r["fender_type"], r["rows"], r["size_measure"], r["smallest_size_mm"], r["largest_size_mm"])
            for r in rows] == [("cone", 2, "height", "300", "500"), ("pneumatic", 1, "diameter", "1000", "1000")]
    assert rows[0]["accessed_on"] == "2026-01-02" and rows[0]["source_ids"] == "SRC-x"

