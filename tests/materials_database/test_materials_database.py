"""Tests for data/materials_database (#2157, owner card D01: public, cited per row).

Comparator classes: conservation (the manifest hashes and row counts match the files;
the rebuild is byte-identical), provenance contract (every row cites a public source
document with a SHA-256) and physical invariants (strength and stiffness fall with
temperature; tensile strength is not below proof strength).
"""

import csv
import hashlib
import re
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "data" / "materials_database"
SOURCES = ROOT / "scripts" / "materials_database" / "sources.yml"
TABLE = "elevated_temperature_properties"


def _rows():
    with open(DATA / f"{TABLE}.csv", newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def _sources():
    return yaml.safe_load(SOURCES.read_text(encoding="utf-8"))


def test_manifest_matches_files():
    manifest = yaml.safe_load((DATA / "manifest.yaml").read_text(encoding="utf-8"))
    assert manifest["route"] == "public"
    assert "D01" in manifest["rights_decision"]
    entry = manifest["tables"][TABLE]
    blob = (DATA / entry["file"]).read_bytes()
    assert hashlib.sha256(blob).hexdigest() == entry["sha256"]
    assert entry["rows"] == len(_rows())


def _load_builder():
    import importlib.util

    path = ROOT / "scripts" / "materials_database" / "build_tables.py"
    spec = importlib.util.spec_from_file_location("materials_build_tables", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def test_rebuild_is_byte_identical(tmp_path):
    _load_builder().build(SOURCES, tmp_path)
    for name in (f"{TABLE}.csv", "manifest.yaml"):
        assert (tmp_path / name).read_bytes() == (DATA / name).read_bytes(), name


def test_every_row_cites_a_public_source_document():
    docs = {d["id"]: d for d in _sources()["documents"]}
    for r in _rows():
        doc = docs[r["source_id"]]
        assert re.fullmatch(r"[0-9a-f]{64}", doc["sha256"])
        assert doc["url"].startswith("https://")
        assert doc["access"].startswith("public")
        # the row cites the public document and its page, never the standard alone
        assert r["source_page"].strip()


def test_row_fields_valid():
    for r in _rows():
        assert r["basis"] in ("minimum", "typical")
        assert r["property"] in ("Rp0.2", "Rp1.0", "Rm", "E")
        assert r["unit"] == ("GPa" if r["property"] == "E" else "MPa")
        float(r["temperature_c"]), float(r["value"])


def _series(prop, grade="1.4404"):
    pts = sorted(
        (float(r["temperature_c"]), float(r["value"]))
        for r in _rows()
        if r["property"] == prop and r["grade"] == grade
    )
    assert pts, prop
    return pts


@pytest.mark.parametrize("prop", ["Rp0.2", "Rp1.0", "Rm", "E"])
def test_property_non_increasing_with_temperature(prop):
    vals = [v for _, v in _series(prop)]
    assert all(b <= a for a, b in zip(vals, vals[1:]))


def test_tensile_not_below_proof_at_common_temperatures():
    rp = dict(_series("Rp0.2"))
    rm = dict(_series("Rm"))
    for t in set(rp) & set(rm):
        assert rm[t] >= rp[t]
