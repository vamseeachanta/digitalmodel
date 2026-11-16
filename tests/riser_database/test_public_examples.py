"""Public example catalogue: independent source readings, no solver execution."""
import csv
import importlib.util
import shutil
from pathlib import Path

import pytest
import yaml

from digitalmodel.riser_database.loader import RiserDatabase

ROOT = Path(__file__).resolve().parents[2]
BASE = "docs/domains/orcaflex/examples/modular"
CASES = [
    ("a01_catenary", "A01/A01 Catenary riser", "catenary", "Catenary Hose", '10" flexible variable Cd'),
    ("a01_lazy_wave", "A01/A01 Lazy wave riser", "lazy_wave", '10" Lazy Wave Distributed', '10" flexible'),
    ("a01_steep_wave", "A01/A01 Steep wave riser", "steep_wave", '10" Steep Wave', '10" flexible'),
    ("a01_pliant_wave", "A01/A01 Pliant wave riser", "pliant_wave", '10" Pliant Simple', '10"Product'),
    ("a02_lazy_s_simple", "A02/A02 Lazy S simple", "lazy_s", '10" riser N', '10"'),
    ("a02_lazy_s_detailed", "A02/A02 Lazy S detailed", "lazy_s", '10" up N', '10"'),
    ("a02_steep_s", "A02/A02 Steep S", "steep_s", '10" upper East', '10"'),
    ("a02_pliant_s", "A02/A02 Pliant S", "pliant_s", '10" upper East', '10"'),
    ("a03_tower_jumper", "A03/A03 Jumper to high tower", "tower_jumper", "Product N Out", "Production Jumper"),
    ("a05_semisub_catenary", "A05/A05 Catenary with semisub", "catenary", "Riser with Flexjoint", "Riser Pipe"),
    ("a05_spar_catenary", "A05/A05 Catenary with spar", "catenary", "Riser", "Riser Pipe"),
    ("a05_fpso_lazy_wave", "A05/A05 Lazy wave with FPSO", "lazy_wave", "Riser with Flexjoint", "Lazy Bare"),
    ("b01_drilling", "B01/B01 Drilling riser", "drilling", "Riser", '24" RISER'),
    ("b06_running_bop", "B06/B06 Running BOP", "running_bop", "Marine riser", '12" Marine Riser'),
]
spec = importlib.util.spec_from_file_location("riser_tables", ROOT / "scripts/riser_database/build_tables.py")
builder = importlib.util.module_from_spec(spec)
spec.loader.exec_module(builder)


def test_public_example_ids():
    selections = yaml.safe_load(builder.SPEC_PATH.read_text())["public_example_configs"]
    fields = ("config_id", "example", "riser_type", "line", "bare_type")
    assert [tuple(s[f] for f in fields) for s in selections] == [
        ("orcaflex_" + key, example, kind, line, bare) for key, example, kind, line, bare in CASES]
    rows = [r for r in RiserDatabase.load().configs if r.analysis_kind == "orcaflex_public_example"]
    assert {r.config_id for r in rows} == {"orcaflex_" + case[0] for case in CASES}
    assert len(rows) == 14
    assert len({r.riser_type for r in rows}) == 10


@pytest.mark.parametrize("key,example,kind,line_name,type_name", CASES)
def test_public_example_source_readings(key, example, kind, line_name, type_name):
    source = f"{BASE}/{example}/master.yml"
    row = RiserDatabase.load().config("orcaflex_" + key)
    assert row.source_path == source and row.riser_type == kind
    doc = {}
    for include in yaml.safe_load((ROOT / source).read_text()):
        doc.update(yaml.safe_load((ROOT / source).parent.joinpath(include["includefile"]).read_text()))
    lines = [v for v in doc["Lines"] if v["Name"] == line_name]
    types = [v for v in doc["LineTypes"] if v["Name"] == type_name]
    assert len(lines) == len(types) == 1
    assert type_name in {v[0] for v in lines[0]["LineType, Length, TargetSegmentLength"]}
    units, env, lt = doc["General"], doc["Environment"], types[0]
    scale = 0.3048 if units["UnitsSystem"] == "User" and units["LengthUnits"] == "ft" else 1
    depth = env["WaterDepth"] if "WaterDepth" in env else env["SeabedOriginDepth"]
    if "WaterDepth" not in env:
        assert env["SeabedType"] == "Flat" and env["SeabedSlope"] == 0
    assert row.water_depth_m == pytest.approx(depth * scale, abs=0.005, rel=0)
    assert row.od_mm == pytest.approx(lt["OD"] * scale * 1000, abs=0.0005, rel=0)
    if lt["Category"] == "Homogeneous pipe":
        assert row.wt_mm == pytest.approx((lt["OD"] - lt["ID"]) * scale * 500, abs=0.0005, rel=0)
    else:
        assert row.wt_mm is None
    assert row.material_grade == ""
    if key == "a05_semisub_catenary":
        with (ROOT / "data/riser_database/config_catalog.csv").open() as fh:
            assert next(r for r in csv.DictReader(fh) if r["config_id"] == row.config_id)["water_depth_m"] == "1999.49"


@pytest.mark.parametrize("defect", [
    "units", "si_ft", "missing_line", "duplicate_line", "missing_type", "duplicate_type",
    "section", "od", "nan", "infinity", "id", "depth", "slope", "escape", "absolute",
    "duplicate_include", "missing_include", "duplicate_section", "id_nan", "depth_nan"])
def test_public_example_rejects_invalid_evidence(tmp_path, monkeypatch, defect):
    doc = {"General": {"UnitsSystem": "SI"}, "Environment": {"WaterDepth": 100},
           "LineTypes": [{"Name": "bare", "Category": "Homogeneous pipe", "OD": 0.3, "ID": 0.2}],
           "Lines": [{"Name": "primary", "LineType, Length, TargetSegmentLength": [["bare", 50, 5]]}]}
    if defect == "units":
        doc["General"]["UnitsSystem"] = "US"
    if defect == "si_ft":
        doc["General"]["LengthUnits"] = "ft"
    for section, suffix in [("Lines", "line"), ("LineTypes", "type")]:
        if defect == "missing_" + suffix:
            doc[section] = []
        if defect == "duplicate_" + suffix:
            doc[section] *= 2
    if defect == "section":
        doc["Lines"][0]["LineType, Length, TargetSegmentLength"] = [["buoyed", 50, 5]]
    if defect in {"od", "nan", "infinity"}:
        doc["LineTypes"][0]["OD"] = {"od": -1, "nan": float("nan"), "infinity": float("inf")}[defect]
    if defect in {"id", "id_nan"}:
        doc["LineTypes"][0]["ID"] = 0.4 if defect == "id" else float("nan")
    if defect in {"depth", "depth_nan"}:
        doc["Environment"]["WaterDepth"] = 0 if defect == "depth" else float("nan")
    if defect == "slope":
        doc["Environment"] = {"SeabedOriginDepth": 100, "SeabedType": "Flat", "SeabedSlope": 1}
    base = tmp_path / BASE / "sample"
    (base / "includes").mkdir(parents=True)
    includes = []
    for filename, section in zip(["01_general", "03_environment", "05_line_types", "07_lines"], doc):
        path = base / "includes" / (filename + ".yml")
        path.write_text(yaml.safe_dump({section: doc[section]}))
        includes.append({"includefile": "includes/" + path.name})
    if defect in {"escape", "absolute"}:
        includes[0]["includefile"] = "../" * 8 + "outside.yml" if defect == "escape" else str(tmp_path / "outside.yml")
    if defect == "duplicate_include":
        includes.append(includes[0])
    if defect == "missing_include":
        includes.pop()
    if defect == "duplicate_section":
        (base / "includes/03_environment.yml").write_text(yaml.safe_dump(doc))
    (base / "master.yml").write_text(yaml.safe_dump(includes))
    monkeypatch.setattr(builder, "REPO_ROOT", tmp_path)
    selection = dict(config_id="sample", example="sample", riser_type="catenary", line="primary", bare_type="bare")
    with pytest.raises(ValueError):
        builder._public_example_row(selection, {"ft_to_m": 0.3048, "m_to_mm": 1000})


@pytest.mark.parametrize("stale", ["config_catalog.csv", "standards_crosswalk.csv", "material_sn_scf_dff.csv", "riser_stackup.csv", "rig_riser_interface.csv", "manifest.yaml"])
def test_check_mode_detects_stale_bytes_without_writes(tmp_path, monkeypatch, stale):
    output = tmp_path / "database"
    shutil.copytree(ROOT / "data/riser_database", output)
    monkeypatch.setattr(builder, "OUT_DIR", output)
    assert builder.main(["--check"]) == 0
    path = output / stale
    path.write_bytes(path.read_bytes() + b"stale\n")
    before = {p.relative_to(output): (p.read_bytes(), p.stat().st_mtime_ns) for p in output.rglob("*") if p.is_file()}
    assert builder.main(["--check"]) == 1
    assert {p.relative_to(output): (p.read_bytes(), p.stat().st_mtime_ns) for p in output.rglob("*") if p.is_file()} == before
