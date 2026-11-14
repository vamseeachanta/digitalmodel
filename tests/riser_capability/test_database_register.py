"""Independent counts, integrity failures and reproducibility for Job 1 of #3954."""
import csv
import importlib.util
import json
import shutil
import sys
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = "scripts/riser_capability/build_database_register.py"
OUTPUTS = [f"docs/domains/risers/riser-input-database-register.{ext}" for ext in ("md", "json")]
DEFERRED = {"config/pipe/*.yml", "data/vessels/ (remaining members)",
            "docs/domains/orcaflex/library/line_types/", "docs/domains/orcaflex/library/buoy_types/",
            "src/digitalmodel/fatigue/ (including __init__.py)", "src/digitalmodel/materials/line_pipe.py"}


@pytest.fixture(scope="module")
def builder():
    assert (ROOT / SCRIPT).is_file(), "database register builder is missing"
    spec = importlib.util.spec_from_file_location("database_register", ROOT / SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def inputs(builder, tmp_path):
    paths = {entry[1] for entry in builder.INPUTS}
    paths.update({"data/riser_capability/riser_types.yml", "data/rao_database.manifest.json"})
    for path in list(paths):
        if path.endswith("/data.json"):
            paths.update(str(Path(path).with_name(name)) for name in ("schema.json", "sources.yml"))
    for path in paths:
        target = tmp_path / path
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(ROOT / path, target)
    return tmp_path


def test_counts_and_provenance_match_independent_sources(builder):
    rows = builder.build_register()["rows"]
    by_path = {r["path"]: r for r in rows if r["path"]}
    for name, expected in [("riser_stackup", 84), ("config_catalog", 28), ("rig_riser_interface", 10)]:
        path = f"data/riser_database/{name}.csv"
        with (ROOT / path).open(newline="", encoding="utf-8") as stream:
            assert by_path[path]["count"] == len(list(csv.DictReader(stream))) == expected
    path = "data/drilling_rigs/udw_rig_fleet_external.yml"
    fleet = yaml.safe_load((ROOT / path).read_text())
    assert {r["unit"]: r["count"] for r in rows if r["path"] == path} == {
        "rigs": len(fleet["rigs"]), "contracts": len(fleet["contracts"])} == {"rigs": 9, "contracts": 18}
    path = "data/vessels/processed/drillship_basis.json"
    basis = json.loads((ROOT / path).read_text())
    assert by_path[path]["count"] == len(basis["dimensions_m"]) == basis["row_count"] == 50
    assert all(r["provenance"] != "none" for r in rows if r["path"])
    assert by_path["data/fatigue/sn_curves.yaml"]["count"] == 23
    assert by_path["data/geotechnical/geotech_database.yaml"]["count"] == 60


def test_paths_inventory_absences_and_deferred_scope(builder):
    register = builder.build_register()
    rows = register["rows"]
    inventory = yaml.safe_load((ROOT / "data/riser_capability/riser_types.yml").read_text())
    for row in rows:
        if row["path"]:
            assert (ROOT / row["path"]).is_file()
            assert row["format"] and row["unit"]
    for database in inventory["databases"]:
        assert sum(r["path"] == database["path"] for r in rows) == 1
    absent = [r for r in rows if r["status"] == "absent"]
    assert {r["category"] for r in absent} == {r["category"] for r in inventory["absent_database_categories"]}
    assert all(r["path"] is None and r["count"] is None for r in absent)
    assert {r["issue"] for r in absent} == {r["issue"] for r in inventory["absent_database_categories"]}
    assert set(register["not_yet_counted"]) == DEFERRED
    text = builder.render(register)[OUTPUTS[0]].decode()
    assert {line[3:-1] for line in text.splitlines() if line.startswith("- `")} == DEFERRED
    assert text.index("Table 1.") > text.rindex("| absent |")
    assert "## Reproduce" in text and "## Run-it-yourself prompt" in text


def test_catalogue_population_and_missing_provenance(builder, inputs):
    path = "data/line_pipe/data.json"
    assert next(r for r in builder.build_register(inputs)["rows"] if r["path"] == path)["status"] == "empty"
    target = inputs / path
    data = json.loads(target.read_text())
    data["records"] = [{"status": "synthetic", "citation_id": "fixture"}]
    target.write_text(json.dumps(data))
    assert next(r for r in builder.build_register(inputs)["rows"] if r["path"] == path)["status"] == "seeded"
    data["records"].append({"status": "cited", "citation_id": "fixture"})
    target.write_text(json.dumps(data))
    assert next(r for r in builder.build_register(inputs)["rows"] if r["path"] == path)["status"] == "populated"
    target = inputs / "data/riser_database/config_catalog.csv"
    target.write_text("unattributed\nvalue\n")
    assert next(r for r in builder.build_register(inputs)["rows"] if r["path"] == str(target.relative_to(inputs)))["provenance"] == "none"


@pytest.mark.parametrize("mutation", ["declaration", "length", "width"])
def test_drillship_mutations_fail_with_filename(builder, inputs, mutation):
    path = inputs / "data/vessels/processed/drillship_basis.json"
    data = json.loads(path.read_text())
    if mutation == "declaration":
        data["row_count"] = 49
    elif mutation == "length":
        data["dimensions_m"].pop()
    else:
        data["dimensions_m"][0].pop()
    path.write_text(json.dumps(data))
    with pytest.raises(ValueError, match=r"drillship_basis.json.*(49.*50|50.*49|2.*3)"):
        builder.build_register(inputs)


@pytest.mark.parametrize("field,value", [("database_sha256", "0" * 64), ("database_bytes", 1)])
def test_rao_integrity_failure_never_replaces_outputs(builder, inputs, field, value):
    assert builder.main([], inputs) == 0
    before = {p: (inputs / p).read_bytes() for p in OUTPUTS}
    path = inputs / "data/rao_database.manifest.json"
    data = json.loads(path.read_text())
    data[field] = value
    path.write_text(json.dumps(data))
    with pytest.raises(ValueError, match=r"rao_database.parquet.*" + field):
        builder.main([], inputs)
    assert before == {p: (inputs / p).read_bytes() for p in OUTPUTS}


@pytest.mark.parametrize("escape", [False, True])
def test_missing_and_escaping_input_fail(builder, inputs, tmp_path, escape):
    target = inputs / "data/riser_database/config_catalog.csv"
    target.unlink()
    if escape:
        target.symlink_to(tmp_path.parent / "outside-register-input.csv")
    with pytest.raises(ValueError, match="config_catalog.csv"):
        builder.build_register(inputs)


@pytest.mark.parametrize("path,key", [("data/line_pipe/data.json", "records"),
    ("data/drilling_rigs/udw_rig_fleet_external.yml", "rigs"),
    ("data/vessels/processed/drillship_basis.json", "dimensions_m")])
def test_array_layout_cannot_silently_become_a_mapping(builder, inputs, path, key):
    target = inputs / path
    data = yaml.safe_load(target.read_text())
    data[key] = {}
    target.write_text(json.dumps(data))
    with pytest.raises(ValueError, match=key + ".*array"):
        builder.build_register(inputs)


def test_output_path_cannot_escape_repository(builder, inputs):
    folder = inputs / Path(OUTPUTS[0]).parent
    folder.parent.mkdir(parents=True)
    outside = inputs.parent / "outside-register-output"
    outside.mkdir()
    folder.symlink_to(outside, target_is_directory=True)
    with pytest.raises(ValueError, match="outside the repository"):
        builder.main([], inputs)
    assert not list(outside.iterdir())


@pytest.mark.parametrize("tampered", OUTPUTS)
def test_deterministic_generation_and_read_only_check(builder, inputs, tampered):
    assert builder.main([], inputs) == 0
    before = {p: (inputs / p).read_bytes() for p in OUTPUTS}
    assert builder.main([], inputs) == 0
    assert before == {p: (inputs / p).read_bytes() for p in OUTPUTS}
    assert builder.main(["--check"], inputs) == 0
    assert before == builder.render(builder.build_register())
    (inputs / tampered).write_bytes(b"drift\n")
    assert builder.main(["--check"], inputs) == 1
    assert (inputs / tampered).read_bytes() == b"drift\n"
    (inputs / tampered).unlink()
    assert builder.main(["--check"], inputs) == 1
    assert not (inputs / tampered).exists()
    assert builder.main(["--check"]) == 0


def test_all_register_dependencies_select_subsea(builder, monkeypatch):
    spec = importlib.util.spec_from_file_location("detect_touched_domains", ROOT / "scripts/ci/detect_touched_domains.py")
    detector = importlib.util.module_from_spec(spec)
    monkeypatch.setitem(sys.modules, spec.name, detector)
    spec.loader.exec_module(detector)
    domains = detector.parse_domains(ROOT / "tests/DOMAINS.md")
    paths = {entry[1] for entry in builder.INPUTS} | set(OUTPUTS) | {SCRIPT,
        "tests/riser_capability/test_database_register.py", "data/riser_capability/riser_types.yml",
        "data/rao_database.manifest.json", "scripts/riser_capability/build_riser_inventory.py"}
    for path in list(paths):
        if path.endswith("/data.json"):
            paths.update(str(Path(path).with_name(n)) for n in ("schema.json", "sources.yml"))
    for path in paths:
        assert "subsea" in {d.name for d in detector.touched_domains([path], domains)}, path
