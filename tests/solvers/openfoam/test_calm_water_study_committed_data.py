# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check the retained study bytes and independently recompute the windows."""

import csv
import hashlib
import importlib.util
import io
import json
import os
from pathlib import Path, PurePosixPath
import re
import statistics

import pytest

ROOT = Path(__file__).resolve().parents[3]
STUDY = ROOT / "data/cfd/calm_water_study"


def load_manifest():
    return json.loads((STUDY / "manifest.json").read_text())


def check_file(root, path, entry):
    data = (root / path).read_bytes()
    assert len(data) == entry.get("bytes", entry.get("size_bytes"))
    assert hashlib.sha256(data).hexdigest() == entry["sha256"]


def store_entries(manifest):
    store = manifest["store"]
    return store["histories"] + [
        entry for entries in store["cases"].values() for entry in entries
    ]


def check_store_entry(entry):
    digest = entry["sha256"]
    assert re.fullmatch(r"[0-9a-f]{64}", digest)
    assert re.fullmatch(
        rf"blobs/sha256/{digest[:2]}/{digest}\.[a-z0-9]+", entry["store_id"]
    )
    path = PurePosixPath(entry["path"])
    assert not path.is_absolute() and ".." not in path.parts
    assert "\\" not in entry["path"] and ":" not in entry["path"]
    assert int(entry["bytes"]) >= 0


def test_retained_files_and_run_log():
    manifest = load_manifest()
    for path, entry in manifest["files"].items():
        check_file(ROOT, path, entry)
    assert "data/cfd/calm_water_study/run-log.jsonl" in manifest["files"]


def test_changed_digest_is_rejected(tmp_path):
    target = tmp_path / "evidence"
    target.write_bytes(b"original")
    entry = {"bytes": 8, "sha256": hashlib.sha256(b"changed!").hexdigest()}
    with pytest.raises(AssertionError):
        check_file(tmp_path, "evidence", entry)


def test_store_index_covers_cases_and_full_histories():
    manifest = load_manifest()
    entries = store_entries(manifest)
    assert len(entries) == 213
    assert len({entry["path"] for entry in entries}) == 213
    assert sum(int(entry["bytes"]) > 1048576 for entry in entries) == 13
    for entry in entries:
        check_store_entry(entry)
    for entry in manifest["store"]["histories"]:
        assert entry["sha256"] == entry["history_sha256"]
    for entries in manifest["store"]["cases"].values():
        assert all(entry["class"].startswith("CASE-") for entry in entries)


@pytest.mark.parametrize("field,value", [
    ("sha256", "bad"), ("store_id", "/absolute/blob"),
    ("store_id", "blobs/sha256/aa/" + "b" * 64 + ".dat"),
    ("path", "../escape"), ("bytes", -1),
])
def test_invalid_store_entry_is_rejected(field, value):
    entry = {"path": "data/example.dat", "bytes": 1,
             "sha256": "a" * 64,
             "store_id": "blobs/sha256/aa/" + "a" * 64 + ".dat"}
    entry[field] = value
    with pytest.raises(AssertionError):
        check_store_entry(entry)


def test_window_statistics_are_reproducible():
    manifest = load_manifest()
    for run in manifest["full_runs"]:
        mesh = run["mesh"]
        stem = f"data/cfd/calm_water_study/raw/{mesh}-full-window-"
        coefficients = list(csv.DictReader(
            io.StringIO((ROOT / (stem + "coefficients.csv")).read_text())))
        forces = [
            line.split() for line in (ROOT / (stem + "force.dat")).read_text().splitlines()
            if line.strip() and not line.startswith("#")
        ]
        assert [float(row["iteration"]) for row in coefficients] == list(range(21001, 25001))
        assert [float(row[0]) for row in forces] == list(range(21001, 25001))
        expected = run["statistics"]
        computed = {"samples": len(coefficients), "first_iteration": 21001,
                    "last_iteration": 25000,
                    "mean_resistance_n": statistics.mean(-2 * float(row[1]) for row in forces)}
        for name in ("ct", "cp", "cv"):
            values = [float(row[name]) for row in coefficients]
            computed["mean_" + name] = statistics.mean(values)
            computed["standard_deviation_" + name] = statistics.pstdev(values)
        assert computed.keys() == expected.keys()
        for key, value in computed.items():
            assert value == pytest.approx(expected[key], rel=1e-12, abs=0)
        for suffix in ("force.dat", "coefficients.csv"):
            entry = manifest["files"][stem + suffix]
            assert (entry["first_iteration"], entry["last_iteration"], entry["row_count"]) == (21001, 25000, 4000)


# The repository autouse marker normally fetches into the checkout. Override it
# here: every blob is read in place and no large file is restored into Git.
@pytest.fixture(autouse=True)
def store_data_marker():
    yield


@pytest.mark.store_data
def test_reachable_store_blobs():
    configured = os.environ.get("ACE_DATA_ROOT")
    base = Path(configured) / "data/digitalmodel" if configured else None
    if base is None or not base.is_dir():
        pytest.skip("store_data: <id> not available (store not reachable); "
                    "run scripts/data/fetch.py <id>")
    for entry in store_entries(load_manifest()):
        check_store_entry(entry)
        check_file(base, entry["store_id"], entry)


def test_committed_html_is_deterministic():
    spec = importlib.util.spec_from_file_location(
        "committed_study_builder", ROOT / "scripts/cfd/build_calm_water_study.py")
    builder = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(builder)
    manifest = load_manifest()
    series = {
        run["mesh"]: (STUDY / "raw" / (run["mesh"] + "-full-window-coefficients.csv")).read_text()
        for run in manifest["full_runs"]
    }
    page = builder.render_report(
        manifest, builder.render_history_plot(series, manifest["criteria"]["averaging_iterations"]))
    assert page == (ROOT / "docs/domains/openfoam/calm-water-resistance-study.html").read_text()


def test_retained_case_method_and_report_limits():
    manifest = load_manifest()
    for mesh, evidence in manifest["case_evidence"].items():
        assert evidence["generator_revision"] == "not recorded in the retained case"
        assert {"blockMeshDict", "snappyHexMeshDict", "refineMeshDict",
                "fvSchemes", "fvSolution", "controlDict"} <= evidence["dictionary_sha256"].keys()
        assert evidence["method_readback"]["local_time_stepping"]
        assert evidence["method_readback"]["turbulence_model"] == "kOmegaSST"
        assert evidence["method_readback"]["two_phase_vof"] != "not recorded"
        if mesh != "fine":
            assert any("OPENFOAM=2312" in line for line in evidence["solver_version_banner"])
    page = (ROOT / "docs/domains/openfoam/calm-water-resistance-study.html").read_text()
    for text in ("kcs_t2005_case_1_1_fixed_bare", "SHIP_RESISTANCE_CT_TOLERANCE",
                 "offsetting components", "read from the retained case", "9.80665"):
        assert text in page


def test_license_comparison_rejects_source_changes(tmp_path):
    spec = importlib.util.spec_from_file_location(
        "header_check", ROOT / "scripts/enforcement/check_license_header_only.py")
    checker = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(checker)
    before, after = tmp_path / "before.py", tmp_path / "after.py"
    before.write_bytes(b"print(1)\n")
    header = b"# # Copyright (c) 2024 Digital Model Project\n# # Licensed under the MIT License. See LICENSE file for details.\n\n"
    after.write_bytes(header + before.read_bytes())
    assert checker.header_only(before, after, ROOT / ".license-header.txt")
    after.write_bytes(header + b"print(2)\n")
    assert not checker.header_only(before, after, ROOT / ".license-header.txt")
