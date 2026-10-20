# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Replay measured FE10 stresses without installing a finite-element solver."""

import csv
import hashlib
import importlib.util
import io
import json
import math
import subprocess
import sys
from pathlib import Path

import pytest

PACKAGE = Path(__file__).resolve().parents[3] / "benchmarks/fe10_nafems_le1"
CASES = {"le1_8x24": 192, "le1_16x48": 768, "le1_32x96": 3072, "patch": 5}
VERDICTS = {"le1_8x24": "FAIL", "le1_16x48": "PASS", "le1_32x96": "PASS", "patch": "PASS"}


def compare(case, path=None):
    option = "--patch" if case == "patch" else "--le1"
    return subprocess.run(
        [sys.executable, "-I", str(PACKAGE / "compare.py"), option,
         str(path or PACKAGE / f"results-{case}.csv")],
        capture_output=True, text=True, check=False,
    )


@pytest.mark.parametrize("case", CASES)
def test_committed_comparisons_match_record(case):
    record = json.loads((PACKAGE / "record.json").read_text())
    expected = VERDICTS[case]
    assert record["cases"][case]["verdict"] == expected
    result = compare(case)
    assert expected in {"PASS", "FAIL"}
    assert result.returncode == (0 if expected == "PASS" else 1), result.stdout
    assert f"SUMMARY {expected}:" in result.stdout
    assert result.stdout == (PACKAGE / "console" / f"compare-{case}.txt").read_text()


@pytest.mark.parametrize("case,count", CASES.items())
def test_csv_has_each_element_once_and_mid_surface_metadata(case, count):
    text = (PACKAGE / f"results-{case}.csv").read_text()
    assert "# program: MYSTRAN " in text
    assert "# recovery: centroid; arithmetic mean of two fibre stresses; global axes" in text
    rows = list(csv.DictReader(io.StringIO("\n".join(
        line for line in text.splitlines() if not line.startswith("#")
    ))))
    assert len(rows) == count
    assert sorted(int(row["eid"]) for row in rows) == list(range(1, count + 1))


@pytest.mark.parametrize("case", ["le1_32x96", "patch"])
def test_perturbed_csv_fails(case, tmp_path):
    source = (PACKAGE / f"results-{case}.csv").read_text()
    lines = source.splitlines()
    header = next(i for i, line in enumerate(lines) if line.startswith("eid,"))
    for i in range(header + 1, len(lines)):
        fields = lines[i].split(",")
        lines[i] = ",".join([fields[0]] + [str(float(v) * 2) for v in fields[1:]])
    modified = tmp_path / "perturbed.csv"
    modified.write_text("\n".join(lines) + "\n")
    result = compare(case, modified)
    assert result.returncode == 1, result.stdout
    assert "SUMMARY FAIL:" in result.stdout
    assert (PACKAGE / f"results-{case}.csv").read_text() == source


def test_comparator_selftest():
    result = subprocess.run(
        [sys.executable, "-I", str(PACKAGE / "compare.py"), "--selftest"],
        capture_output=True, text=True, check=False,
    )
    assert result.returncode == 0, result.stdout
    assert "SELFTEST PASS" in result.stdout


def test_retained_artifacts_match_manifest():
    manifest = json.loads((PACKAGE / "manifest.json").read_text())
    listed = {entry["path"] for entry in manifest["files"]}
    documentation = set(manifest["documentation"])
    actual = {p.relative_to(PACKAGE).as_posix() for p in PACKAGE.rglob("*")
              if p.is_file() and p.name != "manifest.json"}
    assert all(Path(path).suffix == ".md" for path in documentation)
    assert not listed & documentation
    assert listed | documentation == actual
    for entry in manifest["files"]:
        payload = (PACKAGE / entry["path"]).read_bytes()
        assert hashlib.sha256(payload).hexdigest() == entry["sha256"], entry["path"]


def test_decks_retain_received_digests():
    manifest = json.loads((PACKAGE / "manifest.json").read_text())
    for case in CASES:
        path = f"inputs/{case}.bdf"
        received = manifest["received_sha256"][path]
        assert hashlib.sha256((PACKAGE / path).read_bytes()).hexdigest() == received


def extractor():
    spec = importlib.util.spec_from_file_location(
        "fe10_extract", PACKAGE / "tools/extract_stresses.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_unequal_fibres_are_averaged_then_tensor_rotated():
    native = """E L E M E N T   S T R E S S E S   I N   L O C A L
        1 CENTER -50.0 10.0 20.0 3.0 0.0 0.0 0.0 0.0
                  50.0 30.0 40.0 5.0 0.0 0.0 0.0 0.0
"""
    module = extractor()
    mean = module.mid_surface_stresses(native)[1]
    assert mean == (20.0, 30.0, 4.0)
    a = math.sqrt(0.5)
    square = [(0, 0, 0), (a, a, 0), (0, 2 * a, 0), (-a, a, 0)]
    assert module.global_stress(mean, square) == pytest.approx((21.0, 29.0, -5.0))


@pytest.mark.parametrize("native", [
    "1 CENTER -50 10 20 3\n",
    "1 CENTER -50 10 20 3\n50 nan 20 3\n",
    "1 CENTER -50 10 20 3\n50 10 20 3\n1 CENTER -50 10 20 3\n50 10 20 3\n",
])
def test_extractor_rejects_incomplete_nonfinite_or_duplicate_rows(native):
    with pytest.raises(ValueError):
        extractor().mid_surface_stresses(native)


def test_sanitizer_preserves_numeric_results(tmp_path):
    spec = importlib.util.spec_from_file_location(
        "fe10_sanitize", PACKAGE / "tools/sanitize_output.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    private_path = "/" + "ho" + "me" + "/" + tmp_path.name
    original = f"input {private_path}\n1,300.0,300.0,80.7692\n"
    assert module.sanitize(original) == "[path removed]\n1,300.0,300.0,80.7692\n"
    matrix_name = "P" + "M"
    assert module.sanitize(f"Deallocate {matrix_name}\n") == "Deallocate [matrix label removed]\n"


@pytest.mark.parametrize("case", CASES)
def test_retained_native_centroids_reproduce_csv(case, tmp_path):
    regenerated = tmp_path / "recovered.csv"
    extractor().extract(PACKAGE / "inputs" / f"{case}.bdf",
                        PACKAGE / "native" / f"{case}.F06", regenerated)
    assert regenerated.read_bytes() == (PACKAGE / f"results-{case}.csv").read_bytes()
