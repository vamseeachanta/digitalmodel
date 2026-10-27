# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the report against the public workflow and its numeric outputs."""
import copy
import csv
import hashlib
import importlib.util
import json
import re
import subprocess
import sys
from pathlib import Path, PurePosixPath, PureWindowsPath

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reports/build_vessel_seakeeping_module_report.py"
REPORT = ROOT / "docs/reports/modules/vesselseakeeping"
ORACLE = Path(__file__).parent / "fixtures/vessel_seakeeping_hand_calculation.json"
REBUILD = (
    "Rebuild with PYTHONPATH=src python "
    "scripts/reports/build_vessel_seakeeping_module_report.py --scratch <scratch>."
)


def builder():
    spec = importlib.util.spec_from_file_location("seakeeping_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_rebuild_and_every_worked_value(tmp_path):
    from digitalmodel.vessel_seakeeping.workflow import router

    build = builder()
    html = build.render(tmp_path / "build")
    assert html == (REPORT / "index.html").read_text(encoding="utf-8"), REBUILD
    data = json.loads(re.search(r'<script id="calculation" type="application/json">(.*?)</script>', html, re.S)[1])
    cfg = yaml.safe_load((ROOT / build.EXAMPLE).read_text(encoding="utf-8"))
    inputs = copy.deepcopy(cfg["vessel_seakeeping"])
    inputs.pop("vessel")
    inputs.pop("output_dir")
    assert data["inputs"] == inputs
    cfg["vessel_seakeeping"]["output_dir"] = str(tmp_path / "independent")
    result = router(cfg)["vessel_seakeeping"]
    assert data["summary"] == result["dofs"]
    assert data["status"] == result["screening_status"]
    with Path(result["results_csv"]).open(encoding="utf-8") as stream:
        rows = list(csv.DictReader(stream))
    assert len(data["rows"]) == len(rows)
    for actual, row in zip(data["rows"], rows):
        for key in ("hs", "tp", "probability", "significant_amplitude", "criterion"):
            assert actual[key] == pytest.approx(float(row[key]), rel=1e-12, abs=1e-12)
        assert actual["operable"] == (row["operable"] == "True")
        for key in ("dof", "unit"):
            assert actual[key] == row[key]
        assert build.result_row(actual) in html
    for summary in data["summary"]:
        assert build.summary_row(summary) in html
    assert 'class="hs-doc"' in html
    assert 'src="http' not in html and 'href="../' not in html
    assert len(re.findall(r'<h2 id="s\d+">', html)) == 10
    assert "showSaveFilePicker" in html and "changes-panel" in html
    assert "ASSUMED - to be confirmed" in html
    assert "NOT_EVALUATED" in html
    assert json.loads((REPORT / "changes.json").read_text(encoding="utf-8")) == build.CHANGES
    assert "/" + "home/" not in html and "/" + "mnt/" not in html


def test_no_result_is_typed_in_the_template(tmp_path, monkeypatch):
    build = builder()
    original = build.calculate
    def changed(scratch):
        data = original(scratch)
        data["rows"][0]["significant_amplitude"] = 1.23456789
        return data
    monkeypatch.setattr(build, "calculate", changed)
    html = build.render(tmp_path)
    assert "1.235" in html
    data = json.loads(re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>', html, re.S,
    )[1])
    assert data["rows"][0]["significant_amplitude"] == pytest.approx(1.23456789)


def test_worked_values_match_independent_hand_calculation(tmp_path):
    """Catch shared spectral calculation errors that a router replay cannot."""
    expected = json.loads(ORACLE.read_text(encoding="utf-8"))
    build = builder()
    digest = hashlib.sha256((ROOT / build.EXAMPLE).read_bytes()).hexdigest()
    assert digest == expected["input_sha256"], REBUILD
    html = build.render(tmp_path / "oracle-check")
    payload = re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>',
        html, re.S,
    )
    actual = json.loads(payload[1])
    assert len(actual["rows"]) == len(expected["rows"]) == 9
    for row, reference in zip(actual["rows"], expected["rows"]):
        amplitude = row.pop("significant_amplitude")
        assert amplitude == pytest.approx(
            reference["significant_amplitude"], rel=1e-10, abs=1e-12,
        )
        assert row == {
            key: value for key, value in reference.items()
            if key != "significant_amplitude"
        }
    assert actual["summary"] == expected["summary"]
    assert actual["status"] == expected["status"]


@pytest.mark.parametrize("path_type", [PurePosixPath, PureWindowsPath])
def test_provenance_uses_focused_rebuild_evidence(tmp_path, monkeypatch, path_type):
    """Unrelated workflow assertions shall not invalidate the delivered report."""
    build = builder()
    original = Path.read_bytes
    unrelated = ROOT / "tests/workflows/test_durable_workflows.py"

    def read_bytes(path):
        if path == unrelated:
            raise AssertionError("Unrelated workflow test is not report evidence")
        return original(path)

    monkeypatch.setattr(Path, "read_bytes", read_bytes)
    html = build.render(tmp_path / "provenance")
    for path in (Path(__file__), ORACLE):
        relative = path_type(path.relative_to(ROOT)).as_posix()
        evidence = f"{relative}; SHA-256: {hashlib.sha256(path.read_bytes()).hexdigest()}"
        assert evidence in html


def build_delivery(tmp_path):
    """Run the public report command against temporary output paths."""
    output = tmp_path / "published"
    subprocess.run(
        [sys.executable, str(SCRIPT), "--scratch", str(tmp_path / "calculation"),
         "--output-dir", str(output)], cwd=ROOT, check=True,
        capture_output=True, text=True, encoding="utf-8",
    )
    return output


def assert_delivered_manifest_matches(fresh, delivered):
    """Require the delivered source and artifact bindings to match fresh bytes."""
    assert (delivered / "manifest.json").read_bytes() == (fresh / "manifest.json").read_bytes(), (
        f"Delivered manifest has drifted. {REBUILD}"
    )


def test_cli_manifest_binds_written_deliverables(tmp_path):
    output = build_delivery(tmp_path)
    manifest = json.loads((output / "manifest.json").read_text(encoding="utf-8"))
    for name in ("index.html", "changes.json"):
        assert manifest["artifacts"][name] == hashlib.sha256((output / name).read_bytes()).hexdigest(), REBUILD
    assert manifest["generator_sha256"] == hashlib.sha256(SCRIPT.read_bytes()).hexdigest(), REBUILD
    assert_delivered_manifest_matches(output, REPORT)


@pytest.mark.parametrize("fault", ["empty", "source", "artifact"])
def test_delivered_manifest_rejects_altered_hashes(tmp_path, fault):
    fresh = build_delivery(tmp_path)
    delivered = tmp_path / "delivered"
    delivered.mkdir()
    manifest = delivered / "manifest.json"
    manifest.write_bytes((fresh / "manifest.json").read_bytes())
    assert_delivered_manifest_matches(fresh, delivered)
    damaged = json.loads(manifest.read_text(encoding="utf-8"))
    if fault == "empty":
        damaged = {}
    else:
        field = "sources" if fault == "source" else "artifacts"
        damaged[field][next(iter(damaged[field]))] = "0" * 64
    manifest.write_text(json.dumps(damaged) + "\n", encoding="utf-8", newline="\n")
    with pytest.raises(AssertionError, match="Delivered manifest has drifted"):
        assert_delivered_manifest_matches(fresh, delivered)
