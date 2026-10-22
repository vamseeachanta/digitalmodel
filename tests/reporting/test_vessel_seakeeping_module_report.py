"""Rebuild the report against the public workflow and its numeric outputs."""
import copy
import csv
import importlib.util
import json
import re
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reports/build_vessel_seakeeping_module_report.py"
REPORT = ROOT / "docs/reports/modules/vesselseakeeping"


def builder():
    spec = importlib.util.spec_from_file_location("seakeeping_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_rebuild_and_every_worked_value(tmp_path):
    from digitalmodel.vessel_seakeeping.workflow import router

    build = builder()
    html = build.render(tmp_path / "build")
    assert html == (REPORT / "index.html").read_text()
    data = json.loads(re.search(r'<script id="calculation" type="application/json">(.*?)</script>', html, re.S)[1])
    cfg = yaml.safe_load((ROOT / build.EXAMPLE).read_text())
    inputs = copy.deepcopy(cfg["vessel_seakeeping"])
    inputs.pop("vessel")
    inputs.pop("output_dir")
    assert data["inputs"] == inputs
    cfg["vessel_seakeeping"]["output_dir"] = str(tmp_path / "independent")
    result = router(cfg)["vessel_seakeeping"]
    assert data["summary"] == result["dofs"]
    assert data["status"] == result["screening_status"]
    with Path(result["results_csv"]).open() as stream:
        rows = list(csv.DictReader(stream))
    assert len(data["rows"]) == len(rows)
    for actual, row in zip(data["rows"], rows):
        for key in ("hs", "tp", "probability", "significant_amplitude", "criterion"):
            assert actual[key] == float(row[key])
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
    assert json.loads((REPORT / "changes.json").read_text()) == build.CHANGES
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
    assert "1.23456789" in html
