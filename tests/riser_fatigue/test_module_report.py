# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the public example report against the real fatigue workflow."""

import copy
import csv
import hashlib
import importlib.util
import json
import math
import os
import re
import shlex
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

from digitalmodel.riser_fatigue.workflow import router


ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/riserfatigue"
SPEC = importlib.util.spec_from_file_location("module_report", REPORT / "generate_report.py")


def generator():
    module = importlib.util.module_from_spec(SPEC)
    SPEC.loader.exec_module(module)
    return module


class Metrics(HTMLParser):
    """Collect every displayed calculation and the embedded calculation record."""

    def __init__(self):
        super().__init__()
        self.metrics = []
        self.record = ""
        self.in_record = False
        self.current = None
        self.external_assets = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if (tag == "script" and "src" in attrs) or (tag == "link" and attrs.get("rel") == "stylesheet"):
            self.external_assets.append(attrs)
        if "data-metric" in attrs:
            self.current = [attrs, ""]
        if tag == "script" and attrs.get("id") == "calculation-record":
            self.in_record = True

    def handle_data(self, data):
        if self.current is not None:
            self.current[1] += data
        if self.in_record:
            self.record += data

    def handle_endtag(self, tag):
        if tag == "span" and self.current is not None:
            self.metrics.append(self.current)
            self.current = None
        if tag == "script":
            self.in_record = False


@pytest.fixture
def built(tmp_path):
    module = generator()
    record = module.build_payload(tmp_path / "calculation")
    return module, record, module.render(record)


def test_all_example_results_equal_direct_module_returns(built, tmp_path):
    _, record, _ = built
    cfg = yaml.safe_load((ROOT / "examples/workflows/riser-fatigue/input.yml").read_text(encoding="utf-8"))
    original = copy.deepcopy(cfg["riser_fatigue"])
    cfg["riser_fatigue"]["output_dir"] = str(tmp_path / "direct")
    result = router(cfg)["riser_fatigue"]
    assert record["inputs"] == {k: v for k, v in original.items() if k != "output_dir"}
    assert record["result"] == {
        k: v for k, v in result.items() if k not in {"results_csv", "summary_csv"}
    }
    with Path(result["results_csv"]).open(encoding="utf-8", newline="") as stream:
        rows = list(csv.DictReader(stream))
    assert len(record["bins"]) == len(rows)
    for actual, expected in zip(record["bins"], rows):
        assert actual.keys() == expected.keys()
        for key, value in expected.items():
            if key in {"segment_id", "contribution"}:
                assert actual[key] == value
            elif math.isnan(float(value)):
                assert actual[key] is None
            else:
                assert actual[key] == float(value)


def test_every_displayed_metric_uses_the_calculation_record(built):
    module, record, html = built
    parser = Metrics()
    parser.feed(html)
    assert json.loads(parser.record) == record
    assert parser.metrics
    used = set()
    for attrs, visible in parser.metrics:
        key = attrs["data-metric"]
        value = module.lookup(record, key)
        assert float(attrs["data-value"]) == value
        assert visible == module.format_number(value, attrs["data-format"])
        used.add(key)
    # All segment results and every numeric bin field are visible in the appendix.
    for index, segment in enumerate(record["result"]["segments"]):
        for key, value in segment.items():
            if isinstance(value, (float, int)) and not isinstance(value, bool):
                assert f"result.segments.{index}.{key}" in used
    for index, row in enumerate(record["bins"]):
        for key, value in row.items():
            if isinstance(value, (float, int)):
                assert f"bins.{index}.{key}" in used


def test_rebuild_is_deterministic_and_committed_page_is_current(built, tmp_path):
    module, record, html = built
    second = module.build_payload(tmp_path / "second")
    assert second == record
    assert module.render(second) == html
    assert (REPORT / "report.html").read_text(encoding="utf-8") == html
    assert json.loads((REPORT / "changes.json").read_text(encoding="utf-8")) == module.CHANGES


def test_chart_demands_and_limit_come_from_calculation(built):
    _, record, html = built
    for index, segment in enumerate(record["result"]["segments"]):
        chart = record["chart"][index]
        assert chart["demand"] == segment["total_damage"]
        assert chart["limit"] == record["allowable_damage"]
        assert chart["limit"] == 1 / record["inputs"]["dff"]
    assert 'id="damage-limit"' in html
    assert 'tabindex="0"' in html
    assert "pointerenter" in html and "focus" in html


def test_report_has_house_style_sources_and_ten_sections(built):
    _, _, html = built
    assert '<body class="hs-doc">' in html
    for path in ("docs/api/_assets/brand.css",):
        assert (ROOT / path).read_text(encoding="utf-8") in html
    assert len(re.findall(r'<section id="s\d+">', html)) == 10
    assert "Draft for maintainer review" in html
    assert "Revision history" in html
    assert "NOT_EVALUATED" in html
    assert "ASSUMED - to be confirmed" in html
    assert "independent published benchmark" in html
    parser = Metrics()
    parser.feed(html)
    assert not parser.external_assets
    assert 'id="changes-record"' in html
    ids = set(re.findall(r'\bid="([^"]+)"', html))
    for target in re.findall(r'href="#([^"]+)"', html):
        assert target in ids


def test_cli_writes_only_requested_deliverables(tmp_path):
    output = tmp_path / "report"
    subprocess.run(
        [sys.executable, str(REPORT / "generate_report.py"),
         "--output-dir", str(output), "--scratch-dir", str(tmp_path / "scratch")],
        cwd=ROOT, check=True, capture_output=True, text=True,
    )
    assert sorted(p.name for p in output.iterdir()) == ["changes.json", "report.html"]
    assert (output / "report.html").read_bytes() == (REPORT / "report.html").read_bytes()
    for path in output.iterdir():
        assert b"\r" not in path.read_bytes()


def test_report_deliverables_are_visible_to_git():
    for name in ("generate_report.py", "report.html", "changes.json"):
        result = subprocess.run(
            ["git", "check-ignore", "--no-index", "-q", str(REPORT / name)], cwd=ROOT,
        )
        assert result.returncode == 1
    assert subprocess.run(
        ["git", "check-ignore", "--no-index", "-q", "docs/reports/modules/unrelated/report.html"],
        cwd=ROOT,
    ).returncode == 0


def test_all_numeric_example_inputs_are_displayed(built):
    _, record, html = built
    parser = Metrics()
    parser.feed(html)
    used = {attrs["data-metric"] for attrs, _ in parser.metrics}

    def visit(value, key):
        if isinstance(value, dict):
            for name, child in value.items():
                visit(child, key + "." + name)
        elif isinstance(value, list):
            for index, child in enumerate(value):
                visit(child, key + f".{index}")
        else:
            try:
                float(value)
            except (ValueError, TypeError):
                return
            assert key in used

    visit(record["inputs"], "inputs")


def test_a_changed_verdict_requires_a_new_engineering_narrative(built):
    module, record, _ = built
    changed = copy.deepcopy(record)
    changed["result"]["screening_status"] = "fail"
    with pytest.raises(ValueError, match="engineering narrative update"):
        module.render(changed)


def test_comment_roundtrip_and_chart_events_without_a_browser(built, tmp_path):
    """Exercise saving, merging, revision binding and safe rendering in Node."""
    module, _, _ = built
    script = tmp_path / "interactions.cjs"
    script.write_text(r'''
const assert = require('node:assert/strict');
function element() {
  return {textContent: '', value: '', children: [], listeners: {}, dataset: {},
    append(child) {this.children.push(child);}, replaceChildren() {this.children = [];},
    addEventListener(name, handler) {this.listeners[name] = handler;}, click() {}};
}
const ids = new Map();
for (const id of ['comment-config', 'comment-section', 'comment-text', 'comment-list',
  'comment-add', 'comment-load', 'comment-save', 'comment-status', 'chart-readout']) ids.set(id, element());
ids.get('comment-config').textContent = JSON.stringify({document_key: 'matching-document', revision: 'Initial'});
const sections = Array.from({length: 10}, (_, i) => ({id: `s${i + 1}`,
  querySelector() {return {textContent: this.id};}}));
const point = element();
point.dataset.description = 'Case: example; component: touchdown; demand and limit; dimensionless.';
global.document = {getElementById: id => ids.get(id), createElement: element,
  querySelectorAll: selector => selector === 'section[id]' ? sections : [point]};
global.window = {};
let savedBlob;
global.URL = {createObjectURL(blob) {savedBlob = blob; return 'blob:comments';}, revokeObjectURL() {}};
global.setTimeout = callback => callback();
''' + module.INTERACTIONS + r'''
(async () => {
  const el = id => ids.get(id);
  assert.equal(el('comment-section').children.length, 10);
  el('comment-section').value = 's5';
  el('comment-text').value = '<img onerror=invalid> Check the stress basis.';
  el('comment-add').listeners.click();
  assert.equal(el('comment-list').children.length, 1);
  assert.match(el('comment-list').children[0].textContent, /<img/);
  await el('comment-save').listeners.click();
  const saved = JSON.parse(await savedBlob.text());
  assert.equal(saved.comments.length, 1);
  const load = data => el('comment-load').listeners.change({target: {files: [{text: async () => JSON.stringify(data)}]}});
  await load(saved);
  assert.equal(el('comment-list').children.length, 1);
  await load({...saved, comments: [{section: 's3', text: 'Confirm dimensions.'}]});
  assert.equal(el('comment-list').children.length, 2);
  await load({...saved, document_key: 'different-document'});
  assert.match(el('comment-status').textContent, /not loaded/);
  await load({...saved, revision: 'Different'});
  assert.match(el('comment-status').textContent, /not loaded/);
  await load({...saved, comments: [{section: 'missing', text: 'Invalid location'}]});
  assert.equal(el('comment-list').children.length, 2);
  let written, closed = false;
  window.showSaveFilePicker = async () => ({createWritable: async () => ({
    write: async data => {written = data;}, close: async () => {closed = true;}})});
  await el('comment-save').listeners.click();
  assert.equal(JSON.parse(written).comments.length, 2);
  assert.equal(closed, true);
  window.showSaveFilePicker = async () => {const error = new Error(); error.name = 'AbortError'; throw error;};
  await el('comment-save').listeners.click();
  assert.match(el('comment-status').textContent, /cancelled/);
  point.listeners.pointerenter();
  assert.equal(el('chart-readout').textContent, point.dataset.description);
  point.listeners.focus();
  assert.equal(el('chart-readout').textContent, point.dataset.description);
})().catch(error => {console.error(error); process.exitCode = 1;});
''', encoding="utf-8", newline="\n")
    subprocess.run(["node", str(script)], check=True, capture_output=True, text=True)


def test_report_results_against_independent_hand_literals(built):
    """Decimal hand arithmetic, independent of router, pyLife and report rebuilds.

    Apply the public F1 seawater-CP curve to each corrected range, sum n/N,
    and scale the annual wave histogram by the example life. Vibration cycles
    are frequency times exposure seconds times life; L = T/D and M = 1/(F D).
    The literals retain extra digits so relative tolerance is meaningful.
    This checks arithmetic against the encoded basis, not physical validation.
    """
    _, record, html = built
    expected = {
        "governing_wave_damage": 0.0121240540371037,
        "governing_viv_damage": 0.00130253943441560,
        "governing_damage": 0.0134265934715193,
        "governing_fatigue_life_years": 1861.97638686465,
        "governing_dff_margin": 7.44790554745861,
    }
    parser = Metrics()
    parser.feed(html)
    displayed = {attrs["data-metric"]: float(attrs["data-value"])
                 for attrs, _ in parser.metrics}
    for key, literal in expected.items():
        assert record["result"][key] == pytest.approx(literal, rel=1e-6)
        assert displayed[f"result.{key}"] == pytest.approx(literal, rel=1e-6)
    cycles = [row["cycles"] for row in record["bins"]
              if row["segment_id"] == "TDZ-sandwave" and row["contribution"] == "viv"]
    assert cycles == pytest.approx([6_627_096, 1_980_000], rel=1e-6)


def test_source_digests_normalize_checkout_line_endings(tmp_path):
    module = generator()
    source = tmp_path / "source.txt"
    text = "UTF-8: fatigue – supplied response\nsecond line\n"
    source.write_text(text, encoding="utf-8", newline="\n")
    expected = hashlib.sha256(text.encode("utf-8")).hexdigest()
    assert module.source_digest(source) == expected
    source.write_text(text, encoding="utf-8", newline="\r\n")
    assert module.source_digest(source) == expected


def test_checkout_relative_csv_is_resolved_under_scratch(tmp_path, monkeypatch):
    module = generator()
    # Treat a temporary checkout as the root so the real router returns a
    # checkout-relative filename while every calculation stays outside source.
    monkeypatch.setattr("digitalmodel.riser_fatigue.workflow.REPO_ROOT", tmp_path)
    monkeypatch.chdir(tmp_path.parent)
    record = module.build_payload(tmp_path / "checkout" / "calculation")
    assert len(record["bins"]) == 12


@pytest.mark.parametrize("reordered", ["inputs", "results", "both"])
def test_location_labels_follow_ids_when_segments_are_reordered(built, reordered):
    module, original, _ = built
    record = copy.deepcopy(original)
    if reordered in {"inputs", "both"}:
        record["inputs"]["segments"].reverse()
    if reordered in {"results", "both"}:
        record["result"]["segments"].reverse()
    html = module.render(record)
    assert "The governing location is the touchdown zone" in html
    for index, segment in enumerate(record["result"]["segments"]):
        label = "Touchdown zone" if segment["segment_id"] == "TDZ-sandwave" else "Hang-off"
        assert re.search(rf'<td>{label}</td>\s*<td><span data-metric="result.segments.{index}.wave_damage"', html)


@pytest.mark.parametrize("location", ["inputs", "results", "bins", "chart", "governing"])
def test_unknown_location_ids_require_a_narrative_update(built, location):
    module, original, _ = built
    record = copy.deepcopy(original)
    if location == "inputs":
        record["inputs"]["segments"][0]["id"] = "unknown"
    elif location == "results":
        record["result"]["segments"][0]["segment_id"] = "unknown"
    elif location == "governing":
        record["result"]["governing_segment"] = "unknown"
    else:
        record[location][0]["segment_id" if location == "bins" else "component"] = "unknown"
    with pytest.raises(ValueError, match="Unknown segment id"):
        module.render(record)


def test_tables_and_narrative_use_the_fatigue_screening_register(built):
    _, _, html = built
    assert "caption-side: bottom" in html
    assert 'data-format=".3f">22.200</span>' in html
    assert 'data-format=".3f">25.400</span>' in html
    assert 'data-metric="touchdown.0.thickness_ref_mm" data-value="25.0" data-format=".3f">25.000</span>' in html
    assert 'data-format=",.0f">62,500</span>' in html
    assert 'data-format=",.0f">6,627,096</span>' in html
    assert "Without a watch circle" not in html
    assert "Not applicable to this supplied-stress fatigue screen" in html


def test_cli_uses_utf8_even_with_an_ascii_locale(tmp_path):
    subprocess.run(
        [sys.executable, "-X", "utf8=0", str(REPORT / "generate_report.py"),
         "--output-dir", str(tmp_path / "report"), "--scratch-dir", str(tmp_path / "scratch")],
        cwd=ROOT, check=True, capture_output=True,
        env={**os.environ, "LC_ALL": "C", "PYTHONCOERCECLOCALE": "0", "PYTHONUTF8": "0"},
    )
    assert (tmp_path / "report/report.html").read_bytes() == (REPORT / "report.html").read_bytes()


def test_ci_subsea_command_collects_the_report_suite(monkeypatch, capsys):
    """Run the workflow's actual command-selection code and retain every gate."""
    workflow = yaml.safe_load((ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text(encoding="utf-8"))
    gates = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text(encoding="utf-8"))["gates"]
    step = next(step for job in workflow["jobs"].values() for step in job.get("steps", [])
                if step.get("name") == "Read domain gate command")
    code = step["run"].split("<<'PY'\n", 1)[1].split("\nPY", 1)[0]
    for name, gate in gates.items():
        if not name.startswith("tests-") or "command" not in gate:
            continue
        monkeypatch.setenv("GATE_NAME", name)
        exec(compile(code, "<workflow command selection>", "exec"), {})
        command = shlex.split(capsys.readouterr().out.strip())
        original = shlex.split(gate["command"])
        assert command[:len(original)] == original
        if name == "tests-subsea":
            assert "tests/riser_fatigue/" in command
