# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the lifting-lug report and compare its evidence to public calls."""

import copy
import importlib.util
import json
import os
import re
import shutil
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

from digitalmodel.lifting_lug import workflow

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/liftinglug"
EXAMPLE = ROOT / "examples/workflows/lifting-lug-design/input.yml"


def read_utf8_lf(path):
    with path.open(encoding="utf-8", newline="\n") as source:
        return source.read()


class Page(HTMLParser):
    """Read the visible numerical fields and embedded chart evidence."""

    def __init__(self, text):
        super().__init__()
        self.numbers = []
        self.ids = set()
        self.links = []
        self.headings = []
        self.resources = []
        self.current = None
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.add(attrs["id"])
        if tag == "a":
            self.links.append(attrs.get("href", ""))
        if tag == "h2":
            self.headings.append(attrs.get("id"))
        if tag in ("script", "link", "img") and ("src" in attrs or "href" in attrs):
            self.resources.append(attrs.get("src", attrs.get("href")))
        if "data-number" in attrs:
            self.current = [attrs["data-number"], ""]

    def handle_data(self, data):
        if self.current is not None:
            self.current[1] += data

    def handle_endtag(self, tag):
        if tag == "span" and self.current is not None:
            self.numbers.append(tuple(self.current))
            self.current = None


@pytest.fixture
def rebuilt(tmp_path):
    spec = importlib.util.spec_from_file_location("lug_report", REPORT / "build_report.py")
    builder = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(builder)
    output = tmp_path / "report"
    builder.build(output, tmp_path / "calculations")
    text = read_utf8_lf(output / "lifting-lug-analysis.html")
    return builder, output, text


def direct_result(tmp_path):
    cfg = yaml.safe_load(read_utf8_lf(EXAMPLE))
    cfg["lifting_lug"]["outputs"]["directory"] = str(tmp_path)
    return workflow.router(cfg)["lifting_lug"]


def test_report_writes_utf8_lf_on_a_crlf_platform(rebuilt, tmp_path, monkeypatch):
    builder, _, _ = rebuilt
    write_text = Path.write_text

    def platform_write(path, text, encoding=None, errors=None, newline=None):
        # Emulate Windows' default translation even when this test runs on POSIX.
        return write_text(path, text, encoding=encoding, errors=errors,
                          newline="\r\n" if newline is None else newline)

    monkeypatch.setattr(Path, "write_text", platform_write)
    output = tmp_path / "crlf-platform"
    builder.build(output, tmp_path / "crlf-calculations")
    for name in ("lifting-lug-analysis.html", "changes.json"):
        content = (output / name).read_bytes()
        assert b"\r" not in content
        assert content.decode("utf-8").endswith("\n")
        assert content == (REPORT / name).read_bytes()


def test_rebuild_matches_committed_report_and_changes(rebuilt):
    _, output, _ = rebuilt
    for name in ("lifting-lug-analysis.html", "changes.json"):
        assert (output / name).read_bytes() == (REPORT / name).read_bytes()


def test_every_displayed_example_number_matches_hand_literals(rebuilt):
    _, _, text = rebuilt
    page = Page(text)
    # Public input: P = 420 * 2 * 1.1 = 924 kN; t = 40 + 2 * 0 = 40 mm.
    # Ligament = 110 - 80/2 = 70 mm; areas = 75*40 and 2*40*70 mm².
    # Demands = 924000/3000 = 308 and 924000/5600 = 165 MPa.
    # Limits = (0.90, 0.60, 0.40)*355 = (319.5, 213, 142) MPa.
    # Percentages = 100*(308/319.5, 165/213, 165/142); excess = last - 100.
    # Restriction = floor((420*142/165)*10)/10 = 361.4 kN.
    expected = {
        "lug.plate_thickness_mm": "40.000",
        "lug.cheek_plate_thickness_mm": "0.000",
        "lug.pin_diameter_mm": "75.0",
        "lug.hole_diameter_mm": "80.0",
        "lug.outer_radius_mm": "110.0",
        "lug.yield_strength_mpa": "355.0",
        "load.num_slings": "2",
        "load.static_load_kN": "420.0",
        "load.dynamic_amplification_factor": "2.0",
        "load.skew_load_factor": "1.1",
        "design_load_kN": "924.0",
        "total_thickness_mm": "40.000",
        "skew_load_factor": "1.1",
        "skew_load_factor_minimum": "1.1",
        "pin_bearing.demand_mpa": "308.0",
        "pin_bearing.allowable_mpa": "319.5",
        "pin_bearing.utilization": "96.4",
        "net_tension.demand_mpa": "165.0",
        "net_tension.allowable_mpa": "213.0",
        "net_tension.utilization": "77.5",
        "shear_tearout.demand_mpa": "165.0",
        "shear_tearout.allowable_mpa": "142.0",
        "shear_tearout.utilization": "116.2",
        "bearing_factor": "0.90",
        "tension_factor": "0.60",
        "shear_factor": "0.40",
        "limit_percent": "100.0",
        "governing_percent": "116.2",
        "exceedance_percent": "16.2",
        "ligament_mm": "70.0",
        "static_cap_kN": "361.4",
    }
    assert {key for key, _ in page.numbers} == set(expected)
    assert len(page.numbers) > len(expected)
    for key, displayed in page.numbers:
        assert displayed == expected[key], key
    # Every decimal visible outside the chart is a labelled numerical field.
    stripped = re.sub(r'<span data-number="[^"]+">[^<]+</span>', "", text)
    stripped = re.sub(r"<(script|style|svg)\b.*?</\1>", "", stripped, flags=re.S)
    stripped = re.sub(r"<[^>]+>", "", stripped)
    # Section numbers and clause locators identify text, not engineering data.
    stripped = re.sub(r"\b9\.[12]\b|§\d+(?:\.\d+)+", "", stripped)
    assert not re.search(r"\b\d+\.\d+\b", stripped)
    assert "OrcFxAPI" not in sys.modules


def chart_data(text):
    match = re.search(r'<script id="chart-data" type="application/json">(.*?)</script>',
                      text, re.S)
    return json.loads(match[1])


def test_every_chart_point_matches_module_and_hand_arithmetic(rebuilt, tmp_path):
    data = chart_data(rebuilt[2])
    assert data["limit_percent"] == 100.0
    assert len(data["cases"]) > 10
    for index, case in enumerate(data["cases"]):
        cfg = yaml.safe_load(read_utf8_lf(EXAMPLE))
        cfg["lifting_lug"]["load"]["static_load_kN"] = case["static_load_kN"]
        cfg["lifting_lug"]["outputs"]["directory"] = str(tmp_path / str(index))
        result = workflow.router(cfg)["lifting_lug"]
        assert case["status"] == result["screening_status"]
        # Independent areas and limits from the hand calculation above.
        expected_utilizations = []
        for plotted, area, limit in zip(case["checks"], (3000, 5600, 5600),
                                        (319.5, 213.0, 142.0), strict=True):
            demand = case["static_load_kN"] * 2 * 1.1 * 1000 / area
            utilization = 100 * demand / limit
            expected_utilizations.append(utilization)
            assert plotted["demand_mpa"] == round(demand, 1)
            assert plotted["allowable_mpa"] == limit
            assert plotted["utilization_percent"] == round(utilization, 1)
        assert case["status"] == ("pass" if max(expected_utilizations) <= 100 else "fail")
        for actual, plotted in zip(result["checks"], case["checks"], strict=True):
            assert plotted["component"] == actual["name"]
            assert plotted["demand_mpa"] == float(f"{actual['demand_mpa']:.1f}")
            assert plotted["allowable_mpa"] == float(f"{actual['allowable_mpa']:.1f}")
            assert plotted["utilization_percent"] == float(f"{actual['utilization'] * 100:.1f}")
    assert {case["status"] for case in data["cases"]} == {"pass", "fail"}


def test_recommended_load_is_rounded_down_and_passes(rebuilt, tmp_path):
    result = direct_result(tmp_path / "original")
    cap = 361.4  # floor((420 * 142 / 165) * 10) / 10, independent of the router.
    assert dict(Page(rebuilt[2]).numbers)["static_cap_kN"] == "361.4"
    cfg = yaml.safe_load(read_utf8_lf(EXAMPLE))
    cfg["lifting_lug"]["load"]["static_load_kN"] = cap
    cfg["lifting_lug"]["outputs"]["directory"] = str(tmp_path / "limited")
    limited = workflow.router(copy.deepcopy(cfg))["lifting_lug"]
    assert limited["screening_status"] == "pass"
    assert cap <= result["load"]["static_load_kN"] / result["governing_utilization"]
    cfg["lifting_lug"]["load"]["static_load_kN"] = cap + 0.1
    assert workflow.router(cfg)["lifting_lug"]["screening_status"] == "fail"


def test_report_is_offline_house_style_with_resolving_citations(rebuilt):
    builder, _, text = rebuilt
    page = Page(text)
    assert 'class="hs-doc"' in text
    assert len(page.headings) == 10
    assert not page.resources
    assert builder.house_style() in text
    for link in page.links:
        if link.startswith("#"):
            assert link[1:] in page.ids
        elif not link.startswith("https:"):
            assert (REPORT / link).resolve().is_file(), link
    assert "Draft for maintainer review" in text
    assert '<h2 id="controls">3 Load Sensitivity</h2>' in text
    assert "The dashed tear-out planes are conceptual, not to scale" in text
    assert 'calculation uses a loaded ligament of <span data-number="ligament_mm">70.0</span> mm' in text
    assert "NOT_EVALUATED" in text
    assert "ASSUMED - to be confirmed" in text
    assert "No field-load measurements" in text
    assert not re.search(r"<(?:b|strong)\b", text)


def test_comment_and_change_panels_are_embedded(rebuilt):
    _, output, text = rebuilt
    changes = json.loads(read_utf8_lf(output / "changes.json"))
    embedded = re.search(r'<script id="changes-data" type="application/json">(.*?)</script>',
                         text, re.S)
    assert json.loads(embedded[1]) == changes
    assert "showSaveFilePicker" in text
    assert 'download = "lifting-lug-analysis.comments.json"' in text
    assert 'id="import-comments"' in text
    assert "textContent" in text
    assert "localStorage" not in text


def test_report_numbers_follow_changed_module_results(rebuilt, tmp_path, monkeypatch):
    builder, _, _ = rebuilt
    unchanged = direct_result(tmp_path / "unchanged")["checks"][-1]["allowable_mpa"]
    monkeypatch.setattr(builder.workflow, "SHEAR_FACTOR", builder.workflow.SHEAR_FACTOR * 0.95)
    cfg = yaml.safe_load(read_utf8_lf(EXAMPLE))
    result = builder.evaluate(cfg, tmp_path / "changed")
    values = builder.numerical_fields(result)
    chart = builder.chart_cases(cfg, values, tmp_path / "sweep")
    displayed = dict(Page(builder.render(result, values, chart)).numbers)
    assert float(displayed["shear_tearout.allowable_mpa"]) == round(result["checks"][-1]["allowable_mpa"], 1)
    assert float(displayed["shear_tearout.allowable_mpa"]) != unchanged


def test_interactive_selection_and_comment_file_roundtrip(rebuilt, tmp_path):
    """Exercise the shipped JavaScript with a small DOM, without a browser."""
    node = shutil.which("node")
    if node is None:
        pytest.skip("JavaScript runtime is unavailable")
    builder, _, text = rebuilt
    initial = re.search(r'id="case-slider"[^>]*value="(\d+)"', text)[1]
    payload = {
        "code": builder.INTERACTION,
        "chart": chart_data(text),
        "changes": builder.CHANGES,
        "initial": initial,
        "sections": re.findall(r'<h2 id="([^"]+)">([^<]+)</h2>', text),
    }
    harness = r'''
const assert = require("node:assert/strict");
const vm = require("node:vm");
const input = JSON.parse(require("node:fs").readFileSync(0, "utf8"));
class Element {
  constructor(tag) { this.tag = tag; this.children = []; this.events = {}; this.value = ""; this.textContent = ""; }
  setAttribute(key, value) { this[key] = value; }
  append(child) { this.children.push(child); }
  replaceChildren(...children) { this.children = children; }
  addEventListener(event, callback) { this.events[event] = callback; }
  click() { this.clicked = true; }
  set innerHTML(value) { throw new Error("Untrusted HTML insertion"); }
}
const nodes = new Map();
const get = id => { if (!nodes.has(id)) nodes.set(id, new Element("div")); return nodes.get(id); };
get("chart-data").textContent = JSON.stringify(input.chart);
get("changes-data").textContent = JSON.stringify(input.changes);
get("case-slider").value = input.initial;
const headings = input.sections.map(([id, text]) => { const h = get(id); h.id = id; h.textContent = text; return h; });
let saved = "", downloaded = "", revoked = false;
const document = {getElementById: get, querySelectorAll: () => headings,
  createElement: tag => new Element(tag), createElementNS: (ns, tag) => new Element(tag)};
const window = {confirm: () => true, showSaveFilePicker: async () => ({createWritable: async () => ({
  write: async payload => { saved = payload; }, close: async () => {}
})})};
const context = {document, window, setTimeout: fn => fn(),
  Blob: class {constructor(parts) { downloaded = parts.join(""); }},
  URL: {createObjectURL: () => "blob:comments", revokeObjectURL: () => { revoked = true; }}};
vm.runInNewContext(input.code, context);
(async () => {
  assert.match(get("chart-readout").textContent, /demand: .* MPa; limit: .* MPa/);
  get("case-slider").value = "0"; get("case-slider").events.input();
  assert.match(get("chart-readout").textContent, /The screen passes/);
  const points = get("load-chart").children[0].children.filter(c => c.tag === "circle");
  points[points.length - 1].events.focus();
  assert.match(get("chart-readout").textContent, /The screen fails/);
  get("comment-section").value = headings[0].id;
  get("comment-text").value = "<img onerror=invalid> Check the load basis.";
  get("add-comment").events.click();
  await get("save-comments").events.click();
  const exported = JSON.parse(saved);
  assert.equal(exported.comments[0].text, "<img onerror=invalid> Check the load basis.");
  assert.equal(exported.comments[0].section, headings[0].id);
  assert.equal(exported.revision, input.changes.revisions.at(-1).revision);
  assert.match(get("comment-status").textContent, /have been saved/);
  await get("import-comments").events.change({target: {files: [{text: async () => saved}]}});
  assert.match(get("comment-status").textContent, /have been imported/);
  assert.match(get("comment-list").children[0].textContent, /<img onerror=invalid>/);
  const invalid = {...exported, revision: "unknown"};
  await get("import-comments").events.change({target: {files: [{text: async () => JSON.stringify(invalid)}]}});
  assert.match(get("comment-status").textContent, /not a comments file/);
  assert.equal(get("comment-list").children.length, 1);
  const oldRevision = {...exported, revision: input.changes.revisions[0].revision};
  await get("import-comments").events.change({target: {files: [{text: async () => JSON.stringify(oldRevision)}]}});
  assert.match(get("comment-status").textContent, /not a comments file/);
  assert.equal(get("comment-list").children.length, 1);
  window.showSaveFilePicker = undefined;
  await get("save-comments").events.click();
  assert.equal(downloaded, saved); assert.equal(revoked, true);
  window.showSaveFilePicker = async () => { throw new Error("Cancelled"); };
  await get("save-comments").events.click();
  assert.match(get("comment-status").textContent, /were not saved/);
})().catch(error => { console.error(error); process.exitCode = 1; });
'''
    env = os.environ.copy()
    for name in ("UV_TOOL_DIR", "PRE_COMMIT_HOME", "XDG_CACHE_HOME", "npm_config_cache"):
        env[name] = str(tmp_path / name.lower())
    result = subprocess.run([node, "-e", harness], input=json.dumps(payload), text=True,
                            capture_output=True, env=env, timeout=30)
    assert result.returncode == 0, result.stderr
