# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the lifting-lug report and compare its evidence to public calls."""

import copy
import importlib.util
import json
import math
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
    text = (output / "lifting-lug-analysis.html").read_text(encoding="utf-8")
    return builder, output, text


def direct_result(tmp_path):
    cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    cfg["lifting_lug"]["outputs"]["directory"] = str(tmp_path)
    return workflow.router(cfg)["lifting_lug"]


def expected_numbers(result):
    """Use module results, not rounded report data, as the numerical oracle."""
    values = {f"lug.{k}": v for k, v in result["lug"].items()}
    values.update({f"load.{k}": v for k, v in result["load"].items()
                   if isinstance(v, (int, float))})
    for key in ("design_load_kN", "total_thickness_mm", "skew_load_factor",
                "skew_load_factor_minimum"):
        values[key] = result[key]
    for check in result["checks"]:
        for key in ("demand_mpa", "allowable_mpa", "utilization"):
            value = check[key] * (100 if key == "utilization" else 1)
            values[f"{check['name']}.{key}"] = value
    values.update({
        "bearing_factor": workflow.BEARING_FACTOR,
        "tension_factor": workflow.TENSION_FACTOR,
        "shear_factor": workflow.SHEAR_FACTOR,
        "limit_percent": 100.0,
        "governing_percent": result["governing_utilization"] * 100,
        "exceedance_percent": (result["governing_utilization"] - 1) * 100,
        "ligament_mm": result["lug"]["outer_radius_mm"]
        - result["lug"]["hole_diameter_mm"] / 2,
        "static_cap_kN": math.floor(result["load"]["static_load_kN"]
                                     / result["governing_utilization"] * 10) / 10,
    })
    return values


def test_rebuild_matches_committed_report_and_changes(rebuilt):
    _, output, _ = rebuilt
    for name in ("lifting-lug-analysis.html", "changes.json"):
        assert (output / name).read_bytes() == (REPORT / name).read_bytes()


def test_every_displayed_example_number_matches_module(rebuilt, tmp_path):
    _, _, text = rebuilt
    page = Page(text)
    expected = expected_numbers(direct_result(tmp_path / "oracle"))
    assert {key for key, _ in page.numbers} == set(expected)
    assert len(page.numbers) > len(expected)
    for key, displayed in page.numbers:
        places = 2 if key.endswith("factor") and key.split(".")[0] in {
            "bearing_factor", "tension_factor", "shear_factor"
        } else 1
        assert displayed == f"{expected[key]:.{places}f}", key
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


def test_every_chart_point_is_a_module_call(rebuilt, tmp_path):
    data = chart_data(rebuilt[2])
    assert data["limit_percent"] == 100.0
    assert len(data["cases"]) > 10
    for index, case in enumerate(data["cases"]):
        cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
        cfg["lifting_lug"]["load"]["static_load_kN"] = case["static_load_kN"]
        cfg["lifting_lug"]["outputs"]["directory"] = str(tmp_path / str(index))
        result = workflow.router(cfg)["lifting_lug"]
        assert case["status"] == result["screening_status"]
        for actual, plotted in zip(result["checks"], case["checks"], strict=True):
            assert plotted["component"] == actual["name"]
            assert plotted["demand_mpa"] == float(f"{actual['demand_mpa']:.1f}")
            assert plotted["allowable_mpa"] == float(f"{actual['allowable_mpa']:.1f}")
            assert plotted["utilization_percent"] == float(f"{actual['utilization'] * 100:.1f}")
    assert {case["status"] for case in data["cases"]} == {"pass", "fail"}


def test_recommended_load_is_rounded_down_and_passes(rebuilt, tmp_path):
    result = direct_result(tmp_path / "original")
    cap = expected_numbers(result)["static_cap_kN"]
    cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
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
    assert "Draft for client review" in text
    assert "NOT_EVALUATED" in text
    assert "ASSUMED - to be confirmed" in text
    assert "No field-load measurements" in text
    assert not re.search(r"<(?:b|strong)\b", text)


def test_comment_and_change_panels_are_embedded(rebuilt):
    _, output, text = rebuilt
    changes = json.loads((output / "changes.json").read_text())
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
    cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
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
  assert.equal(exported.revision, input.changes.revisions[0].revision);
  assert.match(get("comment-status").textContent, /have been saved/);
  await get("import-comments").events.change({target: {files: [{text: async () => saved}]}});
  assert.match(get("comment-status").textContent, /have been imported/);
  assert.match(get("comment-list").children[0].textContent, /<img onerror=invalid>/);
  const invalid = {...exported, revision: "unknown"};
  await get("import-comments").events.change({target: {files: [{text: async () => JSON.stringify(invalid)}]}});
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
