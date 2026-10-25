# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the module report and verify its public example independently."""

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

import numpy as np
import pytest

from digitalmodel.fatigue.assessment import AssessmentInput, assess_fatigue
from digitalmodel.fatigue.c203_sn_tables import BILINEAR, N_KNEE_AIR
from digitalmodel.fatigue.rainflow import rainflow_count
from digitalmodel.fatigue.sn_curves import get_sn_curve

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/fatigue"
SPEC = importlib.util.spec_from_file_location(
    "fatigue_module_report", ROOT / "scripts/build_fatigue_module_report.py"
)
builder = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(builder)


class ReportParser(HTMLParser):
    """Collect displayed numeric fields, headings and source links."""

    def __init__(self):
        super().__init__()
        self.values = {}
        self.headings = []
        self.ids = set()
        self.links = []
        self.hover_labels = []
        self.current_key = None
        self.in_heading = False
        self.visible = []
        self.hidden_depth = 0

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag in {"style", "script"}:
            self.hidden_depth += 1
        if "id" in attrs:
            assert attrs["id"] not in self.ids
            self.ids.add(attrs["id"])
        if tag == "a":
            self.links.append(attrs.get("href", ""))
        if tag == "circle" and "aria-label" in attrs:
            self.hover_labels.append(attrs["aria-label"])
        if tag == "h2":
            self.in_heading = True
        if "data-value" in attrs:
            self.current_key = attrs["data-value"]
            assert self.current_key not in self.values
            self.values[self.current_key] = ""

    def handle_data(self, data):
        if self.current_key:
            self.values[self.current_key] += data
        if self.in_heading:
            self.headings.append(data)
        if not self.hidden_depth:
            self.visible.append(data)

    def handle_endtag(self, tag):
        if tag in {"style", "script"}:
            self.hidden_depth -= 1
        if tag == "h2":
            self.in_heading = False
        if tag == "span":
            self.current_key = None


@pytest.fixture(scope="module")
def reference():
    """Evaluate the published signal without using the report generator."""
    fixture = json.loads((REPORT / "public-example.json").read_text(encoding="utf-8"))
    w = fixture["waveform"]
    times = [w["span"] * i / w["intervals"] for i in range(w["intervals"] + 1)]
    signal = [w["offset"] + w["sine_amplitude"] * math.sin(t)
              + w["cosine_amplitude"] * math.cos(w["cosine_frequency"] * t)
              + w["secondary_sine_amplitude"] * math.sin(w["secondary_sine_frequency"] * t)
              for t in times]
    a = fixture["assumptions"]
    stress = np.array(signal) * a["stress_scale_mpa"]
    inp = AssessmentInput.model_validate({
        **{k: a[k] for k in ("sn_curve_name", "environment", "block_years",
                             "design_years", "design_fatigue_factor")},
        "load_cases": [{"name": "Public waveform", "kind": "history",
                        "stress_history_mpa": stress.tolist()}],
    })
    return fixture, times, stress, inp, assess_fatigue(inp)


def test_published_count_distribution(reference):
    """The external expected counts qualify this example's counting result."""
    fixture, _, stress, _, _ = reference
    counts = rainflow_count(stress).sort_values("stress_range")
    expected = fixture["published_range_counts"]
    np.testing.assert_allclose(counts.stress_range,
                               [row[0] * fixture["assumptions"]["stress_scale_mpa"]
                                for row in expected], rtol=1e-12)
    np.testing.assert_array_equal(counts.cycles, [row[1] for row in expected])


def test_every_worked_value_and_chart_point_comes_from_native_result(reference):
    fixture, times, stress, inp, result = reference
    data = builder.calculate()
    assert data["input"] == inp.model_dump()
    assert data["result"] == result.model_dump()
    assert data["time"] == times
    assert data["stress"] == stress.tolist()
    curve = get_sn_curve(inp.sn_curve_name, inp.environment)
    native_damage = sum(c.cycles / float(curve.cycles(c.stress_range_mpa))
                        for c in result.cases[0].cycles)
    assert result.block_damage == pytest.approx(native_damage, rel=1e-12)
    assert data["metrics"]["cycle_total"] == sum(c.cycles for c in result.cases[0].cycles)
    assert data["metrics"]["sample_count"] == len(times)
    assert data["metrics"]["maximum_range"] == max(c.stress_range_mpa for c in result.cases[0].cycles)
    assert data["metrics"]["block_damage"] == result.block_damage
    assert data["metrics"]["life_years"] == result.life_years
    assert data["metrics"]["utilisation"] == result.utilisation
    assert data["metrics"]["block_years"] == inp.block_years
    assert data["metrics"]["design_years"] == inp.design_years
    assert data["metrics"]["fatigue_factor"] == inp.design_fatigue_factor
    assert data["metrics"]["acceptance_limit"] == 1.0
    assert data["metrics"]["stress_scale"] == fixture["assumptions"]["stress_scale_mpa"]
    assert data["metrics"]["sample_span"] == fixture["waveform"]["span"]
    counted = sorted(result.cases[0].cycles, key=lambda c: c.stress_range_mpa)
    errors = [abs(c.stress_range_mpa - public[0] * fixture["assumptions"]["stress_scale_mpa"])
              / (public[0] * fixture["assumptions"]["stress_scale_mpa"]) * 100
              for c, public in zip(counted, fixture["published_range_counts"])]
    assert data["metrics"]["count_difference_bound_percent"] == math.ceil(max(errors) * 10) / 10
    assert max(errors) <= data["metrics"]["count_difference_bound_percent"]
    for point in data["exposure"]:
        native = assess_fatigue(inp.model_copy(update={"design_years": point["years"]}))
        assert point["utilisation"] == native.utilisation
    assert all(point["limit"] == 1.0 for point in data["exposure"])
    assert min(p["utilisation"] for p in data["exposure"]) < 1.0
    assert max(p["utilisation"] for p in data["exposure"]) > 1.0
    assert data["metrics"]["exposure_at_limit"] == pytest.approx(
        inp.block_years / (result.block_damage * inp.design_fatigue_factor))
    assert fixture["source"]["section"].startswith("Usage")


def test_published_rows_give_hand_calculated_damage_life_and_utilisation(reference):
    """Evaluate both resistance segments without the native cycle evaluator."""
    fixture, _, _, _, result = reference
    assumptions = fixture["assumptions"]
    assert (assumptions["sn_curve_name"], assumptions["environment"]) == ("D", "air")
    parameters = BILINEAR["D"]
    # DNV-RP-C203 (October 2011), section 2.4, S-N curves:
    # S_D = (a_1/N_D)^(1/m_1); N(S) = N_D*(S_D/S)^m,
    # with m_1 above the knee and m_2 below it. Existing parameter records
    # are reused; neither get_sn_curve nor curve.cycles supplies expectations.
    knee = (10 ** parameters.log_a1_air / N_KNEE_AIR) ** (1 / parameters.m1)
    low, high = [], []
    for public_range, count in fixture["published_range_counts"]:
        stress = public_range * assumptions["stress_scale_mpa"]
        above_knee = stress >= knee
        slope = parameters.m1 if above_knee else parameters.m2
        allowable = N_KNEE_AIR * (knee / stress) ** slope
        (high if above_knee else low).append(count / allowable)
    assert (len(low), len(high)) == (7, 3)
    # DNV-RP-C203 (October 2011), section 2.2, Fatigue damage accumulation.
    # Hand sum of the cited ten rows: seven low-range contributions plus
    # three high-range contributions give D = sum(n_i/N_i), dimensionless.
    assert math.fsum(low) == pytest.approx(7.49184385774e-8, rel=1e-10, abs=0)
    assert math.fsum(high) == pytest.approx(1.22532222645e-6, rel=1e-10, abs=0)
    damage = math.fsum(low + high)
    assert damage == pytest.approx(1.30024066503e-6, rel=1e-10, abs=0)
    # Repeating the assumed one-year block: L = 1/D years; exposure demand
    # is U = D*(25/1)*1. These are duration bookkeeping, not a life forecast.
    life = assumptions["block_years"] / damage
    utilisation = (damage * assumptions["design_years"] / assumptions["block_years"]
                   * assumptions["design_fatigue_factor"])
    assert life == pytest.approx(769088.390246, rel=1e-10, abs=0)
    assert utilisation == pytest.approx(3.25060166257e-5, rel=1e-10, abs=0)
    metrics = builder.calculate()["metrics"]
    for actual, expected in (
        (result.block_damage, damage), (result.life_years, life),
        (result.utilisation, utilisation), (metrics["block_damage"], damage),
        (metrics["life_years"], life), (metrics["utilisation"], utilisation),
    ):
        assert actual == pytest.approx(expected, rel=1e-10, abs=0)


def test_rebuild_is_deterministic_and_committed_outputs_are_current(tmp_path):
    builder.build(tmp_path)
    for name in ("report.html", "changes.json"):
        assert (tmp_path / name).read_bytes() == (REPORT / name).read_bytes(), (
            "Rebuild with PYTHONPATH=src python scripts/build_fatigue_module_report.py"
        )
    builder.build(tmp_path)
    assert (tmp_path / "report.html").read_bytes() == (REPORT / "report.html").read_bytes(), (
        "Rebuild with PYTHONPATH=src python scripts/build_fatigue_module_report.py"
    )


def test_rebuild_with_ascii_locale_writes_utf8_and_lf(tmp_path):
    env = os.environ.copy()
    env.update(PYTHONUTF8="0", PYTHONCOERCECLOCALE="0", LC_ALL="C")
    rebuilt = subprocess.run(
        [sys.executable, "-X", "utf8=0", str(ROOT / "scripts/build_fatigue_module_report.py"),
         "--output-dir", str(tmp_path)],
        env=env, capture_output=True, text=True, encoding="utf-8", timeout=30,
    )
    assert rebuilt.returncode == 0, rebuilt.stderr
    for name in ("report.html", "changes.json"):
        output = (tmp_path / name).read_bytes()
        output.decode("utf-8")
        assert b"\r" not in output
        assert output == (REPORT / name).read_bytes(), (
            "Rebuild with PYTHONPATH=src python scripts/build_fatigue_module_report.py"
        )


def test_visible_values_structure_and_self_containment():
    data = builder.calculate()
    html = (REPORT / "report.html").read_text(encoding="utf-8")
    parser = ReportParser()
    parser.feed(html)
    assert parser.values == {key: builder.display(value) for key, value in data["metrics"].items()}
    embedded = re.search(r'id="calculation-record">(.*?)</script>', html, re.S)[1]
    assert json.loads(embedded) == data
    for cycle in data["result"]["cases"][0]["cycles"]:
        cells = ''.join(f'<td>{builder.display(cycle[key])}</td>' for key in
                        ("stress_range_mpa", "cycles", "allowable_cycles", "damage"))
        assert cells in html
    expected_hover = []
    for x, demand in zip(data["time"], data["stress"]):
        expected_hover.append(
            "Case: Public waveform; component: illustrative hot spot; "
            f"demand: {builder.display(demand)} MPa; limit: NOT_EVALUATED; "
            f"Published sample coordinate (dimensionless): {builder.display(x)}; units: MPa")
    for point in data["exposure"]:
        expected_hover.append(
            "Case: Public waveform; component: illustrative hot spot; "
            f"demand: {builder.display(point['utilisation'])} dimensionless; "
            "limit: 1.0 dimensionless; "
            f"Assumed exposure (years): {builder.display(point['years'])}; units: dimensionless")
    assert parser.hover_labels == expected_hover
    assert len(parser.headings) == 10
    assert all(h.startswith(f"{i}. ") for i, h in enumerate(parser.headings, 1))
    assert 'class="hs-doc"' in html
    assert '<script src=' not in html
    assert '<link rel="stylesheet"' not in html
    assert 'showDirectoryPicker' in html and 'report.comments.json' in html
    assert 'createWritable' in html and 'JSON.parse' in html
    assert 'textContent' in html
    for href in parser.links:
        if href.startswith("#"):
            assert href[1:] in parser.ids, href
        elif not href.startswith("https:"):
            assert (REPORT / href.split("#")[0]).resolve().exists(), href
    text = " ".join(parser.visible)
    for word in ("could", "should", "would", "certified", "compliant"):
        assert word not in text.lower().split()
    assert '<b>' not in html and '<strong>' not in html
    assert 'ASSUMED - to be confirmed' in text
    assert 'NOT_EVALUATED' in text
    assert '<title>Case: Public waveform; component:' in html


def test_comment_roundtrip_and_chart_handlers_without_a_browser(tmp_path):
    """Exercise the emitted script against a small DOM and sidecar interface."""
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable for the offline interaction check")
    html = (REPORT / "report.html").read_text(encoding="utf-8")
    script = re.findall(r'<script>(.*?)</script>', html, re.S)[0]
    harness = r'''
const vm = require("node:vm");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const elements = {};
function element(id) {
  if (!elements[id]) elements[id] = {
    value: "", textContent: "", children: [], handlers: {},
    addEventListener(name, fn) { this.handlers[name] = fn; },
    appendChild(child) { this.children.push(child); },
    replaceChildren() { this.children = []; },
  };
  return elements[id];
}
element("engineering-changes").textContent = JSON.stringify({revisions: []});
let saved = "";
let writes = 0;
let existing = false;
let approve = true;
const sidecar = {async createWritable() { return {
  async write(value) { saved = value; writes += 1; }, async close() {}
}; }};
const folder = {async getFileHandle(name, options) {
  if (name === "report.html") return {};
  assert.equal(name, "report.comments.json");
  if (!existing && !options) {
    const error = new Error("Not found"); error.name = "NotFoundError"; throw error;
  }
  existing = true;
  return sidecar;
}};
const output = {};
const point = {handlers: {}, getAttribute() { return "Demand and limit"; },
  addEventListener(name, fn) { this.handlers[name] = fn; }};
const figure = {querySelectorAll() { return [point]; }, querySelector() { return output; }};
const context = {
  document: {
    querySelectorAll() { return [figure]; },
    getElementById: element,
    createElement() { return {}; },
  },
  window: {showDirectoryPicker: async () => folder, confirm: () => approve},
};
vm.createContext(context);
vm.runInContext(fs.readFileSync(process.argv[2], "utf8"), context);
(async () => {
  point.handlers.focus(); assert.equal(output.textContent, "Demand and limit");
  point.handlers.pointerenter(); assert.equal(output.textContent, "Demand and limit");
  element("comment-section").value = "5. Detailed results";
  element("comment-text").value = "Check <img src=x> exposure basis";
  element("add-comment").handlers.click();
  assert.equal(element("comment-list").children[0].textContent,
               "5. Detailed results: Check <img src=x> exposure basis");
  await element("save-comments").handlers.click();
  assert.equal(JSON.parse(saved).comments[0].text, "Check <img src=x> exposure basis");
  assert.equal(writes, 1);
  approve = false;
  await element("save-comments").handlers.click(); assert.equal(writes, 1);
  await element("load-comments").handlers.change({target: {files: [{text: async () => saved}]}});
  assert.equal(element("comment-list").children.length, 1);
  await element("load-comments").handlers.change({target: {files: [{text: async () => "{}"}]}});
  assert.match(element("comment-status").textContent, /does not match/);
  approve = true;
  await element("save-comments").handlers.click(); assert.equal(writes, 2);
})().catch(error => { console.error(error); process.exitCode = 1; });
'''
    script_path = tmp_path / "interaction.js"
    script_path.write_text(script, encoding="utf-8", newline="\n")
    harness_path = tmp_path / "check.cjs"
    harness_path.write_text(harness, encoding="utf-8", newline="\n")
    checked = subprocess.run([node, str(harness_path), str(script_path)],
                             capture_output=True, text=True, encoding="utf-8", timeout=20)
    assert checked.returncode == 0, checked.stderr
