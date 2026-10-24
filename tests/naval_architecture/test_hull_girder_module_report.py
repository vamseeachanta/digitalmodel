# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the module report and compare its numbers with the public router."""

import copy
import importlib.util
import json
import re
import subprocess
from html.parser import HTMLParser
from pathlib import Path

import numpy as np
import pytest
import yaml

from digitalmodel.hull_girder_screening import router

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/hullgirderscreening"
EXAMPLE = ROOT / "examples/workflows/hull-girder-report-pack/screening_input.yml"


def generator():
    spec = importlib.util.spec_from_file_location("module_report", REPORT / "build_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class ReportParser(HTMLParser):
    """Collect rendered numeric spans, links, headings and embedded results."""

    def __init__(self, text):
        super().__init__()
        self.numbers = []
        self.ids = []
        self.links = []
        self.headings = []
        self.current = None
        self.payload = ""
        self.in_payload = False
        self.resources = []
        self.nonvisible = False
        self.visible = []
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag in ("script", "style"):
            self.nonvisible = True
        if (tag == "link" and attrs.get("rel") == "stylesheet") or (tag == "script" and "src" in attrs):
            self.resources.append(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "a":
            self.links.append(attrs["href"])
        if tag in ("h2", "h3", "h4", "h5", "h6"):
            self.headings.append(tag)
        if tag in ("span", "tspan") and "data-number" in attrs:
            self.current = [attrs, ""]
        if tag == "script" and attrs.get("id") == "calculation-data":
            self.in_payload = True

    def handle_data(self, data):
        if not self.nonvisible:
            self.visible.append(data)
        if self.current is not None:
            self.current[1] += data
        if self.in_payload:
            self.payload += data

    def handle_endtag(self, tag):
        if tag in ("span", "tspan") and self.current is not None:
            self.numbers.append(self.current)
            self.current = None
        if tag in ("script", "style"):
            self.nonvisible = False
        if tag == "script":
            self.in_payload = False


@pytest.fixture
def rebuilt(tmp_path):
    module = generator()
    module.build(tmp_path / "report", tmp_path / "calculations")
    text = (tmp_path / "report/report.html").read_text(encoding="utf-8")
    return module, text, ReportParser(text)


def independent_result(tmp_path):
    settings = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))["hull_girder_screening"]
    settings.pop("wave_loads")
    settings.pop("section_modulus")
    settings["allowables"] = {
        k: v for k, v in settings["allowables"].items() if not k.startswith("total_")
    }
    saved = copy.deepcopy(settings)
    settings["output_dir"] = str(tmp_path)
    result = router({"hull_girder_screening": settings})["hull_girder_screening"]
    return saved, result


def test_report_rebuild_is_exact(rebuilt, tmp_path):
    module, text, parsed = rebuilt
    assert text == (REPORT / "report.html").read_text(encoding="utf-8")
    assert (tmp_path / "report/changes.json").read_bytes() == (REPORT / "changes.json").read_bytes()
    assert 'class="hs-doc"' in text
    style = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    assert style in text
    assert parsed.resources == []
    module.build(tmp_path / "again", tmp_path / "second-calculation")
    assert text == (tmp_path / "again/report.html").read_text(encoding="utf-8")


def test_all_worked_numbers_and_plot_samples_equal_router(rebuilt, tmp_path):
    module, _, parsed = rebuilt
    inputs, result = independent_result(tmp_path / "independent")
    payload = json.loads(parsed.payload)
    assert payload["inputs"] == inputs
    # Every published station and frame field is checked against a fresh call.
    assert payload["result"] == {k: result[k] for k in payload["result"]}
    assert len(payload["result"]["stations"]) == inputs["n_stations"]
    assert len(parsed.numbers) > 40
    for attrs, displayed in parsed.numbers:
        value = payload
        for part in attrs["data-number"].split("."):
            value = value[int(part)] if isinstance(value, list) else value[part]
        scaled = value * float(attrs.get("data-scale", "1"))
        expected = (f"{scaled:,.0f}" if attrs.get("data-format") == "integer"
                    else module.format_number(scaled))
        assert displayed == expected
    block = inputs["weights"]["lightship"][0]
    assert payload["comparison"]["moment"]["reference"] == block["weight_t"] * inputs["length_m"] / 16
    assert payload["comparison"]["shear"]["reference"] == block["weight_t"] / 4
    for row in payload["comparison"].values():
        assert row["difference_percent"] == pytest.approx(100 * (row["calculated"] - row["reference"]) / row["reference"])
        assert abs(row["difference_percent"]) <= row["tolerance_percent"]
    assert payload["comparison"]["moment"]["calculated"] == abs(result["max_sagging_t_m"])
    assert payload["comparison"]["shear"]["calculated"] == abs(result["max_shear_t"])
    basis = json.loads((ROOT / "examples/workflows/hull-girder-report-pack/allowable-basis.json").read_text(encoding="utf-8"))
    assert payload["comparison"]["moment"]["tolerance_percent"] == 100 * basis["validation"]["bending_relative_tolerance"]
    assert payload["comparison"]["shear"]["tolerance_percent"] == 100 * basis["validation"]["shear_relative_tolerance"]
    assert result["closure_ok"]
    assert result["max_shear_utilization"] == pytest.approx(0.4975)
    assert result["max_bending_utilization"] == pytest.approx(0.499975)
    assert payload["strength_disposition"] == "NOT_EVALUATED"


def test_links_structure_and_public_text(rebuilt):
    _, text, parsed = rebuilt
    assert len(parsed.ids) == len(set(parsed.ids))
    assert parsed.headings.count("h2") == 10
    assert set(parsed.headings) == {"h2", "h3"}
    for href in parsed.links:
        if href.startswith("#"):
            assert href[1:] in parsed.ids
        elif not href.startswith("https:"):
            assert (REPORT / href).resolve().exists()
    assert text.index('id="revision-history"') < text.index('id="section-2"')
    assert "ASSUMED - to be confirmed" in text
    assert "No approved loading-instrument comparison" in text
    assert "showSaveFilePicker" in text and "report.comments.json" in text
    assert "textContent" in text and 'id="comment-import"' in text
    assert "pointermove" in text and 'id="chart-limit"' in text
    assert "limit" in text and "Demand" in text and "t·m" in text
    assert not re.search(r"<b[ >]|<strong[ >]", text, re.I)
    assert not re.search(r"\b(could|would|" + "own" + "er" + r")\b", " ".join(parsed.visible), re.I)
    assert not re.search(r"/(?:home|mnt)/|[\w.+-]+@[\w.-]+\.[a-z]{2,}", text, re.I)


def test_exceeded_limit_changes_summary(tmp_path, monkeypatch):
    module = generator()
    config = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    config["hull_girder_screening"]["allowables"]["bending_moment"]["hogging_t_m"][1] = 50000
    example = tmp_path / "exceeded.yml"
    example.write_text(yaml.safe_dump(config), encoding="utf-8", newline="\n")
    monkeypatch.setattr(module, "EXAMPLE", str(example))
    data = module.calculate(tmp_path / "calculation")
    assert data["result"]["screening_status"] == "fail"
    assert data["result"]["max_bending_utilization"] == pytest.approx(1.2499375)
    text = module.render(data)
    assert "Illustrative checks exceed the supplied example limits." in text
    assert "Illustrative checks satisfy" not in text
    assert "strength acceptance is NOT_EVALUATED" in text


def test_build_with_windows_locale_and_newline_defaults(tmp_path, monkeypatch):
    module = generator()
    original_open = Path.open

    def windows_open(path, mode="r", buffering=-1, encoding=None, errors=None, newline=None):
        if "b" not in mode:
            encoding = encoding or "cp1252"
            if newline is None and "w" in mode:
                newline = "\r\n"
        return original_open(path, mode, buffering, encoding, errors, newline)

    monkeypatch.setattr(Path, "open", windows_open)
    module.build(tmp_path / "report", tmp_path / "calculation")
    for name in ("report.html", "changes.json"):
        raw = (tmp_path / "report" / name).read_bytes()
        assert b"\r\n" not in raw
        assert raw.decode("utf-8").endswith("\n")


def test_peak_closure_requirements_and_revision_text(rebuilt):
    module, text, parsed = rebuilt
    visible = " ".join(parsed.visible)
    assert "by construction" in visible
    payload = json.loads(parsed.payload)
    stations = payload["result"]["stations"]
    assert min(stations, key=lambda row: row["shear_force_t"])["x_m"] == 25
    assert max(stations, key=lambda row: row["shear_force_t"])["x_m"] == 75
    assert "negative shear peak" in visible and "positive shear peak" in visible
    assert "Draft for client review" not in visible
    assert "Both sign-specific allowable curves shall be supplied" in visible
    assert "Station refinement and additional reporting frames are required" in visible
    counts = [display for attrs, display in parsed.numbers
              if attrs["data-number"] == "inputs.n_stations"
              or attrs["data-number"].endswith(".definitions")]
    assert counts and all("." not in value for value in counts)
    history = text.split('id="revision-history"')[1].split('</section>')[0]
    panel = text.split('id="changes-panel"')[1].split('</details>')[0]
    for revision in module.CHANGES["history"]:
        for change in revision["changes"]:
            assert change in history
    for change in module.CHANGES["history"][0]["changes"]:
        assert change in panel


def test_small_nonzero_values_are_preserved():
    module = generator()
    assert module.format_number(0) == "0.0"
    assert module.format_number(-0.00001) == "−1.0e−05"
    assert module.format_number(1.25) == "1.2"


@pytest.mark.parametrize("placement", ["before", "after"])
def test_module_ignore_exceptions_coexist(tmp_path, placement):
    """A sibling report remains visible regardless of exception-block order."""
    rules = (ROOT / ".gitignore").read_text(encoding="utf-8")
    sibling = (
        "!docs/reports/modules/hydrodynamics/\n"
        "docs/reports/modules/hydrodynamics/*\n"
        "!docs/reports/modules/hydrodynamics/index.html\n"
    )
    if placement == "before":
        anchor = "!docs/reports/2026-09-13-ansys-evidence-stage-a.html\n"
        assert anchor in rules
        rules = rules.replace(anchor, sibling + anchor, 1)
    else:
        rules += "\n" + sibling
    (tmp_path / ".gitignore").write_text(rules, encoding="utf-8")
    subprocess.run(["git", "init", "--quiet", str(tmp_path)], check=True)
    paths = [
        "docs/reports/modules/hydrodynamics/index.html",
        *(f"docs/reports/modules/hullgirderscreening/{name}"
          for name in ("build_report.py", "report.html", "changes.json")),
        "docs/reports/modules/hydrodynamics/generated.csv",
        "docs/reports/modules/hullgirderscreening/generated.csv",
        "docs/reports/modules/other/generated.csv",
    ]
    result = subprocess.run(
        ["git", "-c", "core.excludesFile=", "check-ignore", "--no-index", *paths],
        cwd=tmp_path, check=False, capture_output=True, text=True,
    )
    assert result.returncode == 0
    assert set(result.stdout.splitlines()) == set(paths[-3:])


def test_interactive_chart_and_comment_round_trip(rebuilt, tmp_path):
    """Exercise the embedded interaction code without starting a browser."""
    import shutil
    import subprocess

    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable for the interaction check")
    _, text, parsed = rebuilt
    payload = json.loads(parsed.payload)
    script = re.findall(r"<script>(.*?)</script>", text, re.S)[0]
    fixture = r"""
const assert = require('node:assert/strict');
class Element {
  constructor() { this.value = ''; this.textContent = ''; this.children = []; this.events = {}; this.attrs = {}; }
  setAttribute(k, v) { this.attrs[k] = v; }
  append(node) { this.children.push(node); }
  replaceChildren(...nodes) { this.children = nodes; }
  addEventListener(name, callback) { this.events[name] = callback; }
  click() { this.clicked = true; }
  querySelector() { return {textContent: 'Section'}; }
  createSVGPoint() { return {matrixTransform() { return {x: 70}; }}; }
  getScreenCTM() { return {inverse() { return {}; }}; }
}
const elements = Object.fromEntries(IDS.map(id => [id, new Element()]));
elements['calculation-data'].textContent = JSON.stringify(DATA);
elements['engineering-changes'].textContent = JSON.stringify({revision: '0'});
elements['chart-quantity'].value = 'bending';
elements['chart-position'].value = Math.floor(DATA.result.stations.length/2);
const sections = Object.entries(elements).filter(([id]) => id.startsWith('section-')).map(([id, el]) => { el.id = id; return el; });
global.document = {getElementById: id => elements[id], querySelectorAll: () => sections,
  createElement: () => new Element(), createElementNS: () => new Element()};
let savedText;
global.window = {showSaveFilePicker: async () => ({createWritable: async () => ({
  write: async text => { savedText = text; }, close: async () => {}}),
  getFile: async () => ({text: async () => savedText})})};
"""
    checks = r"""
(async () => {
  const readout = elements['chart-readout'];
  assert.match(readout.textContent, /Case: central cargo/);
  assert.match(readout.textContent, /Bending moment/);
  assert.match(readout.textContent, /Demand: .*t·m/);
  assert.match(readout.textContent, /Illustrative limit magnitude/);
  assert.ok(elements['chart-limit'].attrs.d.startsWith('M'));
  assert.equal(elements['chart-markers'].children.length, DATA.result.utilization.length);
  elements['chart-quantity'].value = 'shear'; elements['chart-quantity'].events.change();
  assert.match(readout.textContent, /Shear force/);
  elements['demand-chart'].events.pointermove({clientX: 0, clientY: 0});
  assert.equal(elements['chart-position'].value, 0);
  assert.match(readout.textContent, /Position: 0.0 m/);
  elements['comment-section'].value = 'section-6';
  elements['comment-quote'].value = '<tag> & quoted text';
  elements['comment-text'].value = 'Review the illustrative limit.';
  elements['comment-add'].onclick();
  await elements['comment-save'].onclick();
  const saved = JSON.parse(savedText);
  assert.equal(saved.comments.length, 1);
  assert.equal(saved.comments[0].quote, '<tag> & quoted text');
  assert.equal(saved.comments[0].section, 'section-6');
  assert.equal(saved.binding.length, 64);
  await elements['comment-import'].onchange({target:{files:[{text:async () => savedText}]}});
  assert.match(elements['comment-message'].textContent, /Comments imported/);
  saved.binding = 'different report';
  await elements['comment-import'].onchange({target:{files:[{text:async () => JSON.stringify(saved)}]}});
  assert.match(elements['comment-message'].textContent, /does not match/);
  assert.equal(elements['comment-list'].children.length, 1);
})().catch(error => { console.error(error); process.exitCode = 1; });
"""
    check = tmp_path / "interaction.cjs"
    check.write_text("const DATA = " + json.dumps(payload) + ";\nconst IDS = "
                     + json.dumps(parsed.ids) + ";\n" + fixture + script + checks,
                     encoding="utf-8", newline="\n")
    result = subprocess.run([node, str(check)], capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
