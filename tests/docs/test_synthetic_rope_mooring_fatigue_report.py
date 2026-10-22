# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Check report numbers against the public workflow and hand arithmetic."""

from __future__ import annotations

import copy
import csv
import importlib.util
import json
import re
import shutil
import subprocess
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

from digitalmodel.synthetic_rope_mooring_fatigue.workflow import router


ROOT = Path(__file__).resolve().parents[2]
REPORT_DIR = ROOT / "docs/reports/modules/syntheticropemooringfatigue"
REBUILD = (
    "PYTHONPATH=src uv run --no-project --with pyyaml python "
    "docs/reports/modules/syntheticropemooringfatigue/build_report.py "
    "--scratch <scratch>"
)


class Page(HTMLParser):
    """Collect numeric displays, links, headings and chart bindings."""

    def __init__(self, source):
        super().__init__()
        self.numbers = []
        self.ids = []
        self.links = []
        self.bindings = []
        self.headings = []
        self.visible = []
        self._number = None
        self._heading = None
        self._hidden = 0
        self.feed(source)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag in {"script", "style"}:
            self._hidden += 1
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "a" and attrs.get("href", "").startswith("#"):
            self.links.append(attrs["href"][1:])
        if "data-number" in attrs:
            self._number = [attrs["data-number"], "", tag]
        if "data-point" in attrs:
            self.bindings.append(attrs)
        if tag == "h2":
            self._heading = ""

    def handle_data(self, data):
        if not self._hidden:
            self.visible.append(data)
        if self._number is not None:
            self._number[1] += data
        if self._heading is not None:
            self._heading += data

    def handle_endtag(self, tag):
        if tag in {"script", "style"}:
            self._hidden -= 1
        if self._number is not None and tag == self._number[2]:
            self.numbers.append(tuple(self._number[:2]))
            self._number = None
        if tag == "h2" and self._heading is not None:
            self.headings.append(self._heading)
            self._heading = None


@pytest.fixture(scope="module")
def builder():
    path = REPORT_DIR / "build_report.py"
    spec = importlib.util.spec_from_file_location("rope_report", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def example(tmp_path_factory):
    scratch = tmp_path_factory.mktemp("rope-report")
    cfg = yaml.safe_load(
        (ROOT / "examples/workflows/synthetic-rope-mooring-fatigue/input.yml")
        .read_text(encoding="utf-8")
    )
    original = copy.deepcopy(cfg["synthetic_rope_mooring_fatigue"])
    cfg["synthetic_rope_mooring_fatigue"]["output_dir"] = str(scratch / "direct")
    summary = router(cfg)["synthetic_rope_mooring_fatigue"]
    with Path(summary["results_csv"]).open(encoding="utf-8", newline="\n") as stream:
        bins = list(csv.DictReader(stream))
    return scratch, original, summary, bins


def expected_numbers(inputs, summary, bins):
    """Assemble expectations from a fresh call, without builder helpers."""
    expected = {"design_life": inputs["design_life_years"]}
    for index, (source, result) in enumerate(zip(inputs["lines"], summary["lines"])):
        prefix = f"line.{index}."
        for field in ("MBL_kN", "mean_tension_kN", "min_tension_kN"):
            expected[prefix + field] = source[field]
        for field, value in result.items():
            if type(value) in (float, int):
                expected[prefix + field] = value
        expected[prefix + "fatigue_required_years"] = (
            inputs["design_life_years"] * inputs["dff"]
        )
        expected[prefix + "creep_required_years"] = (
            inputs["design_life_years"] * inputs["creep_safety_factor"]
        )
        expected[prefix + "min_required_kN"] = (
            source["MBL_kN"] * inputs["min_tension"]["min_ratio_allow"]
        )
        for j, row in enumerate(r for r in bins if r["line_id"] == result["line_id"]):
            for field in (
                "tension_range_kN", "n_cycles", "normalised_range",
                "allowable_cycles", "damage",
            ):
                expected[prefix + f"bin.{j}." + field] = float(row[field])
    return expected


def test_rebuild_and_every_worked_example_number(builder, example):
    scratch, inputs, summary, bins = example
    html, changes = builder.build(scratch / "build")
    page = Page(html)
    expected = expected_numbers(inputs, summary, bins)
    assert {key for key, _ in page.numbers} == set(expected)
    for key, displayed in page.numbers:
        # Match displayed precision; no exact comparison of library float tails.
        assert displayed == builder.display(expected[key]), key
        if key.endswith(("normalised_range", "min_tension_ratio")):
            # Ratios retain four significant figures so that visible range /
            # visible breaking load agrees at the stated display precision.
            assert float(displayed) == pytest.approx(expected[key], rel=5e-4), key
    saved = (REPORT_DIR / "report.html").read_text(encoding="utf-8")
    assert html == saved, f"Report differs; rebuild with: {REBUILD}"
    assert changes == json.loads((REPORT_DIR / "changes.json").read_text(encoding="utf-8"))


def test_hand_anchors_are_reported(builder, example):
    scratch, _, summary, bins = example
    first, second = summary["lines"]
    # First bin range / breaking load = 900 / 18000 = 1 / 20.
    assert float(bins[0]["normalised_range"]) == pytest.approx(0.05, rel=1e-12)
    # Minimum tension / breaking load = 1500 / 18000 = 1 / 12.
    assert first["min_tension_ratio"] == pytest.approx(0.0833333333333333, rel=1e-12)
    # Required minimum = 18000 * 0.05 = 900; margin = 1500 / 900 = 5 / 3.
    assert first["compression_margin"] == pytest.approx(1.66666666666667, rel=1e-12)
    # Mean ratio = 9900 / 18000 = 0.55; prescribed creep exponent is
    # (0.30 - 0.55) * 6 = -1.5; 100 * 10**(-1.5) = sqrt(10) years.
    assert second["creep_life_years"] == pytest.approx(3.162277660168379, rel=1e-12)
    # Required creep life = 25 * 3 = 75 years.
    html, _ = builder.build(scratch / "anchors")
    numbers = dict(Page(html).numbers)
    assert numbers["line.0.bin.0.normalised_range"] == builder.display(0.05)
    assert numbers["line.0.min_tension_ratio"] == builder.display(0.0833333333333333)
    assert numbers["line.0.compression_margin"] == builder.display(1.66666666666667)
    assert numbers["line.1.creep_life_years"] == builder.display(3.162277660168379)
    assert numbers["line.1.creep_required_years"] == builder.display(75.0)


def test_structure_sources_and_offline_assets(builder, example):
    html, _ = builder.build(example[0] / "structure")
    page = Page(html)
    assert len(page.headings) == 10
    assert len(page.ids) == len(set(page.ids))
    assert set(page.links) <= set(page.ids)
    assert '<body class="hs-doc">' in html
    assert '<script src=' not in html and '<link rel="stylesheet"' not in html
    assert "Draft for maintainer review" in html
    assert "Revision history" in html and html.index("Revision history") < html.index('id="introduction"')
    for fragment in ("Miner", "2001", "NOT_EVALUATED", "ASSUMED - to be confirmed"):
        assert fragment in html
    for fragment in ("showDirectoryPicker", "showSaveFilePicker", "textContent", "changes-panel"):
        assert fragment in html
    assert 'id="mooring-schematic"' in html
    assert html.count('data-limit="1"') == 3
    assert len(page.bindings) == 6
    keys = {key for key, _ in page.numbers}
    for point in page.bindings:
        assert point["data-point"] in keys
        assert point["data-demand"] in keys
        assert point["data-required"] in keys
        assert point["data-component"] and point["data-units"]


def test_visible_register_and_public_wording(builder, example):
    html, changes = builder.build(example[0] / "register")
    visible = " ".join(Page(html).visible)
    assert not re.search(r"\b(could|should|would|ow" + r"ner|validated|certified|compliant)\b", visible, re.I)
    assert "<b>" not in html and "<strong>" not in html
    assert not re.search(r"(?:/ho" + r"me/|/m" + r"nt/|[\w.+-]+@[\w.-]+\.[A-Za-z]{2,})", html)
    assert changes["revisions"][0]["revision"] == "Initial"


def test_utf8_lf_and_no_implicit_file_encoding():
    """Keep text I/O portable when the interpreter has no UTF-8 override."""
    import ast

    for path in (REPORT_DIR / "build_report.py", Path(__file__)):
        tree = ast.parse(path.read_text(encoding="utf-8"))
        for call in (node for node in ast.walk(tree) if isinstance(node, ast.Call)):
            if isinstance(call.func, ast.Attribute) and call.func.attr in {"read_text", "write_text", "open"}:
                keywords = {kw.arg: kw.value for kw in call.keywords}
                assert "encoding" in keywords, (path.name, call.lineno)
                if call.func.attr == "write_text":
                    assert "newline" in keywords, (path.name, call.lineno)
    for path in REPORT_DIR.iterdir():
        if path.is_file():
            assert b"\r\n" not in path.read_bytes(), path.name
            assert all(line == line.rstrip() for line in path.read_text(encoding="utf-8").splitlines()), path.name


def test_report_files_are_not_ignored():
    paths = [REPORT_DIR / name for name in ("build_report.py", "report.html", "changes.json")]
    result = subprocess.run(
        ["git", "check-ignore", *(str(path.relative_to(ROOT)) for path in paths)],
        cwd=ROOT, capture_output=True, text=True, check=False,
    )
    assert result.returncode == 1 and not result.stdout, "Report files must be stageable without forcing."


def test_chart_and_comment_interactions_without_browser(builder, example):
    """Exercise the shipped JavaScript against file handles and a minimal DOM."""
    if not shutil.which("node"):
        pytest.skip("Node is unavailable for the JavaScript interaction check")
    html, _ = builder.build(example[0] / "interactions")
    payload = json.dumps({
        "script": builder.INTERACTION,
        "values": re.search(r'<script id="numeric-record"[^>]*>(.*?)</script>', html, re.S).group(1),
        "changes": json.dumps(builder.CHANGES),
        "point": Page(html).bindings[-1],
    })
    harness = r"""
const assert = require('node:assert/strict');
const vm = require('node:vm');
const fs = require('node:fs');
const payload = JSON.parse(fs.readFileSync(0, 'utf8'));
class Element {
  constructor() { this.textContent = ''; this.value = ''; this.children = []; this.events = {}; this.files = []; }
  addEventListener(name, callback) { this.events[name] = callback; }
  appendChild(child) { this.children.push(child); }
  replaceChildren() { this.children = []; }
  async emit(name, extra = {}) { return this.events[name]({preventDefault() {}, target: this, ...extra}); }
}
const elements = new Map();
const get = id => { if (!elements.has(id)) elements.set(id, new Element()); return elements.get(id); };
get('numeric-record').textContent = payload.values;
get('change-record').textContent = payload.changes;
const point = new Element();
point.dataset = Object.fromEntries(Object.entries(payload.point).filter(([k]) => k.startsWith('data-')).map(([k, v]) => [k.slice(5), v]));
let saved = '';
let existing = JSON.stringify({document: 'synthetic-rope-mooring-fatigue', comments: [{section: 'Results', text: 'Existing note'}]});
const handle = {
  async getFile() { return {async text() { return existing; }}; },
  async createWritable() { return {async write(text) { saved = text; }, async close() {}}; }
};
const directory = {async getFileHandle(name) { assert.ok(['report.html', 'report.comments.json'].includes(name)); return handle; }};
const window = {async showDirectoryPicker() { return directory; }, confirm() { return true; }};
const context = {document: {getElementById: get, querySelectorAll() { return [point]; }, createElement() { return new Element(); }}, window, console, JSON, Blob, URL, setTimeout};
vm.runInNewContext(payload.script, context);
(async () => {
  await point.emit('focus');
  assert.match(get('chart-readout').textContent, /Case: Second polyester line; component: Synthetic rope span;/);
  assert.match(get('chart-readout').textContent, /limit .*kN; margin:/);
  assert.equal(get('changes-panel').children.length, 1);
  await get('comment-folder').emit('click');
  assert.equal(get('comment-list').children[0].textContent, 'Results: Existing note');
  get('comment-section').value = 'Results';
  get('comment-text').value = '<img src=x onerror=alert(1)> Arithmetic note';
  await get('comment-form').emit('submit');
  assert.equal(get('comment-list').children.length, 2);
  assert.match(get('comment-list').children[1].textContent, /<img/);
  await get('comment-save').emit('click');
  assert.equal(JSON.parse(saved).comments.length, 2);
  existing = saved;
  get('comment-import').files = [{async text() { return existing; }}];
  await get('comment-import').emit('change');
  assert.equal(get('comment-list').children.length, 2);
  get('comment-import').files = [{async text() { return '{}'; }}];
  await get('comment-import').emit('change');
  assert.match(get('comment-status').textContent, /not imported/);
  assert.equal(get('comment-list').children.length, 2);
  delete window.showDirectoryPicker;
  await get('comment-folder').emit('click');
  assert.match(get('comment-status').textContent, /Folder access is unavailable/);
  console.log('Chart focus, change panel, sidecar save/load, import validation and text rendering passed.');
})().catch(error => { console.error(error); process.exitCode = 1; });
"""
    result = subprocess.run(["node", "-"], input=harness.replace(
        "const payload = JSON.parse(fs.readFileSync(0, 'utf8'));",
        "const payload = " + payload + ";",
    ), text=True, capture_output=True, check=False)
    assert result.returncode == 0, result.stderr
