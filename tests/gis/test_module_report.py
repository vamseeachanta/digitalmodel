# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check the GIS report's numerical consistency, anchors and rebuild contract."""
from __future__ import annotations

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

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/gis"
REBUILD = "PYTHONPATH=src python docs/reports/modules/gis/build_report.py"
spec = importlib.util.spec_from_file_location("gis_module_report", REPORT / "build_report.py")
builder = importlib.util.module_from_spec(spec)
spec.loader.exec_module(builder)


class Page(HTMLParser):
    """Extract bindings, locators and visible strings from the built artifact."""

    def __init__(self, text):
        super().__init__()
        self.ids = []
        self.links = []
        self.resources = []
        self.numbers = []
        self.visible = []
        self.hidden = None
        self.binding = None
        self.body_class = ""
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag in {"style", "script"}:
            self.hidden = tag
        if tag == "body":
            self.body_class = attrs.get("class", "")
        if tag == "a":
            self.links.append(attrs.get("href", ""))
        if tag in {"link", "script", "img", "iframe"}:
            self.resources.extend(attrs[k] for k in ("src", "href") if k in attrs)
        if "data-value" in attrs:
            self.binding = attrs

    def handle_endtag(self, tag):
        if tag == self.hidden:
            self.hidden = None
        if tag == "span":
            self.binding = None

    def handle_data(self, data):
        if not self.hidden:
            self.visible.append(data)
        if self.binding is not None:
            self.numbers.append((self.binding.copy(), data))


@pytest.fixture(scope="module")
def results():
    return builder.calculate()


def test_hand_anchor_published_easting(results):
    # USGS 1987 eq. (7-1a): x = a*pi*longitude/180.
    # a=6378137 m, longitude=2 degrees: 6378137*3.141592653589793/90
    # =222638.981586547 m. Absolute 0.000001 m tolerance allows CPU rounding.
    assert results["example"]["easting_m"] == pytest.approx(222638.981586547, abs=1e-6)


def test_hand_anchor_quarter_equator(results):
    # USGS 1987 eq. (7-1a): longitude=90 degrees gives a*pi/2.
    # 6378137*3.141592653589793/2 =10018754.171394622 m.
    # This analytic control has the same radius as the published example.
    assert results["anchors"]["quarter_easting_m"] == pytest.approx(10018754.171394622, abs=1e-6)


def test_hand_anchor_midlatitude(results):
    # USGS 1987 eq. (7-2): latitude=45 degrees gives tan(3*pi/8)=1+sqrt(2).
    # ln(1+sqrt(2))=0.881373587019543; 6378137*0.881373587019543
    # =5621521.486192066 m. Absolute tolerance is 0.000001 m.
    assert results["anchors"]["mid_northing_m"] == pytest.approx(5621521.486192066, abs=1e-6)


def test_public_example_reference(results):
    # PROJ 9.5 Web Mercator / Usage publishes coordinates to the nearest cm.
    # Half a displayed increment is 0.005 m, an absolute rounding tolerance.
    assert results["example"]["easting_m"] == pytest.approx(222638.98, abs=0.005)
    assert results["example"]["northing_m"] == pytest.approx(6274861.39, abs=0.005)


def test_headline_from_displayed_inputs(results):
    # First-principles formula, independent of the report builder's calculations.
    import math

    a = float(builder.display(results["basis"]["radius_m"], 0))
    lon = float(builder.display(results["example"]["longitude_deg"], 0))
    lat = float(builder.display(results["example"]["latitude_deg"], 0))
    assert round(a * math.radians(lon), 2) == round(results["example"]["easting_m"], 2)
    assert round(a * math.log(math.tan(math.pi / 4 + math.radians(lat) / 2)), 2) == round(results["example"]["northing_m"], 2)


def test_consistency_with_fresh_module_calls(results):
    from digitalmodel.gis import CoordinateTransformer

    forward = CoordinateTransformer("EPSG:4326", "EPSG:3857")
    reverse = CoordinateTransformer("EPSG:3857", "EPSG:4326")
    example = results["example"]
    x, y = forward.transform_point(example["longitude_deg"], example["latitude_deg"])
    assert example["easting_m"] == pytest.approx(x, abs=1e-6)
    assert example["northing_m"] == pytest.approx(y, abs=1e-6)
    lon, lat = reverse.transform_point(x, y)
    assert example["recovered_longitude_deg"] == pytest.approx(lon, abs=1e-10)
    assert example["recovered_latitude_deg"] == pytest.approx(lat, abs=1e-10)
    for case in results["curve"]:
        _, northing = forward.transform_point(example["longitude_deg"], case["latitude_deg"])
        assert case["northing_m"] == round(northing, 2)


def test_every_display_binding_matches_module_results(results):
    page = Page((REPORT / "report.html").read_text(encoding="utf-8"))
    assert len(page.numbers) >= 15
    for attrs, text in page.numbers:
        value = builder.lookup(results, attrs["data-value"])
        assert text == builder.display(value, int(attrs["data-digits"])), REBUILD


def test_committed_page_matches_fresh_build_at_displayed_precision(tmp_path):
    # Calculation records are rounded before embedding; the whole document then
    # compares at display precision, without platform-dependent float strings.
    environment = dict(os.environ, PYTHONPATH=str(ROOT / "src"))
    subprocess.run([sys.executable, str(REPORT / "build_report.py"), "--output-dir", str(tmp_path)], cwd=ROOT, env=environment, check=True)
    for name in ("report.html", "changes.json"):
        assert (REPORT / name).read_text(encoding="utf-8") == (tmp_path / name).read_text(encoding="utf-8"), f"Stale {name}; rebuild: {REBUILD}"


def test_embedded_record_consistency(results):
    text = (REPORT / "report.html").read_text(encoding="utf-8")
    match = re.search(r'<script type="application/json" id="calculation-record">(.*?)</script>', text, re.S)
    assert json.loads(match[1]) == builder.rounded(results), REBUILD


def test_house_style_and_self_containment():
    text = (REPORT / "report.html").read_text(encoding="utf-8")
    page = Page(text)
    assert "hs-doc" in page.body_class.split()
    assert not page.resources
    exemplar = (ROOT / builder.EXEMPLAR).read_text(encoding="utf-8")
    assert re.search(r"<style>(.*?)</style>", exemplar, re.S)[1] in text
    assert "<b>" not in text and "<strong>" not in text
    assert "Draft for maintainer review" in " ".join(page.visible)
    assert "\r" not in text


def test_ten_sections_and_resolved_locators():
    text = (REPORT / "report.html").read_text(encoding="utf-8")
    page = Page(text)
    assert len(page.ids) == len(set(page.ids))
    assert re.findall(r'<section id="section-(\d+)"', text) == [str(n) for n in range(1, 11)]
    assert "<h4" not in text
    for link in page.links:
        if link.startswith("#"):
            assert link[1:] in page.ids
        elif not link.startswith("https:"):
            assert (REPORT / link.split("#")[0]).is_file(), link
    assert text.index('id="revision-history"') < text.index('id="section-2"')


def test_all_worked_example_numerals_are_bound():
    text = (REPORT / "report.html").read_text(encoding="utf-8")
    worked = text.split('<section id="section-5">')[1].split("</section>")[0]
    # Section/table numbers and bibliographic markers are not inputs or results.
    worked = re.sub(r"<h2>.*?</h2>|<a\b[^>]*>.*?</a>|<span\b[^>]*>.*?</span>", "", worked, flags=re.S)
    visible = re.sub(r"Table \d+", "Table", " ".join(Page(worked).visible))
    assert not re.search(r"\d", visible)


def test_interactive_curve_and_polar_limit(results):
    text = (REPORT / "report.html").read_text(encoding="utf-8")
    assert 'id="polar-limit"' in text
    assert results["basis"]["polar_limit_deg"] == 90
    assert all(c["latitude_deg"] < results["basis"]["polar_limit_deg"] for c in results["curve"])
    assert 'tabindex="0"' in text
    assert 'addEventListener("focus"' in text
    assert "Component: projection" in text
    assert 'id="chart-readout"' in text


def test_comment_saving_and_changes_panel():
    text = (REPORT / "report.html").read_text(encoding="utf-8")
    changes = json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    assert changes["revision"] == builder.REVISION
    assert changes["changes"]
    assert 'id="engineering-changes"' in text
    assert "showDirectoryPicker" in text and "createWritable" in text
    assert "report.comments.json" in text and "textContent" in text
    assert "innerHTML" not in text


def test_comment_json_roundtrip_and_chart_events():
    """Exercise persistence logic with file handles, without starting a browser."""
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is absent; comment persistence logic cannot be executed")
    text = (REPORT / "report.html").read_text(encoding="utf-8")
    javascript = re.findall(r"<script>(.*?)</script>", text, re.S)[-1]
    harness = r'''
const assert = require("node:assert/strict");
const vm = require("node:vm");
const source = JSON.parse(require("node:fs").readFileSync(0, "utf8"));
const elements = new Map();
function element(id) {
  if (!elements.has(id)) elements.set(id, {
    listeners: {}, value: "", textContent: "", children: [], files: [],
    addEventListener(type, fn) { this.listeners[type] = fn; },
    appendChild(child) { this.children.push(child); },
    replaceChildren() { this.children = []; }, click() {}
  });
  return elements.get(id);
}
const point = element("point");
point.dataset = {detail: "Case: test; Component: projection; Northing: 1 m; Limit: pole"};
let saved = null, exported = null;
const directory = { async getFileHandle(name) {
  if (name === "report.html") return {async getFile() {return {async text() {
    return 'id="gis-coordinate-transformation" data-revision="00"';
  }};}};
  if (saved === null && arguments.length < 2) {
    const error = new Error("Missing comments"); error.name = "NotFoundError"; throw error;
  }
  return {
    async getFile() {return {async text() {return saved;}};},
    async createWritable() {return {async write(text) {saved = text;}, async close() {}};}
  };
}};
const document = {
  querySelectorAll() {return [point];},
  getElementById(id) {return id === "invalid-section" ? null : element(id);},
  createElement() {return element(Symbol());}
};
const context = {document, window: {async showDirectoryPicker() {return directory;}},
  Blob, URL: {createObjectURL(blob) {exported = blob; return "blob:comments";}, revokeObjectURL() {}},
  setTimeout(fn) {fn();}
};
vm.runInNewContext(source, context);
(async () => {
  await point.listeners.focus();
  assert.equal(element("chart-readout").textContent, point.dataset.detail);
  await element("select-folder").listeners.click();
  element("comment-section").value = "section-5";
  element("comment-text").value = "Check <coordinate> order.";
  await element("add-comment").listeners.click();
  await element("save-comments").listeners.click();
  const record = JSON.parse(saved);
  assert.equal(record.comments[0].text, "Check <coordinate> order.");
  assert.ok(saved.endsWith("\n"));
  await element("export-comments").listeners.click();
  assert.deepEqual(JSON.parse(await exported.text()), record);
  await element("select-folder").listeners.click();
  assert.equal(element("comment-list").children.length, 1);
  await element("import-comments").listeners.change({target: {files: [{async text() {
    return JSON.stringify({...record, report: "unrelated-report"});
  }}]}});
  assert.equal(element("comment-list").children.length, 1);
  assert.match(element("comment-status").textContent, /do not match/);
})().catch(error => { console.error(error.message); process.exitCode = 1; });
'''
    result = subprocess.run([node, "-e", harness], input=json.dumps(javascript), text=True, capture_output=True)
    assert result.returncode == 0, result.stderr


def test_public_wording_on_built_visible_text():
    text = (REPORT / "report.html").read_text(encoding="utf-8")
    visible = " ".join(Page(text).visible)
    blocked = ["ow" + "ner", "cli" + "ent", "jo" + "b", "ag" + "ent",
               "la" + "ne", "sess" + "ion", "fl" + "eet", "dispat" + "cher",
               "co" + "uld", "sho" + "uld", "wo" + "uld", "veri" + "fied",
               "certi" + "fied", "comp" + "liant"]
    assert not re.search(r"\b(?:" + "|".join(blocked) + r")\b", visible, re.I)


def test_deliverables_are_not_ignored():
    for name in ("build_report.py", "report.html", "changes.json", "sources.json"):
        path = str((REPORT / name).relative_to(ROOT))
        check = subprocess.run(["git", "check-ignore", "-q", "--", path], cwd=ROOT)
        assert check.returncode == 1, f"Required deliverable is ignored: {path}"


def test_public_source_access_records():
    sources = json.loads((REPORT / "sources.json").read_text(encoding="utf-8"))["sources"]
    page = Page((REPORT / "report.html").read_text(encoding="utf-8"))
    assert {source["url"] for source in sources} == {link for link in page.links if link.startswith("https:")}
    for source in sources:
        assert source["http_status"] == 200
        assert source["accessed_utc"].endswith("Z")
        assert source["locator"] and source["title"]
