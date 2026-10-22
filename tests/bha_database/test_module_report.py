# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild and numerical checks for the bottom hole assembly module report."""

from html.parser import HTMLParser
import json
from pathlib import Path
import re
import runpy
import shutil
import subprocess
import sys

import pytest

from digitalmodel.bha_database import load_database
from digitalmodel.bha_database.iso_table14 import audit_table14

ROOT = Path(__file__).resolve().parents[2]
BUILDER = ROOT / "scripts/bha_database/build_module_report.py"
REPORT = ROOT / "docs/reports/modules/bhadatabase/report.html"
REBUILD = "PYTHONPATH=src python scripts/bha_database/build_module_report.py"


@pytest.fixture(scope="module")
def builder():
    return runpy.run_path(str(BUILDER))


class ReportParser(HTMLParser):
    """Read numerical annotations and headings from the delivered page."""

    def __init__(self):
        super().__init__()
        self.numbers = []
        self.current = None
        self.ids = []
        self.links = []
        self.sections = []
        self.external = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "section":
            self.sections.append(attrs["id"])
        if tag == "a" and attrs.get("href", "").startswith("#"):
            self.links.append(attrs["href"][1:])
        if tag in ("script", "link") and ("src" in attrs or "href" in attrs):
            self.external.append(attrs)
        if "data-number" in attrs:
            assert self.current is None
            self.current = [tag, attrs["data-number"], ""]

    def handle_data(self, data):
        if self.current:
            self.current[2] += data

    def handle_endtag(self, tag):
        if self.current and self.current[0] == tag:
            self.numbers.append(tuple(self.current[1:]))
            self.current = None


def test_hand_calculated_length_anchor(builder):
    # Repository building example: (1 + 3 + 10 + 30 + 30) ft = 74 ft.
    # NIST SP 811 (2008), Appendix B.9, Length: 1 ft = 0.3048 m.
    # Decimal multiplication gives 22.5552 m without calling the library.
    values = builder["collect"]()["numbers"]
    assert values["total_length_m"]["value"] == pytest.approx(22.5552, abs=1e-10)


def test_hand_calculated_diameter_anchor(builder):
    # The assumed bit/hole diameter is 12 in. NIST SP 811 (2008), B.9,
    # Length: 1 in = 0.0254 m; 12 * 0.0254 = 0.3048 m.
    values = builder["collect"]()["numbers"]
    assert values["max_od_m"]["value"] == pytest.approx(0.3048, abs=1e-12)


def test_hand_calculated_blade_anchor(builder):
    # Bit length 1 ft + blade offset 0 ft + half of blade length 2 ft
    # gives 2 ft from the bit; 2 * 0.3048 = 0.6096 m (NIST B.9).
    values = builder["collect"]()["numbers"]
    assert values["blade_centre_m"]["value"] == pytest.approx(0.6096, abs=1e-12)


def test_hand_calculated_weight_anchors(builder):
    # Air: 74 ft * 80 lbf/ft = 5920 lbf; NIST B.9 gives 4.448222 N/lbf.
    # Rounded result: 26333.5 N. Archimedes (OpenStax University Physics
    # volume 1, 2016, section 14.4): subtract displaced-fluid weight.
    # Buoyed: 26333.5 * (1 - 10/65.5) = 22313.1 N at display precision.
    # Advisory: below the heavy-weight pipe, (1+3+10+30)*80 = 3520 lbf;
    # 3520 * 4.448222 * (1 - 10/65.5) * (1 - 0.15) = 11277.2 N.
    values = builder["collect"]()["numbers"]
    assert values["air_weight_N"]["value"] == pytest.approx(26333.5, abs=0.05)
    assert values["buoyed_weight_N"]["value"] == pytest.approx(22313.1, abs=0.05)
    assert values["available_wob_N"]["value"] == pytest.approx(11277.2, abs=0.05)


def test_every_displayed_number_matches_module(builder):
    record = builder["collect"]()
    parser = ReportParser()
    parser.feed(builder["render"](record))
    assert set(key for key, _ in parser.numbers) == set(record["numbers"])
    for key, text in parser.numbers:
        number = record["numbers"][key]
        assert text == f'{number["value"]:.{number["digits"]}f}', key

    db = load_database()
    result = db.summary("SYNTHETIC-BUILDING")
    for key in ("total_length_m", "max_od_m", "air_weight_N", "buoyed_weight_N"):
        assert record["numbers"][key]["value"] == pytest.approx(result[key]["value"])
    assert record["numbers"]["available_wob_N"]["value"] == pytest.approx(
        result["r7"]["available_wob_N"]["value"])
    assembly = db.assembly("SYNTHETIC-BUILDING")
    for key, field, unit in (
        ("hole_m", "hole_size", "m"), ("planned_wob_N", "planned_wob", "N"),
        ("mud_density", "mud_weight_basis", "lb/gallon"),
        ("material_density", "material_density", "lb/gallon"),
        ("margin", "wob_margin", "percent"),
    ):
        assert record["numbers"][key]["value"] == pytest.approx(getattr(assembly, field).to(unit))
    sections = db.sections("SYNTHETIC-BUILDING")
    for index, (item, section) in enumerate(zip(assembly.items, sections)):
        component = db.components[item.component_id]
        for name in ("length", "od", "id", "distance_from_bit"):
            assert record["numbers"][f"section_{index}_{name}"]["value"] == pytest.approx(
                section[name]["value"])
        assert record["numbers"][f"section_{index}_count"]["value"] == item.count
        assert record["numbers"][f"section_{index}_length_ft"]["value"] == pytest.approx(
            (item.length_override or component.length).to("ft"))
        assert record["numbers"][f"section_{index}_weight"]["value"] == pytest.approx(
            component.weight_per_length.to("lbf/ft"))
        assert record["numbers"][f"section_{index}_envelope"]["value"] == pytest.approx(
            max(q.to("m") for q in (component.od, component.od_max,
                                    component.blade_od or component.od)))
    assert record["numbers"]["blade_centre_m"]["value"] == pytest.approx(
        sections[1]["blade_centre_from_bit"]["value"])
    stabilizer = db.components[assembly.items[1].component_id]
    for key, field in (("blade_length_m", "blade_length"),
                       ("blade_offset_m", "blade_bottom_to_component_bottom")):
        assert record["numbers"][key]["value"] == pytest.approx(getattr(stabilizer, field).to("m"))
    assert record["numbers"]["planned_wob_lbf"]["value"] == pytest.approx(assembly.planned_wob.to("lbf"))
    assert record["numbers"]["hole_in"]["value"] == pytest.approx(assembly.hole_size.to("inch"))
    assert record["state"] == result["r7"]["state"]
    assert record["audit"] == audit_table14()["counts"]


def test_chart_demands_and_limit_match_the_module(builder):
    record = builder["collect"]()
    html = builder["render"](record)
    titles = re.findall(r'<title>Case: synthetic building\. Component: (.*?)\. Demand: ([\d.]+) m\. Limit: ([\d.]+) m\.', html)
    assert len(titles) == len(record["labels"])
    db = load_database()
    assembly = db.assembly("SYNTHETIC-BUILDING")
    for (label, demand, limit), item in zip(titles, assembly.items):
        component = db.components[item.component_id]
        expected = max(q.to("m") for q in (component.od, component.od_max,
                                          component.blade_od or component.od))
        assert label in record["labels"]
        assert demand == f"{expected:.4f}"
        assert limit == f'{assembly.hole_size.to("m"):.4f}'


def test_deliverables_are_not_ignored_and_comments_remain_local():
    for path in (REPORT, REPORT.parent / "changes.json"):
        result = subprocess.run(["git", "check-ignore", "-q", str(path)], cwd=ROOT, check=False)
        assert result.returncode == 1, f"Deliverable is ignored: {path.name}"
    result = subprocess.run(
        ["git", "check-ignore", "-q", str(REPORT.parent / "report.comments.json")],
        cwd=ROOT, check=False,
    )
    assert result.returncode == 0


def test_rebuild_matches_delivered_report(builder, tmp_path):
    output = tmp_path / "report.html"
    result = subprocess.run(
        [sys.executable, str(BUILDER), "--output", str(output)],
        cwd=ROOT, capture_output=True, text=True, encoding="utf-8", check=False,
    )
    assert result.returncode == 0, result.stderr
    assert output.read_bytes() == REPORT.read_bytes(), f"Rebuild with: {REBUILD}"
    assert b"\r" not in output.read_bytes()


def test_house_style_navigation_and_offline_assets(builder):
    html = builder["render"](builder["collect"]())
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(
        encoding="utf-8")
    assert re.search(r"<style>.*?</style>", exemplar, re.S).group() in html
    assert (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8") in html
    assert '<body class="hs-doc">' in html
    assert "Draft for maintainer review" in html
    parser = ReportParser()
    parser.feed(html)
    assert len(parser.sections) == 10
    assert len(parser.ids) == len(set(parser.ids))
    assert set(parser.links) <= set(parser.ids)
    assert not parser.external
    assert "<h4" not in html and "<b>" not in html and "<strong>" not in html


def test_changes_and_comment_sidecar(builder):
    html = builder["render"](builder["collect"]())
    changes = json.loads((REPORT.parent / "changes.json").read_text(encoding="utf-8"))
    embedded = json.loads(re.search(
        r'<script id="change-data" type="application/json">(.*?)</script>', html, re.S)[1])
    assert embedded == changes
    assert changes["revisions"][0]["revision"] == "1"
    assert "showSaveFilePicker" in html
    assert "report.comments.json" in html
    assert "textContent" in html and "JSON.parse" in html
    assert 'id="import-comments"' in html and 'id="save-comments"' in html
    assert 'id="review-section"' in html


def test_interactive_controls_execute_without_a_browser(builder):
    """Exercise chart focus, revision filtering and both comment save routes."""
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable; interactive controls require review")
    html = builder["render"](builder["collect"]())
    script = re.search(r"<script>(.*?)</script>", html, re.S)[1]
    changes = re.search(
        r'<script id="change-data" type="application/json">(.*?)</script>', html, re.S)[1]
    harness = r'''
const assert = require('node:assert/strict');
const vm = require('node:vm');
class Element {
  constructor() { this.children = []; this.events = {}; this.value = ''; this.textContent = ''; }
  append(child) { this.children.push(child); }
  replaceChildren() { this.children = []; }
  addEventListener(name, callback) { this.events[name] = callback; }
  click() { this.clicked = true; }
  remove() {}
}
const elements = new Map();
const element = id => {
  if (!elements.has(id)) elements.set(id, new Element());
  return elements.get(id);
};
const sections = ['executive', 'example'].map(id => {
  const section = element(id); section.id = id;
  section.querySelector = () => ({textContent:id}); return section;
});
const datum = new Element();
datum.querySelector = () => ({textContent:'Case: synthetic building. Demand: diameter. Limit: hole size.'});
element('change-data').textContent = CHANGES;
element('previous-revision').value = '0';
const document = {
  getElementById:element,
  querySelectorAll:selector => selector === '.datum' ? [datum] : sections,
  createElement:() => new Element(), body:new Element()
};
let savedBody;
const context = {
  document, window:{}, Blob, URL, setTimeout:callback => callback(),
};
vm.createContext(context);
vm.runInContext(SCRIPT, context);
(async () => {
  assert.equal(element('change-list').children.length, 1);
  element('previous-revision').value = '1';
  element('previous-revision').events.change();
  assert.match(element('change-list').children[0].textContent, /No engineering changes/);
  datum.events.focus();
  assert.match(element('diameter-detail').textContent, /Demand:.*Limit:/);
  element('review-section').value = 'example';
  element('review-comment').value = '<script>plain review text</script>';
  element('add-comment').events.click();
  assert.match(element('comment-list').children[0].textContent, /<script>plain review text/);
  context.window.showSaveFilePicker = async options => {
    assert.equal(options.suggestedName, 'report.comments.json');
    return {createWritable:async () => ({write:async body => {savedBody = body;}, close:async () => {}})};
  };
  await element('save-comments').events.click();
  const saved = JSON.parse(savedBody);
  assert.equal(saved.comments[0].section, 'example');
  assert.equal(saved.comments[0].text, '<script>plain review text</script>');
  const importFile = body => ({target:{files:[{text:async () => body}]}});
  await element('import-comments').events.change(importFile(savedBody));
  assert.equal(element('comment-list').children.length, 1);
  await element('import-comments').events.change(importFile(JSON.stringify({...saved, revision:'invalid'})));
  assert.match(element('comment-status').textContent, /not opened/);
  assert.equal(element('comment-list').children.length, 1);
  delete context.window.showSaveFilePicker;
  await element('save-comments').events.click();
  assert.match(element('comment-status').textContent, /download requested/);
  assert.equal(document.body.children[0].download, 'report.comments.json');
  context.window.showSaveFilePicker = async () => {const error = new Error(); error.name = 'AbortError'; throw error;};
  await element('save-comments').events.click();
  assert.match(element('comment-status').textContent, /cancelled/);
})().catch(error => { console.error(error); process.exitCode = 1; });
'''
    harness = "const SCRIPT = " + json.dumps(script) + ";\nconst CHANGES = " + json.dumps(changes) + ";\n" + harness
    result = subprocess.run(
        [node], input=harness, capture_output=True, text=True, encoding="utf-8", check=False,
    )
    assert result.returncode == 0, result.stderr


def test_report_has_no_licensed_values_or_unqualified_claims(builder):
    html = builder["render"](builder["collect"]())
    visible = re.sub(r"<script.*?</script>|<style.*?</style>", "", html, flags=re.S)
    visible = re.sub(r"<[^>]+>", " ", visible)
    assert "ASSUMED - to be confirmed" in visible
    assert "NOT_EVALUATED" in visible
    assert "not an equipment rating" in visible
    assert not re.search(r"\b(own[e]r|could|should|would|certified|compliant)\b", visible, re.I)
    assert "6 5/8" not in html  # The disputed standard cell is not republished.


def test_all_text_io_uses_explicit_encoding_and_newlines():
    # Guard the two newly maintained Python files against Windows defaults.
    import ast

    for path in (BUILDER, Path(__file__)):
        tree = ast.parse(path.read_text(encoding="utf-8"))
        for node in ast.walk(tree):
            if not isinstance(node, ast.Call) or not isinstance(node.func, ast.Attribute):
                continue
            if node.func.attr in ("read_text", "write_text"):
                keywords = {item.arg: item.value for item in node.keywords}
                assert ast.literal_eval(keywords["encoding"]) == "utf-8"
                if node.func.attr == "write_text":
                    assert ast.literal_eval(keywords["newline"]) == "\n"
