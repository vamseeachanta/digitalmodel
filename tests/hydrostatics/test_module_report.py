# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check the built report against independent public data and module calls."""

import importlib.util
import json
import re
import shutil
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path

import pytest

from digitalmodel import hydrostatics as hs

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/hydrostatics"
SCRIPT = REPORT / "build_report.py"


class Document(HTMLParser):
    """Collect numeric bindings, links and visible client text."""

    def __init__(self, html):
        super().__init__()
        self.values = []
        self.ids = []
        self.links = []
        self.headings = []
        self.visible = []
        self.binding = None
        self.hidden = 0
        self.external = []
        self.feed(html)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag in ("style", "script"):
            self.hidden += 1
        if tag in ("script", "link", "img") and (attrs.get("src") or attrs.get("href")):
            self.external.append(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "a":
            self.links.append(attrs["href"])
        if tag in ("h2", "h3", "h4"):
            self.headings.append(tag)
        if "data-value" in attrs:
            self.binding = attrs

    def handle_endtag(self, tag):
        if tag in ("style", "script"):
            self.hidden -= 1
        if tag == "span":
            self.binding = None

    def handle_data(self, data):
        if self.binding:
            self.values.append((self.binding, data))
        if not self.hidden:
            self.visible.append(data)


@pytest.fixture(scope="module")
def builder():
    spec = importlib.util.spec_from_file_location("hydrostatics_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def independent_values():
    """Call the public API independently of the report's calculation adapter."""
    basis = json.loads((REPORT / "basis.json").read_text())
    inputs = basis["inputs"]
    rho = inputs["density_kg_m3"]
    gravity = inputs["gravity_m_s2"]
    depth = inputs["evaluation_depth_m"]
    result = dict(inputs)
    result["pressure_pa"] = hs.hydrostatic_pressure(rho, depth, gravity)
    result["bottom_pa"] = hs.seabed_pressure(
        rho, inputs["water_depth_m"], inputs["surface_gauge_pa"], gravity
    )
    result["seabed_at_evaluation_pa"] = hs.seabed_pressure(
        rho, depth, inputs["surface_gauge_pa"], gravity
    )
    result["pressure_psi"] = hs.pa_to_psi(result["pressure_pa"])
    result["pressure_bar"] = hs.psi_to_bar(result["pressure_psi"])
    result["default_pressure_pa"] = hs.hydrostatic_pressure(rho, depth)
    result["reference_pa"] = basis["reference"]["average_gauge_pressure_pa"]
    result["reference_difference_pct"] = (
        (result["pressure_pa"] - result["reference_pa"]) / result["reference_pa"] * 100
    )
    result["default_difference_pct"] = (
        (result["default_pressure_pa"] - result["reference_pa"])
        / result["reference_pa"] * 100
    )
    return result


def test_public_example_matches_published_pressure(builder):
    values = independent_values()
    assert builder.calculate()["values"] == values
    assert values["pressure_pa"] == values["reference_pa"]
    assert values["seabed_at_evaluation_pa"] == values["pressure_pa"]
    assert values["default_pressure_pa"] != values["reference_pa"]


def test_every_rendered_example_number_and_chart_point(builder):
    html = builder.render()
    doc = Document(html)
    values = independent_values()
    seen = set()
    for attrs, displayed in doc.values:
        key = attrs["data-value"]
        seen.add(key)
        assert float(attrs["data-raw"]) == values[key]
        expected = format(values[key] / float(attrs["data-scale"]), attrs["data-format"])
        assert displayed == expected.replace("-", "−")
    assert seen == set(values)
    payload = json.loads(re.search(
        r'<script id="calculation-data" type="application/json">(.*?)</script>', html, re.S
    ).group(1))
    assert payload["values"] == values
    for point in payload["profile"]:
        assert point["pressure_pa"] == hs.hydrostatic_pressure(
            values["density_kg_m3"], point["depth_m"], values["gravity_m_s2"]
        )
    assert payload["profile"][0]["pressure_pa"] == values["surface_gauge_pa"]
    assert payload["profile"][-1]["pressure_pa"] == values["bottom_pa"]
    # No unsupported absolute pressure or fabricated allowable is emitted.
    assert payload["allowable_pressure_pa"] is None
    assert payload["absolute_pressure_pa"] is None


def test_rebuild_is_identical_and_does_not_touch_comments(tmp_path):
    comments = tmp_path / "report.comments.json"
    comments.write_text('{"retained": true}\n')
    subprocess.run([sys.executable, str(SCRIPT), "--output-dir", str(tmp_path)], check=True)
    for name in ("report.html", "changes.json"):
        assert (tmp_path / name).read_bytes() == (REPORT / name).read_bytes()
    assert comments.read_text() == '{"retained": true}\n'


def test_house_style_and_report_integrity(builder):
    html = builder.render()
    doc = Document(html)
    assert '<body class="hs-doc">' in html
    assert not doc.external
    assert doc.headings.count("h2") == 10
    assert "h4" not in doc.headings
    assert len(doc.ids) == len(set(doc.ids))
    for link in doc.links:
        if link.startswith("#"):
            assert link[1:] in doc.ids
        elif not link.startswith("https:"):
            assert (REPORT / link).resolve().is_file()
    brand = (ROOT / "docs/api/_assets/brand.css").read_text()
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text()
    assert brand in html
    assert re.search(r"<style>.*?</style>", exemplar, re.S).group(0) in html
    visible = " ".join(doc.visible)
    assert not re.search(r"\b(could|should|would|own[e]r|validated|certified|compliant)\b", visible, re.I)
    assert "<strong>" not in html and "<b>" not in html
    for term in ("Draft for client review", "Revision history", "NOT_EVALUATED",
                 "Recommendations", "Not covered", "9.2 Sources and reproduction"):
        assert term in visible


def test_inventory_matches_current_module_and_tests(builder):
    inventory = builder.inventory()
    assert inventory["existing_tests"] == 10
    assert inventory["public_functions"] == 8
    assert inventory["report_tests"] == 6
    assert inventory == builder.calculate()["inventory"]


def test_offline_chart_and_comment_save_load(builder):
    """Exercise the actual inline script without starting a browser."""
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable for the offline interaction check")
    html = builder.render()
    identity = json.loads(re.search(
        r'<script id="review-identity" type="application/json">(.*?)</script>', html
    ).group(1))
    harness = r"""
const assert = require('node:assert/strict');
const vm = require('node:vm');
const fs = require('node:fs');
const input = JSON.parse(fs.readFileSync(0, 'utf8'));
class Element {
  constructor() { this.textContent = ''; this.value = ''; this.children = []; this.events = {}; this.attrs = {}; }
  setAttribute(key, value) { this.attrs[key] = value; }
  appendChild(child) { this.children.push(child); }
  replaceChildren() { this.children = []; }
  addEventListener(key, fn) { this.events[key] = fn; }
  click() { this.clicked = true; }
}
const elements = new Map();
const byId = id => { if (!elements.has(id)) elements.set(id, new Element()); return elements.get(id); };
byId('calculation-data').textContent = JSON.stringify(input.data);
byId('review-identity').textContent = JSON.stringify(input.identity);
byId('depth-sample').value = '8';
const sections = ['executive', 'introduction', 'controls', 'basis', 'theory', 'example',
  'validation', 'recommendations', 'appendix', 'not-covered'].map(id =>
    ({id, querySelector: () => ({textContent:id})}));
let saved = '', closed = false, download = null;
const window = { showSaveFilePicker: async options => {
  assert.equal(options.suggestedName, 'report.comments.json');
  return {createWritable: async () => ({write: async text => { saved = text; },
    close: async () => { closed = true; }}), getFile: async () => ({text: async () => saved})};
}};
const context = {window, document:{getElementById:byId, querySelectorAll:() => sections,
  createElement:() => new Element(), createElementNS:() => new Element()},
  Blob:class { constructor(parts) { download = parts.join(''); } },
  URL:{createObjectURL:() => 'blob:comments', revokeObjectURL:() => {}}, setTimeout:fn => fn()};
vm.runInNewContext(input.script, context);
assert.equal(byId('pressure-points').children.length, input.data.profile.length);
assert.equal(byId('chart-readout').textContent, input.data.profile[8].description);
byId('depth-sample').events.input({target:{value:'16'}});
assert.equal(byId('chart-readout').textContent, input.data.profile[16].description);
byId('pressure-points').children[0].events.focus();
assert.equal(byId('chart-readout').textContent, input.data.profile[0].description);
byId('comment-section').value = 'example';
byId('comment-text').value = '<img src=x onerror=bad()> pressure datum';
byId('add-comment').events.click();
assert.equal(byId('comment-list').children.length, 1);
assert.equal(byId('comment-list').children[0].textContent,
  'example: <img src=x onerror=bad()> pressure datum');
(async () => {
  await byId('save-comments').events.click();
  assert.ok(closed);
  const parsed = JSON.parse(saved);
  assert.equal(parsed.comments[0].section, 'example');
  assert.equal(parsed.identity, input.identity);
  assert.equal(byId('comment-message').textContent, 'Comments saved and read back.');
  const load = async data => byId('load-comments').events.change({target:{value:'',
    files:[{text:async () => JSON.stringify(data)}]}});
  await load({...parsed, identity:'different'});
  assert.match(byId('comment-message').textContent, /do not match/);
  assert.equal(byId('comment-list').children.length, 1);
  await load({...parsed, comments:[{section:'basis', text:'Check density.'}]});
  assert.equal(byId('comment-list').children.length, 2);
  await load(parsed);
  assert.equal(byId('comment-list').children.length, 2);
  window.showSaveFilePicker = undefined;
  await byId('save-comments').events.click();
  assert.equal(JSON.parse(download).comments.length, 2);
  assert.match(byId('comment-message').textContent, /Download offered/);
})().catch(error => { console.error(error); process.exitCode = 1; });
"""
    subprocess.run([node, "-e", harness], input=json.dumps({
        "script": builder.INTERACTION, "data": builder.calculate(), "identity": identity,
    }), text=True, check=True, capture_output=True)
