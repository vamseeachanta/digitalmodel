# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the report and compare displayed quantities with the public API."""

import importlib.util
import json
import re
from html.parser import HTMLParser
from pathlib import Path

import pytest

from digitalmodel.corrosion import ANODIC_INDEX_V, ENVIRONMENTS, screen_couple
from digitalmodel.corrosion import galvanic_screening as screening

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/corrosion"
SPEC = importlib.util.spec_from_file_location(
    "corrosion_module_report", ROOT / "scripts/corrosion/build_module_report.py"
)


@pytest.fixture
def builder():
    """Load the report generator without modifying the import search path."""
    module = importlib.util.module_from_spec(SPEC)
    SPEC.loader.exec_module(module)
    return module


class Page(HTMLParser):
    """Collect bound numbers, embedded evidence, anchors and external resources."""

    def __init__(self, text):
        super().__init__()
        self.bindings = []
        self.ids = []
        self.links = []
        self.resources = []
        self.headings = []
        self.bold = []
        self.chart_notes = []
        self.binding = None
        self.payload = None
        self.in_payload = False
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "a":
            self.links.append(attrs.get("href", ""))
        if tag in {"script", "link", "img", "iframe"}:
            self.resources.extend(attrs[key] for key in ("src", "href") if key in attrs)
        if "data-chart-note" in attrs:
            self.chart_notes.append(attrs["data-chart-note"])
        if tag in {"b", "strong"}:
            self.bold.append(tag)
        if tag == "h2":
            self.headings.append(attrs.get("id", ""))
        if "data-number" in attrs:
            self.binding = attrs["data-number"]
        if tag == "script" and attrs.get("id") == "calculation-data":
            self.in_payload = True

    def handle_endtag(self, tag):
        if tag == "span":
            self.binding = None
        if tag == "script":
            self.in_payload = False

    def handle_data(self, data):
        if self.binding:
            self.bindings.append((self.binding, data))
        if self.in_payload:
            self.payload = json.loads(data)


def resolve(data, key):
    """Resolve report evidence keys without interpreting code."""
    for part in key.split("."):
        data = data[int(part)] if isinstance(data, list) else data[part]
    return data


def test_rebuild_is_identical(builder, tmp_path):
    builder.build(tmp_path)
    assert (tmp_path / "report.html").read_bytes() == (REPORT / "report.html").read_bytes()
    assert (tmp_path / "changes.json").read_bytes() == (REPORT / "changes.json").read_bytes()


def test_every_worked_value_matches_api(builder):
    page = Page(builder.render())
    evidence = page.payload
    assert evidence["indices"] == {
        "zinc": ANODIC_INDEX_V["zinc"], "carbon_steel": ANODIC_INDEX_V["carbon_steel"]
    }
    assert len(evidence["cases"]) == len(ENVIRONMENTS)
    for case in evidence["cases"]:
        expected = screen_couple("zinc", "carbon_steel", case["environment"])
        assert case["delta_v"] == expected.delta_v
        assert case["allowable_delta_v"] == expected.allowable_delta_v
        assert case["verdict"] == expected.verdict
        assert case["anodic"] == expected.anodic
        assert case["cathodic"] == expected.cathodic
        assert case["excess_v"] == pytest.approx(expected.delta_v - expected.allowable_delta_v)
    keys = {key for key, _ in page.bindings}
    assert {"indices.zinc", "indices.carbon_steel", "marginal_band_v"} <= keys
    for i in range(len(ENVIRONMENTS)):
        assert {f"cases.{i}.{key}" for key in
                ("delta_v", "allowable_delta_v", "excess_v")} <= keys
    assert "cases.0.marginal_upper_v" in keys
    assert len(page.chart_notes) == len(ENVIRONMENTS) * 3
    for environment in ENVIRONMENTS:
        expected = screen_couple("zinc", "carbon_steel", environment)
        notes = [note for note in page.chart_notes
                 if f"environment: {builder.ENV_LABELS[environment]};" in note]
        assert len(notes) == 3
        for note in notes:
            assert f"demand: {builder.display(expected.delta_v)} V;" in note
            assert f"limit: {builder.display(expected.allowable_delta_v)} V;" in note
            upper = expected.allowable_delta_v + screening.MARGINAL_BAND_V
            assert f"upper marginal boundary: {builder.display(upper)} V;" in note
    for key, shown in page.bindings:
        assert shown == builder.display(resolve(evidence, key))


def test_changed_module_values_reach_report(builder, monkeypatch):
    monkeypatch.setitem(ANODIC_INDEX_V, "zinc", 1.30)
    monkeypatch.setitem(ENVIRONMENTS, "harsh_marine", 0.18)
    data = builder.calculations()
    assert data["indices"]["zinc"] == ANODIC_INDEX_V["zinc"]
    assert data["cases"][0]["delta_v"] == screen_couple("zinc", "carbon_steel").delta_v
    assert not data["explorer_matches_pairs"]
    with pytest.raises(ValueError, match="published example evidence differs"):
        builder.render()


def test_public_example_and_matrix_evidence(builder):
    data = builder.calculations()
    assert data["matrix_symmetric"]
    assert data["matrix_diagonal_ok"]
    assert data["matrix_matches_pairs"]
    assert data["metals_count"] == len(ANODIC_INDEX_V)
    assert data["environments_count"] == len(ENVIRONMENTS)
    assert data["marginal_band_v"] == screening.MARGINAL_BAND_V
    assert data["explorer_matches_pairs"]


def test_document_integrity_and_house_style(builder):
    text = builder.render()
    page = Page(text)
    assert 'class="hs-doc doc"' in text
    assert builder.house_style() in text
    assert not page.resources
    assert not page.bold
    assert len(page.ids) == len(set(page.ids))
    assert {link[1:] for link in page.links if link.startswith("#")} <= set(page.ids)
    assert len([key for key in page.headings if key.startswith("section-")]) == 10
    assert text.index('id="revision-history"') < text.index('id="section-2"')
    assert 'id="comment-form"' in text
    assert "showSaveFilePicker" in text and "comments.json" in text
    assert 'id="change-data"' in text
    assert 'data-limit="allowable_delta_v"' in text
    assert 'data-limit="marginal_upper_v"' in text
    assert "NOT_EVALUATED" in text
    assert "ASSUMED - to be confirmed" in text
    assert not re.search(r"<(?:h4|h5|h6)\b", text)


def test_no_unbound_numbers_in_worked_prose(builder):
    text = builder.render()
    worked = text.split('id="worked-example"', 1)[1].split('</article>', 1)[0]
    worked = re.sub(r'<span data-number="[^"]+">[^<]+</span>', "", worked)
    worked = re.sub(r"<[^>]+>", "", worked)
    assert not re.search(r"\d", worked)


def test_change_panel_has_engineering_revision(builder):
    changes = json.loads((REPORT / "changes.json").read_text())
    assert changes == builder.CHANGES
    assert changes["revision"] == builder.REVISION
    assert changes["history"][0]["revision"] == builder.REVISION
    assert changes["changes"]


def test_offline_chart_and_comment_controls(builder):
    """Exercise pointer, keyboard, comment save/load and fallback without a browser."""
    import shutil
    import subprocess

    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is absent; interactive controls cannot be executed")
    setup = r'''
const assert = require('node:assert/strict');
const elements = new Map();
function element(id) {
  if (!elements.has(id)) elements.set(id, {textContent: '', value: '', children: [], handlers: {},
    addEventListener(type, handler) {this.handlers[type] = handler;},
    appendChild(item) {this.children.push(item);}});
  return elements.get(id);
}
const point = element('point'); point.dataset = {chartNote: 'Case, component, demand, limit, units'};
global.document = {getElementById: element,
  querySelectorAll: () => [point], createElement: () => ({click() {this.clicked = true;}})};
global.window = {};
let saved = '';
let picks = 0;
window.showSaveFilePicker = async options => {
  assert.equal(options.suggestedName, 'report.comments.json'); picks++;
  return {createWritable: async () => ({write: async text => {saved = text;}, close: async () => {}}),
    getFile: async () => ({text: async () => saved})};
};
'''
    data = builder.calculations()
    setup += "\nelement('calculation-data').textContent = " + json.dumps(json.dumps(data, sort_keys=True)) + ";\n"
    setup += "element('change-data').textContent = " + json.dumps(json.dumps(builder.CHANGES)) + ";\n"
    setup += "element('comment-section').options = " + json.dumps([
        {"value": f"section-{i}"} for i in range(1, 11)
    ]) + ";\n"
    exercise = r'''
(async () => {
  assert.ok(element('changes-list').children.length);
  point.handlers.focus(); assert.equal(element('chart-readout').textContent, point.dataset.chartNote);
  point.handlers.pointerenter(); assert.equal(element('chart-readout').textContent, point.dataset.chartNote);
  const form = {elements: {section: {value: 'section-5'}, comment: {value: 'Check the coating basis.'}}};
  element('comment-form').handlers.submit({preventDefault() {}, target: form});
  assert.equal(JSON.parse(element('comments-preview').textContent)[0].text, 'Check the coating basis.');
  await element('save-comments').handlers.click();
  const original = JSON.parse(saved);
  assert.equal(original.comments.length, 1);
  assert.equal(original.report, 'Corrosion module analysis');
  assert.ok(element('comment-message').textContent.includes('saved and read back'));
  await element('save-comments').handlers.click(); assert.equal(picks, 1);
  const target = {files: [{text: async () => saved}], value: 'selected'};
  await element('load-comments').handlers.change({target});
  assert.equal(JSON.parse(element('comments-preview').textContent).length, 2);
  const mismatched = {...original, revision: 'different'};
  target.files[0].text = async () => JSON.stringify(mismatched);
  await element('load-comments').handlers.change({target});
  assert.ok(element('comment-message').textContent.includes('different report evidence'));
  assert.equal(JSON.parse(element('comments-preview').textContent).length, 2);
  target.files[0].text = async () => JSON.stringify({...original, evidence: 'different'});
  await element('load-comments').handlers.change({target});
  assert.ok(element('comment-message').textContent.includes('different report evidence'));
  delete window.showSaveFilePicker;
  let downloaded;
  URL.createObjectURL = blob => {downloaded = blob; return 'blob:comments';};
  URL.revokeObjectURL = () => {};
  await element('save-comments').handlers.click();
  assert.equal(JSON.parse(await downloaded.text()).comments.length, 2);
  assert.ok(element('comment-message').textContent.includes('manual') ||
    element('comment-message').textContent.includes('Move the file'));
})().catch(error => {console.error(error); process.exitCode = 1;});
'''
    result = subprocess.run(
        [node, "-"], input=setup + builder.INTERACTION + exercise,
        text=True, capture_output=True, check=False,
    )
    assert result.returncode == 0, result.stderr
