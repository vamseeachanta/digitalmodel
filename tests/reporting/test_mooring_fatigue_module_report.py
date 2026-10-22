# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check the delivered report against fresh calls to the fatigue workflow."""

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

from digitalmodel.mooring_fatigue.workflow import router

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/mooringfatigue/report.html"
SPEC = importlib.util.spec_from_file_location(
    "mooring_report", ROOT / "scripts/reporting/build_mooring_fatigue_module_report.py"
)
BUILDER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(BUILDER)


class Document(HTMLParser):
    """Collect numeric observations, references and visible report text."""

    def __init__(self, text):
        super().__init__()
        self.numbers = []
        self.ids = []
        self.links = []
        self.visible = []
        self.headings = []
        self.markers = []
        self.active = None
        self.hidden = 0
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag in {"style", "script"}:
            self.hidden += 1
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if attrs.get("href", "").startswith("#"):
            self.links.append(attrs["href"][1:])
        if tag == "h2":
            self.headings.append(attrs.get("id"))
        if "data-value" in attrs:
            self.active = [attrs["data-value"], attrs["data-format"], ""]
            self.numbers.append(self.active)
        if "data-detail" in attrs:
            self.markers.append(attrs)

    def handle_endtag(self, tag):
        if tag in {"style", "script"}:
            self.hidden -= 1
        if tag == "span":
            self.active = None

    def handle_data(self, data):
        if self.active is not None:
            self.active[2] += data
        if not self.hidden:
            self.visible.append(data)


def fresh_facts(tmp_path):
    """Read public inputs and returned CSVs independently of the builder."""
    cfg = yaml.safe_load(BUILDER.EXAMPLE.read_text())
    inputs = copy.deepcopy(cfg["mooring_fatigue"])
    cfg["mooring_fatigue"]["output_dir"] = str(tmp_path)
    result = router(cfg)["mooring_fatigue"]
    with Path(result["results_csv"]).open() as stream:
        bins = list(csv.DictReader(stream))
    return inputs, result, bins


def test_rebuild_and_all_worked_example_numbers(tmp_path):
    page, changes = BUILDER.build(tmp_path / "build")
    assert page == REPORT.read_text()
    assert changes == json.loads(REPORT.with_name("changes.json").read_text())
    inputs, result, bins = fresh_facts(tmp_path / "independent")
    expected = {
        "design_life": result["design_life_years"],
        "dff": result["dff"],
        "damage_limit": 1 / result["dff"],
        "line_count": len(result["lines"]),
        "bin_count": len(bins),
        "governing_damage": result["governing_damage"],
        "governing_life": result["governing_fatigue_life_years"],
        "governing_margin": result["governing_dff_margin"],
    }
    for i, line in enumerate(result["lines"]):
        for key in ("total_damage", "fatigue_life_years", "required_life_years", "dff_margin"):
            expected[f"line.{i}.{key}"] = line[key]
        expected[f"line.{i}.area"] = inputs["lines"][i]["area_mm2"]
    for i, row in enumerate(bins):
        for key in ("tension_range_kN", "stress_range_MPa", "cycles", "allowable_cycles", "damage"):
            expected[f"bin.{i}.{key}"] = float(row[key])
    doc = Document(page)
    assert {key for key, _, _ in doc.numbers} == set(expected)
    for key, precision, rendered in doc.numbers:
        assert rendered == format(expected[key], precision), key
    for marker, line in zip(doc.markers, result["lines"], strict=True):
        detail = (
            f"Public example; component {line['line_id']}; demand {line['total_damage']:.4f}; "
            f"limit {expected['damage_limit']:.4f}; units: dimensionless damage."
        )
        assert marker["data-detail"] == detail
        assert marker["aria-label"] == detail
    assert result["governing_line"] in page
    assert result["screening_status"] == "pass"
    assert len(doc.numbers) > len(expected)  # Summary and hover repeat the same observations.


def test_equations_against_independent_bin_calculation(tmp_path):
    inputs, result, bins = fresh_facts(tmp_path)
    for line in result["lines"]:
        own_bins = [row for row in bins if row["line_id"] == line["line_id"]]
        damage = sum(float(row["cycles"]) / float(row["allowable_cycles"]) for row in own_bins)
        assert line["total_damage"] == pytest.approx(damage)
        assert line["fatigue_life_years"] == pytest.approx(inputs["design_life_years"] / damage)
        assert line["dff_margin"] == pytest.approx(1 / (damage * inputs["dff"]))
        source = next(item for item in inputs["lines"] if item["id"] == line["line_id"])
        for row in own_bins:
            assert float(row["stress_range_MPa"]) == pytest.approx(
                float(row["tension_range_kN"]) * 1000 / source["area_mm2"]
            )


def test_house_style_structure_and_public_text():
    page = REPORT.read_text()
    doc = Document(page)
    exemplar = BUILDER.EXEMPLAR.read_text()
    assert re.search(r"<style>.*?</style>", exemplar, re.S).group() in page
    assert '<body class="hs-doc">' in page
    assert len(doc.headings) == 10
    assert len(doc.ids) == len(set(doc.ids))
    assert set(doc.links) <= set(doc.ids)
    assert page.index('id="revision-history"') < page.index('id="section-2"')
    assert "<strong" not in page and "<b>" not in page
    assert not re.search(r'<(?:script|link)[^>]+(?:src|href)="https?://', page)
    visible = " ".join(doc.visible)
    assert not re.search(r"\b(could|should|would|ow[n]er|certified|compliant)\b", visible, re.I)
    assert not re.search(r"\d+\.\d{5,}", visible)
    assert 'data-limit="damage_limit"' in page
    assert 'id="chart-detail"' in page and "addEventListener('focus'" in page
    assert "showSaveFilePicker" in page and "report.comments.json" in page
    assert "JSON.stringify(changes" in page


@pytest.mark.parametrize("material", ["POLYESTER", "UNKNOWN"])
def test_material_rejection_is_documented(tmp_path, material):
    cfg = yaml.safe_load(BUILDER.EXAMPLE.read_text())
    cfg["mooring_fatigue"]["output_dir"] = str(tmp_path)
    cfg["mooring_fatigue"]["lines"][0]["material"] = material
    with pytest.raises(ValueError, match="out of scope|unsupported material"):
        router(cfg)


def test_failed_screening_has_documented_action(tmp_path):
    cfg = yaml.safe_load(BUILDER.EXAMPLE.read_text())
    cfg["mooring_fatigue"]["output_dir"] = str(tmp_path)
    for line in cfg["mooring_fatigue"]["lines"]:
        for row in line["tension_range_bins"]:
            row["n_cycles"] *= 1000
    result = router(cfg)["mooring_fatigue"]
    assert result["screening_status"] == "fail"
    assert result["governing_dff_margin"] < 1
    assert "Suspend acceptance" in REPORT.read_text()


def test_comment_roundtrip_and_chart_interaction():
    """Exercise the embedded controls without a browser or external packages."""
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable for the optional interface check")
    harness = r"""
const assert = require('node:assert/strict');
const vm = require('node:vm');
const nodes = {};
for (const id of ['change-data', 'change-record', 'chart-detail', 'comment-panel',
  'comment-section', 'comment-quote', 'comment-text', 'comment-status',
  'comment-preview', 'capture-quote', 'add-comment', 'load-comments', 'save-comments'])
  nodes[id] = {value: '', textContent: '', dataset: {}};
nodes['change-data'].textContent = JSON.stringify({report: 'Mooring fatigue module analysis', revision: 'A'});
nodes['comment-panel'].dataset.basis = 'calculation-basis';
nodes['comment-section'].value = 'section-1';
nodes['comment-section'].options = [{value: 'section-1'}];
let saved = '', clicked = false;
const marker = {dataset: {detail: 'Component demand and limit'}, callbacks: {},
  addEventListener(type, fn) { this.callbacks[type] = fn; }};
const window = {getSelection: () => 'Selected engineering text',
  showSaveFilePicker: async options => {
    assert.equal(options.suggestedName, 'report.comments.json');
    return {createWritable: async () => ({write: async data => {saved = data;}, close: async () => {}}),
      getFile: async () => ({text: async () => saved})};
  }};
const document = {getElementById: id => nodes[id], querySelectorAll: () => [marker],
  createElement: () => ({click() {clicked = true;}})};
const context = {document, window, crypto: {randomUUID: () => 'comment-one'},
  Blob, URL: {createObjectURL: () => 'blob:comments', revokeObjectURL: () => {}},
  setTimeout: fn => fn()};
vm.runInNewContext(UI_SOURCE, context);
async function check() {
  marker.callbacks.focus();
  assert.equal(nodes['chart-detail'].textContent, marker.dataset.detail);
  nodes['capture-quote'].onclick();
  assert.equal(nodes['comment-quote'].value, 'Selected engineering text');
  nodes['comment-text'].value = 'Confirm the exposure basis.';
  nodes['add-comment'].onclick();
  assert.equal(JSON.parse(nodes['comment-preview'].textContent).length, 1);
  await nodes['save-comments'].onclick();
  const roundtrip = JSON.parse(saved);
  assert.equal(roundtrip.comments[0].text, 'Confirm the exposure basis.');
  assert.equal(roundtrip.comments[0].section, 'section-1');
  const load = async payload => {
    nodes['load-comments'].files = [{size: 100, text: async () => JSON.stringify(payload)}];
    await nodes['load-comments'].onchange();
  };
  await load(roundtrip);
  assert.equal(JSON.parse(nodes['comment-preview'].textContent).length, 1);
  await load({...roundtrip, basis: 'different-basis'});
  assert.match(nodes['comment-status'].textContent, /do not match/);
  const conflict = JSON.parse(JSON.stringify(roundtrip));
  conflict.comments[0].text = 'Conflicting text';
  await load(conflict);
  assert.match(nodes['comment-status'].textContent, /conflicting text/);
  assert.equal(JSON.parse(nodes['comment-preview'].textContent)[0].text, 'Confirm the exposure basis.');
  window.showSaveFilePicker = undefined;
  await nodes['save-comments'].onclick();
  assert.equal(clicked, true);
  assert.match(nodes['comment-status'].textContent, /Move report.comments.json beside/);
}
check().catch(error => {console.error(error.message); process.exitCode = 1;});
"""
    completed = subprocess.run(
        [node], input="const UI_SOURCE = " + json.dumps(BUILDER.UI) + ";\n" + harness,
        text=True, capture_output=True, check=False, timeout=30,
    )
    assert completed.returncode == 0, completed.stderr
