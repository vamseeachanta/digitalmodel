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
    cfg = yaml.safe_load(BUILDER.EXAMPLE.read_text(encoding="utf-8"))
    inputs = copy.deepcopy(cfg["mooring_fatigue"])
    cfg["mooring_fatigue"]["output_dir"] = str(tmp_path)
    result = router(cfg)["mooring_fatigue"]
    with Path(result["results_csv"]).open(encoding="utf-8", newline="") as stream:
        bins = list(csv.DictReader(stream))
    return inputs, result, bins


def test_rebuild_and_all_worked_example_numbers(tmp_path):
    page, changes = BUILDER.build(tmp_path / "build")
    assert page == REPORT.read_text(encoding="utf-8")
    assert changes == json.loads(REPORT.with_name("changes.json").read_text(encoding="utf-8"))
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
        "governing_required_life": next(
            line["required_life_years"] for line in result["lines"]
            if line["line_id"] == result["governing_line"]
        ),
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
    page = REPORT.read_text(encoding="utf-8")
    doc = Document(page)
    exemplar = BUILDER.EXEMPLAR.read_text(encoding="utf-8")
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
    cfg = yaml.safe_load(BUILDER.EXAMPLE.read_text(encoding="utf-8"))
    cfg["mooring_fatigue"]["output_dir"] = str(tmp_path)
    cfg["mooring_fatigue"]["lines"][0]["material"] = material
    with pytest.raises(ValueError, match="out of scope|unsupported material"):
        router(cfg)


def test_failed_screening_has_documented_action(tmp_path, monkeypatch):
    cfg = yaml.safe_load(BUILDER.EXAMPLE.read_text(encoding="utf-8"))
    cfg["mooring_fatigue"]["output_dir"] = str(tmp_path)
    for line in cfg["mooring_fatigue"]["lines"]:
        for row in line["tension_range_bins"]:
            row["n_cycles"] *= 1000
    result = router(copy.deepcopy(cfg))["mooring_fatigue"]
    assert result["screening_status"] == "fail"
    assert result["governing_dff_margin"] < 1
    changed_input = tmp_path / "exceedance.yml"
    changed_input.write_text(yaml.safe_dump(cfg), encoding="utf-8", newline="\n")
    monkeypatch.setattr(BUILDER, "EXAMPLE", changed_input)
    page, _ = BUILDER.build(tmp_path / "rendered")
    assert "The screening criterion is not met." in page
    assert "Suspend acceptance" in page
    assert "The screening criterion is met.</td>" not in page


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
    nodes['load-comments'].files = [{size: new Blob([JSON.stringify(payload)]).size, text: async () => JSON.stringify(payload)}];
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
  const picker = window.showSaveFilePicker;
  window.showSaveFilePicker = undefined;
  await nodes['save-comments'].onclick();
  assert.equal(clicked, true);
  assert.match(nodes['comment-status'].textContent, /Move report.comments.json beside/);
  const retained = nodes['comment-preview'].textContent;
  nodes['comment-text'].value = 'é'.repeat(500001);
  nodes['add-comment'].onclick();
  assert.match(nodes['comment-status'].textContent, /size limit/);
  assert.equal(nodes['comment-preview'].textContent, retained);
  assert.equal(nodes['comment-text'].value.length, 500001);
  window.showSaveFilePicker = picker;
  vm.runInNewContext(UI_SOURCE, context);
  const full = {...roundtrip, comments: [{...roundtrip.comments[0], text: ''}]};
  const overhead = new Blob([JSON.stringify(full, null, 2) + '\n']).size;
  full.comments[0].text = 'é'.repeat(Math.floor((1000000 - overhead) / 2)) +
    'x'.repeat((1000000 - overhead) % 2);
  assert.equal(new Blob([JSON.stringify(full, null, 2) + '\n']).size, 1000000);
  await load(full);
  assert.match(nodes['comment-status'].textContent, /Matching comments loaded/);
  await nodes['save-comments'].onclick();
  assert.equal(new Blob([saved]).size, 1000000);
  await load(JSON.parse(saved));
  assert.match(nodes['comment-status'].textContent, /Matching comments loaded/);
  const atLimit = nodes['comment-preview'].textContent;
  await load({...roundtrip, comments: [{...roundtrip.comments[0], id: 'comment-two'}]});
  assert.match(nodes['comment-status'].textContent, /size limit/);
  assert.equal(nodes['comment-preview'].textContent, atLimit);
  full.comments[0].text += 'x';
  await load(full);
  assert.match(nodes['comment-status'].textContent, /size limit/);
  assert.equal(nodes['comment-preview'].textContent, atLimit);
  clicked = false;
  window.showSaveFilePicker = undefined;
  context.Blob = class extends Blob { get size() { return super.size + 1; } };
  await nodes['save-comments'].onclick();
  assert.match(nodes['comment-status'].textContent, /size limit/);
  assert.equal(clicked, false);
}
check().catch(error => {console.error(error.message); process.exitCode = 1;});
"""
    completed = subprocess.run(
        [node], input="const UI_SOURCE = " + json.dumps(BUILDER.UI) + ";\n" + harness,
        text=True, capture_output=True, check=False, timeout=30,
    )
    assert completed.returncode == 0, completed.stderr


def test_headline_values_against_hand_worked_literals(tmp_path):
    """Independent arithmetic, using the public implementation's 2011 Table 2-2 D record.

    S_k = 10**((11.764 - 6)/3); N = 1e6*(stress/S_k)**(-5).
    Stresses are tension*1000/area; D = sum(cycles/N), L = 25/D,
    M = 1/(3*D). Literals are fixed independently of router and HTML.
    """
    _, result, bins = fresh_facts(tmp_path)
    assert 10 ** ((11.764 - 6) / 3) == pytest.approx(83.4321, rel=1e-6)
    assert float(bins[-1]["allowable_cycles"]) == pytest.approx(2.54197e6, rel=2e-6)
    assert result["lines"][0]["total_damage"] == pytest.approx(2.28233e-3, rel=2e-6)
    assert result["governing_damage"] == pytest.approx(1.70378e-2, rel=3e-6)
    assert result["governing_fatigue_life_years"] == pytest.approx(1467.32, rel=4e-6)
    assert result["governing_dff_margin"] == pytest.approx(19.564, rel=2e-5)
    rendered = {key: value for key, _, value in Document(BUILDER.build(tmp_path / "page")[0]).numbers}
    assert rendered["governing_damage"] == "0.0170"
    assert rendered["governing_life"] == "1467.3"
    assert rendered["governing_margin"] == "19.6"


def test_component_basis_is_not_qualified(tmp_path):
    page, _ = BUILDER.build(tmp_path)
    assert "54-fold" in page
    assert "DNV-OS-E301" in page and "API RP 2SK" in page
    assert "Component status: NOT_EVALUATED" in page
    assert "mooring governing component is NOT_EVALUATED" in page
    assert "83.4 MPa" in page and "1,000,000 cycles" in page
    assert "15.6067" in page and "15.606" in page and "0.2 %" in page
    assert "Draft for client review" not in page
    assert "Draft for review" in page


def test_governing_required_life_is_selected_by_identity(tmp_path, monkeypatch):
    observed = BUILDER.observations(tmp_path / "observations")
    observed[1]["governing_line"] = observed[1]["lines"][0]["line_id"]
    observed[1]["lines"][0]["required_life_years"] = 90.0
    observed[3]["line.0.required_life_years"] = 90.0
    monkeypatch.setattr(BUILDER, "observations", lambda work_dir: observed)
    page, _ = BUILDER.build(tmp_path / "page")
    values = [value for key, _, value in Document(page).numbers if key == "governing_required_life"]
    assert values == ["90.0", "90.0"]


@pytest.mark.parametrize("line_count", [1, 3, 8])
def test_chart_markers_stay_within_axis(tmp_path, monkeypatch, line_count):
    cfg = yaml.safe_load(BUILDER.EXAMPLE.read_text(encoding="utf-8"))
    settings = cfg["mooring_fatigue"]
    settings["lines"] = [copy.deepcopy(settings["lines"][0]) for _ in range(line_count)]
    for index, line in enumerate(settings["lines"]):
        line["id"] = f"line-{index}"
    example = tmp_path / "example.yml"
    example.write_text(yaml.safe_dump(cfg), encoding="utf-8", newline="\n")
    monkeypatch.setattr(BUILDER, "EXAMPLE", example)
    page, _ = BUILDER.build(tmp_path / "page")
    coordinates = re.findall(r'<circle cx="([0-9.]+)"', page)
    assert all(re.fullmatch(r"\d+\.\d", x) for x in coordinates)
    positions = [float(x) for x in coordinates]
    assert len(positions) == line_count
    assert len(set(positions)) == line_count
    assert all(90 < x < 740 for x in positions)


def test_basis_is_lf_normalized_and_calculation_scoped(tmp_path, monkeypatch):
    mirror = tmp_path / "mirror"
    required = [BUILDER.EXAMPLE, Path(BUILDER.__file__), BUILDER.EXEMPLAR]
    # The demonstrated route calls only these fatigue helpers, not every fatigue module.
    required += [ROOT / p for p in (
        "src/digitalmodel/mooring_fatigue/workflow.py",
        "src/digitalmodel/fatigue/sn_curves.py",
        "src/digitalmodel/fatigue/c203_sn_tables.py",
        "src/digitalmodel/fatigue/damage.py",
    )]
    for path in required:
        target = mirror / path.relative_to(ROOT)
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(path.read_text(encoding="utf-8"), encoding="utf-8", newline="\n")
    monkeypatch.setattr(BUILDER, "ROOT", mirror)
    monkeypatch.setattr(BUILDER, "__file__", str(mirror / Path(BUILDER.__file__).relative_to(ROOT)))
    monkeypatch.setattr(BUILDER, "EXAMPLE", mirror / BUILDER.EXAMPLE.relative_to(ROOT))
    monkeypatch.setattr(BUILDER, "EXEMPLAR", mirror / BUILDER.EXEMPLAR.relative_to(ROOT))
    page_lf, _ = BUILDER.build(tmp_path / "lf")
    for path in required:
        target = mirror / path.relative_to(ROOT)
        target.write_text(target.read_text(encoding="utf-8"), encoding="utf-8", newline="\r\n")
    page_crlf, _ = BUILDER.build(tmp_path / "crlf")
    assert page_crlf == page_lf
    unrelated = mirror / "src/digitalmodel/fatigue/fatigue_reporting.py"
    unrelated.write_text("# unrelated reporting edit\n", encoding="utf-8", newline="\n")
    assert BUILDER.build(tmp_path / "unrelated")[0] == page_lf
    helper = mirror / "src/digitalmodel/fatigue/damage.py"
    helper.write_text(helper.read_text(encoding="utf-8") + "# calculation edit\n",
                      encoding="utf-8", newline="\n")
    assert BUILDER.build(tmp_path / "changed")[0] != page_lf
