# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Calculation anchors and report consistency checks for VIV screening."""

import importlib.util
import csv
import json
import re
import shutil
import subprocess
from html.parser import HTMLParser
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
REBUILD = (
    "PYTHONPATH=src uv run --no-project --with pytest --with 'numpy<2' --with 'pandas<3' "
    "--with pydantic --with pyyaml --with pylife python "
    "scripts/reporting/build_vivscreening_module_report.py --scratch <temporary-directory>"
)


@pytest.fixture(scope="module")
def builder():
    path = ROOT / "scripts/reporting/build_vivscreening_module_report.py"
    spec = importlib.util.spec_from_file_location("viv_report", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def calculation(builder, tmp_path_factory):
    return builder.calculate(tmp_path_factory.mktemp("screening"))


def test_hand_calculated_anchors(calculation):
    values = calculation["values"]
    # Reynolds definition: U D / nu = 0.22 * 0.2731 / 0.00000119
    # = 0.060082 / 0.00000119 = 50489.0756302521. Relative tolerance 1e-10.
    assert values["reynolds"] == pytest.approx(50489.0756302521, rel=1e-10)
    # Required fatigue life = design duration * fatigue factor = 20 * 3 = 60 years.
    assert values["required_life_years"] == 60
    # Annular integral I = pi (Do^4 - Di^4) / 64:
    # Do^4 = 0.0055627148806321; Di^4 = 0.0027356759876161;
    # I = pi * 0.002827038893016 / 64 = 0.000138771947151745 m^4.
    assert values["inertia"] == pytest.approx(0.000138771947151745, rel=1e-10)
    # Supplied stress is amplitude at unit displacement ratio; full range
    # = 2 * 45 * 0.5221 = 46.989 MPa. Absolute tolerance 1e-9 MPa.
    assert values["mode_1_stress_range_MPa"] == pytest.approx(46.989, abs=1e-9)


def test_headline_from_displayed_inputs(calculation):
    values = calculation["values"]
    # First pinned, untensioned mode: pi/(2 L^2) * sqrt(E I / mass).
    # The displayed diameters are used without truncating wall thickness.
    import math

    inertia = math.pi * (values["outer_diameter_m"] ** 4 - values["inner_diameter_m"] ** 4) / 64
    frequency = math.pi / (2 * values["length_m"] ** 2) * math.sqrt(
        values["youngs_modulus_Pa"] * inertia / values["mass_per_length_kg_m"]
    )
    assert frequency == pytest.approx(values["mode_1_f_n_Hz"], rel=1e-10)
    required = values["design_life_years"] * values["dff"]
    assert required == values["required_life_years"]
    assert values["screening_life_years"] < required
    assert calculation["status"] == "fail"
    assert calculation["il_unassessed"] is True
    assert calculation["escalation_required"] is False


class NumberParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.numbers = []
        self.key = None

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "span" and "data-number" in attrs:
            self.key = (attrs["data-number"], attrs["data-format"])

    def handle_data(self, data):
        if self.key:
            self.numbers.append((*self.key, data))

    def handle_endtag(self, tag):
        if tag == "span":
            self.key = None


def test_displayed_number_consistency(builder, calculation):
    page = builder.render(calculation)
    parser = NumberParser()
    parser.feed(page)
    assert len(parser.numbers) > len(calculation["values"])
    assert {key for key, _, _ in parser.numbers} == set(calculation["values"])
    for key, precision, displayed in parser.numbers:
        assert displayed == format(calculation["values"][key], precision), key
    assert '<body class="hs-doc">' in page
    assert '<script src=' not in page
    assert 'showDirectoryPicker' in page
    assert 'report.comments.json' in page
    assert 'localStorage' not in page


def test_committed_report_rebuild_consistency(builder, calculation):
    committed = (ROOT / builder.REPORT).read_text(encoding="utf-8")
    fresh = builder.render(calculation)
    # Only rounded values enter either rendering; no raw library float strings.
    assert committed == fresh, f"Report is stale. Rebuild: {REBUILD}"
    changes = json.loads((ROOT / builder.CHANGES).read_text(encoding="utf-8"))
    assert changes == builder.changes(), f"Change panel is stale. Rebuild: {REBUILD}"


def test_report_structure_sources_and_public_text(builder, calculation):
    page = builder.render(calculation)
    assert len(re.findall(r'<h2 id="s\d+">', page)) == 10
    assert '<strong' not in page and '<b>' not in page
    links = re.findall(r'href="#([^"]+)"', page)
    ids = set(re.findall(r'id="([^"]+)"', page))
    assert set(links) <= ids
    assert 'Draft for maintainer review' in page
    assert 'NOT_EVALUATED' in page
    assert 'ASSUMED - to be confirmed' in page
    assert 'limit' in page and '<title>Case:' in page
    assert not re.search(r'\b(could|should|would)\b', page, re.I)
    assert not re.search(r'/(?:home|mnt)/|[\w.+-]+@[\w.-]+\.[A-Za-z]{2,}', page)


def test_all_calculated_values_match_fresh_module_consistency(builder, calculation, tmp_path):
    import yaml

    from digitalmodel.viv_screening.inputs import parse_inputs
    from digitalmodel.viv_screening.workflow import router

    cfg = yaml.safe_load((ROOT / builder.EXAMPLE).read_text(encoding="utf-8"))
    _, beam, viv = parse_inputs(cfg)
    cfg["viv_screening"]["output_dir"] = str(tmp_path)
    result = router(cfg)["viv_screening"]
    assert result["screening_status"] == calculation["status"]
    expected = {key: result[key] for key in (
        "modes_checked", "cf_lock_in_count", "il_lock_in_count", "lock_in_count",
        "screening_damage_rate_per_year", "screening_life_years", "required_life_years",
    )}
    expected.update(inertia=beam.moment_of_inertia, reynolds=viv.reynolds_number)
    with Path(result["results_csv"]).open(encoding="utf-8", newline="") as stream:
        for row in csv.DictReader(stream):
            for field in ("mode", "f_n_Hz", "Vr_lo", "A_over_D", "stress_range_MPa", "cycles_per_year", "damage_per_year"):
                expected[f"mode_{row['mode']}_{field}"] = float(row[field])
    for key, value in expected.items():
        assert value == pytest.approx(calculation["values"][key], rel=1e-10, abs=1e-12), key


def test_comment_storage_and_chart_interactions(builder, tmp_path):
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable for the browserless interaction check")
    # Exercise the actual embedded JavaScript with a minimal document adapter.
    # File reads and writes use in-memory handles; no browser or network is used.
    harness = r'''
const assert = require("node:assert/strict");
const elements = new Map();
function element() {
  return {value: "", textContent: "", children: [], events: {},
    append(...values) { this.children.push(...values); },
    replaceChildren() { this.children = []; }, click() {},
    addEventListener(name, callback) { this.events[name] = callback; }};
}
const readout = element(), marker = element();
marker.dataset = {tip: "Synthetic demand and limit"};
marker.closest = () => ({querySelector: () => readout});
global.document = {
  querySelectorAll: () => [marker],
  getElementById(id) { if (!elements.has(id)) elements.set(id, element()); return elements.get(id); },
  createElement: () => element(), createTextNode: value => ({textContent: value})
};
global.window = {};
let saved, closed = false;
const fileHandle = {
  getFile: async () => ({text: async () => JSON.stringify([{section: "s2", text: "Existing comment"}])}),
  createWritable: async () => ({write: async text => {saved = text;}, close: async () => {closed = true;}})
};
const folder = {getFileHandle: async name => {assert.equal(name, "report.comments.json"); return fileHandle;}};
'''
    assertions = r'''
(async () => {
  const el = id => document.getElementById(id);
  marker.events.focus(); assert.equal(readout.textContent, marker.dataset.tip);
  el("comment-section").value = "s1"; el("s1").textContent = "Executive summary";
  el("comment-text").value = "  <b>Plain comment text</b>  ";
  el("comment-add").onclick(); assert.equal(comments.length, 1);
  assert.equal(comments[0].text, "<b>Plain comment text</b>");
  assert.equal(el("comments").children[0].children[1].textContent, ": <b>Plain comment text</b>");
  await el("comment-save").onclick(); assert.match(status.textContent, /Select the report folder/);
  await el("comment-folder").onclick(); assert.match(status.textContent, /unavailable/);
  window.showDirectoryPicker = async () => folder;
  await el("comment-folder").onclick(); assert.equal(comments.length, 2);
  await el("comment-save").onclick(); assert.equal(closed, true);
  assert.equal(JSON.parse(saved).length, 2); assert.equal(saved.endsWith("\n"), true);
  const before = comments.length;
  await el("comment-import").onchange({target: {files: [{text: async () => '[{"section":"bad","text":"invalid"}]'}]}});
  assert.equal(comments.length, before); assert.match(status.textContent, /must contain/);
  await el("comment-import").onchange({target: {files: [{text: async () => '[{"section":"s3","text":"Imported comment"}]'}]}});
  assert.equal(comments.length, 3);
  el("comment-export").onclick(); assert.match(status.textContent, /exported/);
})().catch(error => { console.error(error); process.exitCode = 1; });
'''
    script = tmp_path / "comments.cjs"
    script.write_text(harness + builder._INTERACTIONS + assertions, encoding="utf-8", newline="\n")
    result = subprocess.run([node, str(script)], capture_output=True, text=True, encoding="utf-8", timeout=15)
    assert result.returncode == 0, result.stderr


@pytest.mark.parametrize("previous_folder", [False, True])
@pytest.mark.parametrize("load_result", [
    "lookup-denied", "file-unreadable", "file-disappeared", "text-unreadable",
    "text-disappeared", "malformed-json", "invalid-schema", "absent", "valid-empty",
])
def test_comment_save_uses_only_a_validated_folder(builder, tmp_path, load_result, previous_folder):
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable for the browserless interaction check")
    # Prematurely accepting a folder permits Save to overwrite unread comments.
    # Only lookup-level absence permits a new file; read errors preserve the old destination.
    harness = r'''
const assert = require("node:assert/strict");
const [loadResult, previousFolder] = process.argv.slice(2);
const elements = new Map();
global.document = {
  querySelectorAll: () => [],
  getElementById(id) {
    if (!elements.has(id)) elements.set(id, {
      value: "", textContent: "", append() {}, replaceChildren() {}
    });
    return elements.get(id);
  },
  createElement: () => ({append() {}}), createTextNode: text => ({textContent: text})
};
global.window = {};
const failure = name => Object.assign(new Error("Comment read failed"), {name});
let selectedWrites = 0, previousSaved;
const selectedHandle = {
  async getFile() {
    if (loadResult === "file-unreadable") throw failure("NotReadableError");
    if (loadResult === "file-disappeared") throw failure("NotFoundError");
    return {async text() {
      if (loadResult === "text-unreadable") throw failure("NotReadableError");
      if (loadResult === "text-disappeared") throw failure("NotFoundError");
      if (loadResult === "malformed-json") return "{";
      if (loadResult === "invalid-schema") return '[{"section":"bad","text":"Invalid"}]';
      return "[]";
    }};
  },
  async createWritable() {
    selectedWrites++;
    return {async write() {}, async close() {}};
  }
};
const selectedFolder = {async getFileHandle(name, options) {
  assert.equal(name, "report.comments.json");
  if (!options?.create) {
    if (loadResult === "lookup-denied") throw failure("NotAllowedError");
    if (loadResult === "absent") throw failure("NotFoundError");
  }
  return selectedHandle;
}};
const safeFolder = {async getFileHandle(name) {
  assert.equal(name, "report.comments.json");
  return {
    getFile: async () => ({text: async () => '[{"section":"s2","text":"Previous comment"}]'}),
    createWritable: async () => ({
      write: async text => { previousSaved = JSON.parse(text); }, close: async () => {}
    })
  };
}};
'''
    assertions = r'''
(async () => {
  const el = id => document.getElementById(id);
  if (previousFolder === "yes") {
    window.showDirectoryPicker = async () => safeFolder;
    await el("comment-folder").onclick();
    el("comment-section").value = "s1";
    el("comment-text").value = "Unsaved comment";
    el("comment-add").onclick();
  }
  const before = JSON.parse(JSON.stringify(comments));
  window.showDirectoryPicker = async () => selectedFolder;
  await el("comment-folder").onclick();
  assert.deepEqual(comments, before, "Folder selection must preserve current comments");
  await el("comment-save").onclick();
  if (["absent", "valid-empty"].includes(loadResult)) {
    assert.equal(selectedWrites, 1, "A validated or confirmed absent file permits saving");
    assert.equal(previousSaved, undefined);
  } else {
    assert.equal(selectedWrites, 0, "A failed load must never open a writer in that folder");
    if (previousFolder === "yes") {
      assert.deepEqual(previousSaved, before, "The previous validated folder remains usable");
    } else {
      assert.match(status.textContent, /Select the report folder/);
    }
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
'''
    script = tmp_path / "validated-comments.cjs"
    script.write_text(harness + builder._INTERACTIONS + assertions, encoding="utf-8", newline="\n")
    result = subprocess.run(
        [node, str(script), load_result, "yes" if previous_folder else "no"],
        capture_output=True, text=True, encoding="utf-8", timeout=15,
    )
    assert result.returncode == 0, result.stderr
