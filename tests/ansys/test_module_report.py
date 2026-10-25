# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild and independently check the public module report without a solver."""

import importlib.util
import json
import os
import shutil
import subprocess
from decimal import Decimal, localcontext
from html.parser import HTMLParser
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/ansys"


def builder():
    spec = importlib.util.spec_from_file_location("ansys_module_report", REPORT / "build_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class Page(HTMLParser):
    def __init__(self, text):
        super().__init__()
        self.numbers = []
        self.ids = []
        self.links = []
        self.headings = []
        self.body = None
        self.external_assets = []
        self.displayed = []
        self.current_number = None
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "data-number" in attrs:
            self.numbers.append(attrs["data-number"])
            self.current_number = attrs["data-number"]
        if tag in ("script", "link") and ("src" in attrs or "href" in attrs):
            self.external_assets.append(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "a":
            self.links.append(attrs.get("href", ""))
        if tag == "h2":
            self.headings.append(attrs.get("id"))
        if tag == "body":
            self.body = attrs.get("class")

    def handle_data(self, text):
        if self.current_number:
            self.displayed.append((self.current_number, text))

    def handle_endtag(self, tag):
        if tag == "span":
            self.current_number = None


def test_rebuild_matches_committed_report(tmp_path):
    module = builder()
    module.build(tmp_path)
    for filename in ("report.html", "changes.json"):
        assert (tmp_path / filename).read_bytes() == (REPORT / filename).read_bytes(), (
            "Rebuild with PYTHONPATH=src uv run python "
            "docs/reports/modules/ansys/build_report.py --output-dir docs/reports/modules/ansys"
        )


def test_every_worked_value_calls_module_and_public_example():
    from digitalmodel.ansys.cylinder_benchmark import build_case
    from digitalmodel.ansys.cylinder_reference import reference_text

    data = builder().compute()
    record = json.loads((ROOT / "examples/ansys/cylinder-benchmark/reference.json").read_text(encoding="utf-8"))
    for row in data["stations"]:
        expected = reference_text(row["pressure_mpa"], row["r_mm"], row["y_mm"])
        assert row["values"] == expected
        assert row["pressure_mpa"] == data["pressure_mpa"]
        retained = next(s for s in record["rows"] if s["station_id"] == row["station_id"]
                        and s["pressure_mpa"] == row["pressure_mpa"])
        assert retained["values"] == expected
    for case in data["meshes"]:
        actual = build_case(case["case_id"])
        assert case["nodes"] == len(actual["nodes"])
        assert case["elements"] == len(actual["elements"])
        assert actual["deck_bytes"] == (ROOT / "examples/ansys/cylinder-benchmark/prepared" /
                                         (case["case_id"] + ".inp")).read_bytes()
    for point in data["profile"]:
        assert point["values"] == reference_text(data["pressure_mpa"], point["r_mm"], data["y_mm"])


def test_independent_equilibrium_hooke_law_and_boundary_oracle():
    data = builder().compute()
    a = Decimal(data["basis"]["inner_radius_mm"])
    b = a + Decimal(data["basis"]["wall_thickness_mm"])
    p = Decimal(data["pressure_mpa"])
    e = Decimal(data["basis"]["youngs_modulus_mpa"])
    nu = Decimal(data["basis"]["poisson_ratio"])
    with localcontext() as context:
        context.prec = 50
        aa = p * a * a / (b * b - a * a)
        bb = aa * b * b
        for point in data["profile"]:
            r = Decimal(point["r_mm"])
            v = {k: Decimal(s) for k, s in point["values"].items()}
            assert v["sigma_r"] == pytest.approx(aa - bb / r**2, abs=Decimal("1e-45"))
            assert v["sigma_theta"] == pytest.approx(aa + bb / r**2, abs=Decimal("1e-45"))
            assert v["sigma_z"] == v["tau_rz"] == 0
            assert v["u_r"] == pytest.approx(r * (v["sigma_theta"] - nu * v["sigma_r"]) / e,
                                              abs=Decimal("1e-45"))
        assert Decimal(data["profile"][0]["values"]["sigma_r"]) == -p
        assert Decimal(data["profile"][-1]["values"]["sigma_r"]) == 0
    assert all(Decimal(v) == 0 for v in data["zero_control"].values())
    # Hand calculation: Applied Mechanics of Solids, online edition, 4.1.7–4.1.9.
    # At a=750, b=810, p=10: A=10*750**2/(810**2-750**2)=60.096154 MPa.
    # Bore radial=-p=-10; hoop=2*A+p=130.192308 MPa;
    # u_r=750*(130.192308+0.3*10)/200000=0.499471154 mm.
    bore = data["profile"][0]["values"]
    for key, expected in (("sigma_r", -10.0), ("sigma_theta", 130.192308),
                          ("u_r", 0.499471154)):
        assert float(bore[key]) == pytest.approx(expected, rel=1e-8, abs=1e-9)


def test_all_displayed_example_numbers_are_bound_and_formatted():
    module = builder()
    text = module.render(module.compute())
    page = Page(text)
    # Each numeric input, output, mesh count, comparison and plotted point is
    # registered by the renderer. No unbound worked-result text is permitted.
    assert set(page.numbers) == set(module.numeric_values(module.compute()))
    assert len(page.numbers) > len(set(page.numbers))
    registered = module.numeric_values(module.compute())
    for key, displayed_text in page.displayed:
        assert displayed_text == module.format_value(registered[key], key)
    for key, value in module.numeric_values(module.compute()).items():
        assert f'data-number="{key}"' in text
        assert module.format_value(value, key) in text


def test_structure_sources_and_standalone_assets():
    module = builder()
    text = module.render(module.compute())
    page = Page(text)
    assert page.body == "hs-doc"
    assert page.headings[:10] == [f"section-{i}" for i in range(1, 11)]
    assert len(page.ids) == len(set(page.ids))
    assert all(link[1:] in page.ids for link in page.links if link.startswith("#"))
    for link in page.links:
        if link and not link.startswith(("#", "https:")):
            assert (REPORT / link).exists(), link
    assert page.external_assets == []
    assert '<b>' not in text and '<strong>' not in text
    assert "Draft for maintainer review" in text
    assert "NOT_EVALUATED" in text
    assert "showDirectoryPicker" in text
    assert "textContent" in text
    assert "innerHTML" not in module.INTERACTION
    assert "fetch(" not in module.INTERACTION


@pytest.fixture
def run_interaction(tmp_path):
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is absent; browser-free interaction check is unavailable")
    module = builder()
    # A DOM and file-handle fixture exercises the shipped script, including the
    # asynchronous read/merge/write path. No browser or solver is launched.
    harness = r'''
const vm = require('node:vm');
const {TextEncoder} = require('node:util');
const assert = require('node:assert/strict');
class Element {
  constructor() { this.textContent = ''; this.children = []; this.value = ''; this.files = []; }
  appendChild(child) { this.children.push(child); if (!this.value) this.value = child.value; }
  replaceChildren() { this.children = []; }
  click() { this.clicked = true; }
}
const ids = new Map();
const get = id => { if (!ids.has(id)) ids.set(id, new Element()); return ids.get(id); };
const config = {report: 'ANSYS module analysis', revision: '1', content_digest: 'example',
                sections: ['section-1', 'section-2']};
get('comment-config').textContent = JSON.stringify(config);
let onDisk = null, written = null, writtenText = null, exported = null, nextId = 0, writesOpened = 0;
const stored = () => JSON.stringify({...config, comments: onDisk}, null, 2) + '\n';
const directory = {
  async getFileHandle(name, options) {
    if (name === 'report.html') return {getFile: async () => ({text: async () => 'report'})};
    if (!onDisk && !options) throw Object.assign(new Error('absent'), {name: 'NotFoundError'});
    return {
      getFile: async () => ({size: Buffer.byteLength(stored(), 'utf8'), text: async () => stored()}),
      createWritable: async () => {
        writesOpened++;
        return {write: async value => {writtenText = value; written = JSON.parse(value);},
                close: async () => {onDisk = written.comments;}};
      }
    };
  }
};
for (const q of ['sigma_r', 'sigma_vm']) get('readout-' + q).children = [{hidden:false}, {hidden:true}];
const circles = {};
for (const q of ['sigma_r', 'sigma_vm']) circles[q] = [{dataset: {index:'1'}}];
const document = {
  getElementById: get, createElement: () => new Element(),
  querySelectorAll: selector => circles[selector.includes('sigma_vm') ? 'sigma_vm' : 'sigma_r']
};
class DOMParser { parseFromString() { return {getElementById: () => ({textContent: JSON.stringify(config)})}; } }
const context = {document, DOMParser, TextEncoder, window: {showDirectoryPicker: async () => directory},
  crypto: {randomUUID: () => 'comment-' + (++nextId)}, Blob: class {constructor(parts) {exported = parts[0];}},
  URL: {createObjectURL: () => 'blob:example', revokeObjectURL() {}}};
vm.runInNewContext(SCRIPT, context);
'''
    def run(scenario):
        script = tmp_path / "interactions.cjs"
        footer = "})().catch(error => {console.error(error); process.exitCode = 1;});"
        script.write_text("const SCRIPT = " + json.dumps(module.INTERACTION) + ";\n" +
                          harness + "\n(async () => {\n" + scenario + footer,
                          encoding="utf-8", newline="\n")
        result = subprocess.run([node, script.as_posix()], capture_output=True,
                                text=True, encoding="utf-8", env=dict(os.environ), timeout=30)
        assert result.returncode == 0, result.stderr

    return run


def test_comment_persistence_and_chart_controls_without_browser(run_interaction):
    run_interaction(r'''
  get('comment-text').value = '<img src=x onerror=alert(1)> A boundary question.';
  get('comment-add').onclick();
  assert.match(get('comment-list').children[0].textContent, /<img/);
  await get('comment-save').onclick();
  assert.equal(written, null);
  await get('comment-folder').onclick();
  await get('comment-save').onclick();
  assert.equal(written.comments.length, 1);
  const first = written.comments[0];
  // An externally saved comment is merged immediately before writing.
  onDisk.push({id:'external', section:'section-2', text:'Retained from disk.'});
  get('comment-text').value = 'An additional question.'; get('comment-add').onclick();
  await get('comment-save').onclick();
  assert.equal(written.comments.length, 3);
  assert.equal(written.comments.find(row => row.id === first.id).text, first.text);
  get('comment-export').onclick(); assert.equal(JSON.parse(exported).comments.length, 3);
  // A mismatched revision is refused without losing existing comments.
  get('comment-load').files = [{size:100, text:async () => JSON.stringify({...config, revision:'other', comments:[]})}];
  await get('comment-load').onchange();
  assert.match(get('comment-status').textContent, /different report/);
  await get('comment-save').onclick(); assert.equal(written.comments.length, 3);
  // A valid exported file is loaded and retained.
  get('comment-load').files = [{size:100, text:async () => JSON.stringify({...config,
    comments:[{id:'imported', section:'section-1', text:'Imported question.'}]})}];
  await get('comment-load').onchange(); await get('comment-save').onclick();
  assert.equal(written.comments.length, 4);
  // Concurrent content under the same identity is rejected before replacement.
  onDisk = [{...first, text:'Conflicting disk edit.'}];
  const before = written;
  await get('comment-save').onclick();
  assert.equal(written, before);
  assert.match(get('comment-status').textContent, /Conflicting comment identity/);
  for (const q of ['sigma_r', 'sigma_vm']) {
    get('range-' + q).value = '1'; get('range-' + q).oninput();
    assert.equal(get('readout-' + q).children[0].hidden, true);
    assert.equal(get('readout-' + q).children[1].hidden, false);
    circles[q][0].onfocus();
  }
  context.window.showDirectoryPicker = undefined;
  await get('comment-folder').onclick();
  assert.match(get('comment-status').textContent, /unavailable/);
  console.log('Comment persistence, mismatch, conflict, export and chart controls passed.');
''')


@pytest.mark.parametrize("source", ["import", "folder", "save"])
def test_conflicting_new_ids_are_rejected_atomically(run_interaction, source):
    run_interaction("const source = " + json.dumps(source) + ";\n" + r'''
  get('comment-text').value = 'Unsaved question.'; get('comment-add').onclick();
  await get('comment-folder').onclick();
  const rows = [
    {id:'unique', section:'section-1', text:'Must not be partially merged.'},
    {id:'duplicate', section:'section-1', text:'First finding.'},
    {id:'duplicate', section:'section-1', text:'Different finding.'}
  ];
  if (source === 'import') {
    const text = JSON.stringify({...config, comments:rows});
    get('comment-load').files = [{size:Buffer.byteLength(text), text:async () => text}];
    await get('comment-load').onchange();
  } else {
    onDisk = rows;
    await get(source === 'folder' ? 'comment-folder' : 'comment-save').onclick();
  }
  assert.match(get('comment-status').textContent, /Conflicting comment identity/);
  assert.equal(written, null);
  get('comment-export').onclick();
  assert.deepEqual(JSON.parse(exported).comments.map(row => row.text), ['Unsaved question.']);
  // Identical repeats retain one record and remain reloadable.
  onDisk = null;
  const text = JSON.stringify({...config, comments:[rows[1], rows[1]]});
  get('comment-load').files = [{size:Buffer.byteLength(text), text:async () => text}];
  await get('comment-load').onchange();
  get('comment-export').onclick();
  assert.equal(JSON.parse(exported).comments.length, 2);
''')


@pytest.mark.parametrize("byte_count", [4999999, 5000000, 5000001])
def test_utf8_payload_limit_and_save_reload_boundary(run_interaction, byte_count):
    run_interaction("const byteCount = " + str(byte_count) + ";\n" + r'''
  const rows = Array.from({length:500}, (_, i) => ({
    id:'size-' + i, section:'section-1', text:'é'.repeat(4900)
  }));
  const serialized = () => JSON.stringify({...config, comments:rows}, null, 2) + '\n';
  let remaining = byteCount - Buffer.byteLength(serialized(), 'utf8');
  for (const row of rows) {
    const count = Math.min(10000 - row.text.length, remaining);
    row.text += 'x'.repeat(count); remaining -= count;
  }
  assert.equal(remaining, 0);
  assert.equal(Buffer.byteLength(serialized(), 'utf8'), byteCount);
  assert.ok(serialized().length < 5000000); // UTF-16 length would miss the overflow.
  onDisk = rows.slice(0, 250);
  await get('comment-folder').onclick();
  const text = JSON.stringify({...config, comments:rows.slice(250)});
  get('comment-load').files = [{size:Buffer.byteLength(text), text:async () => text}];
  await get('comment-load').onchange();
  assert.equal(get('comment-list').children.length, 500);
  const retained = JSON.stringify(onDisk);
  await get('comment-save').onclick();
  if (byteCount > 5000000) {
    assert.equal(written, null);
    assert.equal(writesOpened, 0);
    assert.equal(JSON.stringify(onDisk), retained);
    assert.match(get('comment-status').textContent, /size limit/);
    get('comment-export').onclick();
    assert.equal(exported, null);
    assert.match(get('comment-status').textContent, /size limit/);
    assert.equal(get('comment-list').children.length, 500);
    assert.ok(get('comment-list').children[499].textContent.includes(rows[499].text));
  } else {
    assert.deepEqual(written.comments, rows);
    assert.equal(Buffer.byteLength(writtenText, 'utf8'), byteCount);
    get('comment-export').onclick();
    assert.equal(Buffer.byteLength(exported, 'utf8'), byteCount);
    assert.deepEqual(JSON.parse(exported).comments, rows);
    vm.runInNewContext(SCRIPT, context); // Fresh page state reloads the saved records.
    await get('comment-folder').onclick();
    assert.match(get('comment-status').textContent, /folder selected/);
    get('comment-export').onclick();
    assert.deepEqual(JSON.parse(exported).comments, rows);
  }
''')


@pytest.mark.parametrize("record_count", [10000, 10001])
def test_merged_record_limit_preserves_unsaved_comments(run_interaction, record_count):
    run_interaction("const recordCount = " + str(record_count) + ";\n" + r'''
  const rows = Array.from({length:recordCount}, (_, i) => ({
    id:'count-' + i, section:'section-1', text:'Question ' + i
  }));
  onDisk = rows.slice(0, 5000);
  await get('comment-folder').onclick();
  const text = JSON.stringify({...config, comments:rows.slice(5000)});
  get('comment-load').files = [{size:Buffer.byteLength(text), text:async () => text}];
  await get('comment-load').onchange();
  await get('comment-save').onclick();
  if (recordCount > 10000) {
    assert.equal(written, null);
    assert.equal(writesOpened, 0);
    assert.equal(onDisk.length, 5000);
    assert.match(get('comment-status').textContent, /limit/);
    get('comment-export').onclick();
    assert.equal(exported, null);
    assert.match(get('comment-status').textContent, /limit/);
    assert.equal(get('comment-list').children.length, 10001);
    assert.match(get('comment-list').children[10000].textContent, /Question 10000/);
  } else {
    assert.deepEqual(written.comments, rows);
    get('comment-export').onclick();
    assert.deepEqual(JSON.parse(exported).comments, rows);
    vm.runInNewContext(SCRIPT, context);
    await get('comment-folder').onclick();
    assert.match(get('comment-status').textContent, /folder selected/);
    get('comment-export').onclick();
    assert.deepEqual(JSON.parse(exported).comments, rows);
  }
''')
