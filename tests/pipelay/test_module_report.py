# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check rebuilt client text, numerical bindings and standalone report controls."""

import copy
import importlib.util
import json
import re
import shutil
import subprocess
from html.parser import HTMLParser
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/pipelay"


@pytest.fixture
def builder():
    spec = importlib.util.spec_from_file_location("pipelay_module_report", REPORT / "build_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class Page(HTMLParser):
    """Inspect the built artifact rather than its template alone."""

    def __init__(self):
        super().__init__()
        self.ids = []
        self.links = []
        self.headings = []
        self.bindings = []
        self.unbound_worked_numbers = []
        self.hidden = 0
        self.bound = False
        self.worked = False
        self.active = None
        self.resources = []
        self.visible = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "a":
            self.links.append(attrs.get("href", ""))
        if tag == "h2":
            self.headings.append(attrs.get("id"))
        if tag == "section" and attrs.get("id") == "worked-example":
            self.worked = True
        if tag in {"script", "style"}:
            self.hidden += 1
        if "data-value" in attrs:
            self.bound = True
            self.active = [attrs, ""]
        if tag in {"script", "link", "img", "iframe"}:
            self.resources.extend(attrs[k] for k in ("src", "href") if k in attrs)
        self.visible.extend(attrs[key] for key in ("title", "aria-label", "alt", "placeholder") if key in attrs)

    def handle_endtag(self, tag):
        if tag in {"span", "tspan"} and self.bound:
            self.bindings.append(self.active)
            self.active = None
            self.bound = False
        if tag in {"script", "style"}:
            self.hidden -= 1
        if tag == "section":
            self.worked = False

    def handle_data(self, data):
        if self.active:
            self.active[1] += data
        if not self.hidden:
            self.visible.append(data)
            if self.worked and not self.bound and re.search(r"\d", data):
                self.unbound_worked_numbers.append(data)


def test_report_rebuilds_exactly(builder, tmp_path):
    builder.build(tmp_path / "report", tmp_path / "calculations")
    for name in ("report.html", "changes.json"):
        assert (tmp_path / "report" / name).read_bytes() == (REPORT / name).read_bytes()


def test_report_deliverables_are_unignored():
    for name in ("build_report.py", "report.html", "changes.json"):
        result = subprocess.run(["git", "check-ignore", "-q", str(REPORT / name)],
                                cwd=ROOT, capture_output=True)
        assert result.returncode == 1, name
    # Review comments remain local and are not offered for publication.
    result = subprocess.run(["git", "check-ignore", "-q", str(REPORT / "report.comments.json")],
                            cwd=ROOT, capture_output=True)
    assert result.returncode == 0


def test_all_worked_numbers_equal_fresh_module_returns(builder, tmp_path):
    from digitalmodel.pipelay.workflow import router

    record = builder.calculate(tmp_path)
    page = Page()
    page.feed(builder.render(record))
    assert not page.unbound_worked_numbers
    assert len(page.bindings) >= 25
    # A separate invocation verifies the report record against the public router.
    for case in ("jlay", "reel"):
        cfg = builder.load_example(case)
        cfg["pipelay"]["outputs"]["directory"] = str(tmp_path / "independent" / case)
        returned = router(cfg)["pipelay"]
        for block in ("pipe", "config"):
            assert record[case]["inputs"][block] == returned[block]
        for key, value in record[case]["result"].items():
            assert value == returned[key]
    for attrs, text in page.bindings:
        value = builder.lookup(record, attrs["data-value"])
        expected = builder.format_value(value, float(attrs["data-scale"]))
        assert text == expected


def test_rebuild_responds_to_module_result_changes(builder, tmp_path, monkeypatch):
    from digitalmodel.pipelay import workflow

    original = workflow.run_jlay

    def changed(settings):
        result = original(settings)
        result["sagbend"]["horizontal_offset_m"] += 10
        return result

    before = builder.render(builder.calculate(tmp_path / "before"))
    monkeypatch.setattr(workflow, "run_jlay", changed)
    after = builder.render(builder.calculate(tmp_path / "after"))
    assert before != after
    assert "311.7" in after
    assert "301.7" not in after


def test_charts_are_module_samples_and_show_limits(builder, tmp_path):
    record = builder.calculate(tmp_path)
    html = builder.render(record)
    embedded = json.loads(re.search(r'<script id="calculation-record" type="application/json">(.*?)</script>', html, re.S)[1])
    assert embedded == record
    for case in ("jlay", "reel"):
        op = record[case]["result"]["operability"]
        assert f'data-limit="{op["operating_limit_m"]}"' in html
        assert len(op["lay_rate_table"]) > 1
        assert all(row["lay_rate_km_per_day"] == 0 for row in op["lay_rate_table"]
                   if row["hs_m"] > op["operating_limit_m"])
    assert 'aria-live="polite"' in html
    assert 'tabindex="0"' in html
    assert 'class="limit"' in html


def test_house_style_sections_links_and_public_text(builder, tmp_path):
    page_text = builder.render(builder.calculate(tmp_path))
    page = Page()
    page.feed(page_text)
    assert '<body class="hs-doc">' in page_text
    assert builder.house_style() in page_text
    assert not page.resources
    assert len(page.headings) == 10
    assert len(page.ids) == len(set(page.ids))
    assert all(link[1:] in page.ids for link in page.links if link.startswith("#"))
    for link in page.links:
        if link and not link.startswith("#"):
            assert (REPORT / link.split("#")[0]).resolve().exists(), link
    visible = " ".join(page.visible)
    assert "Draft for client review" in visible
    assert "ASSUMED - to be confirmed" in visible
    assert "NOT_EVALUATED" in visible
    assert not re.search(r"\b(could|should|would|validated|certified|compliant)\b", visible, re.I)
    assert "<b>" not in page_text and "<strong>" not in page_text
    assert page_text.index('id="revision-history"') < page_text.index('id="introduction"')


def test_comment_and_change_controls_are_revision_bound(builder, tmp_path):
    page = builder.render(builder.calculate(tmp_path))
    assert "showSaveFilePicker" in page
    assert "report.comments.json" in page
    assert "textContent" in page
    assert "innerHTML" not in builder.REVIEW_SCRIPT
    assert "report_digest" in page
    assert "crypto.subtle.digest" in page
    assert "showDirectoryPicker" in page
    assert "createWritable" in page
    changes = builder.changes()
    assert changes["revision"] == builder.REVISION
    assert changes["changes"]
    assert json.dumps(changes, ensure_ascii=False) in page


def test_no_runtime_paths_or_nonfinite_values_are_embedded(builder, tmp_path):
    record = builder.calculate(tmp_path)
    text = builder.render(record)
    assert str(tmp_path) not in text
    assert "summary_json" not in record["jlay"]["result"]
    altered = copy.deepcopy(record)
    altered["jlay"]["result"]["top_tension_kN"] = float("nan")
    with pytest.raises(ValueError):
        builder.render(altered)


def test_comment_save_load_and_digest_checks_without_browser(builder, tmp_path):
    """Exercise persistence APIs with a small DOM substitute and no browser."""
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable for the standalone comment-control check")
    page = builder.render(builder.calculate(tmp_path))
    harness = r'''
const assert = require('node:assert/strict');
const vm = require('node:vm');
const crypto = require('node:crypto').webcrypto;
const input = JSON.parse(require('node:fs').readFileSync(0, 'utf8'));
const jsonBlock = id => JSON.parse(input.page.match(new RegExp(
  '<script id="' + id + '" type="application/json">([\\s\\S]*?)</script>'))[1]);
const config = jsonBlock('review-config');
const nodes = new Map();
function element() {
  return {value: '', textContent: '', children: [],
    appendChild(child) { this.children.push(child); if (!this.value) this.value = child.value; },
    replaceChildren() { this.children = []; }};
}
function get(id) { if (!nodes.has(id)) nodes.set(id, element()); return nodes.get(id); }
get('review-config').textContent = JSON.stringify(config);
get('calculation-record').textContent = JSON.stringify(jsonBlock('calculation-record'));
const sections = [...input.page.matchAll(/<section id="([^"]+)"><h2>([^<]+)<\/h2>/g)]
  .map(match => ({id: match[1], querySelector: () => ({textContent: match[2]})}));
let saved = '';
let selectedPage = input.page;
let writes = 0;
let createCalls = 0;
const savedHandle = {
  getFile: async () => ({size: saved.length, text: async () => saved}),
  createWritable: async () => ({write: async data => { saved = data; writes++; }, close: async () => {}})
};
const folder = {getFileHandle: async (name, options) => {
  if (name === 'report.html') return {getFile: async () => ({text: async () => selectedPage})};
  assert.equal(name, 'report.comments.json'); assert.deepEqual(JSON.parse(JSON.stringify(options)), {create: true});
  createCalls++; return savedHandle;
}};
const document = {getElementById: get, createElement: element,
  querySelectorAll: selector => selector === 'main > section' ? sections : []};
const window = {showDirectoryPicker: async () => folder};
vm.runInNewContext(input.script, {document, window, crypto, TextEncoder, Blob, URL, setTimeout});
(async () => {
  assert.equal(sections.length, 10);
  get('comment-text').value = 'Confirm the supplied coating density.';
  get('add-comment').onclick();
  await get('save-comments').onclick();
  assert.equal(writes, 1);
  const first = JSON.parse(saved);
  assert.equal(first.comments.length, 1);
  assert.equal(first.report_digest, config.report_digest);
  // Existing compatible comments are retained on save without an explicit import.
  saved = JSON.stringify({...first, comments: [...first.comments,
    {section: 'design-data', text: 'Confirm the empty-bore assumption.'}]});
  get('comment-text').value = 'Confirm the strain criterion.';
  get('add-comment').onclick();
  await get('save-comments').onclick();
  assert.equal(JSON.parse(saved).comments.length, 3);
  // A changed report is rejected before the sidecar is opened for writing.
  selectedPage = input.page.replace('Pipelay module analysis', 'Changed report');
  const callsBefore = createCalls;
  await get('save-comments').onclick();
  assert.equal(createCalls, callsBefore);
  assert.match(get('comment-status').textContent, /different report/);
  selectedPage = input.page;
  // An incompatible existing sidecar is preserved.
  saved = JSON.stringify({...first, revision: 'different'});
  const retained = saved;
  const writesBefore = writes;
  await get('save-comments').onclick();
  assert.equal(saved, retained); assert.equal(writes, writesBefore);
  assert.match(get('comment-status').textContent, /do not match/);
  // A mismatched import changes neither the existing comments nor their display.
  const countBefore = get('comment-list').children.length;
  await get('load-comments').onchange({target: {files: [{size: retained.length,
    text: async () => retained}], value: 'selected'}});
  assert.equal(get('comment-list').children.length, countBefore);
  assert.match(get('comment-status').textContent, /do not match/);
  // The compatible import is de-duplicated with comments already held locally.
  await get('load-comments').onchange({target: {files: [{size: 1,
    text: async () => JSON.stringify(first)}], value: 'selected'}});
  assert.equal(get('comment-list').children.length, countBefore);
  console.log('Comment persistence and report binding passed.');
})().catch(error => { console.error(error); process.exitCode = 1; });
'''
    result = subprocess.run([node, "-e", harness], input=json.dumps({"page": page, "script": builder.REVIEW_SCRIPT}),
                            text=True, capture_output=True, timeout=30)
    assert result.returncode == 0, result.stderr
    assert "Comment persistence and report binding passed." in result.stdout
