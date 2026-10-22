# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand anchors, consistency checks and portable report rebuild checks."""

import importlib.util
import json
import re
import sys
from html.parser import HTMLParser
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
REPORT = ROOT / "docs/reports/modules/power"
REBUILD = "PYTHONPATH=src uv run --no-project --with numpy python docs/reports/modules/power/build_report.py"
spec = importlib.util.spec_from_file_location("power_report", REPORT / "build_report.py")
builder = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = builder
spec.loader.exec_module(builder)


class Numbers(HTMLParser):
    """Collect displayed numerical bindings rather than CPU-specific float text."""

    def __init__(self):
        super().__init__()
        self.values = []
        self.active = None

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "span" and "data-value" in attrs:
            self.active = (attrs["data-value"], int(attrs["data-digits"]))

    def handle_data(self, data):
        if self.active:
            self.values.append((*self.active, data))

    def handle_endtag(self, tag):
        if tag == "span":
            self.active = None


def test_hand_anchors():
    result = builder.hand_case()
    # V1=1+j0, I1=(1-(.96+j.28))/(j*.5)=-.56-j.08;
    # S1=V1*conj(I1)=-.56+j.08, hence Q1=.08 pu. Absolute tol 1e-8 pu.
    assert result.bus_results[0]["q_net_pu"] == pytest.approx(0.08, rel=0, abs=1e-8)
    # I2=2.16-j.16; S2=(.96+j.28)*(2.16+j.16);
    # Q2=.96*.16+.28*2.16=.1536+.6048=.7584 pu. Absolute tol 1e-8 pu.
    assert result.bus_results[1]["q_net_pu"] == pytest.approx(0.7584, rel=0, abs=1e-8)
    # |V3|=sqrt(.90**2+(-.12)**2)=sqrt(.8244)=.9079647570 pu;
    # the literal is rounded to ten decimals; absolute tolerance 1e-8 pu.
    assert result.bus_results[2]["v_pu"] == pytest.approx(0.9079647570, rel=0, abs=1e-8)


def test_public_case_mapping_and_headline():
    case = builder.read_case()
    net = builder.public_network(case)
    # Bus 4 generation minus demand: (318-80)/100=238/100=2.38 pu.
    assert net.buses[3].p_gen_pu - net.buses[3].p_load_pu == pytest.approx(2.38)
    assert net.buses[3].voltage_pu == 1.02  # generator setpoint overrides bus initial guess
    assert all(branch.tap_ratio == 1 for branch in net.branches)
    record = builder.calculate()
    # Headline Qg is recomputed from displayed bus injection and the public load/base.
    q_net = float(builder.format_number(record["public.bus.3.q_net_pu"], 6))
    qg = q_net * case["base_mva"] + case["buses"][3]["q_mvar"]
    assert qg == pytest.approx(record["public.qg4_mvar"], rel=0, abs=0.000051)
    q_reference = float(builder.format_number(record["public.bus.0.q_net_pu"], 6))
    assert q_reference * case["base_mva"] + case["buses"][0]["q_mvar"] == pytest.approx(
        record["public.qg1_mvar"], rel=0, abs=0.000051,
    )
    assert record["public.qg4_mvar"] > case["generators"][0]["q_max_mvar"]
    assert record["public.qg1_mvar"] > case["generators"][1]["q_max_mvar"]
    assert record["public.converged"] is True


def test_displayed_number_consistency():
    """Consistency check: every numerical binding equals a fresh module call."""
    record = builder.calculate()
    parser = Numbers()
    parser.feed((REPORT / "report.html").read_text(encoding="utf-8"))
    assert len(parser.values) > 60
    for key, digits, displayed in parser.values:
        assert displayed == builder.format_number(record[key], digits), f"{key}: {REBUILD}"


def test_rebuild(tmp_path):
    destination = tmp_path / "report.html"
    builder.build(destination)
    committed = (REPORT / "report.html").read_text(encoding="utf-8")
    rebuilt = destination.read_text(encoding="utf-8")
    # Output numbers are rounded to displayed precision before embedding.
    assert committed == rebuilt, f"Report is stale. Rebuild with: {REBUILD}"
    assert b"\r" not in destination.read_bytes()
    changes = json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    assert changes["revision"] == "1"


def test_report_contract():
    html = (REPORT / "report.html").read_text(encoding="utf-8")
    assert '<body class="hs-doc">' in html
    assert len(re.findall(r'<section id="s\d+">', html)) == 10
    assert "Draft for maintainer review" in html
    assert "showSaveFilePicker" in html and "power.comments.json" in html
    assert 'id="changes-data"' in html
    assert "data-case" in html and "<title>" in html
    assert not re.search(r'<(?:script|link)[^>]+(?:src|href)="https?://', html)
    for ref in re.findall(r'href="#(ref-[^"]+)"', html):
        assert f'id="{ref}"' in html
    ids = re.findall(r'\bid="([^"]+)"', html)
    assert len(ids) == len(set(ids))
    embedded = json.loads(re.search(
        r'<script id="calculation-data" type="application/json">(.*?)</script>', html, re.S,
    ).group(1))
    assert embedded["tol"] == builder.TOL


def test_offline_interactions():
    """Exercise chart, change panel and sidecar save/load without a browser."""
    import shutil
    import subprocess

    node = shutil.which("node")
    if not node:
        pytest.skip("Node is unavailable; offline interaction check requires Node.")
    html = (REPORT / "report.html").read_text(encoding="utf-8")
    script = re.findall(r"<script>(.*?)</script>", html, re.S)[0]
    changes = re.search(r'<script id="changes-data" type="application/json">(.*?)</script>', html, re.S).group(1)
    harness = r'''
const vm = require('node:vm');
const assert = require('node:assert/strict');
let saved = null;
function start() {
  const nodes = {};
  function element() {
    return {events: {}, children: [], value: '', textContent: '', style: {},
      addEventListener(event, callback) { this.events[event] = callback; },
      replaceChildren() { this.children = []; },
      append(child) { this.children.push(child); }};
  }
  const get = id => nodes[id] ||= element();
  const point = element(); point.dataset = {case: 'Case; bus; demand; limit; pu'};
  const limit = element();
  get('changes-data').textContent = CHANGES;
  get('prior-revision').value = '0';
  const document = {getElementById: get, createElement: element,
    querySelectorAll(selector) { return selector === '[data-case]' ? [point] : [limit]; }};
  const window = {showDirectoryPicker: async () => ({
    getFileHandle: async name => {
      assert.equal(name, 'power.comments.json');
      return {createWritable: async () => ({write: async text => { saved = text; }, close: async () => {}})};
    }})};
  vm.runInNewContext(SCRIPT, {document, window, setTimeout});
  return {get, point, limit};
}
(async () => {
  const ui = start();
  ui.point.events.focus();
  assert.equal(ui.get('chart-detail').textContent, ui.point.dataset.case);
  ui.get('limits').events.click(); assert.equal(ui.limit.style.display, 'none');
  ui.get('limits').events.click(); assert.equal(ui.limit.style.display, '');
  assert.equal(ui.get('change-list').children.length, 3);
  ui.get('prior-revision').value = '1'; ui.get('prior-revision').events.change();
  assert.equal(ui.get('change-list').children[0].textContent, 'No later engineering changes.');
  ui.get('comment-section').value = 's5'; ui.get('comment-text').value = '<example> Review reactive limit.';
  ui.get('add-comment').events.click();
  await ui.get('save-comments').events.click();
  assert.ok(saved.endsWith(String.fromCharCode(10)));
  const parsed = JSON.parse(saved);
  assert.equal(parsed.comments[0].section, 's5');
  assert.equal(parsed.comments[0].text, '<example> Review reactive limit.');
  const other = start();
  await other.get('load-comments').events.change({target: {files: [{text: async () => saved}]}});
  assert.equal(other.get('comment-list').children[0].textContent, 's5: <example> Review reactive limit.');
  // A wrong revision is rejected before importing comments.
  const wrong = start(); parsed.revision = '99';
  await wrong.get('load-comments').events.change({target: {files: [{text: async () => JSON.stringify(parsed)}]}});
  assert.equal(wrong.get('comment-list').children.length, 0);
  assert.equal(wrong.get('comment-status').textContent, 'Invalid sidecar or revision.');
})().catch(error => { console.error(error.message); process.exitCode = 1; });
'''
    command = "const SCRIPT=" + json.dumps(script) + ";const CHANGES=" + json.dumps(changes) + ";" + harness
    result = subprocess.run([node, "-e", command], capture_output=True, text=True, encoding="utf-8", check=False)
    assert result.returncode == 0, result.stderr
