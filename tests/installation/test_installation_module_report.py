# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the installation report and bind its displayed results to the API."""

import importlib.util
import json
import math
import re
import shutil
import subprocess
from html.parser import HTMLParser
from pathlib import Path

import pytest

from digitalmodel.installation.installation_pamphlet import calculations as calc

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/installation"


@pytest.fixture(scope="module")
def writer():
    path = ROOT / "scripts/reporting/build_installation_module_report.py"
    spec = importlib.util.spec_from_file_location("installation_report", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class Page(HTMLParser):
    """Read numerical result bindings and structural evidence from built HTML."""

    def __init__(self, html):
        super().__init__(convert_charrefs=True)
        self.ids = []
        self.bindings = []
        self.links = []
        self.headings = []
        self.stack = []
        self.visible = []
        self.feed(html)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag in {"script", "style"}:
            self.stack.append(tag)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if "data-result" in attrs:
            self.bindings.append((attrs["data-result"], attrs["data-value"]))
        if tag == "a":
            self.links.append(attrs.get("href", ""))
        if tag in {"h1", "h2", "h3", "h4", "h5", "h6"}:
            self.headings.append(tag)

    def handle_endtag(self, tag):
        if self.stack and self.stack[-1] == tag:
            self.stack.pop()

    def handle_data(self, data):
        if not self.stack:
            self.visible.append(data)


def embedded(html):
    match = re.search(r'<script id="report-data" type="application/json">(.*?)</script>', html, re.S)
    return json.loads(match.group(1))


def test_rebuild_is_exact_and_preserves_comments(writer, tmp_path):
    comments = tmp_path / "report.comments.json"
    comments.write_text('{"comments": ["Retain this engineering query"]}\n')
    before = comments.read_bytes()
    writer.build(tmp_path)
    assert (tmp_path / "index.html").read_bytes() == (REPORT / "index.html").read_bytes()
    assert (tmp_path / "changes.json").read_bytes() == (REPORT / "changes.json").read_bytes()
    assert comments.read_bytes() == before


def test_every_catalog_size_and_period_equals_direct_module_call(writer):
    data = embedded(writer.render())
    catalog = calc.load_mudmats(ROOT / writer.CATALOG)
    assert len(data["cases"]) == len(catalog)
    for item in data["cases"]:
        source = catalog[item["catalog_id"]]
        expected = calc.compute_splashzone_envelope(source, calc.DEFAULT_TP_GRID_S)
        assert item["module"] == expected
        assert item["input"] == source
        assert data["periods_s"] == calc.DEFAULT_TP_GRID_S


def test_all_visible_worked_results_are_bound_to_api_values(writer):
    html = writer.render()
    data = embedded(html)
    page = Page(html)
    expected = writer.result_values(data)
    assert page.bindings
    assert {key for key, value in page.bindings} == set(expected)
    for key, value in page.bindings:
        assert float(value) == expected[key]
        assert f'>{expected[key]:.2f}</span>' in html
    worked = re.search(r'<section id="results">(.*?)</section>', html, re.S).group(1)
    unbound = re.sub(r'<span data-result=.*?</span>', '', worked)
    unbound = re.sub(r'<h2>.*?</h2>|<[^>]+>', '', unbound)
    assert not re.search(r'\d', unbound), "Worked-result numbers require API bindings"


def test_quadratic_root_has_independent_force_balance(writer):
    data = embedded(writer.render())
    for case in data["cases"]:
        structure = case["input"]
        hc = structure["hydrodynamic_coefficients"]
        area = structure["projected_areas"]["bottom_m2"]
        volume = structure["mass_properties"]["displaced_volume_m3"]
        for tp in data["periods_s"]:
            key = f"{tp:.1f}"
            height = case["module"]["hs_by_tp"][key]
            target = structure["mass_properties"]["submerged_weight_kn"] * 1000 / case["module"]["daf"]

            def forces(hs):
                speed = math.pi * hs / tp + case["module"]["v_lowering_ms"]
                slam = .5 * calc.SEAWATER_DENSITY * hc["slamming_coefficient_Cs"] * area * speed**2
                drag = .5 * calc.SEAWATER_DENSITY * hc["drag_coefficient_Cd"] * area * speed**2
                inertia = calc.SEAWATER_DENSITY * (1 + hc["added_mass_coefficient_Ca"]) * volume * 2 * math.pi**2 * hs / tp**2
                return {"slam": slam, "drag": drag, "inertia": inertia}

            # The API rounds Hs to two decimals; bracket the unrounded root.
            assert sum(forces(height - .00501).values()) <= target
            assert sum(forces(height + .00501).values()) >= target
            assert max(forces(height), key=forces(height).get) == case["module"]["gov_by_tp"][key]


def test_stricter_margin_is_an_explicit_module_call(writer):
    data = writer.calculate(snap_daf=calc.SPLASH_SNAP_DAF * 2)
    baseline = writer.calculate()
    for case, original in zip(data["cases"], baseline["cases"]):
        direct = calc.compute_splashzone_envelope(case["input"], data["periods_s"], snap_daf=data["snap_daf"])
        assert case["module"] == direct
        assert all(v <= original["module"]["hs_by_tp"][k] for k, v in direct["hs_by_tp"].items())


@pytest.mark.parametrize("margin", [0, -1, float("nan"), float("inf"), .5, 1])
def test_unphysical_report_margin_rejected(writer, margin):
    with pytest.raises(ValueError, match="margin"):
        writer.calculate(snap_daf=margin)


def test_ten_sections_sources_and_house_style(writer):
    html = writer.render()
    page = Page(html)
    assert page.headings.count("h2") == 10
    assert not set(page.headings) & {"h4", "h5", "h6"}
    assert len(page.ids) == len(set(page.ids))
    assert all(link[1:] in page.ids for link in page.links if link.startswith("#"))
    assert '<body class="hs-doc">' in html
    assert '<link rel="stylesheet"' not in html
    assert '<script src=' not in html
    assert writer.house_css() in html
    assert "NOT_EVALUATED" in html
    assert "Draft for client review" in html


def test_public_visible_text_and_sidecar_controls(writer):
    html = writer.render()
    visible = ' '.join(Page(html).visible)
    visible = visible.replace("ASSUMED - to be confirmed", "assumed input")
    assert not re.search(r"\b(could|should|would|validated|certified|compliant|confirmed)\b", visible, re.I)
    assert "showDirectoryPicker" in html
    assert "report.comments.json" in html
    assert "textContent" in html
    assert "innerHTML = comment" not in html
    assert "changes.json" in html
    assert not re.search(r"https?://|fetch\(", html), "Report must work offline"


def test_change_panel_matches_sidecar(writer):
    html = writer.render()
    assert embedded(html)["changes"] == json.loads((REPORT / "changes.json").read_text())


def test_interactive_charts_and_comment_save_with_dom_double(writer):
    """Exercise the shipped inline code without starting a browser."""
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable; browser-independent interface check cannot start")
    harness = r'''
const fs = require('fs'), vm = require('vm');
const input = JSON.parse(fs.readFileSync(0, 'utf8'));
class Element {
  constructor(tag) { this.tag=tag;this.children=[];this.attrs={};this.listeners={};this.textContent='';this.value=''; }
  append(...nodes) { this.children.push(...nodes); }
  replaceChildren(...nodes) { this.children=nodes; }
  setAttribute(key,value) { this.attrs[key]=value; }
  addEventListener(key,fn) { this.listeners[key]=fn; }
  querySelector() { return {textContent:this.id}; }
}
const elements = new Map();
const el = id => {if(!elements.has(id)){const e=new Element('div');e.id=id;elements.set(id,e);}return elements.get(id);};
el('report-data').textContent=JSON.stringify(input.data);
el('case-select').value='0';el('period-select').value=String(input.data.periods_s[0]);
const sections=input.sections.map(id=>{const e=new Element('section');e.id=id;return e;});
const document={getElementById:el,createElementNS:(ns,tag)=>new Element(tag),createElement:tag=>new Element(tag),querySelectorAll:()=>sections};
let saved=null;
const handle={createWritable:async()=>({write:async text=>{saved=text;},close:async()=>{}}),getFile:async()=>({text:async()=>saved})};
const folder={getFileHandle:async(name,opts)=>{if(saved===null&&!opts){const e=new Error('absent');e.name='NotFoundError';throw e;}return handle;}};
const window={showDirectoryPicker:async()=>folder};
vm.runInNewContext(input.script,{document,window,console,URL,Blob,setTimeout});
const titles = root => {
  const out=[];
  function walk(e){if(e.tag==='circle')out.push(e.children.find(c=>c.tag==='title').textContent);e.children.forEach(walk);}
  walk(root);return out;
};
const initial=titles(el('component-chart'));
el('case-select').value='2';el('period-select').value=String(input.data.periods_s.at(-1));
el('case-select').listeners.change();
const changed=titles(el('component-chart'));
(async()=>{
  el('comment-section').value=input.sections[0];
  el('comment-text').value='<img src=x>Engineering query';el('comment-add').onclick();
  const visible=el('comment-list').textContent;
  await el('comment-folder').onclick();
  const firstSaved=saved, savedStatus=el('comment-status').textContent;
  await el('comment-folder').onclick();
  const overwriteStatus=el('comment-status').textContent;
  const invalid=JSON.parse(saved);invalid.identity='different';
  el('comment-load').files=[{size:100,text:async()=>JSON.stringify(invalid)}];
  await el('comment-load').onchange();
  console.log(JSON.stringify({initial,changed,visible,savedStatus,overwriteStatus,
    loadStatus:el('comment-status').textContent,preserved:saved===firstSaved,saved:JSON.parse(saved)}));
})().catch(error=>{console.error(error);process.exitCode=1;});
'''
    html = writer.render()
    data = embedded(html)
    sections = re.findall(r'<section id="([^"]+)">', html)
    result = subprocess.run(
        [node, "-e", harness],
        input=json.dumps({"script": writer.INTERACTIVE, "data": data, "sections": sections}),
        text=True, capture_output=True, check=True, timeout=30,
    )
    state = json.loads(result.stdout)
    for titles, case, period in (
        (state["initial"], data["cases"][0], data["periods_s"][0]),
        (state["changed"], data["cases"][-1], data["periods_s"][-1]),
    ):
        assert titles
        for title in titles:
            assert case["label"] in title
            assert f"Tp {period:.1f} s" in title
            assert not re.search(r"NaN|undefined|Infinity", title)
            assert "installation NOT_EVALUATED" in title
            assert f'limit {case["module"]["submerged_weight_kn"] / case["module"]["daf"]:.1f} kN' in title
            hs = float(re.search(r"Hs ([\d.]+) m", title).group(1))
            component, demand = re.search(r"m; (\w+); demand ([\d.]+) kN", title).groups()
            source = case["input"]
            hc = source["hydrodynamic_coefficients"]
            speed = math.pi * hs / period + case["module"]["v_lowering_ms"]
            area = source["projected_areas"]["bottom_m2"]
            slam = .5 * calc.SEAWATER_DENSITY * hc["slamming_coefficient_Cs"] * area * speed**2 / 1000
            drag = .5 * calc.SEAWATER_DENSITY * hc["drag_coefficient_Cd"] * area * speed**2 / 1000
            inertia = calc.SEAWATER_DENSITY * (1 + hc["added_mass_coefficient_Ca"]) * source["mass_properties"]["displaced_volume_m3"] * 2 * math.pi**2 * hs / period**2 / 1000
            expected = {"Slamming": slam, "Drag": drag, "Inertia": inertia, "Total": slam + drag + inertia}[component]
            assert float(demand) == float(f"{expected:.1f}")
    assert "<img src=x>Engineering query" in state["visible"]
    assert "saved beside the report and read back" in state["savedStatus"]
    assert "already exists" in state["overwriteStatus"]
    assert "do not match" in state["loadStatus"]
    assert state["preserved"]
    assert len(state["saved"]["comments"]) == 1
