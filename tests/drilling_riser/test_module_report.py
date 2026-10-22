# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the public report and reconcile its engineering values to the module."""

import builtins
import csv
import importlib.util
import json
import os
import re
import shutil
import subprocess
from decimal import Decimal
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

from digitalmodel.drilling_riser import adapter, stackup
from digitalmodel.drilling_riser.workflow import router

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/drillingriser"


class Document(HTMLParser):
    def __init__(self, html):
        super().__init__()
        self.metrics = []
        self.headings = []
        self.external = []
        self.ids = []
        self.links = []
        self.active_metric = None
        self.feed(html)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "data-metric" in attrs:
            self.metrics.append(attrs)
            attrs["text"] = ""
            self.active_metric = attrs
        if tag == "h2":
            self.headings.append(attrs["id"])
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag in ("script", "link", "img"):
            self.external.extend(attrs[k] for k in ("src", "href") if k in attrs)
        if tag == "a" and attrs.get("href", "").startswith("#"):
            self.links.append(attrs["href"][1:])

    def handle_data(self, text):
        if self.active_metric is not None:
            self.active_metric["text"] += text

    def handle_endtag(self, tag):
        if tag == "span":
            self.active_metric = None


@pytest.fixture()
def builder(monkeypatch):
    original = builtins.__import__

    def guard(name, *args, **kwargs):
        if name.split(".")[0] == "OrcFxAPI":
            raise AssertionError("Licensed interface is outside the report basis")
        return original(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", guard)
    spec = importlib.util.spec_from_file_location("drilling_module_report", REPORT / "build_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def direct_results(tmp_path):
    cfg = yaml.safe_load((ROOT / "examples/workflows/riser-stackup/input.yml").read_text())
    inputs = dict(cfg["riser_stackup"])
    cfg["_config_dir_path"] = str(tmp_path)
    summary = router(cfg)["riser_stackup"]
    with Path(summary["profile_csv"]).open() as stream:
        profile = [{k: float(v) for k, v in row.items()} for row in csv.DictReader(stream)]
    return inputs, summary, profile


def test_report_values_equal_public_module_and_independent_equilibrium(builder, tmp_path):
    data = builder.compute(tmp_path / "report")
    inputs, summary, profile = direct_results(tmp_path / "direct")
    assert data["inputs"] == inputs
    for key in ("top_tension_required_kn", "wall_thickness_required_mm", "min_slip_ring_tension_kn", "points"):
        assert data["summary"][key] == summary[key]
    assert data["profile"] == profile
    assert summary["top_tension_required_kn"] == stackup.top_tension_required(
        inputs["submerged_weight_kn"], inputs["dynamic_factor"])
    assert summary["wall_thickness_required_mm"] == stackup.wall_thickness_required(
        inputs["od_mm"], inputs["design_pressure_mpa"], inputs["smys_mpa"], inputs["barlow_safety_factor"])
    d = {k: Decimal(str(v)) for k, v in inputs.items() if isinstance(v, (float, int))}
    top = d["submerged_weight_kn"] * d["dynamic_factor"]
    wall = d["design_pressure_mpa"] * d["od_mm"] * d["barlow_safety_factor"] / (2 * d["smys_mpa"])
    slip = d["submerged_weight_kn"] * Decimal(str(stackup.F_WT_DEFAULT)) - d["buoyancy_uplift_kn"] * Decimal(str(stackup.F_BT_DEFAULT))
    slip += d["internal_area_m2"] * (d["mud_density_kn_m3"] * d["mud_column_m"] - d["seawater_density_kn_m3"] * d["seawater_column_m"])
    assert summary["top_tension_required_kn"] == pytest.approx(float(top))
    assert summary["wall_thickness_required_mm"] == pytest.approx(float(wall))
    assert summary["min_slip_ring_tension_kn"] == pytest.approx(float(slip))
    for row in profile:
        expected = top - d["submerged_weight_kn"] * Decimal(str(row["depth_factor"]))
        assert row["effective_tension_kn"] == pytest.approx(float(expected))
    assert data["hand_profile_kn"] == pytest.approx([r["effective_tension_kn"] for r in profile])
    assert data["minimum_effective_kn"] == min(r["effective_tension_kn"] for r in profile)
    assert data["tension_difference_pct"] == pytest.approx(0)


def test_rebuild_is_byte_identical_and_all_number_bindings_resolve(builder, tmp_path):
    data = builder.compute(tmp_path)
    html = builder.render(data)
    assert html == (REPORT / "index.html").read_text()
    doc = Document(html)
    assert len(doc.metrics) > 30
    for metric in doc.metrics:
        key, unit = metric["data-metric"], metric["data-unit"]
        expected = builder.metric_value(data, key, unit)
        assert float(metric["data-value"]) == expected
        assert metric["text"] == builder.format_value(expected, unit)
    assert len(doc.headings) == 10
    assert doc.external == []
    assert len(doc.ids) == len(set(doc.ids))
    assert set(doc.links) <= set(doc.ids)
    assert '<body class="hs-doc">' in html
    assert '<b>' not in html and '<strong>' not in html
    assert 'showDirectoryPicker' in html and 'report.comments.json' in html
    assert 'textContent = comment.text' in html
    assert json.loads((REPORT / "changes.json").read_text()) == data["changes"]
    start = html.index('<script type="application/json" id="engineering-data">')
    payload = html[start:].split('>', 1)[1].split('</script>', 1)[0]
    assert json.loads(payload) == builder.chart_data(data)
    assert json.loads(payload)["module_kip"] == pytest.approx([
        row["effective_tension_kn"] / adapter.KIPS_TO_KN for row in data["profile"]], rel=1e-15)


def test_input_change_flows_to_summary_profile_chart_and_display(builder, tmp_path):
    cfg = yaml.safe_load((ROOT / "examples/workflows/riser-stackup/input.yml").read_text())
    original = builder.compute(tmp_path / "original")
    cfg["riser_stackup"]["dynamic_factor"] += cfg["riser_stackup"]["dynamic_factor"]
    changed = builder.compute(tmp_path / "changed", cfg=cfg)
    assert changed["summary"]["top_tension_required_kn"] > original["summary"]["top_tension_required_kn"]
    assert changed["profile"] != original["profile"]
    assert builder.chart_data(changed) != builder.chart_data(original)
    assert builder.render(changed) != builder.render(original)


def test_units_use_the_component_adapter_convention(builder, tmp_path):
    data = builder.compute(tmp_path)
    top_kn = data["summary"]["top_tension_required_kn"]
    assert builder.metric_value(data, "summary.top_tension_required_kn", "kip") == top_kn / adapter.KIPS_TO_KN
    assert builder.metric_value(data, "inputs.od_mm", "in") * adapter._IN_TO_MM == data["inputs"]["od_mm"]


def test_built_text_has_no_unbound_example_numbers_or_unresolved_status(builder, tmp_path):
    html = builder.render(builder.compute(tmp_path))
    assert "ASSUMED - to be confirmed" in html
    assert "do not connect" in html
    assert "not evaluated: data needed: closure curves" in html
    assert "No operating window is established" in html
    assert "Draft for client review" in html
    assert "Wall tension is not evaluated" in html


def test_chart_and_comments_execute_without_a_browser(builder, tmp_path):
    """Exercise the embedded interaction code with a minimal DOM and file API."""
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable for the interaction check")
    data = builder.compute(tmp_path / "data")
    html = builder.render(data)
    script = re.findall(r"<script>(.*?)</script>", html, re.S)[0]
    harness = r'''
const assert = require('node:assert/strict');
class Element {
  constructor(tag='DIV') { this.tagName=tag; this.attrs={}; this.children=[]; this.events={}; this.textContent=''; this.value=''; }
  setAttribute(k,v) { this.attrs[k]=v; }
  appendChild(c) { this.children.push(c); return c; }
  append(...children) { this.children.push(...children); }
  replaceChildren() { this.children=[]; }
  addEventListener(name, callback) { this.events[name]=callback; }
  click() { this.clicked=true; }
}
const elements = new Map();
const get = id => { if (!elements.has(id)) elements.set(id,new Element()); return elements.get(id); };
const sectionIds = ['summary','intro','data','method','results','findings','limits','recommendations','references','acronyms'];
sectionIds.forEach(id=>{get(id).tagName='SECTION';get(id).id=id;});
globalThis.document = {getElementById:get, createElement:tag=>new Element(tag),
  createElementNS:(_,tag)=>new Element(tag), querySelectorAll:()=>sectionIds.map(get)};
let saved='', closed=false, existed=false;
globalThis.window = {getSelection:()=>({toString:()=>''}), showDirectoryPicker:async()=>({
  getFileHandle:async(name, options)=>{
    assert.equal(name,'report.comments.json');
    if (!options?.create && !existed) { const error = new Error(); error.name='NotFoundError'; throw error; }
    return {getFile:async()=>({text:async()=>saved}), createWritable:async()=>({
      write:async text=>{saved=text;existed=true;}, close:async()=>{closed=true;}})};
  }})};
get('engineering-data').textContent=CHART_JSON;
get('change-data').textContent=CHANGE_JSON;
'''
    assertions = r'''
(async()=>{
  const markers=get('tension-chart').children.filter(c=>c.tagName==='circle');
  assert.equal(markers.length,CHART_VALUE.module_kip.length);
  markers.forEach((m,i)=>{
    assert(m.attrs['aria-label'].includes(CHART_VALUE.module_kip[i].toFixed(1)+' kip'));
    m.events.focus(); assert.equal(get('chart-readout').textContent,m.attrs['aria-label']);
  });
  get('comment-text').value='Check the weight basis.';
  get('add-comment').events.click();
  await get('save-comments').events.click();
  assert(closed); assert.equal(JSON.parse(saved).comments[0].text,'Check the weight basis.');
  const stored=saved;
  const other = {section:'data',quote:'',text:'Retain this existing comment.'};
  saved=JSON.stringify({revision:JSON.parse(stored).revision,comments:[other]});
  await get('save-comments').events.click();
  assert.equal(JSON.parse(saved).comments.length,2);
  await get('save-comments').events.click();
  assert.equal(JSON.parse(saved).comments.length,2);
  saved='invalid';
  await get('save-comments').events.click();
  assert.equal(saved,'invalid');
  await get('load-comments').events.change({target:{files:[{text:async()=>stored}]}});
  assert.equal(get('comment-list').children.length,1);
  await get('load-comments').events.change({target:{files:[{text:async()=>'{}'}]}});
  assert(get('comment-status').textContent.includes('invalid'));
  window.showDirectoryPicker=undefined;
  await get('save-comments').events.click();
  assert(get('comment-status').textContent.includes('Download comments'));
})().catch(error=>{console.error(error);process.exitCode=1;});
'''
    payload = json.dumps(builder.chart_data(data), ensure_ascii=False)
    harness = harness.replace("CHART_JSON", json.dumps(payload))
    harness = harness.replace("CHANGE_JSON", json.dumps(json.dumps(data["changes"])))
    assertions = assertions.replace("CHART_VALUE", payload)
    path = tmp_path / "interaction.cjs"
    path.write_text(harness + script + assertions)
    result = subprocess.run([node, str(path)], capture_output=True, text=True, env=os.environ, timeout=30)
    assert result.returncode == 0, result.stderr
