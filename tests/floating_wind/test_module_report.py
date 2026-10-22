# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the module report and compare its numbers with fresh module calls."""

import ast
import importlib.util
import json
import math
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

from digitalmodel.floating_wind import floaters
from digitalmodel.floating_wind.screening import LoadCase, ScreeningCriteria, screen_concept
from digitalmodel.floating_wind.workflow import FloatingWindSizingWorkflow
from digitalmodel.orcaflex.batch_parametric import ParameterSweep

ROOT = Path(__file__).resolve().parents[2]
DIRECTORY = Path("docs/reports/modules/floatingwind")
BUILDER = ROOT / "scripts/reporting/build_floating_wind_module_report.py"


class Document(HTMLParser):
    """Collect displayed quantities, headings, links and embedded data."""

    def __init__(self, text):
        super().__init__()
        self.numbers = []
        self.ids = []
        self.links = []
        self.headings = []
        self.scripts = {}
        self.active_number = None
        self.active_script = None
        self.feed(text)

    def handle_starttag(self, tag, attributes):
        attributes = dict(attributes)
        if "id" in attributes:
            self.ids.append(attributes["id"])
        if tag in {"h2", "h3", "h4", "h5", "h6"}:
            self.headings.append(tag)
        if "href" in attributes:
            self.links.append(attributes["href"])
        if "data-number" in attributes:
            self.active_number = attributes["data-number"]
        if tag == "script" and attributes.get("type") == "application/json":
            self.active_script = attributes["id"]

    def handle_endtag(self, tag):
        if tag in {"span", "tspan"}:
            self.active_number = None
        if tag == "script":
            self.active_script = None

    def handle_data(self, text):
        if self.active_number:
            self.numbers.append((self.active_number, text))
        if self.active_script:
            self.scripts[self.active_script] = text


def builder():
    spec = importlib.util.spec_from_file_location("module_report_builder", BUILDER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def reference_numbers():
    """Independently call the documented workflow and its public screen."""
    source = ROOT / "examples/workflows/floating-wind-sizing/input.yml"
    inputs = yaml.safe_load(source.read_text())["floating_wind_sizing"]
    numbers = {}

    def visit(value, prefix):
        if isinstance(value, bool):
            return
        if isinstance(value, (int, float)):
            numbers[prefix] = value
        elif isinstance(value, dict):
            for key, child in value.items():
                visit(child, f"{prefix}.{key}")
        elif isinstance(value, (list, tuple)):
            for index, child in enumerate(value):
                visit(child, f"{prefix}.{index}")

    visit(inputs, "input")
    visit(FloatingWindSizingWorkflow().router({"floating_wind_sizing": inputs})[
        "floating_wind_sizing"]["result"], "summary")
    topside = floaters.TurbineTopside(**inputs["topside"])
    criteria = ScreeningCriteria(**inputs["criteria"])
    loads = [LoadCase(**load) for load in inputs["load_cases"]]
    for concept in inputs["concepts"]:
        result = screen_concept(
            concept["archetype"], concept["base_params"],
            [ParameterSweep(**sweep) for sweep in concept["sweeps"]],
            topside, loads, criteria,
        )
        for variant in result.variants:
            prefix = f"variant.{concept['archetype']}.{variant.case_id}"
            visit(variant.properties.model_dump(), f"{prefix}.properties")
            visit(variant.params, f"{prefix}.params")
            for check in variant.checks:
                visit(check.model_dump(), f"{prefix}.check.{check.name}")
    numbers["constant.density"] = floaters.RHO_SEAWATER
    numbers["constant.gravity"] = floaters.G_STANDARD
    for archetype, model in [("semi", floaters.SemiSubmersible),
                             ("spar", floaters.Spar), ("barge", floaters.Barge)]:
        fields = ["steel_vcg_frac", "ca_heave", "ca_pitch"]
        if archetype == "barge":
            fields += ["block_coeff", "waterplane_coeff"]
        for field in fields:
            numbers[f"default.{archetype}.{field}"] = model.model_fields[field].default
    tree = ast.parse((ROOT / "src/digitalmodel/floating_wind/floaters.py").read_text())
    function = next(node for node in tree.body if isinstance(node, ast.FunctionDef) and node.name == "_assemble")
    assignment = next(node for node in function.body if isinstance(node, ast.Assign)
                      and any(isinstance(target, ast.Name) and target.id == "ballast_cg_m" for target in node.targets))
    numbers["constant.ballast_fraction"] = assignment.value.left.value
    return inputs, numbers


def test_report_rebuilds_exactly(tmp_path):
    builder().build_report(ROOT, tmp_path)
    for filename in ("report.html", "changes.json"):
        assert (tmp_path / filename).read_bytes() == (ROOT / DIRECTORY / filename).read_bytes()


def test_every_displayed_example_number_matches_module(tmp_path):
    builder().build_report(ROOT, tmp_path)
    document = Document((tmp_path / "report.html").read_text())
    data = json.loads(document.scripts["report-data"])
    _, expected = reference_numbers()
    assert data["numbers"]
    assert len(document.numbers) > 100
    for key, record in data["numbers"].items():
        assert key in expected, key
        assert record["value"] == expected[key], key
    for key, displayed in document.numbers:
        record = data["numbers"][key]
        value = expected[key]
        digits = record["digits"]
        assert math.isfinite(value)
        formatted = f"{value:,.{digits}f}".replace("-", "−")
        assert displayed == formatted, key
        assert value == 0 or float(displayed.replace(",", "").replace("−", "-")) != 0
    assert set(key for key, _ in document.numbers) == set(data["numbers"])


def test_report_structure_sources_and_offline_controls(tmp_path):
    builder().build_report(ROOT, tmp_path)
    text = (tmp_path / "report.html").read_text()
    document = Document(text)
    assert '<body class="hs-doc doc">' in text
    assert document.headings.count("h2") == 10
    assert set(document.headings) == {"h2", "h3"}
    assert len(document.ids) == len(set(document.ids))
    for target in document.links:
        if target.startswith("#"):
            assert target[1:] in document.ids
        elif not target.startswith("https:"):
            assert (ROOT / DIRECTORY / target).resolve().exists(), target
    assert "<script src=" not in text and '<link rel="stylesheet"' not in text
    assert "<strong>" not in text and "<b>" not in text
    assert 'id="save-comments"' in text
    assert "showSaveFilePicker" in text and "createObjectURL" in text
    assert 'id="load-comments"' in text
    assert "textContent" in text
    assert "innerHTML" not in text
    changes = json.loads((tmp_path / "changes.json").read_text())
    assert json.loads(document.scripts["report-changes"]) == changes
    assert changes["revisions"][0]["changes"]
    assets = (ROOT / "src/digitalmodel/reporting/assets/calc_report_format.html").read_text()
    style = assets.split("<style>", 1)[1].split("</style>", 1)[0]
    assert style in text
    import re
    from xml.etree import ElementTree

    for image in re.findall(r"<svg\b.*?</svg>", text, re.S):
        ElementTree.fromstring(image)
        assert "<span " not in image


def test_charts_preserve_values_and_limits(tmp_path):
    builder().build_report(ROOT, tmp_path)
    document = Document((tmp_path / "report.html").read_text())
    data = json.loads(document.scripts["report-data"])
    _, expected = reference_numbers()
    assert {chart["quantity"] for chart in data["charts"]} == {"stability", "motion", "modal", "mooring"}
    for chart in data["charts"]:
        assert chart["limits"]
        for limit in chart["limits"]:
            assert limit in expected
        for point in chart["points"]:
            assert expected[point["demand"]] == data["numbers"][point["demand"]]["value"]
            assert point["x"] in expected
            assert point["case"] and point["component"] and point["units"]


def test_builder_uses_changed_module_values(monkeypatch, tmp_path):
    original = floaters.SemiSubmersible.properties

    def altered(self, topside):
        result = original(self, topside)
        return result.model_copy(update={"GM_m": result.GM_m + 2.0})

    module = builder()
    baseline = module.render_report(ROOT)
    monkeypatch.setattr(floaters.SemiSubmersible, "properties", altered)
    changed = module.render_report(ROOT)
    assert baseline != changed
    data = json.loads(Document(changed).scripts["report-data"])
    baseline_data = json.loads(Document(baseline).scripts["report-data"])
    keys = [key for key in data["numbers"] if key.startswith("variant.semi.") and key.endswith(".GM_m")]
    assert keys
    for key in keys:
        assert data["numbers"][key]["value"] == pytest.approx(baseline_data["numbers"][key]["value"] + 2.0)


def test_chart_selection_and_comment_file_round_trip():
    """Exercise browser-independent control logic with a minimal DOM in Node."""
    import subprocess

    harness = r"""
const assert = require('node:assert/strict');
const vm = require('node:vm');
const source = require('node:fs').readFileSync(0, 'utf8');
class Element {
  constructor(tag) { this.tagName = tag; this.children = []; this.events = {}; this.value = ''; this.textContent = ''; }
  appendChild(child) { this.children.push(child); return child; }
  replaceChildren() { this.children = []; }
  setAttribute(key, value) { this[key] = value; }
  addEventListener(name, handler) { this.events[name] = handler; }
}
const ids = {};
const get = id => ids[id] || (ids[id] = new Element('div'));
const sections = Array.from({length: 10}, (_, index) => ({id: 'section-' + (index + 1),
  querySelector: () => ({textContent: String(index + 1)})}));
const keys = {x:{value:12, digits:1}, radius:{value:45, digits:2}, demand:{value:3, digits:1}, limit:{value:2, digits:1}};
get('report-data').textContent = JSON.stringify({numbers:keys, charts:['stability','motion','modal','mooring'].map(quantity => ({
  quantity, label:quantity, units:'m', limits:['limit'], points:[{x:'x',radius:'radius',demand:'demand',case:'Example',component:'Response',units:'m',passed:false,load_case:'Example case'}]
}))});
get('report-changes').textContent = JSON.stringify({report:'Test report',revisions:[{revision:'0', changes:['First issue.']}]});
get('chart-select').value = 'stability'; get('last-revision').value = '-1';
const document = {getElementById:get,createElement:tag=>new Element(tag),createElementNS:(_,tag)=>new Element(tag),querySelectorAll:()=>sections};
let saved = '';
const window = {showSaveFilePicker:async()=>({createWritable:async()=>({write:async blob=>{saved = await blob.text();}, close:async()=>{}})})};
vm.runInNewContext(source, {document,window,Blob,URL});
(async()=>{
  assert.equal(get('comment-section').children.length,10);
  assert.equal(get('changes-list').children[0].textContent,'First issue.');
  get('last-revision').value='0'; get('last-revision').events.change();
  assert.match(get('changes-list').children[0].textContent,/No engineering changes/);
  get('chart-select').value='motion'; get('chart-select').events.change();
  assert.match(get('chart-host').children[0]['aria-label'],/motion/);
  const circle=get('chart-host').children[0].children.find(child=>child.tagName==='circle');
  circle.events.focus();
  assert.match(get('chart-detail').textContent,/demand 3.0 m; limit 2.0 m/);
  get('comment-section').value='section-2'; get('comment-text').value='<img onerror=alert(1)> & check';
  get('add-comment').events.click();
  assert.equal(get('comments-list').children[0].textContent,'section-2: <img onerror=alert(1)> & check');
  await get('save-comments').events.click();
  const parsed=JSON.parse(saved); assert.equal(parsed.comments.length,1);
  await get('load-comments').events.change({target:{files:[{size:saved.length,text:async()=>saved}]}});
  assert.equal(get('comments-list').children.length,1);
  const wrong=JSON.stringify({...parsed,revision:'different'});
  await get('load-comments').events.change({target:{files:[{size:wrong.length,text:async()=>wrong}]}});
  assert.match(get('comment-message').textContent,/invalid or belongs to a different/);
  assert.equal(get('comments-list').children.length,1);
})().catch(error=>{console.error(error);process.exitCode=1;});
"""
    result = subprocess.run(["node", "-e", harness], input=builder().INTERACTION,
                            text=True, capture_output=True, timeout=30)
    assert result.returncode == 0, result.stderr
