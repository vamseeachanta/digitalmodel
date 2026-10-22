# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Numerical consistency, hand anchors and portable report rebuilding."""

import importlib.util
import json
import re
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

from digitalmodel.foam_system.workflow import router

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/foamsystem"
COMMAND = "PYTHONPATH=src uv run --no-project --with pyyaml python docs/reports/modules/foamsystem/build_report.py --scratch <temporary-directory>"


def builder():
    spec = importlib.util.spec_from_file_location("foam_module_report", REPORT / "build_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def data(tmp_path):
    return builder().calculate(tmp_path)


def test_hand_flow_anchor(data):
    # Q = max(3000*0.6, 500*6, 450*3) = max(1800,3000,1350) = 3000 L/min.
    # Absolute tolerance 1e-9 L/min is arithmetic tolerance, not a design margin.
    assert data["metrics"]["design_flow"] == pytest.approx(3000.0, abs=1e-9)


def test_hand_inventory_anchor(data):
    # Concentrate = 3000 L/min * 20 min * (3/100) * (1+0/100) = 1800 L.
    assert data["metrics"]["concentrate"] == pytest.approx(1800.0, abs=1e-9)


def test_hand_injection_anchor(data):
    # Injection = 3000 L/min * (3/100) = 90 L/min.
    assert data["metrics"]["injection"] == pytest.approx(90.0, abs=1e-9)


def test_public_example_consistency(data, tmp_path):
    cfg = yaml.safe_load((ROOT / builder().EXAMPLE).read_text(encoding="utf-8"))
    original = cfg["foam_system_sizing"]
    original["output_dir"] = str(tmp_path / "fresh")
    result = router(cfg)["foam_system_sizing"]
    assert data["metrics"] == builder().metrics(result)
    for key in ("demand", "concentrate_result", "proportioner_result", "hydraulics_result"):
        assert data["result"][key] == result[key]


class Document(HTMLParser):
    def __init__(self):
        super().__init__()
        self.ids = []
        self.links = []
        self.values = []
        self.tag = None
        self.visible = []
        self.hidden = 0
        self.headings = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "a" and attrs.get("href", "").startswith("#"):
            self.links.append(attrs["href"][1:])
        if "data-value" in attrs:
            self.tag = attrs["data-value"]
        if tag in ("script", "style"):
            self.hidden += 1
        if tag in ("h2", "h3"):
            self.headings.append(tag)

    def handle_data(self, value):
        if self.tag:
            self.values.append((self.tag, value))
            self.tag = None
        if not self.hidden:
            self.visible.append(value)

    def handle_endtag(self, tag):
        if tag in ("script", "style"):
            self.hidden -= 1


def test_every_example_display_value_consistency(data):
    module = builder()
    document = Document()
    document.feed(module.render(data))
    assert len(document.values) >= 30
    for path, text in document.values:
        value = module.lookup(data, path)
        assert text == module.display(value), path


def test_headline_from_displayed_inputs(data):
    module = builder()
    areas = data["result"]["demand"]["per_area"]
    # The headline uses the same displayed input precision as the calculation.
    products = [float(module.display(a["area_m2"])) * float(module.display(a["application_rate_lpm_per_m2"])) for a in areas]
    assert max(products) == float(module.display(data["metrics"]["design_flow"]))


def test_curve_points_consistency(data, tmp_path):
    cfg = yaml.safe_load((ROOT / builder().EXAMPLE).read_text(encoding="utf-8"))
    base = cfg["foam_system_sizing"]["hydraulics"]["terminals"]
    for index, point in enumerate(data["curve"]):
        case = yaml.safe_load((ROOT / builder().EXAMPLE).read_text(encoding="utf-8"))
        case["foam_system_sizing"]["output_dir"] = str(tmp_path / f"curve-{index}")
        for terminal, original in zip(case["foam_system_sizing"]["hydraulics"]["terminals"], base):
            terminal["flow_lpm"] = original["flow_lpm"] * point["scale"]
        result = router(case)["foam_system_sizing"]["hydraulics_result"]
        assert point["head"] == pytest.approx(result["required_pump_head_m"], abs=5e-5)
        assert point["flow"] == result["total_flow_lpm"]
        assert point["limit"] == case["foam_system_sizing"]["hydraulics"]["pump"]["rated_head_m"]


def test_rebuild_displayed_digits_and_changes(tmp_path):
    html = builder().build(tmp_path)
    committed = (REPORT / "report.html").read_text(encoding="utf-8")
    assert html == committed, f"Report differs at displayed precision; rebuild: {COMMAND}"
    changes = json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    assert changes["revision"] == "0"
    assert changes["changes"]
    assert "\r" not in html
    assert '<body class="hs-doc">' in html
    assert not re.search(r'<(?:script|link)[^>]+(?:src|href)="https?://', html)


def test_document_structure_and_public_text(data):
    html = builder().render(data)
    document = Document()
    document.feed(html)
    assert len(document.ids) == len(set(document.ids))
    assert set(document.links) <= set(document.ids)
    assert document.headings.count("h2") == 10
    visible = " ".join(document.visible)
    assert "Draft for maintainer review" in visible
    assert not re.search(r"\b(could|should|would|verified|certified|compliant)\b", visible, re.I)
    assert not re.search(r"<b(?:>| )|<strong", html)
    for marker in ("NOT_EVALUATED", "ASSUMED - to be confirmed", "showSaveFilePicker", "report.comments.json", "changes-panel", "chart-tooltip"):
        assert marker in html


def test_text_io_explicit_encoding_and_newlines():
    import ast

    for path in (REPORT / "build_report.py", Path(__file__)):
        tree = ast.parse(path.read_text(encoding="utf-8"))
        for node in ast.walk(tree):
            if isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute):
                name = node.func.attr
                if name in ("read_text", "write_text", "open"):
                    keywords = {key.arg: key.value for key in node.keywords}
                    assert "encoding" in keywords, f"{path.name}:{node.lineno}"
                    if name == "write_text":
                        assert ast.literal_eval(keywords["newline"]) == "\n"


def test_source_access_records(data):
    source = json.loads((REPORT / "sources.json").read_text(encoding="utf-8"))
    html = builder().render(data)
    for record in source["sources"]:
        assert record["http_status"] == 200
        assert record["url"] in html
        assert re.fullmatch(r"[a-f0-9]{64}", record["content_sha256"])
        assert record["locators"]
        assert record["accessed_utc"].endswith("Z")
    import ast

    checks = json.loads((REPORT / "checks.json").read_text(encoding="utf-8"))
    tree = ast.parse(Path(__file__).read_text(encoding="utf-8"))
    count = sum(isinstance(item, ast.FunctionDef) and item.name.startswith("test_") for item in tree.body)
    assert checks["test_results"][0]["passed"] == count, f"Update checks.json and rebuild: {COMMAND}"
    existing = checks["test_results"][1]
    assert existing["passed"] + existing["unavailable"] == sum(item["functions"] for item in data["inventory"]), f"Refresh the evidence and rebuild: {COMMAND}"


def test_comment_and_chart_handlers_without_browser(data, tmp_path):
    import shutil
    import subprocess

    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable for the document interaction check")
    html = builder().render(data)
    program = re.search(r"<script>\n(.*?)</script>", html, re.S).group(1)
    # A DOM stub exercises event handlers and file serialization. It is not a
    # browser rendering or a real file-picker permission test.
    harness = r"""
const assert = require('node:assert/strict'), vm = require('node:vm');
class Element {
  constructor() {this.children=[];this.events={};this.textContent='';this.value='';this.attrs={};}
  append(e) {this.children.push(e);}
  replaceChildren() {this.children=[];}
  setAttribute(k,v) {this.attrs[k]=v;}
  addEventListener(k,f) {this.events[k]=f;}
  click() {this.clicked=true;}
}
const nodes = new Map();
const get = id => {if(!nodes.has(id))nodes.set(id,new Element());return nodes.get(id);};
get('calculation-data').textContent=JSON.stringify(DATA);
get('changes-data').textContent=JSON.stringify({revision:'0',changes:['Engineering change']});
const sections=['summary','introduction','basis','method','results','validation','limits','recommendations','references','appendix'];
const document={getElementById:get,createElement:()=>new Element(),createElementNS:()=>new Element(),querySelectorAll:()=>sections.map(id=>({id,querySelector:()=>({textContent:id})}))};
let saved='',closed=false;
const window={showSaveFilePicker:async()=>({createWritable:async()=>({write:async text=>{saved=text;},close:async()=>{closed=true;}})})};
vm.runInNewContext(PROGRAM,{document,window,URL:{createObjectURL:()=>'',revokeObjectURL:()=>{}},Blob,setTimeout:fn=>fn()});
(async()=>{
  assert.equal(get('comment-section').children.length,sections.length);
  get('case-slider').events.input({target:{value:'0'}});
  assert.ok(get('chart-tooltip').textContent.includes(DATA.curve[0].component));
  assert.ok(get('chart-tooltip').textContent.includes(DATA.curve[0].head.toFixed(1)));
  get('case-slider').events.input({target:{value:String(DATA.curve.length-1)}});
  assert.ok(get('chart-tooltip').textContent.includes('Flow exceeds'));
  get('comment-section').value='basis';get('comment-text').value='Review equipment inputs.';
  get('add-comment').events.click();
  assert.equal(get('comment-list').children.length,1);
  await get('save-comments').events.click();
  assert.ok(closed);assert.deepEqual(JSON.parse(saved).comments,[{section:'basis',text:'Review equipment inputs.'}]);
  await get('load-comments').events.change({target:{files:[{text:async()=>saved}]}});
  assert.equal(get('comment-list').children.length,1);
  await get('load-comments').events.change({target:{files:[{text:async()=>'{}'}]}});
  assert.ok(get('comment-status').textContent.includes('invalid format'));
  window.showSaveFilePicker=async()=>{throw Error('Permission declined');};
  await get('save-comments').events.click();
  assert.ok(get('comment-status').textContent.includes('not saved'));
  delete window.showSaveFilePicker;
  await get('save-comments').events.click();
  assert.ok(get('comment-status').textContent.includes('exported'));
})().catch(error=>{console.error(error);process.exitCode=1;});
"""
    path = tmp_path / "document-interaction.cjs"
    prefix = "const DATA=" + json.dumps(data) + ";\nconst PROGRAM=" + json.dumps(program) + ";\n"
    path.write_text(prefix + harness, encoding="utf-8", newline="\n")
    subprocess.run([node, "--check", str(path)], check=True, capture_output=True, text=True)
    result = subprocess.run([node, str(path)], capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
