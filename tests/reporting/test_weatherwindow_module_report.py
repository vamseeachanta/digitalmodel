# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check the weather-window report against arithmetic and the public interface."""

import importlib.util
import json
import re
import shutil
import subprocess
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
DIRECTORY = ROOT / "docs/reports/modules/weatherwindow"
REBUILD = "PYTHONPATH=src python docs/reports/modules/weatherwindow/build_report.py"


def read_utf8(path):
    """Read report evidence using explicit UTF-8 and LF handling."""
    with path.open(encoding="utf-8", newline="\n") as stream:
        return stream.read()


def builder():
    """Load the report generator without importing other report builders."""
    spec = importlib.util.spec_from_file_location("weatherwindow_report", DIRECTORY / "build_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class Page(HTMLParser):
    """Collect numeric bindings and links from the built HTML."""

    def __init__(self):
        super().__init__()
        self.bindings = []
        self.current = None
        self.ids = []
        self.links = []
        self.resources = []
        self.headings = []
        self.heading = False

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "a" and attrs.get("href", "").startswith("#"):
            self.links.append(attrs["href"][1:])
        if tag in {"script", "link", "img", "iframe"}:
            self.resources.extend(attrs[key] for key in ("src", "href") if key in attrs)
        if "data-value" in attrs:
            self.current = attrs["data-value"]
        if tag == "h2":
            self.heading = True

    def handle_endtag(self, tag):
        if tag == "span":
            self.current = None
        if tag == "h2":
            self.heading = False

    def handle_data(self, data):
        if self.current:
            self.bindings.append((self.current, data))
        if self.heading:
            self.headings.append(data)


def test_public_example_hand_anchors():
    """Anchor displayed results to direct counting, with absolute tolerance 1e-9."""
    record = builder().calculate()
    result = record["result"]
    # Count the published blocks: 12+3+10+4+8+3+11+5 = 56 samples; 56*3 = 168 h.
    assert result["record_hours"] == pytest.approx(168.0, abs=1e-9)
    # Accepted blocks contain 12+10+8+11 = 41 samples; 100*41/56 = 73.214285714...%.
    assert result["workability_pct"] == pytest.approx(73.21428571428571, abs=1e-9)
    # Accepted block lengths are 12*3, 10*3, 8*3, 11*3 = 36,30,24,33 h; max = 36 h.
    assert result["longest_window_hours"] == pytest.approx(36.0, abs=1e-9)
    # (36+30+24+33)/4 = 30.75 h; persistence output rounded to one decimal is 30.8 h.
    assert result["mean_window_hours"] == pytest.approx(30.8, abs=1e-9)
    # Sorted 24,30,33,36: rank 1+.1*(4-1)=1.3; 24+.3*(30-24)=25.8 h.
    assert result["p10_window_hours"] == pytest.approx(25.8, abs=1e-9)
    # Rank 1+.9*(4-1)=3.7; 33+.7*(36-33)=35.1 h.
    assert result["p90_window_hours"] == pytest.approx(35.1, abs=1e-9)
    assert record["windows_hours"] == [36.0, 30.0, 24.0, 33.0]
    assert result["num_windows"] == 4
    assert result["num_viable_windows"] == 0
    assert result["screening_status"] == "fail"


def test_report_numbers_are_module_consistency_checks():
    """Every displayed result is checked against a fresh public-interface call."""
    from digitalmodel.weather_window.workflow import assess_weather_window, window_lengths_hours

    module = builder()
    record = module.calculate()
    source = yaml.safe_load(read_utf8(ROOT / module.EXAMPLE))["weather_window"]
    arguments = {
        "hs_timeseries": source["metocean"]["hs_timeseries_m"],
        "hs_limit": source["operation"]["hs_limit_m"],
        "required_duration_hours": source["operation"]["required_duration_hours"],
        "time_step_hours": source["metocean"]["time_step_hours"],
        "required_workability_pct": source["operation"]["required_workability_pct"],
    }
    actual = assess_weather_window(**arguments)
    assert record["result"] == actual
    assert record["windows_hours"] == window_lengths_hours(
        arguments["hs_timeseries"], arguments["hs_limit"], arguments["time_step_hours"]
    )
    page = Page()
    page.feed(module.render(record))
    assert len(page.bindings) > 90
    for key, text in page.bindings:
        assert text == module.display(module.lookup(record, key)), key
    # A headline result is recomputed from the displayed inputs: 56*3 = 168 h.
    displayed_count = int(module.display(record["sample_count"]))
    displayed_interval = float(module.display(actual["time_step_hours"]))
    assert displayed_count * displayed_interval == float(module.display(actual["record_hours"]))
    # Workability displayed to 0.1%; counting the displayed samples gives that same value.
    heights = [float(module.display(v)) for v in record["hs_m"]]
    limit = float(module.display(actual["hs_limit_m"]))
    assert f"{100 * sum(h <= limit for h in heights) / len(heights):.1f}" == module.display(actual["workability_pct"])


def test_rebuild_matches_committed_display(tmp_path):
    """Compare the complete UTF-8 artifact, including all calculation metadata."""
    module = builder()
    target = tmp_path / "report.html"
    module.build(target)
    assert (DIRECTORY / "report.html").read_bytes() == target.read_bytes(), (
        f"Report is stale; rebuild with: {REBUILD}"
    )
    assert b"\r" not in target.read_bytes()
    target.read_bytes().decode("utf-8")


def test_house_style_references_and_comment_controls():
    """Check the delivered artifact, including local references and offline resources."""
    module = builder()
    text = read_utf8(DIRECTORY / "report.html")
    page = Page()
    page.feed(text)
    assert len(page.headings) == 10
    assert len(page.ids) == len(set(page.ids))
    assert set(page.links) <= set(page.ids)
    assert page.resources == []
    assert '<body class="hs-doc">' in text
    exemplar = read_utf8(ROOT / module.EXEMPLAR)
    assert re.search(r"<style>.*?</style>", exemplar, re.S).group(0) in text
    assert "Draft for maintainer review" in text
    assert "showDirectoryPicker" in text
    assert "report.comments.json" in text
    assert "createWritable" in text
    assert "textContent" in text
    assert "data-limit" in text
    assert "tabindex=\"0\"" in text
    assert "NOT_EVALUATED" in text
    assert "ASSUMED - to be confirmed" in text
    assert "R7 alternative listed in the handbook" in text
    assert "installation start" not in text
    assert "operation start" in text
    changes = json.loads(read_utf8(DIRECTORY / "changes.json"))
    assert changes["history"][0]["revision"] == "0"
    assert changes["history"][0]["changes"]
    assert changes["history"][-1]["revision"] == "1"


def test_rounding_diagnostic_matches_documented_limitation():
    """Preserve evidence of a pre-existing rounded fit/count disagreement."""
    record = builder().calculate()
    diagnostic = record["diagnostic"]
    # A single accepted sample lasting 0.96 h cannot contain a required 1 h spell.
    assert diagnostic["raw_longest_hours"] == pytest.approx(0.96, abs=1e-9)
    assert diagnostic["result"]["num_viable_windows"] == 0
    assert diagnostic["result"]["operation_fits_window"] is True
    assert diagnostic["result"]["screening_status"] == "pass"
    assert diagnostic["result"]["workability_pct"] > 100


def test_chart_inputs_are_module_consistency_checks():
    """Check both interactive chart series, axes and limits against the public call."""
    module = builder()
    record = module.calculate()
    text = module.render(record)
    chart = json.loads(re.search(
        r'<script id="chart-data" type="application/json">(.*?)</script>', text, re.S
    ).group(1))
    assert chart["heights"] == record["hs_m"]
    assert chart["times"] == record["sample_start_hours"]
    assert chart["heightLimit"] == record["result"]["hs_limit_m"]
    assert chart["required"] == record["result"]["required_duration_hours"]
    assert chart["interval"] == record["result"]["time_step_hours"]
    assert chart["windows"] == record["windows_hours"]
    assert chart["heightAccepted"] == [height <= 2.0 for height in record["hs_m"]]
    assert chart["windowAccepted"] == [False, False, False, False]


def boundary_chart(module):
    """Create values whose displayed rounding erases either decision margin."""
    record = module.calculate()
    record.update(hs_m=[2.04, 1.96], sample_count=2, sample_start_hours=[0.0, 3.0],
                  windows_hours=[47.96, 48.04])
    text = module.render(record)
    return json.loads(re.search(
        r'<script id="chart-data" type="application/json">(.*?)</script>', text, re.S
    ).group(1))


def test_chart_decisions_use_unrounded_values():
    """Displayed equalities must preserve the raw rejection and acceptance."""
    chart = boundary_chart(builder())
    assert chart["heights"] == [2.0, 2.0]
    assert chart["windows"] == [48.0, 48.0]
    assert chart["heightAccepted"] == [False, True]
    assert chart["windowAccepted"] == [False, True]


def test_chart_and_comment_interaction_without_browser(tmp_path):
    """Exercise the delivered script using a minimal DOM and in-memory file handles."""
    node = shutil.which("node")
    assert node, "Node is required for the report interaction check"
    text = builder().render(builder().calculate())
    page = Page()
    page.feed(text)
    metadata = dict(re.findall(
        r'<script id="([^"]+)" type="application/json">(.*?)</script>', text, re.S
    ))
    script = re.findall(r"<script>(.*?)</script>", text, re.S)[0]
    harness = r'''
const assert = require("node:assert/strict");
const vm = require("node:vm");
class Element {
  constructor(tag="div") {this.tag=tag;this.attrs={};this.children=[];this.events={};this.textContent="";this.value="";}
  setAttribute(key,value) {this.attrs[key]=String(value);}
  appendChild(child) {this.children.push(child);return child;}
  replaceChildren() {this.children=[];}
  addEventListener(event,callback) {(this.events[event]??=[]).push(callback);}
  async trigger(event,argument={}) {for(const callback of this.events[event]??[]) await callback(argument);}
  click() {downloads.push(this.download);}
  querySelector() {return {textContent:this.textContent};}
}
const nodes={}, downloads=[], blobs=[], stored={};
const get=id=>nodes[id]??=(new Element());
for(const [id,payload] of Object.entries(METADATA)) get(id).textContent=payload;
const sections=SECTION_IDS.map(id=>{const e=get(id);e.id=id;e.textContent=id;return e;});
get("last-revision").value="none";get("comment-section").value=SECTION_IDS[0];
const context={
  document:{getElementById:get,querySelectorAll:()=>sections,
    createElement:tag=>new Element(tag),createElementNS:(_,tag)=>new Element(tag)},
  window:{addEventListener:()=>{}},Blob,
  URL:{createObjectURL:blob=>{blobs.push(blob);return "blob:test";},revokeObjectURL:()=>{}},
  setTimeout:callback=>callback(),
};
vm.createContext(context);vm.runInContext(SCRIPT,context);
(async()=>{
  const heights=get("height-chart");
  assert.equal(heights.children.filter(e=>e.attrs["stroke-width"]==="5").length,56);
  assert.equal(heights.children.find(e=>e.attrs["data-limit"]).attrs["data-limit"],"2");
  // Quarter-axis positions must correspond to the values printed at those positions.
  const ticks=heights.children.filter(e=>e.tag==="text" && e.attrs.x==="15");
  assert.deepEqual(ticks.map(e=>e.textContent),["0","1","2","3","4"]);
  const tickHeight=Number(ticks.at(-1).textContent);
  const limitLine=heights.children.find(e=>e.attrs["data-limit"]);
  assert.equal(Number(limitLine.attrs.y1),260-2/tickHeight*220);
  const point=heights.children.find(e=>e.attrs.tabindex==="0");
  await point.trigger("focus");assert.match(get("height-readout").textContent,/demand 1.5 m; limit 2 m/);
  const windows=get("window-chart");
  assert.equal(windows.children.filter(e=>e.tag==="rect").length,4);
  assert.equal(windows.children.find(e=>e.attrs["data-limit"]).attrs["data-limit"],"48");
  assert.equal(windows.children.filter(e=>e.tag==="rect" && e.attrs.class==="fail").length,4);
  get("comment-text").value="<img> Review the duration limit.";
  await get("add-comment").trigger("click");
  assert.match(get("comment-list").children[0].textContent,/<img>/);
  assert.equal(get("comment-list").children[0].children.length,0);
  await get("save-comments").trigger("click");
  assert.deepEqual(downloads,["report.comments.json"]);
  assert.equal(JSON.parse(await blobs[0].text()).comments.length,1);
  context.window.showDirectoryPicker=async()=>({getFileHandle:async(name,options)=>{
    if(name==="report.html") return {getFile:async()=>({text:async()=>REPORT_HTML})};
    if(!stored[name] && !options?.create) throw Object.assign(new Error(),{name:"NotFoundError"});
    return {getFile:async()=>({text:async()=>stored[name]}),
      createWritable:async()=>({write:async text=>{stored[name]=text;},close:async()=>{}})};
  }});
  await get("open-folder").trigger("click");
  assert.match(get("comment-status").textContent,/adjacent saving is enabled/);
  get("comment-text").value="Use unrounded quantities.";
  await get("add-comment").trigger("click");await get("save-comments").trigger("click");
  assert.equal(JSON.parse(stored["report.comments.json"]).comments[0].text,"Use unrounded quantities.");
  await get("open-folder").trigger("click");
  assert.match(get("comment-list").children[0].textContent,/Use unrounded quantities/);
  get("last-revision").value="0";await get("last-revision").trigger("change");
  assert.match(get("change-list").textContent,/unrounded quantities/);
  assert.doesNotMatch(get("change-list").textContent,/documented using the existing/);
  get("last-revision").value="1";await get("last-revision").trigger("change");
  assert.match(get("change-list").textContent,/No engineering changes/);
  // Re-render rounded-equal values using decisions generated from raw quantities.
  Object.assign(vm.runInContext("chart",context),BOUNDARY_CHART);
  heights.replaceChildren();windows.replaceChildren();
  vm.runInContext("waveChart();windowChart();",context);
  const steps=heights.children.filter(e=>e.attrs["stroke-width"]==="5");
  assert.deepEqual(steps.map(e=>e.attrs.stroke),["var(--nogo)","var(--go)"]);
  const bars=windows.children.filter(e=>e.tag==="rect");
  assert.deepEqual(bars.map(e=>e.attrs.class),["fail","pass"]);
  await steps[0].trigger("focus");
  assert.match(get("height-readout").textContent,/Criterion is not met/);
  await bars[0].trigger("focus");
  assert.match(get("window-readout").textContent,/Criterion is not met/);
})().catch(error=>{console.error(error);process.exitCode=1;});
'''
    # Plain JSON is passed as JavaScript data, never through shell interpolation.
    section_ids = [value for value in page.ids if value in {
        "executive", "introduction", "summary", "basis", "method", "results",
        "validation", "findings", "references", "appendix",
    }]
    content = "\n".join([
        f"const METADATA={json.dumps(metadata, ensure_ascii=False)};",
        f"const SECTION_IDS={json.dumps(section_ids)};",
        f"const SCRIPT={json.dumps(script, ensure_ascii=False)};",
        f"const REPORT_HTML={json.dumps(text, ensure_ascii=False)};",
        f"const BOUNDARY_CHART={json.dumps(boundary_chart(builder()))};",
        harness,
    ])
    target = tmp_path / "interaction.cjs"
    target.write_text(content, encoding="utf-8", newline="\n")
    result = subprocess.run([node, str(target)], capture_output=True, text=True, encoding="utf-8", check=False)
    assert result.returncode == 0, result.stderr


def test_only_report_deliverables_are_reincluded():
    """Keep the requested report visible while local comments remain ignored."""
    for name in ("build_report.py", "changes.json", "report.html"):
        result = subprocess.run(
            ["git", "check-ignore", "--no-index", "-q", str(DIRECTORY / name)],
            cwd=ROOT, check=False,
        )
        assert result.returncode == 1, f"Report deliverable is ignored: {name}"
    result = subprocess.run(
        ["git", "check-ignore", "--no-index", "-q", str(DIRECTORY / "report.comments.json")],
        cwd=ROOT, check=False,
    )
    assert result.returncode == 0, "Local review annotations must stay untracked"


def test_configured_example_router_outputs(tmp_path):
    """Exercise the configuration route with all output directed to temporary storage."""
    from digitalmodel.weather_window.workflow import router

    module = builder()
    settings = yaml.safe_load(read_utf8(ROOT / module.EXAMPLE))["weather_window"]
    settings["outputs"]["directory"] = "weather_window"
    cfg = router({"weather_window": settings, "_config_dir_path": str(tmp_path)})
    payload = cfg["weather_window"]
    path = Path(payload["summary_json"])
    assert path.parent == tmp_path / "weather_window"
    saved = json.loads(read_utf8(path))
    assert saved["result"] == module.calculate()["result"]
    assert saved["screening_status"] == cfg["screening_status"] == "fail"
    assert "summary_json" not in saved
