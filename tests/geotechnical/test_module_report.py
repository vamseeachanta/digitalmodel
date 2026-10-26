# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Check the module report against hand arithmetic and API integration calls."""

from copy import deepcopy
from dataclasses import asdict
from html.parser import HTMLParser
import importlib.util
import json
from pathlib import Path
import re
import shutil
import subprocess

import pytest
import yaml

from digitalmodel.geotechnical.pile_capacity import alpha_method_capacity
from digitalmodel.geotechnical.workflow import GeotechnicalWorkflow


ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/geotechnical"
EXAMPLE = ROOT / "examples/workflows/pile-axial-capacity/input.yml"


def builder():
    """Load the report builder without importing unrelated calculators."""
    spec = importlib.util.spec_from_file_location("geotechnical_report", REPORT / "build_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class Page(HTMLParser):
    """Collect numeric citations, anchors, resources and section headings."""

    def __init__(self, text):
        super().__init__()
        self.metrics = []
        self.ids = []
        self.links = []
        self.resources = []
        self.headings = []
        self.metric = None
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attributes = dict(attrs)
        if "id" in attributes:
            self.ids.append(attributes["id"])
        if tag == "h2":
            self.headings.append(attributes.get("id"))
        if tag == "a":
            self.links.append(attributes.get("href", ""))
        if tag in ("script", "link", "img"):
            self.resources.extend(attributes[k] for k in ("src", "href") if k in attributes)
        if "data-quantity" in attributes:
            self.metric = [attributes, ""]

    def handle_data(self, data):
        if self.metric is not None:
            self.metric[1] += data

    def handle_endtag(self, tag):
        if tag == "span" and self.metric is not None:
            self.metrics.append(self.metric)
            self.metric = None


def payload(text):
    """Read the calculation record embedded in the standalone report."""
    return json.loads(re.search(r'<script id="calculation" type="application/json">(.*?)</script>', text, re.S)[1])


def resolve(data, key):
    """Resolve a dotted numeric field in the embedded calculation record."""
    for part in key.split("."):
        data = data[int(part)] if isinstance(data, list) else data[part]
    return data


@pytest.fixture
def rebuilt(tmp_path):
    output = tmp_path / "report"
    builder().build(output, tmp_path / "calculation")
    return (output / "index.html").read_text(), output


def test_rebuild_is_identical_to_committed_report(rebuilt):
    text, output = rebuilt
    assert text == (REPORT / "index.html").read_text()
    assert (output / "changes.json").read_bytes() == (REPORT / "changes.json").read_bytes()


def test_every_worked_result_matches_public_function(rebuilt):
    data = payload(rebuilt[0])
    fixture = yaml.safe_load(EXAMPLE.read_text())["geotechnical"]["pile_axial_capacity"]
    expected_inputs = dict(D=fixture["pile"]["diameter_m"], L=fixture["pile"]["embedded_length_m"],
                           Su=fixture["soil"]["Su_kpa"], sigma_v=fixture["soil"]["sigma_v_kpa"],
                           Nc=fixture["pile"]["Nc"])
    assert data["inputs"] == expected_inputs
    expected = asdict(alpha_method_capacity(**expected_inputs))
    assert data["result"] == {key: value for key, value in expected.items() if isinstance(value, float)}
    assert set(data["result"]) == {"alpha", "unit_skin_friction_kpa", "skin_friction_kn",
                                   "end_bearing_kn", "total_capacity_kn"}


def test_design_and_every_chart_point_match_workflow(rebuilt, tmp_path):
    data = payload(rebuilt[0])
    fixture = yaml.safe_load(EXAMPLE.read_text())
    for index, point in enumerate([data["design"], *data["sweep"]]):
        cfg = deepcopy(fixture)
        case = cfg["geotechnical"]["pile_axial_capacity"]
        case["pile"]["embedded_length_m"] = point["length_m"]
        case["outputs"]["directory"] = str(tmp_path / str(index))
        expected = GeotechnicalWorkflow().router(cfg)["geotechnical"]["pile_axial_capacity"]
        assert point["allowable_capacity_kn"] == expected["design"]["allowable_capacity_kn"]
        assert point["utilization"] == expected["design"]["utilization"]
        assert point["status"] == expected["design"]["status"]
        assert point["demand_kn"] == expected["design"]["applied_load_kn"]
        assert point["factor_of_safety"] == expected["design"]["factor_of_safety"]
    assert {point["status"] for point in data["sweep"]} == {"PASS", "FAIL"}


@pytest.fixture(scope="module")
def hand_calculation():
    """Load fixed arithmetic answers; never regenerate them from the module."""
    path = Path(__file__).with_name("fixtures") / "module_report_hand_calculation.json"
    return json.loads(path.read_text())


@pytest.mark.parametrize("quantity", [
    "alpha", "unit_skin_friction_kpa", "skin_friction_kn",
    "end_bearing_kn", "total_capacity_kn",
])
def test_worked_results_match_hand_calculation(rebuilt, hand_calculation, quantity):
    """Pin the worked inputs and five answers independently of production code."""
    data = payload(rebuilt[0])
    assert data["inputs"] == hand_calculation["inputs"]
    assert data["result"][quantity] == pytest.approx(
        hand_calculation["result"][quantity], rel=0, abs=1e-9,
    )


@pytest.mark.parametrize("case", ["design", *range(21)])
def test_design_and_chart_match_hand_calculation(rebuilt, hand_calculation, case):
    """Check every capacity, utilization and verdict against fixed answers."""
    data = payload(rebuilt[0])
    assert len(data["sweep"]) == len(hand_calculation["sweep"]) == 21
    actual = data["design"] if case == "design" else data["sweep"][case]
    expected = (hand_calculation["design"] if case == "design"
                else hand_calculation["sweep"][case])
    assert actual.keys() == expected.keys()
    for key in ("length_m", "demand_kn", "factor_of_safety", "status"):
        assert actual[key] == expected[key], key
    for key in ("allowable_capacity_kn", "utilization"):
        assert actual[key] == pytest.approx(expected[key], rel=0, abs=1e-9), key


def test_all_visible_example_numbers_are_bound_to_calculation(rebuilt):
    text = rebuilt[0]
    data = payload(text)
    page = Page(text)
    keys = set()
    for attributes, printed in page.metrics:
        key = attributes["data-quantity"]
        keys.add(key)
        value = resolve(data, key)
        assert float(attributes["data-value"]) == value
        precision = int(attributes["data-precision"])
        assert precision in (1, 2)
        assert printed == f"{value:,.{precision}f}".replace("-", "−")
        assert attributes["data-source"] in {"example", "calculator", "workflow"}
    assert {f"result.{key}" for key in data["result"]} <= keys
    assert {f"inputs.{key}" for key in data["inputs"]} <= keys
    assert {"design.allowable_capacity_kn", "design.demand_kn", "design.utilization",
            "design.factor_of_safety", "design.length_m"} <= keys
    # Prose and result tables in the worked example contain no unbound numerals.
    section = text.split('<h2 id="worked-example">')[1].split('</h2>', 1)[1].split('<h2 id="evidence">')[0]
    section = re.sub(r'<span data-quantity=.*?</span>', "", section, flags=re.S)
    visible = re.sub(r"<[^>]*>", "", section)
    assert not re.search(r"\d", visible)


def test_house_style_offline_resources_and_valid_links(rebuilt):
    text = rebuilt[0]
    page = Page(text)
    assert '<body class="hs-doc">' in text
    brand = (ROOT / "docs/api/_assets/brand.css").read_text().strip()
    assert brand in text
    assert not page.resources
    assert len(page.headings) == 10
    assert len(page.ids) == len(set(page.ids))
    assert all(link[1:] in page.ids for link in page.links if link.startswith("#"))
    for link in page.links:
        if link and not link.startswith(("#", "https:")):
            assert (REPORT / link).is_file(), link
    assert "<b>" not in text and "<strong>" not in text


def test_changes_and_comment_controls(rebuilt):
    text, output = rebuilt
    changes = json.loads((output / "changes.json").read_text())
    assert changes["revision"] == "A"
    assert changes["changes"][0]["section"] == "worked-example"
    assert 'id="changes-panel"' in text and 'id="comments-panel"' in text
    assert "showDirectoryPicker" in text and "createWritable" in text
    assert 'getFileHandle("index.comments.json", {create: true})' in text
    assert "textContent" in text and "JSON.parse" in text
    assert "fetch(" not in text


def test_plot_contains_limit_and_accessible_case_details(rebuilt):
    text = rebuilt[0]
    assert 'id="capacity-chart"' in text
    assert 'id="demand-limit"' in text
    assert 'id="case-selector"' in text
    assert "Applied load limit" in text
    assert "Component: total allowable axial resistance" in text
    assert 'tabindex="0"' in text


def test_report_discloses_provenance_and_boundaries(rebuilt):
    text = rebuilt[0]
    for phrase in ("ASSUMED - to be confirmed", "NOT_EVALUATED", "Revision history",
                   "Not covered", "full circular tip area", "independent pile load-test",
                   "Recommendations", "QA pre-flight"):
        assert phrase in text
    assert not re.search(r"\b(could|should|would)\b", re.sub(r"<style>.*?</style>", "", text, flags=re.S), re.I)


def test_comment_round_trip_and_chart_selection_without_browser(rebuilt, tmp_path):
    """Exercise the shipped JavaScript against a minimal document interface."""
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable for the document interaction check")
    text = rebuilt[0]
    script = re.findall(r"<script>(.*?)</script>", text, re.S)[0]
    calculation = payload(text)
    changes = json.loads((rebuilt[1] / "changes.json").read_text())
    harness = r'''
const assert = require("node:assert/strict");
class Element {
  constructor() { this.value = ""; this.textContent = ""; this.children = []; this.events = {}; }
  addEventListener(name, callback) { this.events[name] = callback; }
  appendChild(child) { this.children.push(child); }
  replaceChildren() { this.children = []; }
  click() { this.clicked = true; }
  remove() {}
}
const elements = new Map();
const get = id => { if (!elements.has(id)) elements.set(id, new Element()); return elements.get(id); };
const document = {
  getElementById: get, querySelectorAll: () => [], createElement: () => new Element(),
  createTextNode: text => ({textContent: text}), body: new Element()
};
const window = {};
get("calculation").textContent = CALCULATION;
get("engineering-changes").textContent = CHANGES;
get("case-selector").value = "10";
get("comment-section").value = "summary";
get("comment-section").options = [{value: "summary"}, {value: "method"}];
SHIPPED_SCRIPT
(async () => {
  get("case-selector").value = "0";
  get("case-selector").events.input();
  assert.match(get("case-readout").textContent, /load check fails/);
  get("case-selector").value = "20";
  get("case-selector").events.input();
  assert.match(get("case-readout").textContent, /load check passes/);
  get("comment-text").value = "<img src=x onerror=alert(1)>";
  get("add-comment").events.click();
  assert.equal(get("comment-list").children[0].textContent, "summary: <img src=x onerror=alert(1)>");
  assert.equal(get("comment-list").children[0].children.length, 0);
  let saved;
  window.showDirectoryPicker = async () => ({getFileHandle: async (name, options) => {
    assert.equal(name, "index.comments.json"); assert.equal(options.create, true);
    return {createWritable: async () => ({write: async text => {saved = text;}, close: async () => {}})};
  }});
  await get("save-comments").events.click();
  const record = JSON.parse(saved);
  assert.equal(record.revision, "A"); assert.equal(record.comments.length, 1);
  const load = value => get("load-comments").events.change({target: {files: [{text: async () => value}]}});
  await load(JSON.stringify({revision: "A", comments: [{section: "method", text: "Check tip condition"}]}));
  assert.equal(get("comment-list").children[0].textContent, "method: Check tip condition");
  await load(JSON.stringify({revision: "older", comments: []}));
  assert.equal(get("comment-list").children[0].textContent, "method: Check tip condition");
  assert.match(get("comment-message").textContent, /retained/);
  await load("invalid json");
  assert.match(get("comment-message").textContent, /retained/);
  delete window.showDirectoryPicker;
  await get("save-comments").events.click();
  assert.match(get("comment-message").textContent, /Download comments/);
  window.showDirectoryPicker = async () => { const error = new Error(); error.name = "AbortError"; throw error; };
  await get("save-comments").events.click();
  assert.match(get("comment-message").textContent, /cancelled/);
  get("download-comments").events.click();
  assert.equal(document.body.children[0].download, "index.comments.json");
})().catch(error => { process.stderr.write(error.message); process.exitCode = 1; });
'''
    harness = harness.replace("CALCULATION", json.dumps(json.dumps(calculation)))
    harness = harness.replace("CHANGES", json.dumps(json.dumps(changes)))
    harness = harness.replace("SHIPPED_SCRIPT", script)
    target = tmp_path / "document-interaction.cjs"
    target.write_text(harness)
    result = subprocess.run([node, str(target)], capture_output=True, text=True, timeout=30)
    assert result.returncode == 0, result.stderr


def test_report_deliverables_are_not_ignored():
    """Keep the three report outputs publishable without broad ignore changes."""
    for name in ("build_report.py", "index.html", "changes.json"):
        result = subprocess.run(["git", "check-ignore", "-q", str(REPORT / name)], cwd=ROOT)
        assert result.returncode == 1
