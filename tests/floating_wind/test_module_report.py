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
    """Reproduce the documented workflow and its public screen."""
    source = ROOT / "examples/workflows/floating-wind-sizing/input.yml"
    inputs = yaml.safe_load(source.read_text(encoding="utf-8"))["floating_wind_sizing"]
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
    tree = ast.parse((ROOT / "src/digitalmodel/floating_wind/floaters.py").read_text(encoding="utf-8"))
    function = next(node for node in tree.body if isinstance(node, ast.FunctionDef) and node.name == "_assemble")
    assignment = next(node for node in function.body if isinstance(node, ast.Assign)
                      and any(isinstance(target, ast.Name) and target.id == "ballast_cg_m" for target in node.targets))
    numbers["constant.ballast_fraction"] = assignment.value.left.value
    return inputs, numbers


@pytest.fixture
def hand_calculated_semi():
    """Independent literals for all six example semi-submersibles, in SI units.

    Basis: Ship Hydrostatics and Stability (2nd ed.), sections 2.2--2.6
    and 7.2 (buoyancy, metacentric height, weight calculations):
    https://shop.elsevier.com/books/ship-hydrostatics-and-stability/9780080982878
    University Physics Volume 1 (2016 ed.), sections 10.6 (inertia),
    15.1 (spring equilibrium and periods), 15.6 (forced oscillations):
    https://openstax.org/books/university-physics-volume-1/pages/15-6-forced-oscillations
    Geometry, coefficients and limits remain assumed, based on previous
    experience; these calculations check the proxy, not physical calibration.
    """
    # Hand calculation for D=12.5 m, R=51.75 m, draft=20 m:
    # A = 3*pi*12.5^2/4 = 368.155389093 m^2.
    # V = 10000 + 13*A = 14786.020058203 m^3.
    # Displacement = 1.025*V = 15155.670559658 t.
    # Topside = 1017 + 1263 = 2280 t; steel = 4000 t.
    # Ballast = displacement - 4000 - 2280 = 8875.670559658 t.
    # KB = (10000*3.5 + 13*A*13.5)/V = 6.736854839 m.
    # BM = (3*pi*12.5^4/64 + A*51.75^2/2)/V = 33.583536095 m.
    # Topside z = 20 + (1017*150 + 1263*82.5)/2280 = 132.608552632 m.
    # KG = (4000*9 + ballast*1 + 2280*z)/displacement = 22.910445908 m.
    # GM = KB + BM - KG = 17.409945026 m, above the 1.0 m minimum.
    # I55 = 4000*(9-KG)^2 + ballast*(1-KG)^2 + 2280*(z-KG)^2
    #       + displacement*51.75^2/2 = 52765618.864240 t*m^2.
    # Assumed heave/pitch added ratios: 1.0/0.4; damping ratio: 0.05.
    # Th = 2*pi*sqrt(displacement*1000*2/(1025*9.80665*A)) = 17.982273256 s.
    # Tp = 2*pi*sqrt(I55*1.4/(displacement*9.80665*GM)) = 33.571657858 s.
    # Both periods lie outside the excluded inclusive band [5,16] s.
    # DAF = 1/sqrt((1-r^2)^2 + (2*0.05*r)^2), r=natural/forcing period.
    # Max over both modes and forcing periods [8,11,14] s is 1.509707753,
    # below 2.0; worst offset = max(2400,2800,3000)/400 = 7.5 m < 30 m.
    # The variant passes all four criteria with non-negative ballast.
    # The same substitutions give the other rows; no production calls occur.
    # D, R, displacement (t), ballast (t), GM (m), Th (s), Tp (s), DAF, pass.
    return [
        (11.0, 45.0, 14048.951281399, 7768.951281399, 2.785561463,
         19.674164384, 81.501297429, 1.015292544, True),
        (11.0, 51.75, 14048.951281399, 7768.951281399, 9.577616809,
         19.674164384, 46.087373206, 1.015292544, True),
        (12.5, 45.0, 15155.670559658, 8875.670559658, 9.279681512,
         17.982273256, 43.774356906, 1.509707753, True),
        (12.5, 51.75, 15155.670559658, 8875.670559658, 17.409945026,
         17.982273256, 33.571657858, 1.509707753, True),
        (14.0, 45.0, 16403.673150035, 10123.673150035, 15.579043308,
         16.703579391, 33.085072333, 2.272710628, False),
        (14.0, 51.75, 16403.673150035, 10123.673150035, 25.001729253,
         16.703579391, 27.489758527, 2.272710628, False),
    ]


def assert_hand_calculated_report(document, expected):
    """Check independent quantities and the plotted component verdicts."""
    data = json.loads(document.scripts["report-data"])
    numbers = data["numbers"]
    for index, (diameter, radius, mass, ballast, gm, heave, pitch, daf, passed) in enumerate(expected):
        prefix = f"variant.semi.case_{index:04d}"
        quantities = {
            "params.column_diameter": diameter, "params.column_radius": radius,
            "properties.displacement_t": mass, "properties.total_mass_t": mass,
            "properties.topside_mass_t": 2280.0, "properties.steel_mass_t": 4000.0,
            "properties.ballast_mass_t": ballast, "properties.GM_m": gm,
            "properties.heave_natural_period_s": heave,
            "properties.pitch_natural_period_s": pitch,
            "check.motion.value": daf, "check.motion.limit": 2.0,
            "check.mooring.value": 7.5, "check.mooring.limit": 30.0,
            "check.stability.limit": 1.0,
        }
        for suffix, value in quantities.items():
            key = f"{prefix}.{suffix}"
            assert numbers[key]["value"] == pytest.approx(value, rel=1e-9, abs=1e-8), key
        verdicts = []
        for chart in data["charts"]:
            for point in chart["points"]:
                if point["x"] == f"{prefix}.params.column_diameter":
                    expected_pass = passed if chart["quantity"] == "motion" else True
                    assert point["passed"] is expected_pass
                    verdicts.append(point["passed"])
        assert len(verdicts) == 5  # stability, two modes, motion and mooring
        assert all(verdicts) is passed
    assert numbers["summary.concepts.0.n_passed"]["value"] == 4


def test_report_matches_hand_calculated_semi_submersibles(hand_calculated_semi):
    assert_hand_calculated_report(Document(builder().render_report(ROOT)), hand_calculated_semi)


def test_hand_calculation_detects_shared_calculation_error(monkeypatch, hand_calculated_semi):
    """A period error shared by rendering and reproduction must be detected."""
    original = floaters._heave_period
    monkeypatch.setattr(floaters, "_heave_period", lambda *args: original(*args) * 1.1)
    document = Document(builder().render_report(ROOT))
    reproduced = reference_numbers()[1]
    key = "variant.semi.case_0000.properties.heave_natural_period_s"
    assert json.loads(document.scripts["report-data"])["numbers"][key]["value"] == reproduced[key]
    with pytest.raises(AssertionError, match="heave_natural_period_s"):
        assert_hand_calculated_report(document, hand_calculated_semi)


def test_report_rebuilds_exactly(tmp_path):
    builder().build_report(ROOT, tmp_path)
    for filename in ("report.html", "changes.json"):
        assert (tmp_path / filename).read_bytes() == (ROOT / DIRECTORY / filename).read_bytes()


def test_every_displayed_example_number_matches_module(tmp_path):
    builder().build_report(ROOT, tmp_path)
    document = Document((tmp_path / "report.html").read_text(encoding="utf-8"))
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
    text = (tmp_path / "report.html").read_text(encoding="utf-8")
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
    changes = json.loads((tmp_path / "changes.json").read_text(encoding="utf-8"))
    assert json.loads(document.scripts["report-changes"]) == changes
    assert changes["revisions"][0]["changes"]
    assets = (ROOT / "src/digitalmodel/reporting/assets/calc_report_format.html").read_text(encoding="utf-8")
    style = assets.split("<style>", 1)[1].split("</style>", 1)[0]
    assert style in text
    import re
    from xml.etree import ElementTree

    for image in re.findall(r"<svg\b.*?</svg>", text, re.S):
        ElementTree.fromstring(image)
        assert "<span " not in image


def test_charts_preserve_values_and_limits(tmp_path):
    builder().build_report(ROOT, tmp_path)
    document = Document((tmp_path / "report.html").read_text(encoding="utf-8"))
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
