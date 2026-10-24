# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the lifting-lug report and compare its evidence to public calls."""

import copy
import importlib.util
import json
import os
import re
import shutil
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

from digitalmodel.lifting_lug import workflow

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/liftinglug"
EXAMPLE = ROOT / "examples/workflows/lifting-lug-design/input.yml"
REBUILD = ("PYTHONPATH=src python docs/reports/modules/liftinglug/build_report.py "
           "--output-dir docs/reports/modules/liftinglug --work-dir CALCULATION_DIRECTORY")

# Hand calculation of the public example, independent of the module constants:
# P = 420 * 2 * 1.1 = 924 kN; t = 40 + 2 * 0 = 40 mm; l = 110 - 80/2 = 70 mm.
# Bearing area = 75 * 40 = 3000 mm2; net/shear area = 2 * 40 * 70 = 5600 mm2.
# Demands = 924000/3000 = 308 MPa and 924000/5600 = 165 MPa.
# Implementation limits = 355 * (0.90, 0.60, 0.40) = (319.5, 213, 142) MPa;
# their design-code applicability remains unestablished, as stated in the report.
# Percent demands = 100 * (308/319.5, 165/213, 165/142) = (96.4, 77.5, 116.2).
# Exceedance = 100 * (165/142 - 1) = 16.2 %; cap = floor(420 * 142/165 * 10)/10
# = 361.4 kN. These are display literals, with a half-last-place tolerance.
EXAMPLE_NUMBERS = {
    "lug.plate_thickness_mm": 40.0, "lug.cheek_plate_thickness_mm": 0.0,
    "lug.pin_diameter_mm": 75.0, "lug.hole_diameter_mm": 80.0,
    "lug.outer_radius_mm": 110.0, "lug.yield_strength_mpa": 355.0,
    "load.num_slings": 2, "load.static_load_kN": 420.0,
    "load.dynamic_amplification_factor": 2.0, "load.skew_load_factor": 1.1,
    "design_load_kN": 924.0, "total_thickness_mm": 40.0,
    "skew_load_factor": 1.1, "skew_load_factor_minimum": 1.1,
    "pin_bearing.demand_mpa": 308.0, "pin_bearing.allowable_mpa": 319.5,
    "pin_bearing.utilization": 96.4,
    "net_tension.demand_mpa": 165.0, "net_tension.allowable_mpa": 213.0,
    "net_tension.utilization": 77.5,
    "shear_tearout.demand_mpa": 165.0, "shear_tearout.allowable_mpa": 142.0,
    "shear_tearout.utilization": 116.2,
    "bearing_factor": 0.90, "tension_factor": 0.60, "shear_factor": 0.40,
    "limit_percent": 100.0, "governing_percent": 116.2,
    "exceedance_percent": 16.2, "ligament_mm": 70.0, "static_cap_kN": 361.4,
}


class Page(HTMLParser):
    """Read the visible numerical fields and embedded chart evidence."""

    def __init__(self, text):
        super().__init__()
        self.numbers = []
        self.ids = set()
        self.links = []
        self.headings = []
        self.resources = []
        self.current = None
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.add(attrs["id"])
        if tag == "a":
            self.links.append(attrs.get("href", ""))
        if tag == "h2":
            self.headings.append(attrs.get("id"))
        if tag in ("script", "link", "img") and ("src" in attrs or "href" in attrs):
            self.resources.append(attrs.get("src", attrs.get("href")))
        if "data-number" in attrs:
            self.current = [attrs["data-number"], ""]

    def handle_data(self, data):
        if self.current is not None:
            self.current[1] += data

    def handle_endtag(self, tag):
        if tag == "span" and self.current is not None:
            self.numbers.append(tuple(self.current))
            self.current = None


@pytest.fixture
def rebuilt(tmp_path):
    spec = importlib.util.spec_from_file_location("lug_report", REPORT / "build_report.py")
    builder = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(builder)
    output = tmp_path / "report"
    builder.build(output, tmp_path / "calculations")
    text = (output / "lifting-lug-analysis.html").read_text(encoding="utf-8")
    return builder, output, text


def direct_result(tmp_path):
    cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    cfg["lifting_lug"]["outputs"]["directory"] = tmp_path.as_posix()
    return workflow.router(cfg)["lifting_lug"]


def test_rebuild_matches_committed_report_and_changes(rebuilt):
    _, output, _ = rebuilt
    for name in ("lifting-lug-analysis.html", "changes.json"):
        assert (output / name).read_bytes() == (REPORT / name).read_bytes(), (
            f"Rebuild with: {REBUILD}")


def test_report_writes_lf_even_with_windows_text_defaults(rebuilt, tmp_path, monkeypatch):
    builder = rebuilt[0]
    real_write = Path.write_text

    def windows_write(path, text, encoding=None, errors=None, newline=None):
        return real_write(path, text, encoding=encoding, errors=errors,
                          newline="\r\n" if newline is None else newline)

    monkeypatch.setattr(Path, "write_text", windows_write)
    output = tmp_path / "windows-output"
    builder.build(output, tmp_path / "windows-calculations")
    for name in ("lifting-lug-analysis.html", "changes.json"):
        payload = (output / name).read_bytes()
        assert b"\r" not in payload, f"Explicit LF output required: {name}"
        assert payload == (REPORT / name).read_bytes(), f"Rebuild with: {REBUILD}"


@pytest.mark.parametrize("name", ["lifting-lug-analysis.html", "changes.json"])
def test_report_goldens_keep_lf_with_crlf_checkout(rebuilt, name):
    """Apply Git's checkout filters without changing the index or working tree."""
    env = {key: value for key, value in os.environ.items()
           if not key.startswith("GIT_")}
    git = ["git", "-c", "core.autocrlf=true", "-c", "core.eol=crlf",
           "-c", "core.attributesFile="]
    path = (REPORT / name).relative_to(ROOT).as_posix()
    blob = f"HEAD:{path}"
    expected = (rebuilt[1] / name).read_bytes()
    # The same blob at an unprotected path proves CRLF conversion is active.
    control = subprocess.run(
        [*git, "cat-file", "--filters", f"--path=checkout-control/{name}", blob],
        cwd=ROOT, env=env, capture_output=True, check=True).stdout
    assert b"\r\n" in control
    assert control.replace(b"\r\n", b"\n") == expected
    checkout = subprocess.run([*git, "cat-file", "--filters", blob],
                              cwd=ROOT, env=env, capture_output=True, check=True).stdout
    assert b"\r" not in checkout, f"Checkout must preserve LF: {path}"
    assert checkout == expected, f"Rebuild with: {REBUILD}"


def test_every_displayed_example_number_matches_hand_calculation(rebuilt):
    _, _, text = rebuilt
    page = Page(text)
    expected = EXAMPLE_NUMBERS
    assert {key for key, _ in page.numbers} == set(expected)
    assert len(page.numbers) > len(expected)
    for key, displayed in page.numbers:
        assert float(displayed) == pytest.approx(expected[key], abs=0.05), key
    # Every decimal visible outside the chart is a labelled numerical field.
    stripped = re.sub(r'<span data-number="[^"]+">[^<]+</span>', "", text)
    stripped = re.sub(r"<(script|style|svg)\b.*?</\1>", "", stripped, flags=re.S)
    stripped = re.sub(r"<[^>]+>", "", stripped)
    # Section numbers and clause locators identify text, not engineering data.
    stripped = re.sub(r"\b9\.[12]\b|§\d+(?:\.\d+)+", "", stripped)
    assert not re.search(r"\b\d+\.\d+\b", stripped)
    assert "OrcFxAPI" not in sys.modules


@pytest.mark.parametrize("thickness,displayed", [(0.0254, "0.0254"),
                                               (0.0000254, "0.0000254")])
def test_input_precision_and_engineering_formats(rebuilt, tmp_path, thickness, displayed):
    builder = rebuilt[0]
    cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    cfg["lifting_lug"]["lug"]["plate_thickness_mm"] = thickness
    cfg["lifting_lug"]["lug"]["cheek_plate_thickness_mm"] = 0.1
    cfg["lifting_lug"]["load"]["dynamic_amplification_factor"] = 2.1234
    cfg["lifting_lug"]["load"]["skew_load_factor"] = 1.33
    result = builder.evaluate(cfg, tmp_path / "precision")
    values = builder.numerical_fields(result)
    chart = builder.chart_cases(cfg, values, tmp_path / "precision-sweep")
    numbers = dict(Page(builder.render(result, values, chart)).numbers)
    assert numbers["lug.plate_thickness_mm"] == displayed
    assert numbers["load.dynamic_amplification_factor"] == "2.1234"
    assert numbers["load.skew_load_factor"] == "1.33"
    baseline = dict(Page(rebuilt[2]).numbers)
    assert baseline["lug.plate_thickness_mm"] == "40.000"
    assert baseline["lug.cheek_plate_thickness_mm"] == "0.000"
    assert baseline["total_thickness_mm"] == "40.000"
    assert baseline["load.num_slings"] == "2"


@pytest.mark.parametrize("section,key,value", [
    ("lug", "pin_diameter_mm", -1.0),
    ("lug", "pin_diameter_mm", float("nan")),
    ("lug", "yield_strength_mpa", float("inf")),
    ("lug", "cheek_plate_thickness_mm", -1.0),
    ("load", "static_load_kN", float("nan")),
    ("load", "dynamic_amplification_factor", None),
    ("load", "dynamic_amplification_factor", True),
])
def test_report_rejects_invalid_inputs_before_calculation(rebuilt, tmp_path,
                                                        section, key, value):
    cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    cfg["lifting_lug"][section][key] = value
    output = tmp_path / "invalid-calculation"
    with pytest.raises(ValueError, match=f"Invalid report input: {section}.{key}"):
        rebuilt[0].evaluate(cfg, output)
    assert not output.exists()


def test_report_rejects_missing_amplification_instead_of_using_a_default(rebuilt, tmp_path):
    cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    del cfg["lifting_lug"]["load"]["dynamic_amplification_factor"]
    with pytest.raises(ValueError, match="Invalid report input: load.dynamic_amplification_factor"):
        rebuilt[0].evaluate(cfg, tmp_path / "missing-input")


def test_report_ignore_rules_preserve_sibling_exceptions(tmp_path):
    """An earlier sibling exception survives; unlisted outputs stay ignored."""
    env = {key: value for key, value in os.environ.items()
           if not key.startswith("GIT_")}
    subprocess.run(["git", "init", "--quiet", tmp_path.as_posix()], env=env, check=True)
    rules = (ROOT / ".gitignore").read_text(encoding="utf-8")
    sibling = ("!docs/reports/modules/\ndocs/reports/modules/*/*\n"
               "!docs/reports/modules/sibling/\n"
               "!docs/reports/modules/sibling/report.html\n")
    rules = rules.replace("# Keep the reproducible lifting lug module report and its source.",
                          sibling + "# Keep the reproducible lifting lug module report and its source.")
    (tmp_path / ".gitignore").write_text(rules, encoding="utf-8", newline="\n")
    paths = ["docs/reports/modules/sibling/report.html",
             "docs/reports/modules/liftinglug/lifting-lug-analysis.html",
             "docs/reports/modules/sibling/unlisted.json",
             "docs/reports/modules/liftinglug/unlisted.json"]
    checked = subprocess.run(["git", "check-ignore", "--no-index", "--stdin"],
                             input="\n".join(paths) + "\n", cwd=tmp_path, env=env,
                             encoding="utf-8", capture_output=True, check=False)
    assert checked.returncode in (0, 1), checked.stderr
    assert set(checked.stdout.splitlines()) == set(paths[2:])


def chart_data(text):
    match = re.search(r'<script id="chart-data" type="application/json">(.*?)</script>',
                      text, re.S)
    return json.loads(match[1])


def test_every_chart_point_is_a_module_call(rebuilt, tmp_path):
    data = chart_data(rebuilt[2])
    assert data["limit_percent"] == 100.0
    assert len(data["cases"]) > 10
    for index, case in enumerate(data["cases"]):
        cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
        cfg["lifting_lug"]["load"]["static_load_kN"] = case["static_load_kN"]
        cfg["lifting_lug"]["outputs"]["directory"] = (tmp_path / str(index)).as_posix()
        result = workflow.router(cfg)["lifting_lug"]
        assert case["status"] == result["screening_status"]
        for actual, plotted in zip(result["checks"], case["checks"], strict=True):
            assert plotted["component"] == actual["name"]
            assert plotted["demand_mpa"] == pytest.approx(actual["demand_mpa"], abs=0.050001)
            assert plotted["allowable_mpa"] == pytest.approx(actual["allowable_mpa"], abs=0.050001)
            assert plotted["utilization_percent"] == pytest.approx(actual["utilization"] * 100, abs=0.050001)
        # At each load, P = 2200 * static kN in N; areas and limits are the
        # hand-worked 3000/5600 mm2 and 319.5/213/142 MPa above.
        for check, area, limit in zip(case["checks"], (3000, 5600, 5600),
                                     (319.5, 213.0, 142.0), strict=True):
            demand = 2200 * case["static_load_kN"] / area
            assert check["demand_mpa"] == pytest.approx(demand, abs=0.050001)
            assert check["allowable_mpa"] == pytest.approx(limit)
            assert check["utilization_percent"] == pytest.approx(100 * demand / limit, abs=0.050001)
    assert {case["status"] for case in data["cases"]} == {"pass", "fail"}


def test_recommended_load_is_rounded_down_and_passes(rebuilt, tmp_path):
    result = direct_result(tmp_path / "original")
    cap = float(dict(Page(rebuilt[2]).numbers)["static_cap_kN"])
    assert cap == pytest.approx(361.4)
    cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    cfg["lifting_lug"]["load"]["static_load_kN"] = cap
    cfg["lifting_lug"]["outputs"]["directory"] = (tmp_path / "limited").as_posix()
    limited = workflow.router(copy.deepcopy(cfg))["lifting_lug"]
    assert limited["screening_status"] == "pass"
    assert cap <= result["load"]["static_load_kN"] / result["governing_utilization"]
    cfg["lifting_lug"]["load"]["static_load_kN"] = cap + 0.1
    assert workflow.router(cfg)["lifting_lug"]["screening_status"] == "fail"


def test_report_is_offline_house_style_with_resolving_citations(rebuilt):
    builder, _, text = rebuilt
    page = Page(text)
    assert 'class="hs-doc"' in text
    assert len(page.headings) == 10
    assert not page.resources
    assert builder.house_style() in text
    for link in page.links:
        if link.startswith("#"):
            assert link[1:] in page.ids
        elif not link.startswith("https:"):
            assert (REPORT / link).resolve().is_file(), link
    assert "Draft for maintainer review" in text
    assert '>3 Load Sensitivity</h2>' in text
    assert "NOT_EVALUATED" in text
    assert "ASSUMED - to be confirmed" in text
    assert "No field-load measurements" in text
    assert not re.search(r"<(?:b|strong)\b", text)


def test_comment_and_change_panels_are_embedded(rebuilt):
    _, output, text = rebuilt
    changes = json.loads((output / "changes.json").read_text(encoding="utf-8"))
    embedded = re.search(r'<script id="changes-data" type="application/json">(.*?)</script>',
                         text, re.S)
    assert json.loads(embedded[1]) == changes
    assert "showSaveFilePicker" in text
    assert 'download = "lifting-lug-analysis.comments.json"' in text
    assert 'id="import-comments"' in text
    assert "textContent" in text
    assert "localStorage" not in text


def test_report_numbers_follow_changed_module_results(rebuilt, tmp_path, monkeypatch):
    builder, _, _ = rebuilt
    unchanged = direct_result(tmp_path / "unchanged")["checks"][-1]["allowable_mpa"]
    monkeypatch.setattr(builder.workflow, "SHEAR_FACTOR", builder.workflow.SHEAR_FACTOR * 0.95)
    cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    result = builder.evaluate(cfg, tmp_path / "changed")
    values = builder.numerical_fields(result)
    chart = builder.chart_cases(cfg, values, tmp_path / "sweep")
    displayed = dict(Page(builder.render(result, values, chart)).numbers)
    assert float(displayed["shear_tearout.allowable_mpa"]) == round(result["checks"][-1]["allowable_mpa"], 1)
    assert float(displayed["shear_tearout.allowable_mpa"]) != unchanged


def test_interactive_selection_and_comment_file_roundtrip(rebuilt, tmp_path):
    """Exercise the shipped JavaScript with a small DOM, without a browser."""
    node = shutil.which("node")
    if node is None:
        pytest.skip("JavaScript runtime is unavailable")
    builder, _, text = rebuilt
    initial = re.search(r'id="case-slider"[^>]*value="(\d+)"', text)[1]
    payload = {
        "code": builder.INTERACTION,
        "chart": chart_data(text),
        "changes": builder.CHANGES,
        "initial": initial,
        "sections": re.findall(r'<h2 id="([^"]+)">([^<]+)</h2>', text),
    }
    harness = r'''
const assert = require("node:assert/strict");
const vm = require("node:vm");
const input = JSON.parse(require("node:fs").readFileSync(0, "utf8"));
class Element {
  constructor(tag) { this.tag = tag; this.children = []; this.events = {}; this.value = ""; this.textContent = ""; }
  setAttribute(key, value) { this[key] = value; }
  append(child) { this.children.push(child); }
  replaceChildren(...children) { this.children = children; }
  addEventListener(event, callback) { this.events[event] = callback; }
  click() { this.clicked = true; }
  set innerHTML(value) { throw new Error("Untrusted HTML insertion"); }
}
const nodes = new Map();
const get = id => { if (!nodes.has(id)) nodes.set(id, new Element("div")); return nodes.get(id); };
get("chart-data").textContent = JSON.stringify(input.chart);
get("changes-data").textContent = JSON.stringify(input.changes);
get("case-slider").value = input.initial;
const headings = input.sections.map(([id, text]) => { const h = get(id); h.id = id; h.textContent = text; return h; });
let saved = "", downloaded = "", revoked = false;
const document = {getElementById: get, querySelectorAll: () => headings,
  createElement: tag => new Element(tag), createElementNS: (ns, tag) => new Element(tag)};
const window = {confirm: () => true, showSaveFilePicker: async () => ({createWritable: async () => ({
  write: async payload => { saved = payload; }, close: async () => {}
})})};
const context = {document, window, setTimeout: fn => fn(),
  Blob: class {constructor(parts) { downloaded = parts.join(""); }},
  URL: {createObjectURL: () => "blob:comments", revokeObjectURL: () => { revoked = true; }}};
vm.runInNewContext(input.code, context);
(async () => {
  assert.match(get("chart-readout").textContent, /demand: .* MPa; limit: .* MPa/);
  get("case-slider").value = "0"; get("case-slider").events.input();
  assert.match(get("chart-readout").textContent, /The screen passes/);
  const points = get("load-chart").children[0].children.filter(c => c.tag === "circle");
  points[points.length - 1].events.focus();
  assert.match(get("chart-readout").textContent, /The screen fails/);
  get("comment-section").value = headings[0].id;
  get("comment-text").value = "<img onerror=invalid> Check the load basis.";
  get("add-comment").events.click();
  await get("save-comments").events.click();
  const exported = JSON.parse(saved);
  assert.equal(exported.comments[0].text, "<img onerror=invalid> Check the load basis.");
  assert.equal(exported.comments[0].section, headings[0].id);
  assert.equal(exported.revision, input.changes.revisions[0].revision);
  assert.match(get("comment-status").textContent, /have been saved/);
  await get("import-comments").events.change({target: {files: [{text: async () => saved}]}});
  assert.match(get("comment-status").textContent, /have been imported/);
  assert.match(get("comment-list").children[0].textContent, /<img onerror=invalid>/);
  const invalid = {...exported, revision: "unknown"};
  await get("import-comments").events.change({target: {files: [{text: async () => JSON.stringify(invalid)}]}});
  assert.match(get("comment-status").textContent, /not a comments file/);
  assert.equal(get("comment-list").children.length, 1);
  window.showSaveFilePicker = undefined;
  await get("save-comments").events.click();
  assert.equal(downloaded, saved); assert.equal(revoked, true);
  window.showSaveFilePicker = async () => { throw new Error("Cancelled"); };
  await get("save-comments").events.click();
  assert.match(get("comment-status").textContent, /were not saved/);
})().catch(error => { console.error(error); process.exitCode = 1; });
'''
    env = os.environ.copy()
    for name in ("UV_TOOL_DIR", "PRE_COMMIT_HOME", "XDG_CACHE_HOME", "npm_config_cache"):
        env[name] = (tmp_path / name.lower()).as_posix()
    result = subprocess.run([node, "-e", harness], input=json.dumps(payload), text=True,
                            encoding="utf-8", capture_output=True, env=env, timeout=30)
    assert result.returncode == 0, result.stderr
