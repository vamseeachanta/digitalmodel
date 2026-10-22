# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the diffraction report and check the numerical presentation."""

import ast
import importlib.util
import json
import re
import shutil
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/diff"


def load_file(name, path):
    """Load a pure numerical file without importing solver adapters."""
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def generator():
    return load_file("diffraction_module_report", REPORT / "generate_report.py")


@pytest.fixture(scope="module")
def example(generator):
    return generator.compute_example()


class Document(HTMLParser):
    """Collect headings, links, scripts and numerical spans from the page."""

    def __init__(self, source):
        super().__init__()
        self.ids, self.links, self.numbers, self.headings = [], [], [], []
        self.scripts, self.visible = {}, []
        self.current_script = None
        self.current_number = None
        self.hidden = 0
        self.feed(source)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "a":
            self.links.append(attrs.get("href", ""))
        if tag in {"h2", "h3", "h4", "h5", "h6"}:
            self.headings.append((tag, attrs.get("id")))
        if tag in {"script", "style"}:
            self.hidden += 1
        if tag == "script":
            self.current_script = attrs.get("id", "anonymous")
            self.scripts[self.current_script] = ""
        if "data-value-key" in attrs:
            self.current_number = [attrs["data-value-key"], attrs["data-format"], ""]

    def handle_data(self, data):
        if self.current_script:
            self.scripts[self.current_script] += data
        if self.current_number:
            self.current_number[2] += data
        if not self.hidden:
            self.visible.append(data)

    def handle_endtag(self, tag):
        if tag in {"script", "style"}:
            self.hidden -= 1
        if tag == "script":
            self.current_script = None
        if tag == "span" and self.current_number:
            self.numbers.append(self.current_number)
            self.current_number = None


def test_example_is_the_module_output(generator, example):
    """Independently call the module for every case, not copied result pins."""
    numerical = load_file(
        "independent_report_cylinder",
        ROOT / "src/digitalmodel/hydrodynamics/diffraction/analytical_cylinder.py",
    )
    geom = numerical.CylinderGeometry(**example["basis"]["geometry"])
    for row in example["coefficients"]:
        returned = numerical.heave_added_mass_damping(
            row["omega"], geom, example["basis"]["density"],
            gravity=example["basis"]["gravity"], modes=row["modes"],
        )
        for key in ("omega", "modes", "added_mass", "damping", "residual", "period"):
            assert row[key] == getattr(returned, key)
        assert row["added_mass_t"] == returned.added_mass / 1000


def test_public_regression_expectations_and_physics(generator, example):
    """Exercise the existing numerical regression expectations without adapters."""
    tree = ast.parse((ROOT / "tests/hydrodynamics/diffraction/test_analytical_cylinder.py").read_text())
    test = generator.function(tree, "test_regression_values_for_the_reference_geometry")
    bindings = {}
    for node in test.body:
        if isinstance(node, ast.Assign):
            call = node.value
            omega = ast.literal_eval(call.args[0])
            modes = next(ast.literal_eval(kw.value) for kw in call.keywords if kw.arg == "modes")
            bindings[node.targets[0].id] = next(r for r in example["coefficients"] if r["omega"] == omega and r["modes"] == modes)
        elif isinstance(node, ast.Assert):
            comparison = node.test
            result = comparison.left
            expected = comparison.comparators[0]
            relative = next(ast.literal_eval(kw.value) for kw in expected.keywords if kw.arg == "rel")
            assert bindings[result.value.id][result.attr] == pytest.approx(ast.literal_eval(expected.args[0]), rel=relative)
    cylinder = generator.load_numerical("analytical_cylinder")
    basis = example["basis"]
    for row in example["coefficients"]:
        k, _ = cylinder.wavenumbers(row["omega"], basis["geometry"]["water_depth"], 0)
        assert basis["gravity"] * k * generator.math.tanh(k * basis["geometry"]["water_depth"]) == pytest.approx(row["omega"] ** 2, rel=1e-12)
        assert row["damping"] > 0


def test_long_wave_curves_call_every_mode(generator, example):
    limits = load_file(
        "independent_report_long_wave",
        ROOT / "src/digitalmodel/hydrodynamics/diffraction/long_wave_limits.py",
    )
    basis = example["long_wave_basis"]
    assert set(example["long_wave"]) == {"surge", "sway", "heave", "roll", "pitch", "yaw"}
    for mode, rows in example["long_wave"].items():
        for row in rows:
            actual = limits.expected_rao(mode, basis["omega"], row["heading"], basis["depth"])
            assert row["value"] == actual
            assert row["display_value"] == (generator.math.degrees(actual) if mode in {"roll", "pitch", "yaw"} else actual)


def test_report_rebuilds_from_fresh_module_returns(generator, example, tmp_path):
    output = tmp_path / "report.html"
    generator.build_report(example, output)
    rebuilt = Document(output.read_text())
    assert json.loads(rebuilt.scripts["example-data"]) == example
    for key, fmt, displayed in rebuilt.numbers:
        assert displayed == generator.format_number(generator.lookup(example, key), fmt)
    assert json.loads(rebuilt.scripts["chart-data"]) == generator.chart_data(example)


def test_all_worked_values_and_chart_points_are_generated(generator, example):
    doc = Document((REPORT / "report.html").read_text())
    recorded = json.loads(doc.scripts["example-data"])
    assert recorded["basis"] == example["basis"]
    assert len(doc.numbers) > 30
    for key, fmt, displayed in doc.numbers:
        assert displayed == generator.format_number(generator.lookup(example, key), fmt)
    charts = json.loads(doc.scripts["chart-data"])
    expected_charts = generator.chart_data(example)
    for actual, expected in zip(charts, expected_charts, strict=True):
        assert actual.keys() == expected.keys()
        for actual_series, expected_series in zip(actual["series"], expected["series"], strict=True):
            for actual_point, expected_point in zip(actual_series["points"], expected_series["points"], strict=True):
                assert actual_point == pytest.approx(expected_point, rel=1e-10, abs=1e-12)
    for chart in charts:
        assert chart["series"] and chart["units"] and chart["limit_label"]
    assert {c["component"] for c in charts if c["id"].startswith("long-wave-")} == set(example["long_wave"])
    assert any(c["limit"] == 0.0 for c in charts)
    assert any(c["limit"] == example["basis"]["damping_target_percent"] for c in charts)


def test_structure_citations_and_house_style(generator):
    source = (REPORT / "report.html").read_text()
    doc = Document(source)
    assert len([tag for tag, _ in doc.headings if tag == "h2"]) == 10
    assert not any(tag in {"h4", "h5", "h6"} for tag, _ in doc.headings)
    assert len(doc.ids) == len(set(doc.ids))
    for target in doc.links:
        if target.startswith("#"):
            assert target[1:] in doc.ids
        elif not target.startswith("https:"):
            assert (REPORT / target.split("#")[0]).exists()
    assert '<body class="hs-doc">' in source
    brand = (ROOT / "docs/api/_assets/brand.css").read_text()
    assert brand in source
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text()
    assert re.search(r"<style>(.*?)</style>", exemplar, re.S)[1] in source
    assert not re.search(r'<(?:script|link)[^>]+(?:src|href)="https?://', source)
    visible = " ".join(doc.visible)
    assert "Draft for client review" in visible
    assert visible.index("Revision history") < visible.index("2. Introduction")
    assert not re.search(r"\b(could|should|would)\b", visible, re.I)
    assert not re.search(r"<(?:b|strong)\b", source)


def test_comments_changes_and_measurement_evidence(generator, example):
    source = (REPORT / "report.html").read_text()
    doc = Document(source)
    assert json.loads(doc.scripts["changes-data"]) == json.loads((REPORT / "changes.json").read_text())
    assert "showDirectoryPicker" in doc.scripts["report-interactions"]
    assert "createWritable" in doc.scripts["report-interactions"]
    assert "textContent" in doc.scripts["report-interactions"]
    record = json.loads((REPORT / "execution.json").read_text())
    assert record["example_digest"] == generator.example_digest(record["example"])
    assert record["numerical_source_digest"] == generator.numerical_source_digest()
    assert record["example"]["basis"] == example["basis"]
    assert record["case_count"] == len(example["coefficients"])
    assert record["elapsed_seconds"] >= 0


def test_failed_screens_and_rendered_chart_values(generator, example):
    source = (REPORT / "report.html").read_text()
    for chart in generator.chart_data(example):
        assert generator.chart_svg(chart) in source
    failed = [
        (i, key) for i, row in enumerate(example["refinement"]) for key in ("added_mass", "damping")
        if row[key + "_difference_percent"] >= example["basis"][key + "_target_percent"]
    ]
    for i, key in failed:
        sentence = f'The {key.replace("_", " ")} refinement target is not met at {generator.number(example, f"refinement.{i}.omega")} rad/s; further mode refinement is required before that coefficient is adopted as a converged reference.'
        assert source.count(sentence) == 2
    assert "Increase the retained expansion count where a refinement target is not met" in source


def test_example_input_provenance(generator, example):
    """Changing a documented example input must change the report basis."""
    basis = generator.read_basis()
    assert example["basis"] == basis
    assert basis["geometry"] == {"radius": 1.0, "draft": 2.0, "water_depth": 10.0}
    assert basis["mode_counts"] == [40, 120]
    assert basis["frequencies"] == [1.0, 2.0]
    for row in example["refinement"]:
        coarse, fine = [r for r in example["coefficients"] if r["omega"] == row["omega"]]
        for key in ("added_mass", "damping"):
            assert row[key + "_difference_percent"] == 100 * abs(coarse[key] - fine[key]) / abs(fine[key])


def test_interactive_charts_and_comment_round_trip():
    """Exercise native interactions without starting a browser."""
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable for the interaction check")
    source = (REPORT / "report.html").read_text()
    doc = Document(source)
    script = r"""
const assert = require('node:assert/strict');
const vm = require('node:vm');
const input = JSON.parse(require('node:fs').readFileSync(0, 'utf8'));
function element(id = '') {
  return {id, textContent: '', value: '', children: [], style: {}, dataset: {}, handlers: {},
    appendChild(child) { this.children.push(child); },
    replaceChildren() { this.children = []; },
    addEventListener(type, handler) { this.handlers[type] = handler; }, click() {}};
}
const nodes = new Map();
const get = id => { if (!nodes.has(id)) nodes.set(id, element(id)); return nodes.get(id); };
get('changes-data').textContent = JSON.stringify(input.changes);
const point = element(), output = element(), group = element(), toggle = element();
point.dataset.tip = 'Case: reference; component: heave; demand: 1; limit: 1; units: m/m';
point.closest = () => ({querySelector: () => output});
toggle.dataset.toggle = 'series'; toggle.checked = true;
const document = {documentElement: {dataset: {evidence: input.digest}}, getElementById: get,
  createElement: () => element(),
  querySelector: () => group,
  querySelectorAll: selector => selector === '[data-tip]' ? [point] : selector === '[data-toggle]' ? [toggle] : [{id:'detail', textContent:'Detailed results'}]};
let saved = null;
const directory = {async getFileHandle(name, options) {
  assert.equal(name, 'comments.json');
  if (!options) { const error = new Error(); error.name = 'NotFoundError'; throw error; }
  return {async createWritable() { return {async write(value) { saved = JSON.parse(value); }, async close() {}}; }};
}};
const context = {document, window:{showDirectoryPicker: async () => directory},
  URL:{createObjectURL: () => 'blob:test', revokeObjectURL: () => {}}, Blob:class {}};
vm.createContext(context); vm.runInContext(input.script, context);
(async () => {
  point.handlers.focus(); assert.equal(output.textContent, point.dataset.tip);
  toggle.checked = false; toggle.handlers.change(); assert.equal(group.style.display, 'none');
  toggle.checked = true; toggle.handlers.change(); assert.equal(group.style.display, '');
  get('change-since').handlers.change({target:{value:'1'}}); assert.equal(get('change-list').children.length, 0);
  get('change-since').handlers.change({target:{value:'0'}}); assert.ok(get('change-list').children.length > 0);
  get('comment-section').value = 'detail'; get('comment-text').value = '<script>comment</script>';
  get('add-comment').handlers.click();
  assert.equal(get('comment-list').children[0].textContent, 'detail: <script>comment</script>');
  await get('choose-folder').handlers.click(); await get('save-comments').handlers.click();
  assert.equal(saved.evidence, input.digest); assert.equal(saved.comments[0].text, '<script>comment</script>');
  await get('import-comments').handlers.change({target:{files:[{text:async () => JSON.stringify(saved)}]}});
  assert.equal(get('comment-status').textContent, 'Comments are loaded.');
  await get('import-comments').handlers.change({target:{files:[{text:async () => JSON.stringify({evidence:'different',comments:[]})}]}});
  assert.ok(get('comment-status').textContent.startsWith('Comments were not loaded.'));
  context.window.showDirectoryPicker = undefined;
  await get('choose-folder').handlers.click();
  assert.ok(get('comment-status').textContent.includes('Export comments'));
})().catch(error => { console.error(error); process.exitCode = 1; });
"""
    result = subprocess.run(
        [node, "-e", script],
        input=json.dumps({"script": doc.scripts["report-interactions"],
                          "changes": json.loads(doc.scripts["changes-data"]),
                          "digest": re.search(r'data-evidence="([a-f0-9]+)"', source)[1]}),
        capture_output=True, text=True, timeout=30,
    )
    assert result.returncode == 0, result.stderr
