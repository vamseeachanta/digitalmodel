# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the diffraction report and check the numerical presentation."""

import ast
import copy
from datetime import datetime, timezone
import importlib.util
import json
import math
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
    tree = ast.parse((ROOT / "tests/hydrodynamics/diffraction/test_analytical_cylinder.py").read_text(encoding="utf-8"))
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
    rebuilt = Document(output.read_text(encoding="utf-8"))
    assert json.loads(rebuilt.scripts["example-data"]) == example
    for key, fmt, displayed in rebuilt.numbers:
        assert displayed == generator.format_number(generator.lookup(example, key), fmt)
    assert json.loads(rebuilt.scripts["chart-data"]) == generator.chart_data(example)
    assert normalized_report(output.read_text(encoding="utf-8")) == normalized_report(
        (REPORT / "report.html").read_text(encoding="utf-8")
    )


def normalized_report(source):
    """Mask raw round-off payloads while retaining narrative and plotted output."""
    source = re.sub(r'data-evidence="[a-f0-9]+"', 'data-evidence="masked"', source)
    return re.sub(
        r'(<script type="application/json" id="(?:example|chart)-data">).*?(</script>)',
        r'\1masked\2', source, flags=re.S,
    )


def test_all_worked_values_and_chart_points_are_generated(generator, example):
    doc = Document((REPORT / "report.html").read_text(encoding="utf-8"))
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
    source = (REPORT / "report.html").read_text(encoding="utf-8")
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
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    assert brand in source
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    assert re.search(r"<style>(.*?)</style>", exemplar, re.S)[1] in source
    assert not re.search(r'<(?:script|link)[^>]+(?:src|href)="https?://', source)
    visible = " ".join(doc.visible)
    assert "Draft for maintainer review" in visible
    assert visible.index("Revision history") < visible.index("2. Introduction")
    assert not re.search(r"\b(could|should|would)\b", visible, re.I)
    assert not re.search(r"<(?:b|strong)\b", source)


def test_comments_changes_and_measurement_evidence(generator, example):
    source = (REPORT / "report.html").read_text(encoding="utf-8")
    doc = Document(source)
    assert json.loads(doc.scripts["changes-data"]) == json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    assert "showDirectoryPicker" in doc.scripts["report-interactions"]
    assert "createWritable" in doc.scripts["report-interactions"]
    assert "textContent" in doc.scripts["report-interactions"]
    record = json.loads((REPORT / "execution.json").read_text(encoding="utf-8"))
    assert record["example_digest"] == generator.example_digest(record["example"])
    assert record["numerical_source_digest"] == generator.numerical_source_digest()
    assert record["example"]["basis"] == example["basis"]
    assert record["case_count"] == len(example["coefficients"])
    assert record["elapsed_seconds"] >= 0


def test_failed_screens_and_rendered_chart_values(generator, example):
    source = (REPORT / "report.html").read_text(encoding="utf-8")
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
    source = (REPORT / "report.html").read_text(encoding="utf-8")
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
  get('change-since').handlers.change({target:{value:Math.max(...input.changes.revisions.map(r => r.revision))}}); assert.equal(get('change-list').children.length, 0);
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


def test_source_register_does_not_claim_an_unretained_retrieval(generator, example, tmp_path):
    output = tmp_path / "report.html"
    generator.build_report(example, output)
    visible = " ".join(Document(output.read_text(encoding="utf-8")).visible)
    assert "retrieval time and successful response are retained" not in visible


def test_generator_uses_utf8_and_lf_with_legacy_defaults(generator, example, tmp_path, monkeypatch):
    """A legacy text locale must not change sources, cached records or HTML."""
    read_text, write_text = Path.read_text, Path.write_text

    def legacy_read(path, encoding=None, errors=None):
        return read_text(path, encoding=encoding or "cp1252", errors=errors)

    def legacy_write(path, data, encoding=None, errors=None, newline=None):
        return write_text(path, data, encoding=encoding or "cp1252", errors=errors,
                          newline="\r\n" if newline is None else newline)

    monkeypatch.setattr(Path, "read_text", legacy_read)
    monkeypatch.setattr(Path, "write_text", legacy_write)
    cache = tmp_path / "cache"
    assert generator.compute_example(cache) == example
    assert generator.compute_example(cache) == example
    output = tmp_path / "report.html"
    generator.build_report(example, output)
    assert "ρ" in output.read_bytes().decode("utf-8")
    for path in [output, *cache.glob("*.json")]:
        assert b"\r\n" not in path.read_bytes()


@pytest.mark.parametrize("without_sysconf", [False, True])
def test_measurement_uses_portable_memory_and_current_utc_date(
    generator, tmp_path, monkeypatch, without_sysconf,
):
    """Measurement uses a disposable report directory, never the retained run."""
    import psutil

    memory = psutil.virtual_memory().total

    class FixedClock(datetime):
        @classmethod
        def now(cls, tz=None):
            assert tz is timezone.utc
            return cls(2031, 4, 5, tzinfo=tz)

    shutil.copyfile(REPORT / "changes.json", tmp_path / "changes.json")
    monkeypatch.setattr(generator, "HERE", tmp_path)
    monkeypatch.setattr(generator, "datetime", FixedClock, raising=False)
    if without_sysconf:
        monkeypatch.delattr(generator.os, "sysconf", raising=False)
    output = tmp_path / "report.html"
    monkeypatch.setattr(sys, "argv", ["generate_report.py", "--measure", "--cache-dir",
                                     str(tmp_path / "cache"), "--output", str(output)])
    generator.main()
    record_bytes = (tmp_path / "execution.json").read_bytes()
    record = json.loads(record_bytes.decode("utf-8"))
    assert record["date"] == "2031-04-05"
    assert record["memory_gib"] == round(memory / 1024 ** 3, 1)
    assert record["example_digest"] == generator.example_digest(record["example"])
    assert b"\r\n" not in record_bytes
    assert output.is_file()


@pytest.mark.parametrize("direction", [math.inf, -math.inf])
def test_comment_identity_survives_numerical_roundoff(generator, example, tmp_path, direction):
    changed = copy.deepcopy(example)

    def perturb(value):
        if isinstance(value, float):
            return math.nextafter(value, direction) if value else value
        if isinstance(value, dict):
            return {key: perturb(item) for key, item in value.items()}
        if isinstance(value, list):
            return [perturb(item) for item in value]
        return value

    for key in changed.keys() - {"basis", "long_wave_basis"}:
        changed[key] = perturb(changed[key])
    original, rebuilt = tmp_path / "original.html", tmp_path / "rebuilt.html"
    generator.build_report(example, original)
    generator.build_report(changed, rebuilt)
    identity = lambda path: re.search(r'data-evidence="([a-f0-9]+)"', path.read_text(encoding="utf-8"))[1]
    assert generator.example_digest(example) != generator.example_digest(changed)
    assert identity(original) == identity(rebuilt)


def test_comment_identity_changes_when_displayed_results_change(generator, example, tmp_path):
    changed = copy.deepcopy(example)
    changed["coefficients"][0]["damping"] += 1
    outputs = [tmp_path / "original.html", tmp_path / "changed.html"]
    for data, output in zip((example, changed), outputs, strict=True):
        generator.build_report(data, output)
    identities = [re.search(r'data-evidence="([a-f0-9]+)"', p.read_text(encoding="utf-8"))[1]
                  for p in outputs]
    assert identities[0] != identities[1]


def test_cached_evidence_still_rejects_full_precision_tampering(generator, tmp_path):
    generator.compute_example(tmp_path)
    path = next(tmp_path.glob("*.json"))
    record = json.loads(path.read_text(encoding="utf-8"))
    record["result"]["damping"] = math.nextafter(record["result"]["damping"], math.inf)
    path.write_text(json.dumps(record), encoding="utf-8", newline="\n")
    with pytest.raises(ValueError, match="Cached numerical evidence digest mismatch"):
        generator.compute_example(tmp_path)


def test_refinement_uses_mode_extrema_independent_of_order(generator, example, tmp_path, monkeypatch):
    basis = copy.deepcopy(example["basis"])
    basis["mode_counts"].reverse()
    monkeypatch.setattr(generator, "read_basis", lambda: basis)
    result = generator.compute_example(tmp_path)
    assert result["refinement"] == example["refinement"]


def test_revision_history_renders_appended_entries(generator, example, tmp_path, monkeypatch):
    changes = json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    changes["revisions"].append({"revision": 99, "date": "2031-04-05",
                               "changes": ["Report portability is corrected."]})
    (tmp_path / "changes.json").write_text(json.dumps(changes), encoding="utf-8", newline="\n")
    shutil.copyfile(REPORT / "execution.json", tmp_path / "execution.json")
    monkeypatch.setattr(generator, "HERE", tmp_path)
    output = tmp_path / "report.html"
    generator.build_report(example, output)
    source = output.read_text(encoding="utf-8")
    assert '<td>99</td><td>2031-04-05</td>' in source
    assert '<option value="99">' in source
    assert "Initial module analysis; calculated cylinder evidence is distinguished from unassessed design behaviour." in source
