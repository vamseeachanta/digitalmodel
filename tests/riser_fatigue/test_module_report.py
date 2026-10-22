# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the public example report against the real fatigue workflow."""

import copy
import csv
import os
import importlib.util
import json
import math
import re
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

from digitalmodel.riser_fatigue.workflow import router


ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/riserfatigue"
SPEC = importlib.util.spec_from_file_location("module_report", REPORT / "generate_report.py")


def generator():
    module = importlib.util.module_from_spec(SPEC)
    SPEC.loader.exec_module(module)
    return module


class Metrics(HTMLParser):
    """Collect every displayed calculation and the embedded calculation record."""

    def __init__(self):
        super().__init__()
        self.metrics = []
        self.record = ""
        self.in_record = False
        self.current = None
        self.external_assets = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if (tag == "script" and "src" in attrs) or (tag == "link" and attrs.get("rel") == "stylesheet"):
            self.external_assets.append(attrs)
        if "data-metric" in attrs:
            self.current = [attrs, ""]
        if tag == "script" and attrs.get("id") == "calculation-record":
            self.in_record = True

    def handle_data(self, data):
        if self.current is not None:
            self.current[1] += data
        if self.in_record:
            self.record += data

    def handle_endtag(self, tag):
        if tag == "span" and self.current is not None:
            self.metrics.append(self.current)
            self.current = None
        if tag == "script":
            self.in_record = False


@pytest.fixture
def built(tmp_path):
    module = generator()
    record = module.build_payload(tmp_path / "calculation")
    return module, record, module.render(record)


def test_all_example_results_equal_direct_module_returns(built, tmp_path):
    _, record, _ = built
    cfg = yaml.safe_load((ROOT / "examples/workflows/riser-fatigue/input.yml").read_text(encoding="utf-8"))
    original = copy.deepcopy(cfg["riser_fatigue"])
    cfg["riser_fatigue"]["output_dir"] = str(tmp_path / "direct")
    result = router(cfg)["riser_fatigue"]
    assert record["inputs"] == {k: v for k, v in original.items() if k != "output_dir"}
    assert record["result"] == {
        k: v for k, v in result.items() if k not in {"results_csv", "summary_csv"}
    }
    with Path(result["results_csv"]).open(encoding="utf-8") as stream:
        rows = list(csv.DictReader(stream))
    assert len(record["bins"]) == len(rows)
    for actual, expected in zip(record["bins"], rows):
        assert actual.keys() == expected.keys()
        for key, value in expected.items():
            if key in {"segment_id", "contribution"}:
                assert actual[key] == value
            elif math.isnan(float(value)):
                assert actual[key] is None
            else:
                assert actual[key] == pytest.approx(float(value), rel=1e-10)


def test_every_displayed_metric_uses_the_calculation_record(built):
    module, record, html = built
    parser = Metrics()
    parser.feed(html)
    assert json.loads(parser.record) == record
    assert parser.metrics
    used = set()
    for attrs, visible in parser.metrics:
        key = attrs["data-metric"]
        value = module.lookup(record, key)
        assert float(attrs["data-value"]) == value
        assert visible == module.format_number(value, attrs["data-format"])
        used.add(key)
    # All segment results and every numeric bin field are visible in the appendix.
    for index, segment in enumerate(record["result"]["segments"]):
        for key, value in segment.items():
            if isinstance(value, (float, int)) and not isinstance(value, bool):
                assert f"result.segments.{index}.{key}" in used
    for index, row in enumerate(record["bins"]):
        for key, value in row.items():
            if isinstance(value, (float, int)):
                assert f"bins.{index}.{key}" in used


REBUILD = (
    "PYTHONPATH=src uv run python docs/reports/modules/riserfatigue/generate_report.py "
    "--output-dir docs/reports/modules/riserfatigue --scratch-dir <scratch>"
)


def assert_numbers_close(actual, expected):
    """Compare calculation records without pinning float serialization."""
    if isinstance(expected, dict):
        assert actual.keys() == expected.keys(), REBUILD
        for key in expected:
            assert_numbers_close(actual[key], expected[key])
    elif isinstance(expected, list):
        assert len(actual) == len(expected), REBUILD
        for left, right in zip(actual, expected):
            assert_numbers_close(left, right)
    elif isinstance(expected, (float, int)) and not isinstance(expected, bool):
        assert actual == pytest.approx(expected, rel=1e-10, abs=1e-12), REBUILD
    else:
        assert actual == expected, REBUILD


def test_rebuild_is_deterministic_and_committed_page_is_current(built, tmp_path):
    module, record, html = built
    second = module.build_payload(tmp_path / "second")
    assert_numbers_close(second, record)
    committed = (REPORT / "report.html").read_text(encoding="utf-8")
    parsed, rebuilt = Metrics(), Metrics()
    parsed.feed(committed)
    rebuilt.feed(html)
    assert_numbers_close(json.loads(parsed.record), record)
    assert len(parsed.metrics) == len(rebuilt.metrics), REBUILD
    for (left, _), (right, _) in zip(parsed.metrics, rebuilt.metrics):
        assert left["data-metric"] == right["data-metric"], REBUILD
        assert float(left["data-value"]) == pytest.approx(float(right["data-value"])), REBUILD
    # Compare narrative/layout while allowing equivalent float serialization.
    def structure(page):
        page = re.sub(r'<script type="application/json" id="calculation-record">.*?</script>',
                      '<script id="calculation-record"></script>', page, flags=re.S)
        page = re.sub(r'<span data-metric="([^"]+)"[^>]*>.*?</span>', r'<span>\1</span>', page)
        page = re.sub(r'"document_key": "[0-9a-f]+"', '"document_key": "digest"', page)
        return page
    assert structure(committed) == structure(html), REBUILD
    assert json.loads((REPORT / "changes.json").read_text(encoding="utf-8")) == module.CHANGES, REBUILD


def test_chart_demands_and_limit_come_from_calculation(built):
    _, record, html = built
    for index, segment in enumerate(record["result"]["segments"]):
        chart = record["chart"][index]
        assert chart["demand"] == segment["total_damage"]
        assert chart["limit"] == record["allowable_damage"]
        assert chart["limit"] == 1 / record["inputs"]["dff"]
    assert 'id="damage-limit"' in html
    assert 'tabindex="0"' in html
    assert "pointerenter" in html and "focus" in html


def test_report_has_house_style_sources_and_ten_sections(built):
    _, _, html = built
    assert '<body class="hs-doc">' in html
    for path in ("docs/api/_assets/brand.css",):
        assert (ROOT / path).read_text(encoding="utf-8") in html
    assert len(re.findall(r'<section id="s\d+">', html)) == 10
    assert "Draft for maintainer review" in html
    assert "Revision history" in html
    assert "NOT_EVALUATED" in html
    assert "ASSUMED - to be confirmed" in html
    assert "independent published benchmark" in html
    parser = Metrics()
    parser.feed(html)
    assert not parser.external_assets
    assert 'id="changes-record"' in html
    ids = set(re.findall(r'\bid="([^"]+)"', html))
    for target in re.findall(r'href="#([^"]+)"', html):
        assert target in ids


def test_cli_writes_only_requested_deliverables(tmp_path):
    output = tmp_path / "report"
    subprocess.run(
        [sys.executable, str(REPORT / "generate_report.py"),
         "--output-dir", str(output), "--scratch-dir", str(tmp_path / "scratch")],
        cwd=ROOT, check=True, capture_output=True, text=True, encoding="utf-8",
    )
    assert sorted(p.name for p in output.iterdir()) == ["changes.json", "report.html"]
    # The numerical rebuild above uses tolerances; this CLI check covers UTF-8 and LF.
    for name in ("report.html", "changes.json"):
        raw = (output / name).read_bytes()
        assert b"\r\n" not in raw
        assert raw.decode("utf-8").endswith("\n")


def test_report_deliverables_are_visible_to_git():
    for name in ("generate_report.py", "report.html", "changes.json"):
        result = subprocess.run(
            ["git", "check-ignore", "--no-index", "-q", str(REPORT / name)], cwd=ROOT,
        )
        assert result.returncode == 1
    assert subprocess.run(
        ["git", "check-ignore", "--no-index", "-q", "docs/reports/modules/unrelated/report.html"],
        cwd=ROOT,
    ).returncode == 0


def test_all_numeric_example_inputs_are_displayed(built):
    _, record, html = built
    parser = Metrics()
    parser.feed(html)
    used = {attrs["data-metric"] for attrs, _ in parser.metrics}

    def visit(value, key):
        if isinstance(value, dict):
            for name, child in value.items():
                visit(child, key + "." + name)
        elif isinstance(value, list):
            for index, child in enumerate(value):
                visit(child, key + f".{index}")
        else:
            try:
                float(value)
            except (ValueError, TypeError):
                return
            assert key in used

    visit(record["inputs"], "inputs")


def test_a_changed_verdict_requires_a_new_engineering_narrative(built):
    module, record, _ = built
    changed = copy.deepcopy(record)
    changed["result"]["screening_status"] = "fail"
    with pytest.raises(ValueError, match="engineering narrative update"):
        module.render(changed)


def test_comment_roundtrip_and_chart_events_without_a_browser(built, tmp_path):
    """Exercise saving, merging, revision binding and safe rendering in Node."""
    module, _, _ = built
    script = tmp_path / "interactions.cjs"
    script.write_text(r'''
const assert = require('node:assert/strict');
function element() {
  return {textContent: '', value: '', children: [], listeners: {}, dataset: {},
    append(child) {this.children.push(child);}, replaceChildren() {this.children = [];},
    addEventListener(name, handler) {this.listeners[name] = handler;}, click() {}};
}
const ids = new Map();
for (const id of ['comment-config', 'comment-section', 'comment-text', 'comment-list',
  'comment-add', 'comment-load', 'comment-save', 'comment-status', 'chart-readout']) ids.set(id, element());
ids.get('comment-config').textContent = JSON.stringify({document_key: 'matching-document', revision: 'Initial'});
const sections = Array.from({length: 10}, (_, i) => ({id: `s${i + 1}`,
  querySelector() {return {textContent: this.id};}}));
const point = element();
point.dataset.description = 'Case: example; component: touchdown; demand and limit; dimensionless.';
global.document = {getElementById: id => ids.get(id), createElement: element,
  querySelectorAll: selector => selector === 'section[id]' ? sections : [point]};
global.window = {};
let savedBlob;
global.URL = {createObjectURL(blob) {savedBlob = blob; return 'blob:comments';}, revokeObjectURL() {}};
global.setTimeout = callback => callback();
''' + module.INTERACTIONS + r'''
(async () => {
  const el = id => ids.get(id);
  assert.equal(el('comment-section').children.length, 10);
  el('comment-section').value = 's5';
  el('comment-text').value = '<img onerror=invalid> Check the stress basis.';
  el('comment-add').listeners.click();
  assert.equal(el('comment-list').children.length, 1);
  assert.match(el('comment-list').children[0].textContent, /<img/);
  await el('comment-save').listeners.click();
  const saved = JSON.parse(await savedBlob.text());
  assert.equal(saved.comments.length, 1);
  const load = data => el('comment-load').listeners.change({target: {files: [{text: async () => JSON.stringify(data)}]}});
  await load(saved);
  assert.equal(el('comment-list').children.length, 1);
  await load({...saved, comments: [{section: 's3', text: 'Confirm dimensions.'}]});
  assert.equal(el('comment-list').children.length, 2);
  await load({...saved, document_key: 'different-document'});
  assert.match(el('comment-status').textContent, /not loaded/);
  await load({...saved, revision: 'Different'});
  assert.match(el('comment-status').textContent, /not loaded/);
  await load({...saved, comments: [{section: 'missing', text: 'Invalid location'}]});
  assert.equal(el('comment-list').children.length, 2);
  let written, closed = false;
  window.showSaveFilePicker = async () => ({createWritable: async () => ({
    write: async data => {written = data;}, close: async () => {closed = true;}})});
  await el('comment-save').listeners.click();
  assert.equal(JSON.parse(written).comments.length, 2);
  assert.equal(closed, true);
  window.showSaveFilePicker = async () => {const error = new Error(); error.name = 'AbortError'; throw error;};
  await el('comment-save').listeners.click();
  assert.match(el('comment-status').textContent, /cancelled/);
  point.listeners.pointerenter();
  assert.equal(el('chart-readout').textContent, point.dataset.description);
  point.listeners.focus();
  assert.equal(el('chart-readout').textContent, point.dataset.description);
})().catch(error => {console.error(error); process.exitCode = 1;});
''', encoding="utf-8", newline="\n")
    subprocess.run(["node", str(script)], check=True, capture_output=True, text=True, encoding="utf-8")


@pytest.mark.parametrize("line_ending", [b"\n", b"\r\n"])
def test_source_digests_are_independent_of_checkout_line_endings(built, tmp_path, monkeypatch, line_ending):
    module, record, _ = built
    source_root = tmp_path / "sources"
    for relative in module.SOURCE_PATHS:
        target = source_root / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        raw = (ROOT / relative).read_bytes().replace(b"\r\n", b"\n")
        target.write_bytes(raw.replace(b"\n", line_ending))
    monkeypatch.setattr(module, "ROOT", source_root)
    actual = module.build_payload(tmp_path / "calculation-two")
    assert actual["source_digests"] == record["source_digests"], REBUILD


def test_cli_text_writes_preserve_lf_with_windows_default(built, tmp_path, monkeypatch):
    module, record, _ = built
    real_write = Path.write_text
    def windows_write(path, data, encoding=None, errors=None, newline=None):
        if newline is None:
            data = data.replace("\n", "\r\n")
        return real_write(path, data, encoding=encoding, errors=errors, newline="\n")
    output = tmp_path / "output"
    monkeypatch.setattr(Path, "write_text", windows_write)
    monkeypatch.setattr(module, "build_payload", lambda _: record)
    monkeypatch.setattr(sys, "argv", ["generate_report.py", "--output-dir", str(output),
                                    "--scratch-dir", str(tmp_path / "calculation")])
    module.main()
    for name in ("report.html", "changes.json"):
        assert b"\r\n" not in (output / name).read_bytes()


def test_report_numbers_against_hand_worked_literals(built):
    _, record, page = built
    # Sum n/N over the supplied wave bins, then scale the annual sum by 25.
    # The five design-life n/N terms are 0.00230451100827 + 0.00444543018571
    # + 0.00349602855181 + 0.00147488704530 + 0.000403197246008 = 0.0121240540371;
    # VIV: 0.42*0.02*31557600*25 = 6627096, 0.55*40*3600*25 = 1980000.
    # VIV: 0.000576592703563 + 0.000725946730853 = 0.00130253943442.
    # D = Dw+Dv = 0.0134265934715; L = 25/D = 1861.97638686;
    # M = L/(25*10) = 7.44790554746. These are arithmetic, not physical, oracles.
    expected = {"governing_damage": 0.0134265934715,
                "governing_fatigue_life_years": 1861.97638686,
                "governing_dff_margin": 7.44790554746}
    parsed = Metrics()
    parsed.feed(page)
    shown = {a["data-metric"]: float(a["data-value"]) for a, _ in parsed.metrics}
    for key, value in expected.items():
        assert record["result"][key] == pytest.approx(value, rel=1e-6)
        assert shown[f"result.{key}"] == pytest.approx(value, rel=1e-6)
    viv = [row["cycles"] for row in record["bins"]
           if row["segment_id"] == "TDZ-sandwave" and row["contribution"] == "viv"]
    assert viv == pytest.approx([6627096, 1980000], rel=1e-6)


def test_reversed_segment_order_retains_location_identity(built):
    module, record, _ = built
    changed = copy.deepcopy(record)
    changed["inputs"]["segments"].reverse()
    changed["result"]["segments"].reverse()
    changed["touchdown"].reverse()
    page = module.render(changed)
    assert "The governing location is the touchdown zone" in page
    parsed = Metrics()
    parsed.feed(page)
    thickness = next(visible for attrs, visible in parsed.metrics
                     if attrs["data-metric"] == "inputs.segments.0.wall_thickness_mm")
    assert thickness == "25.400"


def test_unknown_segment_identity_is_rejected(built):
    module, record, _ = built
    changed = copy.deepcopy(record)
    changed["inputs"]["segments"][0]["id"] = "unknown"
    with pytest.raises(ValueError, match="Unsupported segment identity"):
        module.render(changed)


def test_relative_csv_path_is_resolved_from_repository_not_cwd(built, tmp_path, monkeypatch):
    module, _, _ = built
    # Reproduce the router's repository-relative display path without leaving scratch.
    monkeypatch.setattr(module, "ROOT", tmp_path)
    scratch = tmp_path / "calculation"
    real_router = module.router
    def relative_router(cfg):
        out = real_router(cfg)
        path = Path(out["riser_fatigue"]["results_csv"])
        out["riser_fatigue"]["results_csv"] = path.relative_to(tmp_path).as_posix()
        return out
    # The source hashes stay tied to the real source tree for this path-only probe.
    monkeypatch.setattr(module, "SOURCE_PATHS", [])
    monkeypatch.setattr(module, "router", relative_router)
    record = module.build_payload(scratch)
    assert len(record["bins"]) == 12


@pytest.mark.parametrize("parent_pattern", ["*", "*/*"])
def test_module_ignore_rules_compose_with_sibling_exceptions(tmp_path, parent_pattern):
    env = {k: v for k, v in os.environ.items()
           if k not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"}}
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True, env=env)
    rules = (ROOT / ".gitignore").read_text(encoding="utf-8")
    # A sibling uses the shared parent rule plus its own file exceptions.
    rules += f"\n!docs/reports/modules/\ndocs/reports/modules/{parent_pattern}\n"
    rules += "!docs/reports/modules/sibling/\n!docs/reports/modules/sibling/report.html\n"
    nested = tmp_path / "docs/reports/modules/.gitignore"
    nested.parent.mkdir(parents=True, exist_ok=True)
    nested.write_text((ROOT / "docs/reports/modules/.gitignore").read_text(encoding="utf-8"),
                      encoding="utf-8", newline="\n")
    (tmp_path / ".gitignore").write_text(rules, encoding="utf-8", newline="\n")
    for relative, expected in (("riserfatigue/report.html", 1),
                               ("sibling/report.html", 1), ("unrelated/report.html", 0),
                               ("riserfatigue/scratch.csv", 0)):
        path = tmp_path / "docs/reports/modules" / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        result = subprocess.run(["git", "check-ignore", "--no-index", "-q", path.as_posix()],
                                cwd=tmp_path, env=env)
        assert result.returncode == expected


def test_captions_precision_and_applicability(built):
    _, _, page = built
    for body in re.findall(r"<table>(.*?)</table>", page, flags=re.S):
        if "<caption>" in body:
            assert body.index("<caption>") > body.index("</tbody>")
    parsed = Metrics()
    parsed.feed(page)
    for attrs, visible in parsed.metrics:
        key = attrs["data-metric"]
        if "wall_thickness_mm" in key or "thickness_ref_mm" in key:
            assert re.fullmatch(r"\d+\.\d{3}", visible)
        if ".cycles" in key or "allowable_cycles" in key:
            assert re.fullmatch(r"[\d,]+", visible)
    assert "62,500" in page
    assert "Drilling-riser operability is not applicable to this supplied-stress fatigue screen" in page
    assert "do not connect" not in page
    assert "1-year current" not in page
    assert re.search(r"Initial</td>.*?</tr>\s*<tr><td>1</td>", page, re.S)


@pytest.mark.parametrize("value", [None, float("nan"), float("inf"), -1.0])
def test_invalid_decision_basis_cannot_render_a_pass(built, value):
    module, record, _ = built
    changed = copy.deepcopy(record)
    changed["inputs"]["dff"] = value
    with pytest.raises(ValueError, match="Invalid screening basis"):
        module.render(changed)


@pytest.mark.parametrize("value", [None, float("nan"), float("inf"), -1.0])
def test_invalid_bin_damage_is_not_treated_as_not_applicable(built, value):
    module, record, _ = built
    changed = copy.deepcopy(record)
    changed["bins"][0]["damage"] = value
    with pytest.raises(ValueError, match="Invalid screening basis"):
        module.render(changed)


@pytest.mark.parametrize("key,value", [
    ("inputs.design_life_years", None),
    ("inputs.segments.0.wall_thickness_mm", 200.0),
    ("inputs.segments.0.wave.cycles.0", float("nan")),
    ("inputs.segments.0.viv_cases.0.exposure_fraction", 2.0),
    ("result.governing_damage", 0.2),
    ("result.segments.0.wave_damage", -0.1),
    ("result.segments.0.passes_dff", False),
    ("chart.0.limit", 1.0),
])
def test_inconsistent_screening_evidence_is_rejected(built, key, value):
    module, record, _ = built
    changed = copy.deepcopy(record)
    target = changed
    parts = key.split(".")
    for part in parts[:-1]:
        target = target[int(part)] if isinstance(target, list) else target[part]
    target[int(parts[-1]) if isinstance(target, list) else parts[-1]] = value
    with pytest.raises(ValueError, match="Invalid screening basis"):
        module.render(changed)


@pytest.mark.parametrize("key", ["bins", "chart", "result.segments"])
def test_missing_evidence_cannot_produce_a_passing_report(built, key):
    module, record, _ = built
    changed = copy.deepcopy(record)
    target = changed
    for part in key.split(".")[:-1]:
        target = target[part]
    target[key.split(".")[-1]] = []
    with pytest.raises(ValueError, match="Invalid screening basis"):
        module.render(changed)
