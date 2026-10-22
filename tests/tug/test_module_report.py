"""Numerical and document contracts for the public tug report."""

import importlib.util
import json
import re
import subprocess
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
BUILDER = ROOT / "docs/reports/modules/tug/build_report.py"
REBUILD = "python docs/reports/modules/tug/build_report.py"


def builder():
    spec = importlib.util.spec_from_file_location("tug_report", BUILDER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def assert_record_matches(actual, expected):
    """Compare numerical payloads with tolerance and retain structural checks."""
    if isinstance(expected, dict):
        assert actual.keys() == expected.keys()
        for key in expected:
            assert_record_matches(actual[key], expected[key])
    elif isinstance(expected, list):
        for item, reference in zip(actual, expected, strict=True):
            assert_record_matches(item, reference)
    elif isinstance(expected, float):
        assert actual == pytest.approx(expected)
    else:
        assert actual == expected


def test_first_principles_anchors():
    data = builder().calculate()
    # Energy = 4000 * (0.40*5 + 0.90*1 + 0.10*4) = 13200 kWh.
    assert data["result"]["total_energy_kwh"] == pytest.approx(13200)
    # Peak = 4000 * 0.90 = 3600 kW.
    assert data["result"]["peak_power_kw"] == pytest.approx(3600)
    # Engine baseline = 4000 * 0.50 = 2000 kW.
    assert data["result"]["baseline_engine_kw"] == pytest.approx(2000)
    # Only assist exceeds baseline: (3600 - 2000)*1 = 1600 kWh.
    assert data["result"]["battery_energy_kwh"] == pytest.approx(1600)
    assert data["result"]["battery_power_kw"] == pytest.approx(1600)
    # Battery share = discharge energy / cycle energy = 1600 / 13200.
    assert data["result"]["peak_energy_fraction"] == pytest.approx(1600 / 13200)


def test_fixture_is_the_published_example():
    mod = builder()
    data = mod.calculate()
    assert data["input"]["installed_power_kw"] == 4000
    assert data["input"]["baseline_fraction"] == 0.5
    assert [(p["name"], p["power_fraction"], p["duration_h"])
            for p in data["input"]["profile"]] == [
                ("transit", 0.4, 5.0), ("assist", 0.9, 1.0),
                ("standby", 0.1, 4.0)]


def test_every_output_matches_module():
    from dataclasses import asdict
    from digitalmodel.tug.emissions import OperatingPhase, size_hybrid

    data = builder().calculate()
    inputs = data["input"]
    phases = [OperatingPhase(**p) for p in inputs["profile"]]
    actual = asdict(size_hybrid(inputs["installed_power_kw"], phases,
                              inputs["baseline_fraction"]))
    assert data["result"] == pytest.approx(actual)
    for row, phase in zip(data["phases"], phases, strict=True):
        actual_phase = asdict(size_hybrid(inputs["installed_power_kw"], [phase],
                                        inputs["baseline_fraction"]))
        assert row["result"] == pytest.approx(actual_phase)


def test_rebuild_and_displayed_numbers(tmp_path):
    mod = builder()
    mod.build(tmp_path)
    report_dir = BUILDER.parent
    for name in ("index.html", "changes.json"):
        actual = (tmp_path / name).read_text(encoding="utf-8")
        expected = (report_dir / name).read_text(encoding="utf-8")
        assert actual == expected, f"Stale {name}; rebuild: {REBUILD}"
        assert b"\r" not in (tmp_path / name).read_bytes()
    html = (tmp_path / "index.html").read_text(encoding="utf-8")
    data = json.loads(re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>',
        html, re.S).group(1))
    assert_record_matches(data, mod.calculate())
    spans = re.findall(r'<span data-value="([^"]+)">([^<]+)</span>', html)
    assert len(spans) >= len(data["result"])
    for key, displayed in spans:
        value = mod.lookup(data, key)
        assert float(displayed.replace(",", "")) == pytest.approx(
            value, abs=0.00051), key


def test_structure_sources_and_offline_assets(tmp_path):
    mod = builder()
    mod.build(tmp_path)
    html = (tmp_path / "index.html").read_text(encoding="utf-8")
    assert html.count("<h2") == 10
    assert '<body class="hs-doc">' in html
    assert 'Draft for maintainer review' in html
    assert 'showSaveFilePicker' in html and 'comments.json' in html
    assert 'changes-panel' in html
    markup = re.sub(r"<style>.*?</style>", "", html, flags=re.S)
    assert not re.search(r'<(?:script|link)[^>]+(?:src|href)=', markup)
    ids = set(re.findall(r'\bid="([^"]+)"', html))
    assert all(anchor in ids for anchor in re.findall(r'href="#([^"]+)"', html))
    assert not re.search(r'<(?:b|strong)(?:\s|>)', html)
    assert "openstax.org/books/university-physics-volume-1/pages/7-4-power" in html
    assert "ASSUMED - to be confirmed" in html
    assert "NOT_EVALUATED" in html


def test_house_styles_are_reused():
    mod = builder()
    exemplar = (ROOT / mod.EXEMPLAR).read_text(encoding="utf-8")
    style = re.search(r"<style>.*?</style>", exemplar, re.S).group()
    assert style in mod.render(mod.calculate())


def test_invalid_example_data_is_rejected():
    mod = builder()
    for fraction, duration in [(-0.1, 1), (1.1, 1), (0.5, -1),
                               (float("nan"), 1)]:
        with pytest.raises(ValueError):
            mod.validate_profile(4000, [("assist", fraction, duration)], 0.5)


def test_saved_document_numbers_and_chart_annotations():
    """Read-only check also runs where temporary-directory creation is denied."""
    mod = builder()
    mod.check(BUILDER.parent)
    html = (BUILDER.parent / "index.html").read_text(encoding="utf-8")
    data = json.loads(re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>',
        html, re.S).group(1))
    assert_record_matches(data, mod.calculate())
    for key, displayed in re.findall(
            r'<span data-value="([^"]+)">([^<]+)</span>', html):
        assert float(displayed.replace(",", "")) == pytest.approx(
            mod.lookup(data, key), abs=0.00051)
    baseline = data["result"]["baseline_engine_kw"]
    for phase in data["phases"]:
        for key, label in (("peak_power_kw", "demand"),
                           ("battery_power_kw", "power")):
            assert f'{label} {phase["result"][key]:.3f} kW' in html
    assert f'engine baseline {baseline:.3f} kW' in html
    assert html.count("<h2") == 10
    ids = re.findall(r'\bid="([^"]+)"', html)
    assert len(ids) == len(set(ids))
    assert all(anchor in ids for anchor in re.findall(r'href="#([^"]+)"', html))
    markup = re.sub(r"<style>.*?</style>", "", html, flags=re.S)
    assert not re.search(r'<(?:script|link)[^>]+(?:src|href)=', markup)
    assert not re.search(r'<(?:b|strong)(?:\s|>)', html)
    assert '<body class="hs-doc">' in html


def test_public_wording_in_built_document():
    from html.parser import HTMLParser

    class VisibleText(HTMLParser):
        def __init__(self):
            super().__init__()
            self.hidden = False
            self.parts = []

        def handle_starttag(self, tag, attrs):
            if tag in ("script", "style"):
                self.hidden = True
            self.parts.extend(value for key, value in attrs
                              if key in ("aria-label", "title", "data-detail"))

        def handle_endtag(self, tag):
            if tag in ("script", "style"):
                self.hidden = False

        def handle_data(self, value):
            if not self.hidden:
                self.parts.append(value)

    parser = VisibleText()
    parser.feed((BUILDER.parent / "index.html").read_text(encoding="utf-8"))
    text = " ".join(parser.parts)
    assert not re.search(r"\b(owner|lane|seat|session|sha|could|should|would)\b", text, re.I)
    assert not re.search(r"[A-Za-z]:[\\/]", text)


def test_non_unit_assist_duration(monkeypatch):
    mod = builder()
    rows = [("transit", 0.4, 5.0), ("assist", 0.9, 2.5),
            ("standby", 0.1, 4.0)]
    monkeypatch.setattr(mod, "read_example", lambda: (4000, rows, 0.5, {}))
    result = mod.calculate()["result"]
    # Cycle = 1600*5 + 3600*2.5 + 400*4 = 18600 kWh.
    assert result["total_energy_kwh"] == pytest.approx(18600)
    # Discharge = (3600 - 2000)*2.5 = 4000 kWh; peak remains 1600 kW.
    assert result["battery_energy_kwh"] == pytest.approx(4000)
    assert result["battery_power_kw"] == pytest.approx(1600)
    assert result["peak_energy_fraction"] == pytest.approx(4000 / 18600)


def test_shared_example_supplies_report_and_toolkit(tmp_path, monkeypatch):
    mod = builder()
    fixture = ROOT / "tests/tug/duty_cycle.json"
    example = json.loads(fixture.read_text(encoding="utf-8"))
    spec = importlib.util.spec_from_file_location(
        "tug_toolkit_tests", ROOT / "tests/tug/test_tug_toolkit.py")
    toolkit = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(toolkit)
    assert toolkit.TestEmissions.EXAMPLE == example
    from dataclasses import asdict
    assert [asdict(p) for p in toolkit.TestEmissions.PROFILE] == example["profile"]
    assert mod.calculate()["input"] == example
    # A changed duration, power and baseline must reach the report together.
    example["profile"][1]["duration_h"] = 2.5
    example["installed_power_kw"] = 5000
    example["baseline_fraction"] = 0.6
    target = tmp_path / mod.FIXTURE
    target.parent.mkdir(parents=True)
    target.write_text(json.dumps(example), encoding="utf-8", newline="\n")
    monkeypatch.setattr(mod, "ROOT", tmp_path)
    monkeypatch.setattr(mod, "TESTS", ROOT / "tests/tug/test_tug_toolkit.py", raising=False)
    data = mod.calculate()
    assert data["input"] == example
    # Battery = (5000*0.9 - 5000*0.6)*2.5 = 3750 kWh.
    assert data["result"]["battery_energy_kwh"] == pytest.approx(3750)


def test_comment_import_preserves_unsaved_notes():
    script = (BUILDER.parent / "interactions.js").read_text(encoding="utf-8")
    harness = r"""
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const element = () => ({value: '', textContent: '', children: [], handlers: {},
  append(child) { this.children.push(child); },
  replaceChildren() { this.children = []; },
  addEventListener(name, handler) { this.handlers[name] = handler; }});
const ids = new Map();
const byId = (id) => { if (!ids.has(id)) ids.set(id, element()); return ids.get(id); };
const section = {id: 'summary', querySelector: () => ({textContent: 'Summary'})};
byId('change-data').textContent = JSON.stringify({revision: 'B', changes: []});
const document = {getElementById: byId, createElement: element,
  querySelectorAll: (selector) => selector === 'section[id]' ? [section] : []};
vm.runInNewContext(fs.readFileSync(0, 'utf8'), {document});
byId('comment-section').value = 'summary';
byId('comment-text').value = 'Unsaved calculation note';
byId('add-comment').handlers.click();
const load = async (record) => byId('load-comments').handlers.change({
  target: {files: [{text: async () => JSON.stringify(record)}]}});
const saved = {revision: 'A', comments: [
  {section: 'summary', text: 'Imported calculation note'},
  {section: 'summary', text: 'Unsaved calculation note'}]};
(async () => {
  await load(saved);
  assert.deepEqual(byId('comment-list').children.map((c) => c.textContent),
    ['Summary: Unsaved calculation note', 'Summary: Imported calculation note']);
  await load({...saved, revision: 'B'});
  assert.equal(byId('comment-list').children.length, 2);
  await load({revision: 'A', comments: [saved.comments[0], {section: 'missing', text: 'Bad'}]});
  assert.equal(byId('comment-list').children.length, 2);
  assert.match(byId('comment-status').textContent, /Invalid comment file/);
})().catch((error) => { console.error(error); process.exitCode = 1; });
"""
    result = subprocess.run(["node", "-e", harness], input=script, text=True,
                            encoding="utf-8", capture_output=True, check=False)
    assert result.returncode == 0, result.stderr


def test_input_precision_is_retained(monkeypatch):
    mod = builder()
    rows = [("assist", 0.9, 0.0254)]
    monkeypatch.setattr(mod, "read_example", lambda: (4000, rows, 0.5, {}))
    html = mod.render(mod.calculate())
    displayed = re.search(
        r'<span data-value="input.profile.0.duration_h">([^<]+)</span>', html).group(1)
    assert float(displayed) == pytest.approx(0.0254, abs=1e-12)
