# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check the seismic report; Node.js is required for note persistence tests."""

import importlib.util
import hashlib
import json
import re
import shutil
import subprocess
from pathlib import Path, PureWindowsPath

import numpy as np
import pytest
import yaml
from bs4 import BeautifulSoup
from scipy.integrate import trapezoid

from digitalmodel.seismic import AccelerogramRecord, read_ascii_accelerogram
from digitalmodel.seismic.response_spectrum.calculations import compute_kinematics
from digitalmodel.seismic.response_spectrum.workflow import router
from digitalmodel.signal_processing.signal_analysis.filters.frequency import FrequencyFilter


ROOT = Path(__file__).resolve().parents[2]
REPORT_DIR = ROOT / "docs/reports/modules/seismic"
EXAMPLE = ROOT / "examples/workflows/response-spectrum"
# Motion budgets use 0.1% of the scalar tables' smallest fixed-point motion step
# (0.0001 m or m/s), also applied to acceleration in m/s². Gain is dimensionless;
# its 1e-8 budget uses 0.001% of the chart's 0.001 step in its fixed-point range.
# Absolute sample budgets can span several scientific-notation readout steps.
# These display-derived limits are independent of observed platform differences
# and do not establish physical motion accuracy or design acceptance.
MOTION_ATOL = 1e-7
GAIN_ATOL = 1e-8
# Roundoff budgets for baseline subtraction and the unfiltered integration
# diagnostic are much smaller than their displayed steps. They cover binary64
# reductions over the 20-sample baseline and 1560-sample record, respectively.
BASELINE_ATOL = 1e-12
UNFILTERED_VELOCITY_ATOL = 1e-10
PROCESSED_METRICS = (
    "filtered_peak_m_s2", "peak_velocity_m_s", "peak_displacement_m",
    "end_velocity_m_s", "end_displacement_m",
)


def builder():
    spec = importlib.util.spec_from_file_location(
        "seismic_module_report", ROOT / "scripts/reporting/build_seismic_module_report.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def expected_result():
    config = yaml.safe_load((EXAMPLE / "input.yml").read_text(encoding="utf-8"))["response_spectrum"]
    record = read_ascii_accelerogram(
        EXAMPLE / config["record"]["file"], units=config["record"]["units"]
    )
    result = compute_kinematics(
        record, baseline=config["baseline"], frequency_filter=config["filter"]
    )
    return result, config


def report_content(html):
    """Check displayed binding before separating numeric and fixed content."""
    soup = BeautifulSoup(html, "html.parser")
    evidence = soup.find("script", id="seismic-evidence")
    payload = json.loads(evidence.string)
    payload.pop("versions")
    assert payload["processing_chain"][0]["offset_m_s2"] == payload["metrics"]["baseline_offset_m_s2"]
    for node in soup.select("[data-metric]"):
        name = node["data-metric"]
        shown = f"{payload['metrics'][name]:.{int(node['data-digits'])}f}".replace("-", "−")
        assert node.get_text() == shown, "Displayed number is not bound to its evidence"
        if name in (*PROCESSED_METRICS, "baseline_offset_m_s2", "unfiltered_end_velocity_m_s"):
            node.string = ""
    for cell in soup.select("#dependency-versions tbody tr td:nth-child(2)"):
        cell.string = ""
    return soup, payload


def assert_report_agreement(actual, expected):
    """Allow bounded processed results; retain exact sources, inputs and markup."""
    left, right = report_content(actual), report_content(expected)
    fields = [("metrics", name, MOTION_ATOL) for name in PROCESSED_METRICS]
    fields += [("timeseries", name, MOTION_ATOL) for name in (
        "filtered_acceleration_m_s2", "velocity_m_s", "displacement_m",
    )]
    fields.append(("filter_response", "gain", GAIN_ATOL))
    fields += [("metrics", "baseline_offset_m_s2", BASELINE_ATOL),
               ("timeseries", "corrected_acceleration_m_s2", BASELINE_ATOL),
               ("metrics", "unfiltered_end_velocity_m_s", UNFILTERED_VELOCITY_ATOL)]
    for group, name, tolerance in fields:
        a, b = (np.asarray(payload[group].pop(name)) for _, payload in (left, right))
        assert a.shape == b.shape, f"Changed shape: {group}.{name}"
        assert np.isfinite(a).all() and np.isfinite(b).all(), f"Non-finite: {group}.{name}"
        np.testing.assert_allclose(a, b, rtol=0, atol=tolerance, equal_nan=False,
                                   err_msg=f"Cross-runtime regression budget: {group}.{name}")
    for soup, payload in (left, right):
        payload["processing_chain"][0].pop("offset_m_s2")
        soup.find(id="seismic-evidence").string = json.dumps(payload, ensure_ascii=False, sort_keys=True)
    if str(left[0]) != str(right[0]):
        raise AssertionError("Issued metadata or content changed; rebuild with "
                             "PYTHONPATH=src uv run python scripts/reporting/build_seismic_module_report.py")


def changed_report(group, key, delta):
    soup = BeautifulSoup((REPORT_DIR / "seismic-analysis.html").read_text(encoding="utf-8"), "html.parser")
    script = soup.find(id="seismic-evidence")
    payload = json.loads(script.string)
    value = payload[group][key]
    if isinstance(value, list):
        value[int(np.argmax(np.abs(value)))] += delta
    else:
        payload[group][key] += delta
    if group == "metrics" and key == "baseline_offset_m_s2":
        payload["processing_chain"][0]["offset_m_s2"] = payload[group][key]
    script.string = json.dumps(payload, ensure_ascii=False)
    for node in soup.select("[data-metric]"):
        value = payload["metrics"][node["data-metric"]]
        node.string = f"{value:.{int(node['data-digits'])}f}".replace("-", "−")
    return str(soup)


@pytest.mark.parametrize("group,key,delta", [
    *[("metrics", name, 5e-8) for name in PROCESSED_METRICS],
    ("timeseries", "displacement_m", 5e-8),
    ("timeseries", "velocity_m_s", 5e-8),
    ("timeseries", "filtered_acceleration_m_s2", 5e-8),
    ("filter_response", "gain", 5e-9),
    ("metrics", "baseline_offset_m_s2", 5e-13),
    ("timeseries", "corrected_acceleration_m_s2", 5e-13),
    ("metrics", "unfiltered_end_velocity_m_s", 5e-11),
])
def test_issued_comparison_allows_bounded_numerical_drift(group, key, delta):
    assert_report_agreement(changed_report(group, key, delta),
                            (REPORT_DIR / "seismic-analysis.html").read_text(encoding="utf-8"))


@pytest.mark.parametrize("group,key,delta", [
    *[("metrics", name, 1.1e-7) for name in PROCESSED_METRICS],
    ("timeseries", "displacement_m", 1.1e-7),
    ("timeseries", "velocity_m_s", 1.1e-7),
    ("timeseries", "filtered_acceleration_m_s2", 1.1e-7),
    ("filter_response", "gain", 1.1e-8),
    ("timeseries", "acceleration_m_s2", 1e-12),
    ("timeseries", "corrected_acceleration_m_s2", 2e-12),
    ("metrics", "baseline_offset_m_s2", 2e-12),
    ("metrics", "unfiltered_end_velocity_m_s", 2e-10),
    ("timeseries", "time_s", 1e-12),
    ("timeseries", "displacement_m", float("nan")),
    ("timeseries", "displacement_m", float("inf")),
    ("metrics", "end_displacement_m", float("nan")),
    ("metrics", "end_displacement_m", float("inf")),
])
def test_issued_comparison_rejects_numerical_changes_outside_its_budget(group, key, delta):
    with pytest.raises(AssertionError):
        assert_report_agreement(changed_report(group, key, delta),
                                (REPORT_DIR / "seismic-analysis.html").read_text(encoding="utf-8"))


@pytest.mark.parametrize("fault", ["source", "processing", "markup", "display"])
def test_issued_comparison_retains_exact_metadata_and_display_binding(fault):
    original = (REPORT_DIR / "seismic-analysis.html").read_text(encoding="utf-8")
    soup = BeautifulSoup(original, "html.parser")
    script = soup.find(id="seismic-evidence")
    payload = json.loads(script.string)
    if fault == "source":
        payload["sources"][0]["sha256"] = "0" * 64
    elif fault == "processing":
        payload["processing_chain"][1]["order"] += 1
    elif fault == "display":
        soup.select_one('[data-metric="end_displacement_m"]').string = "100.0000"
    else:
        soup.h1.string = "Changed analysis"
    script.string = json.dumps(payload, ensure_ascii=False)
    with pytest.raises(AssertionError):
        assert_report_agreement(str(soup), original)


NOTE_PERSISTENCE_PROBE = r"""
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const [script, phase, edit] = process.argv.slice(1);
const elements = new Map();
function element(id) {
  if (!elements.has(id)) elements.set(id, {
    value: '', textContent: '', children: [], options: [{value: 'section-1'}], listeners: {},
    addEventListener(type, handler) { this.listeners[type] = handler; },
    append(...nodes) { this.children.push(...nodes); },
    replaceChildren() { this.children = []; }
  });
  return elements.get(id);
}
element('seismic-evidence').textContent = '{}';
element('seismic-changes').textContent = JSON.stringify({document: 'Example analysis', revisions: []});
element('comment-section').value = 'section-1';
const window = {listeners: {}, addEventListener(type, handler) { this.listeners[type] = handler; }};
const document = {
  getElementById: element, querySelectorAll() { return []; },
  querySelector() { return {textContent: '1. Summary'}; },
  createElement() { return element(Symbol()); }, createTextNode(text) { return {textContent: text}; }
};
let release, entered, exported, paused = false, pauseEnabled = true;
const blocked = new Promise(resolve => { release = resolve; });
const reached = new Promise(resolve => { entered = resolve; });
async function pause(stage) {
  if (pauseEnabled && stage === phase && !paused) { paused = true; entered(); await blocked; }
}
window.showSaveFilePicker = async () => {
  await pause('picker');
  return {createWritable: async () => {
    await pause('writable');
    return {
      write: async content => { exported = JSON.parse(content); await pause('write'); },
      close: async () => { await pause('close'); }
    };
  }};
};
vm.runInNewContext(fs.readFileSync(script, 'utf8'), {document, window});
function addNote(value) {
  element('comment-text').value = value;
  element('comment-text').listeners.input?.();
  element('add-comment').listeners.click();
}
function warns() {
  let warned = false;
  window.listeners.beforeunload({preventDefault() { warned = true; }});
  return warned;
}
async function mutate() {
  if (edit === 'add' || edit === 'add_save') addNote('Intervening note');
  if (edit === 'add_save') await element('save-comments').listeners.click();
  if (edit === 'draft' || edit === 'draft_clear') {
    element('comment-text').value = 'Pending draft';
    element('comment-text').listeners.input?.();
  }
  if (edit === 'draft_clear') {
    element('comment-text').value = '';
    element('comment-text').listeners.input?.();
  }
  if (edit === 'load') {
    const content = JSON.stringify({document: 'Example analysis', revision: '00',
      notes: [{section: 'section-1', text: 'Imported note'}]});
    await element('load-comments').listeners.change({target: {value: 'selected',
      files: [{size: content.length, text: async () => content}]}});
  }
}
(async () => {
  if (phase === 'load') {
    const content = JSON.stringify({document: 'Example analysis', revision: '00',
      notes: [{section: 'section-1', text: 'Imported note'}]});
    const target = {value: 'selected', files: [{size: content.length,
      text: async () => { await pause('load'); return content; }}]};
    const loading = element('load-comments').listeners.change({target});
    await reached;
    await mutate();
    release();
    await loading;
    assert.equal(target.value, '');
    if (edit === 'none') {
      assert.equal(element('comments-list').children.length, 1);
      assert.match(element('comments-list').children[0].children[1].textContent, /Imported note/);
      assert.equal(warns(), false);
    } else {
      const count = edit.startsWith('add') ? 1 : 0;
      assert.equal(element('comments-list').children.length, count, 'Load replaced intervening edits');
      if (count) assert.match(element('comments-list').children[0].children[1].textContent, /Intervening note/);
      assert.match(element('comments-status').textContent, /not loaded/);
      if (edit === 'add' || edit === 'draft') assert.equal(warns(), true);
    }
  } else {
    addNote('First note');
    if (edit === 'load') {
      pauseEnabled = false;
      await element('save-comments').listeners.click();
      pauseEnabled = true;
    }
    const saving = element('save-comments').listeners.click();
    await reached;
    await mutate();
    release();
    await saving;
    if (edit !== 'add_save') assert.equal(exported.notes.length, 1, 'Export must retain its initial snapshot');
    assert.equal(element('comments-list').children.length, edit.startsWith('add') ? 2 : 1);
    if (edit === 'load') assert.match(element('comments-list').children[0].children[1].textContent, /Imported note/);
    assert.equal(warns(), edit !== 'none', 'Intervening edits must remain unsaved');
    if (edit.startsWith('add')) {
      assert.match(element('comments-status').textContent, /changed|unsaved/);
      await element('save-comments').listeners.click();
      assert.equal(exported.notes.length, 2);
      assert.equal(warns(), false, 'A subsequent complete save must clear the warning');
    }
  }
})().catch(error => { console.error(error.message); process.exitCode = 1; });
"""


@pytest.mark.parametrize("phase,edit", [
    *[(phase, edit) for phase in ("picker", "writable", "write", "close")
      for edit in ("add", "draft", "add_save", "load")],
    ("write", "none"),
    *[("load", edit) for edit in ("add", "draft", "add_save", "draft_clear", "none")],
])
def test_note_persistence_preserves_intervening_edits(phase, edit):
    node = shutil.which("node")
    assert node, "Node.js is required for the offline note persistence regression"
    result = subprocess.run(
        [node, "-e", NOTE_PERSISTENCE_PROBE,
         (ROOT / "scripts/reporting/templates/seismic_module_interactions.js").as_posix(), phase, edit],
        capture_output=True, text=True, encoding="utf-8", check=False, timeout=15,
    )
    assert result.returncode == 0, result.stdout + result.stderr


def test_rebuild_matches_issued_content_with_bounded_numerical_drift(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)
    builder().build(tmp_path)
    for name in ("seismic-analysis.html", "changes.json"):
        first = (tmp_path / name).read_bytes()
        if name.endswith(".html"):
            assert_report_agreement(first.decode("utf-8"),
                (REPORT_DIR / name).read_text(encoding="utf-8")
            )
        else:
            assert first == (REPORT_DIR / name).read_bytes()


def test_repeated_builds_are_byte_identical_in_one_runtime(tmp_path):
    builder().build(tmp_path)
    first = {name: (tmp_path / name).read_bytes() for name in ("seismic-analysis.html", "changes.json")}
    builder().build(tmp_path)
    for name, content in first.items():
        assert content == (tmp_path / name).read_bytes()


def test_every_worked_value_and_complete_plotted_array_matches_module(tmp_path):
    builder().build(tmp_path)
    soup = BeautifulSoup((tmp_path / "seismic-analysis.html").read_text(encoding="utf-8"), "html.parser")
    payload = json.loads(soup.find("script", id="seismic-evidence").string)
    result, config = expected_result()
    frame = result.timeseries
    expected = dict(result.summary)
    expected.update(
        baseline_samples=result.processing_chain[0]["samples"],
        baseline_offset_m_s2=result.processing_chain[0]["offset_m_s2"],
        low_hz=result.processing_chain[1]["band_hz"][0],
        high_hz=result.processing_chain[1]["band_hz"][1],
        requested_high_hz=config["filter"]["band_hz"][1],
        filter_order=result.processing_chain[1]["order"],
        nyquist_hz=1 / (2 * result.record.dt_s),
        filtered_peak_m_s2=float(frame.filtered_acceleration_m_s2.abs().max()),
        peak_velocity_m_s=float(frame.velocity_m_s.abs().max()),
        peak_displacement_m=float(frame.displacement_m.abs().max()),
        end_velocity_m_s=float(frame.velocity_m_s.iloc[-1]),
        end_displacement_m=float(frame.displacement_m.iloc[-1]),
        unfiltered_end_velocity_m_s=float(trapezoid(frame.corrected_acceleration_m_s2, frame.time_s)),
    )
    assert payload["metrics"] == expected
    # Hand anchors: 0.31882 g * 9.80665 m/s²/g = 3.12656 m/s² (rounded);
    # 1/(2 * 0.02 s) = 25 Hz; 0.99 * 25 Hz = 24.75 Hz.
    assert payload["metrics"]["pga_m_s2"] == pytest.approx(3.12656, abs=0.000005)
    assert payload["metrics"]["nyquist_hz"] == 25.0
    assert payload["metrics"]["high_hz"] == 24.75
    assert payload["processing_chain"] == result.processing_chain
    assert set(payload["timeseries"]) == set(frame.columns)
    for column in frame.columns:
        np.testing.assert_array_equal(payload["timeseries"][column], frame[column])
    shown = soup.select("#section-5 [data-metric]")
    assert {node["data-metric"] for node in shown} == set(expected)
    for node in soup.select("[data-metric]"):
        value = expected[node["data-metric"]]
        digits = int(node["data-digits"])
        assert node.get_text() == f"{value:.{digits}f}".replace("-", "−")
    worked = soup.select_one("#section-5")
    for node in worked.select("[data-metric], h2, h3, sup, sub"):
        node.decompose()
    for caption in worked.select("figcaption"):
        caption.string = re.sub(r"^Figure 5\.\d+\.", "", caption.get_text())
    assert not re.search(r"\d", worked.get_text()), "Unbound worked-example number"


def test_report_structure_sources_and_self_contained_controls(tmp_path):
    builder().build(tmp_path)
    soup = BeautifulSoup((tmp_path / "seismic-analysis.html").read_text(encoding="utf-8"), "html.parser")
    assert "hs-doc" in soup.body["class"]
    assert len(soup.select("main > section[id^='section-']")) == 10
    assert not soup.select("h4, h5, h6, b, strong, script[src], link[rel='stylesheet']")
    assert soup.select_one("#section-1").find_next_sibling()["id"] == "revision-history"
    assert soup.select_one("#revision-history caption")
    assert soup.select_one("#save-comments")
    assert soup.select_one("#load-comments")
    assert soup.select_one("#changes-panel")
    assert soup.select_one("#comments-status")["role"] == "status"
    assert soup.select_one("#schematic title")
    assert len(soup.select("[data-chart]")) == 4
    for link in soup.select("a[href^='#']"):
        assert soup.find(id=link["href"][1:]), link["href"]
    assert soup.select_one("#ref-record a")["href"] == "https://www.vibrationdata.com/elcentro.dat"
    assert "NOT_EVALUATED" in soup.get_text()
    for fragment in ("free-field", "building floor", "oscillator", "original"):
        assert fragment in soup.get_text()
    changes = json.loads((tmp_path / "changes.json").read_text(encoding="utf-8"))
    assert changes == json.loads(soup.find("script", id="seismic-changes").string)
    current = changes["revisions"][-1]
    assert soup.select_one("header .sub").get_text() == (
        f"Revision {current['revision']} · {current['date']}"
    )
    rows = soup.select("#revision-history tbody tr")
    assert [row.select("td")[0].get_text() for row in rows] == [
        revision["revision"] for revision in changes["revisions"]
    ]
    assert [row.select("td")[1].get_text() for row in rows] == [
        revision["date"] for revision in changes["revisions"]
    ]


def test_report_does_not_require_removed_numpy_trapz(monkeypatch, tmp_path):
    monkeypatch.delattr(np, "trapz", raising=False)
    builder().build(tmp_path)


def test_record_digest_mismatch_blocks_the_report(monkeypatch, tmp_path):
    module = builder()
    monkeypatch.setattr(module, "EXPECTED_RECORD_DIGEST", "0" * 64)
    with pytest.raises(ValueError, match="digest"):
        module.build(tmp_path)
    assert not (tmp_path / "seismic-analysis.html").exists()


def test_dependency_versions_are_informational(monkeypatch, tmp_path):
    module = builder()
    version = module.importlib.metadata.version
    monkeypatch.setattr(
        module.importlib.metadata, "version",
        lambda name: "unlocked" if name == "scipy" else version(name),
    )
    module.build(tmp_path)
    html = (tmp_path / "seismic-analysis.html").read_text(encoding="utf-8")
    evidence = json.loads(BeautifulSoup(html, "html.parser").find(id="seismic-evidence").string)
    assert evidence["versions"]["scipy"] == "unlocked"
    assert "uv.lock" not in {source["path"] for source in evidence["sources"]}
    assert_report_agreement(html,
        (REPORT_DIR / "seismic-analysis.html").read_text(encoding="utf-8")
    )


def test_source_digests_normalise_line_endings(tmp_path):
    path = tmp_path / "source.txt"
    path.write_bytes("acceleration m/s²\n0.0 0.0\n".encode("utf-8"))
    expected = builder().digest(path)
    path.write_bytes(path.read_bytes().replace(b"\n", b"\r\n"))
    assert builder().digest(path) == expected
    assert b"\r\n" in path.read_bytes()


def test_source_locators_use_forward_slashes(monkeypatch):
    module = builder()
    monkeypatch.setattr(module, "digest", lambda path: "example-digest")
    source = module.source_record(PureWindowsPath("sources/record.txt"))
    assert source["path"] == "sources/record.txt"


def test_report_text_io_is_explicitly_utf8_and_lf(tmp_path, monkeypatch):
    module = builder()
    read_text, write_text = Path.read_text, Path.write_text

    def read(path, *args, **kwargs):
        assert kwargs.get("encoding") == "utf-8", path.name
        return read_text(path, *args, **kwargs)

    def write(path, *args, **kwargs):
        assert kwargs.get("encoding") == "utf-8", path.name
        assert kwargs.get("newline") == "\n", path.name
        return write_text(path, *args, **kwargs)

    monkeypatch.setattr(Path, "read_text", read)
    monkeypatch.setattr(Path, "write_text", write)
    module.build(tmp_path)
    for name in ("seismic-analysis.html", "changes.json"):
        assert b"\r\n" not in (tmp_path / name).read_bytes()


def test_initial_window_evidence_is_computed_and_reported(tmp_path):
    module = builder()
    module.build(tmp_path)
    soup = BeautifulSoup((tmp_path / "seismic-analysis.html").read_text(encoding="utf-8"), "html.parser")
    evidence = json.loads(soup.find(id="seismic-evidence").string)
    metrics = evidence["metrics"]
    assert metrics["baseline_offset_m_s2"] == pytest.approx(0.053314, abs=0.0000005)
    assert metrics["unfiltered_end_velocity_m_s"] == pytest.approx(-1.662, abs=0.0005)
    assert "0.0533" in soup.select_one("#section-3").get_text()
    assert "−1.662" in soup.select_one("#section-3").get_text()
    assert "Independent evidence that these samples precede significant motion is absent" not in soup.get_text()


def test_filter_plot_and_source_digests_match_the_documented_calculation(tmp_path):
    builder().build(tmp_path)
    soup = BeautifulSoup((tmp_path / "seismic-analysis.html").read_text(encoding="utf-8"), "html.parser")
    evidence = json.loads(soup.find("script", id="seismic-evidence").string)
    result, config = expected_result()
    settings = result.processing_chain[1]
    actual = FrequencyFilter(order=config["filter"]["order"])
    actual.design_filter(tuple(settings["band_hz"]), 1 / result.record.dt_s, btype="bandpass")
    frequency, response = actual.get_frequency_response(fs=1 / result.record.dt_s)
    np.testing.assert_array_equal(evidence["filter_response"]["frequency_hz"], frequency)
    np.testing.assert_allclose(evidence["filter_response"]["gain"], np.abs(response) ** 2, rtol=0, atol=5e-13)
    np.testing.assert_array_equal(
        evidence["filter_response"]["gain"], np.round(evidence["filter_response"]["gain"], 12)
    )
    assert evidence["metrics"]["low_hz"] < evidence["metrics"]["high_hz"]
    assert evidence["metrics"]["high_hz"] < evidence["metrics"]["nyquist_hz"]
    for source in evidence["sources"]:
        content = (ROOT / source["path"]).read_bytes().replace(b"\r\n", b"\n")
        assert hashlib.sha256(content).hexdigest() == source["sha256"], (
            "Source digest changed; rebuild with PYTHONPATH=src uv run python "
            "scripts/reporting/build_seismic_module_report.py"
        )


def test_house_styles_and_report_text(tmp_path):
    module = builder()
    module.build(tmp_path)
    html = (tmp_path / "seismic-analysis.html").read_text(encoding="utf-8")
    soup = BeautifulSoup(html, "html.parser")
    exemplar = BeautifulSoup((ROOT / module.EXEMPLAR).read_text(encoding="utf-8"), "html.parser")
    assert soup.find_all("style")[0].string == (ROOT / module.BRAND).read_text(encoding="utf-8")
    assert soup.find_all("style")[1].string == exemplar.style.string
    assert not re.search(r"/(?:home|mnt)/|[\w.+-]+@[\w.-]+\.[a-z]{2,}", html, re.I)
    for element in soup.select("style, script"):
        element.decompose()
    visible = soup.get_text(" ", strip=True)
    assert "Draft for maintainer review" in visible
    assert "maintains this derived analysis" in visible
    assert not re.search(r"\b(?:owner|agent|session|could|should|would|certified|compliant)\b", visible, re.I)
    assert not re.search(r"\b(?:we|our|my|I)\b", visible)


def test_documented_router_agrees_with_direct_call_without_engine(tmp_path):
    result, _ = expected_result()
    cfg = yaml.safe_load((EXAMPLE / "input.yml").read_text(encoding="utf-8"))
    cfg["_config_dir_path"] = EXAMPLE.as_posix()
    cfg["response_spectrum"]["output_dir"] = tmp_path.as_posix()
    payload = router(cfg)["response_spectrum"]
    assert payload["summary"] == result.summary
    assert payload["processing_chain"] == result.processing_chain
    assert (tmp_path / "response_spectrum_report.html").exists()
    assert (tmp_path / "response_spectrum_timeseries.csv").exists()


def test_kinematics_matches_constant_acceleration_analytical_motion():
    """The unfiltered unit-acceleration case has v=t and u=t²/2."""
    time = np.linspace(0.0, 2.0, 5)
    record = AccelerogramRecord(
        time_s=time, acceleration_m_s2=np.ones_like(time), units="m_s2",
        source_path=Path("analytical-unit-acceleration.txt"), source_digest="analytical",
    )
    result = compute_kinematics(
        record, baseline={"method": "none"}, frequency_filter={"enabled": False}
    )
    np.testing.assert_array_equal(result.timeseries.velocity_m_s, time)
    np.testing.assert_array_equal(result.timeseries.displacement_m, time**2 / 2)
