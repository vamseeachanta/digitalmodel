# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Numerical and document contracts for the public spectral example."""

import importlib.util
import json
import re
import subprocess
import sys
from pathlib import Path
from urllib.parse import unquote, urlsplit

import numpy as np
import pytest

from digitalmodel.signal_processing.signal_analysis import (
    SpectralAnalyzer,
    TimeSeriesProcessor,
)

ROOT = Path(__file__).resolve().parents[3]
REPORT = ROOT / "docs/reports/modules/signalprocessing"


@pytest.fixture
def builder():
    spec = importlib.util.spec_from_file_location("signal_report", REPORT / "build_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def independent_welch(signal, fs, size, overlap):
    """Implement the documented mean/Hann density without scipy.signal.welch."""
    window = (1 - np.cos(2 * np.pi * np.arange(size) / size)) / 2
    starts = range(0, len(signal) - size + 1, size - overlap)
    segments = np.array([signal[start:start + size] for start in starts])
    segments -= segments.mean(axis=1, keepdims=True)
    transformed = np.fft.rfft(segments * window, axis=1)
    density = np.mean(np.abs(transformed) ** 2, axis=0) / (fs * sum(window ** 2))
    density[1:-1] *= 2
    return np.fft.rfftfreq(size, 1 / fs), density


def test_public_input_basis(builder):
    basis = builder.read_basis()
    assert basis["source"]["edition"] == "SciPy 1.14.1"
    assert basis["source"]["section"] == "welch: Examples"
    assert basis["inputs"] == {
        "sampling_rate_hz": 10000,
        "samples": 100000,
        "sine_rms_v": 2,
        "sine_frequency_hz": 1234,
        "noise_density_v2_per_hz": 0.001,
        "segment_samples": 1024,
    }
    assert basis["adaptation"]["random_seed"] == 0
    assert "reference" in basis["adaptation"]


def test_all_spectral_bins_against_independent_equation(builder):
    basis = builder.read_basis()
    signal = builder.make_signal(basis)
    inputs = basis["inputs"]
    size = inputs["segment_samples"]
    spectrum = SpectralAnalyzer(inputs["sampling_rate_hz"], "welch").compute_spectrum(
        signal, nperseg=size, noverlap=size // 2, window="hann",
    )
    frequency, density = independent_welch(signal, inputs["sampling_rate_hz"], size, size // 2)
    np.testing.assert_array_equal(spectrum.frequency, frequency)
    # Report-defined numerical equivalence criterion, unrelated to instrument accuracy.
    np.testing.assert_allclose(spectrum.power, density, rtol=1e-12, atol=1e-15)


def test_every_result_and_plot_value_equals_module_output(builder):
    basis = builder.read_basis()
    signal = builder.make_signal(basis)
    size = basis["inputs"]["segment_samples"]
    fs = basis["inputs"]["sampling_rate_hz"]
    analyzer = SpectralAnalyzer(fs, "welch")
    frame = analyzer.compute_spectrum(signal, nperseg=size, noverlap=size // 2, window="hann")
    peak = analyzer.find_peaks(frame, n_peaks=1).iloc[0]
    stats = TimeSeriesProcessor().calculate_statistics(signal)
    result = builder.calculate(basis)
    np.testing.assert_array_equal(result["spectrum"]["frequency_hz"], frame.frequency)
    np.testing.assert_array_equal(result["spectrum"]["density_v2_per_hz"], frame.power)
    df = float(frame.frequency.iloc[1] - frame.frequency.iloc[0])
    noise = float(frame.power.iloc[size // 4:].mean())
    expected = {
        **basis["inputs"],
        "overlap_samples": size // 2,
        "random_seed": basis["adaptation"]["random_seed"],
        "duration_s": stats["length"] / fs,
        "rms_v": float(stats["rms"]),
        "variance_v2": float(stats["var"]),
        "bins": len(frame),
        "segments": 1 + (len(signal) - size) // (size - size // 2),
        "spacing_hz": df,
        "nyquist_hz": float(frame.frequency.iloc[-1]),
        "peak_hz": float(peak.frequency),
        "peak_density_v2_per_hz": float(peak.power),
        "peak_error_hz": abs(float(peak.frequency) - basis["inputs"]["sine_frequency_hz"]),
        "peak_limit_hz": df / 2,
        "noise_density_estimate_v2_per_hz": noise,
        "noise_error_percent": abs(noise / basis["inputs"]["noise_density_v2_per_hz"] - 1) * 100,
        "noise_tolerance_percent": basis["criteria"]["noise_relative_tolerance_percent"],
        "integrated_density_v2": float(frame.power.sum() * df),
        "noise_band_start_hz": float(frame.frequency.iloc[size // 4]),
    }
    assert result["metrics"] == expected
    assert result["criteria_met"]["peak"]
    assert result["criteria_met"]["noise"]


def test_rebuild_is_exact_and_detects_tampering(builder, tmp_path):
    builder.build(tmp_path)
    for name in ("report.html", "results.json", "changes.json"):
        assert (tmp_path / name).read_bytes() == (REPORT / name).read_bytes()
    command = [sys.executable, str(REPORT / "build_report.py"), "--output-dir", str(tmp_path), "--check"]
    assert subprocess.run(command, capture_output=True).returncode == 0
    target = tmp_path / "report.html"
    target.write_text(target.read_text() + "modified", encoding="utf-8")
    failed = subprocess.run(command, capture_output=True, text=True)
    assert failed.returncode != 0
    assert "report.html" in failed.stderr


def test_displayed_worked_numbers_and_embedded_arrays(builder):
    page = (REPORT / "report.html").read_text()
    result = builder.calculate(builder.read_basis())
    embedded = re.search(r'<script id="calculation-data" type="application/json">(.*?)</script>', page, re.S)
    assert json.loads(embedded[1]) == result
    values = re.findall(r'<span data-metric="([^"]+)">([^<]+)</span>', page)
    assert set(key for key, _ in values) == set(result["metrics"])
    for key, value in values:
        assert value == builder.format_metric(key, result["metrics"][key])
    worked = re.search(r'<section id="section-6">(.*?)</section>', page, re.S)[1]
    prose = re.sub(r'<span data-metric="[^"]+">.*?</span>', "", worked)
    prose = re.sub(r'<h[23]>.*?</h[23]>', "", prose)
    prose = re.sub(r'<figure.*?</figure>', "", prose, flags=re.S)
    prose = re.sub(r'<[^>]+>', "", prose)
    assert not re.search(r'\d', prose), "Worked-example prose contains a hand-entered number"


def test_documented_python_example_is_runnable(builder, monkeypatch):
    monkeypatch.chdir(ROOT)
    page = (REPORT / "report.html").read_text()
    code = re.search(r'<pre>(import json.*?)</pre>', page, re.S)[1]
    namespace = {}
    exec(compile(code, "documented-example", "exec"), namespace)
    result = builder.calculate(builder.read_basis())
    np.testing.assert_array_equal(namespace["spectrum"].power, result["spectrum"]["density_v2_per_hz"])
    assert namespace["statistics"]["rms"] == result["metrics"]["rms_v"]


def test_house_style_structure_citations_and_public_wording(builder):
    page = (REPORT / "report.html").read_text()
    assert '<body class="hs-doc">' in page
    source_page = (ROOT / "docs/reports/pipeline-ffs-assessment.html").read_text()
    assert re.search(r'<style>.*?</style>', source_page, re.S)[0] in page
    assert len(re.findall(r'<section id="section-\d+">', page)) == 10
    assert not re.search(r'<h[4-6]|<strong>|<b>', page)
    ids = re.findall(r'\bid="([^"]+)"', page)
    assert len(ids) == len(set(ids))
    for anchor in re.findall(r'href="#([^"]+)"', page):
        assert anchor in ids
    for href in re.findall(r'href="([^"]+)"', page):
        link = urlsplit(href)
        if not link.scheme and link.path:
            assert (REPORT / unquote(link.path)).resolve().exists(), href
    assert not re.search(r'<(?:link|script)[^>]+(?:href|src)=', page)
    assert not re.search(r'\b(?:owner|could|should|would|certified|compliant)\b', page, re.I)
    assert not re.search(r'(?:/(?:home|mnt)/|[\w.+-]+@[\w.-]+\.[a-z]{2,})', page, re.I)
    assert "Save comments beside report" in page
    assert "showSaveFilePicker" in page
    assert "Load comments" in page
    assert "Nyquist sampling boundary" in page
    assert "Noise comparison upper limit" in page
    assert "ASSUMED - to be confirmed" in page
    assert "NOT_EVALUATED" in page


def test_evidence_and_source_digests(builder):
    evidence = json.loads((REPORT / "validation.json").read_text())
    assert evidence["failed"] == evidence["errors"] == 0
    assert evidence["passed"] > 0
    builder.verify_evidence(evidence)
    changed = json.loads(json.dumps(evidence))
    first_path = next(iter(changed["sha256"]))
    changed["sha256"][first_path] = "0" * 64
    with pytest.raises(ValueError, match="digest"):
        builder.verify_evidence(changed)


def test_change_panel_matches_sidecar():
    page = (REPORT / "report.html").read_text()
    changes = json.loads((REPORT / "changes.json").read_text())
    for revision in changes["revisions"]:
        for sentence in revision["changes"]:
            assert sentence in page


@pytest.fixture
def browser_page():
    sync_playwright = pytest.importorskip("playwright.sync_api").sync_playwright

    with sync_playwright() as runtime:
        browser = runtime.chromium.launch(headless=True)
        page = browser.new_page(accept_downloads=True)
        errors, remote_requests = [], []
        page.on("pageerror", lambda error: errors.append(str(error)))
        page.on("request", lambda request: remote_requests.append(request.url) if request.url.startswith("http") else None)
        page.goto((REPORT / "report.html").as_uri())
        yield page
        assert not errors
        assert not remote_requests
        browser.close()


@pytest.mark.selenium
def test_browser_charts_and_responsive_layout(browser_page):
    page = browser_page
    assert page.locator("#spectrum-chart .chart-line").get_attribute("d")
    page.locator("#frequency-cursor").evaluate("el => {el.value=128;el.dispatchEvent(new Event('input'));}")
    assert "1250.0 Hz" in page.locator("#spectrum-readout").inner_text()
    page.select_option("#spectrum-view", "tone")
    assert "1230.5 Hz" in page.locator("#spectrum-readout").inner_text()
    page.select_option("#spectrum-view", "full")
    assert page.locator("#spectrum-chart .limit-line").count() == 1
    page.locator("#noise-chart circle").focus()
    assert "limits:" in page.locator("#noise-readout").inner_text()
    for width in (1280, 390):
        page.set_viewport_size({"width": width, "height": 900})
        assert page.evaluate("document.documentElement.scrollWidth <= innerWidth")


@pytest.mark.selenium
def test_browser_comments_transfer_and_binding(browser_page, tmp_path):
    page = browser_page
    page.select_option("#comment-section", "section-6")
    text = "Check the acquisition basis. <img src=x onerror=alert(1)>"
    page.fill("#comment-text", text)
    page.click("#add-comment")
    assert page.locator("#comment-list li").count() == 1
    assert page.locator("#comment-list img").count() == 0
    with page.expect_download() as download:
        page.click("#download-comments")
    saved = tmp_path / "report.comments.json"
    download.value.save_as(saved)
    packet = json.loads(saved.read_text())
    assert packet["comments"][0]["text"] == text
    page.reload()
    page.set_input_files("#load-comments", saved)
    assert page.locator("#comment-list li").count() == 1
    page.set_input_files("#load-comments", saved)
    assert page.locator("#comment-list li").count() == 1
    packet["binding"] = "wrong document"
    saved.write_text(json.dumps(packet))
    page.set_input_files("#load-comments", saved)
    assert "do not match" in page.locator("#comment-status").inner_text()
    assert page.locator("#comment-list li").count() == 1


@pytest.mark.selenium
def test_browser_direct_comment_save(browser_page):
    page = browser_page
    page.fill("#comment-text", "Retain the acquisition basis.")
    page.click("#add-comment")
    page.evaluate("""() => { window.showSaveFilePicker = async () => ({
        getFile: async () => new Blob([JSON.stringify({
            document: 'Signal processing module analysis', revision: '00',
            binding: document.querySelector('meta[name="document-binding"]').content,
            comments: [{id: 'earlier-note', section: 'section-4', text: 'Retain the earlier note.'}]
        })]),
        createWritable: async () => ({write: async text => {window.savedComments=text;}, close: async () => {}})
    }); }""")
    page.click("#save-comments")
    page.wait_for_function("window.savedComments")
    assert len(json.loads(page.evaluate("window.savedComments"))["comments"]) == 2
