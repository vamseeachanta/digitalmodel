# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild and independently check the public seismic module report."""

import importlib.util
import hashlib
import json
import re
from pathlib import Path, PureWindowsPath

import numpy as np
import pytest
import yaml
from bs4 import BeautifulSoup

from digitalmodel.seismic import AccelerogramRecord, read_ascii_accelerogram
from digitalmodel.seismic.response_spectrum.calculations import compute_kinematics
from digitalmodel.seismic.response_spectrum.workflow import router
from digitalmodel.signal_processing.signal_analysis.filters.frequency import FrequencyFilter


ROOT = Path(__file__).resolve().parents[2]
REPORT_DIR = ROOT / "docs/reports/modules/seismic"
EXAMPLE = ROOT / "examples/workflows/response-spectrum"


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
    """Compare issued content while retaining informational version differences."""
    soup = BeautifulSoup(html, "html.parser")
    evidence = soup.find("script", id="seismic-evidence")
    payload = json.loads(evidence.string)
    payload.pop("versions")
    evidence.string = json.dumps(payload, ensure_ascii=False, sort_keys=True)
    for cell in soup.select("#dependency-versions tbody tr td:nth-child(2)"):
        cell.string = ""
    return str(soup)


def test_rebuild_matches_issued_content_except_dependency_versions(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)
    builder().build(tmp_path)
    for name in ("seismic-analysis.html", "changes.json"):
        first = (tmp_path / name).read_bytes()
        if name.endswith(".html"):
            assert report_content(first.decode("utf-8")) == report_content(
                (REPORT_DIR / name).read_text(encoding="utf-8")
            )
        else:
            assert first == (REPORT_DIR / name).read_bytes()
        builder().build(tmp_path)
        assert first == (tmp_path / name).read_bytes()


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
        unfiltered_end_velocity_m_s=float(np.trapz(frame.corrected_acceleration_m_s2, frame.time_s)),
    )
    assert payload["metrics"] == expected
    assert payload["metrics"]["pga_m_s2"] == pytest.approx(0.31882 * 9.80665, abs=0.000005)
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
    assert report_content(html) == report_content(
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
        assert hashlib.sha256(content).hexdigest() == source["sha256"]


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
    assert "Draft for review" in visible
    assert "maintains this derived analysis" in visible
    assert not re.search(r"\b(?:owner|agent|session|could|should|would|certified|compliant)\b", visible, re.I)
    assert not re.search(r"\b(?:we|our|my|I)\b", visible)


def test_documented_router_agrees_with_direct_call_without_engine(tmp_path):
    result, _ = expected_result()
    cfg = yaml.safe_load((EXAMPLE / "input.yml").read_text(encoding="utf-8"))
    cfg["_config_dir_path"] = str(EXAMPLE)
    cfg["response_spectrum"]["output_dir"] = str(tmp_path)
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
