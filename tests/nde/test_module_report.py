# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the NDE report and compare its example with the published fixture."""

import importlib.util
import json
import re
import runpy
import shutil
from html.parser import HTMLParser
from pathlib import Path

import pytest

from digitalmodel.nde.pulse_timing import pulse_peak_time


ROOT = Path(__file__).resolve().parents[2]
REPORT_DIR = ROOT / "docs/reports/modules/nde"


def load_builder():
    path = ROOT / "scripts/reporting/build_nde_report.py"
    assert path.is_file(), "The NDE report generator has not been implemented"
    spec = importlib.util.spec_from_file_location("nde_report_builder", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class ReportParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.ids = []
        self.references = []
        self.metrics = []
        self.active_metric = None
        self.metric_text = ""

    def handle_starttag(self, tag, attributes):
        attrs = dict(attributes)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if attrs.get("href", "").startswith("#"):
            self.references.append(attrs["href"][1:])
        if "data-metric" in attrs:
            self.active_metric = attrs["data-metric"]
            self.metric_text = ""

    def handle_data(self, text):
        if self.active_metric is not None:
            self.metric_text += text

    def handle_endtag(self, tag):
        if tag == "span" and self.active_metric is not None:
            self.metrics.append((self.active_metric, self.metric_text))
            self.active_metric = None


def report_data(document, identifier="example-data"):
    match = re.search(
        rf'<script id="{identifier}" type="application/json">(.*?)</script>',
        document,
        re.S,
    )
    assert match, f"Missing embedded {identifier}"
    return json.loads(match[1])


def test_rebuild_matches_committed_report_and_changes(tmp_path, monkeypatch):
    builder = load_builder()
    monkeypatch.setenv("GIT_DIR", str(tmp_path / "unrelated.git"))
    monkeypatch.setenv("GIT_WORK_TREE", str(tmp_path / "unrelated"))
    monkeypatch.setenv("GIT_COMMON_DIR", str(tmp_path / "common.git"))
    builder.build_report(tmp_path)
    for name in ("nde-module-analysis.html", "changes.json"):
        expected = (REPORT_DIR / name).read_bytes().replace(b"\r\n", b"\n")
        assert (tmp_path / name).read_bytes() == expected, (
            "Rebuild with: PYTHONPATH=src uv run --no-project python "
            "scripts/reporting/build_nde_report.py"
        )
    assert report_data(
        (tmp_path / "nde-module-analysis.html").read_text(encoding="utf-8"), "changes-data"
    ) == json.loads((tmp_path / "changes.json").read_text(encoding="utf-8"))


@pytest.mark.parametrize("default_newline", ["\n", "\r\n"], ids=["posix", "windows"])
@pytest.mark.parametrize("name", ["nde-module-analysis.html", "changes.json"])
def test_report_output_bytes_use_utf8_and_lf(
    tmp_path, monkeypatch, default_newline, name
):
    builder = load_builder()
    write_text = Path.write_text

    def platform_write_text(path, data, encoding=None, errors=None, newline=None):
        # Use real file writes, emulating only the platform's default translation.
        return write_text(
            path, data, encoding=encoding, errors=errors,
            newline=default_newline if newline is None else newline,
        )

    monkeypatch.setattr(Path, "write_text", platform_write_text)
    builder.build_report(tmp_path)
    expected = (REPORT_DIR / name).read_bytes().replace(b"\r\n", b"\n")
    assert (tmp_path / name).read_bytes() == expected, (
        "Rebuild with: PYTHONPATH=src uv run --no-project python "
        "scripts/reporting/build_nde_report.py"
    )


def test_source_digests_ignore_checkout_line_endings(tmp_path, monkeypatch):
    builder = load_builder()
    sources = [*builder.LOCAL_SOURCES.values(), "pyproject.toml",
               builder.EXEMPLAR, "docs/api/_assets/brand.css"]
    for source in sources:
        target = tmp_path / source
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(ROOT / source, target)
    monkeypatch.setattr(builder, "ROOT", tmp_path)
    expected_example = builder.compute_example()
    expected_document = builder.render_report()
    for source in sources:
        target = tmp_path / source
        target.write_bytes(target.read_bytes().replace(b"\r\n", b"\n")
                           .replace(b"\n", b"\r\n"))
    assert builder.compute_example() == expected_example
    assert builder.render_report() == expected_document


def test_worked_values_match_public_example_and_direct_module_call():
    builder = load_builder()
    data = builder.compute_example()
    fixture = runpy.run_path(str(ROOT / "tests/nde/test_pulse_timing.py"))
    captured = []

    def capture(times, pressures):
        captured.append((list(times), list(pressures)))
        return pulse_peak_time(times, pressures)

    fixture["test_synthetic_pulse_travel_time"].__globals__["pulse_peak_time"] = capture
    for case in data["cases"]:
        fixture["test_synthetic_pulse_travel_time"](case["polarity"])
        times, pressures = captured[-1]
        assert data["inputs"]["time_s"] == times
        assert case["pressure"] == pressures
        assert case["peak_time_s"] == pulse_peak_time(times, pressures)
        # Fixture hand calculation: 6 m / (2 m/s) = 3 s for either polarity.
        assert case["peak_time_s"] == 3.0
        assert case["reference_peak_time_s"] == 3.0
        assert case["difference_percent"] == 0.0


def test_visible_example_numbers_and_plot_points_come_from_results():
    builder = load_builder()
    document = builder.render_report()
    data = report_data(document)
    assert data == builder.compute_example()
    parser = ReportParser()
    parser.feed(document)
    metrics = builder.example_metrics(data)
    assert set(key for key, _ in parser.metrics) == set(metrics)
    for key, displayed in parser.metrics:
        assert displayed == builder.format_number(metrics[key])
    for case in data["cases"]:
        for index, pressure in enumerate(case["pressure"]):
            assert (
                f'data-case="{case["name"]}" data-index="{index}" '
                f'data-time="{data["inputs"]["time_s"][index]}" '
                f'data-pressure="{pressure}"'
            ) in document
    worked = re.search(r'<section id="s6">(.*?)</section>', document, re.S)[1]
    worked = re.sub(r"<h2>.*?</h2>", "", worked)
    worked = re.sub(r'<span data-metric="[^"]+">[^<]+</span>', "", worked)
    visible = re.sub(r"<[^>]+>", "", worked)
    assert not re.search(r"\d", visible), "A worked-example number was typed by hand"


def test_structure_sources_and_single_file_delivery():
    builder = load_builder()
    document = builder.render_report()
    parser = ReportParser()
    parser.feed(document)
    assert len(parser.ids) == len(set(parser.ids))
    assert set(parser.references) <= set(parser.ids)
    assert all(f'<section id="s{i}">' in document for i in range(1, 11))
    assert '<body class="hs-doc">' in document
    assert not re.search(r'<(?:script|link|img)[^>]+(?:src|href)="https?://', document)
    assert builder.house_style() in document
    for source in builder.LOCAL_SOURCES.values():
        assert (ROOT / source).is_file()
    assert "NOT_EVALUATED" in document
    assert "ASSUMED - to be confirmed" in document
    assert "first-arrival detector" in document
    assert "Save comments" in document
    assert "showDirectoryPicker" in document


def test_missing_layout_styles_fail_explicitly(tmp_path, monkeypatch):
    builder = load_builder()
    for source in (builder.EXEMPLAR, "docs/api/_assets/brand.css"):
        target = tmp_path / source
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(ROOT / source, target)
    (tmp_path / builder.EXEMPLAR).write_text(
        "<html></html>", encoding="utf-8", newline="\n")
    monkeypatch.setattr(builder, "ROOT", tmp_path)
    with pytest.raises(ValueError, match="layout stylesheet"):
        builder.house_style()


def test_fixture_refactor_does_not_change_calculation(tmp_path, monkeypatch):
    builder = load_builder()
    expected = builder.compute_example()
    fixture_path = builder.LOCAL_SOURCES["B"]
    source = (ROOT / fixture_path).read_text(encoding="utf-8")
    changed = tmp_path / fixture_path
    changed.parent.mkdir(parents=True)
    changed.write_text(source.replace("test_synthetic_pulse_travel_time",
                                     "test_isolated_pulse_travel_time"),
                       encoding="utf-8", newline="\n")
    monkeypatch.setattr(builder, "ROOT", tmp_path)
    actual = builder.compute_example()
    assert actual["inputs"] == expected["inputs"]
    assert actual["cases"] == expected["cases"]
    assert actual["source_digest"] != expected["source_digest"]


def test_builder_is_discovered_with_a_reasoned_ownership_entry():
    from scripts import check_generated_html as checker

    source = "scripts/reporting/build_nde_report.py"
    assert source in checker.discover_candidate_generators(ROOT)
    assert checker.DISCOVERY_FALSE_POSITIVES[source].strip()
    assert checker.validate_registry(ROOT) == []
