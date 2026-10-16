# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Public titles and series provenance for the synthetic report self-test."""

import json
import re
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.report_generator import (
    DiffractionReportData,
    generate_diffraction_report,
)
from tests.hydrodynamics.diffraction import test_unit_box_benchmark as builder

ROOT = Path(__file__).resolve().parents[3]
EXPECTED_TITLE = (
    "Report-pipeline self-test on synthetic coefficients: "
    "not a comparison of analysis programs"
)
PROGRAM_NAMES = re.compile(r"aqwa|orcawave|bemrosetta", re.IGNORECASE)
FOLDERS = ("docs/benchmarks/unit_box", "docs/api/hydro/unit-box-benchmark")
PAGES = tuple(
    f"{folder}/{name}"
    for folder in FOLDERS
    for name in builder.DOCS_ARTIFACT_NAMES
    if name.endswith(".html")
)
PLOT_PAGES = tuple(
    relative for relative in PAGES
    if relative.endswith(("benchmark_amplitude.html", "benchmark_phase.html"))
)


class Headings(HTMLParser):
    """Read the document title and main headings without rendering a page."""

    def __init__(self, text):
        super().__init__()
        self.current = None
        self.titles = []
        self.headings = []
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        if tag in ("title", "h1"):
            self.current = self.titles if tag == "title" else self.headings
            self.current.append("")

    def handle_endtag(self, tag):
        if tag in ("title", "h1"):
            self.current = None

    def handle_data(self, text):
        if self.current is not None:
            self.current[-1] += text


def plotly_layout(text):
    """Decode the layout argument of the figure that the page renders."""
    call = re.search(r"Plotly\.newPlot\(\s*", text)
    assert call, "missing Plotly figure"
    decoder = json.JSONDecoder()
    offset = call.end()
    for _ in range(2):
        _, offset = decoder.raw_decode(text, offset)
        separator = re.match(r"\s*,\s*", text[offset:])
        assert separator, "missing Plotly argument separator"
        offset += separator.end()
    layout, end = decoder.raw_decode(text, offset)
    return layout, offset, end


def assert_visible_plot_title(text):
    layout, _, _ = plotly_layout(text)
    assert layout.get("title", {}).get("text") == EXPECTED_TITLE, "missing visible label"


@pytest.mark.parametrize("relative", PAGES)
def test_committed_selftest_titles(relative):
    text = (ROOT / relative).read_text(encoding="utf-8")
    page = Headings(text)
    assert page.titles == [EXPECTED_TITLE]
    if relative in PLOT_PAGES:
        assert_visible_plot_title(text)
    if relative.endswith("benchmark_report.html"):
        assert page.headings == [EXPECTED_TITLE]


@pytest.mark.parametrize("relative", PLOT_PAGES)
def test_plot_title_label_cannot_be_replaced_by_document_title(relative, tmp_path):
    text = (ROOT / relative).read_text(encoding="utf-8")
    layout, start, end = plotly_layout(text)
    del layout["title"]
    copy = tmp_path / "missing-figure-title.html"
    copy.write_text(text[:start] + json.dumps(layout) + text[end:], encoding="utf-8")
    altered = copy.read_text(encoding="utf-8")
    assert Headings(altered).titles == [EXPECTED_TITLE]
    with pytest.raises(AssertionError, match="missing visible label"):
        assert_visible_plot_title(altered)


@pytest.mark.parametrize("relative", PAGES)
def test_committed_selftest_has_no_program_names(relative):
    text = (ROOT / relative).read_text(encoding="utf-8")
    assert not PROGRAM_NAMES.search(text), relative


def test_fresh_selftest_series_names():
    results = builder._build_unit_box_solver_set()
    assert list(results) == ["Series A", "Series B", "Series C"]
    assert [result.analysis_tool for result in results.values()] == list(results)


@pytest.mark.parametrize("name", ["benchmark_report.json", "hydro_data.yml"])
def test_committed_structured_selftest_series_names(name):
    text = (ROOT / FOLDERS[0] / name).read_text(encoding="utf-8")
    data = json.loads(text) if name.endswith(".json") else yaml.safe_load(text)
    names = data["solver_names"] if name.endswith(".json") else data["metadata"]["solver_names"]
    assert sorted(names) == ["Series A", "Series B", "Series C"]
    assert not PROGRAM_NAMES.search(text)


def test_fresh_selftest_titles(tmp_path):
    result = builder._run_unit_box_benchmark(tmp_path, builder._build_unit_box_solver_set())
    assert result.success, result.error_message
    for name in builder.DOCS_ARTIFACT_NAMES:
        if name.endswith(".html"):
            text = (tmp_path / name).read_text(encoding="utf-8")
            page = Headings(text)
            assert page.titles == [EXPECTED_TITLE], name
            if name in ("benchmark_amplitude.html", "benchmark_phase.html"):
                assert_visible_plot_title(text)


def test_default_document_title_is_unchanged(tmp_path):
    """Regression pin: callers without an override retain their title."""
    data = DiffractionReportData(vessel_name="UnitBox", report_title="Existing heading")
    page = generate_diffraction_report(data, tmp_path / "report.html")
    headings = Headings(page.read_text(encoding="utf-8"))
    assert headings.titles == ["Diffraction Report - UnitBox"]
    assert headings.headings == ["Existing heading"]


def numeric_leaves(value, path=()):
    """Match numeric leaves by series identity, independent of label order."""
    mapping = {"aqwa": "Series A", "orcawave": "Series B", "bemrosetta": "Series C"}
    leaves = {}
    if isinstance(value, dict):
        for key, item in value.items():
            key = PROGRAM_NAMES.sub(lambda match: mapping[match.group().lower()], key)
            if "-vs-" in key:
                key = "-vs-".join(sorted(key.split("-vs-")))
            leaves.update(numeric_leaves(item, path + (key,)))
    elif isinstance(value, list):
        for index, item in enumerate(value):
            leaves.update(numeric_leaves(item, path + (str(index),)))
    elif isinstance(value, (int, float)) and not isinstance(value, bool):
        leaves[path] = value
    return leaves


def test_series_renaming_preserves_exact_numeric_metrics(tmp_path):
    """Renaming seeded inputs must preserve even the last stored digit."""
    names = ("AQWA", "OrcaWave", "BEMRosetta")
    legacy = {name: builder._build_unit_box_results(name, seed=index)
              for index, name in enumerate(names)}
    runner = builder.BenchmarkRunner(builder.BenchmarkConfig(**builder.UNIT_BOX_POLICY_INPUTS))
    expected = runner._report_to_dict(runner._compare(legacy))
    result = builder._run_unit_box_benchmark(tmp_path, builder._build_unit_box_solver_set())
    assert result.success, result.error_message
    actual = json.loads(result.report_json_path.read_text(encoding="utf-8"))
    assert numeric_leaves(actual) == numeric_leaves(expected)


@pytest.mark.parametrize("order", [
    ["Series A", "Series A", "Series C"],
    ["Series A", "Series C"],
])
def test_comparison_order_requires_every_series_once(order):
    config = builder.BenchmarkConfig(comparison_order=order, **builder.UNIT_BOX_POLICY_INPUTS)
    with pytest.raises(ValueError, match="comparison_order must list each series exactly once"):
        builder.BenchmarkRunner(config)._compare(builder._build_unit_box_solver_set())
