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


@pytest.mark.parametrize("relative", PAGES)
def test_committed_selftest_titles(relative):
    page = Headings((ROOT / relative).read_text(encoding="utf-8"))
    assert page.titles == [EXPECTED_TITLE]
    if relative.endswith("benchmark_report.html"):
        assert page.headings == [EXPECTED_TITLE]


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
            page = Headings((tmp_path / name).read_text(encoding="utf-8"))
            assert page.titles == [EXPECTED_TITLE], name


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
