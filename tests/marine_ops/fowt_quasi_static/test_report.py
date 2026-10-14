# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Finished-report language, provenance and repeatability contract."""
from hashlib import sha256
from html.parser import HTMLParser
import json
from pathlib import Path
import re

import pytest

ROOT = Path(__file__).resolve().parents[3]
REPORT = ROOT / "docs/reports/fowt-volturnus-mooring-analysis.html"
TITLE = ("Mooring analysis: IEA 15 MW reference turbine on the VolturnUS-S "
         "semi-submersible, static offsets and quasi-static line tensions")


class Text(HTMLParser):
    def __init__(self):
        super().__init__()
        self.visible = []
        self.headings = []
        self.hidden = False
        self.heading = False
        self.section = None
        self.section_text = {}
        self.cell = False
        self.cells = []

    def handle_starttag(self, tag, attrs):
        if tag in ("style", "script"):
            self.hidden = True
        self.heading = tag == "h2"
        if tag == "section":
            self.section = dict(attrs).get("id")
            self.section_text[self.section] = []
        self.cell = tag == "td"

    def handle_endtag(self, tag):
        if tag in ("style", "script"):
            self.hidden = False
        if tag == "h2":
            self.heading = False
        if tag == "section":
            self.section = None
        if tag == "td":
            self.cell = False

    def handle_data(self, data):
        if not self.hidden:
            self.visible.append(data)
        if self.heading:
            self.headings.append(data)
        if self.section is not None and not self.hidden:
            self.section_text[self.section].append(data)
        if self.cell:
            self.cells.append(data)


def test_report_title_sections_and_language():
    assert REPORT.is_file(), "Finished report has not been built"
    parser = Text()
    parser.feed(REPORT.read_text())
    visible = " ".join(parser.visible)
    assert TITLE in visible
    numbered = [h for h in parser.headings if re.match(r"\d+ ", h)]
    assert len(numbered) == 10
    forbidden = r"\b(?:skeleton|worked example|vendor|OrcaFlex|pass|fail|verdict|review|script|repository|Checked|Approved)\b"
    assert not re.search(forbidden, visible, re.I)
    assert not re.search(r"(?:src|docs|examples)/|file://|/(?:home|mnt|tmp)/", visible)
    for identifier, content in parser.section_text.items():
        if identifier not in ("section-4", "section-9"):
            assert not re.search(r"\b(?:Capytaine|MoorPy)\b", " ".join(content))
    assert "AceEngineer Inc." in visible
    assert "assumed, based on previous experience" in visible


def test_report_table_values_have_four_significant_figures():
    parser = Text()
    parser.feed(REPORT.read_text())
    for cell in parser.cells:
        for value in re.findall(r"(?<![\w.])[+-]?\d+(?:\.\d+)?(?:e[+-]?\d+)?", cell, re.I):
            mantissa = value.lower().split("e")[0].lstrip("+-").replace(".", "").lstrip("0")
            assert len(mantissa) <= 4, value


def test_convergence_table_identifies_the_governing_excitation_heading():
    html = REPORT.read_text()
    assert "Heading [°]" in html
    assert "heading-independent" in html


def test_large_damaged_excursion_is_a_finding_with_a_design_action():
    html = REPORT.read_text()
    assert "Large damaged excursion" in html
    assert "mooring-layout and allowable-excursion assessment" in html


def test_standalone_report_retains_the_entire_publisher_data_notice():
    parser = Text()
    parser.feed(REPORT.read_text())
    source = (ROOT/"docs/domains/orcaflex/wind/fowt-volturnus/SOURCES.md").read_text()
    notice = source.split("## Publisher data notice", 1)[1].split("\n## ", 1)[0]
    assert " ".join(notice.split()) in " ".join(" ".join(parser.visible).split())


def test_manifest_and_rebuild_are_byte_identical(tmp_path):
    from digitalmodel.marine_ops.fowt_quasi_static.report import build_report
    manifest_path = REPORT.with_suffix(".manifest.json")
    manifest = json.loads(manifest_path.read_text())
    assert sha256(REPORT.read_bytes()).hexdigest() == manifest["html_sha256"]
    for path, digest in manifest["input_digests"].items():
        assert sha256((ROOT/path).read_bytes()).hexdigest() == digest
    first, second = tmp_path/"first", tmp_path/"second"
    build_report(ROOT, manifest["results"], first)
    build_report(ROOT, manifest["results"], second)
    for path in first.iterdir():
        assert path.read_bytes() == (second/path.name).read_bytes()
    assert (first/REPORT.name).read_bytes() == REPORT.read_bytes()


def test_retained_results_require_the_complete_input_digest_set():
    import importlib.util
    path = ROOT/"scripts/reporting/build_fowt_mooring_report.py"
    spec = importlib.util.spec_from_file_location("fowt_builder", path)
    builder = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(builder)
    with pytest.raises(ValueError, match="complete input digest"):
        builder.retained_results(ROOT, {"input_digests": {}, "results": {}})


def test_case_completeness_requires_unique_heading_sea_state_combinations():
    from digitalmodel.marine_ops.fowt_quasi_static.report import validate_cases
    duplicate = {"heading": 0., "sea": "operating", "state": "intact",
                 "governing": {"utilisation": 0.5}}
    with pytest.raises(ValueError, match="unique"):
        validate_cases([duplicate.copy() for _ in range(12)])
