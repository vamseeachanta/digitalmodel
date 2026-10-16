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


def test_revision_history_follows_summary_and_retains_initial_issue():
    parser = Text()
    parser.feed(REPORT.read_text())
    assert parser.headings[:3] == ["1 Executive Summary", "Revision history", "2 Introduction"]
    history = " ".join(parser.section_text["revision-history"])
    assert "A" in history and "B" in history
    assert "14 October 2026" in history
    assert "both published RNA inertia readings" in history
    assert "Revision C" in REPORT.read_text()


def test_offsets_use_one_decimal_and_document_roundoff_tolerance():
    parser = Text()
    parser.feed(REPORT.read_text())
    summary = " ".join(parser.section_text["section-1"])
    assert "(29.3, 0.0) m" in summary
    assert not re.search(r"e-\d+", summary)
    assert "below 0.000001 m" in " ".join(parser.visible)
    from digitalmodel.marine_ops.fowt_quasi_static.report import offset
    assert offset(-9.781e-16) == "0.0"
    assert offset(-1e-7) == "0.0"
    assert offset(29.25) == "29.3"
    assert offset(-0.051) == "-0.1"


def test_damaged_capacity_governs_and_strength_is_secondary():
    parser = Text()
    parser.feed(REPORT.read_text())
    for identifier in ("section-1", "section-5", "section-6"):
        content = " ".join(parser.section_text[identifier])
        assert "no stationkeeping capacity at rated thrust" in content
        assert content.index("no stationkeeping capacity") < content.index("utilisation")
    assert "No evaluated utilisation exceeds" not in " ".join(parser.visible)


def test_project_strength_criterion_has_independent_stated_basis():
    import yaml
    parser = Text()
    parser.feed(REPORT.read_text())
    criteria = " ".join(parser.section_text["section-3"])
    assert "three multiplicative 25 % allowances" in criteria
    assert "assumed" in criteria
    basis = yaml.safe_load((ROOT / "docs/domains/orcaflex/wind/fowt-volturnus/basis.yml").read_text())
    from digitalmodel.marine_ops.fowt_quasi_static.workflow import load_basis
    assert load_basis(ROOT)["analysis"] == basis["analysis"]
    for state in ("intact", "damaged"):
        factor = basis["analysis"][f"{state}_tension_factor"]
        assert factor["value"] == 1.25 ** 3
        assert "assumed" in factor["source"]
    assert "Clause titles are not established" not in " ".join(parser.visible)
    manifest = json.loads(REPORT.with_suffix(".manifest.json").read_text())
    strength = basis["mooring"]["minimum_breaking_load"]["value"]
    for case in manifest["results"]["cases"]:
        for line in case["lines"]:
            assert line["utilisation"] == pytest.approx(line["tension"] * 1.25 ** 3 / strength)


def test_screening_factor_does_not_claim_prior_experience_or_compliance():
    import yaml
    from digitalmodel.marine_ops.fowt_quasi_static.report import BASIS, INPUT
    parser = Text()
    parser.feed(REPORT.read_text())
    criteria = ' '.join(parser.section_text['section-3'])
    assert 'based on previous experience' not in criteria
    assert 'This screening criterion is not the factor of API RP 2SK' in criteria
    assert 'UNVERIFIED' in criteria
    references = ' '.join(parser.section_text['section-9'])
    assert 'API RP 2SK' in references
    assert '3rd edition, October 2005' in references
    assert 'UNVERIFIED' in references
    for path in (BASIS, INPUT):
        analysis = yaml.safe_load((ROOT/path).read_text())['analysis']
        for state in ('intact', 'damaged'):
            source = analysis[f'{state}_tension_factor']['source']
            assert 'based on previous experience' not in source


def test_inertia_rationale_is_consistent_and_names_quantities():
    import yaml
    from digitalmodel.marine_ops.fowt_quasi_static.report import BASIS
    basis = yaml.safe_load((ROOT/BASIS).read_text())
    rationale = basis['system']['inertia_rationale']
    parser = Text()
    parser.feed(REPORT.read_text())
    for section in ('section-4', 'section-6'):
        content = ' '.join(parser.section_text[section])
        assert rationale in content
        assert 'F24:K24' not in content and 'L24:Q24' not in content
    assert 'cannot be smaller' in rationale
    assert 'adopted_origin' in basis['system']['rna_inertia_cg']


@pytest.mark.parametrize('value', [1.0, 0.99999, 1.00001])
def test_percentage_display_normalises_rounded_zero(value):
    from digitalmodel.marine_ops.fowt_quasi_static.report import delta
    assert delta(value, 1.0) == '0.0 %'
    assert delta(1.001, 1.0) == '+0.1 %'
    assert delta(0.999, 1.0) == '-0.1 %'


def test_summary_excludes_slow_drift_and_excursions_match_table_precision():
    parser = Text()
    parser.feed(REPORT.read_text())
    summary = ' '.join(parser.section_text['section-1'])
    assert 'not upper bounds' in summary
    assert 'low-frequency (slow-drift)' in summary
    assert '; The governing' not in summary
    assert 'surge natural period' in summary
    findings = ' '.join(parser.section_text['section-6'])
    assert '39.3 m' in findings
    assert '39.26 m' not in findings
    assert 'Report issues; earlier entries are retained.' not in REPORT.read_text()


def test_mooring_force_properties_use_meganewtons():
    parser = Text()
    parser.feed(REPORT.read_text())
    design = ' '.join(parser.section_text['section-3'])
    assert 'Axial Stiffness 3270 MN' in design
    assert 'Minimum Breaking Load 22.29 MN' in design


def test_report_rebuild_is_independent_of_caller_plot_settings(tmp_path):
    import matplotlib
    from digitalmodel.marine_ops.fowt_quasi_static.report import build_report
    manifest = json.loads(REPORT.with_suffix('.manifest.json').read_text())
    with matplotlib.rc_context({'font.size': 22, 'axes.facecolor': 'pink'}):
        built, _ = build_report(ROOT, manifest['results'], tmp_path)
    assert built.read_bytes() == REPORT.read_bytes()
