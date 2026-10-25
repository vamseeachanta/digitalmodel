"""Evidence-gap contracts from #2608 and the pending-band finding in #2625.

These are reporting tests, with no calculated engineering expected values.
The issue requires all pending rows to remain NOT_EVALUATED until published
references exist; the follow-up identifies the proposed bands as unsourced.
"""

import hashlib
import json
import re
import sys
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

CAMPAIGN = (
    Path(__file__).resolve().parents[3]
    / "docs/domains/pipeline-installation/slay-campaign"
)
PENDING = {
    "Touchdown x": ("touchdown_x", ("coordinate", "reference frame")),
    "Sag-bend strain": ("sag_bend_strain", ("strain measure", "bending stiffness")),
    "Stiff closure": ("stiff_closure", ("length closure", "end conditions")),
}


def test_preparation_binds_committed_case_table():
    preparation = json.loads((CAMPAIGN / "preparation.json").read_text())
    assert (
        preparation["case_table_sha256"]
        == hashlib.sha256((CAMPAIGN / "case-table.yml").read_bytes()).hexdigest()
    )


@pytest.mark.parametrize("addition", ["edition 2021", "clause or table 4.2"])
def test_pending_evidence_allows_reference_numbers(tmp_path, monkeypatch, addition):
    report = (CAMPAIGN / "analysis.html").read_text(encoding="utf-8")
    report = report.replace(
        "published reference pending,", f"{addition}; published reference pending,"
    )
    (tmp_path / "analysis.html").write_text(report, encoding="utf-8")
    (tmp_path / "criteria.json").write_bytes((CAMPAIGN / "criteria.json").read_bytes())
    monkeypatch.setattr(sys.modules[__name__], "CAMPAIGN", tmp_path)
    for label in PENDING:
        test_pending_report_rows_require_specific_published_evidence(label)


@pytest.mark.parametrize(
    "band",
    [
        "10 m",
        "20 %",
        "0-10 m",
        "5.0 metres",
        "3 degrees",
        "20 percent",
        "20 per cent",
        "10 mm",
        "3 deg",
        "3°",
        "2 ft",
    ],
)
def test_pending_evidence_rejects_numerical_bands(tmp_path, monkeypatch, band):
    report = (CAMPAIGN / "analysis.html").read_text(encoding="utf-8")
    report = report.replace(
        "published reference pending,", f"{band}; published reference pending,"
    )
    (tmp_path / "analysis.html").write_text(report, encoding="utf-8")
    (tmp_path / "criteria.json").write_bytes((CAMPAIGN / "criteria.json").read_bytes())
    monkeypatch.setattr(sys.modules[__name__], "CAMPAIGN", tmp_path)
    for label in PENDING:
        with pytest.raises(AssertionError):
            test_pending_report_rows_require_specific_published_evidence(label)


class TableRows(HTMLParser):
    """Collect rendered cells so assertions apply to the corresponding row."""

    def __init__(self):
        super().__init__()
        self.rows = []
        self.row = []
        self.cell = None

    def handle_starttag(self, tag, attrs):
        if tag == "tr":
            self.row = []
        elif tag == "td":
            self.cell = []

    def handle_data(self, data):
        if self.cell is not None:
            self.cell.append(data)

    def handle_endtag(self, tag):
        if tag == "td" and self.cell is not None:
            self.row.append(" ".join("".join(self.cell).split()))
            self.cell = None
        elif tag == "tr" and self.row:
            self.rows.append(self.row)


@pytest.mark.parametrize("label", PENDING)
def test_pending_report_rows_require_specific_published_evidence(label):
    """#2608 supplies the status; #2625 forbids treating proposed bands as limits."""
    parser = TableRows()
    parser.feed((CAMPAIGN / "analysis.html").read_text(encoding="utf-8"))
    (row,) = [row for row in parser.rows if row[0] == label]
    name, required_terms = PENDING[label]
    criteria = json.loads((CAMPAIGN / "criteria.json").read_text(encoding="utf-8"))
    assert row[1] == criteria["static_qa"][name]["status"] == "NOT_EVALUATED"
    evidence = " ".join(row[2:]).lower()
    assert "published reference pending, #2608" in evidence
    for term in (*required_terms, "edition", "clause or table", "comparison tolerance"):
        assert term in evidence
    # Reference years and clause numbers are allowed; unit-bearing bands are not.
    assert (
        re.search(
            r"\d+(?:\.\d+)?\s*(?:%|°|"
            r"(?:percent|per cent|mm|m|metres?|meters?|deg(?:rees?)?|ft)\b)",
            evidence,
        )
        is None
    )


def test_pending_prose_has_no_unsourced_comparison_bands():
    """#2625 identifies these literal future-band claims in both published views."""
    table = yaml.safe_load((CAMPAIGN / "case-table.yml").read_text(encoding="utf-8"))
    decision = table["static_equilibrium_screen"]["decision"]
    report = (CAMPAIGN / "analysis.html").read_text(encoding="utf-8")
    for text in (decision, report):
        assert re.search(r"future (?:comparison )?bands", text, re.IGNORECASE) is None
        assert re.search(r"touchdown x within\s+\d", text, re.IGNORECASE) is None
        assert re.search(r"sag-bend strain within\s+\d", text, re.IGNORECASE) is None
        assert (
            re.search(r"stiff excess (?:between|from)\s+\d", text, re.IGNORECASE)
            is None
        )
        assert "No comparison tolerance is established for these rows" in text
