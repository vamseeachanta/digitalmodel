"""Evidence-gap contracts from #2608 and the pending-band finding in #2625.

These are reporting tests, with no calculated engineering expected values.
The issue requires all pending rows to remain NOT_EVALUATED until published
references exist; the follow-up identifies the proposed bands as unsourced.
"""

import json
import re
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
    # Issue numbers identify the gap; numerical engineering bands remain absent.
    assert re.search(r"\d", re.sub(r"#\d+", "", evidence)) is None


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
