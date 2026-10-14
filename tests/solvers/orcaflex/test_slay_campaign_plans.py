# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Evidence contracts for calibration and marginal vessel/pipe decisions."""

from html.parser import HTMLParser
from pathlib import Path
import re

import pytest


CAMPAIGN = Path(__file__).resolve().parents[3] / "docs/domains/pipeline-installation/slay-campaign"


class Headings(HTMLParser):
    def __init__(self):
        super().__init__()
        self.current = False
        self.headings = []

    def handle_starttag(self, tag, attrs):
        self.current = tag == "h2"

    def handle_data(self, data):
        if self.current:
            self.headings.append(data)

    def handle_endtag(self, tag):
        if tag == "h2":
            self.current = False


@pytest.mark.parametrize("filename,terms", [
    ("calibration.html", ["effective tension", "roller reaction", "previous experience", "normalized", "tolerance", "rights"]),
    ("vessel-pipe-matrix.html", ["0cef59201f817c28d91ef25e1bfcba99bfc9d588", "CSV line", "residual", "extrapolation", "stopping rule"]),
])
def test_plan_has_revision_history_and_evidence_contract(filename, terms):
    text = (CAMPAIGN / filename).read_text()
    parser = Headings()
    parser.feed(text)
    assert parser.headings[:2] == ["Summary", "Revision history"]
    assert all(term in text for term in terms)
    assert "Not Evaluated" in text


@pytest.mark.parametrize("filename", ["analysis.html", "calibration.html", "vessel-pipe-matrix.html"])
def test_plan_uses_neutral_public_roles(filename):
    text = (CAMPAIGN / filename).read_text(encoding="utf-8").lower()
    forbidden = r"\bow[n]er\b|\bsolver\s+(?:manager|workstation)\b|\bauthorized\s+private\b"
    assert re.search(forbidden, text) is None
