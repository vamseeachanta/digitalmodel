"""Presentation regressions for the pore-pressure explorer.

The disclosure criteria come from issue 2706 and Introduction to Energy
Geomechanics (2024), section 6.5, https://dnicolasespinoza.github.io/node48.html:
the mud window needs mechanical bounds; vertical stress alone is insufficient.
No numerical method or acceptance limit is introduced by this presentation slice.
"""

import json
import re
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import urljoin, urlsplit

import pytest

from scripts.capabilities import build_pore_pressure_explorer as explorer


class PageText(HTMLParser):
    def __init__(self):
        super().__init__()
        self.parts = []
        self.links = []
        self.hidden = False

    def handle_starttag(self, tag, attrs):
        if tag in {"script", "style"}:
            self.hidden = True
        if tag == "a":
            self.links.append(dict(attrs).get("href", ""))

    def handle_endtag(self, tag):
        if tag in {"script", "style"}:
            self.hidden = False

    def handle_data(self, data):
        if not self.hidden:
            self.parts.append(data)


@pytest.fixture
def generated(tmp_path, monkeypatch):
    monkeypatch.setattr(explorer, "_OUT_DIR", tmp_path)
    monkeypatch.setattr(explorer, "_REPO", tmp_path)
    explorer.main()
    html = (tmp_path / "pore-pressure-explorer.html").read_text()
    data = json.loads((tmp_path / "pore-pressure-explorer.json").read_text())
    return html, data


def test_no_safety_or_overburden_ceiling_claim(generated):
    html, _ = generated
    page = PageText()
    page.feed(html)
    text = " ".join(page.parts).lower()
    assert re.search(r"\bsafe\b", text) is None
    assert "overburden ceiling" not in text
    assert "minimum" not in text
    assert "pore + assumed margin" in text


def test_no_window_band_or_dynamic_overburden_fill(generated):
    html, _ = generated
    assert 'id="win"' not in html
    assert "win.style" not in html
    assert ".win{" not in html
    assert "linear-gradient" not in html


def test_missing_fracture_basis_is_explicit(generated):
    html, data = generated
    assert "NOT_EVALUATED" in html
    assert "Fracture gradient is not supplied or calculated" in html
    assert "Overburden is vertical stress, not a loss bound" in html
    assert data["meta"]["drilling_window"]["status"] == "NOT_EVALUATED"
    assert data["meta"]["drilling_window"]["ceiling_basis"] == "not supplied or calculated"


@pytest.mark.parametrize("exclusion", [
    "collapse (shear failure)",
    "weak bedding planes",
    "natural fractures",
    "chemical or time-dependent effects",
])
def test_exclusions_are_visible_and_machine_readable(generated, exclusion):
    html, data = generated
    page = PageText()
    page.feed(html)
    assert exclusion in " ".join(page.parts)
    assert exclusion in data["meta"]["drilling_window"]["not_covered"]


def test_assumptions_and_source_are_visible(generated):
    html, _ = generated
    assert "assumed, based on previous experience" in html
    assert 'href="https://dnicolasespinoza.github.io/node48.html"' in html


def test_stability_link_resolves_to_published_workflow(generated):
    html, _ = generated
    page = PageText()
    page.feed(html)
    href = next(link for link in page.links if "wellbore-stability" in link)
    # Mirror the MkDocs page route under a site prefix, without network access.
    published = urljoin(
        "https://docs.example.org/reference/structural/pore-pressure-explorer.html",
        href,
    )
    assert urlsplit(published).path == "/reference/structural/wellbore-stability/"
    repo = Path(__file__).resolve().parents[2]
    published_source = repo / "docs/api/structural/wellbore-stability.md"
    workflow_source = repo / "examples/workflows/wellbore-stability/README.md"
    assert published_source.read_bytes() == workflow_source.read_bytes()


def test_margin_basis_is_machine_readable(generated):
    _, data = generated
    assert data["meta"]["kick_margin_basis"] == "assumed, not sourced"


@pytest.mark.parametrize("heading", [
    "<title>Pore-pressure explorer — digitalmodel</title>",
    "<h1>Pore pressure explorer</h1>",
])
def test_headings_describe_pore_pressure_only(generated, heading):
    html, _ = generated
    assert heading in html


def test_embedded_data_equals_json_and_committed_page(generated):
    html, data = generated
    embedded = html.split("const DATA = ", 1)[1].split(";\nconst M", 1)[0]
    assert json.loads(embedded) == data
    committed = Path(__file__).resolve().parents[2] / "docs/api/structural"
    assert html == (committed / "pore-pressure-explorer.html").read_text()
    assert data == json.loads((committed / "pore-pressure-explorer.json").read_text())
