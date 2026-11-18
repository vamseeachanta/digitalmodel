# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Data-preservation checks for the documented table-width example.

These tests inspect emitted content and CSS contracts, not browser geometry.
The expected strings are literal synthetic inputs; preserving each input cell
is an independent round-trip oracle, without an engineering calculation.
"""

from html.parser import HTMLParser
from pathlib import Path
import re
import runpy

import pytest


EXAMPLE = (
    Path(__file__).resolve().parents[2] / "examples/reporting/design_data_layout.py"
)


class TableRows(HTMLParser):
    """Read table cell text, decoding HTML entities as a reader would see them."""

    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.rows = []
        self._row = []
        self._cell = None

    def handle_starttag(self, tag, attrs):
        if tag == "tr":
            self._row = []
        elif tag == "td":
            self._cell = []

    def handle_data(self, data):
        if self._cell is not None:
            self._cell.append(data)

    def handle_endtag(self, tag):
        if tag == "td":
            self._row.append("".join(self._cell))
            self._cell = None
        elif tag == "tr":
            self.rows.append(tuple(self._row))


@pytest.fixture
def example():
    return runpy.run_path(str(EXAMPLE))


@pytest.mark.parametrize("slot", ["design_data", "assumptions", "results"])
def test_example_preserves_long_values_in_each_table_context(example, slot):
    """Expected cells equal the supplied text, including punctuation and units.

    The long token has no spaces to wrap at. The prose and markup-like text
    exercise separate break opportunities and HTML entity decoding. No pixel
    width or clipping conclusion is inferred from the round trip.
    """
    from digitalmodel.reporting import DataRow, DesignDataGroup

    inputs = [
        (
            "Long provenance",
            "Synthetic input supplied for the full layout review "
            "with its complete provenance retained through the final word.",
            "",
        ),
        ("Unbroken identifier", "ABCDEFGHIJKLMNOPQRSTUVWXYZ_" * 8, ""),
        ("Stress symbols & units", 'Range < lower bound & note "retained">', "unit"),
        ("Long label " + "ABCDEFGHIJKLMNOPQRSTUVWXYZ_" * 8, "Tail retained", ""),
        ("Empty optional value", "", ""),
    ]
    # Eight repetitions are synthetic stress data, not an acceptance limit.
    group = DesignDataGroup(
        caption="Synthetic stress inputs",
        rows=[
            DataRow(label=label, value=value, unit=unit)
            for label, value, unit in inputs
        ],
    )
    report = example["build_report"]()
    if slot == "results":
        report.results[0].groups = [group]
    else:
        setattr(report, slot, [group])
    parser = TableRows()
    parser.feed(report.render_html())
    for label, value, unit in inputs:
        expected = (label, value + (f" {unit}" if unit else ""))
        assert parser.rows.count(expected) == 1


def test_example_writes_complete_offline_html_to_requested_directory(example, tmp_path):
    destination = tmp_path / "review" / "design-data.html"
    example["main"](["--output", str(destination)])
    emitted = destination.read_text(encoding="utf-8")
    report = example["build_report"]()
    assert report.completeness().complete
    assert emitted == report.render_html()
    assert "<script src=" not in emitted
    assert 'name="viewport"' in emitted
    for group in report.design_data:
        assert group.render() in emitted
    assert "NOT_EVALUATED" in emitted


def test_example_keeps_the_inherited_floor_and_scroll_contract(example):
    """Source: calc_report_format.html, table and .kv rules; PR #2443 (2026).

    The 440px minimum is an existing presentation policy, not a sourced
    readability threshold. The single-column track, wrapping and scroll
    container preserve access below that floor; geometry needs browser review.
    """
    emitted = example["build_report"]().render_html()
    styles = "\n".join(re.findall(r"<style[^>]*>(.*?)</style>", emitted, re.S))
    css = re.sub(r"/\*.*?\*/", "", styles, flags=re.S)
    assert re.search(r"\btable\{[^}]*min-width:440px", css)
    assert not re.search(r"\.kv\s+table\s*\{[^}]*min-width", css)
    container = re.search(r"\.kv\{([^}]*)\}", css).group(1)
    assert "overflow-x:auto" in container
    assert "overflow:hidden" not in container
    assert "text-overflow:ellipsis" not in css
    assert "table-layout: auto !important" in css
    assert "grid-template-columns:1fr" in re.search(
        r"\.datagrid\{([^}]*)\}", css
    ).group(1)
