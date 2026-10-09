"""Shared fixtures for the riser stack-up drawing tests (#2152)."""

from __future__ import annotations

from pathlib import Path

import pytest

FIXTURES = Path(__file__).resolve().parent / "fixtures"
SYNTHETIC_SPEC = FIXTURES / "synthetic_spec.json"
GOLDEN_SVG = FIXTURES / "synthetic.svg"


def row_span(svg: str, cid: str) -> tuple[int, int]:
    """Line span [start, end] of the table row of component ``cid`` (#2158)."""
    lines = svg.split("\n")
    head = f'<g data-role="table-row" data-component-id="{cid}"'
    start = next(i for i, ln in enumerate(lines) if ln.startswith(head))
    end = next(i for i in range(start, len(lines)) if lines[i] == "</g>")
    return start, end


def row_cell(svg: str, cid: str, col: str) -> str:
    """The value cell line (not a marker text) of column ``col`` in row ``cid``."""
    lines = svg.split("\n")
    start, end = row_span(svg, cid)
    hits = [
        ln
        for ln in lines[start:end]
        if f'data-col="{col}"' in ln and 'class="mk"' not in ln
    ]
    assert len(hits) == 1, (cid, col, hits)
    return hits[0]


@pytest.fixture()
def spec_text() -> str:
    """Raw JSON text of the synthetic (non-project) fixture spec."""
    return SYNTHETIC_SPEC.read_text(encoding="utf-8")


@pytest.fixture()
def spec(spec_text):
    from digitalmodel.drilling_riser.stackup_drawing import from_json

    return from_json(spec_text)


@pytest.fixture()
def svg(spec) -> str:
    from digitalmodel.drilling_riser.stackup_drawing import render

    return render(spec)
