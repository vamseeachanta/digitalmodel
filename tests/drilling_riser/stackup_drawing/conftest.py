"""Shared fixtures for the riser stack-up drawing tests (#2152)."""

from __future__ import annotations

from pathlib import Path

import pytest

FIXTURES = Path(__file__).resolve().parent / "fixtures"
SYNTHETIC_SPEC = FIXTURES / "synthetic_spec.json"
GOLDEN_SVG = FIXTURES / "synthetic.svg"


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
