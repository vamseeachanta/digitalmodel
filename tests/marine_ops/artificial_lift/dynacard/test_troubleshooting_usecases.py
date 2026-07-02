# ABOUTME: Drift guard for the published dynacard troubleshooting use cases.
# ABOUTME: Re-runs every frozen case against the live classifier and P1 calcs.
"""The capabilities page publishes seven troubleshooting use cases frozen by
``scripts/capabilities/build_dynacard_troubleshooting.py`` into
``docs/api/artificial-lift/dynacard-troubleshooting.json``. These tests re-run
each frozen case against the live generator + classifier so a retrained model
or changed generator cannot silently invalidate the published page."""

import json
from pathlib import Path

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    ALL_GENERATORS,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
    PumpDiagnostics,
)

_REPO = Path(__file__).resolve().parents[4]
_FROZEN = _REPO / "docs" / "api" / "artificial-lift" / "dynacard-troubleshooting.json"


def _frozen_cases() -> list[dict]:
    return json.loads(_FROZEN.read_text(encoding="utf-8"))["cases"]


def test_frozen_artifact_exists_and_has_cases():
    cases = _frozen_cases()
    assert len(cases) >= 5
    assert any(case["mode"] == "NORMAL" for case in cases)


@pytest.mark.parametrize("case", _frozen_cases(), ids=lambda case: case["mode"])
def test_published_diagnosis_matches_live_classifier(case):
    """Each published card must still classify to its published diagnosis."""
    card = ALL_GENERATORS[case["mode"]](seed=case["seed"])
    live = PumpDiagnostics.classify_card(card)
    assert live == case["classification"], (
        f"{case['mode']} (seed {case['seed']}) now classifies as {live}, but the "
        f"published page says {case['classification']}. Rebuild the page with "
        "scripts/capabilities/build_dynacard_troubleshooting.py and re-review."
    )


@pytest.mark.parametrize("case", _frozen_cases(), ids=lambda case: case["mode"])
def test_published_diagnosis_is_honest(case):
    """The page only publishes cases whose diagnosis equals the generated mode."""
    assert case["classification"] == case["mode"]


def test_html_page_embeds_all_cases():
    html = (_FROZEN.parent / "dynacard-troubleshooting.html").read_text(encoding="utf-8")
    for case in _frozen_cases():
        assert case["mode"] in html
