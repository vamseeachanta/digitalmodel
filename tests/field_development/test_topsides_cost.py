# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Invented topsides inputs test arithmetic only; no market cost basis is implied."""

from dataclasses import replace
from importlib import import_module

import pytest

from digitalmodel.citations.schema import Citation, CitationResolutionError, CitedValue


@pytest.fixture
def inputs(tmp_path, monkeypatch):
    """Create invented citations locally; no real cost data or wiki is consumed."""
    monkeypatch.setenv("LLM_WIKI_PATH", str(tmp_path))
    directory = tmp_path / "wikis" / "cost-fixture"
    directory.mkdir(parents=True)
    citations = {}
    for name in ("rate", "complexity"):
        (directory / f"{name}.md").write_text(
            f"---\ncode_id: INVENTED-{name}\npublisher: Invented fixture\n"
            "revision: test-only\n---\nInvented values, not an industry source.\n",
            encoding="utf-8",
        )
        citations[name] = Citation(
            code_id=f"INVENTED-{name}",
            publisher="Invented fixture",
            revision="test-only",
            section="Invented arithmetic fixture",
            wiki_path=f"wikis/cost-fixture/{name}.md",
            source_sibling="generic",
        )
    return {
        "weight_tonnes": 10.0,
        "usd_per_tonne": CitedValue(20.0, citations["rate"], "USD/tonne"),
        "complexity_factor": CitedValue(1.5, citations["complexity"], "1"),
    }


def estimate(inputs):
    # Import at the call boundary so absent implementation produces a red test.
    module = import_module("digitalmodel.field_development.cost_estimation")
    return module.estimate_topsides_cost(**inputs)


@pytest.mark.parametrize(
    ("weight", "rate", "factor", "expected"),
    [(10, 20, 1.5, 300), (20, 20, 1.5, 600), (10, 40, 1.5, 600), (10, 20, 3, 600)],
)
def test_weight_rate_and_complexity_scaling(inputs, weight, rate, factor, expected):
    # Invented, independently calculated totals; each input acts linearly.
    inputs["weight_tonnes"] = weight
    inputs["usd_per_tonne"] = replace(inputs["usd_per_tonne"], value=rate)
    inputs["complexity_factor"] = replace(inputs["complexity_factor"], value=factor)
    assert estimate(inputs).total_usd == pytest.approx(expected)


def test_citation_sidecar_identifies_both_cost_inputs(inputs):
    result = estimate(inputs)
    assert result.citations == {
        "usd_per_tonne": inputs["usd_per_tonne"].citation,
        "complexity_factor": inputs["complexity_factor"].citation,
    }


@pytest.mark.parametrize("name", ["usd_per_tonne", "complexity_factor"])
@pytest.mark.parametrize("missing", ["bare", "none", "record"])
def test_uncited_cost_inputs_fail_closed(inputs, name, missing):
    if missing == "bare":
        inputs[name] = 2.0  # Invented uncited value.
    elif missing == "none":
        inputs[name] = None
    else:
        inputs[name] = replace(inputs[name], citation=None)
    with pytest.raises(CitationResolutionError, match=name):
        estimate(inputs)


@pytest.mark.parametrize("name", ["usd_per_tonne", "complexity_factor"])
@pytest.mark.parametrize("defect", ["missing", "code_id", "publisher", "revision"])
def test_citations_resolve_at_each_calculation(inputs, tmp_path, name, defect):
    estimate(inputs)
    citation = inputs[name].citation
    page = tmp_path / citation.wiki_path
    if defect == "missing":
        # Retain the fixture page and point at an absent path instead of deleting it.
        inputs[name] = replace(
            inputs[name], citation=replace(citation, wiki_path="wikis/cost-fixture/absent.md")
        )
    else:
        original = page.read_text(encoding="utf-8")
        page.write_text(original.replace(f"{defect}:", "changed_field:"), encoding="utf-8")
    with pytest.raises(CitationResolutionError, match=citation.code_id):
        estimate(inputs)


@pytest.mark.parametrize(
    ("name", "units"),
    [("usd_per_tonne", "USD/kg"), ("usd_per_tonne", ""),
     ("complexity_factor", "%"), ("complexity_factor", "")],
)
def test_incompatible_units_are_rejected(inputs, name, units):
    inputs[name] = replace(inputs[name], units=units)
    with pytest.raises(ValueError, match=name):
        estimate(inputs)


@pytest.mark.parametrize("name", ["weight_tonnes", "usd_per_tonne", "complexity_factor"])
@pytest.mark.parametrize("value", [0, -1, float("nan"), float("inf"), float("-inf"), True, "2"])
def test_invalid_numeric_inputs_are_rejected(inputs, name, value):
    if name == "weight_tonnes":
        inputs[name] = value
    else:
        inputs[name] = replace(inputs[name], value=value)
    with pytest.raises(ValueError, match=name):
        estimate(inputs)


def test_overflow_is_rejected(inputs):
    inputs["weight_tonnes"] = 1e308
    with pytest.raises(ValueError, match="total_usd"):
        estimate(inputs)
