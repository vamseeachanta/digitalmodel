# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Invented topsides inputs test arithmetic only; no market cost basis is implied."""

from dataclasses import asdict, replace
from datetime import date
from decimal import Decimal, Inexact, ROUND_UP, localcontext
from importlib import import_module

import numpy as np
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
            newline="\n",
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
        "price_base_year": 2024,
        "usd_per_tonne": CitedValue(20.0, citations["rate"], "USD/tonne"),
        "complexity_factor": CitedValue(1.5, citations["complexity"], "1"),
    }


def estimate(inputs):
    # Import at the call boundary so absent implementation produces a red test.
    module = import_module("digitalmodel.field_development.cost_estimation")
    return module.estimate_topsides_cost(**inputs)


def test_price_base_year_is_required(inputs):
    inputs.pop("price_base_year")
    with pytest.raises(TypeError, match="price_base_year"):
        estimate(inputs)


@pytest.mark.parametrize(
    "year", [1, 2014, 2024, pytest.param(np.int64(2024), id="integral-scalar")]
)
def test_price_base_year_is_retained(inputs, year):
    inputs["price_base_year"] = year
    result = estimate(inputs)
    assert result.price_base_year == year
    assert type(result.price_base_year) is int
    assert asdict(result)["price_base_year"] == year
    assert result.total_usd == Decimal("300.00")


@pytest.mark.parametrize(
    "year",
    [None, True, False, 0, -1, 9999, 10000, date.today().year + 1,
     2024.0, 2024.5, "2024",
     float("nan"), float("inf"), Decimal("2024")],
)
def test_invalid_price_base_year_is_rejected(inputs, year):
    inputs["price_base_year"] = year
    with pytest.raises(ValueError, match="price_base_year"):
        estimate(inputs)


def test_price_base_year_uses_calculation_date(inputs):
    inputs["today"] = date(2026, 1, 1)
    inputs["price_base_year"] = 2026
    assert estimate(inputs).price_base_year == 2026
    inputs["price_base_year"] = 2027
    with pytest.raises(ValueError, match="price_base_year"):
        estimate(inputs)
    inputs["today"] = date(2027, 1, 1)
    assert estimate(inputs).price_base_year == 2027


def test_price_base_year_defaults_to_current_date(inputs, monkeypatch):
    module = import_module("digitalmodel.field_development.cost_estimation")

    class FixedDate(date):
        @classmethod
        def today(cls):
            return cls(2026, 12, 31)

    monkeypatch.setattr(module, "date", FixedDate)
    inputs["price_base_year"] = 2026
    assert estimate(inputs).price_base_year == 2026
    inputs["price_base_year"] = 2027
    with pytest.raises(ValueError, match="price_base_year"):
        estimate(inputs)


@pytest.mark.parametrize(
    ("weight", "rate", "factor", "expected"),
    [(10, 20, 1.5, 300), (20, 20, 1.5, 600), (10, 40, 1.5, 600), (10, 20, 3, 600),
     (12.5, 32.4, 1.2, 486.0), (7.25, 19.6, 1.1, 156.31),
     (0.125, 8.4, 2.5, 2.62), (0.1, 0.2, 3, 0.06), (0.7, 0.1, 3, 0.21)],
)
def test_weight_rate_and_complexity_scaling(inputs, weight, rate, factor, expected):
    # Invented, independently calculated totals; each input acts linearly.
    # 12.5 * 32.4 = 405; 405 * 1.2 = 486.0.
    # 7.25 * 19.6 = 142.1; 142.1 * 1.1 = 156.31.
    # 0.125 * 8.4 = 1.05; 1.05 * 2.5 = 2.625, half-even to cents = 2.62.
    inputs["weight_tonnes"] = weight
    inputs["usd_per_tonne"] = replace(inputs["usd_per_tonne"], value=rate)
    inputs["complexity_factor"] = replace(inputs["complexity_factor"], value=factor)
    result = estimate(inputs)
    assert isinstance(result.total_usd, Decimal)
    assert result.total_usd == Decimal(str(expected))
    assert result.total_usd.as_tuple().exponent == -2
    assert result.weight_tonnes == Decimal(str(weight))
    assert result.usd_per_tonne is inputs["usd_per_tonne"]
    assert result.complexity_factor is inputs["complexity_factor"]


@pytest.mark.parametrize("units", ["1", "dimensionless"])
def test_dimensionless_complexity_units_are_accepted(inputs, units):
    inputs["complexity_factor"] = replace(inputs["complexity_factor"], units=units)
    result = estimate(inputs)
    assert result.total_usd == Decimal("300.00")
    assert result.complexity_factor is inputs["complexity_factor"]
    assert result.complexity_factor.units == units


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
        page.write_text(
            original.replace(f"{defect}:", "changed_field:"),
            encoding="utf-8",
            newline="\n",
        )
    with pytest.raises(CitationResolutionError, match=citation.code_id):
        estimate(inputs)


@pytest.mark.parametrize(
    ("name", "units"),
    [("usd_per_tonne", "USD/kg"), ("usd_per_tonne", ""),
     ("complexity_factor", "%"), ("complexity_factor", ""),
     ("complexity_factor", "Dimensionless"), ("complexity_factor", " 1"),
     ("complexity_factor", "1 "), ("complexity_factor", "-")],
)
def test_incompatible_units_are_rejected(inputs, name, units):
    inputs[name] = replace(inputs[name], units=units)
    with pytest.raises(ValueError, match=name):
        estimate(inputs)


@pytest.mark.parametrize("name", ["weight_tonnes", "usd_per_tonne", "complexity_factor"])
@pytest.mark.parametrize(
    "value", [0, -1, float("nan"), float("inf"), float("-inf"), True,
              np.bool_(True), "2", Decimal("NaN"), Decimal("sNaN"),
              Decimal("Infinity"), Decimal("-Infinity"), Decimal("0"), Decimal("-1")]
)
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


def test_total_rounding_to_zero_is_rejected(inputs):
    inputs["weight_tonnes"] = 1e-200
    inputs["usd_per_tonne"] = replace(inputs["usd_per_tonne"], value=1e-200)
    with pytest.raises(ValueError, match="total_usd"):
        estimate(inputs)


@pytest.mark.parametrize("scalar", [Decimal, np.float64, np.int64])
def test_numeric_scalars_are_retained_without_float_conversion(inputs, scalar):
    inputs["weight_tonnes"] = scalar(10)
    inputs["usd_per_tonne"] = replace(inputs["usd_per_tonne"], value=scalar(20))
    inputs["complexity_factor"] = replace(inputs["complexity_factor"], value=scalar(3))
    result = estimate(inputs)
    assert result.total_usd == Decimal("600.00")
    assert isinstance(result.weight_tonnes, Decimal)
    assert result.weight_tonnes == Decimal("10")
    assert result.usd_per_tonne is inputs["usd_per_tonne"]
    assert result.complexity_factor is inputs["complexity_factor"]


@pytest.mark.parametrize(
    ("weight", "expected"),
    [("1.005", "1.00"), ("1.015", "1.02"),
     ("1.004999999999999999999999999999999999", "1.00"),
     ("1.005000000000000000000000000000000001", "1.01")],
)
def test_decimal_product_rounds_once_to_cents(inputs, weight, expected):
    # A low-precision caller context must not round inputs or the product early.
    inputs["weight_tonnes"] = Decimal(weight)
    inputs["usd_per_tonne"] = replace(inputs["usd_per_tonne"], value=Decimal("0.1"))
    inputs["complexity_factor"] = replace(inputs["complexity_factor"], value=Decimal("10"))
    with localcontext() as context:
        context.prec = 2
        context.rounding = ROUND_UP
        context.traps[Inexact] = True
        result = estimate(inputs)
    assert result.total_usd == Decimal(expected)
    assert result.total_usd.as_tuple().exponent == -2
    assert result.weight_tonnes == Decimal(weight)


def test_large_decimal_product_preserves_all_digits(inputs):
    inputs["weight_tonnes"] = Decimal("123456789012345678901234567890.125")
    inputs["usd_per_tonne"] = replace(inputs["usd_per_tonne"], value=Decimal("8.4"))
    inputs["complexity_factor"] = replace(inputs["complexity_factor"], value=Decimal("2.5"))
    result = estimate(inputs)
    assert result.total_usd == Decimal("2592592569259259256925925925692.62")


def test_decimal_total_rounding_to_zero_is_rejected(inputs):
    inputs["weight_tonnes"] = Decimal("0.0049")
    inputs["usd_per_tonne"] = replace(inputs["usd_per_tonne"], value=Decimal("1"))
    inputs["complexity_factor"] = replace(inputs["complexity_factor"], value=Decimal("1"))
    with pytest.raises(ValueError, match="total_usd"):
        estimate(inputs)
