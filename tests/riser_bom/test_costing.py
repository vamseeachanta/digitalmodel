# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic arithmetic only; no procurement records are used."""

from dataclasses import replace
from decimal import Decimal

import pytest

from digitalmodel.citations import Citation, CitationResolutionError
from digitalmodel.riser_bom import Configuration, CostBasis, Rate, cost_bom


CITATION = Citation(
    code_id="SYNTHETIC-BOM",
    publisher="Synthetic fixture",
    revision="1",
    section="Invented arithmetic",
    source_sibling="generic",
    wiki_path="wikis/engineering/concepts/synthetic-bom.md",
)


def cited_root(tmp_path):
    page = tmp_path / CITATION.wiki_path
    page.parent.mkdir(parents=True)
    page.write_text(
        "---\ncode_id: SYNTHETIC-BOM\npublisher: Synthetic fixture\nrevision: 1\n---\n"
        "Invented arithmetic, no real cost basis.\n",
        encoding="utf-8",
    )
    return tmp_path


def configuration(**changes):
    values = dict(
        riser_length_m="25",
        joint_length_m="10",
        coated_length_m="25",
        buoyancy_module_count=4,
        ancillary_count=2,
        installation_days="1.5",
        citation=CITATION,
    )
    return Configuration(**(values | changes))


def basis():
    # Values are consecutive small primes, deliberately invented for arithmetic.
    return CostBasis(
        currency="TEST",
        revision="1",
        data_kind="synthetic",
        rates=(
            Rate("riser_joints", "joint", "2", CITATION),
            Rate("coating", "m", "3", CITATION),
            Rate("buoyancy_modules", "module", "5", CITATION),
            Rate("ancillaries", "item", "7", CITATION),
            Rate("installation_spread", "day", "11", CITATION),
        ),
    )


def test_configuration_rolls_up_all_five_categories(tmp_path):
    """Hand calculation: ceil(25/10)*2 + 25*3 + 4*5 + 2*7 + 1.5*11.

    Three whole joints cost 6; coating 75; buoyancy 20; ancillaries 14;
    installation 16.5. Addition gives 131.5 TEST units. All inputs invented.
    """
    result = cost_bom(configuration(), basis(), citation_root=cited_root(tmp_path))
    assert result["total"] == "131.5"
    assert result["currency"] == "TEST"
    assert result["data_kind"] == "synthetic"
    assert [row["subtotal"] for row in result["breakdown"]] == [
        "6",
        "75",
        "20",
        "14",
        "16.5",
    ]
    assert Decimal(result["breakdown"][0]["quantity"]) == 3
    assert len(result["citations"]) == 6


@pytest.mark.parametrize(
    "field,value",
    [
        ("riser_length_m", "0"),
        ("riser_length_m", "-1"),
        ("riser_length_m", "NaN"),
        ("riser_length_m", "Infinity"),
        ("joint_length_m", "0"),
        ("joint_length_m", "-1"),
        ("coated_length_m", "26"),
        ("coated_length_m", "-1"),
        ("buoyancy_module_count", "1.5"),
        ("buoyancy_module_count", "-1"),
        ("ancillary_count", "0.5"),
        ("installation_days", "-1"),
        ("installation_days", True),
        ("installation_days", None),
    ],
)
def test_invalid_quantity_fails_closed(tmp_path, field, value):
    with pytest.raises(ValueError):
        cost_bom(
            configuration(**{field: value}), basis(), citation_root=cited_root(tmp_path)
        )


def test_missing_quantity_citation_fails_closed(tmp_path):
    with pytest.raises(ValueError, match="Citation"):
        cost_bom(
            configuration(citation=None), basis(), citation_root=cited_root(tmp_path)
        )


def test_missing_page_fails_at_calculation(tmp_path):
    with pytest.raises(CitationResolutionError, match="page_missing"):
        cost_bom(configuration(), basis(), citation_root=tmp_path)


def test_mismatched_citation_revision_fails_at_calculation(tmp_path):
    root = cited_root(tmp_path)
    changed = replace(CITATION, revision="other")
    with pytest.raises(CitationResolutionError, match="frontmatter_mismatch:revision"):
        cost_bom(configuration(citation=changed), basis(), citation_root=root)


def test_zero_quantity_does_not_hide_uncited_rate(tmp_path):
    rates = list(basis().rates)
    rates[2] = replace(rates[2], citation=None)
    with pytest.raises(ValueError, match="Citation"):
        cost_bom(
            configuration(buoyancy_module_count=0),
            replace(basis(), rates=tuple(rates)),
            citation_root=cited_root(tmp_path),
        )


@pytest.mark.parametrize(
    "change", ["missing", "duplicate", "unknown", "wrong_unit", "negative", "nan"]
)
def test_invalid_rate_schedule_rejected(tmp_path, change):
    rates = list(basis().rates)
    if change == "missing":
        rates.pop()
    elif change == "duplicate":
        rates.append(rates[0])
    elif change == "unknown":
        rates[0] = replace(rates[0], component="unknown")
    elif change == "wrong_unit":
        rates[0] = replace(rates[0], unit="m")
    else:
        rates[0] = replace(rates[0], value="-1" if change == "negative" else "NaN")
    with pytest.raises(ValueError):
        cost_bom(
            configuration(),
            replace(basis(), rates=tuple(rates)),
            citation_root=cited_root(tmp_path),
        )


def test_configuration_change_has_visible_buoyancy_cost_consequence(tmp_path):
    """Hand difference: (6 - 4) modules * 5 TEST/module = 10 TEST."""
    root = cited_root(tmp_path)
    original = cost_bom(configuration(), basis(), citation_root=root)
    changed = cost_bom(
        configuration(buoyancy_module_count=6), basis(), citation_root=root
    )
    assert Decimal(changed["total"]) - Decimal(original["total"]) == 10
    assert changed["breakdown"][0:2] == original["breakdown"][0:2]
    assert changed["breakdown"][3:] == original["breakdown"][3:]


def test_exact_joint_multiple_does_not_purchase_extra_joint(tmp_path):
    """Hand sum: 2*2 + 20*3 + 4*5 + 2*7 + 1.5*11 = 114.5."""
    result = cost_bom(
        configuration(riser_length_m="20", coated_length_m="20"),
        basis(),
        citation_root=cited_root(tmp_path),
    )
    assert result["total"] == "114.5"


def test_decimal_arithmetic_is_independent_of_callers_precision(tmp_path):
    """Hand product: 1.5 days * 0.1 TEST/day = 0.15; total = 115.15."""
    from decimal import localcontext

    rates = list(basis().rates)
    rates[-1] = replace(rates[-1], value="0.1")
    with localcontext() as context:
        context.prec = 2
        result = cost_bom(
            configuration(),
            replace(basis(), rates=tuple(rates)),
            citation_root=cited_root(tmp_path),
        )
    assert result["total"] == "115.15"


def test_fixture_values_have_only_invented_provenance():
    """The public schedule is the first five primes, in TEST currency only.

    This checks provenance and the exact artificial schedule, not an impossible
    numerical comparison against undisclosed private procurement records.
    """
    fixture = basis()
    assert fixture.data_kind == "synthetic"
    assert fixture.currency == "TEST"
    assert [rate.value for rate in fixture.rates] == ["2", "3", "5", "7", "11"]
    assert all(rate.citation.code_id == "SYNTHETIC-BOM" for rate in fixture.rates)


def test_joint_ceiling_retains_remainder_at_large_magnitude(tmp_path):
    """For L=10**60+1 and joint length=10**60, two joints are required."""
    length = str(10**60 + 1)
    result = cost_bom(
        configuration(riser_length_m=length, joint_length_m=str(10**60)),
        basis(),
        citation_root=cited_root(tmp_path),
    )
    assert result["breakdown"][0]["quantity"] == "2"
