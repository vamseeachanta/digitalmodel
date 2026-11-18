# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Selection, validation and life targets for design fatigue factors."""

import sys
from dataclasses import replace

import pandas as pd
import pytest
from pylife.materiallaws.woehlercurve import WoehlerCurve

from digitalmodel.citations import Citation, CitationResolutionError

from digitalmodel.fatigue import (
    DFFCategory,
    required_calculated_life,
    select_design_fatigue_factor,
)
from digitalmodel.fatigue.damage import design_life_check, miner_damage
from digitalmodel.fatigue.sn_curves import get_sn_curve


@pytest.fixture
def factor_basis(tmp_path, monkeypatch):
    """Resolve a synthetic source; no engineering factor is qualified here."""
    wiki_path = "wikis/engineering/wiki/concepts/synthetic-fatigue.md"
    page = tmp_path / wiki_path
    page.parent.mkdir(parents=True)
    page.write_text(
        "---\ncode_id: SYNTHETIC\npublisher: Synthetic\nrevision: 1\n---\n"
        "Synthetic arithmetic inputs only; no governing-code applicability.\n",
        encoding="utf-8",
        newline="\n",
    )
    monkeypatch.setenv("LLM_WIKI_PATH", str(tmp_path))
    return Citation(
        code_id="SYNTHETIC",
        publisher="Synthetic",
        revision="1",
        section="Synthetic arithmetic inputs",
        wiki_path=wiki_path,
        source_sibling="generic",
    )


@pytest.mark.parametrize(
    "category, factor, years",
    [
        (DFFCategory.ACCESSIBLE_NON_CRITICAL, 1.25, 31.25),
        (DFFCategory.ACCESSIBLE_CRITICAL, 1.5, 37.5),
        (DFFCategory.NOT_ACCESSIBLE, 2.5, 62.5),
        (DFFCategory.SUBSTANTIAL_CONSEQUENCE, 4.0, 100.0),
    ],
)
def test_explicit_cited_factors(category, factor, years, factor_basis):
    # Synthetic targets: 25 * 1.25 = 31.25; * 1.5 = 37.5;
    # * 2.5 = 62.5; * 4 = 100 years, independent of any code table.
    factors = {category: factor}
    citations = {category: factor_basis}
    assert select_design_fatigue_factor(
        category, factors=factors, citations=citations
    ) == factor
    assert required_calculated_life(25.0, factor) == years
    assert select_design_fatigue_factor(
        category.value, factors=factors, citations=citations
    ) == factor


@pytest.mark.parametrize("category", list(DFFCategory))
@pytest.mark.parametrize("factors", [None, {}])
def test_missing_selected_factor_is_rejected(category, factors, factor_basis):
    with pytest.raises(ValueError, match="explicit.*factor"):
        select_design_fatigue_factor(
            category, factors=factors, citations={category: factor_basis}
        )


@pytest.mark.parametrize("category", list(DFFCategory))
def test_factor_for_another_category_is_rejected(category, factor_basis):
    other = (
        DFFCategory.ACCESSIBLE_CRITICAL
        if category == DFFCategory.NOT_ACCESSIBLE
        else DFFCategory.NOT_ACCESSIBLE
    )
    with pytest.raises(ValueError, match="explicit.*factor"):
        select_design_fatigue_factor(
            category, factors={other: 4.0}, citations={category: factor_basis}
        )


def test_omitted_factors_are_rejected():
    with pytest.raises(ValueError, match="explicit.*factor"):
        select_design_fatigue_factor(DFFCategory.NOT_ACCESSIBLE)


def test_selection_preserves_input(factor_basis):
    factors = {DFFCategory.NOT_ACCESSIBLE: 4.0}
    citations = {DFFCategory.NOT_ACCESSIBLE: factor_basis}
    factor = select_design_fatigue_factor(
        DFFCategory.NOT_ACCESSIBLE, factors=factors, citations=citations
    )
    assert factor == 4.0
    assert required_calculated_life(25.0, factor) == 100.0
    assert factors == {DFFCategory.NOT_ACCESSIBLE: 4.0}
    assert citations == {DFFCategory.NOT_ACCESSIBLE: factor_basis}


@pytest.mark.parametrize("basis", [None, "unverified", 3.0])
def test_missing_or_invalid_citation_is_rejected(basis):
    category = DFFCategory.NOT_ACCESSIBLE
    with pytest.raises(ValueError, match="citation"):
        select_design_fatigue_factor(
            category, factors={category: 3.0}, citations={category: basis}
        )


@pytest.mark.parametrize("citations", [None, {}])
def test_missing_citation_mapping_is_rejected(citations):
    category = DFFCategory.NOT_ACCESSIBLE
    with pytest.raises(ValueError, match="citation"):
        select_design_fatigue_factor(
            category, factors={category: 3.0}, citations=citations
        )


def test_citation_for_another_category_is_rejected(factor_basis):
    category = DFFCategory.NOT_ACCESSIBLE
    with pytest.raises(ValueError, match="citation"):
        select_design_fatigue_factor(
            category,
            factors={category: 3.0},
            citations={DFFCategory.ACCESSIBLE_CRITICAL: factor_basis},
        )


@pytest.mark.parametrize("field", ["code_id", "publisher", "revision"])
def test_mismatched_source_metadata_is_rejected(field, factor_basis):
    category = DFFCategory.NOT_ACCESSIBLE
    with pytest.raises(CitationResolutionError, match=f"frontmatter_mismatch:{field}"):
        select_design_fatigue_factor(
            category,
            factors={category: 3.0},
            citations={category: replace(factor_basis, **{field: "unresolved"})},
        )


def test_unresolved_source_is_rejected(factor_basis):
    category = DFFCategory.NOT_ACCESSIBLE
    missing = replace(factor_basis, wiki_path="wikis/engineering/missing.md")
    with pytest.raises(CitationResolutionError, match="page_missing"):
        select_design_fatigue_factor(
            category, factors={category: 3.0}, citations={category: missing}
        )


def test_unknown_category_is_rejected():
    with pytest.raises(ValueError):
        select_design_fatigue_factor("unknown")


@pytest.mark.parametrize(
    "factor",
    [0, -1, 0.5, float("nan"), float("inf"), -float("inf"), True, "3", None],
)
def test_invalid_factors_are_rejected(factor, factor_basis):
    with pytest.raises(ValueError, match="factor"):
        select_design_fatigue_factor(
            DFFCategory.NOT_ACCESSIBLE,
            factors={DFFCategory.NOT_ACCESSIBLE: factor},
            citations={DFFCategory.NOT_ACCESSIBLE: factor_basis},
        )
    with pytest.raises(ValueError, match="factor"):
        required_calculated_life(25.0, factor)


@pytest.mark.parametrize(
    "years", [0, -1, float("nan"), float("inf"), -float("inf"), True, "25", None]
)
def test_invalid_service_life_is_rejected(years):
    with pytest.raises(ValueError, match="service_life_years"):
        required_calculated_life(years, 3.0)


def test_overflow_is_rejected():
    with pytest.raises(ValueError, match="required calculated life"):
        required_calculated_life(sys.float_info.max, 3.0)


def test_fractional_years_and_configured_factor():
    assert required_calculated_life(12.5, 1.5) == 18.75


def test_required_life_is_a_target_for_existing_life_check(factor_basis):
    curve = get_sn_curve("D")
    annual_cycles = float(curve.cycles(100.0)) / 50.0
    histogram = pd.DataFrame({"stress_range": [100.0], "cycles": [annual_cycles]})
    factor = select_design_fatigue_factor(
        DFFCategory.NOT_ACCESSIBLE,
        factors={DFFCategory.NOT_ACCESSIBLE: 3.0},
        citations={DFFCategory.NOT_ACCESSIBLE: factor_basis},
    )
    target = required_calculated_life(25.0, factor)

    unfactored = design_life_check(histogram, curve, target_years=25.0)
    factored = design_life_check(histogram, curve, target_years=target)

    assert unfactored["years_to_failure"] == pytest.approx(50.0)
    assert unfactored["pass_fail"] == "PASS"
    assert factored["years_to_failure"] == pytest.approx(50.0)
    assert factored["pass_fail"] == "FAIL"


def test_required_life_with_hand_calculated_synthetic_loading(factor_basis):
    # Synthetic single-slope curve: N = 1,000,000 * (100 / stress_range)**3.
    # At 100 and 200 MPa, N is 1,000,000 and 125,000 cycles respectively.
    # Annual damage = 10,000/1,000,000 + 1,250/125,000 = 0.02.
    # Life = 1/0.02 = 50 years; required life = 25*3 = 75 years.
    # Damage at 25 years = 0.5; damage at 75 years = 1.5.
    curve = WoehlerCurve(pd.Series({"k_1": 3.0, "k_2": 3.0, "ND": 1e6, "SD": 100.0}))
    histogram = pd.DataFrame(
        {"stress_range": [100.0, 200.0], "cycles": [10000.0, 1250.0]}
    )
    category = DFFCategory.NOT_ACCESSIBLE
    factor = select_design_fatigue_factor(
        category, factors={category: 3.0}, citations={category: factor_basis}
    )
    target = required_calculated_life(25.0, factor)
    damage = miner_damage(histogram, curve)
    unfactored = design_life_check(histogram, curve, target_years=25.0)
    factored = design_life_check(histogram, curve, target_years=target)

    assert damage["allowable_cycles"].tolist() == pytest.approx([1000000.0, 125000.0])
    assert damage.attrs["total_damage"] == pytest.approx(0.02)
    assert target == pytest.approx(75.0)
    assert unfactored["damage"] == pytest.approx(0.5)
    assert unfactored["years_to_failure"] == pytest.approx(50.0)
    assert unfactored["pass_fail"] == "PASS"
    assert factored["damage"] == pytest.approx(1.5)
    assert factored["years_to_failure"] == pytest.approx(50.0)
    assert factored["pass_fail"] == "FAIL"
