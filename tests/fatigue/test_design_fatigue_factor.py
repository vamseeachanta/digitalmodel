# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Selection, validation and life targets for design fatigue factors."""

import sys
from dataclasses import replace

import pandas as pd
import pytest
from pylife.materiallaws.woehlercurve import WoehlerCurve

from digitalmodel.citations import Citation, CitedValue, CitationResolutionError
from digitalmodel.fatigue import (
    DFFCategory,
    required_calculated_life,
    select_design_fatigue_factor,
)
from digitalmodel.fatigue.damage import design_life_check, miner_damage
from digitalmodel.fatigue.sn_curves import get_sn_curve


@pytest.fixture
def factor_basis(tmp_path):
    """Synthetic selection record; no governing-code suitability is asserted."""
    citation = Citation(
        code_id="SYNTHETIC-DFF",
        publisher="Synthetic test fixture",
        revision="1",
        section="Synthetic category selection",
        wiki_path="wikis/fatigue/synthetic-factor.md",
        source_sibling="generic",
        note="Synthetic test only: category, access and consequences supplied by test.",
    )
    page = tmp_path / citation.wiki_path
    page.parent.mkdir(parents=True)
    page.write_text(
        "---\ncode_id: SYNTHETIC-DFF\npublisher: Synthetic test fixture\n"
        "revision: 1\n---\nSynthetic selection record for arithmetic tests only.\n",
        encoding="utf-8",
    )
    return citation, tmp_path


def select_with_basis(category, factor, factor_basis):
    citation, root = factor_basis
    return select_design_fatigue_factor(
        category,
        factors={DFFCategory(category): CitedValue(factor, citation, units="1")},
        repo_root=root,
    )


@pytest.mark.parametrize(
    "category, factor, years",
    [
        (DFFCategory.ACCESSIBLE_NON_CRITICAL, 1.0, 25.0),
        (DFFCategory.ACCESSIBLE_CRITICAL, 2.0, 50.0),
        (DFFCategory.NOT_ACCESSIBLE, 3.0, 75.0),
        (DFFCategory.SUBSTANTIAL_CONSEQUENCE, 10.0, 250.0),
    ],
)
def test_explicit_synthetic_factors(category, factor, years, factor_basis):
    assert select_with_basis(category, factor, factor_basis) == factor
    assert required_calculated_life(25.0, factor) == years
    assert select_with_basis(category.value, factor, factor_basis) == factor


def test_configured_factor_preserves_input_without_fallback(factor_basis):
    citation, root = factor_basis
    factors = {DFFCategory.NOT_ACCESSIBLE: CitedValue(4.0, citation)}
    original = factors.copy()
    factor = select_design_fatigue_factor(
        DFFCategory.NOT_ACCESSIBLE, factors=factors, repo_root=root
    )
    assert factor == 4.0
    assert required_calculated_life(25.0, factor) == 100.0
    assert factors == original
    with pytest.raises(ValueError, match="explicit"):
        select_design_fatigue_factor(DFFCategory.ACCESSIBLE_CRITICAL, factors=factors)


@pytest.mark.parametrize("factors", [None, {}, {DFFCategory.ACCESSIBLE_CRITICAL: 2.0}])
def test_missing_selected_factor_is_rejected(factors):
    with pytest.raises(ValueError, match="explicit"):
        select_design_fatigue_factor(DFFCategory.NOT_ACCESSIBLE, factors=factors)


@pytest.mark.parametrize("value", [3.0, CitedValue(3.0, None)])
def test_missing_citation_is_rejected(value):
    with pytest.raises(ValueError, match="citation"):
        select_design_fatigue_factor(
            DFFCategory.NOT_ACCESSIBLE, factors={DFFCategory.NOT_ACCESSIBLE: value}
        )


@pytest.mark.parametrize("note", ["", "   ", None])
def test_missing_applicability_record_is_rejected(note, factor_basis):
    citation, root = factor_basis
    with pytest.raises(ValueError, match="applicability"):
        select_with_basis(DFFCategory.NOT_ACCESSIBLE, 3.0, (replace(citation, note=note), root))


def test_unresolved_citation_is_rejected(factor_basis):
    citation, root = factor_basis
    missing = replace(citation, wiki_path="wikis/fatigue/missing.md")
    with pytest.raises(CitationResolutionError, match="page_missing"):
        select_with_basis(DFFCategory.NOT_ACCESSIBLE, 3.0, (missing, root))


def test_citation_is_revalidated_on_each_selection(factor_basis):
    citation, root = factor_basis
    assert select_with_basis(DFFCategory.NOT_ACCESSIBLE, 3.0, factor_basis) == 3.0
    page = root / citation.wiki_path
    page.write_text(page.read_text().replace("revision: 1", "revision: 2"))
    with pytest.raises(CitationResolutionError, match="frontmatter_mismatch:revision"):
        select_with_basis(DFFCategory.NOT_ACCESSIBLE, 3.0, factor_basis)


def test_selection_uses_configured_citation_resolver(monkeypatch, factor_basis):
    citation, root = factor_basis
    monkeypatch.setenv("LLM_WIKI_PATH", str(root))
    factors = {DFFCategory.NOT_ACCESSIBLE: CitedValue(3.0, citation)}
    assert select_design_fatigue_factor(DFFCategory.NOT_ACCESSIBLE, factors=factors) == 3.0
    missing = replace(citation, wiki_path="wikis/fatigue/missing.md")
    factors[DFFCategory.NOT_ACCESSIBLE] = CitedValue(3.0, missing)
    with pytest.raises(CitationResolutionError, match="page_missing"):
        select_design_fatigue_factor(DFFCategory.NOT_ACCESSIBLE, factors=factors)


def test_dimensioned_factor_is_rejected(factor_basis):
    citation, root = factor_basis
    with pytest.raises(ValueError, match="dimensionless"):
        select_design_fatigue_factor(
            DFFCategory.NOT_ACCESSIBLE,
            factors={DFFCategory.NOT_ACCESSIBLE: CitedValue(3.0, citation, units="years")},
            repo_root=root,
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
        select_with_basis(DFFCategory.NOT_ACCESSIBLE, factor, factor_basis)
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


def test_required_life_round_trip_with_existing_life_check(factor_basis):
    curve = get_sn_curve("D")
    annual_cycles = float(curve.cycles(100.0)) / 50.0
    histogram = pd.DataFrame({"stress_range": [100.0], "cycles": [annual_cycles]})
    factor = select_with_basis(DFFCategory.NOT_ACCESSIBLE, 3.0, factor_basis)
    target = required_calculated_life(25.0, factor)

    unfactored = design_life_check(histogram, curve, target_years=25.0)
    factored = design_life_check(histogram, curve, target_years=target)

    assert unfactored["years_to_failure"] == pytest.approx(50.0)
    assert unfactored["pass_fail"] == "PASS"
    assert factored["years_to_failure"] == pytest.approx(50.0)
    assert factored["pass_fail"] == "FAIL"


def test_required_life_against_independent_synthetic_reference(factor_basis):
    """Hand reference: N(S) = 1e6 * (100/S)^3, without a slope change.

    At 100 MPa, N = 1e6; at 200 MPa, N = 125000. Fixed annual
    counts 10000 and 1250 give damage 0.01 + 0.01 = 0.02 and life
    1/0.02 = 50 years. Targets 25 and 75 years give damage 0.5 and 1.5.
    All curve, loading and factor inputs are synthetic, not code coefficients.
    """
    curve = WoehlerCurve(pd.Series({"k_1": 3.0, "k_2": 3.0, "SD": 100.0, "ND": 1e6}))
    histogram = pd.DataFrame({"stress_range": [100.0, 200.0], "cycles": [10000, 1250]})
    damage = miner_damage(histogram, curve)
    assert damage["allowable_cycles"].tolist() == pytest.approx([1000000, 125000])
    assert damage["damage"].tolist() == pytest.approx([0.01, 0.01])
    assert damage.attrs["total_damage"] == pytest.approx(0.02)
    factor = select_with_basis(DFFCategory.NOT_ACCESSIBLE, 3.0, factor_basis)
    target = required_calculated_life(25.0, factor)
    assert target == 75.0
    unfactored = design_life_check(histogram, curve, target_years=25.0)
    factored = design_life_check(histogram, curve, target_years=target)
    assert unfactored["years_to_failure"] == pytest.approx(50.0)
    assert factored["years_to_failure"] == pytest.approx(50.0)
    assert unfactored["damage"] == pytest.approx(0.5)
    assert factored["damage"] == pytest.approx(1.5)
    assert unfactored["pass_fail"] == "PASS"
    assert factored["pass_fail"] == "FAIL"
