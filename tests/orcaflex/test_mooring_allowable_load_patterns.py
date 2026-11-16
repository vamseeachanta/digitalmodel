"""Regression patterns for existing allowable mooring load calculations.

All numeric inputs below are illustrative example inputs, independently chosen.
The source is a legacy engineering workbook. Its repeated method is allowable
force = breaking force / safety factor, with a dimensionless factor and forces
in the same unit. The existing API accepts kN; the shared registry converts
boundary units. No new calculation implementation or material data is added.

Standards as stated in the source: API RP 2SK and API 2SK;
clause not stated in source. These tests check arithmetic and dimensional
identities, not the independent suitability of the existing standard factors.

Input gaps: the source does not establish the standard edition, clause, analysis
method or intact/damaged applicability. Its force notation requires an explicit
force-versus-mass interpretation. Material certificates, catalogue provenance,
and load-analysis provenance remain caller responsibilities. The existing input
model accepts positive infinity; rejection of that input remains unresolved.
No catenary equilibrium, environmental loading, fatigue or material sizing is
calculated here. Reference copies and charts introduce no additional method.
"""

from decimal import Decimal, localcontext
from fractions import Fraction

import pytest
from pint import DimensionalityError
from pydantic import ValidationError

from digitalmodel.orcaflex.code_check_engine import APIRP2SKInput, LoadCondition
from digitalmodel.units import Q_


# divisor as stated in a legacy engineering workbook citing API RP 2SK; clause not stated in source
SOURCE_STATED_DIVISOR = 1.67


@pytest.mark.parametrize("breaking_force_kn", [3600.0, 7200.0, 14400.0])
@pytest.mark.parametrize("condition", ["operating", "extreme"])
def test_allowable_force_matches_independent_decimal_division(
    breaking_force_kn, condition
):
    data = APIRP2SKInput(mbl_kN=breaking_force_kn, condition=condition)
    assert data.required_safety_factor() == SOURCE_STATED_DIVISOR
    with localcontext() as context:
        context.prec = 40
        expected_kn = float(Decimal(str(breaking_force_kn)) / Decimal("1.67"))
    assert data.max_allowable_tension() == pytest.approx(expected_kn, rel=1e-12)


def test_allowable_force_matches_literal_reference_value():
    assert APIRP2SKInput(
        mbl_kN=3600.0, condition="extreme"
    ).max_allowable_tension() == pytest.approx(2155.688622754491, rel=1e-12)


@pytest.mark.parametrize("condition", ["operating", "extreme"])
def test_allowable_force_scales_linearly_and_recovers_breaking_force(condition):
    base = APIRP2SKInput(mbl_kN=3600.0, condition=condition)
    scaled = APIRP2SKInput(mbl_kN=10800.0, condition=condition)
    allowable_kn = base.max_allowable_tension()
    assert scaled.max_allowable_tension() == pytest.approx(3.0 * allowable_kn)
    assert allowable_kn * base.required_safety_factor() == pytest.approx(3600.0)


def test_allowable_force_is_invariant_under_boundary_unit_conversion():
    breaking_force_n = 3_600_000.0  # illustrative example
    breaking_force_kn = Q_(breaking_force_n, "N").to("kN").magnitude
    data = APIRP2SKInput(mbl_kN=breaking_force_kn, condition=LoadCondition.EXTREME)
    allowable_n = Q_(data.max_allowable_tension(), "kN").to("N").magnitude
    assert allowable_n * data.required_safety_factor() == pytest.approx(breaking_force_n)


def test_existing_length_converter_matches_exact_length_identity():
    diameter_inch = 0.375  # illustrative example
    # International inch in metres: the exact rational number 127 / 5000.
    expected_m = float(Fraction(3, 8) * Fraction(127, 5000))
    actual_m = Q_(diameter_inch, "inch").to("m").magnitude
    assert actual_m == pytest.approx(expected_m, rel=1e-12)
    expected_mm = float(Fraction(3, 8) * Fraction(127, 5))
    actual_mm = Q_(diameter_inch, "inch").to("mm").magnitude
    assert actual_mm == pytest.approx(expected_mm, rel=1e-12)


@pytest.mark.parametrize("breaking_force_kn", [0.0, -3600.0, float("nan"), "invalid"])
def test_existing_input_model_rejects_invalid_breaking_force(breaking_force_kn):
    with pytest.raises(ValidationError):
        APIRP2SKInput(mbl_kN=breaking_force_kn, condition="extreme")


@pytest.mark.xfail(
    strict=True,
    reason="existing model accepts non-finite breaking force; see open issue",
)
def test_existing_input_model_rejects_infinite_breaking_force():
    with pytest.raises(ValidationError):
        APIRP2SKInput(mbl_kN=float("inf"), condition="extreme")


def test_existing_input_model_rejects_unknown_condition():
    with pytest.raises(ValidationError):
        APIRP2SKInput(mbl_kN=3600.0, condition="unknown")


def test_existing_unit_converter_rejects_incompatible_dimensions():
    with pytest.raises(DimensionalityError):
        Q_(0.375, "second").to("m")
