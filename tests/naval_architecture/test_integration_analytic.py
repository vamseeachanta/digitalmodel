# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic polynomial oracles derived by hand, without external data.

The interval [1, 3] and the polynomial coefficients below are synthetic
choices. All expected constants follow from these independent calculations:

* Cubic A(x) = x**3 + 2*x**2 + x + 1 has antiderivative
  F(x) = x**4/4 + 2*x**3/3 + x**2/2 + x, so F(3)-F(1) = 130/3.
* Quartic A(x) = x**4 has F(x) = x**5/5, so F(3)-F(1) = 242/5.
  On a panel centered at c, expand (c+t)**4. The cubic and lower terms
  are integrated exactly. The quadratic interpolant of t**4 at -h, 0, h
  is h**2*t**2. Its integral minus the exact integral is
  2*h**5/3 - 2*h**5/5 = 4*h**5/15. Summing (b-a)/(2*h) panels gives
  S-I = 2*(b-a)*h**4/15, positive. Halving h divides this error by 2**4.

The nested grids have 2, 4, 8 and 16 intervals. Their binary-exact stations
and polynomial values keep rounding confined to the final division and
multiplication in the rule and the conversion of the rational oracle.
Four ulps of the exact integral conservatively bound those roundings,
including the subtraction when measuring the quartic error.

This closes one numerical-method slice; other analytical functions and
regulatory criteria remain open.
"""

from fractions import Fraction
from math import ulp

import pytest

from digitalmodel.naval_architecture.integration import simpsons_first_rule


@pytest.mark.parametrize("intervals", [2, 4, 8, 16])
def test_simpson_cubic_matches_antiderivative(intervals):
    """Integrate a positive cubic area curve to the rounding bound above."""
    a, b = 1, 3
    spacing = (b - a) / intervals
    stations = [a + i * spacing for i in range(intervals + 1)]
    areas = [x**3 + 2 * x**2 + x + 1 for x in stations]

    def antiderivative(x):
        return Fraction(x**4, 4) + Fraction(2 * x**3, 3) + Fraction(x**2, 2) + x

    exact = float(antiderivative(b) - antiderivative(a))
    result = simpsons_first_rule(areas, spacing)

    assert result == pytest.approx(exact, rel=0, abs=4 * ulp(exact))


def test_simpson_quartic_error_and_refinement():
    """Check signed truncation error and fourth-order nested refinement."""
    a, b = 1, 3
    exact = float(Fraction(b**5 - a**5, 5))
    roundoff = 4 * ulp(exact)
    errors = []
    expected_errors = []

    for intervals in (2, 4, 8, 16):
        spacing = (b - a) / intervals
        stations = [a + i * spacing for i in range(intervals + 1)]
        result = simpsons_first_rule([x**4 for x in stations], spacing)
        error = result - exact
        expected_error = 2 * (b - a) * spacing**4 / 15

        assert error > 0
        assert error == pytest.approx(expected_error, rel=0, abs=roundoff)
        errors.append(error)
        expected_errors.append(expected_error)

    refinement_ratio = 2**4
    for coarse, fine, expected_fine in zip(
        errors, errors[1:], expected_errors[1:]
    ):
        # If each error is perturbed by at most d, the ratio perturbation
        # is bounded by (1+r)*d/(E_fine-d), where r = 2**4 and d = roundoff.
        assert expected_fine > roundoff
        ratio_bound = (1 + refinement_ratio) * roundoff / (expected_fine - roundoff)
        assert coarse / fine == pytest.approx(
            refinement_ratio, rel=0, abs=ratio_bound
        )
