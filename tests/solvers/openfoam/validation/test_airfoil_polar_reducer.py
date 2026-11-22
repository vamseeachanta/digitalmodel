# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for a synthetic airfoil polar.

The fixtures are defined here, without aerodynamic reference data: the exact
line is Cl = 0.2 + 0.1 * alpha_deg. The unit conversion follows directly from
pi radians = 180 degrees. Drag values are synthetic nonnegative coefficients.
pytest.approx supplies its standard floating point comparison tolerances;
the explicit zero-residual bound allows roundoff in an exact linear fit.
The least-squares basis is NIST/SEMATECH e-Handbook of Statistical Methods
(2012 edition), section 4.4.3.1, "Least Squares"; the angle-unit basis is BIPM,
The International System of Units, 9th edition (2019), section 4, Table 8.
All expected fit values below are independent synthetic hand calculations.
"""

from math import pi

import pytest

from digitalmodel.solvers.openfoam.validation.airfoil_polar import reduce_airfoil_polar


def test_linear_polar_hand_slope():
    """Delta Cl / delta alpha = 0.8 / 8 = 0.1 per degree."""
    result = reduce_airfoil_polar(
        [-4, -2, 0, 2, 4], [-0.2, 0, 0.2, 0.4, 0.6], [0.02] * 5,
        angle_units="degrees", fit_interval=(-4, 4),
    )
    assert result.lift_slope_per_degree == pytest.approx(0.1)
    assert result.lift_slope_per_radian == pytest.approx(0.1 * 180 / pi)
    assert result.lift_intercept == pytest.approx(0.2)
    assert result.fitted_cl == pytest.approx([-0.2, 0, 0.2, 0.4, 0.6])
    assert result.residuals == pytest.approx([0] * 5, abs=1e-14)


def test_angle_units_agree():
    angles = [-4, -2, 0, 2, 4]
    cl = [-0.2, 0, 0.2, 0.4, 0.6]
    degree = reduce_airfoil_polar(
        angles, cl, [0.02] * 5, angle_units="degrees", fit_interval=(-2, 2),
    )
    radian = reduce_airfoil_polar(
        [a * pi / 180 for a in angles], cl, [0.02] * 5,
        angle_units="radians", fit_interval=(-2 * pi / 180, 2 * pi / 180),
    )
    assert degree.selected_indices == radian.selected_indices == (1, 2, 3)
    assert degree.lift_slope_per_radian == pytest.approx(radian.lift_slope_per_radian)
    assert degree.lift_slope_per_degree == pytest.approx(radian.lift_slope_per_degree)
    assert degree.lift_intercept == pytest.approx(radian.lift_intercept)
    assert degree.residuals == pytest.approx(radian.residuals)


def test_fit_interval_selection():
    """Retain both endpoints, original order and paired drag; exclude outliers."""
    angles = [4, 2, -4, 0, -2]
    cl = [20, 0.4, -20, 0.2, 0]
    cd = [0.04, 0.03, 0.04, 0.02, 0.03]
    result = reduce_airfoil_polar(
        angles, cl, cd, angle_units="degrees", fit_interval=(-2, 2),
    )
    assert result.selected_indices == (1, 3, 4)
    assert result.selected_angles == (2, 0, -2)
    assert result.selected_cl == (0.4, 0.2, 0)
    assert result.selected_cd == (0.03, 0.02, 0.03)
    assert result.angle_units == "degrees"
    assert result.fit_interval == (-2, 2)
    assert result.lift_slope_per_degree == pytest.approx(0.1)
    assert angles == [4, 2, -4, 0, -2]
    assert cl == [20, 0.4, -20, 0.2, 0]
    assert cd == [0.04, 0.03, 0.04, 0.02, 0.03]


def test_residuals_hand_calculation():
    """At x=(-1,0,1), mean Cl=1, Sxx=2, Sxy=1, hence slope=0.5."""
    result = reduce_airfoil_polar(
        [-1, 0, 1], [0, 2, 1], [0, 0, 0],
        angle_units="radians", fit_interval=(-1, 1),
    )
    assert result.lift_slope_per_radian == pytest.approx(0.5)
    assert result.lift_slope_per_degree == pytest.approx(0.5 * pi / 180)
    assert result.lift_intercept == pytest.approx(1)
    assert result.fitted_cl == pytest.approx([0.5, 1, 1.5])
    assert result.residuals == pytest.approx([-0.5, 1, -0.5])


def test_two_points_and_negative_lift_slope():
    """The line through (-1,3) and (1,-1) has slope -2 and intercept 1."""
    result = reduce_airfoil_polar(
        [-1, 1], [3, -1], [0, 0], angle_units="radians", fit_interval=(-1, 1),
    )
    assert result.lift_slope_per_radian == pytest.approx(-2)
    assert result.lift_intercept == pytest.approx(1)


def test_small_angle_spacing():
    """The line through (0,0) and (1e-200,1) has slope 1e200 per radian."""
    result = reduce_airfoil_polar(
        [0, 1e-200], [0, 1], [0, 0],
        angle_units="radians", fit_interval=(0, 1e-200),
    )
    assert result.lift_slope_per_radian == pytest.approx(1e200)
    assert result.lift_intercept == pytest.approx(0)


@pytest.mark.parametrize("angle_units", ["radians", "degrees"])
@pytest.mark.parametrize("reverse", [False, True])
def test_adjacent_angles_with_offset(angle_units, reverse):
    """Subtracting an absolute rounded mean must not halve a two-point slope."""
    # Hand calculation: delta Cl / delta angle = 1 / 2**-52 = 2**52;
    # Cl(1) = 0, so intercept = -2**52 and both residuals are zero.
    angles = [1.0, 1.0 + 2**-52]
    cl = [0.0, 1.0]
    if reverse:
        angles.reverse()
        cl.reverse()
    result = reduce_airfoil_polar(
        angles, cl, [0, 0], angle_units=angle_units,
        fit_interval=(1.0, 1.0 + 2**-52),
    )
    slope = (result.lift_slope_per_radian if angle_units == "radians"
             else result.lift_slope_per_degree)
    assert slope == pytest.approx(4503599627370496.0, rel=1e-15)
    assert result.lift_intercept == pytest.approx(-4503599627370496.0, rel=1e-15)
    assert result.fitted_cl == pytest.approx(cl, rel=1e-15, abs=1e-15)
    assert result.residuals == pytest.approx([0, 0], abs=1e-15)


@pytest.mark.parametrize("angle_units", ["radians", "degrees"])
@pytest.mark.parametrize("reverse", [False, True])
def test_fit_is_invariant_to_angle_translation(angle_units, reverse):
    """Translation preserves a noisy three-point fit and its signed residuals."""
    spacing = 2**-52
    angles = [0.0, spacing, 3 * spacing]
    cl = [0.0, 2.0, 1.0]
    if reverse:
        angles.reverse()
        cl.reverse()
    results = [
        reduce_airfoil_polar(
            [origin + angle for angle in angles], cl, [0, 0, 0],
            angle_units=angle_units, fit_interval=(origin, origin + 3 * spacing),
        )
        for origin in (0.0, 1.0)
    ]
    original, translated = results
    # In spacing units: mean x=4/3, mean Cl=1, Sxx=14/3, Sxy=1.
    # Slope=3/14, intercept=5/7, fitted=(5/7,13/14,19/14),
    # residuals=(-5/7,15/14,-5/14); none depends on the angle origin.
    expected_fitted = [5 / 7, 13 / 14, 19 / 14]
    expected_residuals = [-5 / 7, 15 / 14, -5 / 14]
    if reverse:
        expected_fitted.reverse()
        expected_residuals.reverse()
    for result in results:
        assert result.fitted_cl == pytest.approx(expected_fitted, abs=1e-15)
        assert result.residuals == pytest.approx(expected_residuals, abs=1e-15)
    assert translated.lift_slope_per_radian == pytest.approx(
        original.lift_slope_per_radian, rel=1e-15,
    )
    assert translated.lift_slope_per_degree == pytest.approx(
        original.lift_slope_per_degree, rel=1e-15,
    )
    assert original.lift_intercept == pytest.approx(5 / 7, abs=1e-15)
    # Translation by one subtracts the slope in the declared angle units.
    assert translated.lift_intercept == pytest.approx(5 / 7 - 3 / 14 / spacing)


def test_translation_preserves_wide_finite_angle_range():
    """Subtracting a sample origin must not overflow a representable fit."""
    # Hand calculation: slope = 2 / 2e308 = 1e-308; intercept = 0.
    result = reduce_airfoil_polar(
        [-1e308, 1e308], [-1, 1], [0, 0], angle_units="radians",
        fit_interval=(-1e308, 1e308),
    )
    assert result.lift_slope_per_radian == pytest.approx(1e-308, rel=1e-14, abs=0)
    assert result.lift_intercept == pytest.approx(0, abs=1e-15)
    assert result.fitted_cl == pytest.approx([-1, 1], abs=1e-15)
    assert result.residuals == pytest.approx([0, 0], abs=1e-15)


def test_unrepresentable_fit_fails_clearly():
    """Slope 1 / 1e-310 = 1e310 exceeds finite floating point range."""
    with pytest.raises(ValueError, match="finite floating point"):
        reduce_airfoil_polar(
            [0, 1e-310], [0, 1], [0, 0],
            angle_units="radians", fit_interval=(0, 1e-310),
        )


@pytest.mark.parametrize(
    "overrides, message",
    [
        ({"angle_units": "deg"}, "angle_units"),
        ({"angles": [0, 0, 1]}, "distinct"),
        ({"angles": [0, float("nan"), 1]}, "finite"),
        ({"angles": [0, float("inf"), 1]}, "finite"),
        ({"cl": [0, float("nan"), 1]}, "finite"),
        ({"cl": [0, float("inf"), 1]}, "finite"),
        ({"cd": [0, float("nan"), 0]}, "finite"),
        ({"cd": [0, float("inf"), 0]}, "finite"),
        ({"cd": [0, -0.01, 0]}, "nonnegative"),
        ({"cd": [0, 0]}, "length"),
        ({"cl": [0, 0]}, "length"),
        ({"angles": [], "cl": [], "cd": []}, "at least two"),
        ({"angles": [0], "cl": [0], "cd": [0]}, "at least two"),
        ({"fit_interval": (0, 0.5)}, "at least two"),
        ({"fit_interval": (3, 4)}, "at least two"),
        ({"fit_interval": (1, 0)}, "increasing"),
        ({"fit_interval": (1, 1)}, "increasing"),
        ({"fit_interval": (0, float("inf"))}, "finite"),
        ({"fit_interval": (float("nan"), 1)}, "finite"),
        ({"fit_interval": (0,)}, "two bounds"),
        ({"fit_interval": (0, 1, 2)}, "two bounds"),
        ({"angles": [[0], [1], [2]]}, "real numbers"),
    ],
)
def test_invalid_inputs_fail_clearly(overrides, message):
    arguments = dict(
        angles=[0, 1, 2], cl=[0, 1, 2], cd=[0, 0, 0],
        angle_units="degrees", fit_interval=(0, 2),
    )
    arguments.update(overrides)
    with pytest.raises(ValueError, match=message):
        reduce_airfoil_polar(**arguments)


def test_invalid_samples_outside_interval_are_rejected():
    with pytest.raises(ValueError, match="nonnegative"):
        reduce_airfoil_polar(
            [-1, 0, 1], [-1, 0, 1], [-0.01, 0, 0],
            angle_units="degrees", fit_interval=(0, 1),
        )
