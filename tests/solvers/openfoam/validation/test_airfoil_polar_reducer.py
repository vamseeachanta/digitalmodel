# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for a synthetic airfoil polar.

The fixtures are defined here, without aerodynamic reference data: the exact
line is Cl = 0.2 + 0.1 * alpha_deg. The unit conversion follows directly from
pi radians = 180 degrees. Drag values are synthetic nonnegative coefficients.
pytest.approx supplies its standard floating point comparison tolerances;
the explicit zero-residual bound allows roundoff in an exact linear fit.
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
