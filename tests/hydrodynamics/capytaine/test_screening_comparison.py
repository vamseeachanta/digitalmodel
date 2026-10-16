# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Hand examples for diffraction comparison and dimensional transformations.

Mass-transfer hand inputs are synthetic, assumed, based on previous experience.
"""

import numpy as np
import pytest



def test_difference_uses_held_maximum_over_periods():
    """A ten-unit change is ten percent of the held maximum of one hundred."""
    from digitalmodel.hydrodynamics.capytaine.screening import normalised_difference
    held = np.array([[50.0], [100.0]])
    computed = np.array([[60.0], [80.0]])
    np.testing.assert_array_equal(normalised_difference(computed, held), [[10.0], [-20.0]])


def test_mass_transfer_matches_hand_parallel_axis_tensor():
    """The tensor follows the parallel-axis theorem and velocity-reference transfer."""
    from digitalmodel.hydrodynamics.capytaine.screening import rigid_body_mass_matrix
    matrix = rigid_body_mass_matrix(2.0, np.array([1.0, 2.0, 3.0]), np.diag([5.0, 6.0, 7.0]))
    np.testing.assert_array_equal(matrix[:3, :3], 2.0 * np.eye(3))
    np.testing.assert_array_equal(matrix[3:, 3:], [[31.0, -4.0, -6.0], [-4.0, 26.0, -12.0], [-6.0, -12.0, 17.0]])
    np.testing.assert_array_equal(matrix[:3, 3:], [[0.0, 6.0, -4.0], [-6.0, 0.0, 2.0], [4.0, -2.0, 0.0]])
    np.testing.assert_array_equal(matrix, matrix.T)


def test_finite_depth_dispersion_equation():
    """The computed wave number satisfies the finite-depth dispersion relation."""
    from digitalmodel.hydrodynamics.capytaine.screening import finite_depth_wave_number
    period, depth, gravity = 200.0, 1000.0, 9.80665
    k = finite_depth_wave_number(period, depth, gravity)
    assert gravity * k * np.tanh(k * depth) == pytest.approx((2.0 * np.pi / period)**2, rel=1e-10)


def test_natural_period_matches_constant_added_mass_hand_example():
    """Mass one plus added mass three gives four pi for unit restoring stiffness."""
    from digitalmodel.hydrodynamics.capytaine.screening import natural_period
    period = natural_period(np.array([10.0, 15.0, 20.0]), np.array([3.0, 3.0, 3.0]), 1.0, 1.0)
    assert period == pytest.approx(4.0 * np.pi, rel=1e-10)


def test_drag_band_includes_twenty_percent_boundaries():
    """The assumed band includes both endpoints and excludes adjacent periods."""
    from digitalmodel.hydrodynamics.capytaine.screening import drag_band_flags
    periods = np.array([7.9, 8.0, 10.0, 12.0, 12.1])
    np.testing.assert_array_equal(drag_band_flags(periods, [10.0]), [False, True, True, True, False])
