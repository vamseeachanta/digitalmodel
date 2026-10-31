# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent synthetic controls, derived from diagonal eigenvalues and algebra.

The six unit modal masses and squared integer stiffnesses are arithmetic
controls, not design data. For each mode det(K-w²I)=0 gives w=j, j=1,...,6.
Relative/absolute tolerances of 1e-11/1e-12 allow the bracket solver's default
absolute frequency tolerance and binary64 roundoff in this small system.
"""
import math

import numpy as np

from digitalmodel.marine_ops.fowt_quasi_static.response import (
    natural_periods,
    response_operators,
)


def assert_hand_periods(omega):
    """Compare every labelled period with 2*pi/j, independently of the solver."""
    expected = np.array([2 * math.pi / j for j in range(1, 7)])
    actual = natural_periods(
        omega, np.zeros((len(omega), 6, 6)), np.eye(6),
        np.diag([1., 4., 9., 16., 25., 36.]),
    )
    np.testing.assert_allclose(actual, expected, rtol=1e-11, atol=1e-12)


def test_exact_grid_natural_roots_are_retained():
    assert_hand_periods(np.arange(0.5, 7.0, 0.5))


def test_between_grid_natural_roots_are_retained():
    assert_hand_periods(np.arange(0.5, 7.0, 1.0))


def test_endpoint_natural_roots_are_retained():
    assert_hand_periods(np.arange(1., 6.5, 0.5))


def test_sampled_root_tolerance_scales_with_the_eigenvalues():
    """Uniform scaling by 1e-8 gives roots j*scale, never the lower grid bound.

    This is a synthetic arithmetic scaling of the same determinant, not an
    additional physical input. A unit absolute residual floor would accept
    nonroots when every eigenvalue is small.
    """
    scale = 1e-8
    omega = np.arange(0.5, 7.0, 0.5) * scale
    actual = natural_periods(
        omega, np.zeros((len(omega), 6, 6)), np.eye(6),
        np.diag((np.arange(1., 7.) * scale) ** 2),
    )
    expected = [2 * math.pi / (j * scale) for j in range(1, 7)]
    np.testing.assert_allclose(actual, expected, rtol=1e-11, atol=1e-12)


def test_scalar_impedance_full_complex_vector():
    """Z=5-1²(2+1)-i*1*1=2-i; 2/Z=(4+2i)/5.

    The diagonal system has condition number one. A 1e-12 tolerance is
    conservative relative to binary64 epsilon and tests both phasor components.
    """
    force = np.zeros((1, 1, 6), dtype=complex)
    force[0, 0, 0] = 2
    actual = response_operators(
        np.array([1.]), np.eye(6)[None], np.eye(6)[None],
        force, 2 * np.eye(6), 5 * np.eye(6), 0.,
    )
    expected = np.zeros_like(force)
    expected[0, 0, 0] = (4 + 2j) / 5
    np.testing.assert_allclose(actual, expected, rtol=1e-12, atol=1e-12)
    np.testing.assert_allclose(abs(actual[0, 0, 0]), math.sqrt(20) / 5,
                               rtol=1e-12, atol=1e-12)
    np.testing.assert_allclose(np.angle(actual[0, 0, 0]), math.atan2(2, 4),
                               rtol=1e-12, atol=1e-12)
