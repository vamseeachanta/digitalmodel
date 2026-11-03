# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand-calculation cases for the pure response chain (issue #1771).

All inputs below are synthetic choices, with no measured or generated spectrum.
The piecewise-linear wave spectrum is defined by these knots:

    omega (rad/s):        0.25    0.50    1.25    1.50    2.50
    S (m**2 s):          0.00    2.00    1.00    3.00    0.00

Hand calculation: each interval contributes width * (left + right) / 2:

    (0.50 - 0.25) * (0 + 2) / 2 = 1/4
    (1.25 - 0.50) * (2 + 1) / 2 = 9/8
    (1.50 - 1.25) * (1 + 3) / 2 = 1/2
    (2.50 - 1.50) * (3 + 0) / 2 = 3/2

Thus wave variance is 27/8 m**2. For a constant response factor c,
response variance is c**2 * 27/8 and significant single-amplitude is
2 * sqrt(c**2 * 27/8), following the significant_amplitude API convention.
The six factors below are independent synthetic component inputs, in m/m
for translations and deg/m for rotations; they do not represent coupling.
The scaling control chooses k = 3, giving variance ratio 9 and amplitude
ratio 3. Assertion tolerances allow only floating-point roundoff.
"""

import math

import numpy as np
import pytest

from digitalmodel.hydrodynamics.seakeeping import (
    compute_response_spectrum,
    significant_amplitude,
    spectral_moments,
)


@pytest.fixture
def piecewise_linear_spectrum():
    """Return the declared hand-calculation knots on a nonuniform grid."""
    frequencies = np.array([0.25, 0.50, 1.25, 1.50, 2.50])
    spectrum = np.array([0.00, 2.00, 1.00, 3.00, 0.00])
    return frequencies, spectrum


@pytest.fixture(
    params=[
        pytest.param(0.50, id="surge"),
        pytest.param(0.75, id="sway"),
        pytest.param(1.00, id="heave"),
        pytest.param(1.25, id="roll"),
        pytest.param(1.50, id="pitch"),
        pytest.param(2.00, id="yaw"),
    ]
)
def response_factor(request):
    """Supply the synthetic constant factor for each independent component."""
    return request.param


def test_piecewise_linear_response_variance_matches_hand_integral(
    piecewise_linear_spectrum, response_factor
):
    """Recover the hand-integrated variance and its significant amplitude."""
    frequencies, wave_spectrum = piecewise_linear_spectrum
    rao = np.full_like(frequencies, response_factor)
    response = compute_response_spectrum(rao, wave_spectrum)
    variance = spectral_moments(frequencies, response, orders=[0])[0]
    amplitude = significant_amplitude(variance)

    # Independent oracle: the four exact trapezoid areas documented above.
    wave_variance = 1 / 4 + 9 / 8 + 1 / 2 + 3 / 2
    expected_variance = response_factor**2 * wave_variance
    expected_amplitude = 2 * math.sqrt(expected_variance)

    np.testing.assert_allclose(
        response, response_factor**2 * wave_spectrum, rtol=1e-12, atol=1e-12
    )
    assert variance == pytest.approx(expected_variance, rel=1e-12, abs=1e-12)
    assert amplitude == pytest.approx(expected_amplitude, rel=1e-12, abs=1e-12)


def test_response_statistics_follow_rao_scaling(
    piecewise_linear_spectrum, response_factor
):
    """Tripling the supplied RAO squares variance and triples amplitude."""
    frequencies, wave_spectrum = piecewise_linear_spectrum
    rao = np.full_like(frequencies, response_factor)
    scale = 3.0  # Synthetic scaling control declared in the hand calculation.
    response = compute_response_spectrum(rao, wave_spectrum)
    scaled_response = compute_response_spectrum(scale * rao, wave_spectrum)
    variance = spectral_moments(frequencies, response, orders=[0])[0]
    scaled_variance = spectral_moments(frequencies, scaled_response, orders=[0])[0]
    amplitude = significant_amplitude(variance)
    scaled_amplitude = significant_amplitude(scaled_variance)

    # Closed-form oracle, independent of either computed variance.
    expected_variance = (scale * response_factor) ** 2 * (27 / 8)
    expected_amplitude = 2 * math.sqrt(expected_variance)
    assert scaled_variance == pytest.approx(expected_variance, rel=1e-12, abs=1e-12)
    assert scaled_amplitude == pytest.approx(expected_amplitude, rel=1e-12, abs=1e-12)
    assert scaled_variance / variance == pytest.approx(9.0, rel=1e-12, abs=1e-12)
    assert scaled_amplitude / amplitude == pytest.approx(3.0, rel=1e-12, abs=1e-12)
