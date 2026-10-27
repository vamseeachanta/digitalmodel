# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check Rayleigh damping against independent hand calculations.

All inputs below are caller-chosen synthetic values, unrelated to material data.
The calculation basis is zeta(w) = alpha/(2*w) + beta*w/2, with w in rad/s.
Requiring zeta(w1) = zeta(w2) = z gives
alpha = 2*z*w1*w2/(w1+w2), beta = 2*z/(w1+w2).

Hand calculation A: w1=2, w2=18, z=1/20 gives alpha=9/50, beta=1/200.
At the targets, zeta(2)=9/200+1/200=1/20 and
zeta(18)=1/200+9/200=1/20.
Hand calculation B: w1=1/2, w2=8, z=17/200 gives alpha=2/25, beta=1/50.
At the targets, zeta(1/2)=2/25+1/200=17/200 and
zeta(8)=1/200+2/25=17/200.

Differentiation gives zeta'(w) = -alpha/(2*w**2) + beta/2, so the stationary
frequency is sqrt(alpha/beta). Since zeta''(w) = alpha/w**3 > 0 for positive
alpha and w, this is a strict minimum, with zeta_min = sqrt(alpha*beta).
For A, w_min=sqrt(36)=6 and zeta_min=sqrt(9/10000)=3/100.
The flanking values are zeta(3)=3/100+3/400=3/80 and
zeta(12)=3/400+3/100=3/80.

Limit calculation C: alpha=6/25, beta=0 gives zeta(2)=3/50, zeta(8)=3/200.
Limit calculation D: alpha=0, beta=1/200 gives zeta(2)=1/200, zeta(8)=1/50.
For the unit checks, one cycle is 2*pi radians, hence f=1 Hz means w=2*pi:
C gives 3/(50*pi), D gives pi/200, and their sum gives the combined damping.
Zero coefficients remove the corresponding term exactly. pytest.approx uses
its default floating-point tolerance for these closed-form calculations.
"""

import math

import pytest

from digitalmodel.drilling_riser.damping import (
    modal_damping_equivalent,
    rayleigh_damping_coefficients,
)


@pytest.mark.parametrize(
    "omega_1, omega_2, target_zeta, expected_alpha, expected_beta",
    [
        pytest.param(2, 18, 1 / 20, 9 / 50, 1 / 200, id="calculation-a"),
        pytest.param(1 / 2, 8, 17 / 200, 2 / 25, 1 / 50, id="calculation-b"),
    ],
)
def test_rayleigh_targets_recover_supplied_damping(
    omega_1, omega_2, target_zeta, expected_alpha, expected_beta
):
    """Recover the supplied damping at both target frequencies (A and B)."""
    alpha, beta = rayleigh_damping_coefficients(omega_1, omega_2, target_zeta)

    assert alpha == pytest.approx(expected_alpha)
    assert beta == pytest.approx(expected_beta)
    for omega in (omega_1, omega_2):
        freq_hz = omega / (2 * math.pi)
        assert modal_damping_equivalent(alpha, beta, freq_hz) == pytest.approx(
            target_zeta
        )


def test_rayleigh_interior_minimum():
    """Check the differentiated minimum and both flanks from calculation A."""
    alpha, beta = rayleigh_damping_coefficients(2, 18, 1 / 20)
    omega_min = 6
    expected_minimum = 3 / 100

    minimum = modal_damping_equivalent(alpha, beta, omega_min / (2 * math.pi))

    assert minimum == pytest.approx(expected_minimum)
    for omega in (3, 12):
        flank = modal_damping_equivalent(alpha, beta, omega / (2 * math.pi))
        assert flank == pytest.approx(3 / 80)
        assert flank > minimum


@pytest.mark.parametrize("omega, expected", [(2, 3 / 50), (8, 3 / 200)])
def test_modal_damping_mass_only(omega, expected):
    """Check the inverse-frequency mass-only limit from calculation C."""
    result = modal_damping_equivalent(6 / 25, 0, omega / (2 * math.pi))

    assert result == pytest.approx(expected)


@pytest.mark.parametrize("omega, expected", [(2, 1 / 200), (8, 1 / 50)])
def test_modal_damping_stiffness_only(omega, expected):
    """Check the linear-frequency stiffness-only limit from calculation D."""
    result = modal_damping_equivalent(0, 1 / 200, omega / (2 * math.pi))

    assert result == pytest.approx(expected)


@pytest.mark.parametrize(
    "alpha, beta, expected",
    [
        pytest.param(6 / 25, 0, 3 / (50 * math.pi), id="mass-only"),
        pytest.param(0, 1 / 200, math.pi / 200, id="stiffness-only"),
        pytest.param(
            6 / 25, 1 / 200, 3 / (50 * math.pi) + math.pi / 200, id="combined"
        ),
    ],
)
def test_modal_damping_frequency_units(alpha, beta, expected):
    """Use 1 Hz directly to require conversion to 2*pi rad/s (C and D)."""
    assert modal_damping_equivalent(alpha, beta, 1) == pytest.approx(expected)
