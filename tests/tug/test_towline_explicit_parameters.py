# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Explicit speed-factor parameter coverage for issue #1197.

Numeric basis: independent hand calculations with synthetic inputs, using
min(cap, max(unity, (speed / reference) ** exponent)). The unity floor is 1.
These examples specify arithmetic behaviour, not classification factors.
"""

import pytest

from digitalmodel.tug.towline import speed_amplification_factor


# Hand calculations: with reference 4 and exponent 3, speeds 0, 2, 4 and 6
# give raw factors 0, 1/8, 1 and 27/8; cap 6 leaves only the floor active.
# Reference 8, exponent 1/2 and speed 18 give sqrt(9/4) = 3/2, below cap 3.
# Reference 4, exponent 3/2 and speed 16 give 4**(3/2) = 8, below cap 12.
@pytest.mark.parametrize(
    "speed, reference, exponent, cap, expected",
    [
        pytest.param(
            0.0, 4.0, 3.0, 6.0,
            min(6.0, max(1.0, (0.0 / 4.0) ** 3.0)),
            id="zero-speed-floor",
        ),
        pytest.param(
            2.0, 4.0, 3.0, 6.0,
            min(6.0, max(1.0, (2.0 / 4.0) ** 3.0)),
            id="below-reference-floor",
        ),
        pytest.param(
            4.0, 4.0, 3.0, 6.0,
            min(6.0, max(1.0, (4.0 / 4.0) ** 3.0)),
            id="at-reference-floor",
        ),
        pytest.param(
            6.0, 4.0, 3.0, 6.0,
            min(6.0, max(1.0, (6.0 / 4.0) ** 3.0)),
            id="cubic-interior",
        ),
        pytest.param(
            18.0, 8.0, 0.5, 3.0,
            min(3.0, max(1.0, (18.0 / 8.0) ** 0.5)),
            id="square-root-interior",
        ),
        pytest.param(
            16.0, 4.0, 1.5, 12.0,
            min(12.0, max(1.0, (16.0 / 4.0) ** 1.5)),
            id="fractional-power-interior",
        ),
    ],
)
def test_explicit_speed_reference_and_exponent_are_used(
    speed, reference, exponent, cap, expected
):
    """Explicit reference and exponent control the floor and interior factor."""
    actual = speed_amplification_factor(
        assist_speed_kn=speed,
        reference_speed_kn=reference,
        exponent=exponent,
        cap=cap,
    )
    assert actual == pytest.approx(expected)


# Hand calculations: reference 4, exponent 3, cap 8 reaches the cap at speed 8
# because (8/4)**3 = 8; speeds 7.5 and 8.5 bracket that threshold.
# Reference 3, exponent 1, cap 4 reaches it at speed 12; 11 and 13 bracket it.
# Reference 4, exponent 1/2, cap 3 reaches it at speed 36; 25 and 49 give
# sqrt(25/4) = 5/2 and sqrt(49/4) = 7/2, respectively, bracketing cap 3.
@pytest.mark.parametrize(
    "speed, reference, exponent, cap, expected",
    [
        pytest.param(
            7.5, 4.0, 3.0, 8.0,
            min(8.0, max(1.0, (7.5 / 4.0) ** 3.0)),
            id="cubic-below-cap",
        ),
        pytest.param(
            8.0, 4.0, 3.0, 8.0,
            min(8.0, max(1.0, (8.0 / 4.0) ** 3.0)),
            id="cubic-at-cap",
        ),
        pytest.param(
            8.5, 4.0, 3.0, 8.0,
            min(8.0, max(1.0, (8.5 / 4.0) ** 3.0)),
            id="cubic-above-cap",
        ),
        pytest.param(
            11.0, 3.0, 1.0, 4.0,
            min(4.0, max(1.0, (11.0 / 3.0) ** 1.0)),
            id="linear-below-cap",
        ),
        pytest.param(
            12.0, 3.0, 1.0, 4.0,
            min(4.0, max(1.0, (12.0 / 3.0) ** 1.0)),
            id="linear-at-cap",
        ),
        pytest.param(
            13.0, 3.0, 1.0, 4.0,
            min(4.0, max(1.0, (13.0 / 3.0) ** 1.0)),
            id="linear-above-cap",
        ),
        pytest.param(
            25.0, 4.0, 0.5, 3.0,
            min(3.0, max(1.0, (25.0 / 4.0) ** 0.5)),
            id="square-root-below-cap",
        ),
        pytest.param(
            36.0, 4.0, 0.5, 3.0,
            min(3.0, max(1.0, (36.0 / 4.0) ** 0.5)),
            id="square-root-at-cap",
        ),
        pytest.param(
            49.0, 4.0, 0.5, 3.0,
            min(3.0, max(1.0, (49.0 / 4.0) ** 0.5)),
            id="square-root-above-cap",
        ),
    ],
)
def test_speed_factor_transitions_at_explicit_cap(
    speed, reference, exponent, cap, expected
):
    """Explicit caps clamp factors only once the raw power reaches the cap."""
    actual = speed_amplification_factor(
        assist_speed_kn=speed,
        reference_speed_kn=reference,
        exponent=exponent,
        cap=cap,
    )
    assert actual == pytest.approx(expected)
