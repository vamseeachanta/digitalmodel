# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic balances for signed wall skin-friction extraction.

All finite fixture values are chosen synthetic inputs. Expected coefficients
come from the hand calculations documented below, using Cf = tau / (rho U² / 2).
No fitted turbulence constants or solver output are used.
"""

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.validation.wall_skin_friction import (
    skin_friction_profile,
)


@pytest.mark.parametrize(
    ("density", "speed", "expected"),
    [
        (2.0, 4.0, [0.5, -0.25, 0.0]),
        (4.0, 4.0, [0.25, -0.125, 0.0]),
        (2.0, 8.0, [0.125, -0.0625, 0.0]),
    ],
)
def test_skin_friction_hand_balance(density, speed, expected):
    """q = 2*4²/2 = 16 Pa; doubling rho gives 32, doubling U gives 64.

    Shears of 8, -4 and 0 Pa divided by each pressure give the literal
    coefficients above. Negative shear must retain its sign.
    """
    x, cf = skin_friction_profile(
        [0.0, 0.5, 1.0], [8.0, -4.0, 0.0], density=density, speed=speed
    )
    np.testing.assert_array_equal(x, [0.0, 0.5, 1.0])
    np.testing.assert_allclose(cf, expected)


def test_profile_mask_preserves_coordinates():
    """Selection retains input order and repeated coordinates; q = 16 Pa."""
    x, cf = skin_friction_profile(
        [3.0, 1.0, 3.0, 2.0],
        [8.0, -4.0, 0.0, 16.0],
        density=2.0,
        speed=4.0,
        developed_mask=[True, False, True, True],
    )
    np.testing.assert_array_equal(x, [3.0, 3.0, 2.0])
    np.testing.assert_allclose(cf, [0.5, 0.0, 1.0])


@pytest.mark.parametrize(
    ("x", "shear"),
    [
        ([], []),
        ([1.0], []),
        ([], [8.0]),
        ([1.0, 2.0], [8.0]),
        (1.0, [8.0]),
        ([1.0], 8.0),
        ([[1.0]], [8.0]),
        ([1.0], [[8.0]]),
    ],
)
def test_profile_rejects_empty_or_inconsistent_inputs(x, shear):
    """Scalar, multidimensional and unmatched samples are not profiles."""
    with pytest.raises(ValueError):
        skin_friction_profile(x, shear, density=2.0, speed=4.0)


@pytest.mark.parametrize(
    "mask",
    [[], [False, False], [True], [True, False, True], [1, 0], True, [[True, False]]],
)
def test_profile_rejects_invalid_or_empty_mask(mask):
    """A mask must be Boolean, match the profile and select at least one row."""
    with pytest.raises(ValueError):
        skin_friction_profile(
            [1.0, 2.0], [8.0, -4.0], density=2.0, speed=4.0, developed_mask=mask
        )


@pytest.mark.parametrize("bad", [np.nan, np.inf, -np.inf])
@pytest.mark.parametrize("field", ["x", "wall_shear"])
@pytest.mark.parametrize("mask", [None, [True, False]])
def test_profile_rejects_nonfinite_samples_before_selection(bad, field, mask):
    """Even a row excluded by the developed-region mask must be valid."""
    samples = {"x": [1.0, 2.0], "wall_shear": [8.0, -4.0]}
    samples[field][1] = bad
    with pytest.raises(ValueError, match="finite"):
        skin_friction_profile(
            **samples, density=2.0, speed=4.0, developed_mask=mask
        )


@pytest.mark.parametrize("field", ["density", "speed"])
@pytest.mark.parametrize("bad", [0.0, -1.0, np.nan, np.inf, -np.inf, [2.0]])
def test_profile_rejects_invalid_normalization(field, bad):
    """Reference density and speed must be finite positive scalars."""
    reference = {"density": 2.0, "speed": 4.0}
    reference[field] = bad
    with pytest.raises(ValueError, match=field):
        skin_friction_profile([1.0], [8.0], **reference)


@pytest.mark.parametrize(
    ("density", "speed", "shear"),
    [
        (1.0, 1e308, 8.0),
        (1.0, 1e-308, 8.0),
        (1e-308, 1.0, 1e308),
    ],
)
def test_profile_rejects_unrepresentable_normalization(density, speed, shear):
    """Synthetic float extremes must fail rather than emit zero, inf or NaN."""
    with pytest.raises(ValueError):
        skin_friction_profile([1.0], [shear], density=density, speed=speed)


@pytest.mark.parametrize("mask", [None, np.array([True, False])])
def test_profile_does_not_modify_or_alias_inputs(mask):
    """The returned coordinate and coefficient arrays belong to the result."""
    x = np.array([1.0, 2.0])
    shear = np.array([8.0, -4.0])
    selected_x, cf = skin_friction_profile(
        x, shear, density=2.0, speed=4.0, developed_mask=mask
    )
    selected_x[:] = 0.0
    cf[:] = 0.0
    np.testing.assert_array_equal(x, [1.0, 2.0])
    np.testing.assert_array_equal(shear, [8.0, -4.0])
    if mask is not None:
        np.testing.assert_array_equal(mask, [True, False])
