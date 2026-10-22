# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Analytic curl checks through the shear-flow loader, without dataset access."""

import numpy as np
import pytest

import digitalmodel.hydrodynamics.well_datasets.shear_flow_loader as flow


@pytest.mark.parametrize(
    "dx,dy", [(0.5, 2), (2, 0.5), (np.float32(0.5), np.int64(2))]
)
def test_loaded_linear_field_has_hand_derived_vorticity(monkeypatch, dx, dy):
    """OpenStax Calculus Vol. 3 (2016), §6.5, Definition of Curl.

    Choose u=3*y+2*x+7 and v=5*x-4*y-1 in m/s, x,y in m. The z curl is
    dv/dx-du/dy = 5-3 = 2 s^-1 everywhere, including boundaries. Unequal
    spacings expose an axis swap or an assumption of unit spacing. The
    integers are synthetic field coefficients, not empirical constants.
    """
    x, y = np.meshgrid(np.arange(4) * dx, np.arange(3) * dy)
    velocity = np.stack((3 * y + 2 * x + 7, 5 * x - 4 * y - 1))
    sample = {"velocity": velocity, "vorticity": np.full((1, 3, 4), 2.0)}
    monkeypatch.setattr(flow, "WELL_AVAILABLE", True)
    monkeypatch.setattr(flow.ShearFlowLoader, "_build_dataset", lambda self: [sample])
    snapshot = flow.ShearFlowLoader().stream_sample(n_steps=1)[0]
    calculated = flow.vorticity_from_velocity(snapshot["velocity"], dx_m=dx, dy_m=dy)
    np.testing.assert_array_equal(calculated, np.full((1, 3, 4), 2.0))
    np.testing.assert_array_equal(snapshot["vorticity"], calculated)


def test_irrotational_field_has_zero_curl():
    """For chosen u=2*x, v=3*y, both cross derivatives vanish (§6.5 curl)."""
    x, y = np.meshgrid(np.arange(2), np.arange(2))
    velocity = np.stack((2 * x, 3 * y))
    np.testing.assert_array_equal(
        flow.vorticity_from_velocity(velocity, dx_m=1, dy_m=1),
        np.zeros((1, 2, 2)),
    )


@pytest.mark.parametrize(
    "spacing",
    [
        0,
        -1,
        float("nan"),
        float("inf"),
        True,
        False,
        "2",
        2 + 0j,
        None,
        np.bool_(True),
        np.bool_(False),
        np.array(1.0),
        [1],
    ],
)
@pytest.mark.parametrize("axis", ["dx_m", "dy_m"])
def test_invalid_grid_spacing(spacing, axis):
    """Uniform coordinate increments must be positive finite lengths."""
    inputs = {"dx_m": 1, "dy_m": 1}
    inputs[axis] = spacing
    with pytest.raises(ValueError, match=axis):
        flow.vorticity_from_velocity(np.zeros((2, 3, 4)), **inputs)


@pytest.mark.parametrize("shape", [(3, 3, 4), (2, 1, 4), (2, 3), (2, 3, 0)])
def test_invalid_velocity_shape(shape):
    """The documented layout is (u,v), y, x, with two points per derivative."""
    with pytest.raises(ValueError, match="shape"):
        flow.vorticity_from_velocity(np.zeros(shape), dx_m=1, dy_m=1)


def test_nonfinite_velocity_is_rejected():
    """A missing sample cannot represent a finite derivative."""
    velocity = np.zeros((2, 3, 4))
    velocity[0, 0, 0] = float("nan")
    with pytest.raises(ValueError, match="finite"):
        flow.vorticity_from_velocity(velocity, dx_m=1, dy_m=1)
