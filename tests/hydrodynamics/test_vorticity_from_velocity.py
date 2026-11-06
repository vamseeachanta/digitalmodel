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
    The injected sample already uses the adapter's (component, y, x) order;
    comparison with its injected curl checks transport, not upstream layout.
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


@pytest.mark.parametrize("shape", [(4, 3, 2), (3, 4), (1, 3, 4), (2, 0, 4)])
def test_stream_rejects_velocity_outside_local_layout(monkeypatch, shape):
    """Raw (x, y, component) fields must not silently enter the local adapter."""
    sample = {"velocity": np.zeros(shape), "vorticity": np.zeros((1, 3, 4))}
    monkeypatch.setattr(flow, "WELL_AVAILABLE", True)
    monkeypatch.setattr(flow.ShearFlowLoader, "_build_dataset", lambda self: [sample])
    with pytest.raises(ValueError, match="velocity shape"):
        flow.ShearFlowLoader().stream_sample(n_steps=1)


@pytest.mark.parametrize("shape", [(4, 3, 1), (3, 4), (1, 4, 3), (2, 3, 4)])
def test_stream_rejects_vorticity_outside_matching_layout(monkeypatch, shape):
    """The scalar field must share the velocity grid with one leading component."""
    sample = {"velocity": np.zeros((2, 3, 4)), "vorticity": np.zeros(shape)}
    monkeypatch.setattr(flow, "WELL_AVAILABLE", True)
    monkeypatch.setattr(flow.ShearFlowLoader, "_build_dataset", lambda self: [sample])
    with pytest.raises(ValueError, match="vorticity shape"):
        flow.ShearFlowLoader().stream_sample(n_steps=1)


def test_explicit_upstream_transpose_preserves_coordinates(monkeypatch):
    """A non-square (x, y, component) snapshot needs both axis permutations.

    Chosen spacings dx=0.5 m, dy=2 m and u=3y+2x+7, v=5x-4y-1
    give the independent curl 5-3=2 inverse seconds. Moving only the
    component axis would leave x and y reversed.
    """
    x, y = np.meshgrid(np.arange(4) * 0.5, np.arange(3) * 2, indexing="ij")
    raw_velocity = np.stack((3 * y + 2 * x + 7, 5 * x - 4 * y - 1), axis=-1)
    raw_vorticity = np.full((4, 3, 1), 2.0)
    sample = {
        "velocity": raw_velocity.transpose(2, 1, 0),
        "vorticity": raw_vorticity.transpose(2, 1, 0),
    }
    monkeypatch.setattr(flow, "WELL_AVAILABLE", True)
    monkeypatch.setattr(flow.ShearFlowLoader, "_build_dataset", lambda self: [sample])
    snapshot = flow.ShearFlowLoader().stream_sample(n_steps=1)[0]
    assert snapshot["velocity"].shape == (2, 3, 4)
    assert snapshot["vorticity"].shape == (1, 3, 4)
    np.testing.assert_array_equal(
        flow.vorticity_from_velocity(snapshot["velocity"], dx_m=0.5, dy_m=2),
        np.full((1, 3, 4), 2.0),
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
        pytest.param(10**400, id="positive-overflow"),
        pytest.param(-(10**400), id="negative-overflow"),
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
