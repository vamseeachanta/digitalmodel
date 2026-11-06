# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Engineering-value checks for the shear-flow loader using manufactured fields.

Source: OpenStax, Calculus Volume 3 (2016), section 6.5, equation (6.17):
https://openstax.org/books/calculus-volume-3/pages/6-5-divergence-and-curl
The out-of-plane curl is omega_z = dv/dx - du/dy. All grid coordinates,
slopes and offsets below are chosen synthetic inputs, not measured data or
coefficients from a standard. No real dataset or external service is used.
"""

import numpy as np
import pytest

from digitalmodel.hydrodynamics.well_datasets import ShearFlowLoader
from digitalmodel.hydrodynamics.well_datasets import shear_flow_loader as loader_module


class _TensorLike:
    """Exercise the loader's tensor conversion without a tensor dependency."""

    def __init__(self, values):
        self.values = values

    def numpy(self):
        return self.values.copy()


def _encode(values, representation):
    if representation == "list":
        return values.tolist()
    if representation == "tensor_like":
        return _TensorLike(values)
    return values.copy()


def _synthetic_dataset(expected_frames, expected_curl, representation, data_root):
    class SyntheticDataset:
        def __init__(self, **kwargs):
            assert kwargs == {
                "dataset_name": "shear_flow",
                "split": "test",
                "data_root": str(data_root),
            }

        def __iter__(self):
            for velocity in expected_frames:
                yield {
                    "velocity": _encode(velocity, representation),
                    "vorticity": _encode(expected_curl, representation),
                }

    return SyntheticDataset


def _linear_frames(du_dy, dv_dx):
    # Synthetic choices; see the test docstring for units and hand derivatives.
    x = np.array([-2, 0, 2, 4], dtype=float)
    y = np.array([-3, 0, 3], dtype=float)
    xx, yy = np.meshgrid(x, y, indexing="xy")
    frames = [np.stack((du_dy * yy + 1 + t, dv_dx * xx - 2 - t)) for t in (0, 1)]
    return x, y, frames


@pytest.mark.parametrize("representation", ["array", "list", "tensor_like"])
@pytest.mark.parametrize(
    ("du_dy", "dv_dx", "expected_vorticity"),
    [(2, 5, 3), (5, 2, -3), (2, 2, 0)],
    ids=["positive_curl", "negative_curl", "irrotational"],
)
def test_stream_preserves_linear_velocity_and_hand_calculated_curl(
    monkeypatch, tmp_path, representation, du_dy, dv_dx, expected_vorticity
):
    """Check signed curl and velocity values after loading two synthetic frames.

    Independent hand calculation, using the cited equation (6.17):
    u(x,y,t) = a*y + 1 + t, v(x,y,t) = c*x - 2 - t, in m/s;
    x = [-2, 0, 2, 4] m, y = [-3, 0, 3] m, t = [0, 1] s.
    The chosen frame offsets vary with time; a and c have units s^-1.
    Thus du/dy = a and dv/dx = c everywhere, including the boundaries:
    (a,c)=(2,5) gives 5-2=3 s^-1; (5,2) gives 2-5=-3 s^-1;
    (2,2) gives 2-2=0 s^-1 despite nonzero velocity gradients.
    The oracle is these hand derivatives, not a loader calculation.

    The grid is rectangular, with distinct x/y spacings, to expose axis swaps.
    Integer inputs and exactly representable slopes permit exact equality;
    no empirical coefficient or engineering acceptance limit is applied.
    """
    x, y, expected_frames = _linear_frames(du_dy, dv_dx)
    expected_curl = np.full((1, len(y), len(x)), expected_vorticity, dtype=float)
    monkeypatch.setattr(loader_module, "WELL_AVAILABLE", True)
    monkeypatch.setattr(
        loader_module,
        "WellDataset",
        _synthetic_dataset(expected_frames, expected_curl, representation, tmp_path),
    )
    loader = ShearFlowLoader(split="test", data_root=str(tmp_path))
    snapshots = loader.stream_sample(n_steps=len(expected_frames))

    assert len(snapshots) == len(expected_frames)
    for snapshot, expected_velocity in zip(snapshots, expected_frames, strict=True):
        velocity = snapshot["velocity"]
        vorticity = snapshot["vorticity"]
        assert isinstance(velocity, np.ndarray)
        assert isinstance(vorticity, np.ndarray)
        np.testing.assert_array_equal(velocity, expected_velocity)
        np.testing.assert_array_equal(vorticity, expected_curl)
        # OpenStax (2016), section 6.5, eq. (6.17), applied to loaded fields.
        # Difference quotients are exact for these linear, integer-valued inputs.
        curl_from_velocity = np.gradient(velocity[1], x, axis=1) - np.gradient(
            velocity[0], y, axis=0
        )
        np.testing.assert_array_equal(curl_from_velocity, expected_curl[0])
        np.testing.assert_array_equal(vorticity[0], curl_from_velocity)
