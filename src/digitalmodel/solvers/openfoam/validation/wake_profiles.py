# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reduce supplied wake samples without running a flow solver.

The definition used here is mean = sum(u)/N and population fluctuation
RMS = sqrt(sum((u - mean)**2)/N), independently for each probe and component.
The denominator is N, with no sample-variance correction. Samples carry equal
weight even when their timestamps are irregular; this is not time integration.
Coordinates and velocities retain the caller's units and component frame.

Example using constructed scalar data and an independent hand calculation:
the mean of [1, 3, 5] is 3; the squared deviations sum to 4 + 0 + 4 = 8.

>>> result = wake_profile_statistics(
...     [0, 1, 2], [[1], [3], [5]], [[4, 0, 0]], (0, 2)
... )
>>> result.mean_velocity.tolist()
[3.0]
>>> bool(np.allclose(result.rms_fluctuations, [np.sqrt(8 / 3)]))
True
>>> result.selected_times.tolist(), result.sample_count
([0.0, 1.0, 2.0], 3)
"""

from dataclasses import dataclass, field
from typing import Literal

import numpy as np
from numpy.typing import ArrayLike, NDArray


@dataclass(frozen=True)
class WakeProfileStatistics:
    """Per-probe statistics and the exact sample support used to compute them.

    Attributes:
        probe_coords: Copied Cartesian coordinates in input probe order.
        mean_velocity: Mean of the selected samples, with the time axis removed.
        rms_fluctuations: Componentwise RMS about the selected mean.
        selected_times: Copied timestamps inside the inclusive requested window.
        averaging_interval: Requested inclusive bounds, without extrapolation.
        sample_count: Number of selected samples, the population denominator.
        rms_convention: Population convention, including for a single sample.
        weighting: Equal sample weights, without temporal quadrature.
    """

    probe_coords: NDArray[np.float64]
    mean_velocity: NDArray[np.float64]
    rms_fluctuations: NDArray[np.float64]
    selected_times: NDArray[np.float64]
    averaging_interval: tuple[float, float]
    sample_count: int
    rms_convention: Literal["population"] = field(default="population", init=False)
    weighting: Literal["equal_samples"] = field(default="equal_samples", init=False)


def _finite_real_array(values: ArrayLike, name: str) -> NDArray[np.float64]:
    """Convert real numerical inputs without discarding imaginary components."""
    if np.iscomplexobj(values):
        raise ValueError(f"{name} must contain real numbers")
    try:
        array = np.asarray(values, dtype=np.float64)
    except (TypeError, ValueError, OverflowError) as exc:
        raise ValueError(f"{name} must contain real numbers") from exc
    if not np.all(np.isfinite(array)):
        raise ValueError(f"{name} must contain only finite values")
    return array


def wake_profile_statistics(
    times: ArrayLike,
    velocity_samples: ArrayLike,
    probe_coords: ArrayLike,
    averaging_interval: tuple[float, float],
) -> WakeProfileStatistics:
    """Compute equal-sample mean velocity and population RMS fluctuations.

    Args:
        times: Strictly increasing finite timestamps, shape (n_times,).
        velocity_samples: Finite scalar samples, shape (n_times, n_probes),
            or Cartesian vectors, shape (n_times, n_probes, 3). The trailing
            dimension represents the three Cartesian velocity components;
            vector magnitudes are not computed.
        probe_coords: Finite Cartesian locations, shape (n_probes, 3).
            Every supplied probe is retained in its original order.
        averaging_interval: Explicit finite (start, end) bounds, inclusive.
            Bounds may extend beyond the supplied times; selected_times records
            actual support. A single selected sample has zero population RMS.

    Returns:
        Statistics with the time axis removed and copied averaging support.

    Raises:
        ValueError: If shapes, finite real values, time order or bounds are
            invalid, no probes or samples are selected, or reduction overflows.
            Nonfinite samples are rejected even outside the selected window.
    """
    time = _finite_real_array(times, "times")
    velocity = _finite_real_array(velocity_samples, "velocity samples")
    coords = _finite_real_array(probe_coords, "probe coordinates")
    interval = _finite_real_array(averaging_interval, "averaging interval")

    if time.ndim != 1:
        raise ValueError("times must be one-dimensional")
    if np.any(time[1:] <= time[:-1]):
        raise ValueError("times must be strictly increasing")
    # Three coordinates/components define Cartesian space, not a physical fit.
    if coords.ndim != 2 or coords.shape[1] != 3:
        raise ValueError("probe coordinates must have shape (n_probes, 3)")
    if coords.shape[0] == 0:
        raise ValueError("at least one probe is required")
    if velocity.ndim not in (2, 3):
        raise ValueError("velocity samples must have two or three dimensions")
    if velocity.shape[0] != time.size:
        raise ValueError("velocity time dimension must match times")
    if velocity.shape[1] != coords.shape[0]:
        raise ValueError("velocity probe dimension must match probe coordinates")
    if velocity.ndim == 3 and velocity.shape[2] != 3:
        raise ValueError("vector velocity samples must have three components")
    if interval.shape != (2,) or interval[0] > interval[1]:
        raise ValueError("averaging interval must be (start, end) with start <= end")

    selected = (time >= interval[0]) & (time <= interval[1])
    selected_times = time[selected]
    if selected_times.size == 0:
        raise ValueError("averaging interval contains no samples")
    samples = velocity[selected]
    try:
        with np.errstate(over="raise", invalid="raise"):
            mean = np.mean(samples, axis=0)
            rms = np.sqrt(np.mean(np.square(samples - mean), axis=0))
    except FloatingPointError as exc:
        raise ValueError("velocity statistics exceed floating-point range") from exc

    return WakeProfileStatistics(
        probe_coords=coords.copy(),
        mean_velocity=mean,
        rms_fluctuations=rms,
        selected_times=selected_times,
        averaging_interval=(float(interval[0]), float(interval[1])),
        sample_count=int(selected_times.size),
    )
