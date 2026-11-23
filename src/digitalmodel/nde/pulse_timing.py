# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Sampled peak timing for an isolated acoustic receiver pulse."""

from collections.abc import Sequence
from math import isfinite


def pulse_peak_time(time_s: Sequence[float], pressure: Sequence[float]) -> float:
    """Return the timestamp of the unique largest absolute pressure sample.

    This is a sampled peak convention, not a first-arrival detector. The caller
    supplies a window containing one isolated, baseline-corrected pulse and
    timestamps in seconds relative to a known emission reference. No material
    speed, distance, detection threshold or acceptance limit is assumed.

    For a nondispersive pulse with its peak emitted at time zero, the expected
    peak time is distance / speed (OpenStax, College Physics 2e, 2022, section
    2.3, equation 2.5):
    https://openstax.org/books/college-physics-2e/pages/2-3-time-velocity-and-speed
    With another emission reference, subtract it from the returned timestamp.
    The result has sample resolution only; there is no interpolation, noise
    rejection, echo discrimination or validation of The Well's frame schema.

    Args:
        time_s: Finite, strictly increasing receiver timestamps in seconds.
        pressure: Finite, signed, baseline-corrected samples in consistent units.

    Returns:
        Timestamp in seconds of the unique absolute peak.

    Raises:
        ValueError: If arrays are empty, mismatched, nonfinite, unordered,
            identically zero, or have tied absolute maxima.
    """
    if len(time_s) != len(pressure):
        raise ValueError("time_s and pressure must have the same length")
    if not len(time_s):
        raise ValueError("time_s and pressure must be nonempty")
    if not all(isfinite(value) for value in (*time_s, *pressure)):
        raise ValueError("timestamps and pressure must be finite")
    if any(later <= earlier for earlier, later in zip(time_s, time_s[1:])):
        raise ValueError("timestamps must be strictly increasing")
    magnitudes = [abs(value) for value in pressure]
    peak = max(magnitudes)
    # Zero denotes absence of signal algebraically, not an acceptance threshold.
    if peak == 0:
        raise ValueError("pressure must contain a nonzero pulse")
    if magnitudes.count(peak) != 1:
        raise ValueError("absolute pressure peak must be unique")
    return float(time_s[magnitudes.index(peak)])
