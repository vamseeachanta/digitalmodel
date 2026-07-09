"""Baseline-correction helpers for strong-motion records."""

from __future__ import annotations

import numpy as np


def pre_event_mean(
    acceleration_m_s2: list[float] | np.ndarray,
    *,
    samples: int,
) -> tuple[np.ndarray, float]:
    """Remove the mean of the first ``samples`` acceleration values."""

    values = np.asarray(acceleration_m_s2, dtype=float)
    if samples <= 0:
        raise ValueError("pre_event_mean samples must be positive")
    if samples > values.size:
        raise ValueError("pre_event_mean samples exceeds record length")
    offset_m_s2 = float(np.mean(values[:samples]))
    return values - offset_m_s2, offset_m_s2


def polynomial_detrend(
    time_s: list[float] | np.ndarray,
    acceleration_m_s2: list[float] | np.ndarray,
    *,
    order: int,
) -> tuple[np.ndarray, np.ndarray]:
    """Subtract a fitted polynomial trend from acceleration."""

    time = np.asarray(time_s, dtype=float)
    values = np.asarray(acceleration_m_s2, dtype=float)
    if time.size != values.size:
        raise ValueError("time_s and acceleration_m_s2 must have the same length")
    if order < 0:
        raise ValueError("polynomial order must be non-negative")
    if values.size <= order:
        raise ValueError("record length must exceed polynomial order")
    coefficients = np.polyfit(time, values, order)
    trend = np.polyval(coefficients, time)
    return values - trend, coefficients
