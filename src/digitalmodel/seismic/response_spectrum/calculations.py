"""PR1 kinematic preprocessing for the response_spectrum workflow.

This module performs baseline correction, optional signal filtering, and
kinematic acceleration-to-velocity/displacement integration. It does not solve
the SDOF equation of motion; Newmark EoM integration belongs to a later slice.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

import numpy as np
import pandas as pd
from scipy.integrate import cumulative_trapezoid

from digitalmodel.seismic.baseline import polynomial_detrend, pre_event_mean
from digitalmodel.seismic.readers.ascii import AccelerogramRecord
from digitalmodel.signal_processing.signal_analysis.filters.frequency import (
    FrequencyFilter,
)


@dataclass(frozen=True)
class KinematicResult:
    """Computed preprocessing result for one accelerogram."""

    record: AccelerogramRecord
    timeseries: pd.DataFrame
    summary: dict[str, Any]
    processing_chain: list[dict[str, Any]]
    method: str = "kinematic_cumtrapz"


def compute_kinematics(
    record: AccelerogramRecord,
    *,
    baseline: dict[str, Any] | None = None,
    frequency_filter: dict[str, Any] | None = None,
) -> KinematicResult:
    """Baseline-correct, optionally filter, then integrate an accelerogram."""

    baseline = baseline or {"method": "pre_event_mean"}
    filter_cfg = frequency_filter or {}
    corrected, baseline_step = _apply_baseline(record, baseline)
    filtered, filter_step = _apply_filter(corrected, record.dt_s, filter_cfg)
    velocity = cumulative_trapezoid(filtered, record.time_s, initial=0.0)
    displacement = cumulative_trapezoid(velocity, record.time_s, initial=0.0)
    timeseries = pd.DataFrame(
        {
            "time_s": record.time_s,
            "acceleration_m_s2": record.acceleration_m_s2,
            "corrected_acceleration_m_s2": corrected,
            "filtered_acceleration_m_s2": filtered,
            "velocity_m_s": velocity,
            "displacement_m": displacement,
        }
    )
    return KinematicResult(
        record=record,
        timeseries=timeseries,
        summary=_summary(record),
        processing_chain=[baseline_step, filter_step],
    )


def _apply_baseline(
    record: AccelerogramRecord,
    config: dict[str, Any],
) -> tuple[np.ndarray, dict[str, Any]]:
    method = str(config.get("method", "pre_event_mean")).lower()
    if method == "none":
        return record.acceleration_m_s2.copy(), {"step": "baseline", "method": "none"}
    if method == "pre_event_mean":
        samples = int(config.get("samples", min(10, record.npts)))
        corrected, offset = pre_event_mean(record.acceleration_m_s2, samples=samples)
        return corrected, {
            "step": "baseline",
            "method": method,
            "samples": samples,
            "offset_m_s2": offset,
        }
    if method in {"polynomial", "poly"}:
        order = int(config.get("order", 1))
        corrected, coefficients = polynomial_detrend(
            record.time_s,
            record.acceleration_m_s2,
            order=order,
        )
        return corrected, {
            "step": "baseline",
            "method": "polynomial",
            "order": order,
            "coefficients": coefficients.tolist(),
        }
    raise ValueError(f"Unsupported response_spectrum baseline method: {method}")


def _apply_filter(
    acceleration_m_s2: np.ndarray,
    dt_s: float,
    config: dict[str, Any],
) -> tuple[np.ndarray, dict[str, Any]]:
    if config.get("enabled") is False or not config.get("band_hz"):
        return acceleration_m_s2.copy(), {"step": "filter", "method": "none"}
    low_hz, high_hz = [float(value) for value in config["band_hz"]]
    sampling_rate_hz = 1.0 / dt_s
    nyquist_hz = 0.5 * sampling_rate_hz
    effective_high_hz = min(high_hz, 0.99 * nyquist_hz)
    filtered = FrequencyFilter(order=int(config.get("order", 4))).bandpass_filter(
        acceleration_m_s2,
        low_hz,
        effective_high_hz,
        sampling_rate_hz,
    )
    return filtered, {
        "step": "filter",
        "method": "butterworth_bandpass",
        "band_hz": [low_hz, effective_high_hz],
        "requested_band_hz": [low_hz, high_hz],
        "order": int(config.get("order", 4)),
    }


def _summary(record: AccelerogramRecord) -> dict[str, Any]:
    return {
        "npts": record.npts,
        "dt_s": record.dt_s,
        "duration_s": float(record.time_s[-1] - record.time_s[0]),
        "pga_m_s2": record.pga_m_s2,
        "pga_g": record.pga_g,
    }
