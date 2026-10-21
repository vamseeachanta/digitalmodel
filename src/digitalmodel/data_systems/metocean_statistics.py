# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Optional statistical analysis of timestamped metocean procurement records.

The backend is metocean-stats 1.2.0, ``stats.general.nb_hours_below_threshold``:
https://github.com/MET-OM/metocean-stats/blob/3f99e50bf08cbf0ec47370c0a8eee349b0226b85/metocean_stats/stats/general.py

Counting basis: NIST/SEMATECH e-Handbook (2012), section 1.3.3.14, frequency:
https://www.itl.nist.gov/div898/handbook/eda/section3/histogra.htm
Time units: BIPM SI Brochure, ninth edition (2019), table 8:
https://www.bipm.org/documents/20126/41483022/SI-Brochure-9.pdf
"""

from collections.abc import Iterable, Mapping
from datetime import datetime, timezone
from math import isfinite
from numbers import Real


def _finite_number(value: object, label: str) -> float:
    """Reject missing, nonnumeric and nonfinite quantities without coercion."""
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{label} must be a finite numeric value")
    try:
        number = float(value)
    except (OverflowError, ValueError) as exc:
        raise ValueError(f"{label} must be a finite numeric value") from exc
    if not isfinite(number):
        raise ValueError(f"{label} must be a finite numeric value")
    return number


def hours_below_threshold(
    records: Iterable[Mapping[str, object]],
    *,
    parameter: str,
    thresholds: Iterable[float],
) -> dict[float, dict[int, float]]:
    """Return observed hours by threshold and UTC sample year.

    Each observation represents one full sample interval, including the last.
    A sample on a year boundary is assigned to its timestamp's UTC year; its
    represented interval is not split across years. Naive datetimes mean UTC.
    This is exposure counting, not a duration-qualified weather window or an
    annual extrapolation. Values and thresholds must use the same caller-defined
    units; no physical limits or unit conversions are inferred.

    Args:
        records: Complete, strictly increasing, regularly spaced records with
            datetime ``timestamp`` entries and finite numeric parameter values.
            Consumes the iterable once and retains the selected series in memory.
        parameter: Record field to analyse, such as ``wave_height``.
        thresholds: Nonempty, distinct finite numeric upper bounds. Equality is
            excluded, matching the backend's strict less-than comparison.

    Returns:
        Mapping of threshold to observed sample year to hours below that threshold.

    Raises:
        ValueError: Invalid values, timestamps, thresholds or sampling interval.
        ImportError: The optional statistical backend is unavailable.
    """
    if not isinstance(parameter, str) or not parameter:
        raise ValueError("parameter must be a nonempty field name")
    bounds = [_finite_number(value, "threshold") for value in thresholds]
    if not bounds or len(set(bounds)) != len(bounds):
        raise ValueError("thresholds must be nonempty and distinct")

    timestamps = []
    values = []
    awareness = set()
    for record in records:
        timestamp = record.get("timestamp")
        # NaT is datetime-like in pandas; a missing time is never an observation.
        if not isinstance(timestamp, datetime) or timestamp != timestamp:
            raise ValueError("timestamp must be a valid datetime")
        aware = timestamp.utcoffset() is not None
        awareness.add(aware)
        if len(awareness) > 1:
            raise ValueError("mixed naive and timezone-aware timestamps")
        timestamps.append(
            timestamp.astimezone(timezone.utc)
            if aware
            else timestamp.replace(tzinfo=timezone.utc)
        )
        values.append(_finite_number(record.get(parameter), parameter))

    # Two timestamps are the minimum needed to infer one sampling interval.
    if len(timestamps) < 2:
        raise ValueError("at least two observations are required")
    intervals = [end - start for start, end in zip(timestamps, timestamps[1:])]
    interval = intervals[0]
    if interval.total_seconds() <= 0 or any(dt != interval for dt in intervals):
        raise ValueError("timestamps must be strictly increasing and regular")

    import pandas as pd

    try:
        from metocean_stats.stats.general import nb_hours_below_threshold
    except ModuleNotFoundError as exc:
        if exc.name != "metocean_stats":
            raise
        raise ImportError(
            "Install digitalmodel[metocean] for metocean statistics"
        ) from exc

    frame = pd.DataFrame({parameter: values}, index=pd.DatetimeIndex(timestamps))
    # H(year, bound) = dt_hours * count(value < bound), delegated to the backend.
    # The backend's seconds/hour conversion is BIPM (2019), table 8, as cited above.
    hours = nb_hours_below_threshold(frame, parameter, bounds)
    years = frame.index.year.unique()
    return {
        bound: {int(year): float(value) for year, value in zip(years, row)}
        for bound, row in zip(bounds, hours)
    }
