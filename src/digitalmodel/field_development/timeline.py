# ABOUTME: Project timeline benchmark analysis — inter-phase durations and schedule distributions.
# ABOUTME: Issue #2060 — extracted to field_development per Option B operator decision.
"""
digitalmodel.field_development.timeline
========================================

Derives schedule benchmarks from :class:`SubseaProject` timeline fields
(``year_concept``, ``year_feed``, ``year_fid``, ``year_first_oil``).

Three analysis functions are provided:

- :func:`timeline_duration_stats`        — P10/P50/P90 across all projects
- :func:`duration_stats_by_concept_type` — same, split by concept type
- :func:`schedule_distributions`         — per-phase P10/P50/P90 distributions

Empty phase populations have integer ``count=0`` and ``None`` statistics.
A measured same-year transition instead contributes a duration of 0.0 years.

Quantiles retain the default ``statistics.quantiles`` exclusive convention:
the position for probability p is (n + 1) * p, with linear extrapolation
outside the observed sample for small populations. For synthetic durations
[1, 3] years, positions 0.3, 1.5 and 2.7 give P10=-0.4, P50=2 and P90=4.4
by 1 + (position - 1) * (3 - 1); the mean is (1 + 3) / 2 = 2 years.
The negative percentile is a software convention, not a physical negative
duration. A single observation is returned unchanged for every percentile.

Usage
-----
>>> from digitalmodel.field_development.timeline import (
...     timeline_duration_stats,
...     duration_stats_by_concept_type,
...     schedule_distributions,
... )
>>> stats = timeline_duration_stats(projects)
>>> by_type = duration_stats_by_concept_type(projects)
>>> dists = schedule_distributions(projects)
"""

from __future__ import annotations

from statistics import mean, quantiles
from typing import TypedDict

from .benchmarks import SubseaProject

__all__ = [
    "timeline_duration_stats",
    "duration_stats_by_concept_type",
    "schedule_distributions",
]


class SchedulePercentiles(TypedDict):
    """Percentiles in years, or None when the phase has no observations."""

    p10: float | None
    p50: float | None
    p90: float | None


class DurationStats(SchedulePercentiles):
    """Integer observation count and optional duration statistics in years."""

    count: int
    mean: float | None


# Phase pairs: (label, from_attr, to_attr)
_PHASE_PAIRS: tuple[tuple[str, str, str], ...] = (
    ("concept_to_feed",      "year_concept",   "year_feed"),
    ("feed_to_fid",          "year_feed",       "year_fid"),
    ("fid_to_first_oil",     "year_fid",        "year_first_oil"),
    ("concept_to_first_oil", "year_concept",    "year_first_oil"),
)


def _duration_values(
    projects: list[SubseaProject], from_attr: str, to_attr: str
) -> list[float]:
    """Extract non-None inter-phase durations (years) from a project list.

    Retrograde timelines (``to_yr < from_yr``) are silently excluded.
    Zero-duration phases (``to_yr == from_yr``) are **included** as 0.0 —
    same-year phase transitions are valid data (e.g. fast-track tiebacks).
    """
    result = []
    for p in projects:
        from_yr = getattr(p, from_attr)
        to_yr = getattr(p, to_attr)
        if from_yr is not None and to_yr is not None and to_yr >= from_yr:
            result.append(float(to_yr - from_yr))
    return result


def _stats(values: list[float]) -> DurationStats:
    """Return count, mean, P10, P50, P90 for a list of duration values."""
    n = len(values)
    if n == 0:
        return {"count": 0, "mean": None, "p10": None, "p50": None, "p90": None}
    if n == 1:
        v = values[0]
        return {"count": 1, "mean": v, "p10": v, "p50": v, "p90": v}
    qs = quantiles(values, n=10)
    return {
        "count": n,
        "mean": mean(values),
        "p10": qs[0],   # 10th percentile
        "p50": qs[4],   # 50th percentile
        "p90": qs[8],   # 90th percentile
    }


def timeline_duration_stats(
    projects: list[SubseaProject],
) -> dict[str, DurationStats]:
    """Compute P10/P50/P90 inter-phase duration stats across all projects.

    Parameters
    ----------
    projects : list[SubseaProject]

    Returns
    -------
    dict[str, DurationStats]
        Keys are phase-pair labels (e.g. ``"concept_to_feed"``).
        Values contain ``count``, ``mean``, ``p10``, ``p50``, ``p90`` (years).
        Count is an integer; mean and percentiles are None for empty phases.
        Nonempty percentiles follow the exclusive convention described above.
    """
    return {
        label: _stats(_duration_values(projects, from_attr, to_attr))
        for label, from_attr, to_attr in _PHASE_PAIRS
    }


def duration_stats_by_concept_type(
    projects: list[SubseaProject],
) -> dict[str, dict[str, DurationStats]]:
    """Compute inter-phase duration stats grouped by concept type.

    Projects missing ``concept_type`` are excluded.

    Parameters
    ----------
    projects : list[SubseaProject]

    Returns
    -------
    dict[str, dict[str, DurationStats]]
        Outer key is concept type (e.g. ``"FPSO"``).
        Inner dict mirrors :func:`timeline_duration_stats` structure.
    """
    groups: dict[str, list[SubseaProject]] = {}
    for p in projects:
        if p.concept_type is None:
            continue
        groups.setdefault(p.concept_type, []).append(p)

    return {
        ctype: {
            label: _stats(_duration_values(group, from_attr, to_attr))
            for label, from_attr, to_attr in _PHASE_PAIRS
        }
        for ctype, group in sorted(groups.items())
    }


def schedule_distributions(
    projects: list[SubseaProject],
) -> dict[str, SchedulePercentiles]:
    """P10/P50/P90 schedule distributions for each inter-phase pair.

    Equivalent to :func:`timeline_duration_stats` but returns only the
    percentile columns — suited for plotting schedule uncertainty bands.

    Parameters
    ----------
    projects : list[SubseaProject]

    Returns
    -------
    dict[str, SchedulePercentiles]
        Keys are phase-pair labels.
        Values contain ``p10``, ``p50``, ``p90`` (years).
        Empty phases retain these keys with None values. Nonempty percentiles
        follow the exclusive convention described above.
    """
    result: dict[str, SchedulePercentiles] = {}
    for label, from_attr, to_attr in _PHASE_PAIRS:
        full = _stats(_duration_values(projects, from_attr, to_attr))
        result[label] = {"p10": full["p10"], "p50": full["p50"], "p90": full["p90"]}
    return result
