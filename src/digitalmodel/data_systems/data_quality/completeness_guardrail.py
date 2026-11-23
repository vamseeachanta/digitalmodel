"""Reporting-lag / partial-year completeness guardrail (issue #903, epic #890).

Purpose
-------
Several BSEE-derived sources publish on a *lag*: the most recent periods keep
receiving late filings for months (WAR) or are still mid-year (OGOR annual).
The raw counts for those trailing periods are therefore *artificially low* and
must NOT be read as a real decline. Concretely (verified by hand off
``mv_war_main_prop.txt`` in the issue):

================  =====================  ==============================
Activity year     Total WAR records      Workover/intervention (WO+REC+CHZ)
================  =====================  ==============================
2000              17,548                 2,864
2019               2,113                   162
2023               1,203                    20
2024                 598                     4
2025                 243                     0
================  =====================  ==============================

The monotonic collapse hits *every* activity type, so it is a **filing
artifact**, not a real cessation. OGOR-A is analogous: the current calendar
year is a *partial* year (e.g. 2025 has ~58k rows vs 100k+ for every full
year), so its total is structurally low.

What this module does
---------------------
Pure logic over a *passed-in* date-indexed series (no live fetch). Given the
series, an ``as_of`` date and a :class:`SourceConfig` describing the source's
granularity and reporting lag, it:

1. Classifies every period as ``COMPLETE`` / ``PARTIAL`` / ``PROVISIONAL``.
2. Computes a ``safe_window`` -- the date range ending at the latest *complete*
   period -- that downstream trend/forecast code should restrict itself to.
3. Provides :func:`guard_trend`, which truncates a series to the safe window
   before a trend is computed and *warns* (or raises) when the caller tried to
   trend over the incomplete tail.

The two completeness rules
--------------------------
* **WAR reporting-lag rule** (period granularity, e.g. weekly/monthly): a period
  is incomplete while it still lies inside the source's lag window measured back
  from ``as_of``. Periods whose *end* is more than ``lag_days`` before ``as_of``
  are ``COMPLETE``; the trailing ones inside the lag are ``PARTIAL`` (and, deep
  inside, ``PROVISIONAL`` -- the very newest periods that are barely populated).
* **OGOR partial-year rule** (annual granularity): the calendar year containing
  ``as_of`` is ``PARTIAL`` until it has ended *and* the lag has elapsed past its
  Dec-31 boundary. A whole year is only ``COMPLETE`` once
  ``as_of >= year_end + lag``.

Both rules are the same idea -- "is this period's collection window closed yet?"
-- specialised for sub-annual vs annual sources.
"""

from __future__ import annotations

import warnings
from dataclasses import dataclass
from enum import Enum
from typing import Iterable, Optional

import pandas as pd

__all__ = [
    "Completeness",
    "CompletenessReport",
    "IncompleteTrendWarning",
    "SourceConfig",
    "WAR_DEEPWATER_CONFIG",
    "OGOR_ANNUAL_CONFIG",
    "classify_completeness",
    "guard_trend",
]


class Completeness(str, Enum):
    """Per-period completeness classification.

    * ``COMPLETE``    -- collection window closed; safe to trend.
    * ``PARTIAL``     -- still inside the reporting lag / partial year; the count
      is artificially low and must be excluded from trends.
    * ``PROVISIONAL`` -- the newest period(s), barely populated; a strict subset
      of partial. Treated exactly like ``PARTIAL`` for the safe window but
      surfaced separately so dashboards can label "no data yet" vs "still
      filling in".
    """

    COMPLETE = "complete"
    PARTIAL = "partial"
    PROVISIONAL = "provisional"


class IncompleteTrendWarning(UserWarning):
    """Raised/warned when a trend is requested over incomplete trailing periods."""


@dataclass(frozen=True)
class SourceConfig:
    """Describes a lagged data source's completeness behaviour.

    Parameters
    ----------
    name:
        Human-readable source name (used in warning messages).
    granularity:
        A pandas period-style code: ``"W"`` (weekly), ``"M"`` (monthly),
        ``"Q"`` (quarterly) or ``"A"`` (annual / OGOR partial-year rule).
    lag_days:
        How long after a period's *end* late filings keep arriving. A period is
        considered COMPLETE only once ``as_of`` is at least this many days past
        the period end. For WAR this captures the multi-month loading lag; for
        OGOR annual it captures the post-year finalisation lag.
    provisional_days:
        Periods whose end is within this many days *before* ``as_of`` are flagged
        ``PROVISIONAL`` (the freshest, near-empty tail). Must be ``<= lag_days``
        to be meaningful; if ``0`` no period is ever provisional.
    """

    name: str
    granularity: str
    lag_days: int
    provisional_days: int = 0

    def __post_init__(self) -> None:
        if self.granularity not in {"W", "M", "Q", "A", "Y"}:
            raise ValueError(
                f"granularity must be one of W/M/Q/A(/Y), got {self.granularity!r}"
            )
        if self.lag_days < 0:
            raise ValueError("lag_days must be >= 0")
        if self.provisional_days < 0:
            raise ValueError("provisional_days must be >= 0")


# --- Curated configs for the two sources named in the issue --------------------

# BSEE WAR: late filings keep arriving for ~18 months; the issue's table shows
# 2024 and 2025 (and to a lesser extent 2023) still badly under-reported as of
# mid-2026. We treat the source as monthly with an 18-month lag and flag the
# freshest ~6 months as provisional. (Tune ``lag_days`` to the observed
# "reliable-through" cutoff for a given pull.)
WAR_DEEPWATER_CONFIG = SourceConfig(
    name="BSEE WAR (deepwater interventions)",
    granularity="M",
    lag_days=548,  # ~18 months
    provisional_days=183,  # ~6 months
)

# BSEE OGOR-A: annual. The current calendar year is partial until it has ended
# and a short finalisation lag elapses past Dec-31.
OGOR_ANNUAL_CONFIG = SourceConfig(
    name="BSEE OGOR-A (annual production)",
    granularity="A",
    lag_days=120,  # ~4 months post year-end finalisation
    provisional_days=0,
)


@dataclass(frozen=True)
class CompletenessReport:
    """Result of :func:`classify_completeness`.

    Attributes
    ----------
    flags:
        ``Series`` (indexed like the input, aligned to period *end* timestamps)
        of :class:`Completeness` values.
    safe_window:
        ``(start, end)`` timestamps of the contiguous range ending at the latest
        COMPLETE period. ``None`` if no period is complete.
    n_incomplete:
        Count of trailing periods flagged partial or provisional.
    config:
        The :class:`SourceConfig` used.
    as_of:
        The as-of timestamp used.
    """

    flags: pd.Series
    safe_window: Optional[tuple[pd.Timestamp, pd.Timestamp]]
    n_incomplete: int
    config: SourceConfig
    as_of: pd.Timestamp

    @property
    def has_incomplete_tail(self) -> bool:
        return self.n_incomplete > 0

    def safe_mask(self) -> pd.Series:
        """Boolean mask (aligned to ``flags.index``) of COMPLETE periods."""
        return self.flags == Completeness.COMPLETE


def _period_end(ts: pd.Timestamp, granularity: str) -> pd.Timestamp:
    """Return the *end* timestamp of the period containing ``ts``.

    ``"A"`` (annual) is normalised to pandas' current ``"Y"`` alias to avoid the
    ``FutureWarning`` while keeping ``"A"`` accepted in the public API.
    """
    freq = "Y" if granularity == "A" else granularity
    return pd.Period(ts, freq=freq).end_time


def classify_completeness(
    series: pd.Series | Iterable | pd.DataFrame,
    config: SourceConfig,
    as_of: Optional[pd.Timestamp | str] = None,
    *,
    value_column: Optional[str] = None,
) -> CompletenessReport:
    """Classify each period of a date-indexed series as complete/partial/provisional.

    Parameters
    ----------
    series:
        A date-indexed ``pd.Series``, a ``pd.DataFrame`` (use ``value_column`` or
        its first column), or any iterable convertible via ``pd.Series``. The
        index must be (or be coercible to) datetimes -- it is interpreted as the
        period each observation belongs to.
    config:
        :class:`SourceConfig` describing granularity and reporting lag.
    as_of:
        The reference "today". Defaults to ``pd.Timestamp.now().normalize()``.
        A period is COMPLETE only once ``as_of`` is at least ``config.lag_days``
        past that period's end.
    value_column:
        When ``series`` is a DataFrame, which column to use (defaults to the
        first column). The values themselves do not affect classification --
        only the index does -- but carrying them through keeps the API ergonomic.

    Returns
    -------
    CompletenessReport
    """
    as_of_ts = (
        pd.Timestamp(as_of)
        if as_of is not None
        else pd.Timestamp.now().normalize()
    )

    if isinstance(series, pd.DataFrame):
        col = value_column if value_column is not None else series.columns[0]
        s = series[col]
    elif isinstance(series, pd.Series):
        s = series
    else:
        s = pd.Series(list(series))

    if not isinstance(s.index, pd.DatetimeIndex):
        s = s.copy()
        s.index = pd.to_datetime(s.index)

    s = s.sort_index()

    flags: dict[pd.Timestamp, Completeness] = {}
    for idx in s.index:
        end = _period_end(idx, config.granularity)
        days_since_end = (as_of_ts - end).days
        if days_since_end >= config.lag_days:
            flags[idx] = Completeness.COMPLETE
        else:
            # Inside the lag window -> incomplete. Distinguish the freshest tail.
            days_to_end = (end - as_of_ts).days
            # period whose end is within provisional_days of as_of (either side
            # of as_of, i.e. just closed or not yet closed) is provisional.
            if config.provisional_days and days_to_end >= -config.provisional_days:
                flags[idx] = Completeness.PROVISIONAL
            else:
                flags[idx] = Completeness.PARTIAL

    flag_series = pd.Series(
        [flags[i] for i in s.index], index=s.index, name="completeness"
    )

    complete_idx = flag_series.index[flag_series == Completeness.COMPLETE]
    if len(complete_idx) == 0:
        safe_window = None
    else:
        safe_window = (complete_idx.min(), complete_idx.max())

    n_incomplete = int((flag_series != Completeness.COMPLETE).sum())

    return CompletenessReport(
        flags=flag_series,
        safe_window=safe_window,
        n_incomplete=n_incomplete,
        config=config,
        as_of=as_of_ts,
    )


def guard_trend(
    series: pd.Series | pd.DataFrame,
    config: SourceConfig,
    as_of: Optional[pd.Timestamp | str] = None,
    *,
    value_column: Optional[str] = None,
    on_incomplete: str = "warn",
) -> tuple[pd.Series, CompletenessReport]:
    """Truncate a series to its safe (complete) window before trending.

    Use this *immediately before* fitting a trend/forecast on a lagged source so
    the artificially low recent tail cannot bias the slope.

    Parameters
    ----------
    series, config, as_of, value_column:
        See :func:`classify_completeness`.
    on_incomplete:
        What to do when the series has an incomplete trailing tail:

        * ``"warn"`` (default) -- emit an :class:`IncompleteTrendWarning` and
          return the series truncated to the safe window.
        * ``"raise"`` -- raise :class:`ValueError` instead of warning.
        * ``"ignore"`` -- silently truncate, no warning.

    Returns
    -------
    (safe_series, report):
        ``safe_series`` is the input restricted to COMPLETE periods (ready to
        trend); ``report`` is the full :class:`CompletenessReport`.

    Raises
    ------
    ValueError
        If ``on_incomplete="raise"`` and an incomplete tail is present, or if no
        period is complete (nothing safe to trend on).
    """
    if on_incomplete not in {"warn", "raise", "ignore"}:
        raise ValueError("on_incomplete must be 'warn', 'raise' or 'ignore'")

    report = classify_completeness(
        series, config, as_of=as_of, value_column=value_column
    )

    # Reconstruct a value series aligned to the report's index for truncation.
    if isinstance(series, pd.DataFrame):
        col = value_column if value_column is not None else series.columns[0]
        values = series[col]
    else:
        values = series
    if not isinstance(values.index, pd.DatetimeIndex):
        values = values.copy()
        values.index = pd.to_datetime(values.index)
    values = values.sort_index()

    if report.safe_window is None:
        raise ValueError(
            f"{config.name}: every period is within the reporting lag "
            f"(as_of={report.as_of.date()}, lag_days={config.lag_days}); "
            "no complete data to trend on."
        )

    safe_mask = report.safe_mask()
    safe_series = values[safe_mask.values]

    if report.has_incomplete_tail:
        msg = (
            f"{config.name}: {report.n_incomplete} trailing period(s) are "
            f"incomplete (reporting lag {config.lag_days}d as of "
            f"{report.as_of.date()}). Excluded from trend; safe window is "
            f"{report.safe_window[0].date()}..{report.safe_window[1].date()}. "
            "Trending over the raw tail would misread reporting lag as decline."
        )
        if on_incomplete == "raise":
            raise ValueError(msg)
        if on_incomplete == "warn":
            warnings.warn(msg, IncompleteTrendWarning, stacklevel=2)

    return safe_series, report
