"""Tests for the reporting-lag / partial-year completeness guardrail (#903).

All data is synthetic and deterministic -- no network, no live fetch. A fixed
``as_of`` is passed to every call so results do not drift with the wall clock.
"""

import warnings

import pandas as pd
import pytest

from digitalmodel.data_systems.data_quality import (
    Completeness,
    IncompleteTrendWarning,
    OGOR_ANNUAL_CONFIG,
    SourceConfig,
    WAR_DEEPWATER_CONFIG,
    classify_completeness,
    guard_trend,
)


# A short-lag config so monthly fixtures stay compact and explicit.
MONTHLY_90D = SourceConfig(
    name="synthetic monthly", granularity="M", lag_days=90, provisional_days=31
)


def _monthly_series(start: str, n: int) -> pd.Series:
    idx = pd.date_range(start=start, periods=n, freq="MS")  # month-start stamps
    return pd.Series(range(n), index=idx, dtype="int64")


# --------------------------------------------------------------------------- #
# WAR reporting-lag rule: recent periods inside the lag are flagged + excluded
# --------------------------------------------------------------------------- #
def test_recent_periods_within_lag_are_partial_and_excluded():
    # 12 months Jan..Dec 2025; as_of = 2026-01-15, lag = 90 days.
    series = _monthly_series("2025-01-01", 12)
    as_of = "2026-01-15"

    report = classify_completeness(series, MONTHLY_90D, as_of=as_of)

    # The last few months (Nov, Dec) are within 90 days of as_of -> incomplete.
    flags = report.flags
    assert flags.loc["2025-12-01"] != Completeness.COMPLETE
    assert flags.loc["2025-11-01"] != Completeness.COMPLETE
    # Early months are well past the lag -> complete.
    assert flags.loc["2025-01-01"] == Completeness.COMPLETE
    assert flags.loc["2025-06-01"] == Completeness.COMPLETE

    # Safe window ends before the incomplete tail.
    assert report.has_incomplete_tail
    assert report.safe_window is not None
    start, end = report.safe_window
    assert start == pd.Timestamp("2025-01-01")
    assert end <= pd.Timestamp("2025-10-01")
    # Every flagged-incomplete period is outside the safe window.
    safe_idx = report.flags.index[report.safe_mask()]
    assert safe_idx.max() < pd.Timestamp("2025-11-01")


def test_freshest_period_is_provisional_not_just_partial():
    series = _monthly_series("2025-01-01", 12)
    report = classify_completeness(series, MONTHLY_90D, as_of="2026-01-15")
    # Dec 2025 ends ~2 weeks before as_of -> within provisional_days (31).
    assert report.flags.loc["2025-12-01"] == Completeness.PROVISIONAL


# --------------------------------------------------------------------------- #
# Complete series: nothing flagged, full window returned
# --------------------------------------------------------------------------- #
def test_fully_complete_series_yields_full_window():
    # All of 2020; as_of years later so everything is well past the lag.
    series = _monthly_series("2020-01-01", 12)
    report = classify_completeness(series, MONTHLY_90D, as_of="2026-01-15")

    assert not report.has_incomplete_tail
    assert report.n_incomplete == 0
    assert (report.flags == Completeness.COMPLETE).all()
    assert report.safe_window == (
        pd.Timestamp("2020-01-01"),
        pd.Timestamp("2020-12-01"),
    )


# --------------------------------------------------------------------------- #
# OGOR partial-year rule: current/most-recent year is excluded
# --------------------------------------------------------------------------- #
def test_ogor_partial_current_year_excluded():
    # Annual series 2021..2025; as_of mid-2025 -> 2025 is a partial year.
    idx = pd.to_datetime([f"{y}-01-01" for y in range(2021, 2026)])
    series = pd.Series([100_000, 110_000, 105_000, 102_000, 58_000], index=idx)
    as_of = "2025-06-30"

    report = classify_completeness(series, OGOR_ANNUAL_CONFIG, as_of=as_of)

    # 2025 still mid-year -> not complete; 2024 ended >120d ago -> complete.
    assert report.flags.loc["2025-01-01"] != Completeness.COMPLETE
    assert report.flags.loc["2024-01-01"] == Completeness.COMPLETE
    assert report.flags.loc["2021-01-01"] == Completeness.COMPLETE

    assert report.safe_window is not None
    _, end = report.safe_window
    assert end == pd.Timestamp("2024-01-01")


def test_ogor_year_becomes_complete_after_finalisation_lag():
    idx = pd.to_datetime([f"{y}-01-01" for y in range(2021, 2026)])
    series = pd.Series([1, 2, 3, 4, 5], index=idx)
    # as_of well past 2025-12-31 + 120 days -> 2025 now complete.
    report = classify_completeness(series, OGOR_ANNUAL_CONFIG, as_of="2026-06-30")
    assert report.flags.loc["2025-01-01"] == Completeness.COMPLETE
    assert not report.has_incomplete_tail


# --------------------------------------------------------------------------- #
# guard_trend: warns / truncates / raises correctly
# --------------------------------------------------------------------------- #
def test_guard_trend_warns_and_truncates():
    series = _monthly_series("2025-01-01", 12)
    with pytest.warns(IncompleteTrendWarning, match="reporting lag"):
        safe, report = guard_trend(series, MONTHLY_90D, as_of="2026-01-15")

    # Truncated to complete periods only.
    assert len(safe) < len(series)
    assert safe.index.max() < pd.Timestamp("2025-11-01")
    assert (report.flags.loc[safe.index] == Completeness.COMPLETE).all()


def test_guard_trend_raise_mode():
    series = _monthly_series("2025-01-01", 12)
    with pytest.raises(ValueError, match="incomplete"):
        guard_trend(series, MONTHLY_90D, as_of="2026-01-15", on_incomplete="raise")


def test_guard_trend_ignore_mode_no_warning():
    series = _monthly_series("2025-01-01", 12)
    with warnings.catch_warnings():
        warnings.simplefilter("error")  # any warning -> test failure
        safe, _ = guard_trend(
            series, MONTHLY_90D, as_of="2026-01-15", on_incomplete="ignore"
        )
    assert len(safe) < len(series)


def test_guard_trend_complete_series_no_warning_full_passthrough():
    series = _monthly_series("2020-01-01", 12)
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        safe, report = guard_trend(series, MONTHLY_90D, as_of="2026-01-15")
    pd.testing.assert_series_equal(safe, series)
    assert not report.has_incomplete_tail


def test_guard_trend_raises_when_nothing_complete():
    # All periods inside the lag -> no safe window at all.
    series = _monthly_series("2026-01-01", 3)
    with pytest.raises(ValueError, match="no complete data"):
        guard_trend(series, MONTHLY_90D, as_of="2026-02-15")


# --------------------------------------------------------------------------- #
# API ergonomics: DataFrame input, non-datetime index, config validation
# --------------------------------------------------------------------------- #
def test_dataframe_input_with_value_column():
    series = _monthly_series("2020-01-01", 6)
    df = pd.DataFrame({"records": series.values}, index=series.index)
    report = classify_completeness(df, MONTHLY_90D, as_of="2026-01-15", value_column="records")
    assert (report.flags == Completeness.COMPLETE).all()


def test_string_index_is_coerced():
    idx = ["2020-01-01", "2020-02-01", "2020-03-01"]
    series = pd.Series([1, 2, 3], index=idx)
    report = classify_completeness(series, MONTHLY_90D, as_of="2026-01-15")
    assert (report.flags == Completeness.COMPLETE).all()


def test_invalid_config_rejected():
    with pytest.raises(ValueError):
        SourceConfig(name="x", granularity="DAILY", lag_days=10)
    with pytest.raises(ValueError):
        SourceConfig(name="x", granularity="M", lag_days=-1)


def test_curated_war_config_flags_recent_years():
    # Mirror the issue's annual collapse as a monthly source pulled mid-2026.
    idx = pd.date_range("2019-01-01", "2025-12-01", freq="MS")
    series = pd.Series(range(len(idx)), index=idx)
    report = classify_completeness(series, WAR_DEEPWATER_CONFIG, as_of="2026-06-01")
    # 18-month lag as of 2026-06 -> roughly everything from ~2024-12 on is incomplete.
    assert report.flags.loc["2025-06-01"] != Completeness.COMPLETE
    assert report.flags.loc["2023-01-01"] == Completeness.COMPLETE
    assert report.has_incomplete_tail
