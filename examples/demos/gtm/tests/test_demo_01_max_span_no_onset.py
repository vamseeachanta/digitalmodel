# ABOUTME: Regression test for #653 — calc_max_allowable_span no-onset sentinel, not 500 m saturation.
# ABOUTME: Pins (a) genuine onset -> finite span, (b) no onset in range -> None + status, not a fake 500.
"""Regression tests for demo_01 ``calc_max_allowable_span`` no-onset handling (#653).

Run with:
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest \\
        examples/demos/gtm/tests/test_demo_01_max_span_no_onset.py -q
"""
from __future__ import annotations

import math

import pytest

import demo_01_dnv_freespan_viv as demo

# A genuine-onset case: cross-flow onset is reached well within the 500 m ceiling.
# (6" pipeline, 0.3 m/s, mid-water) -> ~13.9 m, byte-identical to the pre-fix math.
_ONSET = dict(od_m=0.1683, wt_m=0.0095, v_current=0.3, e_over_d=float("inf"))

# A no-onset case: a stiff 20" pipe in a near-still current. Even at L = 500 m the
# reduced velocity stays below the CF onset target, so onset never occurs in range.
# Pre-fix this silently returned 500.0; post-fix it must be a clear sentinel.
_NO_ONSET = dict(od_m=0.508, wt_m=0.0318, v_current=0.001, e_over_d=float("inf"))


def test_genuine_onset_returns_finite_span():
    """(a) A real onset still returns the correct finite span (math unchanged)."""
    span = demo.calc_max_allowable_span(**_ONSET)
    assert span is not None
    assert math.isfinite(span)
    # Onset is well inside the search range, not pinned at the ceiling.
    assert 1.0 < span < demo.MAX_SPAN_SEARCH_CEILING_M
    assert span == pytest.approx(13.902, abs=0.01)

    status = demo.calc_max_allowable_span_status(**_ONSET)
    assert status["status"] == demo.MAX_SPAN_STATUS_OK
    assert status["span_m"] == pytest.approx(span)


def test_no_onset_returns_sentinel_not_500():
    """(b) No onset in range -> None + 'no_cf_onset_in_range', NOT a saturated 500 m."""
    span = demo.calc_max_allowable_span(**_NO_ONSET)
    # The core bug: must NOT silently saturate at the ceiling.
    assert span != 500.0
    assert span is None

    status = demo.calc_max_allowable_span_status(**_NO_ONSET)
    assert status["span_m"] is None
    assert status["status"] == demo.MAX_SPAN_STATUS_NO_ONSET
    assert status["search_ceiling_m"] == demo.MAX_SPAN_SEARCH_CEILING_M


def test_no_current_returns_sentinel_not_500():
    """v_current <= 0 -> None + 'no_current', NOT the old misleading 500.0."""
    span = demo.calc_max_allowable_span(0.1683, 0.0095, 0.0)
    assert span != 500.0
    assert span is None

    status = demo.calc_max_allowable_span_status(0.1683, 0.0095, 0.0)
    assert status["span_m"] is None
    assert status["status"] == demo.MAX_SPAN_STATUS_NO_CURRENT


def test_raised_ceiling_recovers_onset():
    """Parameterizing the ceiling resolves a previously no-onset case (issue option 2)."""
    # At the default ceiling this case has no onset...
    assert demo.calc_max_allowable_span(**_NO_ONSET) is None
    # ...but raising l_hi brackets a genuine onset above 500 m.
    status = demo.calc_max_allowable_span_status(**_NO_ONSET, l_hi=2000.0)
    assert status["status"] == demo.MAX_SPAN_STATUS_OK
    assert 500.0 < status["span_m"] < 2000.0


def test_run_single_case_emits_none_for_no_onset():
    """run_single_case surfaces None (not 500.0) in max_allowable_span_m for no-onset."""
    row = demo.run_single_case(
        pipe_type="pipeline",
        nominal_size="20in",
        od_m=_NO_ONSET["od_m"],
        wt_m=_NO_ONSET["wt_m"],
        span_m=50.0,
        v_current=_NO_ONSET["v_current"],
        e_over_d=_NO_ONSET["e_over_d"],
    )
    assert row["max_allowable_span_m"] is None
