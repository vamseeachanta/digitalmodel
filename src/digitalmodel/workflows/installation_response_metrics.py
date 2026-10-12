"""Signed-tension diagnostics and conditional, sampled sea-state envelopes.

Negative effective tension is retained as a model response. Neither compression
magnitude nor low-tension duration is a measurement of geometric sling slack.
Criteria supplied to this module require independent engineering qualification.
"""
from __future__ import annotations

import numpy as np


def _signals(times, values):
    t, y = np.asarray(times, dtype=float), np.asarray(values, dtype=float)
    if t.ndim != 1 or y.ndim != 1 or len(t) != len(y) or len(t) < 2:
        raise ValueError("Aligned one-dimensional histories with at least two samples required")
    if not np.isfinite(t).all() or not np.isfinite(y).all():
        raise ValueError("Finite samples required")
    dt = np.diff(t)
    if not (dt > 0).all() or not np.allclose(dt, dt[0], rtol=1e-7, atol=1e-10):
        raise ValueError("Strictly increasing uniformly spaced times required")
    return t, y


def _intervals(t, y, threshold, inclusive):
    inside = y <= threshold if inclusive else y < threshold
    intervals = []
    for i in range(len(t) - 1):
        a, b = bool(inside[i]), bool(inside[i + 1])
        if not a and not b:
            continue
        start, end = float(t[i]), float(t[i + 1])
        if a != b:
            crossing = start + (end - start) * (threshold - y[i]) / (y[i + 1] - y[i])
            start, end = (start, float(crossing)) if a else (float(crossing), end)
        if end <= start:
            continue
        if intervals and intervals[-1][1] == start and inside[i]:
            intervals[-1][1] = end
        else:
            intervals.append([start, end])
    return intervals


def _event(t, y, start, end, window):
    right_censored = bool(end == t[-1])
    stop = min(end + window, float(t[-1]))
    event = {"start_s": start, "end_s": end, "duration_s": end - start,
             "left_censored": bool(start == t[0]), "right_censored": right_censored,
             "retension_peak": None, "retension_peak_time_s": None,
             "retension_window_censored": bool(end + window > t[-1])}
    if not right_censored:
        points = np.concatenate(([end], t[(t > end) & (t < stop)], [stop]))
        vals = np.interp(points, t, y)
        idx = int(np.argmax(vals))
        event["retension_peak"] = float(max(0, vals[idx]))
        event["retension_peak_time_s"] = float(points[idx]) if vals[idx] >= 0 else None
    return event


def _event_summary(t, y, threshold, inclusive, window):
    events = [_event(t, y, start, end, window)
              for start, end in _intervals(t, y, threshold, inclusive)]
    return {"threshold": threshold, "comparison": "<=" if inclusive else "<",
            "event_count": len(events), "events": events,
            "total_duration_s": sum(e["duration_s"] for e in events),
            "maximum_duration_s": max((e["duration_s"] for e in events), default=0.0)}


def tension_event_metrics(times, tension, *, units, near_zero_threshold=0.0,
                          retension_window_s=2.0):
    """Interpolate crossings linearly; quantify positive-duration events only.

    ``near_zero_threshold`` uses the supplied tension units. Peaks are observed
    post-exit maxima over a fixed window, not snap-load amplification factors.
    End-of-record events and truncated peak windows are marked censored.
    """
    t, y = _signals(times, tension)
    if not isinstance(units, str) or not units.strip():
        raise ValueError("Explicit tension units required")
    threshold, window = float(near_zero_threshold), float(retension_window_s)
    if not np.isfinite([threshold, window]).all() or threshold < 0 or window <= 0:
        raise ValueError("Finite nonnegative threshold and positive peak window required")
    return {"units": units, "time_units": "s", "retension_window_s": window,
            "minimum_signed_tension": float(y.min()), "maximum_signed_tension": float(y.max()),
            "compression": _event_summary(t, y, 0.0, False, window),
            "low_tension": _event_summary(t, y, threshold, True, window),
            "interpretation": "Signed tension diagnostics; geometric slack is not established."}


def _criterion(metric, limit):
    if not isinstance(metric, dict) or not isinstance(limit, dict):
        return "NOT EVALUATED"
    unit = limit.get("units")
    if not isinstance(unit, str) or not unit.strip() or metric.get("units") != unit:
        return "NOT EVALUATED"
    try:
        value, maximum = float(metric["value"]), float(limit["maximum"])
    except (KeyError, TypeError, ValueError):
        return "NOT EVALUATED"
    if not np.isfinite([value, maximum]).all():
        return "NOT EVALUATED"
    return "PASS" if value <= maximum else "FAIL"


def classify_case(metrics, limits, *, run_status="completed"):
    """Classify against caller-supplied maxima; PASS is conditional on these only.

    Supply every required criterion in ``limits``; missing/unqualified maxima
    may be represented by None. No automatic unit conversion is performed.
    """
    if run_status in ("missing", "failed"):
        return {"status": run_status.upper(), "criteria": {}, "basis": "Run unavailable"}
    if run_status != "completed":
        raise ValueError("run_status must be completed, missing or failed")
    if not isinstance(metrics, dict) or not isinstance(limits, dict):
        raise ValueError("Metrics and limits must be mappings")
    criteria = {key: _criterion(metrics.get(key), limit) for key, limit in limits.items()}
    status = "PASS"
    if not criteria or "NOT EVALUATED" in criteria.values():
        status = "NOT EVALUATED"
    if "FAIL" in criteria.values():
        status = "FAIL"
    return {"status": status, "criteria": criteria,
            "basis": "Conditional on supplied criteria; not operational approval."}


def _grid_row(tp, hs, lookup):
    states = [lookup.get((h, tp), "MISSING") for h in hs]
    bands = []
    for i, status in enumerate(states):
        if status == "PASS":
            if i and states[i - 1] == "PASS":
                bands[-1][1] = hs[i]
            else:
                bands.append([hs[i], hs[i]])
    first_upper = bands[0][1] if states[0] == "PASS" else None
    bracket = None
    if first_upper is not None:
        index = hs.index(first_upper)
        if index + 1 < len(hs) and states[index + 1] == "FAIL":
            bracket = [first_upper, hs[index + 1]]
    fail_before_pass = any(s == "FAIL" and "PASS" in states[i + 1:]
                           for i, s in enumerate(states))
    return {"tp_s": tp, "hs_m": hs, "statuses": states, "pass_bands_hs_m": bands,
            "first_pass_band_upper_hs_m": first_upper, "boundary_bracket_hs_m": bracket,
            "upper_censored": states[-1] == "PASS", "disconnected": len(bands) > 1,
            "nonmonotonic": fail_before_pass,
            "incomplete": any(s not in ("PASS", "FAIL") for s in states)}


def summarize_grid(cases):
    """Describe sampled cells without extrapolating an operating envelope.

    Missing Cartesian cells are explicit. Include rows for expected unsolved
    Hs/Tp coordinates as MISSING so entirely absent coordinates are represented.
    Pass bands are sample adjacency only; gaps between samples are untested.
    """
    lookup = {}
    allowed = {"PASS", "FAIL", "NOT EVALUATED", "FAILED", "MISSING", "RUNNING"}
    for case in cases:
        hs, tp, status = float(case["hs_m"]), float(case["tp_s"]), case["status"]
        if not np.isfinite([hs, tp]).all() or hs < 0 or tp <= 0 or status not in allowed:
            raise ValueError("Finite physical Hs/Tp and recognized status required")
        if (hs, tp) in lookup:
            raise ValueError("Duplicate sea-state coordinates; aggregate seeds explicitly first")
        lookup[hs, tp] = status
    hs_values = sorted({key[0] for key in lookup})
    periods = sorted({key[1] for key in lookup})
    return {"rows": [_grid_row(tp, hs_values, lookup) for tp in periods],
            "basis": "Sampled conditional results only; no interpolation or operational approval."}
