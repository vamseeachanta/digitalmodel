"""Outer-fibre axial stress range along the riser (owner decision W501; API RP 16Q 1993 Table 3.1 note [5]).

The channel is read from a solved (reopened) OrcaFlex result, so it can be added to results already extracted while
the ``.sim`` file is kept. OrcaFlex ``ZZ stress`` at the outer fibre is the axial wall stress plus the bending stress
at circumferential position ``theta``. For each ``theta`` a range graph over the main stage gives the minimum and
maximum along the line; the double-amplitude range at an arc length is the largest ``max - min`` over the ``theta``
values (24 positions, 15 deg apart: the bending part is under-read by at most 1 - cos 7.5 deg = 0.9 %).

The result is a supplementary document (schema ``riser-w5-stress-range/1``) keyed to the ledger record it was
read from; :func:`merge_stress_range` adds ``range_graphs.<line>.zz_range_max`` (kPa) to the W5 channel document.

W510 (owner decisions 2026-10-07) adds the significant range (schema ``riser-w5-stress-range/2``,
:func:`extract_significant`): ``SIGNIFICANT_DEFINITION`` - H1/3, the mean of the highest one-third of the rainflow
ranges of the ``ZZ stress`` time history, half cycles weighted 0.5 (counter:
``global_model.fatigue_channels.half_cycles``). It is taken at every range-graph result point and the largest value
over ``theta`` is kept (``zz_range_sig``) beside the history span max - min (``zz_range_max``, the screening value).
:func:`coupling_positions` and :func:`classify_stations` map the result points to riser couplings, joint body, the
excluded tension-ring / telescopic-joint section and the first-pup fatigue hot spot (W7), for CR-10 in
:mod:`checks`.
"""

from __future__ import annotations

import math
from typing import Any, Iterable, Mapping, Sequence

SCHEMA = "riser-w5-stress-range/1"
SCHEMA_SIG = "riser-w5-stress-range/2"
VAR = "ZZ stress"
THETAS_DEG = tuple(float(x) for x in range(0, 360, 15))
LINES = ("Riser",)
AXIS = "arc_zz_m"
KEY = "zz_range_max"
KEY_SIG = "zz_range_sig"
SIGNIFICANT_DEFINITION = ("API RP 16Q (1993) 3.3.2 significant range interpreted as H1/3 of rainflow ranges - "
                          "owner decision R03, 2026-10-07")
JOINT_LENGTH_M = 27.432
STATION_KINDS = ("excluded", "hot_spot", "coupling", "body")
_ARC_TOL = 1e-6


def stress_range_limit_ksi(saf: float) -> float:
    """API RP 16Q 1993 Table 3.1 note [5]: 10 ksi if SAF <= 1.5, else 15 / SAF ksi."""
    if saf <= 0.0:
        raise ValueError(f"SAF must be positive, got {saf}")
    return 10.0 if saf <= 1.5 else 15.0 / saf


def range_over_theta(envs: Mapping[float, tuple[Sequence[float], Sequence[float]]]) -> tuple[list[float], list[float]]:
    """``envs``: theta -> (minimum, maximum) along one arc-length grid. Returns (range, theta of the range) per arc."""
    items = sorted(envs.items())
    n = {len(lo) for _, (lo, hi) in items} | {len(hi) for _, (lo, hi) in items}
    if len(n) != 1:
        raise ValueError("range graphs of the theta positions are on different arc-length grids")
    rng: list[float] = []
    th: list[float] = []
    for i in range(n.pop()):
        best, at = None, None
        for theta, (lo, hi) in items:
            r = float(hi[i]) - float(lo[i])
            if best is None or r > best:
                best, at = r, float(theta)
        rng.append(best)
        th.append(at)
    return rng, th


def extract(model, ofx, *, lines: Iterable[str] = LINES, thetas: Iterable[float] = THETAS_DEG,
            period=None) -> dict[str, Any]:
    """Stress range along ``lines`` of a reopened dynamic result (main stage, build-up excluded)."""
    period = period if period is not None else ofx.Period(1)
    out: dict[str, Any] = {"schema": SCHEMA, "variable": VAR, "radial_position": "outer", "units": {KEY: "kPa"},
                           "thetas_deg": list(thetas), "lines": {}}
    for ln in lines:
        line = model[ln]
        envs, arc = {}, None
        for th in thetas:
            rg = line.RangeGraph(VAR, period, ofx.oeLine(RadialPos=ofx.rpOuter, Theta=float(th)))
            x = [float(v) for v in rg.X]
            if arc is None:
                arc = x
            elif len(x) != len(arc):
                raise ValueError(f"{ln}: range-graph grid changed with theta")
            envs[float(th)] = ([float(v) for v in rg.Min], [float(v) for v in rg.Max])
        rng, at = range_over_theta(envs)
        out["lines"][ln] = {"arc_m": arc, KEY: rng, "theta_deg": at}
    return out


def merge_stress_range(doc: dict[str, Any], supp: dict[str, Any]) -> dict[str, Any]:
    """Add the stress range of ``supp`` (schema /1 or /2) to the W5 channel document ``doc`` (``channels.w5``)
    in place; a /2 document also adds the significant range ``zz_range_sig`` on the same axis."""
    if supp.get("schema") not in (SCHEMA, SCHEMA_SIG):
        raise ValueError(f"unsupported stress-range schema {supp.get('schema')!r}")
    rgs = doc["channels"]["w5"].setdefault("range_graphs", {})
    for ln, s in supp["lines"].items():
        rg = rgs.setdefault(ln, {})
        rg[AXIS] = list(s["arc_m"])
        keys = [KEY] + ([KEY_SIG] if supp["schema"] == SCHEMA_SIG and KEY_SIG in s else [])
        for k in keys:
            if len(s[k]) != len(s["arc_m"]):
                raise ValueError(f"{ln}: {k} has {len(s[k])} values for {len(s['arc_m'])} arc lengths")
            rg[k] = list(s[k])
            rg.setdefault("axis_of", {})[k] = AXIS
    return doc


# ---------------------------------------------------------------------------------------------- W510: significant range


def turning_points(series) -> "Any":
    """The reversals of ``series`` with both ends (consecutive equal values merged). Rainflow counting of the
    turning points equals counting of the full history, at a fraction of the cost."""
    import numpy as np

    x = np.asarray(series, dtype=float)
    if x.size < 3:
        return x
    keep = np.r_[True, x[1:] != x[:-1]]
    x = x[keep]
    if x.size < 3:
        return x
    d = np.diff(x)
    rev = np.r_[True, d[1:] * d[:-1] < 0.0, True]
    return x[rev]


def significant_range(series=None, *, half_cycle_ranges: Sequence[float] | None = None) -> float:
    """H1/3 of the rainflow ranges (``SIGNIFICANT_DEFINITION``): each half cycle weighs 0.5 cycle; the ranges are
    taken in descending order until one-third of the total weight, the last one in part, and their weighted mean
    is returned. Never exceeds max - min (the largest half cycle equals the span); equals 2A on a regular sine."""
    if half_cycle_ranges is None:
        from digitalmodel.drilling_riser.global_model.fatigue_channels import half_cycles

        half_cycle_ranges = half_cycles(turning_points(series))
    r = sorted((float(v) for v in half_cycle_ranges), reverse=True)
    if not r:
        return 0.0
    third = 0.5 * len(r) / 3.0  # cycles
    acc, total = 0.0, 0.0
    for v in r:
        w = min(0.5, third - acc)
        if w <= 0.0:
            break
        total += w * v
        acc += w
    return total / third


def theta_histories(h0, h90, h180, *, thetas: Iterable[float] = THETAS_DEG) -> dict[float, Any]:
    """Outer-fibre axial stress at each ``theta`` from the histories at 0, 90 and 180 deg: the axial wall stress
    plus the bending stress is a + b cos(theta) + c sin(theta), with a = (s0 + s180) / 2, b = (s0 - s180) / 2,
    c = s90 - a (exact for axial-plus-bending wall stress; :func:`extract_significant` checks it at 45 deg)."""
    import numpy as np

    s0, s90, s180 = (np.asarray(h, dtype=float) for h in (h0, h90, h180))
    a = 0.5 * (s0 + s180)
    b = 0.5 * (s0 - s180)
    c = s90 - a
    return {float(th): a + b * math.cos(math.radians(th)) + c * math.sin(math.radians(th)) for th in thetas}


def significant_over_theta(histories: Mapping[float, Any]) -> tuple[float, float, float]:
    """(largest significant range over ``theta``, its ``theta``, largest span max - min over ``theta``)."""
    best, at, span = None, None, 0.0
    for th, h in sorted(histories.items()):
        tp = turning_points(h)
        s = significant_range(tp)
        if tp.size:
            span = max(span, float(tp.max() - tp.min()))
        if best is None or s > best:
            best, at = s, float(th)
    return float(best or 0.0), at, span


def coupling_positions(sections_m: Sequence[float], joint_length_m: float = JOINT_LENGTH_M, *,
                       tol_m: float = 1e-3) -> list[float]:
    """Arc lengths (from End A) of the riser couplings: every section boundary and End B, plus k x joint length
    inside a section longer than one joint. Such a section must hold a whole number of joints (``ValueError``)."""
    out, arc = [], 0.0
    for i, length in enumerate(float(v) for v in sections_m):
        if length > joint_length_m + tol_m:
            n = length / joint_length_m
            if abs(n - round(n)) * joint_length_m > tol_m:
                raise ValueError(f"section {i} ({length} m) is longer than a joint and not a whole number of "
                                 f"{joint_length_m} m joints")
            out.extend(arc + k * joint_length_m for k in range(1, int(round(n))))
        arc += length
        out.append(arc)
    return sorted(out)


def classify_stations(arcs: Sequence[float], *, sections_m: Sequence[float], joint_length_m: float = JOINT_LENGTH_M,
                      exclude_below_m: float, hot_spot_m: float | None = None,
                      hot_spot_tol_m: float = 1.0) -> list[str]:
    """Kind of each result point for CR-10 (``STATION_KINDS``), in this order of precedence:

    ``excluded`` - arc below ``exclude_below_m`` (the tension-ring / telescopic-joint section);
    ``hot_spot`` - the result point nearest ``hot_spot_m`` (the first-pup point, a W7 fatigue hot spot reported
    beside CR-10, never governing; ``ValueError`` if no point lies within ``hot_spot_tol_m``);
    ``coupling`` - the nearest result point on each side of a coupling (:func:`coupling_positions`);
    ``body`` - every other point on the joint body.
    """
    a = [float(x) for x in arcs]
    kinds = ["body"] * len(a)
    for i, x in enumerate(a):
        if x < exclude_below_m - _ARC_TOL:
            kinds[i] = "excluded"
    if hot_spot_m is not None:
        cand = [i for i in range(len(a)) if kinds[i] != "excluded"]
        near = min(cand, key=lambda i: abs(a[i] - hot_spot_m)) if cand else None
        if near is None or abs(a[near] - hot_spot_m) > hot_spot_tol_m:
            raise ValueError(f"no result point within {hot_spot_tol_m} m of the hot spot at {hot_spot_m} m")
        kinds[near] = "hot_spot"
    for c in coupling_positions(sections_m, joint_length_m):
        below = [i for i, x in enumerate(a) if x <= c + _ARC_TOL]
        above = [i for i, x in enumerate(a) if x >= c - _ARC_TOL]
        for idx in ((max(below, key=lambda i: a[i]),) if below else ()) + \
                   ((min(above, key=lambda i: a[i]),) if above else ()):
            if kinds[idx] == "body":
                kinds[idx] = "coupling"
    return kinds


def extract_significant(model, ofx, *, lines: Iterable[str] = LINES, thetas: Iterable[float] = THETAS_DEG,
                        period=None, check_every: int = 10, check_rtol: float = 1e-6) -> dict[str, Any]:
    """Significant and max - min outer-fibre stress range at every range-graph result point of ``lines`` of a
    reopened dynamic result (main stage, build-up excluded; schema ``riser-w5-stress-range/2``).

    Per point, the ``ZZ stress`` histories at 0, 90 and 180 deg give every ``theta`` (:func:`theta_histories`);
    every ``check_every``-th point also reads 45 deg directly, and the largest relative deviation from the
    reconstruction is recorded (``theta_check_max_rel``; above ``check_rtol`` the document says so)."""
    import numpy as np

    thetas = [float(t) for t in thetas]
    period = period if period is not None else ofx.Period(1)
    out: dict[str, Any] = {"schema": SCHEMA_SIG, "variable": VAR, "radial_position": "outer",
                           "definition": SIGNIFICANT_DEFINITION,
                           "counter": "global_model.fatigue_channels.half_cycles (turning points)",
                           "units": {KEY: "kPa", KEY_SIG: "kPa"}, "thetas_deg": thetas, "lines": {}}
    worst_dev = 0.0
    for ln in lines:
        line = model[ln]
        arc = [float(v) for v in line.RangeGraph(VAR, period, ofx.oeLine(RadialPos=ofx.rpOuter, Theta=0.0)).X]
        sig, th_sig, span, ratio = [], [], [], []
        for i, x in enumerate(arc):
            want = [0.0, 90.0, 180.0] + ([45.0] if check_every and i % check_every == 0 else [])
            specs = [ofx.TimeHistorySpecification(line, VAR, ofx.oeLine(ArcLength=x, RadialPos=ofx.rpOuter,
                                                                            Theta=t)) for t in want]
            h = np.asarray(ofx.GetMultipleTimeHistories(specs, period), dtype=float)
            hs = theta_histories(h[:, 0], h[:, 1], h[:, 2], thetas=thetas)
            if len(want) == 4:
                rec = theta_histories(h[:, 0], h[:, 1], h[:, 2], thetas=[45.0])[45.0]
                scale = max(float(np.max(np.abs(h[:, 3]))), 1.0)
                worst_dev = max(worst_dev, float(np.max(np.abs(rec - h[:, 3]))) / scale)
            s, t, sp = significant_over_theta(hs)
            sig.append(s)
            th_sig.append(t)
            span.append(sp)
            ratio.append(sp / s if s > 0 else None)
        out["lines"][ln] = {"arc_m": arc, KEY_SIG: sig, "theta_sig_deg": th_sig, KEY: span,
                            "span_over_sig": ratio}
    out["theta_check_max_rel"] = worst_dev
    out["theta_check_ok"] = worst_dev <= check_rtol
    return out


__all__ = ["JOINT_LENGTH_M", "KEY", "KEY_SIG", "LINES", "SCHEMA", "SCHEMA_SIG", "SIGNIFICANT_DEFINITION",
           "STATION_KINDS", "THETAS_DEG", "classify_stations", "coupling_positions", "extract", "extract_significant",
           "merge_stress_range", "range_over_theta", "significant_over_theta", "significant_range",
           "stress_range_limit_ksi", "theta_histories", "turning_points"]
