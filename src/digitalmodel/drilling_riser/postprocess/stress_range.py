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
_ON_TOL = 1e-4  # a result point this close to a coupling or exclusion boundary is on it (stored-arc rounding)


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
    if supp["schema"] == SCHEMA_SIG and supp.get("theta_check_ok") is not True:
        raise ValueError(f"theta reconstruction check not passed (theta_check_ok {supp.get('theta_check_ok')!r}, "
                         f"max relative deviation {supp.get('theta_check_max_rel')}); the significant range is "
                         "not used")
    keys = [KEY] + ([KEY_SIG] if supp["schema"] == SCHEMA_SIG else [])
    for ln, s in supp["lines"].items():  # validate every line before changing the document
        for k in keys:
            if k not in s:
                raise ValueError(f"{ln}: {supp['schema']} document has no {k}")
            if len(s[k]) != len(s["arc_m"]):
                raise ValueError(f"{ln}: {k} has {len(s[k])} values for {len(s['arc_m'])} arc lengths")
    rgs = doc["channels"]["w5"].setdefault("range_graphs", {})
    for ln, s in supp["lines"].items():
        rg = rgs.setdefault(ln, {})
        rg[AXIS] = list(s["arc_m"])
        if KEY_SIG not in keys and rg.get("axis_of", {}).get(KEY_SIG) == AXIS:  # never leave it on a new axis
            rg.pop(KEY_SIG, None)
            rg["axis_of"].pop(KEY_SIG, None)
        for k in keys:
            rg[k] = list(s[k])
            rg.setdefault("axis_of", {})[k] = AXIS
    return doc


# ---------------------------------------------------------------------------------------------- W510: significant range


def turning_points(series) -> "Any":
    """The reversals of ``series`` with both ends (consecutive equal values merged). Rainflow counting of the
    turning points equals counting of the full history, at a fraction of the cost."""
    import numpy as np

    x = np.asarray(series, dtype=float)
    if x.ndim != 1:
        raise ValueError(f"a stress history must be one-dimensional, got shape {x.shape}")
    if not np.all(np.isfinite(x)):
        raise ValueError("stress history holds non-finite values (NaN or infinity)")
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
    if not histories:
        raise ValueError("no theta position given")
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
                      exclude_below_m: float, exclude_above_m: float | None = None,
                      hot_spot_m: float | None = None, hot_spot_tol_m: float = 1.0,
                      pup_side_couplings_m: Sequence[float] = (), pup_side_tol_m: float = 1.0,
                      coupling_tol_m: float | None = None) -> list[str]:
    """Kind of each result point for CR-10 (``STATION_KINDS``), in this order of precedence:

    ``excluded`` - arc below ``exclude_below_m`` (the tension-ring / telescopic-joint section) or above
    ``exclude_above_m`` (components below the last riser joint, e.g. riser adaptor and flex-joint body);
    ``coupling`` (pup side, W04 owner decision 2026-10-07) - for each coupling in ``pup_side_couplings_m`` whose
    other side is excluded (the outer barrel to first pup coupling), the nearest kept result point is the
    coupling's station, even when it is the hot-spot point (``ValueError`` if the arc is not a coupling or no kept
    point lies within ``pup_side_tol_m``);
    ``hot_spot`` - the result point nearest ``hot_spot_m`` (the first-pup point, a W7 fatigue hot spot reported
    beside CR-10, never governing; ``ValueError`` if no point lies within ``hot_spot_tol_m``);
    ``coupling`` - the nearest result point on each side of a coupling (:func:`coupling_positions`);
    ``body`` - every other point on the joint body.

    A point within ``_ON_TOL`` (0.1 mm, rounding of stored arcs) of a coupling is on it. Two points on an exclusion
    boundary are its two sides and the excluded side's point is excluded. With ``coupling_tol_m`` a coupling inside
    the kept span with no result point within that distance is refused (``ValueError``); a neighbour farther away
    is not taken as the coupling's station.
    """
    a = [float(x) for x in arcs]
    if not all(math.isfinite(x) for x in a) or any(y < x - _ARC_TOL for x, y in zip(a, a[1:])):
        raise ValueError("the arc-length axis is not finite and non-decreasing")
    # End B is taken from the section geometry (below), so the geometry must cover every result point and the
    # exclusion boundary: sections that stop short would make an interior coupling "End B" and drop its upper side
    end_b = float(sum(float(v) for v in sections_m))
    if list(sections_m) and a and a[-1] > end_b + _ON_TOL:
        raise ValueError(f"result point at {a[-1]} m lies beyond End B ({end_b} m from the section geometry)")
    if exclude_above_m is not None and list(sections_m) and exclude_above_m > end_b + _ON_TOL:
        raise ValueError(f"exclude_above_m {exclude_above_m} m lies beyond End B ({end_b} m from the section "
                         f"geometry)")
    kinds = ["body"] * len(a)
    for i, x in enumerate(a):
        if x < exclude_below_m - _ON_TOL or (exclude_above_m is not None and x > exclude_above_m + _ON_TOL):
            kinds[i] = "excluded"
    # two result points on an exclusion boundary are its two sides (arcs ascend from End A): the first on
    # exclude_below_m is the excluded section's, the last on exclude_above_m is the component below the last joint
    on_lo = [i for i, x in enumerate(a) if abs(x - exclude_below_m) <= _ON_TOL]
    if len(on_lo) > 1:
        kinds[on_lo[0]] = "excluded"
    if exclude_above_m is not None:
        on_hi = [i for i, x in enumerate(a) if abs(x - exclude_above_m) <= _ON_TOL]
        if len(on_hi) > 1:
            kinds[on_hi[-1]] = "excluded"
    if hot_spot_m is not None:
        cand = [i for i in range(len(a)) if kinds[i] != "excluded"]
        near = min(cand, key=lambda i: abs(a[i] - hot_spot_m)) if cand else None
        if near is None or abs(a[near] - hot_spot_m) > hot_spot_tol_m:
            raise ValueError(f"no result point within {hot_spot_tol_m} m of the hot spot at {hot_spot_m} m")
        kinds[near] = "hot_spot"
    couplings = coupling_positions(sections_m, joint_length_m)
    pup_side = [float(v) for v in pup_side_couplings_m]
    if hot_spot_m is not None:
        on_c = [c for c in couplings if abs(a[near] - c) <= _ON_TOL and not any(abs(c - p) <= _ON_TOL
                                                                                for p in pup_side)]
        if on_c:  # the hot spot must not take a coupling's own station
            raise ValueError(f"the hot-spot point at {a[near]} m lies on the coupling at {on_c[0]} m")
    # upper end of the kept span: the exclusion boundary, else End B of the line (from the section geometry)
    top = exclude_above_m if exclude_above_m is not None else end_b
    for c in couplings:
        # every point on the coupling (duplicates included, one per side of a section boundary); else the nearest
        # point (all duplicates at that arc) on each side
        idxs = [i for i, x in enumerate(a) if abs(x - c) <= _ON_TOL]
        if not idxs and coupling_tol_m is None:
            below = [x for x in a if x < c]
            above = [x for x in a if x > c]
            near_x = ([max(below)] if below else []) + ([min(above)] if above else [])
            idxs = [i for i, x in enumerate(a) if x in near_x]
        elif not idxs:
            # with a tolerance: couplings in the kept span only; each kept side (not the side of an exclusion
            # boundary) needs its nearest KEPT point within the tolerance - an excluded point never stands in
            if c < exclude_below_m - _ON_TOL or c > top + _ON_TOL:
                continue
            sides = []
            if abs(c - exclude_below_m) > _ON_TOL:
                sides.append([x for x, k in zip(a, kinds) if k != "excluded" and x < c])
            if abs(c - top) > _ON_TOL:  # the exterior side of End B / the exclusion boundary is not required
                sides.append([x for x, k in zip(a, kinds) if k != "excluded" and x > c])
            near_x = []
            for side in sides:
                x = (max(side) if side and side[0] < c else min(side)) if side else None
                if x is None or abs(x - c) > coupling_tol_m:
                    raise ValueError(f"coupling at {c:.3f} m: no kept result point within {coupling_tol_m} m on "
                                     f"one side")
                near_x.append(x)
            idxs = [i for i, x in enumerate(a) if x in near_x and kinds[i] != "excluded"]
        for idx in idxs:
            if kinds[idx] == "body":
                kinds[idx] = "coupling"
    for c in pup_side:
        if not any(abs(c - p) <= _ON_TOL for p in couplings):
            raise ValueError(f"pup-side station at {c} m: no coupling at that arc length")
        borders = [abs(c - exclude_below_m) <= _ON_TOL] + \
            ([abs(c - exclude_above_m) <= _ON_TOL] if exclude_above_m is not None else [])
        if not any(borders):
            raise ValueError(f"pup-side station at {c} m: the coupling does not border the excluded section")
        kept = [i for i in range(len(a)) if kinds[i] != "excluded"]
        near = min(kept, key=lambda i: abs(a[i] - c)) if kept else None
        if near is None or abs(a[near] - c) > pup_side_tol_m:
            raise ValueError(f"no kept result point within {pup_side_tol_m} m of the coupling at {c} m")
        kinds[near] = "coupling"
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
    if not thetas:
        raise ValueError("no theta position given")
    period = period if period is not None else ofx.Period(1)
    out: dict[str, Any] = {"schema": SCHEMA_SIG, "variable": VAR, "radial_position": "outer",
                           "definition": SIGNIFICANT_DEFINITION,
                           "counter": "global_model.fatigue_channels.half_cycles (turning points)",
                           "units": {KEY: "kPa", KEY_SIG: "kPa"}, "thetas_deg": thetas, "lines": {}}
    worst_dev, checked = 0.0, 0
    for ln in lines:
        line = model[ln]
        arc = [float(v) for v in line.RangeGraph(VAR, period, ofx.oeLine(RadialPos=ofx.rpOuter, Theta=0.0)).X]
        sig, th_sig, span, ratio = [], [], [], []
        for i, x in enumerate(arc):
            want = [0.0, 90.0, 180.0] + ([45.0] if check_every and i % check_every == 0 else [])
            specs = [ofx.TimeHistorySpecification(line, VAR, ofx.oeLine(ArcLength=x, RadialPos=ofx.rpOuter,
                                                                            Theta=t)) for t in want]
            h = np.asarray(ofx.GetMultipleTimeHistories(specs, period), dtype=float)
            if h.ndim != 2 or h.shape[1] != len(want) or not np.all(np.isfinite(h)):
                raise ValueError(f"{ln} arc {x} m: ZZ stress histories at theta {want} are malformed or not "
                                 f"finite (shape {h.shape})")
            hs = theta_histories(h[:, 0], h[:, 1], h[:, 2], thetas=thetas)
            if len(want) == 4:
                rec = theta_histories(h[:, 0], h[:, 1], h[:, 2], thetas=[45.0])[45.0]
                scale = max(float(np.max(np.abs(h[:, 3]))), 1.0)
                worst_dev = max(worst_dev, float(np.max(np.abs(rec - h[:, 3]))) / scale)
                checked += 1
            s, t, sp = significant_over_theta(hs)
            sig.append(s)
            th_sig.append(t)
            span.append(sp)
            ratio.append(sp / s if s > 0 else None)
        out["lines"][ln] = {"arc_m": arc, KEY_SIG: sig, "theta_sig_deg": th_sig, KEY: span,
                            "span_over_sig": ratio}
    out["theta_check_max_rel"] = worst_dev
    out["theta_check_points"] = checked
    out["theta_check_ok"] = checked > 0 and worst_dev <= check_rtol  # nothing checked is not a pass
    return out


__all__ = ["JOINT_LENGTH_M", "KEY", "KEY_SIG", "LINES", "SCHEMA", "SCHEMA_SIG", "SIGNIFICANT_DEFINITION",
           "STATION_KINDS", "THETAS_DEG", "classify_stations", "coupling_positions", "extract", "extract_significant",
           "merge_stress_range", "range_over_theta", "significant_over_theta", "significant_range",
           "stress_range_limit_ksi", "theta_histories", "turning_points"]
