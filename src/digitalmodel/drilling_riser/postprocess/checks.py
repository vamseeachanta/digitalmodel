"""Demand functions for criteria-register rows, keyed by the row's ``check_key``.

Each function reads one results document (``riser-w5-channels/1``), the register row (its limit) and a case
context (values set per case by the caller: tension setting, T_min at the case mud weight, stroke datum,
connector map, interface offsets) and returns a :class:`CheckValue` for that document (one static case, one
regular-wave case or one seed). Utilisation is demand / allowable; a sign criterion (effective tension > 0)
has no utilisation and passes on the sign. ``demand`` and ``allowable`` are in the check's own ``unit`` (deg, m,
MPa, kips, ft-kips - it differs per check), which is why neither carries a unit suffix: the unit travels with the
value in ``unit``, and the pair always satisfies ``u = |demand| / allowable``.

``combine`` (per check) says how the seeds of an irregular case combine: ``max`` - Gumbel fit over the seed
maxima of the utilisation; ``min`` - Gumbel fit over the seed minima of the demand; ``mean`` - mean of the seed
values; ``same`` - a setting identical in every seed. A :class:`CheckValue` may override the rule: CR-10 on the
significant range of an irregular sea sets ``combine = "station_mean"`` (W03, owner decision 2026-10-07: the seed
mean at each station, :func:`cr10_station_mean`), because H1/3 is a statistic of the whole sea state, not an
extreme.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Any, Callable

from digitalmodel.drilling_riser.postprocess.channels import (
    MissingChannel,
    contents,
    extreme_row,
    is_dynamic,
    point_rows,
    point_value,
    range_extreme,
    range_series,
    row_value,
    stroke_range,
)
from digitalmodel.drilling_riser.postprocess.pressure import REFERENCE as PRESSURE_REFERENCE
from digitalmodel.drilling_riser.postprocess.pressure import burst_pressure_mpa, collapse_pressure_mpa
from digitalmodel.drilling_riser.postprocess.stress_range import (
    SIGNIFICANT_DEFINITION,
    classify_stations,
    stress_range_limit_ksi,
)

G = 9.80665
KIP_KN = 4.4482216152605  # 1 kip in kN
FT_KIP_KNM = 1.3558179483314  # 1 ft-kip in kN m
KSI_KPA = 6894.757293168  # 1 ksi in kPa
FJ_POINTS = {"upper": "ufj", "lower": "lfj"}
U_TOL = 1e-9


class NotEvaluated(Exception):
    """The check cannot be evaluated for a stated reason other than a missing channel."""


@dataclass
class CheckValue:
    demand: float
    allowable: float | None  # in ``unit`` (per check), the denominator of ``u``
    unit: str
    u: float | None
    location: str = ""
    time_s: float | None = None
    passed: bool | None = None
    detail: dict[str, Any] = field(default_factory=dict)
    screening: bool = False  # reported for information only, not part of the verdict (status SCREENING)
    combine: str | None = None  # seed combination overriding the check's ``combine`` rule (``station_mean``)

    def __post_init__(self) -> None:
        if self.screening:
            self.passed = None
        elif self.passed is None and self.u is not None:
            self.passed = self.u <= 1.0 + U_TOL


# ---------------------------------------------------------------------------------------------- building blocks


def envelope_min_capacity(p: float, pressures: list[float], capacities: list[float]) -> float:
    """Lowest capacity of a piecewise-linear capacity-pressure curve over [0, p] (p within the curve)."""
    if p < pressures[0] - 1e-12 or p > pressures[-1] + 1e-12:
        raise ValueError(f"pressure {p} outside the capacity curve {pressures[0]}..{pressures[-1]}")
    vals = [c for q, c in zip(pressures, capacities) if q <= p]
    for (q0, c0), (q1, c1) in zip(zip(pressures, capacities), zip(pressures[1:], capacities[1:])):
        if q0 <= p <= q1:
            vals.append(c0 + (c1 - c0) * (p - q0) / (q1 - q0) if q1 > q0 else c0)
    return min(vals)


def bore_differential_kpa(po_kpa: float, *, rho_contents: float, z_ref_m: float, rho_water: float) -> float:
    """Internal minus external pressure at a point from its external hydrostatic pressure: depth
    d = po / (rho_water g); internal = rho_contents g (d + z_ref), z_ref = the contents-column reference above MSL."""
    depth = po_kpa * 1000.0 / (rho_water * G)
    return rho_contents * G * (depth + z_ref_m) / 1000.0 - po_kpa


def zero_crossing(xs: list[float], ys: list[float]) -> float | None:
    """First x at which y changes sign from positive, by linear interpolation between adjacent points."""
    for (x0, y0), (x1, y1) in zip(zip(xs, ys), zip(xs[1:], ys[1:])):
        if y0 > 0 >= y1:
            return x0 + (x1 - x0) * y0 / (y0 - y1)
    return None


def _ratio(demand: float, allowable: float, unit: str, **kw) -> CheckValue:
    return CheckValue(demand=demand, allowable=allowable, unit=unit, u=abs(demand) / allowable, **kw)


def _fj_points(row: dict) -> list[str]:
    loc = str(row.get("location") or "upper and lower")
    return [p for k, p in FJ_POINTS.items() if k in loc]


# ---------------------------------------------------------------------------------------------- checks


def fj_mean(w, row, ctx) -> CheckValue:
    best = None
    for p in _fj_points(row):
        v = abs(point_value(w, p, "ez_angle_deg", "mean"))
        c = _ratio(v, float(row["limit"]["value"]), "deg", location=p)
        best = c if best is None or c.u > best.u else best
    return best


def _fj_max_at(w, p: str) -> tuple[float, float | None]:
    hi = point_value(w, p, "ez_angle_deg", "max")
    lo = point_value(w, p, "ez_angle_deg", "min")
    kind = "max" if abs(hi) >= abs(lo) else "min"
    r = extreme_row(w, p, "ez_angle_deg", kind)
    return max(abs(hi), abs(lo)), (r or {}).get("t")


def fj_max(w, row, ctx) -> CheckValue:
    best = None
    for p in _fj_points(row):
        v, t = _fj_max_at(w, p)
        c = _ratio(v, float(row["limit"]["value"]), "deg", location=p, time_s=t)
        best = c if best is None or c.u > best.u else best
    return best


def fj_max_avail(w, row, ctx) -> CheckValue:
    best, by = None, {}
    for k, cap in row["limit"]["limit_deg"].items():
        p = FJ_POINTS[k]
        v, t = _fj_max_at(w, p)
        c = _ratio(v, float(cap), "deg", location=p, time_s=t)
        by[p] = c.u
        best = c if best is None or c.u > best.u else best
    best.detail["by_location"] = by
    return best


def von_mises(w, row, ctx) -> CheckValue:
    line = ctx.get("stress_line", "Riser")
    v, arc = range_extreme(w, line, "vm", "max")
    return _ratio(v / 1000.0, float(row["limit"]["value"]), "MPa", location=f"{line} arc {arc:.1f} m")


def coupling_rating(w, row, ctx) -> CheckValue:
    v, arc = range_extreme(w, "Riser", "tw", "max")
    return _ratio(v / KIP_KN, float(row["limit"]["rated_kips"]), "kips", location=f"Riser arc {arc:.1f} m")


def t_eff_min(w, row, ctx) -> CheckValue:
    v, arc = range_extreme(w, "Riser", "te", "min")
    return CheckValue(demand=v, allowable=0.0, unit="kN", u=None, location=f"Riser arc {arc:.1f} m", passed=v > 0.0)


def t_min(w, row, ctx) -> CheckValue:
    return _ratio(float(ctx["t_min_kips"]), float(ctx["top_tension_kips"]), "kips", location="tension setting",
                  detail={"note": "demand = T_min at the case mud weight; allowable = the case top-tension setting"})


def tension_setting_max(w, row, ctx) -> CheckValue:
    return _ratio(float(ctx["top_tension_kips"]), float(row["limit"]["value"]), "kips", location="tension setting")


def tj_stroke(w, row, ctx) -> CheckValue:
    """Telescopic-joint stroke: the excursion from the datum stroke against the travel available in that direction
    (demand and allowable are the excursion and the travel, so u = demand / allowable; the absolute stroke
    positions are in ``detail``)."""
    lo, hi = (float(v) for v in row["limit"]["usable_m"])
    s0 = float(ctx["tj_datum_m"])
    smin, smax, _ = stroke_range(w)
    ext_m, col_m = max(smax, 0.0), max(-smin, 0.0)
    u_ext, u_col = ext_m / (hi - s0), col_m / (s0 - lo)
    detail = {"datum_m": s0, "usable_m": [lo, hi], "stroke_max_m": s0 + smax, "stroke_min_m": s0 + smin}
    if u_ext >= u_col:
        return CheckValue(demand=ext_m, allowable=hi - s0, unit="m", u=u_ext, location="telescopic joint, extension",
                          detail=detail)
    return CheckValue(demand=col_m, allowable=s0 - lo, unit="m", u=u_col, location="telescopic joint, collapse",
                      detail=detail)


def tensioner_stroke(w, row, ctx) -> CheckValue:
    half = float(row["limit"]["stroke_m"]) / 2.0
    smin, smax, _ = stroke_range(w)
    v = max(abs(smin), abs(smax))
    return _ratio(v, half, "m", location="tensioner excursion from mid-stroke")


def connector_tmp(w, row, ctx) -> CheckValue:
    envs = row["limit"]["envelopes"]
    level = ctx.get("connector_level", "operational")
    c = contents(w)
    rho_w = float(ctx["rho_water_kg_m3"])
    best, by = None, {}
    for point, env_name in ctx["connectors"].items():
        env = envs[env_name]
        t_rated = float(env["tension_kips"])
        caps = env[f"{level}_ft_kips"]
        off = float(ctx["interface_offset_kn"][point])
        p_chart = env["bore_pressure_ksi"]
        for r in point_rows(w, point):
            te_below = row_value(r, point, "te")
            te_if = te_below + off
            if abs(te_if) / KIP_KN > t_rated + 1e-9:
                raise NotEvaluated(f"{point}: interface tension {te_if / KIP_KN:,.0f} kips at t = {r.get('t')} s is "
                                   f"outside the capacity chart ({t_rated:,.0f} kips)")
            dp = bore_differential_kpa(row_value(r, point, "po"), rho_contents=float(c["density_kg_m3"]),
                                       z_ref_m=float(c["pressure_ref_z_m"]), rho_water=rho_w)
            p_ksi = max(dp, 0.0) / KSI_KPA
            if not p_chart[0] - 1e-12 <= p_ksi <= p_chart[-1] + 1e-12:
                raise NotEvaluated(f"{point}: bore differential {p_ksi:,.3f} ksi at t = {r.get('t')} s is outside the "
                                   f"capacity chart ({p_chart[0]:g}-{p_chart[-1]:g} ksi)")
            cap = envelope_min_capacity(p_ksi, p_chart, caps)
            m = abs(row_value(r, point, "m")) / FT_KIP_KNM
            u = m / cap
            by[point] = max(by.get(point, 0.0), u)
            if best is None or u > best.u:
                best = CheckValue(demand=m, allowable=cap, unit="ft-kips", u=u, location=point, time_s=r.get("t"),
                                  detail={"te_interface_kn": te_if, "te_below_kn": te_below,
                                          "bore_differential_ksi": p_ksi, "level": level, "envelope": env_name})
    best.detail["by_location"] = by
    return best


def conductor_bending(w, row, ctx) -> CheckValue:
    lim = row["limit"]
    wh_point = ctx["wellhead_point"]
    wh_cap = float(lim["wellhead_system_ft_kips"])
    wh_m = max(abs(point_value(w, wh_point, "m", "max")), abs(point_value(w, wh_point, "m", "min")))
    wr = extreme_row(w, wh_point, "m", "max")
    wh = CheckValue(demand=wh_m / FT_KIP_KNM, allowable=wh_cap, unit="ft-kips", u=wh_m / FT_KIP_KNM / wh_cap,
                    location=wh_point, time_s=(wr or {}).get("t"))
    # conductor: every range-graph point against the capacity of the section at its arc length
    cd = None
    for arc, m in range_series(w, "Conductor", "m", "max"):
        sec = next((s for s in ctx["conductor_sections"]
                    if arc is not None and s["arc_from_m"] - 1e-6 <= arc <= s["arc_to_m"] + 1e-6), None)
        if sec is None:
            raise NotEvaluated(f"conductor arc {arc} m is outside the section map")
        cap = float(sec["capacity_ft_kips"])
        u = abs(m) / FT_KIP_KNM / cap
        if cd is None or u > cd.u:
            cd = CheckValue(demand=abs(m) / FT_KIP_KNM, allowable=cap, unit="ft-kips", u=u,
                            location=f"Conductor arc {arc:.1f} m ({sec['name']})")
    best = cd if cd.u >= wh.u else wh
    best.detail["by_location"] = {wh_point: wh.u, "Conductor": cd.u}
    return best


CR10_DETAIL_BY_KIND = {"coupling": "riser coupling weld", "body": "riser girth weld"}
CR10_BASIS = {"irregular": ("sig", "significant"), "regular": ("max", "screening (max - min)")}


def _cr10_limits(row) -> dict[str, float]:
    saf = row["limit"].get("saf") or {}
    return ({d: stress_range_limit_ksi(float(s)) * KSI_KPA / 1000.0 for d, s in saf.items()}
            or {"nominal (SAF <= 1.5)": float(row["limit"]["value_if_saf_le_1_5"])})


def _cr10_hot_spot(series, kinds, hot_spot_m, cap_of) -> dict[str, Any] | None:
    """The W7 hot-spot report: the kept result point nearest ``hot_spot_m``. ``cr10_station`` names its CR-10
    kind when it is also a CR-10 station (the pup-side station of the barrel coupling, W04), else None."""
    if hot_spot_m is None:
        return None
    kept = [i for i, k in enumerate(kinds) if k != "excluded"]
    if not kept:
        return None
    i = min(kept, key=lambda j: abs(series[j][0] - float(hot_spot_m)))
    arc, v = series[i]
    mpa = v / 1000.0
    station = kinds[i] if kinds[i] in cap_of else None
    return {"arc_m": arc, "demand": mpa, "unit": "MPa", "cr10_station": station,
            "use": "W7 fatigue hot spot" + ("; also the CR-10 station of its coupling" if station
                                            else "; not used for CR-10"),
            "allowable_mpa": dict(cap_of), **{f"u_{kk}": mpa / cap for kk, cap in cap_of.items()}}


def _cr10_governing(stations: list[dict], line: str, detail: dict, *, screening: bool,
                    combine: str | None) -> CheckValue:
    """The station with the largest utilisation, with the largest per kind in ``detail["by_kind"]``. A non-finite
    value at any station, or no station at all, leaves CR-10 NOT_EVALUATED."""
    if not stations:
        raise NotEvaluated("CR-10: no station on the stress-range axis")
    bad = [s["arc_m"] for s in stations if not (math.isfinite(s["demand"]) and math.isfinite(s["u"]))]
    if bad:
        raise NotEvaluated(f"CR-10: stress range not finite at {len(bad)} station(s), first at arc {bad[0]} m")
    by_kind: dict[str, dict] = {}
    for s in stations:
        if s["kind"] not in by_kind or s["u"] > by_kind[s["kind"]]["u"]:
            by_kind[s["kind"]] = {"u": s["u"], "demand": s["demand"], "arc_m": s["arc_m"]}
    g = max(stations, key=lambda s: s["u"])
    detail = {**detail, "by_kind": by_kind}
    return CheckValue(demand=g["demand"], allowable=g["allowable"], unit="MPa", u=g["u"],
                      location=f"{line} arc {g['arc_m']:.1f} m ({g['kind']}, {g['detail']})", detail=detail,
                      screening=screening, combine=combine)


def cr10_station_mean(values: dict) -> CheckValue:
    """W03 (owner decision 2026-10-07): the significant range of an irregular sea combines over the seeds by the
    seed MEAN at each station, then the station with the largest mean utilisation governs (the extremes keep the
    Gumbel fit). Every seed must hold the same stations (``NotEvaluated`` otherwise)."""
    seeds = list(values)
    grids = {tuple((round(s["arc_m"], 6), s["kind"], round(s["allowable"], 9))
                   for s in values[k].detail["station_values"]) for k in seeds}
    if len(grids) != 1:
        raise NotEvaluated("CR-10 seed mean: the seeds do not hold the same stations and allowables")
    hot_arcs = {None if values[k].detail.get("hot_spot") is None else round(values[k].detail["hot_spot"]["arc_m"], 6)
                for k in seeds}
    if len(hot_arcs) != 1:
        raise NotEvaluated("CR-10 seed mean: the seeds do not report the hot spot at the same arc")
    first = values[seeds[0]]
    n = len(seeds)
    stations = []
    for j, s0 in enumerate(first.detail["station_values"]):
        d = sum(values[k].detail["station_values"][j]["demand"] for k in seeds) / n
        stations.append({**s0, "demand": d, "u": d / s0["allowable"]})
    line = first.detail["line"]
    detail = {k: v for k, v in first.detail.items() if k not in ("station_values", "by_kind", "hot_spot")}
    if "by_detail" in detail:  # whole-line path: recompute from the mean, not seed 1
        peak = max(s["demand"] for s in stations)
        detail["by_detail"] = {d: peak / cap for d, cap in detail["limits_mpa"].items()}
    hots = [values[k].detail.get("hot_spot") for k in seeds]
    if all(hots):
        hd = sum(h["demand"] for h in hots) / n
        detail["hot_spot"] = {**hots[0], "demand": hd, "combine": "seed mean",
                              **{f"u_{k}": hd / cap for k, cap in hots[0]["allowable_mpa"].items()}}
    v = _cr10_governing(stations, line, detail, screening=False, combine=None)
    j = max(range(len(stations)), key=lambda i: stations[i]["u"])
    seed_u = {str(k): values[k].detail["station_values"][j]["u"] for k in seeds}
    v.detail.update({"combine": "seed mean per station", "seed_values": seed_u,
                     "station_values": stations})
    return v


def _dyn_stress_range_joints(w, row, ctx, wave: str) -> CheckValue:
    """W510 (owner decisions 2026-10-07). R02: irregular seas (CON-I1) evaluate CR-10 on the significant range;
    regular waves (CON-R1) give a screening value on the max - min range that takes no part in the verdict.
    R03: ``SIGNIFICANT_DEFINITION`` (H1/3 of rainflow ranges). With ``cr10_stations`` in the context (keyword
    arguments of :func:`stress_range.classify_stations`) each result point takes the SAF of its kind (coupling or
    joint body), the excluded section is dropped and the first-pup hot spot is reported in ``detail`` without
    governing; without it the whole line is taken against the lowest allowable range."""
    if wave not in CR10_BASIS:
        raise ValueError(f"wave_kind must be one of {sorted(CR10_BASIS)}, got {wave!r}")
    kind_key, basis = CR10_BASIS[wave]
    line = ctx.get("stress_line", "Riser")
    series = range_series(w, line, "zz_range", kind_key)
    limits = _cr10_limits(row)
    detail: dict[str, Any] = {"basis": basis, "wave_kind": wave, "line": line,
                              "channel": f"range_graphs.{line}.zz_range_{kind_key}",
                              "theta_grid_note": "24 theta positions 15 deg apart: the bending part may be under-read "
                                                 "by up to 1 - cos 7.5 deg = 0.86 %",
                              "definition": SIGNIFICANT_DEFINITION if wave == "irregular" else None,
                              "limits_mpa": limits}
    st = ctx.get("cr10_stations")
    if st:
        try:
            kinds = classify_stations([a for a, _ in series], **st)
        except ValueError as e:
            raise NotEvaluated(f"CR-10 station map: {e}") from None
        detail_of = {**CR10_DETAIL_BY_KIND, **(ctx.get("cr10_detail_by_kind") or {})}
        cap_of = {k: limits[d] for k, d in detail_of.items() if d in limits}
        lacking = sorted({detail_of.get(k, k) for k in kinds if k in ("coupling", "body") and k not in cap_of})
        if lacking:  # a station kind on the line without an allowable must not drop out of the verdict
            raise NotEvaluated(f"no SAF in the register row for {', '.join(lacking)}")
        stations = [{"arc_m": arc, "kind": k, "detail": detail_of[k], "demand": v / 1000.0, "allowable": cap_of[k],
                     "u": v / 1000.0 / cap_of[k]} for (arc, v), k in zip(series, kinds) if k in cap_of]
        if not stations:
            raise NotEvaluated("no riser-joint station on the stress-range axis")
        hot = _cr10_hot_spot(series, kinds, st.get("hot_spot_m"), cap_of)
        detail.update({"hot_spot": hot, "stations": st, "station_values": stations})
        return _cr10_governing(stations, line, detail, screening=wave == "regular",
                               combine="station_mean" if wave == "irregular" else None)
    gov = min(limits, key=limits.get)  # one demand per point: the lowest allowable governs
    stations = [{"arc_m": arc, "kind": "whole line", "detail": gov, "demand": v / 1000.0, "allowable": limits[gov],
                 "u": v / 1000.0 / limits[gov]} for arc, v in series]
    finite = [s["demand"] for s in stations if math.isfinite(s["demand"])]
    peak = max(finite) if finite else float("nan")
    detail.update({"by_detail": {d: peak / cap for d, cap in limits.items()}, "station_values": stations})
    return _cr10_governing(stations, line, detail, screening=wave == "regular",
                           combine="station_mean" if wave == "irregular" else None)


def dyn_stress_range(w, row, ctx) -> CheckValue:
    """CR-10: outer-fibre axial stress range (double amplitude) along the riser against the allowable range of each
    weld detail (10 ksi if SAF <= 1.5, else 15/SAF ksi); the detail with the largest utilisation governs.

    With ``wave_kind`` in the context (``irregular`` or ``regular``) the W510 joint evaluation applies
    (:func:`_dyn_stress_range_joints`); without it the W501 whole-line max - min path is kept unchanged."""
    if not is_dynamic(w):
        raise NotEvaluated("static case: no dynamic stress range")
    if ctx.get("wave_kind") is not None:
        return _dyn_stress_range_joints(w, row, ctx, ctx["wave_kind"])
    line = ctx.get("stress_line", "Riser")
    v, arc = range_extreme(w, line, "zz_range", "max")
    mpa = v / 1000.0
    saf = row["limit"].get("saf") or {}
    limits = ({d: stress_range_limit_ksi(float(s)) * KSI_KPA / 1000.0 for d, s in saf.items()}
              or {"nominal (SAF <= 1.5)": float(row["limit"]["value_if_saf_le_1_5"])})
    by = {d: mpa / cap for d, cap in limits.items()}
    gov = max(by, key=by.get)
    return CheckValue(demand=mpa, allowable=limits[gov], unit="MPa", u=by[gov], location=f"{line} arc {arc:.1f} m ({gov})",
                      detail={"by_detail": by, "saf": saf})


def _pressure_rows(w, ctx):
    for p in ctx.get("pressure_points", ["riser_top", "lfj"]):
        for r in point_rows(w, p):
            yield p, r


def _pipe_capacity(ctx, kind: str) -> float:
    p = ctx["pipe"]
    if kind == "burst":
        return burst_pressure_mpa(p["od_m"], p["t_min_m"], p["smys_mpa"], p["smts_mpa"])
    return collapse_pressure_mpa(p["od_m"], p["t_min_m"], p["smys_mpa"], p.get("e_mpa", 207000.0),
                                 p.get("poisson", 0.3))


def _pressure_check(w, row, ctx, sign: float, kind: str) -> CheckValue:
    cap = float(row["limit"]["factor"]) * _pipe_capacity(ctx, kind)
    best = None
    for p, r in _pressure_rows(w, ctx):
        d = sign * (row_value(r, p, "pi") - row_value(r, p, "po")) / 1000.0
        if best is None or d > best[0]:
            best = (d, p, r.get("t"))
    d, p, t = best
    return CheckValue(demand=d, allowable=cap, unit="MPa", u=max(d, 0.0) / cap, location=p, time_s=t,
                      detail={"capacity_basis": f"{row['limit']['factor']} x p_{kind[0]} ({PRESSURE_REFERENCE})"})


def burst(w, row, ctx) -> CheckValue:
    """CR-23: internal overpressure p_i - p_e against F_D p_b along the riser pressure points."""
    return _pressure_check(w, row, ctx, 1.0, "burst")


def collapse(w, row, ctx) -> CheckValue:
    """CR-25: net external pressure p_e - p_i against F_D p_c (a negative demand has no collapse load)."""
    return _pressure_check(w, row, ctx, -1.0, "collapse")


def moonpool_clearance(w, row, ctx) -> CheckValue:
    """CR-21: largest upper flex-joint angle against the smallest limiting clearance angle (context
    ``clearance_limit_deg``: obstruction -> angle, from :mod:`clearance`)."""
    lims = {k: float(v) for k, v in ctx["clearance_limit_deg"].items()}
    v, t = _fj_max_at(w, "ufj")
    by = {k: v / lim for k, lim in lims.items()}
    gov = max(by, key=by.get)
    return CheckValue(demand=v, allowable=lims[gov], unit="deg", u=by[gov], location=f"ufj ({gov})", time_s=t,
                      detail={"by_obstruction": by, "limit_deg": lims})


CHECKS: dict[str, tuple[Callable[..., CheckValue], str]] = {
    "fj_mean": (fj_mean, "mean"),
    "fj_max": (fj_max, "max"),
    "fj_max_avail": (fj_max_avail, "max"),
    "von_mises": (von_mises, "max"),
    "coupling_rating": (coupling_rating, "max"),
    "t_eff_min": (t_eff_min, "min"),
    "t_min": (t_min, "same"),
    "tension_setting_max": (tension_setting_max, "same"),
    "tj_stroke": (tj_stroke, "max"),
    "tensioner_stroke": (tensioner_stroke, "max"),
    "connector_tmp": (connector_tmp, "max"),
    "conductor_bending": (conductor_bending, "max"),
    "dyn_stress_range": (dyn_stress_range, "max"),
    "burst": (burst, "max"),
    "collapse": (collapse, "max"),
    "moonpool_clearance": (moonpool_clearance, "max"),
}


def evaluate_doc(doc_w5: dict, row: dict, ctx: dict) -> CheckValue:
    """One check on one results document (raises MissingChannel, NotEvaluated or KeyError on unknown keys)."""
    fn, _ = CHECKS[row["check_key"]]
    v = fn(doc_w5, row, ctx)
    if v.u is not None and not math.isfinite(v.u):
        raise NotEvaluated("utilisation not finite")
    return v


__all__ = ["CHECKS", "CheckValue", "FT_KIP_KNM", "KIP_KN", "KSI_KPA", "MissingChannel", "NotEvaluated",
           "bore_differential_kpa", "envelope_min_capacity", "evaluate_doc", "is_dynamic", "zero_crossing"]
