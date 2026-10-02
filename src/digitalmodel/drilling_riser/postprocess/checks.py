"""Demand functions for criteria-register rows, keyed by the row's ``check_key``.

Each function reads one results document (``riser-w5-channels/1``), the register row (its limit) and a case
context (values set per case by the caller: tension setting, T_min at the case mud weight, stroke datum,
connector map, interface offsets) and returns a :class:`CheckValue` for that document (one static case, one
regular-wave case or one seed). Utilisation is demand / capacity; a sign criterion (effective tension > 0)
has no utilisation and passes on the sign.

``combine`` (per check) says how the seeds of an irregular case combine: ``max`` - Gumbel fit over the seed
maxima of the utilisation; ``min`` - Gumbel fit over the seed minima of the demand; ``mean`` - mean of the seed
values; ``same`` - a setting identical in every seed.
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
    stroke_range,
)
from digitalmodel.drilling_riser.postprocess.pressure import REFERENCE as PRESSURE_REFERENCE
from digitalmodel.drilling_riser.postprocess.pressure import burst_pressure_mpa, collapse_pressure_mpa
from digitalmodel.drilling_riser.postprocess.stress_range import stress_range_limit_ksi

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
    capacity: float | None
    unit: str
    u: float | None
    location: str = ""
    time_s: float | None = None
    passed: bool | None = None
    detail: dict[str, Any] = field(default_factory=dict)

    def __post_init__(self) -> None:
        if self.passed is None and self.u is not None:
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


def _ratio(demand: float, capacity: float, unit: str, **kw) -> CheckValue:
    return CheckValue(demand=demand, capacity=capacity, unit=unit, u=abs(demand) / capacity, **kw)


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
    return CheckValue(demand=v, capacity=0.0, unit="kN", u=None, location=f"Riser arc {arc:.1f} m", passed=v > 0.0)


def t_min(w, row, ctx) -> CheckValue:
    return _ratio(float(ctx["t_min_kips"]), float(ctx["top_tension_kips"]), "kips", location="tension setting",
                  detail={"note": "demand = T_min at the case mud weight; capacity = the case top-tension setting"})


def tension_setting_max(w, row, ctx) -> CheckValue:
    return _ratio(float(ctx["top_tension_kips"]), float(row["limit"]["value"]), "kips", location="tension setting")


def tj_stroke(w, row, ctx) -> CheckValue:
    lo, hi = (float(v) for v in row["limit"]["usable_m"])
    s0 = float(ctx["tj_datum_m"])
    smin, smax, _ = stroke_range(w)
    u_ext, u_col = smax / (hi - s0), -smin / (s0 - lo)
    if u_ext >= u_col:
        return CheckValue(demand=s0 + smax, capacity=hi, unit="m", u=u_ext, location="telescopic joint, extension")
    return CheckValue(demand=s0 + smin, capacity=lo, unit="m", u=u_col, location="telescopic joint, collapse")


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
        for r in point_rows(w, point):
            te_if = float(r["te"]) + off
            if abs(te_if) / KIP_KN > t_rated + 1e-9:
                raise NotEvaluated(f"{point}: interface tension {te_if / KIP_KN:,.0f} kips at t = {r.get('t')} s is "
                                   f"outside the capacity chart ({t_rated:,.0f} kips)")
            dp = bore_differential_kpa(float(r["po"]), rho_contents=float(c["density_kg_m3"]),
                                       z_ref_m=float(c["pressure_ref_z_m"]), rho_water=rho_w)
            p_ksi = max(dp, 0.0) / KSI_KPA
            cap = envelope_min_capacity(p_ksi, env["bore_pressure_ksi"], caps)
            m = abs(float(r["m"])) / FT_KIP_KNM
            u = m / cap
            by[point] = max(by.get(point, 0.0), u)
            if best is None or u > best.u:
                best = CheckValue(demand=m, capacity=cap, unit="ft-kips", u=u, location=point, time_s=r.get("t"),
                                  detail={"te_interface_kn": te_if, "te_below_kn": float(r["te"]),
                                          "bore_differential_ksi": p_ksi, "level": level, "envelope": env_name})
    best.detail["by_location"] = by
    return best


def conductor_bending(w, row, ctx) -> CheckValue:
    lim = row["limit"]
    wh_point = ctx["wellhead_point"]
    wh_cap = float(lim["wellhead_system_ft_kips"])
    wh_m = max(abs(point_value(w, wh_point, "m", "max")), abs(point_value(w, wh_point, "m", "min")))
    wr = extreme_row(w, wh_point, "m", "max")
    wh = CheckValue(demand=wh_m / FT_KIP_KNM, capacity=wh_cap, unit="ft-kips", u=wh_m / FT_KIP_KNM / wh_cap,
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
            cd = CheckValue(demand=abs(m) / FT_KIP_KNM, capacity=cap, unit="ft-kips", u=u,
                            location=f"Conductor arc {arc:.1f} m ({sec['name']})")
    best = cd if cd.u >= wh.u else wh
    best.detail["by_location"] = {wh_point: wh.u, "Conductor": cd.u}
    return best


def dyn_stress_range(w, row, ctx) -> CheckValue:
    """CR-10: outer-fibre axial stress range (double amplitude) along the riser against the allowable range of each
    weld detail (10 ksi if SAF <= 1.5, else 15/SAF ksi); the detail with the largest utilisation governs."""
    if not is_dynamic(w):
        raise NotEvaluated("static case: no dynamic stress range")
    line = ctx.get("stress_line", "Riser")
    v, arc = range_extreme(w, line, "zz_range", "max")
    mpa = v / 1000.0
    saf = row["limit"].get("saf") or {}
    limits = ({d: stress_range_limit_ksi(float(s)) * KSI_KPA / 1000.0 for d, s in saf.items()}
              or {"nominal (SAF <= 1.5)": float(row["limit"]["value_if_saf_le_1_5"])})
    by = {d: mpa / cap for d, cap in limits.items()}
    gov = max(by, key=by.get)
    return CheckValue(demand=mpa, capacity=limits[gov], unit="MPa", u=by[gov], location=f"{line} arc {arc:.1f} m ({gov})",
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
        d = sign * (float(r["pi"]) - float(r["po"])) / 1000.0
        if best is None or d > best[0]:
            best = (d, p, r.get("t"))
    d, p, t = best
    return CheckValue(demand=d, capacity=cap, unit="MPa", u=max(d, 0.0) / cap, location=p, time_s=t,
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
    return CheckValue(demand=v, capacity=lims[gov], unit="deg", u=by[gov], location=f"ufj ({gov})", time_s=t,
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
