"""Criteria evaluator: one register row on one case (static, regular wave, or the seeds of an irregular sea).

Statics and regular waves give a deterministic value. The seeds of an irregular sea are combined per the
check's ``combine`` rule (see :mod:`checks`): a Gumbel fit over the seed maxima of the utilisation (or the seed
minima of a sign-criterion demand) gives the most probable extreme (MPM) and its 90 % confidence interval; the
status is set on the MPM. The governing seed is the seed with the largest observed utilisation (smallest demand
for a minimum), and its time is the time of the coincident row that set it.

Status: ``PASS`` (utilisation <= 1, or the sign criterion holds), ``FAIL``, ``NOT_EVALUATED`` (reason given),
``SCREENING`` (a value reported for information and excluded from the verdict, e.g. CR-10 on a regular wave under
W510 / R02). A missing channel is recorded in ``missing_channel``; the caller fails the step on any such record
(plan 8).
"""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass, field
from typing import Any, Iterable

from digitalmodel.drilling_riser.postprocess.channels import MissingChannel, w5
from digitalmodel.drilling_riser.postprocess.checks import (
    CHECKS,
    U_TOL,
    CheckValue,
    NotEvaluated,
    cr10_station_mean,
    evaluate_doc,
)
from digitalmodel.drilling_riser.postprocess.extremes import gumbel_fit

STATUSES = ("PASS", "FAIL", "NOT_EVALUATED", "SCREENING")


@dataclass
class CaseCheck:
    row_id: str
    status: str
    reason: str
    u: float | None = None
    demand: float | None = None
    allowable: float | None = None  # in ``unit`` (per check; see :mod:`checks`)
    unit: str = ""
    location: str = ""
    seed: int | None = None
    time_s: float | None = None
    stats: dict[str, Any] | None = None
    missing_channel: str | None = None
    detail: dict[str, Any] = field(default_factory=dict)

    def as_dict(self) -> dict[str, Any]:
        return asdict(self)


def _fmt(v: float | None) -> str:
    return "-" if v is None else f"{v:,.4g}"


def _reason(v: CheckValue, passed: bool, unit: str, prefix: str = "") -> str:
    if v.u is None:
        rel = "above" if passed else "not above"
        return f"{prefix}demand {_fmt(v.demand)} {unit} {rel} the limit {_fmt(v.allowable)} {unit}"
    rel = "within" if passed else "exceeds"
    return (f"{prefix}demand {_fmt(v.demand)} {unit} {rel} the allowable {_fmt(v.allowable)} {unit} "
            f"(U = {v.u:.3f})")


def _single(row: dict, value: CheckValue, seed: int | None = None) -> CaseCheck:
    status = "PASS" if value.passed else "FAIL"
    return CaseCheck(row_id=row["id"], status=status, reason=_reason(value, bool(value.passed), value.unit),
                     u=value.u, demand=value.demand, allowable=value.allowable, unit=value.unit,
                     location=value.location, seed=seed, time_s=value.time_s, detail=value.detail)


def evaluate_case(docs: dict[int | None, dict], row: dict, ctx: dict, *, seeds_expected: int | None = None,
                  seed_ids: Iterable[int] | None = None) -> CaseCheck:
    """``docs`` maps seed -> results document; a static or regular case has the single key None.

    An irregular case names its seeds by ``seed_ids`` (or ``seeds_expected``, meaning seeds 1..n): the documents
    must hold exactly those seeds - a missing seed, or a seed outside the set standing in for it, leaves the case
    NOT_EVALUATED rather than fitting the Gumbel distribution to the wrong sample."""
    rid = row["id"]
    if row.get("check_key") not in CHECKS:
        return CaseCheck(rid, "NOT_EVALUATED", f"no demand function for check_key {row.get('check_key')!r}")
    if seed_ids is None and seeds_expected:
        seed_ids = range(1, int(seeds_expected) + 1)
    if seed_ids is not None:
        want, have = {int(s) for s in seed_ids}, set(docs)
        if have != want:
            missing = sorted(want - have)
            extra = sorted((s for s in have - want), key=lambda s: (s is None, s))
            held = len(have & want)
            parts = [f"{held} of {len(want)} seeds hold results"]
            if missing:
                parts.append(f"missing seeds {missing}")
            if extra:
                parts.append(f"unexpected seeds {extra}")
            return CaseCheck(rid, "NOT_EVALUATED", "; ".join(parts) + "; the Gumbel fit needs every seed",
                             detail={"missing_seeds": missing, "unexpected_seeds": extra})
    values: dict[int | None, CheckValue] = {}
    for seed, doc in sorted(docs.items(), key=lambda kv: (kv[0] is None, kv[0])):
        try:
            values[seed] = evaluate_doc(w5(doc), row, ctx)
        except MissingChannel as e:
            return CaseCheck(rid, "NOT_EVALUATED", str(e), missing_channel=e.channel, seed=seed)
        except NotEvaluated as e:
            return CaseCheck(rid, "NOT_EVALUATED", str(e), seed=seed)
    if any(v.screening for v in values.values()):
        return _screening(row, values)
    if list(values) == [None]:
        return _single(row, values[None])
    if any(v.combine == "station_mean" for v in values.values()):
        try:
            return _station_mean(row, values)
        except NotEvaluated as e:
            return CaseCheck(rid, "NOT_EVALUATED", str(e))
    return _combine(row, values)


def _station_mean(row: dict, values: dict[int, CheckValue]) -> CaseCheck:
    """W03: CR-10 on the significant range - seed mean at each station (:func:`checks.cr10_station_mean`)."""
    v = cr10_station_mean(values)
    us = [float(u) for u in v.detail["seed_values"].values()]
    n = len(us)
    mean = sum(us) / n
    sd = math.sqrt(sum((u - mean) ** 2 for u in us) / (n - 1)) if n > 1 else 0.0
    passed = bool(v.passed)
    reason = _reason(v, passed, v.unit, prefix=f"seed mean over {n} seeds: ") + \
        f"; seed U at the governing station {min(us):.3f}-{max(us):.3f}"
    return CaseCheck(row_id=row["id"], status="PASS" if passed else "FAIL", reason=reason, u=v.u, demand=v.demand,
                     allowable=v.allowable, unit=v.unit, location=v.location, seed=None, time_s=None,
                     stats={"estimator": "seed mean", "n": n, "u_mean": mean, "u_sd": sd, "u_min": min(us),
                            "u_max": max(us)},
                     detail=v.detail)


def _screening(row: dict, values: dict[int | None, CheckValue]) -> CaseCheck:
    """A screening value (e.g. CR-10 on a regular wave, W510 / R02): reported with its utilisation, no verdict."""
    gov = max(values, key=lambda s: (values[s].u if values[s].u is not None else float("-inf")))
    v = values[gov]
    reason = (f"screening only, not used for the verdict: demand {_fmt(v.demand)} {v.unit} against "
              f"{_fmt(v.allowable)} {v.unit}" + ("" if v.u is None else f" (U = {v.u:.3f})"))
    return CaseCheck(row_id=row["id"], status="SCREENING", reason=reason, u=v.u, demand=v.demand,
                     allowable=v.allowable, unit=v.unit, location=v.location, seed=gov, time_s=v.time_s,
                     detail=v.detail)


def _combine(row: dict, values: dict[int, CheckValue]) -> CaseCheck:
    how = CHECKS[row["check_key"]][1]
    seeds = list(values)
    first = values[seeds[0]]
    if how == "same":
        return _single(row, first, seed=None)
    if how == "mean":
        d = sum(v.demand for v in values.values()) / len(values)
        v = CheckValue(demand=d, allowable=first.allowable, unit=first.unit,
                       u=None if first.allowable is None else abs(d) / first.allowable, location=first.location,
                       detail={"seeds": len(values)})
        c = _single(row, v)
        c.reason = f"mean over {len(values)} seeds: " + c.reason
        return c
    if how == "max":
        us = [values[s].u for s in seeds]
        fit = gumbel_fit(us, kind="max")
        gov = max(seeds, key=lambda s: values[s].u)
        g = values[gov]
        cap = g.allowable
        demand = fit.mpm * cap if cap is not None else None
        passed = fit.mpm <= 1.0 + U_TOL
        v = CheckValue(demand=demand, allowable=cap, unit=g.unit, u=fit.mpm, passed=passed)
        reason = _reason(v, passed, g.unit, prefix=f"Gumbel MPM over {fit.n} seeds: ") + \
            f"; 90 % CI of U {fit.ci_low:.3f}-{fit.ci_high:.3f}"
    else:  # min
        ds = [values[s].demand for s in seeds]
        fit = gumbel_fit(ds, kind="min")
        gov = min(seeds, key=lambda s: values[s].demand)
        g = values[gov]
        passed = fit.mpm > g.allowable
        v = CheckValue(demand=fit.mpm, allowable=g.allowable, unit=g.unit, u=None, passed=passed)
        reason = _reason(v, passed, g.unit, prefix=f"Gumbel MPM of the minimum over {fit.n} seeds: ") + \
            f"; 90 % CI {fit.ci_low:,.4g}-{fit.ci_high:,.4g} {g.unit}"
    return CaseCheck(row_id=row["id"], status="PASS" if passed else "FAIL", reason=reason, u=v.u, demand=v.demand,
                     allowable=v.allowable, unit=g.unit, location=g.location, seed=gov, time_s=g.time_s,
                     stats=fit.as_dict(),
                     detail={**g.detail, "governing_seed_u": g.u, "governing_seed_demand": g.demand,
                             "seed_values": {str(s): (values[s].u if how == "max" else values[s].demand)
                                             for s in seeds}})


def _order_key(c: CaseCheck) -> float:
    if c.u is not None:
        return c.u
    if c.demand is not None and c.allowable is not None:  # sign criterion: the smallest margin governs
        return -(c.demand - c.allowable)
    return float("-inf")


def summarise(results: Iterable[tuple[str, CaseCheck]]) -> dict[str, dict[str, Any]]:
    """Per row: counts by status, governing case (largest utilisation, or smallest margin for a sign criterion),
    row status (FAIL if any case fails, PASS if every evaluated case passes, NOT_EVALUATED if none evaluated).
    ``SCREENING`` cases are counted (the key appears only when present) but never govern and never set the row\n    status."""
    out: dict[str, dict[str, Any]] = {}
    for case_id, c in results:
        s = out.setdefault(c.row_id, {"counts": {k: 0 for k in STATUSES[:3]}, "governing": None, "max_u": None,
                                      "_key": float("-inf")})
        s["counts"][c.status] = s["counts"].get(c.status, 0) + 1  # SCREENING appears only when present
        if c.status in ("NOT_EVALUATED", "SCREENING"):
            continue
        k = _order_key(c)
        if k > s["_key"]:
            s["_key"] = k
            s["governing"] = {"case_id": case_id, **c.as_dict()}
            s["max_u"] = c.u
    for s in out.values():
        s.pop("_key")
        n = s["counts"]
        s["status"] = "FAIL" if n["FAIL"] else ("PASS" if n["PASS"] else "NOT_EVALUATED")
    return out
