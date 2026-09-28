"""Criteria evaluator: one register row on one case (static, regular wave, or the seeds of an irregular sea).

Statics and regular waves give a deterministic value. The seeds of an irregular sea are combined per the
check's ``combine`` rule (see :mod:`checks`): a Gumbel fit over the seed maxima of the utilisation (or the seed
minima of a sign-criterion demand) gives the most probable extreme (MPM) and its 90 % confidence interval; the
status is set on the MPM. The governing seed is the seed with the largest observed utilisation (smallest demand
for a minimum), and its time is the time of the coincident row that set it.

Status: ``PASS`` (utilisation <= 1, or the sign criterion holds), ``FAIL``, ``NOT_EVALUATED`` (reason given).
A missing channel is recorded in ``missing_channel``; the caller fails the step on any such record (plan 8).
"""

from __future__ import annotations

from dataclasses import asdict, dataclass, field
from typing import Any, Iterable

from digitalmodel.drilling_riser.postprocess.channels import MissingChannel, w5
from digitalmodel.drilling_riser.postprocess.checks import CHECKS, U_TOL, CheckValue, NotEvaluated, evaluate_doc
from digitalmodel.drilling_riser.postprocess.extremes import gumbel_fit

STATUSES = ("PASS", "FAIL", "NOT_EVALUATED")


@dataclass
class CaseCheck:
    row_id: str
    status: str
    reason: str
    u: float | None = None
    demand: float | None = None
    capacity: float | None = None
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
        return f"{prefix}demand {_fmt(v.demand)} {unit} {rel} the limit {_fmt(v.capacity)} {unit}"
    rel = "within" if passed else "exceeds"
    return (f"{prefix}demand {_fmt(v.demand)} {unit} {rel} the capacity {_fmt(v.capacity)} {unit} "
            f"(U = {v.u:.3f})")


def _single(row: dict, value: CheckValue, seed: int | None = None) -> CaseCheck:
    status = "PASS" if value.passed else "FAIL"
    return CaseCheck(row_id=row["id"], status=status, reason=_reason(value, bool(value.passed), value.unit),
                     u=value.u, demand=value.demand, capacity=value.capacity, unit=value.unit,
                     location=value.location, seed=seed, time_s=value.time_s, detail=value.detail)


def evaluate_case(docs: dict[int | None, dict], row: dict, ctx: dict, *, seeds_expected: int | None = None) -> CaseCheck:
    """``docs`` maps seed -> results document; a static or regular case has the single key None."""
    rid = row["id"]
    if row.get("check_key") not in CHECKS:
        return CaseCheck(rid, "NOT_EVALUATED", f"no demand function for check_key {row.get('check_key')!r}")
    if seeds_expected and len(docs) < seeds_expected:
        return CaseCheck(rid, "NOT_EVALUATED", f"{len(docs)} of {seeds_expected} seeds hold results; the Gumbel fit "
                                               f"needs every seed")
    values: dict[int | None, CheckValue] = {}
    for seed, doc in sorted(docs.items(), key=lambda kv: (kv[0] is None, kv[0])):
        try:
            values[seed] = evaluate_doc(w5(doc), row, ctx)
        except MissingChannel as e:
            return CaseCheck(rid, "NOT_EVALUATED", str(e), missing_channel=e.channel, seed=seed)
        except NotEvaluated as e:
            return CaseCheck(rid, "NOT_EVALUATED", str(e), seed=seed)
    if list(values) == [None]:
        return _single(row, values[None])
    return _combine(row, values)


def _combine(row: dict, values: dict[int, CheckValue]) -> CaseCheck:
    how = CHECKS[row["check_key"]][1]
    seeds = list(values)
    first = values[seeds[0]]
    if how == "same":
        return _single(row, first, seed=None)
    if how == "mean":
        d = sum(v.demand for v in values.values()) / len(values)
        v = CheckValue(demand=d, capacity=first.capacity, unit=first.unit,
                       u=None if first.capacity is None else abs(d) / first.capacity, location=first.location,
                       detail={"seeds": len(values)})
        c = _single(row, v)
        c.reason = f"mean over {len(values)} seeds: " + c.reason
        return c
    if how == "max":
        us = [values[s].u for s in seeds]
        fit = gumbel_fit(us, kind="max")
        gov = max(seeds, key=lambda s: values[s].u)
        g = values[gov]
        cap = g.capacity
        demand = fit.mpm * cap if cap is not None else None
        passed = fit.mpm <= 1.0 + U_TOL
        v = CheckValue(demand=demand, capacity=cap, unit=g.unit, u=fit.mpm, passed=passed)
        reason = _reason(v, passed, g.unit, prefix=f"Gumbel MPM over {fit.n} seeds: ") + \
            f"; 90 % CI of U {fit.ci_low:.3f}-{fit.ci_high:.3f}"
    else:  # min
        ds = [values[s].demand for s in seeds]
        fit = gumbel_fit(ds, kind="min")
        gov = min(seeds, key=lambda s: values[s].demand)
        g = values[gov]
        passed = fit.mpm > g.capacity
        v = CheckValue(demand=fit.mpm, capacity=g.capacity, unit=g.unit, u=None, passed=passed)
        reason = _reason(v, passed, g.unit, prefix=f"Gumbel MPM of the minimum over {fit.n} seeds: ") + \
            f"; 90 % CI {fit.ci_low:,.4g}-{fit.ci_high:,.4g} {g.unit}"
    return CaseCheck(row_id=row["id"], status="PASS" if passed else "FAIL", reason=reason, u=v.u, demand=v.demand,
                     capacity=v.capacity, unit=g.unit, location=g.location, seed=gov, time_s=g.time_s,
                     stats=fit.as_dict(),
                     detail={**g.detail, "governing_seed_u": g.u, "governing_seed_demand": g.demand,
                             "seed_values": {str(s): (values[s].u if how == "max" else values[s].demand)
                                             for s in seeds}})


def _order_key(c: CaseCheck) -> float:
    if c.u is not None:
        return c.u
    if c.demand is not None and c.capacity is not None:  # sign criterion: the smallest margin governs
        return -(c.demand - c.capacity)
    return float("-inf")


def summarise(results: Iterable[tuple[str, CaseCheck]]) -> dict[str, dict[str, Any]]:
    """Per row: counts by status, governing case (largest utilisation, or smallest margin for a sign criterion),
    row status (FAIL if any case fails, PASS if every evaluated case passes, NOT_EVALUATED if none evaluated)."""
    out: dict[str, dict[str, Any]] = {}
    for case_id, c in results:
        s = out.setdefault(c.row_id, {"counts": {k: 0 for k in STATUSES}, "governing": None, "max_u": None,
                                      "_key": float("-inf")})
        s["counts"][c.status] += 1
        if c.status == "NOT_EVALUATED":
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
