"""Trade-space analysis for floating-wind concept screening (issue #1026).

Turn a screening result set (issue #1024) of hundreds-thousands of variants into
a decision:

* :func:`pareto_front` -- the non-dominated set over chosen objectives
  (e.g. minimise steel mass *and* peak motion *and* maximise robustness margin).
* :func:`correlation_map` -- Pearson correlations between the swept parameters
  and the response metrics, exposing which knobs drive which responses.

Both consume :class:`digitalmodel.floating_wind.screening.ScreeningResults` (or a
plain list of variants) and operate on a flat per-variant **metric record** that
merges the swept parameters with the derived responses. Licence-free.
"""

from __future__ import annotations

import math
from typing import Any, Iterable, Literal

from pydantic import BaseModel, Field

from digitalmodel.floating_wind.screening import ScreeningResults, VariantScreening

__all__ = [
    "Objective",
    "variant_metrics",
    "metric_records",
    "pareto_front",
    "correlation_map",
]

Direction = Literal["min", "max"]


class Objective(BaseModel):
    """One trade-space objective: a metric key and the direction that is better."""

    key: str
    direction: Direction = "min"


def variant_metrics(v: VariantScreening) -> dict[str, Any]:
    """Flatten one variant to a metric record (swept params + responses).

    Parameter columns are prefixed ``param_`` to avoid clashing with response
    metric names. Non-finite periods (statically unstable / rigid) are dropped
    to ``None`` so they neither dominate nor anchor correlations.
    """
    p = v.properties

    def _finite(x: float) -> float | None:
        return x if math.isfinite(x) else None

    rec: dict[str, Any] = {
        "case_id": v.case_id,
        "archetype": v.archetype.value,
        "feasible": v.feasible,
        "passed": v.passed,
        "displacement_t": p.displacement_t,
        "steel_mass_t": p.steel_mass_t,
        "ballast_mass_t": p.ballast_mass_t,
        "GM_m": p.GM_m,
        "heave_period_s": _finite(p.heave_natural_period_s),
        "pitch_period_s": _finite(p.pitch_natural_period_s),
        "governing_margin": v.governing_margin,
    }
    for k, val in v.params.items():
        rec[f"param_{k}"] = val
    return rec


def _variants(
    results: ScreeningResults | Iterable[VariantScreening],
) -> list[VariantScreening]:
    if isinstance(results, ScreeningResults):
        return list(results.variants)
    return list(results)


def metric_records(
    results: ScreeningResults | Iterable[VariantScreening],
    *,
    feasible_only: bool = True,
) -> list[dict[str, Any]]:
    """Flat metric records for all (optionally feasible-only) variants."""
    variants = _variants(results)
    if feasible_only:
        variants = [v for v in variants if v.feasible]
    return [variant_metrics(v) for v in variants]


def _dominates(a: dict, b: dict, objectives: list[Objective]) -> bool:
    """True if record ``a`` Pareto-dominates ``b`` over ``objectives``.

    ``a`` dominates ``b`` if it is no worse on every objective and strictly
    better on at least one. Records missing a value on any objective cannot
    dominate (a missing response is treated as not-comparable-favourable).
    """
    not_worse_all = True
    strictly_better_any = False
    for obj in objectives:
        av, bv = a.get(obj.key), b.get(obj.key)
        if av is None:
            return False
        if bv is None:
            # b is undefined on this objective; a is at least as good here.
            strictly_better_any = True
            continue
        if obj.direction == "min":
            better = av < bv
            worse = av > bv
        else:
            better = av > bv
            worse = av < bv
        if worse:
            not_worse_all = False
            break
        if better:
            strictly_better_any = True
    return not_worse_all and strictly_better_any


def pareto_front(
    results: ScreeningResults | Iterable[VariantScreening],
    objectives: list[Objective] | list[tuple[str, str]] | list[dict],
    *,
    passed_only: bool = False,
) -> list[dict[str, Any]]:
    """Return the non-dominated metric records over ``objectives``.

    ``objectives`` accepts :class:`Objective` instances, ``(key, direction)``
    tuples or ``{"key", "direction"}`` dicts. By default the front is taken over
    all *feasible* variants; set ``passed_only`` to restrict to variants that
    passed every screening check.
    """
    objs = _coerce_objectives(objectives)
    variants = _variants(results)
    variants = [v for v in variants if v.feasible]
    if passed_only:
        variants = [v for v in variants if v.passed]
    records = [variant_metrics(v) for v in variants]

    front: list[dict[str, Any]] = []
    for i, rec in enumerate(records):
        dominated = any(
            _dominates(other, rec, objs)
            for j, other in enumerate(records)
            if j != i
        )
        if not dominated:
            front.append(rec)
    return front


def _coerce_objectives(objectives) -> list[Objective]:
    out: list[Objective] = []
    for o in objectives:
        if isinstance(o, Objective):
            out.append(o)
        elif isinstance(o, dict):
            out.append(Objective(**o))
        else:  # (key, direction) tuple/list
            key, direction = o
            out.append(Objective(key=key, direction=direction))
    return out


def correlation_map(
    results: ScreeningResults | Iterable[VariantScreening],
    *,
    columns: list[str] | None = None,
    feasible_only: bool = True,
) -> dict[str, dict[str, float]]:
    """Pearson correlation matrix over the numeric metric columns.

    Returns a nested dict ``{col: {col2: r}}``. Columns that are constant across
    the variant set (zero variance, e.g. a fixed parameter) are dropped, since
    their correlation is undefined. By default every numeric column is used;
    pass ``columns`` to restrict (e.g. swept params vs a single response).
    """
    records = metric_records(results, feasible_only=feasible_only)
    if not records:
        return {}

    numeric = _numeric_columns(records, columns)
    series = {
        col: [r[col] for r in records]
        for col in numeric
        if all(isinstance(r.get(col), (int, float)) and r.get(col) is not None for r in records)
    }
    # Drop zero-variance columns.
    series = {col: vals for col, vals in series.items() if _variance(vals) > 0.0}

    cols = list(series)
    out: dict[str, dict[str, float]] = {a: {} for a in cols}
    for a in cols:
        for b in cols:
            out[a][b] = _pearson(series[a], series[b])
    return out


def _numeric_columns(records: list[dict], columns: list[str] | None) -> list[str]:
    if columns is not None:
        return columns
    cols: list[str] = []
    skip = {"case_id", "archetype", "feasible", "passed"}
    for col in records[0]:
        if col in skip:
            continue
        cols.append(col)
    return cols


def _variance(xs: list[float]) -> float:
    n = len(xs)
    if n < 2:
        return 0.0
    mean = sum(xs) / n
    return sum((x - mean) ** 2 for x in xs) / (n - 1)


def _pearson(xs: list[float], ys: list[float]) -> float:
    n = len(xs)
    mx, my = sum(xs) / n, sum(ys) / n
    cov = sum((x - mx) * (y - my) for x, y in zip(xs, ys))
    sx = math.sqrt(sum((x - mx) ** 2 for x in xs))
    sy = math.sqrt(sum((y - my) ** 2 for y in ys))
    if sx == 0.0 or sy == 0.0:
        return float("nan")
    return cov / (sx * sy)
