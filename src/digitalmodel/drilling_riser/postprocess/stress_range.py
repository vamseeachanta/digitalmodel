"""Outer-fibre axial stress range along the riser (owner decision W501; API RP 16Q 1993 Table 3.1 note [5]).

The channel is read from a solved (reopened) OrcaFlex result, so it can be added to results already extracted while
the ``.sim`` file is kept. OrcaFlex ``ZZ stress`` at the outer fibre is the axial wall stress plus the bending stress
at circumferential position ``theta``. For each ``theta`` a range graph over the main stage gives the minimum and
maximum along the line; the double-amplitude range at an arc length is the largest ``max - min`` over the ``theta``
values (24 positions, 15 deg apart: the bending part is under-read by at most 1 - cos 7.5 deg = 0.9 %).

The result is a supplementary document (schema ``riser-w5-stress-range/1``) keyed to the ledger record it was
read from; :func:`merge_stress_range` adds ``range_graphs.<line>.zz_range_max`` (kPa) to the W5 channel document.
"""

from __future__ import annotations

from typing import Any, Iterable, Mapping, Sequence

SCHEMA = "riser-w5-stress-range/1"
VAR = "ZZ stress"
THETAS_DEG = tuple(float(x) for x in range(0, 360, 15))
LINES = ("Riser",)
AXIS = "arc_zz_m"
KEY = "zz_range_max"


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
    """Add the stress range of ``supp`` to the W5 channel document ``doc`` (``channels.w5``) in place."""
    if supp.get("schema") != SCHEMA:
        raise ValueError(f"unsupported stress-range schema {supp.get('schema')!r}")
    rgs = doc["channels"]["w5"].setdefault("range_graphs", {})
    for ln, s in supp["lines"].items():
        rg = rgs.setdefault(ln, {})
        rg[AXIS] = list(s["arc_m"])
        rg[KEY] = list(s[KEY])
        rg.setdefault("axis_of", {})[KEY] = AXIS
    return doc


__all__ = ["KEY", "LINES", "SCHEMA", "THETAS_DEG", "extract", "merge_stress_range", "range_over_theta",
           "stress_range_limit_ksi"]
