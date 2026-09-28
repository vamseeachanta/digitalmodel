"""Accessors for the ``riser-w5-channels/1`` result schema (see ``global_model/w5_channels.py`` on the W4 runner).

Statics hold one value per quantity (``points.<p>.static``, ``range_graphs.<line>.<var>``, ``stroke.static``).
Dynamics hold statistics over the main stage (``points.<p>.stats``), coincident rows (``points.<p>.extremes`` and
``points.<p>.tm_hull``: every variable at one time step ``t``), range-graph envelopes
(``range_graphs.<line>.<var>_max`` / ``_min``) and ``stroke.stats``. Every accessor raises :class:`MissingChannel`
with the dotted channel name when a channel is absent.
"""

from __future__ import annotations

from typing import Any

SCHEMA = "riser-w5-channels/1"


class MissingChannel(KeyError):
    """A channel needed by a check is absent from a results file."""

    def __init__(self, channel: str):
        super().__init__(channel)
        self.channel = channel

    def __str__(self) -> str:
        return f"missing channel: {self.channel}"


def w5(doc: dict[str, Any]) -> dict[str, Any]:
    try:
        w = doc["channels"]["w5"]
    except (KeyError, TypeError):
        raise MissingChannel("channels.w5") from None
    if w.get("schema") not in (None, SCHEMA):
        raise ValueError(f"unsupported channel schema {w.get('schema')!r}")
    return w


def is_dynamic(w: dict[str, Any]) -> bool:
    return w.get("analysis") == "dynamics"


def _point(w: dict[str, Any], point: str) -> dict[str, Any]:
    try:
        return w["points"][point]
    except KeyError:
        raise MissingChannel(f"points.{point}") from None


def point_value(w: dict[str, Any], point: str, var: str, stat: str) -> float:
    """``stat`` = ``max``, ``min`` or ``mean`` over the main stage; statics return the static value."""
    p = _point(w, point)
    try:
        return float(p["stats"][var][stat]) if is_dynamic(w) else float(p["static"][var])
    except KeyError:
        where = f"stats.{var}.{stat}" if is_dynamic(w) else f"static.{var}"
        raise MissingChannel(f"points.{point}.{where}") from None


def point_rows(w: dict[str, Any], point: str) -> list[dict[str, Any]]:
    """Coincident load rows (same time step, same location). Statics: the one static row, ``t`` = None."""
    p = _point(w, point)
    if not is_dynamic(w):
        if "static" not in p:
            raise MissingChannel(f"points.{point}.static")
        return [{**p["static"], "t": None}]
    if "extremes" not in p:
        raise MissingChannel(f"points.{point}.extremes")
    return list(p["extremes"]) + list(p.get("tm_hull") or [])


def extreme_row(w: dict[str, Any], point: str, driver: str, kind: str) -> dict[str, Any] | None:
    """The coincident row at the extreme of ``driver`` (dynamics), or None."""
    if not is_dynamic(w):
        return None
    for r in _point(w, point).get("extremes", []):
        if r.get("driver") == driver and r.get("kind") == kind:
            return r
    return None


def range_extreme(w: dict[str, Any], line: str, var: str, kind: str) -> tuple[float, float | None]:
    """(value, arc length) of the largest (``kind`` = max) or smallest range-graph value along ``line``.
    Dynamics read ``<var>_max`` / ``<var>_min``; statics read ``<var>``."""
    try:
        rg = w["range_graphs"][line]
    except KeyError:
        raise MissingChannel(f"range_graphs.{line}") from None
    key = f"{var}_{kind}" if is_dynamic(w) else var
    if key not in rg:
        raise MissingChannel(f"range_graphs.{line}.{key}")
    vals = rg[key]
    axis = rg.get((rg.get("axis_of") or {}).get(key, "arc1_m")) or rg.get("arc1_m") or []
    i = max(range(len(vals)), key=lambda j: vals[j]) if kind == "max" else min(range(len(vals)),
                                                                               key=lambda j: vals[j])
    return float(vals[i]), (float(axis[i]) if i < len(axis) else None)


def stroke_range(w: dict[str, Any]) -> tuple[float, float, float]:
    """(min, max, mean) of the telescopic-joint stroke channel; statics give the static value three times."""
    s = w.get("stroke")
    if s is None:
        raise MissingChannel("stroke")
    if is_dynamic(w):
        try:
            return float(s["stats"]["min"]), float(s["stats"]["max"]), float(s["stats"]["mean"])
        except KeyError:
            raise MissingChannel("stroke.stats") from None
    if "static" not in s:
        raise MissingChannel("stroke.static")
    v = float(s["static"])
    return v, v, v


def contents(w: dict[str, Any]) -> dict[str, float]:
    c = w.get("contents")
    if not c or "density_kg_m3" not in c or "pressure_ref_z_m" not in c:
        raise MissingChannel("contents")
    return c
