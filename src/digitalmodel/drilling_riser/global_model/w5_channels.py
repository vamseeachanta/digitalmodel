"""W5 channel set of one solved riser case (plan Section 7.2), read from the reopened result.

Everything the W5 post-processing needs is taken here, so that the ``.sim`` file can be deleted once the
extraction is verified (owner decision W403):

* **points** - the upper and lower flex joints (``ufj``, ``lfj``: Ez angle), the riser top below the tension ring
  (``riser_top``), and every stack section boundary from the wellhead datum up to the lower flex-joint pivot
  (``stack:datum``, ``stack:<lower>|<upper>``, ``stack:top``; the wellhead, BOP and LMRP connectors), plus the
  conductor top when the model has a foundation. Per point: the signed load vector - effective tension ``te``,
  wall tension ``tw``, bend moments ``mx``, ``my`` and their resultant ``m``, shear ``sx``, ``sy``, internal and
  external pressure ``pi``, ``po``. Statics: the static values. Dynamics (the main stage, build-up excluded):
  statistics per channel, the rows coincident with the maximum and minimum of ``te``, ``tw``, ``m`` (and the flex-
  joint angle) - same time step, same location - and, at the stack points and the flex joints, the rows at the
  vertices of the convex hull of the (tension, moment) samples for both ``te`` and ``tw``. For a connector capacity
  curve that is convex in tension-moment at the recorded pressure, the governing coincident load lies on that hull.
* **range_graphs** along InnerBarrel, Riser, Stack (and Conductor): arc length, static elevation, effective and
  wall tension, resultant bend moment, von Mises stress (riser and inner barrel); min/max over the main stage.
* **stroke** - the telescopic-joint (slip-joint constraint) z displacement; **ring** - tension-ring position and
  yaw; **tensioners** - each line tension; **vessel** - surge/sway.

Units are those of OrcaFlex: kN, kN.m, kPa, m, deg (``UNITS``). Values are rounded to 6 significant digits.
"""

from __future__ import annotations

import math
from typing import Any, Iterable, Sequence

SCHEMA = "riser-w5-channels/1"
SIG = 6
UNITS = {"te": "kN", "tw": "kN", "mx": "kN.m", "my": "kN.m", "m": "kN.m", "sx": "kN", "sy": "kN", "pi": "kPa",
         "po": "kPa", "ez_angle_deg": "deg", "vm": "kPa", "arc_m": "m", "z_m": "m", "stroke": "m", "t": "s"}
LOAD_VARS = {"te": "Effective tension", "tw": "Wall tension", "mx": "x bend moment", "my": "y bend moment",
             "m": "Bend moment", "sx": "x shear force", "sy": "y shear force", "pi": "Internal pressure",
             "po": "External pressure"}
ANGLE_VAR = "Ez angle"
DRIVERS = ("te", "tw", "m", "ez_angle_deg")
REQUIRED_POINTS = ("ufj", "riser_top", "lfj")
REQUIRED_LINES = ("InnerBarrel", "Riser", "Stack")


# --------------------------------------------------------------------------- pure helpers


def stats(values: Iterable[float]) -> dict[str, float]:
    v = [float(x) for x in values]
    m = sum(v) / len(v)
    return {"min": min(v), "max": max(v), "mean": m, "std": math.sqrt(sum((x - m) ** 2 for x in v) / len(v)),
            "n": len(v)}


def _sig(x: float, digits: int = SIG) -> float:
    if x == 0 or not math.isfinite(x):
        return float(x)
    return float(f"{x:.{digits - 1}e}")


def compact(obj: Any, digits: int = SIG) -> Any:
    """Round every float to ``digits`` significant digits (ints, strings and None unchanged)."""
    if isinstance(obj, bool) or obj is None or isinstance(obj, (int, str)):
        return obj
    if isinstance(obj, float):
        return _sig(obj, digits)
    if isinstance(obj, dict):
        return {k: compact(v, digits) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)):
        return [compact(v, digits) for v in obj]
    return obj


def _row(t: Sequence[float], series: dict[str, Sequence[float]], i: int) -> dict[str, float]:
    return {"t": float(t[i]), **{k: float(v[i]) for k, v in series.items()}}


def extreme_rows(t: Sequence[float], series: dict[str, Sequence[float]],
                 drivers: Iterable[str] = DRIVERS) -> list[dict[str, Any]]:
    """For each driver channel present, the rows (all channels, same sample) at its maximum and minimum."""
    rows = []
    for d in drivers:
        if d not in series:
            continue
        v = series[d]
        for kind, i in (("max", max(range(len(v)), key=v.__getitem__)), ("min", min(range(len(v)), key=v.__getitem__))):
            rows.append({"driver": d, "kind": kind, **_row(t, series, i)})
    return rows


def convex_hull_indices(points: Sequence[tuple[float, float]]) -> list[int]:
    """Indices of the convex-hull vertices (Andrew's monotone chain; collinear points dropped)."""
    idx = sorted(range(len(points)), key=lambda i: (points[i][0], points[i][1]))
    if len(idx) <= 2:
        return idx

    def cross(o, a, b):
        return ((points[a][0] - points[o][0]) * (points[b][1] - points[o][1])
                - (points[a][1] - points[o][1]) * (points[b][0] - points[o][0]))

    lower: list[int] = []
    for i in idx:
        while len(lower) >= 2 and cross(lower[-2], lower[-1], i) <= 0:
            lower.pop()
        lower.append(i)
    upper: list[int] = []
    for i in reversed(idx):
        while len(upper) >= 2 and cross(upper[-2], upper[-1], i) <= 0:
            upper.pop()
        upper.append(i)
    return lower[:-1] + upper[:-1]


def tm_hull_rows(t: Sequence[float], series: dict[str, Sequence[float]]) -> list[dict[str, float]]:
    """Coincident rows at the convex-hull vertices of (tension, resultant moment), for ``te`` and ``tw``.

    The resultant moment is recomputed from ``mx``, ``my`` when ``m`` is absent. Rows are unique and time-ordered.
    """
    s = dict(series)
    if "m" not in s:
        s["m"] = [math.hypot(a, b) for a, b in zip(s["mx"], s["my"])]
    keep: set[int] = set()
    for tk in ("te", "tw"):
        if tk in s:
            # scale both axes to unit range so the hull is not distorted by the units
            a, b = s[tk], s["m"]
            ra = (max(a) - min(a)) or 1.0
            rb = (max(b) - min(b)) or 1.0
            keep.update(convex_hull_indices([(x / ra, y / rb) for x, y in zip(a, b)]))
    return [_row(t, s, i) for i in sorted(keep)]


def missing_channels(doc: dict, *, foundation: bool) -> list[str]:
    """Required W5 keys absent from an extraction document (empty list = complete)."""
    miss = []
    pts = doc.get("points", {})
    for p in REQUIRED_POINTS:
        if p not in pts:
            miss.append(f"points.{p}")
    if not any(k.startswith("stack:") for k in pts):
        miss.append("points.stack:*")
    dyn = doc.get("analysis") == "dynamics"
    for p in ("ufj", "lfj"):
        if p in pts:
            src = pts[p].get("stats" if dyn else "static", {})
            if "ez_angle_deg" not in src:
                miss.append(f"points.{p}.ez_angle_deg")
    if dyn:
        for k, p in pts.items():
            if k.startswith("stack:") and not p.get("tm_hull"):
                miss.append(f"points.{k}.tm_hull")
    rg = doc.get("range_graphs", {})
    for line in REQUIRED_LINES + (("Conductor",) if foundation else ()):
        if line not in rg:
            miss.append(f"range_graphs.{line}")
    for k in ("stroke", "ring", "tensioners"):
        if k not in doc:
            miss.append(k)
    return miss


# --------------------------------------------------------------------------- OrcaFlex extraction


def _stack_points(spec) -> list[tuple[str, float]]:
    """(name, arc length from the wellhead datum) of every stack section boundary (the stack runs up from End A)."""
    secs = list(reversed(spec.stack))
    out, arc = [("stack:datum", 0.0)], 0.0
    for lo, hi in zip(secs, secs[1:]):
        arc += lo.length_m
        out.append((f"stack:{lo.name}|{hi.name}", arc))
    out.append(("stack:top", arc + secs[-1].length_m))
    return out


def _points(spec, ofx) -> list[tuple[str, str, Any, bool]]:
    """(name, line, objectExtra, has flex-joint angle)."""
    pts = [("ufj", "InnerBarrel", ofx.oeEndA, True), ("riser_top", "Riser", ofx.oeEndA, False),
           ("lfj", "Riser", ofx.oeEndB, True)]
    pts += [(n, "Stack", ofx.oeArcLength(a), False) for n, a in _stack_points(spec)]
    if spec.foundation is not None:
        pts.append(("conductor:top", "Conductor", ofx.oeEndB, False))
    return pts


def _line_vars(line, extra, period, *, angle: bool, static: bool, ofx) -> dict[str, Any]:
    names = dict(LOAD_VARS)
    if angle:
        names["ez_angle_deg"] = ANGLE_VAR
    if static:
        return {k: float(line.StaticResult(v, extra)) for k, v in names.items()}
    return {k: [float(x) for x in line.TimeHistory(v, period, extra)] for k, v in names.items()}


RANGE = {
    "InnerBarrel": (("te", "Effective tension", "minmax"), ("m", "Bend moment", "max"),
                    ("vm", "Max von Mises stress", "max")),
    "Riser": (("te", "Effective tension", "minmax"), ("tw", "Wall tension", "minmax"), ("m", "Bend moment", "max"),
              ("vm", "Max von Mises stress", "max")),
    "Stack": (("te", "Effective tension", "minmax"), ("tw", "Wall tension", "minmax"), ("m", "Bend moment", "max")),
    "Conductor": (("te", "Effective tension", "minmax"), ("m", "Bend moment", "max"), ("s", "Shear force", "max")),
}


def _range_graph(line, period, static_period, *, static: bool, rows) -> dict[str, Any]:
    """Range graphs of one line. Node and segment variables sit on different arc-length grids: each distinct grid
    is stored once under ``axes`` and ``axis_of`` names the grid of every value array (``arc_m`` = the node grid)."""
    z = line.RangeGraph("Z", static_period)
    node = [float(x) for x in z.X]
    axes: dict[str, list[float]] = {"arc_m": node}
    out: dict[str, Any] = {"arc_m": node, "z_m": [float(x) for x in z.Mean], "axis_of": {"z_m": "arc_m"}}

    def axis(xs) -> str:
        xs = [float(x) for x in xs]
        for name, a in axes.items():
            if len(a) == len(xs) and all(abs(p - q) < 1e-9 for p, q in zip(a, xs)):
                return name
        name = f"arc{len(axes)}_m"
        axes[name] = xs
        out[name] = xs
        return name

    for key, var, kind in rows:
        rg = line.RangeGraph(var, static_period if static else period)
        ax = axis(rg.X)
        if static:
            out[key] = [float(x) for x in rg.Mean]
            out["axis_of"][key] = ax
        elif kind == "minmax":
            out[f"{key}_min"] = [float(x) for x in rg.Min]
            out[f"{key}_max"] = [float(x) for x in rg.Max]
            out["axis_of"][f"{key}_min"] = out["axis_of"][f"{key}_max"] = ax
        else:
            out[f"{key}_max"] = [float(x) for x in rg.Max]
            out["axis_of"][f"{key}_max"] = ax
    return out


def extract(model, spec, analysis: str, ofx) -> dict[str, Any]:
    """The W5 channel set of a solved (reopened) model: ``analysis`` = ``statics`` or ``dynamics``."""
    static = analysis == "statics"
    sp = ofx.Period(ofx.pnStaticState)
    period = sp if static else ofx.Period(1)  # stage 1 = the main stage after the build-up
    doc: dict[str, Any] = {"schema": SCHEMA, "analysis": analysis, "units": UNITS, "points": {}, "range_graphs": {}}
    t = None if static else [float(x) for x in model.SampleTimes(period)]
    if t is not None:
        doc["time"] = {"start_s": t[0], "end_s": t[-1], "samples": len(t), "dt_s": (t[-1] - t[0]) / max(1, len(t) - 1)}
    for name, line_name, extra, angle in _points(spec, ofx):
        vals = _line_vars(model[line_name], extra, period, angle=angle, static=static, ofx=ofx)
        if static:
            doc["points"][name] = {"line": line_name, "static": vals}
            continue
        pt = {"line": line_name, "stats": {k: stats(v) for k, v in vals.items()},
              "extremes": extreme_rows(t, vals)}
        if name.startswith("stack:") or name in ("lfj", "ufj", "conductor:top"):
            pt["tm_hull"] = tm_hull_rows(t, vals)
        doc["points"][name] = pt
    lines = list(REQUIRED_LINES) + (["Conductor"] if spec.foundation is not None else [])
    for ln in lines:
        doc["range_graphs"][ln] = _range_graph(model[ln], period, sp, static=static, rows=RANGE[ln])
    slip, ring = model["SlipJoint"], model["TensionRing"]
    if static:
        doc["stroke"] = {"static": float(slip.StaticResult("z"))}
        doc["ring"] = {"static": {k: float(ring.StaticResult(v)) for k, v in
                                  (("x", "X"), ("y", "Y"), ("z", "Z"), ("yaw_deg", "Rotation 3"))}}
        doc["tensioners"] = {o.name: {"static": float(o.StaticResult("Tension"))}
                             for o in model.objects if o.typeName == "Winch" and o.name.startswith("Tensioner")}
        v = model[spec.vessel_name]
        doc["vessel"] = {"static": {"x": float(v.InitialX), "y": float(v.InitialY)}}
    else:
        doc["stroke"] = {"stats": stats(slip.TimeHistory("z", period))}
        doc["ring"] = {"stats": {k: stats(ring.TimeHistory(v, period)) for k, v in
                                 (("x", "X"), ("y", "Y"), ("z", "Z"), ("yaw_deg", "Rotation 3"))}}
        doc["tensioners"] = {o.name: {"stats": stats(o.TimeHistory("Tension", period))}
                             for o in model.objects if o.typeName == "Winch" and o.name.startswith("Tensioner")}
        v = model[spec.vessel_name]
        doc["vessel"] = {"stats": {"x": stats(v.TimeHistory("X", period)), "y": stats(v.TimeHistory("Y", period))}}
    doc["contents"] = {"density_kg_m3": spec.contents.density_kg_m3, "pressure_ref_z_m": spec.contents.pressure_ref_z_m}
    return compact(doc)
