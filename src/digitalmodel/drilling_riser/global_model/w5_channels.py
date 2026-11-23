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


def _main(model, spec):
    from .orcaflex_run import main_period

    return main_period(model, spec)


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


OW_REQUIRED_POINTS = ("frame", "rotary", "edp")
OW_REQUIRED_LINES = ("Upper", "Riser", "Stack")


def _missing_open_water(doc: dict, *, foundation: bool) -> list[str]:
    miss = []
    pts = doc.get("points", {})
    for p in OW_REQUIRED_POINTS:
        if p not in pts:
            miss.append(f"points.{p}")
    if not any(k.startswith("stack:") for k in pts):
        miss.append("points.stack:*")
    if doc.get("analysis") == "dynamics":
        for k, p in pts.items():
            if (k.startswith("stack:") or k == "edp") and not p.get("tm_hull"):
                miss.append(f"points.{k}.tm_hull")
    rg = doc.get("range_graphs", {})
    for line in OW_REQUIRED_LINES + (("Conductor",) if foundation else ()):
        if line not in rg:
            miss.append(f"range_graphs.{line}")
    for k in ("frame", "top_tension", "stroke"):
        if k not in doc:
            miss.append(k)
    return miss


HO_REQUIRED_POINTS = ("ufj", "riser_top", "riser_bottom")


def _missing_hang_off(doc: dict) -> list[str]:
    miss = []
    pts = doc.get("points", {})
    for p in HO_REQUIRED_POINTS:
        if p not in pts:
            miss.append(f"points.{p}")
    lmrp = bool(doc.get("with_lmrp"))
    if lmrp and not any(k.startswith("stack:") for k in pts):
        miss.append("points.stack:*")
    if doc.get("analysis") == "dynamics":
        for k, p in pts.items():
            if (k.startswith("stack:") or k in ("ufj", "riser_bottom")) and not p.get("tm_hull"):
                miss.append(f"points.{k}.tm_hull")
    rg = doc.get("range_graphs", {})
    for line in ("InnerBarrel", "Riser") + (("Stack",) if lmrp else ()):
        if line not in rg:
            miss.append(f"range_graphs.{line}")
    for k in ("stroke", "ring", "top_load"):
        if k not in doc:
            miss.append(k)
    return miss


def missing_channels(doc: dict, *, foundation: bool) -> list[str]:
    """Required W5 keys absent from an extraction document (empty list = complete)."""
    if doc.get("riser_kind") == "open_water":
        return _missing_open_water(doc, foundation=foundation)
    if doc.get("riser_kind") == "hang_off":
        return _missing_hang_off(doc)
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


ABOVE_M = 1.0e-3  # arc-length step into the segment above a stack connector node


def _points(spec, ofx) -> list[tuple[str, str, Any, bool, Any]]:
    """(name, line, objectExtra, has flex-joint angle, objectExtra of the segment above a connector node or None).

    A stack connector sits on a node, where the segment tension jumps by the lumped node weight and the value OrcaFlex
    reports exactly at the node depends on the rounding of the arc length. The point is therefore read 1 mm below the
    node (the load vector, ``te``/``tw`` of the segment below) and 1 mm above (``te_above``/``tw_above``, the segment
    above); the interface tension lies between the two.
    """
    pts = [("ufj", "InnerBarrel", ofx.oeEndA, True, None), ("riser_top", "Riser", ofx.oeEndA, False, None),
           ("lfj", "Riser", ofx.oeEndB, True, None)]
    # recoil model: the LMRP is its own line on the BOP top, the stack line ends at the BOP top
    stack_spec = spec if spec.recoil is None else spec.model_copy(update={"stack": spec.stack[1:]})
    for n, a in _stack_points(stack_spec):
        if "|" in n:
            pts.append((n, "Stack", ofx.oeArcLength(a - ABOVE_M), False, ofx.oeArcLength(a + ABOVE_M)))
        else:
            pts.append((n, "Stack", ofx.oeEndA if n == "stack:datum" else ofx.oeEndB, False, None))
    if spec.recoil is not None:
        from .events import LMRP_LINE

        pts += [("lmrp:bottom", LMRP_LINE, ofx.oeEndA, False, None), ("lmrp:top", LMRP_LINE, ofx.oeEndB, False, None)]
    if spec.foundation is not None:
        pts.append(("conductor:top", "Conductor", ofx.oeEndB, False, None))
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
    period = sp if static else _main(model, spec)  # the main stage after the build-up
    doc: dict[str, Any] = {"schema": SCHEMA, "analysis": analysis, "units": UNITS, "points": {}, "range_graphs": {}}
    t = None if static else [float(x) for x in model.SampleTimes(period)]
    if t is not None:
        doc["time"] = {"start_s": t[0], "end_s": t[-1], "samples": len(t), "dt_s": (t[-1] - t[0]) / max(1, len(t) - 1)}
    for name, line_name, extra, angle, above in _points(spec, ofx):
        line = model[line_name]
        vals = _line_vars(line, extra, period, angle=angle, static=static, ofx=ofx)
        if above is not None:
            for key, var in (("te_above", "Effective tension"), ("tw_above", "Wall tension")):
                vals[key] = (float(line.StaticResult(var, above)) if static
                             else [float(x) for x in line.TimeHistory(var, period, above)])
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
    if not static and (spec.vessel_trajectory is not None or spec.recoil is not None):
        doc["event_series"] = event_series(model, spec, ofx)
    if not static and spec.recoil is not None:
        doc["recoil"] = recoil_summary(model, spec, ofx)
    return compact(doc)


# --------------------------------------------------------------------------- open-water (C2) riser

OW_RANGE = {
    "Upper": (("te", "Effective tension", "minmax"), ("tw", "Wall tension", "minmax"), ("m", "Bend moment", "max"),
              ("vm", "Max von Mises stress", "max")),
    "Riser": RANGE["Riser"],
    "Stack": RANGE["Stack"],
    "Conductor": RANGE["Conductor"],
}


def open_water_points(spec) -> list[tuple[str, str, Any, bool, Any]]:
    """(name, line, where, has angle, arc of the segment above or None) of the open-water W5 points; ``where`` is
    ``"A"``/``"B"`` (line end) or an arc length. Points: the string top below the frame (``frame``), the rotary
    (``rotary``), every riser section boundary (``riser:<upper>|<lower>``, the stress-joint base among them), the EDP /
    LRP interface (``edp``), the stack boundaries as the drilling riser, and the conductor top."""
    pts: list[tuple[str, str, Any, bool, Any]] = [("frame", "Upper", "A", True, None), ("rotary", "Riser", "A", True, None)]
    arc = 0.0
    for hi, lo in zip(spec.riser, spec.riser[1:]):
        arc += hi.length_m
        pts.append((f"riser:{hi.name}|{lo.name}", "Riser", arc, True, None))
    pts.append(("edp", "Riser", "B", True, None))
    for n, a in _stack_points(spec):
        if "|" in n:
            pts.append((n, "Stack", a - ABOVE_M, False, a + ABOVE_M))
        else:
            pts.append((n, "Stack", "A" if n == "stack:datum" else "B", False, None))
    if spec.foundation is not None:
        pts.append(("conductor:top", "Conductor", "B", False, None))
    return pts


def _extra(where, ofx):
    if where == "A":
        return ofx.oeEndA
    if where == "B":
        return ofx.oeEndB
    return ofx.oeArcLength(float(where))


def extract_open_water(model, spec, analysis: str, ofx) -> dict[str, Any]:
    """The W5 channel set of a solved open-water (C2) riser case - the drilling-riser set with the tension frame,
    rotary, riser boundaries and EDP in place of the flex joints and ring. ``stroke`` is the tension-frame elevation
    relative to the vessel point at the frame (the tensioner / tension-joint stroke, CR-59)."""
    static = analysis == "statics"
    sp = ofx.Period(ofx.pnStaticState)
    period = sp if static else ofx.Period(1)
    doc: dict[str, Any] = {"schema": SCHEMA, "riser_kind": "open_water", "analysis": analysis, "units": UNITS,
                           "points": {}, "range_graphs": {}}
    t = None if static else [float(x) for x in model.SampleTimes(period)]
    if t is not None:
        doc["time"] = {"start_s": t[0], "end_s": t[-1], "samples": len(t), "dt_s": (t[-1] - t[0]) / max(1, len(t) - 1)}
    for name, line_name, where, angle, above in open_water_points(spec):
        line = model[line_name]
        vals = _line_vars(line, _extra(where, ofx), period, angle=angle, static=static, ofx=ofx)
        if above is not None:
            ex = ofx.oeArcLength(above)
            for key, var in (("te_above", "Effective tension"), ("tw_above", "Wall tension")):
                vals[key] = (float(line.StaticResult(var, ex)) if static
                             else [float(x) for x in line.TimeHistory(var, period, ex)])
        if static:
            doc["points"][name] = {"line": line_name, "static": vals}
            continue
        pt = {"line": line_name, "stats": {k: stats(v) for k, v in vals.items()}, "extremes": extreme_rows(t, vals)}
        if name.startswith(("stack:", "riser:")) or name in ("frame", "rotary", "edp", "conductor:top"):
            pt["tm_hull"] = tm_hull_rows(t, vals)
        doc["points"][name] = pt
    lines = list(OW_REQUIRED_LINES) + (["Conductor"] if spec.foundation is not None else [])
    for ln in lines:
        doc["range_graphs"][ln] = _range_graph(model[ln], period, sp, static=static, rows=OW_RANGE[ln])
    frame, vessel, winch = model["TensionFrame"], model[spec.vessel_name], model["TopTensioner"]
    at_frame = ofx.oeVessel((0.0, 0.0, spec.tension_frame.z_m))
    if static:
        fz = float(frame.StaticResult("Z"))
        doc["frame"] = {"static": {k: float(frame.StaticResult(v)) for k, v in (("x", "X"), ("y", "Y"), ("z", "Z"))}}
        doc["stroke"] = {"static": fz - float(vessel.StaticResult("Z", at_frame))}
        doc["top_tension"] = {"static": float(winch.StaticResult("Tension"))}
        doc["vessel"] = {"static": {"x": float(vessel.InitialX), "y": float(vessel.InitialY)}}
    else:
        fz = [float(x) for x in frame.TimeHistory("Z", period)]
        vz = [float(x) for x in vessel.TimeHistory("Z", period, at_frame)]
        doc["frame"] = {"stats": {k: stats(frame.TimeHistory(v, period)) for k, v in (("x", "X"), ("y", "Y"), ("z", "Z"))}}
        doc["stroke"] = {"stats": stats([a - b for a, b in zip(fz, vz)])}
        doc["top_tension"] = {"stats": stats(winch.TimeHistory("Tension", period))}
        doc["vessel"] = {"stats": {"x": stats(vessel.TimeHistory("X", period)), "y": stats(vessel.TimeHistory("Y", period))}}
    doc["contents"] = {"density_kg_m3": spec.contents.density_kg_m3, "pressure_ref_z_m": spec.contents.pressure_ref_z_m}
    return compact(doc)


# --------------------------------------------------------------------------- events (drift-off, recoil)

EVENT_SERIES_DT_S = 1.0


def _downsample(t: Sequence[float], v: Sequence[float], dt: float) -> list[float]:
    out, nxt = [], t[0]
    for a, b in zip(t, v):
        if a >= nxt - 1e-9:
            out.append(float(b))
            nxt += dt
    return out


def event_series(model, spec, ofx, *, dt_s: float = EVENT_SERIES_DT_S) -> dict[str, Any]:
    """Event runs: downsampled histories (every ``dt_s`` over the main stage) of the vessel offset and the
    responses that set the watch circles and the recoil checks - the W5 red-offset search reads them."""
    period = _main(model, spec)
    t = [float(x) for x in model.SampleTimes(period)]
    v, ib, riser = model[spec.vessel_name], model["InnerBarrel"], model["Riser"]
    ch = {"vessel_x": v.TimeHistory("X", period), "vessel_y": v.TimeHistory("Y", period),
          "ufj_angle_deg": ib.TimeHistory("Ez angle", period, ofx.oeEndA),
          "lfj_angle_deg": riser.TimeHistory("Ez angle", period, ofx.oeEndB),
          "stroke_m": model["SlipJoint"].TimeHistory("z", period),
          "ring_z_m": model["TensionRing"].TimeHistory("Z", period),
          "riser_top_te_kn": riser.TimeHistory("Effective tension", period, ofx.oeEndA),
          "lfj_te_kn": riser.TimeHistory("Effective tension", period, ofx.oeEndB)}
    stack = model["Stack"]
    ch["datum_m_knm"] = stack.TimeHistory("Bend moment", period, ofx.oeEndA)
    ch["datum_te_kn"] = stack.TimeHistory("Effective tension", period, ofx.oeEndA)
    out = {"dt_s": dt_s, "t": _downsample(t, t, dt_s)}
    for k, s in ch.items():
        out[k] = _downsample(t, [float(x) for x in s], dt_s)
    return out


def recoil_summary(model, spec, ofx) -> dict[str, Any]:
    """Recoil checks after the release at the start of stage 1: LMRP lift off the BOP top (CR-33 against the minimum
    lift), the smallest clearance after the release and whether the LMRP falls back (re-contact)."""
    from .events import LMRP_LINE

    period = _main(model, spec)
    t = [float(x) for x in model.SampleTimes(period)]
    lm = [float(x) for x in model[LMRP_LINE].TimeHistory("Z", period, ofx.oeEndA)]
    bop = [float(x) for x in model["Stack"].TimeHistory("Z", period, ofx.oeEndB)]
    z0 = lm[0]
    lift = [a - z0 for a in lm]
    gap = [a - b for a, b in zip(lm, bop)]
    i_max = max(range(len(lift)), key=lift.__getitem__)
    after = gap[i_max:]
    return {"lmrp_lift_max_m": max(lift), "t_lift_max_s": t[i_max], "clearance_min_after_release_m": min(gap),
            "clearance_min_after_peak_m": min(after), "ring_z_range_m": max(model["TensionRing"].TimeHistory("Z", period))
            - min(model["TensionRing"].TimeHistory("Z", period)), "stages": spec.recoil.stages}


# --------------------------------------------------------------------------- hang-off / running


def _hang_off_points(spec, ofx) -> list[tuple[str, str, Any, bool, Any]]:
    pts = [("ufj", "InnerBarrel", ofx.oeEndA, True, None), ("riser_top", "Riser", ofx.oeEndA, False, None),
           ("riser_bottom", "Riser", ofx.oeEndB, True, None)]
    if spec.hang_off.with_lmrp:
        for n, a in _stack_points(spec):
            if "|" in n:
                pts.append((n, "Stack", ofx.oeArcLength(a - ABOVE_M), False, ofx.oeArcLength(a + ABOVE_M)))
            else:
                pts.append((n, "Stack", ofx.oeEndA if n == "stack:datum" else ofx.oeEndB, False, None))
    return pts


def _top_load_series(model, spec, period, ofx, *, static: bool):
    """Vertical load on the vessel (N-sign: positive down on the vessel = the hung load): the upper flex-joint
    vertical reaction plus, in a soft hang-off, the spring tension; and the spring tension alone."""
    from .build import HANG_OFF_SPRING

    ib = model["InnerBarrel"]
    soft = spec.hang_off.mode == "soft"
    if static:
        ufj = -float(ib.StaticResult("End GZ force", ofx.oeEndA))
        spring = float(model[HANG_OFF_SPRING].StaticResult("Tension")) if soft else 0.0
        return ufj, spring
    ufj = [-float(x) for x in ib.TimeHistory("End GZ force", period, ofx.oeEndA)]
    spring = [float(x) for x in model[HANG_OFF_SPRING].TimeHistory("Tension", period)] if soft else [0.0] * len(ufj)
    return ufj, spring


def extract_hang_off(model, spec, analysis: str, ofx) -> dict[str, Any]:
    """W5 channel set of a hang-off or running case: the upper flex joint, the riser top and bottom (the lower
    flex joint with the LMRP), the hanging stack bodies, range graphs, stroke, ring and the load on the vessel."""
    static = analysis == "statics"
    sp = ofx.Period(ofx.pnStaticState)
    period = sp if static else _main(model, spec)
    ho = spec.hang_off
    doc: dict[str, Any] = {"schema": SCHEMA, "riser_kind": "hang_off", "mode": ho.mode, "with_lmrp": ho.with_lmrp,
                           "running": ho.running, "analysis": analysis, "units": UNITS, "points": {},
                           "range_graphs": {}}
    t = None if static else [float(x) for x in model.SampleTimes(period)]
    if t is not None:
        doc["time"] = {"start_s": t[0], "end_s": t[-1], "samples": len(t), "dt_s": (t[-1] - t[0]) / max(1, len(t) - 1)}
    for name, line_name, extra, angle, above in _hang_off_points(spec, ofx):
        line = model[line_name]
        vals = _line_vars(line, extra, period, angle=angle, static=static, ofx=ofx)
        if above is not None:
            for key, var in (("te_above", "Effective tension"), ("tw_above", "Wall tension")):
                vals[key] = (float(line.StaticResult(var, above)) if static
                             else [float(x) for x in line.TimeHistory(var, period, above)])
        if static:
            doc["points"][name] = {"line": line_name, "static": vals}
            continue
        pt = {"line": line_name, "stats": {k: stats(v) for k, v in vals.items()}, "extremes": extreme_rows(t, vals)}
        if name.startswith("stack:") or name in ("ufj", "riser_bottom"):
            pt["tm_hull"] = tm_hull_rows(t, vals)
        doc["points"][name] = pt
    for ln in ("InnerBarrel", "Riser") + (("Stack",) if ho.with_lmrp else ()):
        doc["range_graphs"][ln] = _range_graph(model[ln], period, sp, static=static, rows=RANGE[ln])
    slip, ring = model["SlipJoint"], model["TensionRing"]
    ufj, spring = _top_load_series(model, spec, period, ofx, static=static)
    if static:
        doc["stroke"] = {"static": float(slip.StaticResult("z"))}
        doc["ring"] = {"static": {k: float(ring.StaticResult(v)) for k, v in
                                  (("x", "X"), ("y", "Y"), ("z", "Z"), ("yaw_deg", "Rotation 3"))}}
        doc["top_load"] = {"static": {"total": ufj + spring, "ufj": ufj, "spring": spring}}
        v = model[spec.vessel_name]
        doc["vessel"] = {"static": {"x": float(v.InitialX), "y": float(v.InitialY)}}
    else:
        doc["stroke"] = {"stats": stats(slip.TimeHistory("z", period))}
        doc["ring"] = {"stats": {k: stats(ring.TimeHistory(v, period)) for k, v in
                                 (("x", "X"), ("y", "Y"), ("z", "Z"), ("yaw_deg", "Rotation 3"))}}
        doc["top_load"] = {"stats": {"total": stats([a + b for a, b in zip(ufj, spring)]), "ufj": stats(ufj),
                                     "spring": stats(spring)}}
        v = model[spec.vessel_name]
        doc["vessel"] = {"stats": {"x": stats(v.TimeHistory("X", period)), "y": stats(v.TimeHistory("Y", period))}}
    doc["contents"] = {"density_kg_m3": spec.contents.density_kg_m3, "pressure_ref_z_m": spec.contents.pressure_ref_z_m}
    return compact(doc)


def hang_off_summary(model, spec, analysis: str, ofx) -> dict[str, float]:
    """Screening responses of a hang-off case (SI: deg, Pa, N, m): upper flex-joint and riser-bottom angles, riser
    von Mises maximum, load on the vessel (max / min) and telescopic-joint stroke."""
    static = analysis == "statics"
    period = ofx.Period(ofx.pnStaticState) if static else _main(model, spec)
    ib, riser = model["InnerBarrel"], model["Riser"]
    ufj, spring = _top_load_series(model, spec, period, ofx, static=static)
    if static:
        vm = max(float(x) for x in riser.RangeGraph("Max von Mises stress", period).Mean)
        return {"ufj_angle_deg": abs(float(ib.StaticResult("Ez angle", ofx.oeEndA))),
                "riser_bottom_angle_deg": abs(float(riser.StaticResult("Ez angle", ofx.oeEndB))),
                "riser_von_mises_max_pa": vm * 1000.0, "top_load_n": (ufj + spring) * 1000.0,
                "stroke_m": float(model["SlipJoint"].StaticResult("z"))}
    a = [abs(float(x)) for x in ib.TimeHistory("Ez angle", period, ofx.oeEndA)]
    b = [abs(float(x)) for x in riser.TimeHistory("Ez angle", period, ofx.oeEndB)]
    vm = max(float(x) for x in riser.RangeGraph("Max von Mises stress", period).Max)
    tot = [x + y for x, y in zip(ufj, spring)]
    z = [float(x) for x in model["SlipJoint"].TimeHistory("z", period)]
    return {"ufj_angle_max_deg": max(a), "riser_bottom_angle_max_deg": max(b), "riser_von_mises_max_pa": vm * 1000.0,
            "top_load_max_n": max(tot) * 1000.0, "top_load_min_n": min(tot) * 1000.0,
            "stroke_max_m": max(z), "stroke_min_m": min(z), "stroke_static_m": float(model["SlipJoint"].StaticResult("z"))}
