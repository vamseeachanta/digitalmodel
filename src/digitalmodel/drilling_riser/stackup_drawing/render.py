"""Riser stack-up SVG renderer (#2152).

``render(spec) -> str`` depends only on :mod:`.schema` and the standard
library. Output is deterministic (no timestamps, no random ids): the same spec
gives a byte-identical, self-contained SVG.

Drawing conventions
-------------------
* True vertical scale inside each zone. Long runs of repeated riser joints are
  drawn in a condensed middle zone between break lines; its own (smaller)
  vertical scale is stated on the drawing.
* Diameters are exaggerated horizontally (``px_per_in``) and the drawing says so.
* Every component is one ``<g data-role="component" data-component-id=...>``.
  Its ``data-part="body"`` elements span exactly top..bottom elevation through
  the elevation transform, which is embedded as JSON in
  ``<g id="elevation-transform" data-zones=...>`` so the reconciler can invert it.
* Every printed number sits in its own ``<tspan>`` carrying ``data-field``
  (and ``data-unit`` / ``data-decimals``). NOT_FOUND renders as grey "n/a".

Primitives: ``marine_ops.artificial_lift.dynacard.visualization.svg_primitives``
was reviewed and not reused - its ``CoordMapper`` is linear only (no broken
axis), and its helpers emit inline presentation attributes with fixed 1-dp
formatting and cannot carry ``class``/``data-*`` attributes. The small helpers
below follow the same naming style so a later merge is mechanical.
"""

from __future__ import annotations

import html
import json
import math
from dataclasses import dataclass
from typing import Any, Iterable, Optional

from digitalmodel.drilling_riser.stackup_drawing.schema import (
    NESTED_TYPES,
    NOT_FOUND,
    StackupComponent,
    StackupDrawingSpec,
)
from digitalmodel.drilling_riser.stackup_drawing.schema import (
    ComponentType as CT,
)

__all__ = ["ElevationTransform", "Layout", "render"]

M2FT = 1.0 / 0.3048
IN_PER_M = 1.0 / 0.0254

RUN_TYPES = {CT.RISER_JOINT_BARE, CT.RISER_JOINT_BUOYANT}
COUNTED_TYPES = RUN_TYPES | {
    CT.PUP_JOINT,
    CT.TERMINATION_JOINT,
    CT.CONDUCTOR,
    CT.CASING,
}
WT_TYPES = {
    CT.RISER_JOINT_BARE,
    CT.PUP_JOINT,
    CT.TERMINATION_JOINT,
    CT.CONDUCTOR,
    CT.CASING,
}
FRAME_TYPES = {CT.LMRP, CT.BOP, CT.TREE, CT.TUBING_HANGER_SPOOL}


# ---------------------------------------------------------------------------
# layout + elevation transform
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class Layout:
    width: float = 860.0
    axis_x: float = 70.0
    center_x: float = 330.0
    label_x: float = 524.0
    top_margin: float = 18.0
    px_per_m: float = 6.5
    px_per_in: float = 0.8
    condensed_height: float = 440.0
    break_gap: float = 30.0
    above_floor_m: float = 10.0
    below_lowest_m: float = 10.0
    run_margin_m: float = 12.0
    label_pitch: float = 37.0
    title_block_height: float = 196.0


class ElevationTransform:
    """Piecewise-linear elevation (m, rel. MSL) -> SVG y, one piece per zone."""

    def __init__(self, zones: list[dict[str, float]]):
        self.zones = zones

    @classmethod
    def build(cls, spec: StackupDrawingSpec, lay: Layout) -> "ElevationTransform":
        d = spec.datums
        stacked = [
            c
            for c in spec.components
            if c.type not in NESTED_TYPES
            and c.known("top_el_m")
            and c.known("bottom_el_m")
        ]
        z_hi = (
            max([float(d.drill_floor_el_m)] + [float(c.top_el_m) for c in stacked])
            + lay.above_floor_m
        )
        low_candidates = [float(c.bottom_el_m) for c in stacked]
        if d.mudline_el_m not in (None, NOT_FOUND):
            low_candidates.append(float(d.mudline_el_m))
        z_lo = min(low_candidates) - lay.below_lowest_m
        runs = [c for c in stacked if c.type in RUN_TYPES and _int(c.count, 0) >= 2]
        zones: list[dict[str, float]] = []
        y = lay.top_margin
        if runs:
            first, last = runs[0], runs[-1]
            b1 = float(first.top_el_m) - min(
                lay.run_margin_m, 0.3 * float(first.length_m)
            )
            b2 = float(last.bottom_el_m) + min(
                lay.run_margin_m, 0.3 * float(last.length_m)
            )
            mid_scale = lay.condensed_height / (b1 - b2)
            spans = [
                (z_hi, b1, lay.px_per_m, "true"),
                (b1, b2, mid_scale, "condensed"),
                (b2, z_lo, lay.px_per_m, "true"),
            ]
        else:
            spans = [(z_hi, z_lo, lay.px_per_m, "true")]
        for i, (hi, lo, s, kind) in enumerate(spans):
            zones.append(
                {
                    "z_hi": round(hi, 6),
                    "z_lo": round(lo, 6),
                    "y_top": round(y, 6),
                    "px_per_m": round(s, 9),
                    "kind": kind,
                }
            )
            y += (hi - lo) * s
            if i < len(spans) - 1:
                y += lay.break_gap
        return cls(zones)

    @property
    def y_bottom(self) -> float:
        z = self.zones[-1]
        return z["y_top"] + (z["z_hi"] - z["z_lo"]) * z["px_per_m"]

    def y(self, z: float, prefer: str = "upper") -> float:
        cands = [zn for zn in self.zones if zn["z_lo"] - 1e-9 <= z <= zn["z_hi"] + 1e-9]
        if cands:
            zn = cands[0] if prefer == "upper" else cands[-1]
        else:
            zn = self.zones[0] if z > self.zones[0]["z_hi"] else self.zones[-1]
        return zn["y_top"] + (zn["z_hi"] - z) * zn["px_per_m"]

    def pieces(self, z_top: float, z_bot: float):
        """Visible pieces of [z_bot, z_top]: (y_top, y_bot, cut_top, cut_bot)."""
        if abs(z_top - z_bot) < 1e-12:
            for zn in self.zones:
                if zn["z_lo"] - 1e-9 <= z_top <= zn["z_hi"] + 1e-9:
                    y = zn["y_top"] + (zn["z_hi"] - z_top) * zn["px_per_m"]
                    return [(y, y, False, False)]
            return []
        out = []
        for zn in self.zones:
            hi, lo = min(z_top, zn["z_hi"]), max(z_bot, zn["z_lo"])
            if hi > lo + 1e-9:
                yt = zn["y_top"] + (zn["z_hi"] - hi) * zn["px_per_m"]
                yb = zn["y_top"] + (zn["z_hi"] - lo) * zn["px_per_m"]
                out.append((yt, yb, hi < z_top - 1e-9, lo > z_bot + 1e-9))
        return out

    def to_json(self) -> str:
        return json.dumps(self.zones, separators=(",", ":"))


# ---------------------------------------------------------------------------
# primitives (class/data-attribute aware, deterministic number formatting)
# ---------------------------------------------------------------------------


def _n(v: float) -> str:
    s = f"{v:.2f}"
    return "0" if s in ("-0.00", "0.00") else s.rstrip("0").rstrip(".")


def _attrs(attrs: Optional[dict[str, Any]]) -> str:
    if not attrs:
        return ""
    return "".join(
        f' {k}="{html.escape(str(v), quote=True)}"'
        for k, v in attrs.items()
        if v is not None
    )


def svg_rect(x, y, w, h, cls=None, **attrs) -> str:
    return (
        f'<rect x="{_n(x)}" y="{_n(y)}" width="{_n(max(w, 0))}" height="{_n(max(h, 0))}"'
        f'{_attrs({"class": cls, **_data(attrs)})}/>'
    )


def svg_line(x1, y1, x2, y2, cls=None, **attrs) -> str:
    return (
        f'<line x1="{_n(x1)}" y1="{_n(y1)}" x2="{_n(x2)}" y2="{_n(y2)}"'
        f'{_attrs({"class": cls, **_data(attrs)})}/>'
    )


def svg_circle(cx, cy, r, cls=None, **attrs) -> str:
    return f'<circle cx="{_n(cx)}" cy="{_n(cy)}" r="{_n(r)}"{_attrs({"class": cls, **_data(attrs)})}/>'


def svg_path(d: str, cls=None, **attrs) -> str:
    return f'<path d="{d}"{_attrs({"class": cls, **_data(attrs)})}/>'


def svg_polyline(points: Iterable[tuple[float, float]], cls=None, **attrs) -> str:
    pts = " ".join(f"{_n(x)},{_n(y)}" for x, y in points)
    return f'<polyline points="{pts}"{_attrs({"class": cls, **_data(attrs)})}/>'


def svg_text(x, y, spans: list[tuple[str, dict]], cls=None, **attrs) -> str:
    inner = "".join(
        f"<tspan{_attrs(a)}>{html.escape(t, quote=False)}</tspan>" for t, a in spans
    )
    return f'<text x="{_n(x)}" y="{_n(y)}"{_attrs({"class": cls, **_data(attrs)})}>{inner}</text>'


def _data(attrs: dict[str, Any]) -> dict[str, Any]:
    """kwargs data_foo_bar -> data-foo-bar; other keys pass through."""
    return {k.replace("_", "-"): v for k, v in attrs.items()}


def fmt_num(v: float, decimals: int, signed: bool = False) -> str:
    s = f"{abs(v):,.{decimals}f}"
    if float(s.replace(",", "")) == 0:
        return s
    if v < 0:
        return "−" + s
    return ("+" + s) if signed else s


def _decimals_for(v: float, max_dec: int = 3) -> int:
    for d in range(max_dec + 1):
        if abs(round(v, d) - v) < 1e-9:
            return d
    return max_dec


def _int(v: Any, default: int) -> int:
    try:
        return int(v)
    except (TypeError, ValueError):
        return default


# ---------------------------------------------------------------------------
# span builders (numbers are always traceable to a spec field)
# ---------------------------------------------------------------------------


def _txt(t: str, cls: Optional[str] = None) -> tuple[str, dict]:
    return (t, {"class": cls} if cls else {})


def _num(
    value: Any,
    field: str,
    unit: str,
    decimals: Optional[int] = None,
    *,
    signed: bool = False,
    dagger: bool = False,
    factor: float = 1.0,
    cls: Optional[str] = None,
) -> list[tuple[str, dict]]:
    if value == NOT_FOUND:
        return [("n/a", {"class": "na", "data-field": field})]
    v = float(value) * factor
    d = _decimals_for(v) if decimals is None else decimals
    spans = [
        (
            fmt_num(v, d, signed),
            {
                "class": cls,
                "data-field": field,
                "data-unit": unit,
                "data-decimals": str(d),
            },
        )
    ]
    if dagger:
        spans.append(("†", {"class": "rpt"}))
    return spans


def _is_report(prov: dict, name: str) -> bool:
    p = prov.get(name)
    return bool(p) and p.basis == "report table"


# ---------------------------------------------------------------------------
# renderer
# ---------------------------------------------------------------------------

STYLE = """
svg{background:#fff}
text{font-family:system-ui,-apple-system,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif;fill:#1d2733;paint-order:stroke;stroke:#fff;stroke-width:3px;stroke-linejoin:round}
.bg{fill:#fff}
.water{fill:#eef5fa}
.wave{fill:none;stroke:#3f7fb0;stroke-width:1.4}
.soil{fill:url(#soil-hatch)}
.soil-edge{fill:none;stroke:#6b5236;stroke-width:2}
.rig{fill:none;stroke:#8a939c;stroke-width:1}
.rig-floor{fill:url(#steel-hatch);stroke:#3b4652;stroke-width:1.2}
.rotary{fill:#cfd6dd;stroke:#3b4652;stroke-width:1}
.datum{stroke:#3b4652;stroke-width:1.6;fill:none}
.datum-msl{stroke:#3f7fb0;stroke-width:0.8;stroke-dasharray:6 3;fill:none}
.datum-ml{stroke:#6b5236;stroke-width:1.8;fill:none}
.axis{stroke:#3b4652;stroke-width:1;fill:none}
.tick{stroke:#3b4652;stroke-width:0.8}
.tick-lbl{font-size:11px;fill:#3b4652}
.axis-hdr{font-size:11px;font-weight:600;fill:#3b4652}
.centre{stroke:#7a8691;stroke-width:0.6;stroke-dasharray:14 3 2 3;fill:none}
.pipe{fill:url(#g-steel);stroke:#27313b;stroke-width:0.9}
.pipe-dark{fill:url(#g-steel-dark);stroke:#27313b;stroke-width:0.9}
.chrome{fill:url(#g-chrome);stroke:#27313b;stroke-width:0.9}
.buoy-a{fill:url(#g-buoy-a);stroke:#5e4a1c;stroke-width:0.9}
.buoy-b{fill:url(#g-buoy-b);stroke:#5e4a1c;stroke-width:0.9}
.buoy-c{fill:url(#g-buoy-c);stroke:#5e4a1c;stroke-width:0.9}
.seam{stroke:#5e4a1c;stroke-width:0.8}
.flange{fill:#58636e;stroke:#27313b;stroke-width:0.6}
.body-thin{fill:#58636e;stroke:none}
.ball{fill:url(#g-ball);stroke:#1c252e;stroke-width:1}
.ring{fill:url(#g-steel-dark);stroke:#1c252e;stroke-width:1}
.wire{stroke:#46505a;stroke-width:0.9;fill:none}
.sheave{fill:#e3e7eb;stroke:#27313b;stroke-width:1}
.cyl{fill:url(#g-steel);stroke:#27313b;stroke-width:0.8}
.frame{fill:#f4f6f8;stroke:#1c252e;stroke-width:1.3}
.post{fill:#9aa4ad;stroke:#27313b;stroke-width:0.6}
.annular{fill:url(#g-red);stroke:#3a1d1d;stroke-width:0.9}
.ram{fill:url(#g-red);stroke:#3a1d1d;stroke-width:0.9}
.bonnet{fill:#d8a3a0;stroke:#3a1d1d;stroke-width:0.8}
.connector{fill:url(#g-steel-dark);stroke:#1c252e;stroke-width:0.8}
.pod{fill:#e8d58f;stroke:#5e4a1c;stroke-width:0.7}
.valve{fill:#fff;stroke:#1c252e;stroke-width:0.8}
.cond{fill:url(#g-cond);stroke:#1c252e;stroke-width:0.9}
.cond-b{fill:url(#g-steel);stroke:#1c252e;stroke-width:0.9}
.hidden{fill:none;stroke:#1c252e;stroke-width:1.2;stroke-dasharray:6 3}
.housing{fill:url(#g-steel);stroke:#1c252e;stroke-width:1}
.break{fill:none;stroke:#1c252e;stroke-width:1}
.cut{fill:none;stroke:#1c252e;stroke-width:1.1}
.leader{fill:none;stroke:#46505a;stroke-width:0.7}
.dot{fill:#46505a}
.co1{font-size:14px;font-weight:600}
.co2{font-size:12.5px;fill:#39444f}
.na{fill:#8a939c;font-style:italic}
.rpt{fill:#8a3b12}
.dlbl{font-size:12px;font-weight:600}
.dlbl2{font-size:11px;fill:#39444f}
.note-l{font-size:11.5px;fill:#39444f;font-style:italic}
.tb{fill:#fff;stroke:#1c252e;stroke-width:1.2}
.tb-line{stroke:#1c252e;stroke-width:0.7}
.tb-title{font-size:16px;font-weight:700}
.tb-sub{font-size:12px;fill:#39444f}
.tb-hdr{font-size:9.5px;font-weight:600;fill:#5b6670;letter-spacing:.04em}
.tb-val{font-size:13px;font-weight:600}
.tb-note{font-size:11.5px;fill:#39444f}
"""


def _grad(gid: str, c_edge: str, c_mid: str) -> str:
    return (
        f'<linearGradient id="{gid}" x1="0" x2="1" y1="0" y2="0">'
        f'<stop offset="0" stop-color="{c_edge}"/><stop offset="0.38" stop-color="{c_mid}"/>'
        f'<stop offset="1" stop-color="{c_edge}"/></linearGradient>'
    )


DEFS = (
    "<defs>"
    + _grad("g-steel", "#8d98a3", "#eef1f4")
    + _grad("g-steel-dark", "#5b6670", "#c5ccd3")
    + _grad("g-chrome", "#a9b3bc", "#ffffff")
    + _grad("g-cond", "#8f9aa4", "#e6eaee")
    + _grad("g-buoy-a", "#d9c07a", "#fbf2d2")
    + _grad("g-buoy-b", "#caa650", "#f4e3ad")
    + _grad("g-buoy-c", "#b98f34", "#ecd28a")
    + _grad("g-red", "#a9605c", "#f0d0cd")
    + '<radialGradient id="g-ball" cx="0.35" cy="0.35" r="0.7"><stop offset="0" stop-color="#ffffff"/>'
    '<stop offset="1" stop-color="#6c7883"/></radialGradient>'
    + '<pattern id="soil-hatch" width="9" height="9" patternUnits="userSpaceOnUse" patternTransform="rotate(45)">'
    '<rect width="9" height="9" fill="#efe6d8"/><line x1="0" y1="0" x2="0" y2="9" stroke="#b69d7a" stroke-width="1"/></pattern>'
    + '<pattern id="steel-hatch" width="6" height="6" patternUnits="userSpaceOnUse" patternTransform="rotate(45)">'
    '<rect width="6" height="6" fill="#dfe4e8"/><line x1="0" y1="0" x2="0" y2="6" stroke="#8a939c" stroke-width="0.8"/></pattern>'
    + "</defs>"
)


class _Renderer:
    def __init__(self, spec: StackupDrawingSpec, lay: Layout):
        problems = spec.validate()
        if problems:
            raise ValueError(f"invalid spec: {problems}")
        self.s = spec
        self.lay = lay
        self.T = ElevationTransform.build(spec, lay)
        self.cx = lay.center_x
        self.out: list[str] = []
        self.callouts: list[dict] = []
        bare = [c for c in spec.components if c.type in RUN_TYPES and c.known("od_in")]
        self.nominal_od = float(bare[0].od_in) if bare else 21.0
        self.buoy_class: dict[str, str] = {}
        for i, c in enumerate(
            [c for c in spec.components if c.type == CT.RISER_JOINT_BUOYANT]
        ):
            self.buoy_class[c.id] = ("buoy-a", "buoy-b", "buoy-c")[i % 3]
        self.y_draw_bottom = self.T.y_bottom
        self._cuts: list[tuple[Optional[float], Optional[float], float]] = []

    # -- helpers ------------------------------------------------------------
    def w(self, inches: float) -> float:
        return inches * self.lay.px_per_in

    def draw_width_in(self, c: StackupComponent) -> float:
        for name in (
            "envelope_width_in",
            "buoyancy_od_in",
            "od_in",
            "drag_diameter_in",
        ):
            if c.known(name):
                if name == "drag_diameter_in" and c.type not in FRAME_TYPES:
                    continue
                return float(getattr(c, name))
        return self.nominal_od

    @property
    def exaggeration(self) -> float:
        return self.lay.px_per_in * IN_PER_M / self.T.zones[0]["px_per_m"]

    @property
    def condensed_ratio(self) -> Optional[float]:
        mids = [z for z in self.T.zones if z["kind"] == "condensed"]
        return self.T.zones[0]["px_per_m"] / mids[0]["px_per_m"] if mids else None

    # -- document -----------------------------------------------------------
    def render(self) -> str:
        lay = self.lay
        H = self.y_draw_bottom + 14 + lay.title_block_height + 12
        self.H = H
        tb = self.s.title_block
        o = self.out
        o.append(
            f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {_n(lay.width)} {_n(H)}" '
            f'width="{_n(lay.width)}" height="{_n(H)}" role="img" '
            f'aria-label="{html.escape(tb.title, quote=True)}">'
        )
        o.append(f"<title>{html.escape(tb.title)}</title>")
        o.append(
            f"<desc>{html.escape(tb.configuration)}. Elevations relative to MSL. "
            f"Diameters not to scale.</desc>"
        )
        o.append(f"<style>{STYLE}</style>")
        o.append(DEFS)
        o.append(f'<g data-role="decor">{svg_rect(0, 0, lay.width, H, "bg")}</g>')
        o.append(
            f'<g id="elevation-transform" data-role="axis" data-zones="{html.escape(self.T.to_json(), quote=True)}" '
            f'data-px-per-in="{lay.px_per_in}" data-center-x="{_n(self.cx)}" '
            f'data-y-bottom="{round(self.y_draw_bottom, 6)}"></g>'
        )
        self.decor()
        self.datums()
        self.axis()
        self.centreline()
        self.tensioners()
        for c in self.s.components:
            self.component(c)
        self.breaks()
        self.place_callouts()
        self.title_block()
        o.append("</svg>")
        return "\n".join(o) + "\n"

    # -- decor ---------------------------------------------------------------
    def decor(self) -> None:
        d, T, lay, cx = self.s.datums, self.T, self.lay, self.cx
        g = ['<g data-role="decor">']
        y0 = T.y(0.0)
        yml = (
            T.y(float(d.mudline_el_m))
            if d.mudline_el_m != NOT_FOUND
            else self.y_draw_bottom
        )
        xr = lay.label_x - 24
        g.append(svg_rect(lay.axis_x + 1, y0, xr - lay.axis_x - 1, yml - y0, "water"))
        g.append(
            svg_rect(
                lay.axis_x + 1,
                yml,
                xr - lay.axis_x - 1,
                self.y_draw_bottom - yml,
                "soil",
            )
        )
        # sea surface waves
        pts, x = [], lay.axis_x + 1
        while x <= xr:
            pts.append((x, y0 - 2.2 * math.sin((x - lay.axis_x) / 7.0)))
            x += 2.5
        g.append(svg_polyline(pts, "wave"))
        # rig: floor beam, rotary table, derrick legs
        yf = T.y(float(d.drill_floor_el_m))
        half = 168.0
        g.append(svg_rect(cx - half, yf, 2 * half, 9, "rig-floor"))
        g.append(svg_rect(cx - 36, yf - 2, 72, 11, "rotary"))
        ytop = lay.top_margin
        for sgn in (-1, 1):
            g.append(svg_line(cx + sgn * 140, yf, cx + sgn * 92, ytop, "rig"))
            g.append(svg_line(cx + sgn * 128, yf, cx + sgn * 84, ytop, "rig"))
        k, yk = 0, yf
        while yk - 14 > ytop:
            t0, t1 = (yf - yk) / (yf - ytop), (yf - (yk - 14)) / (yf - ytop)
            xa, xb = cx - 140 + 48 * t0, cx - 128 + 44 * t1
            g.append(svg_line(xa, yk, xb, yk - 14, "rig"))
            g.append(svg_line(2 * cx - xa, yk, 2 * cx - xb, yk - 14, "rig"))
            yk -= 14
            k += 1
        g.append("</g>")
        self.out.extend(g)

    # -- datums --------------------------------------------------------------
    def datums(self) -> None:
        d, T, lay, cx = self.s.datums, self.T, self.lay, self.cx
        g = []
        if d.drill_floor_el_m != NOT_FOUND:
            yf = T.y(float(d.drill_floor_el_m))
            g.append(
                f'<g data-role="datum" data-datum="drill_floor_el_m" data-el-m="{d.drill_floor_el_m}">'
                + svg_line(cx - 168, yf, cx + 168, yf, "datum", data_part="datum-line")
                + svg_line(
                    lay.axis_x, yf, cx - 168, yf, "datum-msl", data_part="datum-line"
                )
                + "</g>"
            )
            self.callouts.append(
                {
                    "ay": yf,
                    "ax": cx + 168,
                    "role": "datum",
                    "datum": "drill_floor_el_m",
                    "l1": [_txt("Drill floor (RKB)")],
                    "l2": [_txt("EL ")]
                    + _num(
                        d.drill_floor_el_m,
                        "datums.drill_floor_el_m",
                        "m",
                        2,
                        signed=True,
                    )
                    + [_txt(" m (")]
                    + _num(
                        d.drill_floor_el_m,
                        "datums.drill_floor_el_m",
                        "ft",
                        1,
                        signed=True,
                        factor=M2FT,
                    )
                    + [_txt(" ft) · ")]
                    + [_txt(self.s.title_block.vessel_label)],
                }
            )
        y0 = T.y(0.0)
        g.append(
            '<g data-role="datum" data-datum="msl_el_m" data-el-m="0">'
            + svg_line(
                lay.axis_x + 52,
                y0,
                lay.label_x - 24,
                y0,
                "datum-msl",
                data_part="datum-line",
            )
            + svg_path(f"M{_n(lay.axis_x + 64)},{_n(y0 - 13)} l6,9 l6,-9 z", "datum")
            + svg_text(lay.axis_x + 80, y0 - 5, [_txt("MSL")], "dlbl")
            + svg_text(
                lay.axis_x + 112,
                y0 - 5,
                [_txt("EL ")] + _num(0.0, "datums.msl_el_m", "m", 2),
                "dlbl2",
            )
            + "</g>"
        )
        if d.mudline_el_m != NOT_FOUND:
            yml = T.y(float(d.mudline_el_m))
            g.append(
                f'<g data-role="datum" data-datum="mudline_el_m" data-el-m="{d.mudline_el_m}">'
                + svg_line(
                    lay.axis_x + 52,
                    yml,
                    lay.label_x - 24,
                    yml,
                    "datum-ml",
                    data_part="datum-line",
                )
                + svg_text(lay.axis_x + 64, yml + 17, [_txt("Mudline")], "dlbl")
                + svg_text(
                    lay.axis_x + 64,
                    yml + 31,
                    [_txt("EL ")]
                    + _num(d.mudline_el_m, "datums.mudline_el_m", "m", 2, signed=True)
                    + [_txt(" m (")]
                    + _num(
                        d.mudline_el_m,
                        "datums.mudline_el_m",
                        "ft",
                        1,
                        signed=True,
                        factor=M2FT,
                    )
                    + [_txt(" ft)")],
                    "dlbl2",
                )
                + "</g>"
            )
        self.out.extend(g)

    # -- axis ------------------------------------------------------------------
    def axis(self) -> None:
        T, lay = self.T, self.lay
        ax = lay.axis_x
        g = ['<g data-role="axis">']
        g.append(
            svg_text(
                ax - 8,
                lay.top_margin + 6,
                [_txt("EL m")],
                "axis-hdr",
                text_anchor="end",
            )
        )
        g.append(svg_text(ax + 8, lay.top_margin + 6, [_txt("ft")], "axis-hdr"))
        g.append(
            svg_text(
                ax - 8,
                lay.top_margin + 19,
                [_txt("MSL")],
                "tick-lbl",
                text_anchor="end",
            )
        )
        for zi, zn in enumerate(T.zones):
            y1 = zn["y_top"]
            y2 = zn["y_top"] + (zn["z_hi"] - zn["z_lo"]) * zn["px_per_m"]
            g.append(svg_line(ax, y1, ax, y2, "axis"))
            s = zn["px_per_m"]
            step_m = next(
                (
                    v
                    for v in (10.0, 20.0, 50.0, 100.0, 200.0, 500.0, 1000.0)
                    if v * s >= 34
                ),
                1000.0,
            )
            step_ft = next(
                (
                    v
                    for v in (25.0, 50.0, 100.0, 250.0, 500.0, 1000.0, 2500.0)
                    if v * 0.3048 * s >= 34
                ),
                2500.0,
            )
            for unit, step, side in (("m", step_m, -1), ("ft", step_ft, 1)):
                f = 1.0 if unit == "m" else M2FT
                v = math.ceil(zn["z_lo"] * f / step) * step
                while v <= zn["z_hi"] * f + 1e-9:
                    z = v / f
                    y = zn["y_top"] + (zn["z_hi"] - z) * zn["px_per_m"]
                    if y - y1 >= 9 and y2 - y >= 5 and (zi > 0 or y - y1 >= 26):
                        tick = f"{z:.9f}"
                        g.append(
                            svg_line(
                                ax,
                                y,
                                ax + side * 6,
                                y,
                                "tick",
                                data_tick_el_m=tick,
                                data_unit=unit,
                            )
                        )
                        g.append(
                            svg_text(
                                ax + side * 8,
                                y + 3.8,
                                [
                                    (
                                        fmt_num(v, 0),
                                        {
                                            "data-tick-el-m": tick,
                                            "data-unit": unit,
                                            "data-decimals": "0",
                                        },
                                    )
                                ],
                                "tick-lbl",
                                text_anchor="end" if side < 0 else "start",
                            )
                        )
                    v += step
        # axis break marks
        for zi in range(len(T.zones) - 1):
            zn = T.zones[zi]
            yb = zn["y_top"] + (zn["z_hi"] - zn["z_lo"]) * zn["px_per_m"]
            for dy in (6, 13):
                g.append(svg_line(ax - 7, yb + dy + 3, ax + 7, yb + dy - 3, "break"))
        g.append("</g>")
        self.out.extend(g)

    def centreline(self) -> None:
        stacked = [
            c
            for c in self.s.components
            if c.known("top_el_m") and c.known("bottom_el_m")
        ]
        if not stacked:
            return
        top = max(float(c.top_el_m) for c in stacked) + 1.0
        bot = min(float(c.bottom_el_m) for c in stacked)
        g = ['<g data-role="decor">']
        for yt, yb, _, _ in self.T.pieces(top, bot):
            g.append(
                svg_line(self.cx, yt, self.cx, min(yb, self.y_draw_bottom), "centre")
            )
        g.append("</g>")
        self.out.extend(g)

    # -- tensioners --------------------------------------------------------------
    def tensioners(self) -> None:
        ts = self.s.tensioner_system
        ring = next((c for c in self.s.components if c.type == CT.TENSION_RING), None)
        if (
            ts is None
            or ring is None
            or ts.sheave_el_m == NOT_FOUND
            or not ring.known("top_el_m")
        ):
            return
        T, cx = self.T, self.cx
        ys = T.y(float(ts.sheave_el_m))
        yr = T.y(float(ring.top_el_m))
        r_px = (
            self.w(float(ts.sheave_radius_m) * IN_PER_M)
            if ts.sheave_radius_m != NOT_FOUND
            else 110.0
        )
        ring_half = self.w(self.draw_width_in(ring)) / 2
        yf = T.y(float(self.s.datums.drill_floor_el_m))
        g = [
            f'<g data-role="tensioner" data-sheave-el-m="{ts.sheave_el_m}" data-count="{ts.count}">'
        ]
        rs = 7.0
        for sgn in (-1, 1):
            xs = cx + sgn * r_px
            g.append(svg_line(xs, yf + 9, xs, ys, "rig"))
            g.append(
                svg_rect(
                    xs + sgn * 12 - 5,
                    yf + 9,
                    10,
                    max(ys - yf - 4, 12),
                    "cyl",
                    data_part="symbol",
                )
            )
            for off in (-1.1, 1.1):
                g.append(
                    svg_line(
                        cx + sgn * (ring_half - 3) + off,
                        yr,
                        xs - sgn * rs * 0.8 + off,
                        ys + rs * 0.6,
                        "wire",
                    )
                )
            g.append(svg_line(xs, ys - rs, xs + sgn * 12, ys - rs, "wire"))
            g.append(svg_circle(xs, ys, rs, "sheave", data_part="sheave"))
            g.append(svg_circle(xs, ys, 2.0, "dot"))
        g.append("</g>")
        self.out.extend(g)
        self.callouts.append(
            {
                "ay": ys,
                "ax": cx + r_px + rs + 12,
                "role": "tensioner",
                "l1": [_txt("Riser tensioners, ")]
                + _num(ts.count, "tensioner_system.count", "count", 0)
                + [_txt(" lines (two shown)")],
                "l2": [_txt("Sheave EL ")]
                + _num(
                    ts.sheave_el_m, "tensioner_system.sheave_el_m", "m", 2, signed=True
                )
                + [_txt(" m · radius ")]
                + _num(ts.sheave_radius_m, "tensioner_system.sheave_radius_m", "m", 2)
                + [_txt(" m")],
            }
        )

    # -- components ------------------------------------------------------------------
    def component(self, c: StackupComponent) -> None:
        if not (c.known("top_el_m") and c.known("bottom_el_m")):
            self.out.append(
                f'<g data-role="component" data-component-id="{html.escape(c.id)}" data-type="{c.type.value}" '
                f'data-top-el-m="{c.top_el_m}" data-bottom-el-m="{c.bottom_el_m}" data-od-in="{_av(c.od_in)}" '
                f'data-count="{c.count}" data-source="{html.escape(c.source)}" data-not-drawn="elevation NOT_FOUND"></g>'
            )
            return
        top, bot = float(c.top_el_m), float(c.bottom_el_m)
        pieces = self.T.pieces(top, bot)
        clipped = bot < self.T.zones[-1]["z_lo"] - 1e-9
        attrs = (
            f'data-role="component" data-component-id="{html.escape(c.id)}" data-type="{c.type.value}" '
            f'data-top-el-m="{c.top_el_m}" data-bottom-el-m="{c.bottom_el_m}" '
            f'data-od-in="{_av(c.od_in)}" data-count="{c.count}" '
            f'data-source="{html.escape(c.source, quote=True)}"'
        )
        if clipped:
            attrs += ' data-clipped="bottom"'
        g = [f"<g {attrs}>"]
        wpx = self.w(self.draw_width_in(c))
        cx = self.cx
        t = c.type
        n = _int(c.count, 1)
        L = float(c.joint_length_m) if c.known("joint_length_m") else None

        def body(yt, yb, cls, width):
            return svg_rect(cx - width / 2, yt, width, yb - yt, cls, data_part="body")

        def seams():
            if L is None or n < 2:
                return []
            return [top - i * L for i in range(1, n)]

        for yt, yb, cut_top, cut_bot in pieces:
            yb_draw = min(yb, self.y_draw_bottom)
            if t == CT.RISER_JOINT_BUOYANT:
                g.append(body(yt, yb_draw, self.buoy_class.get(c.id, "buoy-a"), wpx))
            elif t in (
                CT.RISER_JOINT_BARE,
                CT.PUP_JOINT,
                CT.TERMINATION_JOINT,
                CT.OTHER,
            ):
                g.append(body(yt, yb_draw, "pipe", wpx))
            elif t == CT.DIVERTER:
                hw = max(wpx * 2.6, 52)
                g.append(
                    svg_rect(
                        cx - hw / 2, yt, hw, yb - yt, "housing", data_part="symbol"
                    )
                )
                g.append(body(yt, yb, "pipe", wpx))
                g.append(
                    svg_rect(
                        cx - hw / 2 - 4, yt, hw + 8, 2.5, "flange", data_part="symbol"
                    )
                )
            elif t in (CT.UPPER_FLEX_JOINT, CT.LOWER_FLEX_JOINT):
                g.append(body(yt, yb, "body-thin", wpx))
            elif t == CT.TELESCOPIC_JOINT_INNER:
                g.append(body(yt, yb, "chrome", wpx))
            elif t == CT.TELESCOPIC_JOINT_OUTER:
                g.append(body(yt, yb, "pipe-dark", wpx))
            elif t == CT.TENSION_RING:
                g.append(
                    svg_line(
                        cx - wpx / 2, yt, cx + wpx / 2, yt, "cut", data_part="body"
                    )
                )
            elif t in FRAME_TYPES:
                g.append(body(yt, yb, "frame", wpx))
            elif t == CT.WELLHEAD:
                g.append(body(yt, yb, "housing", wpx))
            elif t == CT.CONDUCTOR:
                cls = (
                    "cond"
                    if c.known("wall_thickness_in")
                    and float(c.wall_thickness_in) >= 1.25
                    else "cond-b"
                )
                g.append(body(yt, yb_draw, cls, wpx))
            elif t == CT.CASING:
                g.append(body(yt, yb_draw, "hidden", wpx))
            else:
                g.append(body(yt, yb_draw, "pipe", wpx))
            if clipped and yb >= self.y_draw_bottom - 1e-6:
                cut_bot = False
            if cut_bot or cut_top:
                self._cuts.append(
                    (yt if cut_top else None, yb if cut_bot else None, wpx)
                )

        # details --------------------------------------------------------------
        T = self.T
        yT, yB = T.y(top), T.y(bot, prefer="lower")
        if t in (
            CT.RISER_JOINT_BARE,
            CT.RISER_JOINT_BUOYANT,
            CT.PUP_JOINT,
            CT.TERMINATION_JOINT,
        ):
            for zs in seams():
                ys = T.y(zs)
                if not any(p[0] - 0.01 <= ys <= p[1] + 0.01 for p in pieces):
                    continue
                if t == CT.RISER_JOINT_BUOYANT:
                    g.append(
                        svg_line(
                            cx - wpx / 2, ys, cx + wpx / 2, ys, "seam", data_part="seam"
                        )
                    )
                    rw = (
                        self.w(float(c.od_in))
                        if c.known("od_in")
                        else self.w(self.nominal_od)
                    )
                    g.append(
                        svg_rect(
                            cx - rw / 2 - 2,
                            ys - 1,
                            rw + 4,
                            2,
                            "flange",
                            data_part="seam",
                        )
                    )
                else:
                    g.append(
                        svg_rect(
                            cx - wpx / 2 - 3,
                            ys - 1,
                            wpx + 6,
                            2,
                            "flange",
                            data_part="seam",
                        )
                    )
            if t in (CT.PUP_JOINT, CT.TERMINATION_JOINT):
                g.append(
                    svg_rect(
                        cx - wpx / 2 - 3,
                        yT - 1,
                        wpx + 6,
                        2,
                        "flange",
                        data_part="symbol",
                    )
                )
        elif t in (CT.UPPER_FLEX_JOINT, CT.LOWER_FLEX_JOINT):
            ym = (yT + yB) / 2
            g.append(svg_rect(cx - 14, ym - 12, 28, 3, "flange", data_part="symbol"))
            g.append(svg_rect(cx - 14, ym + 9, 28, 3, "flange", data_part="symbol"))
            g.append(svg_circle(cx, ym, 9, "ball", data_part="symbol"))
        elif t == CT.TENSION_RING:
            g.append(
                svg_rect(cx - wpx / 2, yT - 3.5, wpx, 7, "ring", data_part="symbol")
            )
            for sgn in (-1, 1):
                g.append(
                    svg_circle(
                        cx + sgn * (wpx / 2 - 3), yT, 2.2, "dot", data_part="symbol"
                    )
                )
        elif t == CT.TELESCOPIC_JOINT_OUTER:
            g.append(
                svg_rect(
                    cx - wpx / 2 - 4, yB - 3, wpx + 8, 3, "flange", data_part="symbol"
                )
            )
        elif t == CT.OTHER:
            g.append(
                svg_rect(
                    cx - wpx / 2 - 5, yB - 3, wpx + 10, 3, "flange", data_part="symbol"
                )
            )
        elif t in FRAME_TYPES:
            g.extend(self._frame_detail(c, yT, yB, wpx))
        elif t == CT.WELLHEAD:
            g.append(
                svg_rect(
                    cx - wpx / 2 - 5,
                    yT,
                    wpx + 10,
                    min(5, yB - yT),
                    "connector",
                    data_part="symbol",
                )
            )
            g.append(
                svg_rect(
                    cx - wpx / 2 - 9, yB - 6, wpx + 18, 6, "housing", data_part="symbol"
                )
            )
        elif t == CT.CASING and clipped:
            yb = self.y_draw_bottom
            g.append(
                svg_path(
                    f"M{_n(cx - wpx / 2)},{_n(yb - 10)} l{_n(wpx / 2)},8 l{_n(wpx / 2)},-8",
                    "hidden",
                    data_part="symbol",
                )
            )
        g.append("</g>")
        self.out.extend(g)
        self._queue_callout(c, pieces, wpx, clipped)

    def _frame_detail(self, c, yT, yB, wpx) -> list[str]:
        cx, h = self.cx, yB - yT
        x0 = cx - wpx / 2
        g = []
        if c.type in (CT.LMRP, CT.BOP):
            for xp in (x0 + 2, x0 + wpx - 7):
                g.append(svg_rect(xp, yT + 1, 5, h - 2, "post", data_part="symbol"))
        if c.type == CT.LMRP:
            bw = wpx * 0.40
            conn_h = h * 0.2
            an_h = (h - conn_h - 6) / 2
            for k in range(2):
                y = yT + 3 + k * an_h
                g.append(
                    svg_path(
                        f"M{_n(cx - bw / 2)},{_n(y + an_h - 1)} v{_n(-(an_h - 7))} "
                        f"q0,-6 {_n(bw * 0.18)},-6 h{_n(bw * 0.64)} q{_n(bw * 0.18)},0 {_n(bw * 0.18)},6 "
                        f"v{_n(an_h - 7)} z",
                        "annular",
                        data_part="symbol",
                    )
                )
            g.append(
                svg_rect(
                    cx - wpx * 0.19,
                    yB - conn_h - 1,
                    wpx * 0.38,
                    conn_h,
                    "connector",
                    data_part="symbol",
                )
            )
            for sgn in (-1, 1):
                g.append(
                    svg_rect(
                        cx + sgn * wpx * 0.33 - 9,
                        yT + h * 0.18,
                        18,
                        h * 0.5,
                        "pod",
                        data_part="symbol",
                    )
                )
        elif c.type == CT.BOP:
            conn_h = h * 0.16
            n_ram = 4
            rh = (h - conn_h - 4) / n_ram
            for k in range(n_ram):
                y = yT + 2 + k * rh
                g.append(
                    svg_rect(
                        cx - wpx * 0.15,
                        y + 1,
                        wpx * 0.30,
                        rh - 2,
                        "ram",
                        data_part="symbol",
                    )
                )
                for sgn in (-1, 1):
                    xb = cx + sgn * wpx * 0.15 + (0 if sgn > 0 else -wpx * 0.2)
                    g.append(
                        svg_rect(
                            xb,
                            y + rh * 0.22,
                            wpx * 0.2,
                            rh * 0.56,
                            "bonnet",
                            data_part="symbol",
                            rx="2",
                        )
                    )
            g.append(
                svg_rect(
                    cx - wpx * 0.17,
                    yB - conn_h - 1,
                    wpx * 0.34,
                    conn_h,
                    "connector",
                    data_part="symbol",
                )
            )
        elif c.type == CT.TREE:
            g.append(
                svg_rect(
                    cx - wpx * 0.2,
                    yT + 1,
                    wpx * 0.4,
                    h - 2,
                    "connector",
                    data_part="symbol",
                )
            )
            ym = (yT + yB) / 2
            for sgn in (-1, 1):
                xv = cx + sgn * wpx * 0.32
                g.append(
                    svg_path(
                        f"M{_n(xv - 6)},{_n(ym - 4)} L{_n(xv + 6)},{_n(ym + 4)} L{_n(xv + 6)},{_n(ym - 4)} "
                        f"L{_n(xv - 6)},{_n(ym + 4)} z",
                        "valve",
                        data_part="symbol",
                    )
                )
        elif c.type == CT.TUBING_HANGER_SPOOL:
            g.append(
                svg_rect(
                    cx - wpx * 0.28,
                    yT + 2,
                    wpx * 0.56,
                    h - 4,
                    "connector",
                    data_part="symbol",
                )
            )
        return g

    # -- breaks ---------------------------------------------------------------------------
    def breaks(self) -> None:
        g = ['<g data-role="break">']
        for y_top_cut, y_bot_cut, wpx in self._cuts:
            for y, sgn in ((y_top_cut, 1), (y_bot_cut, -1)):
                if y is None:
                    continue
                x0, x1 = self.cx - wpx / 2 - 3, self.cx + wpx / 2 + 3
                steps = 8
                pts = [
                    (
                        x0 + (x1 - x0) * i / steps,
                        y - sgn * (2.5 * math.sin(i * math.pi / 2)),
                    )
                    for i in range(steps + 1)
                ]
                g.append(svg_polyline(pts, "cut"))
        lay = self.lay
        for zi in range(len(self.T.zones) - 1):
            zn = self.T.zones[zi]
            yb = zn["y_top"] + (zn["z_hi"] - zn["z_lo"]) * zn["px_per_m"]
            ym = yb + lay.break_gap / 2
            x0, x1 = lay.axis_x + 14, self.cx - 60
            pts = [
                (x0, ym),
                (x1 - 16, ym),
                (x1 - 11, ym - 6),
                (x1 - 5, ym + 6),
                (x1, ym),
                (x1 + 6, ym),
            ]
            g.append(svg_polyline(pts, "break"))
        cond = [z for z in self.T.zones if z["kind"] == "condensed"]
        if cond:
            zn = cond[0]
            ym = zn["y_top"] + (zn["z_hi"] - zn["z_lo"]) * zn["px_per_m"] / 2
            g.append(
                svg_text(
                    lay.axis_x + 64, ym - 8, [_txt("Condensed section:")], "note-l"
                )
            )
            g.append(
                svg_text(
                    lay.axis_x + 64,
                    ym + 7,
                    [_txt("vertical scale reduced ÷")]
                    + _num(self.condensed_ratio, "derived.condensed_ratio", "ratio", 1),
                    "note-l",
                )
            )
        g.append("</g>")
        self.out.extend(g)

    # -- callouts -----------------------------------------------------------------------------
    def _queue_callout(self, c: StackupComponent, pieces, wpx, clipped) -> None:
        if not pieces:
            return
        big = max(pieces, key=lambda p: p[1] - p[0])
        ay = (big[0] + min(big[1], self.y_draw_bottom)) / 2
        if c.type in (CT.UPPER_FLEX_JOINT, CT.LOWER_FLEX_JOINT):
            ax = self.cx + 10
        elif c.type == CT.TENSION_RING:
            ax = self.cx + wpx / 2 + 1
        else:
            ax = self.cx + wpx / 2 + 1
        P = c.provenance
        l1: list = []
        if c.type in COUNTED_TYPES:
            l1 += _num(c.count, "count", "count", 0) + [_txt(" × ")]
            if c.known("joint_length_m"):
                lft = float(c.joint_length_m) * M2FT
                l1 += _num(
                    c.joint_length_m,
                    "joint_length_m",
                    "ft",
                    0 if abs(lft - round(lft)) < 0.05 else 1,
                    factor=M2FT,
                    dagger=_is_report(P, "joint_length_m"),
                ) + [_txt(" ft ")]
            else:
                l1 += [
                    ("n/a", {"class": "na", "data-field": "joint_length_m"}),
                    _txt(" "),
                ]
            l1 += [_txt(c.label)]
            if c.type == CT.RISER_JOINT_BUOYANT and c.buoyancy_od_in is not None:
                l1 += (
                    [_txt(" (Ø")]
                    + _num(
                        c.buoyancy_od_in,
                        "buoyancy_od_in",
                        "in",
                        dagger=_is_report(P, "buoyancy_od_in"),
                    )
                    + [_txt(" in)")]
                )
        else:
            l1 = [_txt(c.label)]
        long_form = c.type in RUN_TYPES or clipped or c.type == CT.CASING
        dg = lambda f: _is_report(P, f)  # noqa: E731
        if long_form:
            l2 = (
                [_txt("EL ")]
                + _num(
                    c.top_el_m, "top_el_m", "m", 2, signed=True, dagger=dg("top_el_m")
                )
                + [_txt(" to ")]
                + _num(
                    c.bottom_el_m,
                    "bottom_el_m",
                    "m",
                    2,
                    signed=True,
                    dagger=dg("bottom_el_m"),
                )
                + [_txt(" m")]
            )
        else:
            l2 = (
                [_txt("EL ")]
                + _num(
                    c.top_el_m, "top_el_m", "m", 2, signed=True, dagger=dg("top_el_m")
                )
                + [_txt(" m (")]
                + _num(c.top_el_m, "top_el_m", "ft", 1, signed=True, factor=M2FT)
                + [_txt(" ft)")]
            )
        if c.od_in is not None:
            if c.od_in == NOT_FOUND:
                l2 += [_txt(" · OD "), ("n/a", {"class": "na", "data-field": "od_in"})]
            else:
                l2 += [_txt(" · Ø")] + _num(c.od_in, "od_in", "in", dagger=dg("od_in"))
                if c.type in WT_TYPES and c.wall_thickness_in is not None:
                    l2 += [_txt(" × ")] + _num(
                        c.wall_thickness_in,
                        "wall_thickness_in",
                        "in",
                        3,
                        dagger=dg("wall_thickness_in"),
                    )
                l2 += [_txt(" in")]
        elif c.envelope_width_in is not None:
            l2 += [_txt(" · W ")] + _num(
                c.envelope_width_in,
                "envelope_width_in",
                "in",
                dagger=dg("envelope_width_in"),
            )
            if c.envelope_width_in != NOT_FOUND:
                l2 += [_txt(" in")]
        if c.type == CT.RISER_JOINT_BUOYANT and c.buoyancy_depth_rating_ft is not None:
            l2 += (
                [_txt(" · ")]
                + _num(c.buoyancy_depth_rating_ft, "buoyancy_depth_rating_ft", "ft", 0)
                + [_txt(" ft rating")]
            )
        self.callouts.append(
            {"ay": ay, "ax": ax, "role": "callout", "cid": c.id, "l1": l1, "l2": l2}
        )

    def place_callouts(self) -> None:
        lay = self.lay
        items = sorted(self.callouts, key=lambda k: k["ay"])
        pitch = lay.label_pitch
        lo, hi = lay.top_margin + 4, self.y_draw_bottom - pitch + 10
        # cluster relaxation: labels centred on their anchors, min spacing = pitch
        clusters = [[i] for i in range(len(items))]
        pos = [it["ay"] - 6 for it in items]

        def place(cl):
            mean = sum(items[i]["ay"] - 6 for i in cl) / len(cl)
            start = mean - (len(cl) - 1) * pitch / 2
            start = max(lo, min(start, hi - (len(cl) - 1) * pitch))
            for k, i in enumerate(cl):
                pos[i] = start + k * pitch

        for cl in clusters:
            place(cl)
        changed = True
        while changed:
            changed = False
            for k in range(len(clusters) - 1):
                a, b = clusters[k], clusters[k + 1]
                if pos[b[0]] - pos[a[-1]] < pitch - 1e-6:
                    clusters[k] = a + b
                    del clusters[k + 1]
                    place(clusters[k])
                    changed = True
                    break
        g = []
        lx = lay.label_x
        for i, it in enumerate(items):
            y = pos[i]
            role = it["role"]
            head = f'<g data-role="{"datum-label" if role == "datum" else role}"'
            if it.get("cid"):
                head += f' data-component-id="{html.escape(it["cid"])}"'
            if it.get("datum"):
                head += f' data-datum="{it["datum"]}"'
            g.append(head + ">")
            ax, ay = it["ax"], it["ay"]
            g.append(
                svg_polyline(
                    [(ax + 2, ay), (lx - 34, ay), (lx - 8, y - 4), (lx - 3, y - 4)],
                    "leader",
                )
            )
            g.append(svg_circle(ax + 2, ay, 1.8, "dot"))
            extra = {"data_component_id": it["cid"]} if it.get("cid") else {}
            g.append(svg_text(lx, y, it["l1"], "co1", **extra))
            g.append(svg_text(lx, y + 15, it["l2"], "co2", **extra))
            g.append("</g>")
        self.out.extend(g)

    # -- title block ---------------------------------------------------------------------------
    def title_block(self) -> None:
        lay, s = self.lay, self.s
        d, tb, rt = s.datums, s.title_block, s.reference_totals
        x0, y0 = 12.0, self.y_draw_bottom + 14
        w, h = lay.width - 24, lay.title_block_height
        g = ['<g data-role="titleblock">']
        g.append(svg_rect(x0, y0, w, h, "tb"))
        r1 = 46.0
        r2 = 50.0
        g.append(svg_line(x0, y0 + r1, x0 + w, y0 + r1, "tb-line"))
        g.append(svg_line(x0, y0 + r1 + r2, x0 + w, y0 + r1 + r2, "tb-line"))
        g.append(svg_text(x0 + 10, y0 + 20, [_txt(tb.title)], "tb-title"))
        g.append(
            svg_text(
                x0 + 10,
                y0 + 37,
                [_txt(tb.vessel_label + " · " + tb.configuration)],
                "tb-sub",
            )
        )
        xs = x0 + w - 190
        g.append(svg_line(xs, y0, xs, y0 + r1, "tb-line"))
        g.append(svg_text(xs + 10, y0 + 15, [_txt("SOURCE")], "tb-hdr"))
        g.append(
            svg_text(
                xs + 10,
                y0 + 35,
                [
                    (
                        tb.document_ref,
                        {"data-field": "title_block.document_ref", "data-kind": "text"},
                    )
                ],
                "tb-val",
            )
        )
        cells = [
            (
                "WATER DEPTH",
                _num(d.water_depth_m, "datums.water_depth_m", "m", 2)
                + [_txt(" m (")]
                + _num(d.water_depth_m, "datums.water_depth_m", "ft", 1, factor=M2FT)
                + [_txt(" ft)")],
            ),
            (
                "DRILL FLOOR (RKB) ABOVE MSL",
                _num(d.air_gap_m, "datums.air_gap_m", "m", 2)
                + [_txt(" m (")]
                + _num(d.air_gap_m, "datums.air_gap_m", "ft", 1, factor=M2FT)
                + [_txt(" ft)")],
            ),
            ("STACK-UP LENGTH, TOP TO MUDLINE", self._ref("stackup_length_m", 2, " m")),
            ("RISER LENGTH, UFJ TO LFJ", self._ref("riser_length_ufj_lfj_m", 2, " m")),
        ]
        cw = w / 4
        for i, (hdr, spans) in enumerate(cells):
            cx0 = x0 + i * cw
            if i:
                g.append(svg_line(cx0, y0 + r1, cx0, y0 + r1 + r2, "tb-line"))
            g.append(svg_text(cx0 + 10, y0 + r1 + 16, [_txt(hdr)], "tb-hdr"))
            g.append(svg_text(cx0 + 10, y0 + r1 + 36, spans, "tb-val"))
        yn = y0 + r1 + r2 + 18
        notes = [
            [
                _txt(
                    "Elevations in m (ft) relative to MSL, positive up. Diameters not to scale: horizontal exaggeration ×"
                )
            ]
            + _num(self.exaggeration, "derived.diameter_exaggeration", "ratio", 1)
            + [_txt(".")],
        ]
        if self.condensed_ratio is not None:
            notes.append(
                [
                    _txt(
                        "Vertical scale true within each zone; repeated joints condensed ÷"
                    )
                ]
                + _num(self.condensed_ratio, "derived.condensed_ratio", "ratio", 1)
                + [_txt(" between break lines.")]
            )
        closure = (
            [_txt("Closure: water depth + air gap ")]
            + _num(
                (
                    float(d.water_depth_m) + float(d.air_gap_m)
                    if NOT_FOUND not in (d.water_depth_m, d.air_gap_m)
                    else NOT_FOUND
                ),
                "derived.wd_plus_air_gap_m",
                "m",
                2,
            )
            + [_txt(" m vs stack-up ")]
            + self._ref("stackup_length_m", 2, " m")
        )
        if "closure_residual_m" in rt:
            closure += (
                [_txt("; residual ")]
                + self._ref("closure_residual_m", 2, " m", signed=True)
                + [_txt(" (source-flagged).")]
            )
        notes.append(closure)
        notes.append(
            [
                _txt("† value from design-basis report table; "),
                ("n/a", {"class": "na"}),
                _txt(" = not available in the sources."),
            ]
        )
        for k, line in enumerate(notes):
            g.append(svg_text(x0 + 10, yn + k * 17, line, "tb-note"))
        g.append("</g>")
        self.out.extend(g)

    def _ref(self, key: str, dec: int, suffix: str, signed: bool = False):
        rv = self.s.reference_totals.get(key)
        if rv is None:
            return [
                ("n/a", {"class": "na", "data-field": f"reference_totals.{key}.value"})
            ]
        return _num(
            rv.value, f"reference_totals.{key}.value", "m", dec, signed=signed
        ) + [_txt(suffix)]


def _av(v: Any) -> str:
    return "" if v is None else str(v)


def render(spec: StackupDrawingSpec, layout: Optional[Layout] = None) -> str:
    """Render ``spec`` to a self-contained SVG string (deterministic)."""
    return _Renderer(spec, layout or Layout()).render()
