"""Riser stack-up SVG renderer (#2152, #2158).

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
  (and ``data-unit`` / ``data-decimals``). NOT_FOUND renders as grey "n/a";
  a field that does not apply renders as "–".
* Callouts are table rows (design A, owner decision L11): one row per
  component plus the drill floor and the tensioners, fixed columns under one
  header (units in the header), each row placed at the nearest free position
  to its component's elevation with an angled leader when rows cluster. The
  last column lists the report design-data items (``D-nn``) behind the row's
  values with a class flag: P public, D owner decision, A assumed. The sheet
  width follows the table columns.
* A review list above the title block lists every open data conflict and gap
  keyed by ``component_id``; the rows named there carry a warning mark.
* A component with ``nested_in`` is drawn after its host.

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
    SOURCE_CLASSES,
    StackupComponent,
    StackupDrawingSpec,
    open_review_entries,
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

# -- table (design A) and review list; the reconciler freezes the same numbers ----------
TABLE_X0 = 516.0
TABLE_RIGHT_MARGIN = 14.0
ROW_PITCH = 20.0
HEADER_HEIGHT = 32.0
CELL_BASELINE = 4.4
LEGEND_HEIGHT = 56.0
WARN_INSET = 16.0
#: (key, header, header units, width px); units live in the header only.
COLUMNS = (
    ("component", "Component", "", 206.0),
    ("qty", "Qty × length", "no. × ft", 84.0),
    ("top", "Top EL", "m (ft)", 144.0),
    ("bot", "Bottom EL", "m", 80.0),
    ("od", "OD / width", "in", 76.0),
    ("wall", "Wall", "in", 60.0),
    ("buoy", "Buoyancy", "OD in / rating ft", 104.0),
    ("dd", "Design data", "D-ID and class", 240.0),
)
#: data-col -> (column, x offset from the column's left edge, text-anchor).
CELL_POS = {
    "component": ("component", 6.0, "start"),
    "qty": ("qty", 18.0, "end"),
    "qty_x": ("qty", 33.0, "middle"),
    "qty_len": ("qty", 73.0, "end"),
    "top": ("top", 64.0, "end"),
    "top_ft": ("top", 138.0, "end"),
    "bot": ("bot", 66.0, "end"),
    "od_w": ("od", 5.0, "start"),
    "od": ("od", 62.0, "end"),
    "wall": ("wall", 46.0, "end"),
    "buoy": ("buoy", 28.0, "end"),
    "buoy_sep": ("buoy", 44.0, "middle"),
    "buoy_rating": ("buoy", 90.0, "end"),
    "dd": ("dd", 6.0, "start"),
}
LITERAL_COLS = {"qty_x", "od_w", "buoy_sep"}
#: Design data column (owner decision G08): sized from its widest cell, never
#: narrower than the ``COLUMNS`` width, in whole steps; the sheet follows.
#: The right pad keeps the text clear of a row's warning mark.
DD_RIGHT_PAD = 20.0
DD_WIDTH_STEP = 10.0
#: Upper-bound advance widths (em) of the design-data text: ``.tdd`` 11 px,
#: class flags 8.5 px bold. Deliberately wider than common UI fonts.
DD_FONT_PX = 11.0
DD_FLAG_PX = 8.5
DD_EM = {"-": 0.42, "/": 0.42, ",": 0.26, " ": 0.28}
DD_EM_DIGIT = 0.56
DD_EM_LOWER = 0.6
DD_EM_OTHER = 0.78
#: "not applicable" cell content.
DASH = (("–", {"class": "nap"}),)
REVIEW_GAP = 14.0
REVIEW_HEADER_BASELINE = 16.0
REVIEW_PITCH = 15.0
REVIEW_BOTTOM_PAD = 9.0
REVIEW_TEXT_X = 26.0
REVIEW_CHAR_PX = 6.2
TITLE_GAP = 10.0
TITLE_NOTE_LINES = 5


# ---------------------------------------------------------------------------
# layout + elevation transform
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class Layout:
    """Drawing-band layout. The sheet width follows the table (``COLUMNS``)."""

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


def dd_text_width(spans: list[tuple[str, dict]]) -> float:
    """Upper-bound width (px) of design-data text spans under ``DD_EM``."""
    total = 0.0
    for text, attrs in spans:
        flag = str(attrs.get("class") or "").startswith("ddc-")
        px = DD_FLAG_PX if flag else DD_FONT_PX
        for ch in text:
            if ch in DD_EM:
                em = DD_EM[ch]
            elif ch.isdigit():
                em = DD_EM_DIGIT
            elif ch.islower():
                em = DD_EM_LOWER
            else:
                em = DD_EM_OTHER
            total += em * px
    return total


def _num(
    value: Any,
    field: str,
    unit: str,
    decimals: Optional[int] = None,
    *,
    signed: bool = False,
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
    return spans


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
.na{fill:#8a939c;font-style:italic}
.nap{fill:#9aa3ab}
.tc{font-size:13px;stroke:none;font-variant-numeric:tabular-nums lining-nums;font-feature-settings:"tnum" 1,"lnum" 1}
.tl{font-size:11.5px;fill:#39444f;stroke:none}
.tdd{font-size:11px;fill:#2b3642;stroke:none;font-variant-numeric:tabular-nums;font-feature-settings:"tnum" 1}
.th{font-size:11px;font-weight:700;fill:#2b3642;stroke:none;letter-spacing:.02em}
.thu{font-size:10.5px;fill:#5b6670;stroke:none}
.ddc-p,.ddc-d,.ddc-a{font-size:8.5px;font-weight:700;baseline-shift:super}
.ddc-p{fill:#1f6f43}
.ddc-d{fill:#6b2fa3}
.ddc-a{fill:#9a6a00}
.row-a{fill:#f3f6f9}
.row-b{fill:#ffffff}
.row-rule{stroke:#c9d1d8;stroke-width:0.6}
.hdr-bg{fill:#e6ebf0;stroke:#9aa4ad;stroke-width:0.8}
.grid{stroke:#b3bcc4;stroke-width:0.6;fill:none}
.warn{fill:#f2a60c;stroke:#8a5a00;stroke-width:0.7}
.rl-bg{fill:#fffaf0;stroke:#1c252e;stroke-width:1}
.rl-hdr{font-size:9.5px;font-weight:600;fill:#5b6670;letter-spacing:.04em}
.rl,.rl-none{font-size:11.5px;fill:#39444f}
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
        #: (row key attribute, value) -> leader anchor (x, y) of drawn items.
        self.anchors: dict[tuple[str, str], tuple[float, float]] = {}
        bare = [c for c in spec.components if c.type in RUN_TYPES and c.known("od_in")]
        self.nominal_od = float(bare[0].od_in) if bare else 21.0
        self.buoy_class: dict[str, str] = {}
        for i, c in enumerate(
            [c for c in spec.components if c.type == CT.RISER_JOINT_BUOYANT]
        ):
            self.buoy_class[c.id] = ("buoy-a", "buoy-b", "buoy-c")[i % 3]
        self.y_draw_bottom = self.T.y_bottom
        self._cuts: list[tuple[Optional[float], Optional[float], float]] = []
        self.col_w = {key: w for key, _, _, w in COLUMNS}
        self.col_w["dd"] = self._dd_column_width()
        self.col_x: dict[str, float] = {}
        x = TABLE_X0
        for key, _, _, _ in COLUMNS:
            self.col_x[key] = x
            x += self.col_w[key]
        self.table_x1 = x
        self.width = x + TABLE_RIGHT_MARGIN

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
        self.review_lines = self._review_lines()
        self.review_height = (
            REVIEW_HEADER_BASELINE
            + len(self.review_lines) * REVIEW_PITCH
            + REVIEW_BOTTOM_PAD
        )
        self.y_review = self.y_draw_bottom + REVIEW_GAP
        self.y_title = self.y_review + self.review_height + TITLE_GAP
        H = self.y_title + lay.title_block_height + 12
        self.H = H
        W = self.width
        tb = self.s.title_block
        o = self.out
        o.append(
            f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {_n(W)} {_n(H)}" '
            f'width="{_n(W)}" height="{_n(H)}" role="img" '
            f'aria-label="{html.escape(tb.title, quote=True)}">'
        )
        o.append(f"<title>{html.escape(tb.title)}</title>")
        o.append(
            f"<desc>{html.escape(tb.configuration)}. Elevations relative to MSL. "
            f"Diameters not to scale.</desc>"
        )
        o.append(f"<style>{STYLE}</style>")
        o.append(DEFS)
        o.append(f'<g data-role="decor">{svg_rect(0, 0, W, H, "bg")}</g>')
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
        self.components()
        self.breaks()
        self.place_table()
        self.review_list()
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
            # leader anchor: the right end of the drill-floor line
            self.anchors[("data-datum", "drill_floor_el_m")] = (cx + 166, yf)
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
                # every printed number carries its unit (#2152 r2)
                [_txt("EL ")] + _num(0.0, "datums.msl_el_m", "m", 2) + [_txt(" m")],
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
                    self._inline_dd(
                        [_txt("EL ")]
                        + _num(
                            d.mudline_el_m, "datums.mudline_el_m", "m", 2, signed=True
                        )
                        + [_txt(" m (")]
                        + _num(
                            d.mudline_el_m,
                            "datums.mudline_el_m",
                            "ft",
                            1,
                            signed=True,
                            factor=M2FT,
                        )
                        + [_txt(" ft)")]
                    ),
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
        # leader anchor: the right sheave's rim
        self.anchors[("data-row", "tensioner_system")] = (cx + r_px + rs - 2, ys)

    # -- components ------------------------------------------------------------------
    def components(self) -> None:
        """Draw in spec order, each ``nested_in`` component right after its host."""
        done: set[str] = set()
        waiting: list[StackupComponent] = []

        def draw(c: StackupComponent) -> None:
            self.component(c)
            done.add(c.id)
            for w in [w for w in waiting if w.nested_in == c.id]:
                waiting.remove(w)
                draw(w)

        for c in self.s.components:
            if c.nested_in and c.nested_in not in done:
                waiting.append(c)
            else:
                draw(c)
        for c in waiting:  # host not drawable: keep spec order
            self.component(c)

    def _group_attrs(self, c: StackupComponent) -> str:
        attrs = (
            f'data-role="component" data-component-id="{html.escape(c.id)}" data-type="{c.type.value}" '
            f'data-top-el-m="{c.top_el_m}" data-bottom-el-m="{c.bottom_el_m}" '
            f'data-od-in="{_av(c.od_in)}" data-count="{c.count}"'
        )
        if c.nested_in:
            attrs += f' data-nested-in="{html.escape(c.nested_in, quote=True)}"'
        return attrs

    def _pivot(self, c: StackupComponent) -> Optional[float]:
        """Pivot elevation for the first flex joint of each kind, if stated."""
        key = {
            CT.UPPER_FLEX_JOINT: "ufj_pivot_el_m",
            CT.LOWER_FLEX_JOINT: "lfj_pivot_el_m",
        }.get(c.type)
        if key is None:
            return None
        first = next(x for x in self.s.components if x.type == c.type)
        rv = self.s.reference_totals.get(key)
        if first is not c or rv is None or rv.value in (None, NOT_FOUND):
            return None
        return float(rv.value)

    def component(self, c: StackupComponent) -> None:
        if not (c.known("top_el_m") and c.known("bottom_el_m")):
            self.out.append(
                f'<g {self._group_attrs(c)} data-not-drawn="elevation NOT_FOUND"></g>'
            )
            return
        top, bot = float(c.top_el_m), float(c.bottom_el_m)
        pieces = self.T.pieces(top, bot)
        clipped = bot < self.T.zones[-1]["z_lo"] - 1e-9
        attrs = self._group_attrs(c)
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
            pivot = self._pivot(c)
            ym = (yT + yB) / 2 if pivot is None else T.y(pivot)
            g.append(svg_rect(cx - 14, ym - 12, 28, 3, "flange", data_part="symbol"))
            g.append(svg_rect(cx - 14, ym + 9, 28, 3, "flange", data_part="symbol"))
            if pivot is None:
                g.append(svg_circle(cx, ym, 9, "ball", data_part="symbol"))
            else:
                # the pivot carries its elevation and is position-checked
                g.append(
                    svg_circle(
                        cx,
                        ym,
                        9,
                        "ball",
                        data_part="pivot",
                        data_pivot_el_m=repr(pivot),
                    )
                )
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
        self._anchor(c, pieces, wpx)

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

    # -- table rows (design A, #2158) --------------------------------------------------------
    def _anchor(self, c: StackupComponent, pieces, wpx) -> None:
        """Leader anchor of a drawn component: body edge, middle of its largest piece."""
        if not pieces:
            return
        big = max(pieces, key=lambda p: p[1] - p[0])
        ay = (big[0] + min(big[1], self.y_draw_bottom)) / 2
        if c.type in (CT.UPPER_FLEX_JOINT, CT.LOWER_FLEX_JOINT):
            ax = self.cx + 10
        else:
            ax = self.cx + wpx / 2 + 1
        self.anchors[("data-component-id", c.id)] = (ax, ay)

    def _rows(self) -> list[dict]:
        """Every table row: drill floor, tensioners, then components."""
        s = self.s
        rows = []
        if s.datums.drill_floor_el_m not in (None, NOT_FOUND):
            rows.append({"kind": "datum", "key": ("data-datum", "drill_floor_el_m")})
        if s.tensioner_system is not None:
            rows.append({"kind": "tensioner", "key": ("data-row", "tensioner_system")})
        for c in s.components:
            rows.append(
                {"kind": "component", "key": ("data-component-id", c.id), "c": c}
            )
        for r in rows:
            r["anchor"] = self.anchors.get(r["key"])
        return rows

    def _val(self, v, fld, unit, dec=None, *, signed=False, factor=1.0):
        if v is None:
            return list(DASH)
        return _num(v, fld, unit, dec, signed=signed, factor=factor)

    def _cells(self, row: dict) -> list[tuple[str, list]]:
        """``(data-col, spans)`` of one row; the design-data cell is added later."""
        s = self.s
        if row["kind"] == "datum":
            v = s.datums.drill_floor_el_m
            return [
                ("component", [_txt("Drill floor (RKB)")]),
                ("qty", list(DASH)),
                ("top", _num(v, "datums.drill_floor_el_m", "m", 2, signed=True)),
                ("top_ft", self._ft(v, "datums.drill_floor_el_m")),
                ("bot", list(DASH)),
                ("od", list(DASH)),
                ("wall", list(DASH)),
                ("buoy", list(DASH)),
            ]
        if row["kind"] == "tensioner":
            ts = s.tensioner_system
            radius = self._val(
                ts.sheave_radius_m, "tensioner_system.sheave_radius_m", "m", 2
            )
            label = [_txt("Tensioners, sheave R ")] + radius
            if ts.sheave_radius_m not in (None, NOT_FOUND):
                label += [_txt(" m")]
            return [
                ("component", label),
                ("qty", self._val(ts.count, "tensioner_system.count", "count", 0)),
                ("qty_len", list(DASH)),
                (
                    "top",
                    self._val(
                        ts.sheave_el_m,
                        "tensioner_system.sheave_el_m",
                        "m",
                        2,
                        signed=True,
                    ),
                ),
                ("top_ft", self._ft(ts.sheave_el_m, "tensioner_system.sheave_el_m")),
                ("bot", list(DASH)),
                ("od", list(DASH)),
                ("wall", list(DASH)),
                ("buoy", list(DASH)),
            ]
        c = row["c"]
        out = [
            ("component", [_txt(c.label[:1].upper() + c.label[1:])]),
            ("qty", self._val(c.count, "count", "count", 0)),
        ]
        if c.joint_length_m is None:
            out.append(("qty_len", list(DASH)))
        else:
            dec = 1
            if c.joint_length_m != NOT_FOUND:
                lft = float(c.joint_length_m) * M2FT
                dec = 0 if abs(lft - round(lft)) < 0.05 else 1
            out.append(("qty_x", [_txt("×")]))
            out.append(
                (
                    "qty_len",
                    _num(c.joint_length_m, "joint_length_m", "ft", dec, factor=M2FT),
                )
            )
        out.append(("top", self._val(c.top_el_m, "top_el_m", "m", 2, signed=True)))
        out.append(("top_ft", self._ft(c.top_el_m, "top_el_m")))
        out.append(
            ("bot", self._val(c.bottom_el_m, "bottom_el_m", "m", 2, signed=True))
        )
        if c.od_in is not None:
            out.append(("od", _num(c.od_in, "od_in", "in")))
        elif c.envelope_width_in is not None:
            out.append(("od_w", [_txt("W")]))
            out.append(("od", _num(c.envelope_width_in, "envelope_width_in", "in")))
        else:
            out.append(("od", list(DASH)))
        out.append(
            ("wall", self._val(c.wall_thickness_in, "wall_thickness_in", "in", 3))
        )
        if c.buoyancy_od_in is None and c.buoyancy_depth_rating_ft is None:
            out.append(("buoy", list(DASH)))
        else:
            out.append(("buoy", self._val(c.buoyancy_od_in, "buoyancy_od_in", "in")))
            out.append(("buoy_sep", [_txt("/")]))
            out.append(
                (
                    "buoy_rating",
                    self._val(
                        c.buoyancy_depth_rating_ft, "buoyancy_depth_rating_ft", "ft", 0
                    ),
                )
            )
        return out

    def _ft(self, v, fld):
        if v is None:
            return list(DASH)
        if v == NOT_FOUND:
            return _num(v, fld, "ft")
        return (
            [_txt("(")] + _num(v, fld, "ft", 1, signed=True, factor=M2FT) + [_txt(")")]
        )

    def _prov_of(self, row: Optional[dict], fld: str):
        """Provenance entry behind a printed field (a row field or a dotted path)."""
        s = self.s
        if "." not in fld:
            return row["c"].provenance.get(fld) if row and row.get("c") else None
        head, rest = fld.split(".", 1)
        if head == "datums":
            return s.datums.provenance.get(rest)
        if head == "tensioner_system" and s.tensioner_system is not None:
            return s.tensioner_system.provenance.get(rest)
        if head == "reference_totals":
            return s.reference_totals.get(rest.rsplit(".", 1)[0])
        return None

    def _dd_ids(self, row: Optional[dict], spans_list) -> tuple[list[str], bool]:
        """Sorted D-IDs behind the printed values, and whether any value lacks one."""
        ids, missing = set(), False
        for spans in spans_list:
            for _, a in spans:
                fld = a.get("data-field")
                if fld is None or a.get("class") == "na" or fld.startswith("derived."):
                    continue
                if fld == "datums.msl_el_m":
                    continue
                prov = self._prov_of(row, fld)
                did = getattr(prov, "design_data_id", None)
                if did is not None and self.s.design_item(did) is not None:
                    ids.add(did)
                else:
                    missing = True
        return sorted(ids, key=lambda d: int(d.split("-")[1])), missing

    def _dd_spans(self, ids: list[str], missing: bool, lead: str = "") -> list:
        """``D-nn`` + class flag per id, comma separated; ``n/a`` when a value has none."""
        parts = []
        for did in ids:
            flag = SOURCE_CLASSES[self.s.design_item(did).source_class]
            parts.append(
                [(did, {"data-dd": did}), (flag, {"class": f"ddc-{flag.lower()}"})]
            )
        if missing:
            parts.append([("n/a", {"class": "na"})])
        out = [_txt(lead)] if lead and parts else []
        for i, part in enumerate(parts):
            if i:
                out.append(_txt(", "))
            out += part
        return out

    def _dd_column_width(self) -> float:
        """Design data column width: the widest row cell plus pads, in whole steps."""
        widest = 0.0
        for row in self._rows():
            ids, missing = self._dd_ids(row, [sp for _, sp in self._cells(row)])
            widest = max(widest, dd_text_width(self._dd_spans(ids, missing)))
        need = CELL_POS["dd"][1] + widest + DD_RIGHT_PAD
        base = dict((k, w) for k, _, _, w in COLUMNS)["dd"]
        return max(base, math.ceil(need / DD_WIDTH_STEP) * DD_WIDTH_STEP)

    def _inline_dd(self, spans: list) -> list:
        """Append the D-IDs of the values printed in ``spans`` (non-table text)."""
        ids, _ = self._dd_ids(None, [spans])
        return spans + self._dd_spans(ids, False, lead=" ")

    def place_table(self) -> None:
        P = ROW_PITCH
        x0, x1 = TABLE_X0, self.table_x1
        rows = self._rows()
        drawn = sorted(
            [r for r in rows if r["anchor"] is not None], key=lambda r: r["anchor"][1]
        )
        undrawn = [r for r in rows if r["anchor"] is None]
        lo = self.lay.top_margin + HEADER_HEIGHT + P / 2
        legend_top = self.y_draw_bottom - LEGEND_HEIGHT
        hi = legend_top - P / 2 - len(undrawn) * P
        pos = _relax([r["anchor"][1] for r in drawn], P, lo, hi)
        placed = list(zip(drawn, pos)) + [
            (r, hi + P * (k + 1)) for k, r in enumerate(undrawn)
        ]
        o = self.out
        o.extend(self._header())
        review = {}
        for kind, i, entry in open_review_entries(self.s):
            review.setdefault(entry.get("component_id"), []).append(f"{kind}[{i}]")
        for n, (row, yc) in enumerate(placed):
            akey, aval = row["key"]
            g = [f'<g data-role="table-row" {akey}="{html.escape(aval, quote=True)}">']
            g.append(
                svg_rect(x0, yc - P / 2, x1 - x0, P, "row-a" if n % 2 == 0 else "row-b")
            )
            g.append(svg_line(x0, yc + P / 2, x1, yc + P / 2, "row-rule"))
            if row["anchor"] is not None:
                ax, ay = row["anchor"]
                bend = max(ax + 2, x0 - 34)
                g.append(
                    svg_polyline(
                        [(ax + 2, ay), (bend, ay), (x0 - 10, yc), (x0 - 1, yc)],
                        "leader",
                    )
                )
                g.append(svg_circle(ax + 2, ay, 1.8, "dot"))
            cells = self._cells(row)
            ids, missing = self._dd_ids(row, [sp for _, sp in cells])
            cells.append(("dd", self._dd_spans(ids, missing)))
            base = yc + CELL_BASELINE
            for col, spans in cells:
                ckey, off, anchor = CELL_POS[col]
                cls = "tdd" if col == "dd" else ("tl" if col in LITERAL_COLS else "tc")
                g.append(
                    svg_text(
                        self.col_x[ckey] + off,
                        base,
                        spans,
                        cls,
                        text_anchor=anchor,
                        data_col=col,
                    )
                )
            rkey = {
                "datum": "datums",
                "tensioner": "tensioner_system",
                "component": aval,
            }[row["kind"]]
            if rkey in review:
                wx = x1 - WARN_INSET
                g.append(
                    svg_path(
                        f"M{_n(wx)},{_n(yc + 5)} l5.5,-10 l5.5,10 z",
                        "warn",
                        data_part="warn",
                        data_review=" ".join(review[rkey]),
                    )
                )
            g.append("</g>")
            o.extend(g)
        o.extend(self._legend(legend_top))

    def _header(self) -> list[str]:
        x0, x1, y0 = TABLE_X0, self.table_x1, self.lay.top_margin
        g = ['<g data-role="table-header">']
        g.append(svg_rect(x0, y0, x1 - x0, HEADER_HEIGHT, "hdr-bg"))
        for key, h1, h2, _ in COLUMNS:
            a = self.col_x[key]
            b = a + self.col_w[key]
            if key == "component":
                g.append(svg_text(a + 6, y0 + 20, [_txt(h1)], "th", data_col=key))
            else:
                xm = (a + b) / 2
                g.append(
                    svg_text(
                        xm,
                        y0 + 13,
                        [_txt(h1)],
                        "th",
                        text_anchor="middle",
                        data_col=key,
                    )
                )
                g.append(
                    svg_text(
                        xm,
                        y0 + 26,
                        [_txt(h2)],
                        "thu",
                        text_anchor="middle",
                        data_col=key,
                    )
                )
            if a > x0:
                g.append(svg_line(a, y0 + 3, a, y0 + HEADER_HEIGHT - 3, "grid"))
        g.append("</g>")
        return g

    def _legend(self, y_top: float) -> list[str]:
        x = TABLE_X0 + 2
        y = y_top + 14
        g = ['<g data-role="table-legend">']
        g.append(
            svg_text(
                x,
                y,
                [
                    _txt(
                        "Design data: D-ID = item in the report's Design data table; "
                    ),
                    ("P", {"class": "ddc-p"}),
                    _txt(" = public source · "),
                    ("D", {"class": "ddc-d"}),
                    _txt(" = owner decision · "),
                    ("A", {"class": "ddc-a"}),
                    _txt(" = ASSUMED - to be confirmed (no public data)."),
                ],
                "tl",
            )
        )
        g.append(
            svg_text(
                x,
                y + 15,
                [
                    ("n/a", {"class": "na"}),
                    _txt(" applies but not found in the sources · "),
                    ("–", {"class": "nap"}),
                    _txt(" not applicable to the item type."),
                ],
                "tl",
            )
        )
        g.append(svg_path(f"M{_n(x)},{_n(y + 35)} l5.5,-10 l5.5,10 z", "warn"))
        g.append(
            svg_text(
                x + 16,
                y + 34,
                [_txt("Open data conflict or gap for the item: see the review list.")],
                "tl",
            )
        )
        g.append("</g>")
        return g

    # -- review list (L12) --------------------------------------------------------------------
    def _review_lines(self) -> list[tuple[Optional[str], list]]:
        """``(data-review key or None, spans)`` per line of the review list."""
        lines: list[tuple[Optional[str], list]] = []
        width_chars = int((self.width - 24 - REVIEW_TEXT_X - 12) / REVIEW_CHAR_PX)
        for kind, i, e in open_review_entries(self.s):
            key = f"{kind}[{i}]"
            path = f"{kind}.{i}"
            head = [
                _txt("Conflict" if kind == "data_conflicts" else "Gap"),
                _txt(" · "),
            ]
            cid = e.get("component_id")
            if cid is None:
                head += [_txt("drawing")]
            else:
                head += [
                    (
                        str(cid),
                        {"data-field": f"{path}.component_id", "data-kind": "text"},
                    )
                ]
            if e.get("item") not in (None, ""):
                head += [
                    _txt(" · "),
                    (
                        str(e["item"]),
                        {"data-field": f"{path}.item", "data-kind": "text"},
                    ),
                ]
            detail = e.get("detail")
            if detail in (None, ""):
                lines.append((key, head))
                continue
            head += [_txt(": ")]
            used = sum(len(t) for t, _ in head)
            words = str(detail).split(" ")
            fld = {"data-field": f"{path}.detail", "data-kind": "text"}
            cur: list[str] = []
            budget = max(width_chars - used, 20)
            first = True
            for w in words:
                trial = " ".join(cur + [w])
                if cur and len(trial) > budget:
                    spans = (head if first else []) + [(" ".join(cur), dict(fld))]
                    lines.append((key, spans))
                    first, cur, budget = False, [w], width_chars
                else:
                    cur.append(w)
            spans = (head if first else []) + [(" ".join(cur), dict(fld))]
            lines.append((key, spans))
        if not lines:
            lines.append((None, [_txt("No open data conflicts or gaps.")]))
        return lines

    def review_list(self) -> None:
        x0, y0 = 12.0, self.y_review
        w = self.width - 24
        lines = self.review_lines
        g = ['<g data-role="review-list">']
        g.append(svg_rect(x0, y0, w, self.review_height, "rl-bg"))
        g.append(
            svg_text(
                x0 + 10,
                y0 + REVIEW_HEADER_BASELINE,
                [_txt("REVIEW LIST: OPEN DATA CONFLICTS AND GAPS")],
                "rl-hdr",
            )
        )
        seen = set()
        for k, (key, spans) in enumerate(lines):
            y = y0 + REVIEW_HEADER_BASELINE + (k + 1) * REVIEW_PITCH
            if key is not None and key not in seen:
                seen.add(key)
                g.append(
                    svg_path(
                        f"M{_n(x0 + 10)},{_n(y + 1)} l5.5,-10 l5.5,10 z",
                        "warn",
                        data_part="warn",
                        data_review=key,
                    )
                )
            extra = {"data_review": key} if key is not None else {}
            cls = "rl" if key is not None else "rl-none"
            g.append(svg_text(x0 + REVIEW_TEXT_X, y, spans, cls, **extra))
        g.append("</g>")
        self.out.extend(g)

    # -- title block ---------------------------------------------------------------------------
    def title_block(self) -> None:
        s = self.s
        d, tb, rt = s.datums, s.title_block, s.reference_totals
        x0, y0 = 12.0, self.y_title
        w, h = self.width - 24, self.lay.title_block_height
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
        g.append(svg_text(xs + 10, y0 + 15, [_txt("DOCUMENT")], "tb-hdr"))
        # the report's own number; the archive document_ref is never printed
        doc_fld = "title_block.report_document_no"
        doc = (
            [("n/a", {"class": "na", "data-field": doc_fld})]
            if tb.report_document_no in (None, "")
            else [(tb.report_document_no, {"data-field": doc_fld, "data-kind": "text"})]
        )
        g.append(svg_text(xs + 10, y0 + 35, doc, "tb-val"))
        cells = [
            (
                "WATER DEPTH",
                self._inline_dd(
                    _num(d.water_depth_m, "datums.water_depth_m", "m", 2)
                    + [_txt(" m (")]
                    + _num(
                        d.water_depth_m, "datums.water_depth_m", "ft", 1, factor=M2FT
                    )
                    + [_txt(" ft)")]
                ),
            ),
            (
                "DRILL FLOOR (RKB) ABOVE MSL",
                self._inline_dd(
                    _num(d.air_gap_m, "datums.air_gap_m", "m", 2)
                    + [_txt(" m (")]
                    + _num(d.air_gap_m, "datums.air_gap_m", "ft", 1, factor=M2FT)
                    + [_txt(" ft)")]
                ),
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
            # An adapter-computed residual restates the data; it is not an
            # independent source, so the drawing must not call it one.
            tail = (
                " (not established: adapter-derived, string incomplete)."
                if rt["closure_residual_m"].basis == "adapter"
                else " (source-flagged)."
            )
            closure += (
                [_txt("; residual ")]
                + self._ref("closure_residual_m", 2, " m", signed=True)
                + [_txt(tail)]
            )
        notes.append(closure)
        if "tj_ib_tensioned_m" in rt:
            notes.append(
                [_txt("Telescopic joint: inner barrel out ")]
                + self._ref("tj_ib_geometric_m", 2, " m")
                + [_txt(" as drawn, ")]
                + self._ref("tj_ib_tensioned_m", 2, " m")
                + [_txt(" under target tension (static stretch ")]
                + self._ref("static_stretch_m", 2, " m")
                + [_txt("); ")]
                + self._ref("tj_offset_from_mid_m", 2, " m", signed=True)
                + [_txt(" from mid-stroke (")]
                + self._ref("tj_ib_mid_stroke_m", 2, " m")
                + [_txt(" out).")]
            )
        if "ufj_pivot_el_m" in rt or "lfj_pivot_el_m" in rt:
            notes.append(
                [_txt("Flex-joint pivots (ball symbol): UFJ EL ")]
                + self._ref("ufj_pivot_el_m", 2, " m", signed=True)
                + [_txt(" · LFJ EL ")]
                + self._ref("lfj_pivot_el_m", 2, " m", signed=True)
                + [_txt(".")]
            )
        for k, line in enumerate(notes[:TITLE_NOTE_LINES]):
            g.append(svg_text(x0 + 10, yn + k * 17, line, "tb-note"))
        g.append("</g>")
        self.out.extend(g)

    def _ref(self, key: str, dec: int, suffix: str, signed: bool = False):
        """A reference total, its unit and (when it has one) its D-ID."""
        rv = self.s.reference_totals.get(key)
        if rv is None:
            return [
                ("n/a", {"class": "na", "data-field": f"reference_totals.{key}.value"})
            ]
        spans = _num(rv.value, f"reference_totals.{key}.value", "m", dec, signed=signed)
        if rv.value == NOT_FOUND:
            return spans
        return self._inline_dd(spans + [_txt(suffix)])


def _relax(anchors: list[float], pitch: float, lo: float, hi: float) -> list[float]:
    """Row centres nearest their (sorted) anchors, at least ``pitch`` apart.

    Rows start at their anchors; touching clusters merge and centre on the
    mean anchor, clamped to ``[lo, hi]``.
    """
    pos = list(anchors)
    clusters = [[i] for i in range(len(anchors))]

    def place(cl):
        mean = sum(anchors[i] for i in cl) / len(cl)
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
    return pos


def _av(v: Any) -> str:
    return "" if v is None else str(v)


def render(spec: StackupDrawingSpec, layout: Optional[Layout] = None) -> str:
    """Render ``spec`` to a self-contained SVG string (deterministic)."""
    return _Renderer(spec, layout or Layout()).render()
