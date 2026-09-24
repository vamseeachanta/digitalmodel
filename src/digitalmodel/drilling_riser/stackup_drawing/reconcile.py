"""Reconcile a rendered riser stack-up SVG against its spec (#2152).

``reconcile(spec, svg_text) -> dict`` depends only on :mod:`.schema` and the
standard library; it does not import the renderer. Steps:

1. ``spec.validate()``: every numeric spec value finite (or NOT_FOUND/None),
   before any arithmetic. Problems fail (d) as "spec invalid: ...".
2. Closed grammar (allowlist): only the elements, parent/child placements,
   attributes, attribute values and tspan attribute profiles the renderer
   emits are accepted; the single ``<style>`` and ``<defs>`` must hash to the
   frozen canonical ones; no DOCTYPE, comment, CDATA or processing
   instruction. Only top-level ``<g>`` groups count as rendered instances.
   Violations are reported as "grammar: ..." in (a), in (b) for positioning
   attributes, and in (c) for tspan profiles.
3. The zone table embedded in the SVG is validated, then the root
   ``width``/``height``/``viewBox`` must equal the sheet the zone table and
   the frozen layout constants imply, and every drawn element must lie inside
   it.

Then it checks:

  (a) mapping   - every spec component drawn exactly once; no orphan groups;
                  group data-* attributes equal the spec; every drawn
                  component owns exactly one callout carrying its required
                  fields
  (b) positions - the zone table is finite,
                  positive, contiguous and matches the drawn axis and break
                  marks; each component's body pieces equal the expected
                  per-zone intervals within 0.5 px (no missing, extra,
                  duplicate, gap or overlap); joint seams sit at
                  T(top - i*L); datum lines, sheaves and axis ticks sit at
                  T(z); every required datum is drawn; each callout leader
                  runs from its label to its component's body and each
                  further callout line sits directly under the first
  (c) numbers   - every text holds only flat ``<tspan>`` children with no
                  direct or tail text; the text is read as rendered (all
                  tspans concatenated): every number token, with any sign in
                  front of it, lies wholly inside one field tspan and equals
                  its whole text; no stray digits; every m/ft/in field is
                  followed by its own unit (or " to "/" × " and a field of the
                  same unit); each field uses the precision fixed by its
                  field type and equals the spec value at that precision
  (d) totals    - per component, count x joint length equals the elevation
                  span; lengths sum to the stack-up length; elevations are
                  continuous; water depth and air gap equal the datum
                  elevations; nothing stands above the drill floor; the
                  stack-up closes, or its residual is flagged by an
                  independent source. A residual computed by an adapter from
                  the same data is reported ``not_established``.
  (e) NOT_FOUND - never printed or attributed as a number; shown as the
                  literal text "n/a"

Result: ``"fail"`` if any check fails; else ``"pass_with_open_items"`` if any
check is ``"not_established"``; else ``"pass"``.

Changing the renderer's emitted grammar, stylesheet or definitions is a
deliberate act: update the frozen constants below (``canonical_digests``
recomputes the two hashes) together with the golden snapshot.

CLI::

    python -m digitalmodel.drilling_riser.stackup_drawing.reconcile SPEC.json SVG.svg \
        [--out reconcile.json]

exits 1 on failure (0 for pass and pass_with_open_items).
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import re
import sys
import xml.etree.ElementTree as ET
from pathlib import Path
from typing import Any, Optional

from digitalmodel.drilling_riser.stackup_drawing.schema import (
    NESTED_TYPES,
    NOT_FOUND,
    StackupComponent,
    StackupDrawingSpec,
    not_found_fields,
)
from digitalmodel.drilling_riser.stackup_drawing.schema import (
    ComponentType as CT,
)

__all__ = ["PX_TOL", "reconcile"]

NS = "{http://www.w3.org/2000/svg}"
M2FT = 1.0 / 0.3048
PX_TOL = 0.5
#: Horizontal reach allowed between a leader's anchor and the body edge.
LEADER_TOL_PX = 5.0
#: Distance allowed between a leader's label end and its first text anchor.
LABEL_TOL_PX = 6.0
LEN_TOL_M = 1e-3
DATUM_TOL_M = 1e-6
MAX_DECIMALS = 4
CHECKS = ("a_mapping", "b_positions", "c_numbers", "d_totals", "e_not_found")
ALLOWED_ROLES = {
    "decor",
    "axis",
    "datum",
    "datum-label",
    "tensioner",
    "component",
    "callout",
    "break",
    "titleblock",
}
DATUM_NAMES = ("drill_floor_el_m", "msl_el_m", "mudline_el_m")
#: Types whose callout states count x joint length.
COUNTED_TYPES = {
    CT.RISER_JOINT_BARE,
    CT.RISER_JOINT_BUOYANT,
    CT.PUP_JOINT,
    CT.TERMINATION_JOINT,
    CT.CONDUCTOR,
    CT.CASING,
}
#: A printed number: optional sign, comma-grouped integer part, fraction.
NUM_FULL = re.compile(r"([+\-−]?)(\d{1,3}(?:,\d{3})*)(?:\.(\d+))?")
#: A number as it reads in the rendered string, with any sign before it.
RENDERED_NUM = re.compile(r"(?<![\d.,])([+\-−]\s*)?\d[\d,]*(?:\.\d+)?")

# -- frozen SVG grammar -----------------------------------------------------------
# The closed set of elements and attributes the renderer emits (derived from
# its output for the synthetic, adapter and prototype specs). Anything else is
# rejected. These are grammar, not geometry: no coordinate or scale is taken
# from them. A deliberate renderer change must update them, which the tests
# enforce through the golden snapshot and the positive fixtures.

#: Sheet layout the root must declare: width, and sheet height below the
#: drawing's last zone (gap + title block + margin).
SHEET_WIDTH = 860.0
SHEET_BELOW_DRAWING = 14.0 + 196.0 + 12.0
#: Vertical pitch between successive callout text lines.
CALLOUT_LINE_PITCH = 15.0
#: sha256 of the canonical <style> text and of the canonical <defs> tree.
STYLE_SHA256 = "e5624e4872cafada452ddb0fb9dd042b1e6c80d3728c8ec3ce5c0427f3fb99c3"
DEFS_SHA256 = "5be3761097837b884410f947978afdec12be2ed91ebafa54fd3f9b362ed09c2f"

_ELEMENTS = {
    "svg",
    "title",
    "desc",
    "style",
    "defs",
    "linearGradient",
    "radialGradient",
    "stop",
    "pattern",
    "g",
    "rect",
    "line",
    "circle",
    "path",
    "polyline",
    "text",
    "tspan",
}
#: (element, parent element) -> allowed attributes. ``g`` is keyed by role.
_ATTRS: dict[tuple[str, Optional[str]], frozenset[str]] = {
    ("svg", None): frozenset({"aria-label", "height", "role", "viewBox", "width"}),
    ("title", "svg"): frozenset(),
    ("desc", "svg"): frozenset(),
    ("style", "svg"): frozenset(),
    ("defs", "svg"): frozenset(),
    ("linearGradient", "defs"): frozenset({"id", "x1", "x2", "y1", "y2"}),
    ("radialGradient", "defs"): frozenset({"cx", "cy", "id", "r"}),
    ("pattern", "defs"): frozenset(
        {"height", "id", "patternTransform", "patternUnits", "width"}
    ),
    ("stop", "linearGradient"): frozenset({"offset", "stop-color"}),
    ("stop", "radialGradient"): frozenset({"offset", "stop-color"}),
    ("rect", "pattern"): frozenset({"fill", "height", "width"}),
    ("line", "pattern"): frozenset({"stroke", "stroke-width", "x1", "x2", "y1", "y2"}),
    ("g", "svg"): frozenset(),  # per role, see _G_ATTRS
    ("rect", "g"): frozenset({"class", "data-part", "height", "rx", "width", "x", "y"}),
    ("line", "g"): frozenset(
        {"class", "data-part", "data-tick-el-m", "data-unit", "x1", "x2", "y1", "y2"}
    ),
    ("circle", "g"): frozenset({"class", "cx", "cy", "data-part", "r"}),
    ("path", "g"): frozenset({"class", "d", "data-part"}),
    ("polyline", "g"): frozenset({"class", "points"}),
    ("text", "g"): frozenset({"class", "data-component-id", "text-anchor", "x", "y"}),
    ("tspan", "text"): frozenset(
        {
            "class",
            "data-decimals",
            "data-field",
            "data-kind",
            "data-tick-el-m",
            "data-unit",
        }
    ),
}
_G_ATTRS: dict[str, frozenset[str]] = {
    "component": frozenset(
        {
            "data-role",
            "data-component-id",
            "data-type",
            "data-top-el-m",
            "data-bottom-el-m",
            "data-od-in",
            "data-count",
            "data-source",
            "data-clipped",
            "data-not-drawn",
        }
    ),
    "callout": frozenset({"data-role", "data-component-id"}),
    "datum": frozenset({"data-role", "data-datum", "data-el-m"}),
    "datum-label": frozenset({"data-role", "data-datum"}),
    "tensioner": frozenset({"data-role", "data-sheave-el-m", "data-count"}),
    "axis": frozenset({"data-role"}),
    "decor": frozenset({"data-role"}),
    "break": frozenset({"data-role"}),
    "titleblock": frozenset({"data-role"}),
}
_TRANSFORM_G_ATTRS = frozenset(
    {
        "id",
        "data-role",
        "data-zones",
        "data-px-per-in",
        "data-center-x",
        "data-y-bottom",
    }
)
#: Frozen attribute values (outside <defs>).
_VALUES: dict[str, dict[str, frozenset[str]]] = {
    "svg": {"role": frozenset({"img"})},
    "g": {"data-clipped": frozenset({"bottom"})},
    "rect": {
        "class": frozenset(
            "bg body-thin bonnet buoy-a buoy-b buoy-c chrome cond cond-b connector "
            "cyl flange frame hidden housing pipe pipe-dark pod post ram rig-floor "
            "ring rotary soil tb water".split()
        ),
        "data-part": frozenset({"body", "seam", "symbol"}),
        "rx": frozenset({"2"}),
    },
    "line": {
        "class": frozenset(
            "axis break centre cut datum datum-ml datum-msl rig seam tb-line tick "
            "wire".split()
        ),
        "data-part": frozenset({"body", "datum-line", "seam"}),
        "data-unit": frozenset({"m", "ft"}),
    },
    "circle": {
        "class": frozenset({"ball", "dot", "sheave"}),
        "data-part": frozenset({"sheave", "symbol"}),
    },
    "path": {
        "class": frozenset({"annular", "datum", "hidden", "valve"}),
        "data-part": frozenset({"symbol"}),
    },
    "polyline": {"class": frozenset({"break", "cut", "leader", "wave"})},
    "text": {
        "class": frozenset(
            "axis-hdr co1 co2 dlbl dlbl2 note-l tb-hdr tb-note tb-sub tb-title "
            "tb-val tick-lbl".split()
        ),
        "text-anchor": frozenset({"start", "end"}),
    },
    "tspan": {
        "class": frozenset({"na", "rpt"}),
        "data-kind": frozenset({"text"}),
        "data-unit": frozenset({"count", "ft", "in", "m", "ratio"}),
    },
}
#: Allowed tspan attribute sets.
_TSPAN_PROFILES = {
    frozenset(): "literal",
    frozenset({"class"}): "marker",
    frozenset({"data-field", "data-unit", "data-decimals"}): "number",
    frozenset({"class", "data-field"}): "na-field",
    frozenset({"data-field", "data-kind"}): "text-field",
    frozenset({"data-tick-el-m", "data-unit", "data-decimals"}): "tick",
}
#: Attributes that position or move content: violations go to (b).
_POSITIONAL = frozenset(
    {
        "transform",
        "x",
        "y",
        "dx",
        "dy",
        "rotate",
        "viewBox",
        "x1",
        "x2",
        "y1",
        "y2",
        "cx",
        "cy",
        "textLength",
        "lengthAdjust",
        "patternTransform",
    }
)
_UNIT_JOINERS = (" to ", " × ")
_ZONE_KEYS = ("z_hi", "z_lo", "y_top", "px_per_m")


class _T:
    """Independent forward map of the embedded (validated) zone table."""

    def __init__(self, zones: list[dict]):
        self.z = zones

    @staticmethod
    def y_bot(zn: dict) -> float:
        return zn["y_top"] + (zn["z_hi"] - zn["z_lo"]) * zn["px_per_m"]

    def y(self, z: float, prefer: str = "upper") -> float:
        c = [zn for zn in self.z if zn["z_lo"] - 1e-9 <= z <= zn["z_hi"] + 1e-9]
        zn = (
            (c[0] if prefer == "upper" else c[-1])
            if c
            else (self.z[0] if z > self.z[0]["z_hi"] else self.z[-1])
        )
        return zn["y_top"] + (zn["z_hi"] - z) * zn["px_per_m"]

    def pieces(self, z_top: float, z_bot: float) -> list[tuple[float, float]]:
        """Expected visible (y_top, y_bot) of [z_bot, z_top], one per zone."""
        if abs(z_top - z_bot) < 1e-12:
            for zn in self.z:
                if zn["z_lo"] - 1e-9 <= z_top <= zn["z_hi"] + 1e-9:
                    y = zn["y_top"] + (zn["z_hi"] - z_top) * zn["px_per_m"]
                    return [(y, y)]
            return []
        out = []
        for zn in self.z:
            hi, lo = min(z_top, zn["z_hi"]), max(z_bot, zn["z_lo"])
            if hi > lo + 1e-9:
                out.append(
                    (
                        zn["y_top"] + (zn["z_hi"] - hi) * zn["px_per_m"],
                        zn["y_top"] + (zn["z_hi"] - lo) * zn["px_per_m"],
                    )
                )
        return out


def _num(text: str) -> float:
    return float(
        text.replace(",", "").replace("−", "-").replace("+", "").replace("†", "")
    )


def _known(v: Any) -> bool:
    return v is not None and v != NOT_FOUND


def _finite(v: Any) -> bool:
    return isinstance(v, (int, float)) and not isinstance(v, bool) and math.isfinite(v)


def _float_attr(el: ET.Element, name: str) -> Optional[float]:
    try:
        v = float(el.get(name, "nan"))
    except ValueError:
        return None
    return v if math.isfinite(v) else None


def _fnan(el: ET.Element, name: str) -> float:
    """Finite float attribute, or NaN (NaN never satisfies a tolerance test)."""
    v = _float_attr(el, name)
    return math.nan if v is None else v


def _seam_y(el: ET.Element) -> float:
    if el.tag == NS + "line":
        return _fnan(el, "y1")
    return _fnan(el, "y") + _fnan(el, "height") / 2


def _path(d: Any, path: str) -> Any:
    for part in path.split("."):
        if isinstance(d, dict):
            if part not in d:
                raise KeyError(path)
            d = d[part]
        else:
            raise KeyError(path)
    return d


def _min_decimals(v: float, max_dec: int = 3) -> int:
    for d in range(max_dec + 1):
        if abs(round(v, d) - v) < 1e-9:
            return d
    return max_dec


def _precision_rule(fld: str, value: float) -> Optional[dict[str, set[int]]]:
    """Allowed ``{unit: {decimals}}`` for a printed field, fixed by field type."""
    last = fld.split(".")[-2] if fld.endswith(".value") else fld.split(".")[-1]
    if last == "count":
        return {"count": {0}}
    if fld.startswith("derived.") and not last.endswith("_m"):
        return {"ratio": {1}}
    if last == "joint_length_m":
        return {"ft": {0, 1}, "m": {2}}
    if last == "wall_thickness_in":
        return {"in": {3}}
    if last.endswith("_in"):
        return {"in": {_min_decimals(value)}}
    if last == "buoyancy_depth_rating_ft":
        return {"ft": {0}}
    if last.endswith("_m"):
        return {"m": {2}, "ft": {1}}
    return None


def _parse_number(txt: str) -> Optional[tuple[float, int]]:
    """(value, decimals) when ``txt`` is exactly one formatted number."""
    m = NUM_FULL.fullmatch(txt)
    if m is None:
        return None
    return _num(txt), len(m.group(3) or "")


def _local(name: str) -> str:
    return name.split("}", 1)[1] if name.startswith("{") else name


def _canon(el: ET.Element) -> tuple:
    """Order-preserving canonical form of an element tree (ignores tails)."""
    return (
        _local(el.tag),
        tuple(sorted(el.attrib.items())),
        (el.text or "").strip(),
        tuple(_canon(c) for c in el),
    )


def _sha256(text: str) -> str:
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def canonical_digests(svg_text: str) -> dict[str, str]:
    """sha256 of the <style> text and the canonical <defs> tree of an SVG.

    Used to freeze :data:`STYLE_SHA256` / :data:`DEFS_SHA256` deliberately.
    """
    root = ET.fromstring(svg_text)
    style = root.find(NS + "style")
    defs = root.find(NS + "defs")
    return {
        "style": _sha256(style.text or "") if style is not None else "",
        "defs": _sha256(repr(_canon(defs))) if defs is not None else "",
    }


def _grammar_problems(root: ET.Element, parent: dict) -> list[tuple[str, str]]:
    """(check, reason) for everything outside the frozen SVG grammar."""
    out: list[tuple[str, str]] = []
    if root.tag != NS + "svg":
        return [("a_mapping", f"grammar: root is <{_local(root.tag)}>, not <svg>")]
    for tag in ("title", "desc", "style", "defs"):
        n = sum(1 for c in root if c.tag == NS + tag)
        if n != 1:
            out.append(
                ("a_mapping", f"grammar: <{tag}> appears {n} times (expected 1)")
            )
    for el in root.iter():
        if not el.tag.startswith(NS):
            out.append(
                ("a_mapping", f"grammar: element {el.tag} outside the SVG namespace")
            )
            continue
        tag = _local(el.tag)
        par = parent.get(el)
        ptag = _local(par.tag) if par is not None else None
        if tag not in _ELEMENTS:
            out.append(("a_mapping", f"grammar: element <{tag}> not allowed"))
            continue
        allowed = _ATTRS.get((tag, ptag))
        if allowed is None:
            out.append(("a_mapping", f"grammar: <{tag}> not allowed under <{ptag}>"))
            continue
        if tag == "g":
            role = el.get("data-role")
            if el.get("id") == "elevation-transform" and role == "axis":
                allowed = _TRANSFORM_G_ATTRS
                if len(el):
                    out.append(
                        ("a_mapping", "grammar: elevation transform group has children")
                    )
            elif role in _G_ATTRS:
                allowed = _G_ATTRS[role]
            else:
                out.append(
                    ("a_mapping", f"grammar: <g> without a known data-role ({role!r})")
                )
                allowed = frozenset({"data-role", "data-component-id"})
        for attr in el.attrib:
            if attr not in allowed:
                check = "b_positions" if _local(attr) in _POSITIONAL else "a_mapping"
                out.append(
                    (
                        check,
                        f"grammar: attribute '{_local(attr)}' not allowed on <{tag}>",
                    )
                )
        in_defs = ptag in ("defs", "pattern", "linearGradient", "radialGradient")
        if not in_defs:
            for attr, values in _VALUES.get(tag, {}).items():
                v = el.get(attr)
                if v is not None and v not in values:
                    out.append(
                        ("a_mapping", f"grammar: <{tag}> {attr}={v!r} not allowed")
                    )
        if tag == "style" and _sha256(el.text or "") != STYLE_SHA256:
            out.append(
                (
                    "a_mapping",
                    "grammar: stylesheet differs from the canonical stylesheet",
                )
            )
        if tag == "defs" and _sha256(repr(_canon(el))) != DEFS_SHA256:
            out.append(
                ("a_mapping", "grammar: <defs> differs from the canonical definitions")
            )
        if tag == "tspan":
            out.extend(_tspan_problems(el, parent))
        if tag == "line" and el.get("data-tick-el-m") is not None:
            if (
                par is None
                or par.get("data-role") != "axis"
                or el.get("class") != "tick"
            ):
                out.append(
                    ("a_mapping", "grammar: tick metadata outside the axis group")
                )
    return out


def _tspan_problems(sp: ET.Element, parent: dict) -> list[tuple[str, str]]:
    keys = frozenset(sp.attrib)
    if "data-field" in keys and "data-tick-el-m" in keys:
        return [("c_numbers", "grammar: tspan mixes data-field with tick metadata")]
    profile = _TSPAN_PROFILES.get(keys)
    if profile is None:
        # unknown attributes are reported by the attribute allowlist
        if keys <= _ATTRS[("tspan", "text")]:
            return [
                (
                    "c_numbers",
                    f"grammar: tspan attribute set {sorted(keys)} is not an allowed profile",
                )
            ]
        return []
    text = sp.text or ""
    if profile == "marker" and not (
        (sp.get("class") == "rpt" and text == "†")
        or (sp.get("class") == "na" and text == "n/a")
    ):
        return [
            (
                "c_numbers",
                f"grammar: marker tspan class={sp.get('class')!r} shows {text!r}",
            )
        ]
    if profile == "na-field" and sp.get("class") != "na":
        return [("c_numbers", "grammar: field tspan with a class other than na")]
    if profile == "tick":
        t = parent.get(sp)
        g = parent.get(t) if t is not None else None
        if g is None or g.get("data-role") != "axis" or t.get("class") != "tick-lbl":
            return [("c_numbers", "grammar: tick metadata outside the axis group")]
    return []


def _points(el: ET.Element) -> Optional[list[tuple[float, float]]]:
    """Extreme points of a drawn element, or None when unreadable."""
    tag = _local(el.tag)
    try:
        if tag == "rect":
            x, y = float(el.get("x", "0")), float(el.get("y", "0"))
            w, h = float(el.get("width")), float(el.get("height"))
            pts = [(x, y), (x + w, y + h)]
        elif tag == "line":
            pts = [
                (float(el.get("x1")), float(el.get("y1"))),
                (float(el.get("x2")), float(el.get("y2"))),
            ]
        elif tag == "circle":
            cx, cy, r = float(el.get("cx")), float(el.get("cy")), float(el.get("r"))
            pts = [(cx - r, cy - r), (cx + r, cy + r)]
        elif tag == "polyline":
            pts = [
                tuple(float(v) for v in p.split(","))
                for p in (el.get("points") or "").split()
            ]
        elif tag == "path":
            m = re.match(r"\s*M\s*([-\d.]+)\s*,\s*([-\d.]+)", el.get("d") or "")
            pts = [(float(m.group(1)), float(m.group(2)))] if m else []
        elif tag == "text":
            pts = [(float(el.get("x")), float(el.get("y")))]
        else:
            return []
    except (TypeError, ValueError, AttributeError):
        return None
    if not pts or any(len(p) != 2 or not all(map(math.isfinite, p)) for p in pts):
        return None
    return pts


def _viewport_problems(root: ET.Element, top_groups, y_bottom: float) -> list[str]:
    """Root viewport pinned to the layout; every drawn element inside it."""
    out = []
    w, h = SHEET_WIDTH, y_bottom + SHEET_BELOW_DRAWING
    try:
        vb = [float(v) for v in (root.get("viewBox") or "").split()]
    except ValueError:
        vb = []
    if len(vb) != 4 or any(abs(a - b) > 0.01 for a, b in zip(vb, (0.0, 0.0, w, h))):
        out.append(f"root viewBox {root.get('viewBox')!r} != '0 0 {w:g} {h:.2f}'")
    for name, exp in (("width", w), ("height", h)):
        v = _float_attr(root, name)
        if v is None or abs(v - exp) > 0.01:
            out.append(f"root {name} {root.get(name)!r} != {exp:.2f}")
    for g in top_groups:
        for el in g.iter():
            if el is g or _local(el.tag) == "tspan":
                continue
            pts = _points(el)
            if pts is None:
                out.append(f"<{_local(el.tag)}> has unreadable coordinates")
                continue
            for x, y in pts:
                if not (-PX_TOL <= x <= w + PX_TOL and -PX_TOL <= y <= h + PX_TOL):
                    out.append(
                        f"<{_local(el.tag)}> at ({x:.2f}, {y:.2f}) outside the viewBox"
                    )
                    break
    return out


def _rendered_text_problems(t: ET.Element, ctx: str) -> list[str]:
    """Numbers, signs and units as the whole <text> reads, across tspans."""
    spans = [c for c in t if c.tag == NS + "tspan"]
    rendered, owner = "", []
    for i, sp in enumerate(spans):
        s = sp.text or ""
        rendered += s
        owner += [i] * len(s)
    out: list[str] = []

    def kind(sp) -> Optional[str]:
        return _TSPAN_PROFILES.get(frozenset(sp.attrib))

    covered = set()
    for m in RENDERED_NUM.finditer(rendered):
        s, e = m.span()
        covered.update(range(s, e))
        digits_at = m.start(0) + (len(m.group(1)) if m.group(1) else 0)
        i = owner[digits_at]
        if m.group(1) and owner[s] != i:
            out.append(
                f"{ctx}: sign outside its field before {m.group(0)!r} in {rendered!r}"
            )
            continue
        if any(owner[k] != i for k in range(s, e)):
            out.append(f"{ctx}: number {m.group(0)!r} crosses tspans in {rendered!r}")
            continue
        if kind(spans[i]) == "text-field":
            # verbatim spec text (e.g. a document reference), checked for
            # exact equality with the spec; its digits are not a number
            continue
        if kind(spans[i]) not in ("number", "tick"):
            out.append(f"{ctx}: number {m.group(0)!r} outside a field span")
        elif m.group(0) != (spans[i].text or ""):
            out.append(
                f"{ctx}: number {m.group(0)!r} is not the whole field text "
                f"{spans[i].text!r}"
            )
    stray = [k for k, ch in enumerate(rendered) if ch.isdigit() and k not in covered]
    if stray:
        out.append(f"{ctx}: stray digits in {rendered!r}")
    # every m/ft/in field is followed by its own unit, or by a joiner and
    # another field of the same unit ("EL a to b m", "OD x WT in")
    for i, sp in enumerate(spans):
        unit = sp.get("data-unit")
        if kind(sp) != "number" or unit not in ("m", "ft", "in"):
            continue
        j = i + 1
        if j < len(spans) and spans[j].get("class") == "rpt":
            j += 1
        following = "".join(x.text or "" for x in spans[j:])
        if re.match(rf" {unit}(?![A-Za-z])", following):
            continue
        if (
            j + 1 < len(spans)
            and (spans[j].text or "") in _UNIT_JOINERS
            and kind(spans[j + 1]) == "number"
            and spans[j + 1].get("data-unit") == unit
        ):
            continue
        out.append(
            f"{ctx}: {sp.get('data-field')} is not followed by its unit '{unit}'"
        )
    return out


def _validate_zones(zones: Any, y_bottom: Optional[float], px_per_in) -> list[str]:
    """Structural problems of the embedded zone table (empty = valid)."""
    if not isinstance(zones, list) or not zones:
        return ["zone table is not a non-empty list"]
    problems: list[str] = []
    for i, zn in enumerate(zones):
        if not isinstance(zn, dict):
            return [f"zone {i} is not an object"]
        for k in _ZONE_KEYS:
            if not _finite(zn.get(k)):
                problems.append(f"zone {i} {k}={zn.get(k)!r} is not a finite number")
    if problems:
        return problems
    for i, zn in enumerate(zones):
        if zn["px_per_m"] <= 0:
            problems.append(f"zone {i} scale {zn['px_per_m']} is not positive")
        if zn["z_hi"] <= zn["z_lo"]:
            problems.append(f"zone {i} z_hi {zn['z_hi']} <= z_lo {zn['z_lo']}")
        if i:
            prev = zones[i - 1]
            if abs(zn["z_hi"] - prev["z_lo"]) > 1e-6:
                problems.append(f"zone {i} does not continue zone {i - 1} in elevation")
            if zn["y_top"] < _T.y_bot(prev) - 1e-6:
                problems.append(f"zone {i} overlaps zone {i - 1} on the sheet")
    if y_bottom is None:
        problems.append("data-y-bottom missing or non-finite")
    elif not problems and abs(y_bottom - _T.y_bot(zones[-1])) > 1e-6:
        problems.append("data-y-bottom does not match the last zone")
    if px_per_in is None or px_per_in <= 0:
        problems.append("data-px-per-in missing, non-finite or not positive")
    return problems


def reconcile(spec: StackupDrawingSpec, svg_text: str) -> dict[str, Any]:
    """Check ``svg_text`` against ``spec``.

    ``result`` is ``"pass"``, ``"pass_with_open_items"`` or ``"fail"`` (see
    the module docstring). The report carries one entry per check
    (``a_mapping`` .. ``e_not_found``) with ``status`` (``pass``,
    ``not_established`` or ``fail``), ``failures``, ``notes`` and
    ``open_items``, plus the spec's NOT_FOUND paths, conflicts and gaps.
    """
    res: dict[str, dict[str, Any]] = {
        k: {"status": "pass", "failures": [], "notes": [], "open_items": []}
        for k in CHECKS
    }
    # the spec first: no arithmetic on NaN/inf or on a structurally bad spec
    spec_problems = spec.validate()
    if spec_problems:
        for p in spec_problems:
            res["d_totals"]["status"] = "fail"
            res["d_totals"]["failures"].append(f"spec invalid: {p}")
        return _finish(spec, res)
    if re.search(r"<!|<\?", svg_text):
        res["a_mapping"]["status"] = "fail"
        res["a_mapping"]["failures"].append(
            "grammar: DOCTYPE, entity, comment, CDATA or processing instruction"
        )
        return _finish(spec, res)
    root = ET.fromstring(svg_text)
    parent = {c: p for p in root.iter() for c in p}
    sd = spec.to_dict()
    comps = {c.id: c for c in spec.components}

    def fail(k, msg):
        res[k]["status"] = "fail"
        res[k]["failures"].append(msg)

    def note(k, msg):
        res[k]["notes"].append(msg)

    def open_item(k, msg):
        if res[k]["status"] == "pass":
            res[k]["status"] = "not_established"
        res[k]["open_items"].append(msg)

    def role_of(el):
        while el is not None:
            r = el.get("data-role")
            if r:
                return r, el
            el = parent.get(el)
        return None, None

    # closed grammar: only what the renderer emits ------------------------------
    for check, msg in _grammar_problems(root, parent):
        fail(check, msg)
    top_groups = [g for g in root if g.tag == NS + "g"]
    texts = [t for g in top_groups for t in g.iter(NS + "text")]

    # zone table: validate before any arithmetic ------------------------------------
    tf = next((g for g in top_groups if g.get("id") == "elevation-transform"), None)
    if tf is None:
        fail("b_positions", "no embedded elevation transform")
        return _finish(spec, res)
    try:
        zones = json.loads(tf.get("data-zones") or "")
    except (TypeError, ValueError) as exc:
        fail("b_positions", f"zone table is not valid JSON: {exc}")
        return _finish(spec, res)
    px_per_in = _float_attr(tf, "data-px-per-in")
    y_bottom = _float_attr(tf, "data-y-bottom")
    zone_problems = _validate_zones(zones, y_bottom, px_per_in)
    if zone_problems:
        for p in zone_problems:
            fail("b_positions", f"zone table: {p}")
        return _finish(spec, res)
    T = _T(zones)
    z_min = zones[-1]["z_lo"]
    for msg in _viewport_problems(root, top_groups, y_bottom):
        fail("b_positions", msg)
    if root.get("aria-label") != spec.title_block.title:
        fail("a_mapping", "root aria-label differs from the spec title")
    d_ = spec.datums
    top_datum = float(d_.drill_floor_el_m) if _known(d_.drill_floor_el_m) else 0.0
    if zones[0]["z_hi"] < top_datum - 1e-6:
        fail("b_positions", "zone table does not reach the drill floor")
    if _known(d_.mudline_el_m) and z_min > float(d_.mudline_el_m) + 1e-6:
        fail("b_positions", "zone table does not reach the mudline")
    derived = {
        "derived.diameter_exaggeration": px_per_in
        * (1 / 0.0254)
        / zones[0]["px_per_m"],
        "derived.condensed_ratio": (
            (zones[0]["px_per_m"] / zones[1]["px_per_m"]) if len(zones) > 1 else None
        ),
        "derived.wd_plus_air_gap_m": (
            float(d_.water_depth_m) + float(d_.air_gap_m)
            if _known(d_.water_depth_m) and _known(d_.air_gap_m)
            else NOT_FOUND
        ),
    }

    # (a) mapping -----------------------------------------------------------------
    # only top-level groups are rendered instances (the grammar forbids others)
    groups = top_groups
    comp_groups: dict[str, list] = {}
    callout_groups: dict[str, list] = {}
    for g in groups:
        r = g.get("data-role")
        cid = g.get("data-component-id")
        if r is None and cid is None:
            fail(
                "a_mapping",
                f"orphan <g> without data-role/data-component-id (id={g.get('id')})",
            )
            continue
        if r is not None and r not in ALLOWED_ROLES:
            fail("a_mapping", f"unknown data-role {r!r}")
        if r == "component":
            comp_groups.setdefault(cid, []).append(g)
            if cid not in comps:
                fail("a_mapping", f"component group {cid!r} not in spec (orphan)")
        if r == "callout":
            callout_groups.setdefault(cid, []).append(g)
            if cid not in comps:
                fail("a_mapping", f"callout for unknown component {cid!r}")
        if r == "decor":
            for t in g.iter(NS + "text"):
                if re.search(r"\d", "".join(t.itertext())):
                    fail("a_mapping", "decor element carries a number")
    for t in texts:
        cid = t.get("data-component-id")
        if cid is None:
            continue
        r, owner = role_of(t)
        if r != "callout" or owner.get("data-component-id") != cid:
            fail("a_mapping", f"{cid}: callout text outside its own callout group")
    for cid, c in comps.items():
        gl = comp_groups.get(cid, [])
        if len(gl) != 1:
            fail("a_mapping", f"{cid}: drawn {len(gl)} times (expected exactly once)")
            continue
        g = gl[0]
        expect = {
            "data-type": c.type.value,
            "data-count": str(c.count),
            "data-source": c.source,
            "data-od-in": "" if c.od_in is None else str(c.od_in),
        }
        for k, v in expect.items():
            if g.get(k) != v:
                fail("a_mapping", f"{cid}: {k}={g.get(k)!r} != spec {v!r}")
        for k, f in (
            ("data-top-el-m", c.top_el_m),
            ("data-bottom-el-m", c.bottom_el_m),
        ):
            a = g.get(k)
            if _known(f):
                try:
                    ok = abs(float(a) - float(f)) <= 1e-9
                except (TypeError, ValueError):
                    ok = False
                if not ok:
                    fail("a_mapping", f"{cid}: {k}={a!r} != spec {f!r}")
            elif a != str(f):
                fail("a_mapping", f"{cid}: {k}={a!r} but spec is {f!r}")
        cl = callout_groups.get(cid, [])
        if len(cl) != 1:
            fail("a_mapping", f"{cid}: {len(cl)} callout groups (expected exactly one)")
            continue
        shown = {
            sp.get("data-field")
            for t in cl[0].iter(NS + "text")
            if t.get("data-component-id") == cid
            for sp in t.iter(NS + "tspan")
        }
        missing = sorted(_required_callout_fields(c) - shown)
        if missing:
            fail("a_mapping", f"{cid}: callout lacks required fields {missing}")
    note(
        "a_mapping",
        f"{len(comps)} spec components, {sum(len(v) for v in comp_groups.values())} component groups",
    )

    # (b) positions ---------------------------------------------------------------
    for problem in _axis_correspondence(root, zones):
        fail("b_positions", problem)

    def y_ext(el):
        if el.tag == NS + "rect":
            y, h = _float_attr(el, "y"), _float_attr(el, "height")
            return None if y is None or h is None or h < 0 else (y, y + h)
        if el.tag == NS + "line":
            y1, y2 = _float_attr(el, "y1"), _float_attr(el, "y2")
            return None if y1 is None or y2 is None else (min(y1, y2), max(y1, y2))
        return None

    def x_ext(el):
        if el.tag == NS + "rect":
            x, w = _float_attr(el, "x"), _float_attr(el, "width")
            return None if x is None or w is None else (x, x + w)
        if el.tag == NS + "line":
            x1, x2 = _float_attr(el, "x1"), _float_attr(el, "x2")
            return None if x1 is None or x2 is None else (min(x1, x2), max(x1, x2))
        return None

    max_dev = 0.0
    body_boxes: dict[str, list[tuple[float, float, float, float]]] = {}
    for cid, c in comps.items():
        gl = comp_groups.get(cid, [])
        if len(gl) != 1:
            continue
        g = gl[0]
        bodies = [e for e in g.iter() if e.get("data-part") == "body"]
        if not (_known(c.top_el_m) and _known(c.bottom_el_m)):
            if bodies:
                fail("b_positions", f"{cid}: elevation NOT_FOUND but a body is drawn")
            else:
                note("b_positions", f"{cid}: not drawn (elevation NOT_FOUND)")
            continue
        top, bot = float(c.top_el_m), float(c.bottom_el_m)
        clipped = bot < z_min - 1e-9
        if clipped and g.get("data-clipped") != "bottom":
            fail(
                "b_positions",
                f"{cid}: extends below the sheet but is not flagged data-clipped",
            )
        actual = []
        for e in bodies:
            ye, xe = y_ext(e), x_ext(e)
            if ye is None or xe is None:
                fail("b_positions", f"{cid}: unreadable body element <{e.tag}>")
                continue
            actual.append(ye)
            body_boxes.setdefault(cid, []).append((*xe, *ye))
        actual.sort()
        expected = sorted((yt, min(yb, y_bottom)) for yt, yb in T.pieces(top, bot))
        if len(actual) != len(expected):
            fail(
                "b_positions",
                f"{cid}: {len(actual)} body pieces drawn, the zones imply {len(expected)}",
            )
        else:
            for k, ((at, ab), (et, eb)) in enumerate(zip(actual, expected)):
                dev = max(abs(at - et), abs(ab - eb))
                max_dev = max(max_dev, dev)
                if dev > PX_TOL:
                    fail(
                        "b_positions",
                        f"{cid}: body piece {k} y=[{at:.2f}, {ab:.2f}] vs "
                        f"expected [{et:.2f}, {eb:.2f}] (|d|={dev:.2f} px)",
                    )
        # joint seams (verifies count x length inside the condensed break too)
        if c.type in (CT.RISER_JOINT_BARE, CT.RISER_JOINT_BUOYANT) and _known(
            c.joint_length_m
        ):
            n = int(c.count)
            tag = NS + ("line" if c.type == CT.RISER_JOINT_BUOYANT else "rect")
            seams = sorted(
                _seam_y(e) for e in g.iter(tag) if e.get("data-part") == "seam"
            )
            exp = sorted(T.y(top - i * float(c.joint_length_m)) for i in range(1, n))
            if len(seams) != len(exp):
                fail(
                    "b_positions",
                    f"{cid}: {len(seams)} joint seams drawn, data implies {len(exp)}",
                )
            else:
                bad = [(a, b) for a, b in zip(seams, exp) if not abs(a - b) <= PX_TOL]
                if bad:
                    fail(
                        "b_positions",
                        f"{cid}: {len(bad)} joint seams off T(top - i*L) by > {PX_TOL} px",
                    )
                zk = {
                    zn.get("kind", "?")
                    for zn in zones
                    if zn["z_lo"] < top and zn["z_hi"] > bot
                }
                note(
                    "b_positions",
                    f"{cid}: {n} joints x {float(c.joint_length_m):.4f} m, {len(seams)} seams at "
                    f"T(top-i*L) (zones: {', '.join(sorted(str(k) for k in zk))})",
                )
    # callout leaders: label end at the text, anchor end on the component body
    for cid, cl in callout_groups.items():
        if cid not in comps or len(cl) != 1 or cid not in body_boxes:
            continue
        for problem in _leader_problems(cid, cl[0], body_boxes[cid]):
            fail("b_positions", problem)
    # datums: every known datum drawn exactly once, at its elevation
    datum_groups: dict[str, list] = {}
    for g in groups:
        if g.get("data-role") == "datum":
            datum_groups.setdefault(g.get("data-datum"), []).append(g)
    for name in datum_groups:
        if name not in DATUM_NAMES:
            fail("a_mapping", f"unknown datum {name!r}")
    for name in DATUM_NAMES:
        val = getattr(d_, name)
        gl = datum_groups.get(name, [])
        if not _known(val):
            if gl:
                fail("b_positions", f"datum {name}: drawn although NOT_FOUND")
            continue
        if len(gl) != 1:
            fail("b_positions", f"datum {name}: drawn {len(gl)} times (expected once)")
            continue
        el_attr = _float_attr(gl[0], "data-el-m")
        if el_attr is None or abs(el_attr - float(val)) > 1e-9:
            fail(
                "b_positions",
                f"datum {name}: data-el-m={gl[0].get('data-el-m')!r} != spec {val!r}",
            )
        lines = [
            ln for ln in gl[0].iter(NS + "line") if ln.get("data-part") == "datum-line"
        ]
        if not lines:
            fail("b_positions", f"datum {name}: no datum line")
        for ln in lines:
            y1, y2 = _float_attr(ln, "y1"), _float_attr(ln, "y2")
            exp_y = T.y(float(val))
            if (
                y1 is None
                or y2 is None
                or max(abs(y1 - exp_y), abs(y2 - exp_y)) > PX_TOL
            ):
                fail("b_positions", f"datum {name}: line not at T({val})")
    for g in groups:
        if g.get("data-role") == "tensioner" and spec.tensioner_system is not None:
            for cc in g.iter(NS + "circle"):
                if cc.get("data-part") == "sheave":
                    cy = _float_attr(cc, "cy")
                    exp_y = T.y(float(spec.tensioner_system.sheave_el_m))
                    if cy is None or abs(cy - exp_y) > PX_TOL:
                        fail("b_positions", "tensioner sheave not at T(sheave_el_m)")
    n_ticks = 0
    for ln in (ln for g in top_groups for ln in g.iter(NS + "line")):
        z = ln.get("data-tick-el-m")
        if z is not None:
            n_ticks += 1
            zf, y1 = _float_attr(ln, "data-tick-el-m"), _float_attr(ln, "y1")
            if zf is None or y1 is None or abs(y1 - T.y(zf)) > PX_TOL:
                fail("b_positions", f"axis tick {z} not at T({z})")
    note(
        "b_positions",
        f"max body-piece deviation {max_dev:.3f} px; {n_ticks} axis ticks checked",
    )

    # (c) numbers + (e) NOT_FOUND -----------------------------------------------------
    n_checked = 0
    for t in texts:
        cid = t.get("data-component-id")
        r, _ = role_of(t)
        for msg in _rendered_text_problems(t, f"{r}:{cid or ''}"):
            fail("c_numbers", msg)
        if (t.text or "").strip():
            fail("c_numbers", f"text outside a tspan: {t.text.strip()!r} (role={r})")
        for ch in t:
            if ch.tag != NS + "tspan" or len(ch):
                fail("c_numbers", f"<text> child <{ch.tag}> is not a flat <tspan>")
            if (ch.tail or "").strip():
                fail(
                    "c_numbers",
                    f"text outside a tspan: {ch.tail.strip()!r} (role={r}, component={cid})",
                )
        for sp in t.iter(NS + "tspan"):
            txt = sp.text or ""
            fld = sp.get("data-field")
            classes = (sp.get("class") or "").split()
            if txt == "n/a":
                if fld is not None:
                    exp = _resolve(fld, cid, comps, sd, derived)
                    if exp != NOT_FOUND:
                        fail(
                            "e_not_found",
                            f"{cid or ''}:{fld} shown as n/a but spec value is {exp!r}",
                        )
                continue
            if "na" in classes:
                fail(
                    "e_not_found",
                    f"{cid or ''}:{fld} has class na but shows {txt!r}, not 'n/a'",
                )
                continue
            if sp.get("data-tick-el-m") is not None:
                z = _float_attr(sp, "data-tick-el-m")
                parsed = _parse_number(txt)
                unit = sp.get("data-unit")
                if (
                    z is None
                    or parsed is None
                    or parsed[1] != 0
                    or sp.get("data-decimals") != "0"
                    or unit not in ("m", "ft")
                ):
                    fail(
                        "c_numbers", f"axis label {txt!r} is not an integer tick label"
                    )
                else:
                    v = z * (M2FT if unit == "ft" else 1.0)
                    if abs(parsed[0] - v) > 0.5 + 1e-6:
                        fail("c_numbers", f"axis label {txt!r} != {v:.3f}")
                n_checked += 1
                continue
            if fld is None:
                if re.search(r"\d", txt):
                    fail(
                        "c_numbers",
                        f"untraceable number {txt!r} (role={r}, component={cid})",
                    )
                continue
            if sp.get("data-kind") == "text":
                try:
                    exp = _path(sd, fld)
                except KeyError:
                    exp = None
                if txt != exp:
                    fail("c_numbers", f"{fld}: {txt!r} != {exp!r}")
                n_checked += 1
                continue
            exp = _resolve(fld, cid, comps, sd, derived)
            if not _known(exp):
                fail(
                    "e_not_found",
                    f"{cid or ''}:{fld} printed as {txt!r} but spec value is {exp!r}",
                )
                continue
            problem = _number_problem(fld, float(exp), txt, sp)
            if problem:
                fail("c_numbers", f"{cid or ''}:{fld} {problem}")
            n_checked += 1
    note("c_numbers", f"{n_checked} printed numbers verified")

    for cid, c in comps.items():
        for f in (
            "od_in",
            "envelope_width_in",
            "buoyancy_od_in",
            "wall_thickness_in",
            "joint_length_m",
            "top_el_m",
            "bottom_el_m",
            "count",
            "buoyancy_depth_rating_ft",
        ):
            if getattr(c, f) == NOT_FOUND:
                shown = [
                    sp
                    for t in texts
                    if t.get("data-component-id") == cid
                    for sp in t.iter(NS + "tspan")
                    if sp.get("data-field") == f
                ]
                if any((sp.text or "") != "n/a" for sp in shown):
                    fail("e_not_found", f"{cid}:{f} NOT_FOUND drawn as a value")
                note(
                    "e_not_found",
                    f"{cid}:{f} NOT_FOUND - "
                    + (
                        "rendered as n/a"
                        if shown
                        else "not printed (field not shown for this type)"
                    ),
                )
        g = comp_groups.get(cid, [None])[0]
        if g is not None and c.od_in == NOT_FOUND and g.get("data-od-in") != NOT_FOUND:
            fail(
                "e_not_found",
                f"{cid}: data-od-in attribute shows {g.get('data-od-in')!r} for NOT_FOUND",
            )
        if c.od_in == NOT_FOUND and c.type in (
            CT.UPPER_FLEX_JOINT,
            CT.LOWER_FLEX_JOINT,
        ):
            note(
                "e_not_found",
                f"{cid}: OD n/a - body drawn at the nominal riser width as an unscaled symbol",
            )
        if c.envelope_width_in == NOT_FOUND:
            note(
                "e_not_found",
                f"{cid}: width n/a - frame drawn at the drag diameter, callout shows n/a",
            )

    # (d) totals ---------------------------------------------------------------------------
    _check_totals(spec, res, fail, note, open_item)
    return _finish(spec, res)


def _required_callout_fields(c: StackupComponent) -> set[str]:
    """Fields a drawn component's callout must print (a value or n/a)."""
    req = {"top_el_m"}
    if c.type in COUNTED_TYPES:
        req |= {"count", "joint_length_m"}
    if c.od_in is not None:
        req.add("od_in")
    elif c.envelope_width_in is not None:
        req.add("envelope_width_in")
    if c.type == CT.RISER_JOINT_BUOYANT:
        for f in ("buoyancy_od_in", "buoyancy_depth_rating_ft"):
            if getattr(c, f) is not None:
                req.add(f)
    return req


def _leader_problems(cid: str, group: ET.Element, boxes) -> list[str]:
    leaders = [
        p
        for p in group.iter(NS + "polyline")
        if "leader" in (p.get("class") or "").split()
    ]
    texts = [t for t in group.iter(NS + "text") if t.get("data-component-id") == cid]
    if len(leaders) != 1 or not texts:
        return [
            f"{cid}: callout needs exactly one leader and a label ({len(leaders)} leaders)"
        ]
    try:
        pts = [
            tuple(float(v) for v in pair.split(","))
            for pair in (leaders[0].get("points") or "").split()
        ]
        if len(pts) < 2 or any(
            len(p) != 2 or not all(map(math.isfinite, p)) for p in pts
        ):
            raise ValueError
    except ValueError:
        return [f"{cid}: unreadable leader points"]
    (ax, ay), (lx, ly) = pts[0], pts[-1]
    tx, ty = _float_attr(texts[0], "x"), _float_attr(texts[0], "y")
    out = []
    if (
        tx is None
        or ty is None
        or abs(lx - tx) > LABEL_TOL_PX
        or abs(ly - ty) > LABEL_TOL_PX
    ):
        out.append(f"{cid}: leader does not start at its callout label")
    else:
        # every further line sits directly under the first (tspans cannot move:
        # the grammar forbids x/y/dx/dy on them)
        for i, t in enumerate(texts[1:], start=1):
            x, y = _float_attr(t, "x"), _float_attr(t, "y")
            if (
                x is None
                or y is None
                or abs(x - tx) > PX_TOL
                or abs(y - (ty + i * CALLOUT_LINE_PITCH)) > PX_TOL
            ):
                out.append(f"{cid}: callout line {i + 1} is not under its label")
    on_body = any(
        x0 - LEADER_TOL_PX <= ax <= x1 + LEADER_TOL_PX
        and y0 - PX_TOL <= ay <= y1 + PX_TOL
        for x0, x1, y0, y1 in boxes
    )
    if not on_body:
        out.append(
            f"{cid}: leader does not end on the component body ({ax:.2f}, {ay:.2f})"
        )
    return out


def _axis_correspondence(root: ET.Element, zones: list[dict]) -> list[str]:
    """The drawn axis segments and break marks must match the zone table."""
    out = []
    axis_segs, axis_breaks, zone_breaks = [], [], []
    for g in (c for c in root if c.tag == NS + "g"):
        role = g.get("data-role")
        for el in g:
            cls = (el.get("class") or "").split()
            if role == "axis" and el.tag == NS + "line" and "axis" in cls:
                axis_segs.append(el)
            elif role == "axis" and el.tag == NS + "line" and "break" in cls:
                axis_breaks.append(el)
            elif role == "break" and el.tag == NS + "polyline" and "break" in cls:
                zone_breaks.append(el)
    segs = sorted((_fnan(e, "y1"), _fnan(e, "y2")) for e in axis_segs)
    exp_segs = [(zn["y_top"], _T.y_bot(zn)) for zn in zones]
    if len(segs) != len(exp_segs) or any(
        not (abs(a - c) <= PX_TOL and abs(b - d) <= PX_TOL)
        for (a, b), (c, d) in zip(segs, exp_segs)
    ):
        out.append("axis segments do not match the zone table")
    joins = [(_T.y_bot(zones[i]), zones[i + 1]["y_top"]) for i in range(len(zones) - 1)]
    mids = sorted((_fnan(e, "y1") + _fnan(e, "y2")) / 2 for e in axis_breaks)
    exp_mids = sorted(yb + dy for yb, _ in joins for dy in (6.0, 13.0))
    if len(mids) != len(exp_mids) or any(
        not abs(a - b) <= PX_TOL for a, b in zip(mids, exp_mids)
    ):
        out.append("axis break marks do not match the zone boundaries")
    ys = []
    for e in zone_breaks:
        try:
            ys.append(float((e.get("points") or "").split()[0].split(",")[1]))
        except (IndexError, ValueError):
            ys.append(math.nan)
    exp_ys = sorted((yb + yt) / 2 for yb, yt in joins)
    if len(ys) != len(exp_ys) or any(
        not abs(a - b) <= PX_TOL for a, b in zip(sorted(ys), exp_ys)
    ):
        out.append("break lines do not match the zone boundaries")
    return out


def _number_problem(fld: str, value: float, txt: str, sp: ET.Element) -> Optional[str]:
    """Why ``txt`` is not the permitted printed form of ``value`` (or None)."""
    unit = sp.get("data-unit")
    rule = _precision_rule(fld, value)
    if rule is None:
        return "has no precision rule"
    if unit not in rule:
        return f"unit {unit!r} not allowed (allowed {sorted(rule)})"
    try:
        d = int(sp.get("data-decimals", ""))
    except ValueError:
        return f"data-decimals {sp.get('data-decimals')!r} is not an integer"
    if not 0 <= d <= MAX_DECIMALS or d not in rule[unit]:
        return f"precision {d} dp not allowed for {unit} (allowed {sorted(rule[unit])})"
    parsed = _parse_number(txt)
    if parsed is None:
        return f"printed {txt!r} is not exactly one formatted number"
    got, dec = parsed
    if dec != d:
        return f"printed {txt!r} with {dec} dp, declared {d}"
    last = fld.split(".")[-2] if fld.endswith(".value") else fld.split(".")[-1]
    v = value * (M2FT if unit == "ft" and last.endswith("_m") else 1.0)
    if abs(got - v) > 0.5 * 10 ** (-d) + 1e-9:
        return f"printed {txt!r}, spec {v:.6g} at {d} dp"
    return None


def _check_totals(spec: StackupDrawingSpec, res, fail, note, open_item) -> None:
    d_ = spec.datums
    rt = spec.reference_totals
    k = "d_totals"
    # per component: count x joint length == elevation span
    for c in spec.components:
        if all(
            _known(getattr(c, f))
            for f in ("count", "joint_length_m", "top_el_m", "bottom_el_m")
        ):
            span = float(c.top_el_m) - float(c.bottom_el_m)
            length = int(c.count) * float(c.joint_length_m)
            if abs(length - span) > LEN_TOL_M:
                fail(
                    k,
                    f"{c.id}: count x joint length {length:.4f} m != elevation span {span:.4f} m",
                )
    # datums: water depth and air gap against the datum elevations
    msl = float(d_.msl_el_m)
    if _known(d_.water_depth_m) and _known(d_.mudline_el_m):
        if abs(float(d_.water_depth_m) - (msl - float(d_.mudline_el_m))) > DATUM_TOL_M:
            fail(
                k,
                f"water depth {d_.water_depth_m} m != MSL - mudline "
                f"{msl - float(d_.mudline_el_m)} m",
            )
    if _known(d_.air_gap_m) and _known(d_.drill_floor_el_m):
        if abs(float(d_.air_gap_m) - (float(d_.drill_floor_el_m) - msl)) > DATUM_TOL_M:
            fail(
                k,
                f"air gap {d_.air_gap_m} m != drill floor - MSL "
                f"{float(d_.drill_floor_el_m) - msl} m",
            )
    stacked = [c for c in spec.components if c.type not in NESTED_TYPES]
    if _known(d_.drill_floor_el_m):
        for c in stacked:
            if (
                _known(c.top_el_m)
                and float(c.top_el_m) > float(d_.drill_floor_el_m) + 1e-6
            ):
                fail(k, f"{c.id}: top EL {c.top_el_m} m is above the drill floor")
    for a, b in zip(stacked, stacked[1:]):
        if (
            _known(a.bottom_el_m)
            and _known(b.top_el_m)
            and abs(float(a.bottom_el_m) - float(b.top_el_m)) > 1e-6
        ):
            fail(
                k,
                f"elevation gap/overlap between {a.id} and {b.id}: "
                f"{float(a.bottom_el_m) - float(b.top_el_m):+.4f} m",
            )
    if not _known(d_.mudline_el_m):
        open_item(k, "mudline NOT_FOUND: stack-up totals not established")
        return
    mud = float(d_.mudline_el_m)
    above = [
        c
        for c in stacked
        if _known(c.bottom_el_m) and float(c.bottom_el_m) >= mud - 1e-6
    ]
    sum_el = sum(float(c.top_el_m) - float(c.bottom_el_m) for c in above)
    sum_len = sum(
        int(c.count) * float(c.joint_length_m)
        for c in above
        if _known(c.joint_length_m) and _known(c.count)
    )
    res[k]["values"] = vals = {
        "sum_component_lengths_m": round(sum_len, 4),
        "sum_elevation_spans_m": round(sum_el, 4),
    }
    if abs(sum_len - sum_el) > LEN_TOL_M:
        fail(
            k,
            f"sum(count x joint length) {sum_len:.4f} m != sum(elevation spans) {sum_el:.4f} m",
        )
    ref = rt.get("stackup_length_m")
    if ref is not None and _known(ref.value):
        vals["stackup_length_ref_m"] = ref.value
        if abs(sum_len - float(ref.value)) > LEN_TOL_M:
            fail(
                k,
                f"sum of lengths {sum_len:.4f} m != stack-up length {ref.value} ({ref.source})",
            )
    reff = rt.get("stackup_length_ft")
    if (
        reff is not None
        and _known(reff.value)
        and abs(sum_len * M2FT - float(reff.value)) > 0.01
    ):
        fail(
            k,
            f"sum of lengths {sum_len * M2FT:.3f} ft != {reff.value} ft ({reff.source})",
        )
    ufj = next((c for c in stacked if c.type == CT.UPPER_FLEX_JOINT), None)
    lfj = next((c for c in stacked if c.type == CT.LOWER_FLEX_JOINT), None)
    rl = rt.get("riser_length_ufj_lfj_m")
    if ufj and lfj and rl is not None and _known(rl.value):
        L = float(ufj.top_el_m) - float(lfj.top_el_m)
        vals["riser_length_ufj_top_to_lfj_top_m"] = round(L, 4)
        if abs(L - float(rl.value)) > LEN_TOL_M:
            fail(k, f"UFJ-LFJ length {L:.4f} m != {rl.value} ({rl.source})")
    if _known(d_.water_depth_m) and _known(d_.air_gap_m):
        wd_ag = float(d_.water_depth_m) + float(d_.air_gap_m)
        resid = sum_len - wd_ag
        vals.update(
            {
                "water_depth_plus_air_gap_m": round(wd_ag, 4),
                "closure_residual_m": round(resid, 4),
            }
        )
        flag = rt.get("closure_residual_m")
        if flag is not None and _known(flag.value):
            vals["closure_residual_source_m"] = flag.value
            vals["closure_residual_basis"] = flag.basis
            if abs(resid - float(flag.value)) > LEN_TOL_M:
                fail(
                    k,
                    f"closure residual {resid:.4f} m != flagged {flag.value} ({flag.source})",
                )
            elif abs(resid) > 0.01 and flag.basis == "adapter":
                # the adapter computed this residual from the same data: an
                # identity, not evidence. Closure is open, not passed.
                if resid > 0.01:
                    fail(
                        k,
                        f"stack-up is {resid:.3f} m LONGER than water depth + air gap",
                    )
                else:
                    open_item(
                        k,
                        f"stack-up does NOT close to the drill floor: residual {resid:+.3f} m "
                        f"computed by an adapter ({flag.source}); closure not established",
                    )
            elif abs(resid) > 0.01:
                note(
                    k,
                    f"stack-up does NOT close to the drill floor: residual {resid:+.3f} m, "
                    f"identical to the source's own flag ({flag.source}); recorded as a data gap",
                )
        elif abs(resid) > 0.01:
            fail(
                k,
                f"stack-up does not close: residual {resid:+.4f} m and no source flag",
            )
    else:
        open_item(k, "water depth or air gap NOT_FOUND: closure not established")
    top = stacked[0] if stacked else None
    rk = rt.get("rkb_node_above_mudline_m")
    if (
        top is not None
        and rk is not None
        and _known(rk.value)
        and _known(top.top_el_m)
        and abs(float(top.top_el_m) - (float(rk.value) + mud)) > LEN_TOL_M
    ):
        fail(k, f"top of stack {top.top_el_m} != RKB node {rk.value} + mudline")


def _resolve(fld: str, cid, comps, sd, derived):
    """Spec value behind a printed ``data-field``; absent paths are NOT_FOUND."""
    if fld.startswith("derived."):
        return derived.get(fld, NOT_FOUND)
    if cid is not None and "." not in fld:
        if cid not in comps or not hasattr(comps[cid], fld):
            return NOT_FOUND
        return getattr(comps[cid], fld)
    try:
        return _path(sd, fld)
    except KeyError:
        return NOT_FOUND


def _finish(spec: StackupDrawingSpec, res: dict) -> dict[str, Any]:
    statuses = {v["status"] for v in res.values()}
    if "fail" in statuses:
        result = "fail"
    elif "not_established" in statuses:
        result = "pass_with_open_items"
    else:
        result = "pass"
    return {
        "spec_id": spec.spec_id,
        "result": result,
        "checks": res,
        "not_found": not_found_fields(spec),
        "data_conflicts": spec.data_conflicts,
        "gaps": spec.gaps,
    }


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("spec")
    ap.add_argument("svg")
    ap.add_argument("--out")
    a = ap.parse_args(argv)
    spec = StackupDrawingSpec.from_json(Path(a.spec).read_text(encoding="utf-8"))
    rep = reconcile(spec, Path(a.svg).read_text(encoding="utf-8"))
    text = json.dumps(rep, indent=2, ensure_ascii=False) + "\n"
    if a.out:
        Path(a.out).write_text(text, encoding="utf-8")
    for k, v in rep["checks"].items():
        print(f"{k:14s} {v['status'].upper():15s}  " + "; ".join(v["failures"][:5]))
    print("RESULT", rep["result"].upper())
    return 1 if rep["result"] == "fail" else 0


if __name__ == "__main__":
    sys.exit(main())
