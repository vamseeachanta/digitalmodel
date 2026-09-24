"""Reconcile a rendered riser stack-up SVG against its spec (#2152, #2158).

``reconcile(spec, svg_text) -> dict`` depends only on :mod:`.schema` and the
standard library; it does not import the renderer. Every layout number it
needs (table columns, row pitch, band heights) is frozen below, independent
of the renderer's copy. Steps:

1. ``spec.validate()``: every numeric spec value finite (or NOT_FOUND/None),
   nesting hosts, provenance and reference keys, review-entry
   ``component_id`` values and the design-data register rules (ids,
   resolvable references, source classes) before any arithmetic. Problems
   fail (d) as "spec invalid: ...".
2. Closed grammar (allowlist): only the elements, parent/child placements,
   attributes, attribute values and tspan attribute profiles the renderer
   emits are accepted; the single ``<style>`` and ``<defs>`` must hash to the
   frozen canonical ones; no DOCTYPE, comment, CDATA or processing
   instruction. Only top-level ``<g>`` groups count as rendered instances.
   The table columns (``data-col``) and review keys (``data-review``) are
   closed sets too. Violations are reported as "grammar: ..." in (a), in (b)
   for positioning attributes, and in (c) for tspan profiles.
3. The zone table embedded in the SVG is validated. The root
   ``width``/``height``/``viewBox`` must equal the sheet the table columns
   (width) and the zone table, review band and title block (height) imply,
   and every drawn element must lie inside it.
4. Paint and geometry: the top-level group order (by ``data-role``) must be
   the renderer's; the first painted group is the full-sheet background and
   nothing decorative follows the drawing; the review list and title block
   stay below the drawing. Every drawn element carries its role's class and
   all the attributes the renderer always emits for it; geometric attributes
   and ``points`` are plain finite SVG numbers; each path ``d`` matches the
   full command grammar of its symbol; rects, circles, lines, polylines and
   paths have positive size, with the frozen symbol sizes.

Threat model: the gate is built to catch generator bugs and drift - a
changed renderer, a hand-edited drawing or an AI-generated SVG that follows
the drawing contract - and to state which check a wrong drawing breaks. It
closes: undrawn, extra or duplicate data; hidden, covered, clipped or moved
content (transforms, visibility, paint order, viewport, positioning
attributes); degenerate or empty geometry; wrong, extra, re-signed or
re-unit-ed printed numbers; NOT_FOUND shown as a value and "not applicable"
confused with "not found"; broken length and datum closure; non-finite spec
or SVG numbers. Since #2158 it also closes: table cells that differ from
their spec field under the column's format, that name another field, or
that sit off their column; rows moved away from their component, rows
overlapping, leaders that do not join a row to its component; a nested
component that is neither an overlay of its host nor in the chain, and an
overlap in the chain without an explicit landing relation; flex-joint
pivots off their stated elevation; open conflicts and gaps missing from the
review list or from their row's warning mark (matched by ``component_id``);
printed values without a design-data reference, design-data IDs that do not
resolve, class flags that differ from the register, register values that
differ from the field they back, archive citations (calculation numbers in
any case, quoted or unquoted spreadsheet cell or range references,
workbook or report-table wording, provenance source strings) anywhere on
the drawing; ft axis ticks that are not round feet. After review r1 of PR
#2159: a register item that states no value for a field it backs leaves
that field ``not_established`` (its class flag is not a verified source);
a public item must cite a resolved ``source`` reference (context citations
do not count); reference URLs and retrieval dates are parsed, not pattern
matched (no URL is fetched: an id that resolves is not a reference that is
reachable); a review entry must target a row that exists; each review
entry's classification, whole text and warning-mark position are pinned.
It is not proof against a deliberately adversarial author: within the
allowed grammar and frozen stylesheet, content can still be made hard to
read without being wrong (e.g. geometry the checks do not pin, such as the
horizontal extent of decorative and symbol shapes, or a long review-list
line running past the sheet edge). The rendered pixels are not compared;
the checks read the SVG, not a raster of it.

Then it checks:

  (a) mapping   - every spec component drawn exactly once and after its
                  ``nested_in`` host; no orphan groups; group data-* equal
                  the spec; exactly one table row per component, plus the
                  drill floor and tensioner rows; each row has exactly the
                  cells its item needs; header and legend text frozen; the
                  review list has one entry per open conflict or gap and
                  each row's warning mark lists exactly its open entries
  (b) positions - the zone table is finite, positive, contiguous and matches
                  the drawn axis and break marks; each component's body
                  pieces equal the expected per-zone intervals within 0.5 px;
                  joint seams sit at T(top - i*L); datum lines, sheaves,
                  pivots and axis ticks sit at T(z); every required datum is
                  drawn; the header, rows and cells sit on the frozen column
                  edges; rows do not overlap, keep anchor order, stay within
                  a bounded offset of their anchor, and each leader runs from
                  the component body (datum line, sheave) to the row's left
                  edge at the row's centre
  (c) numbers   - every text holds only flat ``<tspan>`` children with no
                  direct or tail text; every number token lies wholly inside
                  one field tspan and equals its whole text; no stray digits;
                  a table cell's number carries its column's header unit, any
                  other m/ft/in field is followed by its own unit; each field
                  uses the precision fixed by its field type and equals the
                  spec value at that precision; every table cell equals its
                  spec field under the column format; verbatim text fields
                  equal the spec text
  (d) totals    - per component, count x joint length equals the elevation
                  span; lengths sum to the stack-up length; elevations are
                  continuous along the chain (a landing overlap is excused
                  only by an explicit ``nested_in``; overlays are outside the
                  chain); water depth and air gap equal the datum elevations;
                  nothing stands above the drill floor; the stack-up closes,
                  or its residual is flagged by an independent source. A
                  residual computed by an adapter from the same data is
                  reported ``not_established``.
  (e) NOT_FOUND - never printed or attributed as a number; shown as the
                  literal text "n/a"; "–" only where the field does not apply
  (f) design data - every printed spec value's field has a
                  ``design_data_id`` (``not_established`` when the spec has
                  no register at all); the IDs printed per row equal the IDs
                  of the row's printed fields, and inline IDs those of the
                  text's fields; every printed ID resolves and its class flag
                  equals the item's ``source_class``; every register value
                  agrees with the fields it backs within the formatting
                  tolerance; no archive citation text on the drawing; ft axis
                  ticks are round feet

Result: ``"fail"`` if any check fails; else ``"pass_with_open_items"`` if any
check is ``"not_established"``; else ``"pass"``.

Changing the renderer's emitted grammar, layout, stylesheet or definitions
is a deliberate act: update the frozen constants below (``canonical_digests``
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
    NOT_FOUND,
    SOURCE_CLASSES,
    StackupDrawingSpec,
    not_found_fields,
    open_review_entries,
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
LEN_TOL_M = 1e-3
DATUM_TOL_M = 1e-6
MAX_DECIMALS = 4
CHECKS = (
    "a_mapping",
    "b_positions",
    "c_numbers",
    "d_totals",
    "e_not_found",
    "f_design_data",
)
ALLOWED_ROLES = {
    "decor",
    "axis",
    "datum",
    "tensioner",
    "component",
    "break",
    "table-header",
    "table-row",
    "table-legend",
    "review-list",
    "titleblock",
}
DATUM_NAMES = ("drill_floor_el_m", "msl_el_m", "mudline_el_m")
#: A printed number: optional sign, comma-grouped integer part, fraction.
NUM_FULL = re.compile(r"([+\-−]?)(\d{1,3}(?:,\d{3})*)(?:\.(\d+))?", re.ASCII)
#: A number as it reads in the rendered string, with any sign before it.
RENDERED_NUM = re.compile(r"(?<![\d.,])([+\-−]\s*)?\d[\d,]*(?:\.\d+)?", re.ASCII)

# -- frozen layout (#2158) ----------------------------------------------------------------
# The table, review band and title block the renderer lays out. These are
# frozen here independently of the renderer; a deliberate layout change
# updates both, and the golden snapshot.

#: (column, header, header units, width px). Units live in the header only.
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
TABLE_X0 = 516.0
TABLE_X1 = TABLE_X0 + sum(c[3] for c in COLUMNS)
COL_X = {}
_x = TABLE_X0
for _key, _h1, _h2, _w in COLUMNS:
    COL_X[_key] = _x
    _x += _w
#: Sheet width: the table's right edge plus a margin.
SHEET_WIDTH = TABLE_X1 + 14.0
ROW_PITCH = 20.0
HEADER_HEIGHT = 32.0
#: Text baseline below a row's centre line.
CELL_BASELINE = 4.4
LEGEND_HEIGHT = 56.0
WARN_INSET = 16.0
#: Largest vertical distance between a row's centre and its anchor.
MAX_ROW_OFFSET_PX = 160.0
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
#: Unit a numeric cell's value carries (stated once, in the column header).
CELL_UNITS = {
    "qty": "count",
    "qty_len": "ft",
    "top": "m",
    "top_ft": "ft",
    "bot": "m",
    "od": "in",
    "wall": "in",
    "buoy": "in",
    "buoy_rating": "ft",
}
CELL_CLASS = {
    **{c: "tc" for c in CELL_POS},
    "qty_x": "tl",
    "od_w": "tl",
    "buoy_sep": "tl",
    "dd": "tdd",
}
LEGEND_TEXTS = (
    "Design data: D-ID = item in the report's Design data table; P = public source"
    " · D = owner decision · A = ASSUMED - to be confirmed (no public data).",
    "n/a applies but not found in the sources · – not applicable to the item type.",
    "Open data conflict or gap for the item: see the review list.",
)
REVIEW_GAP = 14.0
REVIEW_HEADER_BASELINE = 16.0
REVIEW_PITCH = 15.0
REVIEW_BOTTOM_PAD = 9.0
REVIEW_TEXT_X = 26.0
#: A review entry's warning mark: x from the band's left edge, y below the
#: baseline of the entry's first line (the triangle's lower-left corner).
REVIEW_WARN_X = 10.0
REVIEW_WARN_DY = 1.0
REVIEW_HEADER = "REVIEW LIST: OPEN DATA CONFLICTS AND GAPS"
TITLE_GAP = 10.0
TITLE_BLOCK_HEIGHT = 196.0
SHEET_BOTTOM_MARGIN = 12.0
DASH = "–"
NA = "n/a"
#: sha256 of the canonical <style> text and of the canonical <defs> tree.
STYLE_SHA256 = "45c5e4779eea74dea32f7a83afb2e38d19653963c8de27052ac9d73b715394f2"
DEFS_SHA256 = "5be3761097837b884410f947978afdec12be2ed91ebafa54fd3f9b362ed09c2f"

#: Archive citations that must never reach the drawing (owner instruction
#: 2026-09-24): calculation numbers, spreadsheet sheet!cell references,
#: spreadsheet files, workbook or report-table wording.
#: A spreadsheet cell or range: A1, $A$1, A1:C7, $B$4:$D$9.
_CELL = r"\$?[A-Za-z]{1,3}\$?\d+(?::\$?[A-Za-z]{1,3}\$?\d+)?"
ARCHIVE_PATTERNS = (
    (re.compile(r"\bCAL-\d", re.I), "calculation document number (CAL-<n>)"),
    # quoted ('Input Data'!B12) or unquoted (Data!$A$1) sheet reference; a
    # cell must follow the "!", so "Confirm!" or "Note!" is punctuation
    (
        re.compile(rf"(?:'[^'\n]+'|\b[A-Za-z_][\w.]*)!{_CELL}(?![\w$])"),
        "spreadsheet cell reference (<sheet>!<cell>)",
    ),
    (re.compile(r"\.xls[xmb]?\b", re.I), "spreadsheet file name"),
    (re.compile(r"\bworkbook\b", re.I), "workbook citation"),
    (re.compile(r"\breport table\b", re.I), "report-table citation"),
)
#: Provenance source strings at least this long must not be printed verbatim.
MIN_SOURCE_LEN = 8


# -- frozen SVG grammar -----------------------------------------------------------
# The closed set of elements and attributes the renderer emits. Anything else
# is rejected. These are grammar, not geometry.

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
    ("circle", "g"): frozenset(
        {"class", "cx", "cy", "data-part", "data-pivot-el-m", "r"}
    ),
    ("path", "g"): frozenset({"class", "d", "data-part", "data-review"}),
    ("polyline", "g"): frozenset({"class", "points"}),
    ("text", "g"): frozenset(
        {"class", "data-col", "data-review", "text-anchor", "x", "y"}
    ),
    ("tspan", "text"): frozenset(
        {
            "class",
            "data-dd",
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
            "data-nested-in",
            "data-clipped",
            "data-not-drawn",
        }
    ),
    "table-row": frozenset(
        {"data-role", "data-component-id", "data-datum", "data-row"}
    ),
    "datum": frozenset({"data-role", "data-datum", "data-el-m"}),
    "tensioner": frozenset({"data-role", "data-sheave-el-m", "data-count"}),
    "axis": frozenset({"data-role"}),
    "decor": frozenset({"data-role"}),
    "break": frozenset({"data-role"}),
    "table-header": frozenset({"data-role"}),
    "table-legend": frozenset({"data-role"}),
    "review-list": frozenset({"data-role"}),
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
#: Review keys: ``gaps[<i>]`` / ``data_conflicts[<i>]``, space separated.
_REVIEW_KEY = r"(?:gaps|data_conflicts)\[\d+\]"
_REVIEW_RE = re.compile(rf"{_REVIEW_KEY}(?: {_REVIEW_KEY})*")
#: Frozen attribute values (outside <defs>).
_VALUES: dict[str, dict[str, frozenset[str]]] = {
    "svg": {"role": frozenset({"img"})},
    "g": {
        "data-clipped": frozenset({"bottom"}),
        "data-row": frozenset({"tensioner_system"}),
    },
    "rect": {
        "data-part": frozenset({"body", "seam", "symbol"}),
        "rx": frozenset({"2"}),
    },
    "line": {
        "data-part": frozenset({"body", "datum-line", "seam"}),
        "data-unit": frozenset({"m", "ft"}),
    },
    "circle": {"data-part": frozenset({"sheave", "symbol", "pivot"})},
    "path": {"data-part": frozenset({"symbol", "warn"})},
    "text": {
        "text-anchor": frozenset({"start", "end", "middle"}),
        "data-col": frozenset(CELL_POS),
    },
    "tspan": {
        "class": frozenset({"na", "nap", "ddc-p", "ddc-d", "ddc-a"}),
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
    frozenset({"data-dd"}): "dd-id",
}
#: Marker tspans: class -> the only text it may show.
_MARKERS = {"na": NA, "nap": DASH, "ddc-p": "P", "ddc-d": "D", "ddc-a": "A"}
_DD_ID = re.compile(r"D-\d{2,3}")
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

# -- required attribute profiles -------------------------------------------------
_L = frozenset({"class", "x1", "x2", "y1", "y2"})
_R = frozenset({"class", "x", "y", "width", "height"})
_T = frozenset({"class", "x", "y"})
_C = frozenset({"class", "cx", "cy", "r"})
_PL = frozenset({"class", "points"})
_P = frozenset({"data-part"})
_NONE: frozenset[str] = frozenset()


def _classes(names: str, required, optional=_NONE) -> dict:
    return {n: (frozenset(required), frozenset(optional)) for n in names.split()}


_COMPONENT_RECTS = (
    "buoy-a buoy-b buoy-c pipe pipe-dark chrome body-thin frame housing cond "
    "cond-b hidden flange ring post connector pod ram"
)
_WARN = frozenset({"class", "d"})
#: (group role, element) -> {class: (required attributes, optional attributes)}.
_PROFILES: dict[tuple[str, str], dict[str, tuple[frozenset, frozenset]]] = {
    ("decor", "rect"): _classes("bg water soil rig-floor rotary", _R),
    ("decor", "polyline"): _classes("wave", _PL),
    ("decor", "line"): _classes("rig centre", _L),
    ("axis", "line"): {
        **_classes("axis break", _L),
        **_classes("tick", _L | {"data-tick-el-m", "data-unit"}),
    },
    ("axis", "text"): {
        **_classes("axis-hdr", _T, {"text-anchor"}),
        **_classes("tick-lbl", _T | {"text-anchor"}),
    },
    ("datum", "line"): _classes("datum datum-msl datum-ml", _L | _P),
    ("datum", "path"): _classes("datum", {"class", "d"}),
    ("datum", "text"): _classes("dlbl dlbl2", _T),
    ("tensioner", "line"): _classes("rig wire", _L),
    ("tensioner", "rect"): _classes("cyl", _R | _P),
    ("tensioner", "circle"): {
        **_classes("sheave", _C | _P),
        **_classes("dot", _C),
    },
    ("component", "rect"): {
        **_classes(_COMPONENT_RECTS, _R | _P),
        **_classes("bonnet", _R | _P | {"rx"}),
    },
    ("component", "line"): _classes("seam cut", _L | _P),
    ("component", "circle"): {
        **_classes("ball", _C | _P, {"data-pivot-el-m"}),
        **_classes("dot", _C | _P),
    },
    ("component", "path"): _classes("annular valve hidden", {"class", "d"} | _P),
    ("break", "polyline"): _classes("break cut", _PL),
    ("break", "text"): _classes("note-l", _T),
    ("table-header", "rect"): _classes("hdr-bg", _R),
    ("table-header", "line"): _classes("grid", _L),
    ("table-header", "text"): _classes("th thu", _T | {"data-col"}, {"text-anchor"}),
    ("table-row", "rect"): _classes("row-a row-b", _R),
    ("table-row", "line"): _classes("row-rule", _L),
    ("table-row", "polyline"): _classes("leader", _PL),
    ("table-row", "circle"): _classes("dot", _C),
    ("table-row", "text"): _classes("tc tl tdd", _T | {"text-anchor", "data-col"}),
    ("table-row", "path"): _classes("warn", _WARN | _P | {"data-review"}),
    ("table-legend", "text"): _classes("tl", _T),
    ("table-legend", "path"): _classes("warn", _WARN),
    ("review-list", "rect"): _classes("rl-bg", _R),
    ("review-list", "text"): {
        **_classes("rl-hdr rl-none", _T),
        **_classes("rl", _T | {"data-review"}),
    },
    ("review-list", "path"): _classes("warn", _WARN | _P | {"data-review"}),
    ("titleblock", "rect"): _classes("tb", _R),
    ("titleblock", "line"): _classes("tb-line", _L),
    ("titleblock", "text"): _classes("tb-title tb-sub tb-hdr tb-val tb-note", _T),
}
#: data-part value required per (role, element, class), where the renderer fixes it.
_PART_VALUES = {
    **{
        ("datum", "line", c): frozenset({"datum-line"})
        for c in ("datum", "datum-msl", "datum-ml")
    },
    ("tensioner", "circle", "sheave"): frozenset({"sheave"}),
    ("tensioner", "rect", "cyl"): frozenset({"symbol"}),
    ("component", "circle", "ball"): frozenset({"symbol", "pivot"}),
    ("component", "circle", "dot"): frozenset({"symbol"}),
    ("table-row", "path", "warn"): frozenset({"warn"}),
    ("review-list", "path", "warn"): frozenset({"warn"}),
}

# -- layer (paint) order ------------------------------------------------------------
#: Top-level children in paint order, as tokens: element name, or g:<role>
#: (g:transform for the elevation-transform group).
_LAYER_ORDER = re.compile(
    r"title desc style defs g:decor g:transform g:decor (?:g:datum ){1,3}g:axis "
    r"(?:g:decor )?(?:g:tensioner )?(?:g:component )*g:break "
    r"g:table-header (?:g:table-row )*g:table-legend g:review-list g:titleblock "
)

# -- numbers and paths ----------------------------------------------------------------
_PLAIN = r"[+-]?(?:[0-9]+(?:\.[0-9]+)?|\.[0-9]+)"  # ASCII digits only
_PLAIN_RE = re.compile(_PLAIN)
_PAIR = rf"{_PLAIN},{_PLAIN}"
_POINTS_RE = re.compile(rf"{_PAIR}(?: {_PAIR})*")
_TRIANGLE = re.compile(rf"M{_PAIR} l{_PAIR} l{_PAIR} z")
#: Full ``d`` grammar per (role, class).
_PATH_GRAMMAR = {
    ("datum", "datum"): _TRIANGLE,
    ("component", "annular"): re.compile(
        rf"M{_PAIR} v{_PLAIN} q{_PAIR} {_PAIR} h{_PLAIN} q{_PAIR} {_PAIR} v{_PLAIN} z"
    ),
    ("component", "valve"): re.compile(rf"M{_PAIR} L{_PAIR} L{_PAIR} L{_PAIR} z"),
    ("component", "hidden"): re.compile(rf"M{_PAIR} l{_PAIR} l{_PAIR}"),
    ("table-row", "warn"): _TRIANGLE,
    ("table-legend", "warn"): _TRIANGLE,
    ("review-list", "warn"): _TRIANGLE,
}
#: Symbol extents the renderer fixes exactly: (width, height) in px.
_PATH_EXTENT = {
    ("datum", "datum"): (12.0, 9.0),
    ("component", "valve"): (12.0, 8.0),
    ("table-row", "warn"): (11.0, 10.0),
    ("table-legend", "warn"): (11.0, 10.0),
    ("review-list", "warn"): (11.0, 10.0),
}
#: Smallest extent, in px, of any other symbol path, line or polyline.
MIN_EXTENT_PX = 0.5
#: Circle radii the renderer fixes, per (role, class).
_RADII = {
    ("table-row", "dot"): frozenset({1.8}),
    ("tensioner", "dot"): frozenset({2.0}),
    ("tensioner", "sheave"): frozenset({7.0}),
    ("component", "dot"): frozenset({2.2}),
    ("component", "ball"): frozenset({9.0}),
}
#: Geometric attributes that must be plain numbers wherever they appear.
_GEOM_ATTRS = ("x", "y", "width", "height", "r", "cx", "cy", "x1", "y1", "x2", "y2")
#: Horizontal sheave offset (px) when the spec gives no sheave radius.
DEFAULT_SHEAVE_OFFSET_PX = 110.0
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
        if ptag == "g":
            out.extend(_profile_problems(el, par.get("data-role"), tag))
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


def _profile_problems(el: ET.Element, role: Optional[str], tag: str) -> list:
    """Required attributes, role-specific class, plain numbers, path grammar."""
    out: list[tuple[str, str]] = []
    where = f"<{tag}> in <g data-role={role}>"
    classes = _PROFILES.get((role, tag))
    if classes is None:
        return [("a_mapping", f"grammar: {where} is not an element this role draws")]
    cls = el.get("class")
    if cls is None:
        return [("a_mapping", f"grammar: {where} lacks required attribute 'class'")]
    if cls not in classes:
        return [
            (
                "a_mapping",
                f"grammar: class '{cls}' not allowed on <{tag}> in <g data-role={role}>",
            )
        ]
    required, optional = classes[cls]
    for a in sorted(required - set(el.attrib)):
        out.append(
            (
                "a_mapping",
                f"grammar: {where} class '{cls}' lacks required attribute '{a}'",
            )
        )
    for a in sorted(set(el.attrib) - required - optional):
        check = "b_positions" if _local(a) in _POSITIONAL else "a_mapping"
        out.append(
            (
                check,
                f"grammar: attribute '{_local(a)}' not allowed on <{tag} class={cls}>",
            )
        )
    part = _PART_VALUES.get((role, tag, cls))
    if part is not None and el.get("data-part") not in part:
        out.append(
            (
                "a_mapping",
                f"grammar: {where} data-part={el.get('data-part')!r} not allowed",
            )
        )
    for a in _GEOM_ATTRS:
        v = el.get(a)
        if v is not None and not _PLAIN_RE.fullmatch(v):
            out.append(
                (
                    "b_positions",
                    f"numeric: <{tag}> {a}={v!r} is not a plain finite SVG number",
                )
            )
    if tag == "polyline" and not _POINTS_RE.fullmatch(el.get("points") or ""):
        out.append(
            (
                "b_positions",
                f"numeric: <polyline> points={el.get('points')!r} is not a plain finite SVG number list",
            )
        )
    if tag == "path":
        grammar = _PATH_GRAMMAR.get((role, cls))
        d = el.get("d") or ""
        if grammar is None or not grammar.fullmatch(d):
            out.append(
                (
                    "b_positions",
                    f"path d {d!r} does not match the <{cls}> symbol grammar",
                )
            )
    return out


def _path_points(d: str) -> list[tuple[float, float]]:
    """Every vertex and control point of a grammar-checked path."""
    tokens = re.findall(rf"[MLlvhqz]|{_PLAIN}", d)
    pts: list[tuple[float, float]] = []
    x = y = 0.0
    i = 0
    cmd = ""
    while i < len(tokens):
        tok = tokens[i]
        if tok in "MLlvhqz":
            cmd = tok
            i += 1
            if cmd == "z":
                continue
        nums = []
        take = {"M": 2, "L": 2, "l": 2, "v": 1, "h": 1, "q": 4}[cmd]
        for _ in range(take):
            nums.append(float(tokens[i]))
            i += 1
        if cmd in "ML":
            x, y = nums
        elif cmd == "l":
            x, y = x + nums[0], y + nums[1]
        elif cmd == "v":
            y += nums[0]
        elif cmd == "h":
            x += nums[0]
        elif cmd == "q":
            pts.append((x + nums[0], y + nums[1]))
            x, y = x + nums[2], y + nums[3]
        pts.append((x, y))
    return pts


def _plain(v: Optional[str]) -> float:
    """Float of a plain SVG number; ValueError for anything else (NaN, 1e3)."""
    if v is None or not _PLAIN_RE.fullmatch(v):
        raise ValueError(v)
    return float(v)


def _points(el: ET.Element) -> Optional[list[tuple[float, float]]]:
    """Extreme points of a drawn element, or None when unreadable."""
    tag = _local(el.tag)

    def f(name: str) -> float:
        return _plain(el.get(name))

    try:
        if tag == "rect":
            x, y, w, h = f("x"), f("y"), f("width"), f("height")
            pts = [(x, y), (x + w, y + h)]
        elif tag == "line":
            pts = [(f("x1"), f("y1")), (f("x2"), f("y2"))]
        elif tag == "circle":
            cx, cy, r = f("cx"), f("cy"), f("r")
            pts = [(cx - r, cy - r), (cx + r, cy + r)]
        elif tag == "polyline":
            points = el.get("points") or ""
            if not _POINTS_RE.fullmatch(points):
                return None
            pts = [tuple(float(v) for v in p.split(",")) for p in points.split()]
        elif tag == "path":
            d = el.get("d") or ""
            if not any(g.fullmatch(d) for g in _PATH_GRAMMAR.values()):
                return None
            pts = _path_points(d)
        elif tag == "text":
            pts = [(f("x"), f("y"))]
        else:
            return []
    except (TypeError, ValueError):
        return None
    if not pts or any(len(p) != 2 or not all(map(math.isfinite, p)) for p in pts):
        return None
    return pts


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


def _path(d: Any, path: str) -> Any:
    """Value at a dotted path; digit parts index lists (``gaps.0.detail``)."""
    for part in path.split("."):
        if isinstance(d, dict):
            if part not in d:
                raise KeyError(path)
            d = d[part]
        elif isinstance(d, list) and part.isdigit() and int(part) < len(d):
            d = d[int(part)]
        else:
            raise KeyError(path)
    return d


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
    if profile == "marker" and _MARKERS.get(sp.get("class")) != text:
        return [
            (
                "c_numbers",
                f"grammar: marker tspan class={sp.get('class')!r} shows {text!r}",
            )
        ]
    if profile == "na-field" and (sp.get("class") != "na" or text != NA):
        return [("c_numbers", "grammar: field tspan with a class other than na")]
    if profile == "dd-id" and not (
        _DD_ID.fullmatch(text) and sp.get("data-dd") == text
    ):
        return [
            (
                "c_numbers",
                f"grammar: design-data tspan shows {text!r} for {sp.get('data-dd')!r}",
            )
        ]
    if profile == "tick":
        t = parent.get(sp)
        g = parent.get(t) if t is not None else None
        if g is None or g.get("data-role") != "axis" or t.get("class") != "tick-lbl":
            return [("c_numbers", "grammar: tick metadata outside the axis group")]
    return []


def _viewport_problems(root: ET.Element, top_groups, w: float, h: float) -> list[str]:
    """Root viewport pinned to the layout; every drawn element inside it."""
    out = []
    raw = root.get("viewBox") or ""
    parts = raw.split(" ")
    if len(parts) != 4 or not all(_PLAIN_RE.fullmatch(p) for p in parts):
        out.append(f"root viewBox {raw!r} is not four plain finite SVG numbers")
    elif any(abs(float(a) - b) > 0.01 for a, b in zip(parts, (0.0, 0.0, w, h))):
        out.append(f"root viewBox {raw!r} != '0 0 {w:g} {h:.2f}'")
    for name, exp in (("width", w), ("height", h)):
        try:
            v = _plain(root.get(name))
        except ValueError:
            out.append(
                f"root {name} {root.get(name)!r} is not a plain finite SVG number"
            )
            continue
        if abs(v - exp) > 0.01:
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


def _layer_problems(root: ET.Element, top_groups, w: float, h: float) -> list[str]:
    """Paint order frozen; the background is first and covers the sheet."""
    out = []
    tokens = []
    for c in root:
        tag = _local(c.tag)
        if tag != "g":
            tokens.append(tag)
        elif c.get("id") == "elevation-transform":
            tokens.append("g:transform")
        else:
            tokens.append(f"g:{c.get('data-role')}")
    if not _LAYER_ORDER.fullmatch(" ".join(tokens) + " "):
        out.append(
            "layer order: top-level paint order "
            f"{' '.join(t for i, t in enumerate(tokens) if i == 0 or t != tokens[i - 1])!r}"
            " is not the renderer's"
        )
    decor = [g for g in top_groups if g.get("data-role") == "decor"]
    if decor:
        first = list(decor[0])
        ok = (
            len(first) == 1
            and _local(first[0].tag) == "rect"
            and first[0].get("class") == "bg"
            and _points(first[0]) is not None
            and all(
                abs(a - b) <= 0.01
                for a, b in zip(
                    (_fnan(first[0], k) for k in ("x", "y", "width", "height")),
                    (0.0, 0.0, w, h),
                )
            )
        )
        if not ok:
            out.append(
                "layer order: the first decor group is not the full-sheet background"
            )
        for g in decor[1:]:
            if any(el.get("class") == "bg" for el in g):
                out.append(
                    "layer order: a background rect outside the first decor group"
                )
    return out


def _geometry_problems(top_groups, y_bottom: float, tf: ET.Element, spec) -> list[str]:
    """Positive, finite, frozen-size geometry; bands below the drawing."""
    out = []
    for g in top_groups:
        role = g.get("data-role")
        for el in g:
            tag, cls = _local(el.tag), el.get("class")
            where = f"<{tag} class={cls}> in <g data-role={role}>"
            pts = _points(el)
            if pts is None:
                continue  # reported as unreadable by the viewport check
            if tag == "rect":
                if not (_fnan(el, "width") > 0 and _fnan(el, "height") > 0):
                    out.append(f"{where}: width/height not positive")
            elif tag == "circle":
                r = _fnan(el, "r")
                radii = _RADII.get((role, cls))
                if not r > 0:
                    out.append(f"{where}: radius {el.get('r')!r} not positive")
                elif radii is not None and not any(abs(r - v) <= 1e-9 for v in radii):
                    out.append(
                        f"{where}: radius {r:g} is not the symbol's {sorted(radii)}"
                    )
            elif tag in ("line", "polyline"):
                length = sum(math.dist(a, b) for a, b in zip(pts, pts[1:]))
                if len(pts) < 2 or not length >= MIN_EXTENT_PX:
                    out.append(f"{where}: length {length:.3f} px below {MIN_EXTENT_PX}")
            elif tag == "path":
                xs, ys = [p[0] for p in pts], [p[1] for p in pts]
                ext = (max(xs) - min(xs), max(ys) - min(ys))
                fixed = _PATH_EXTENT.get((role, cls))
                if fixed is not None and any(
                    abs(a - b) > 0.02 for a, b in zip(ext, fixed)
                ):
                    out.append(
                        f"{where}: path d extent {ext} is not the symbol's {fixed}"
                    )
                elif fixed is None and not min(ext) >= MIN_EXTENT_PX:
                    out.append(f"{where}: path d extent {ext} below {MIN_EXTENT_PX} px")
            if (
                role in ("titleblock", "review-list")
                and min(p[1] for p in pts) < y_bottom - 1e-6
            ):
                name = "title block" if role == "titleblock" else "review list"
                out.append(f"{name}: {where} reaches above the drawing band")
    # sheaves: frozen symbol radius (above) at the spec's sheave radius
    ts = spec.tensioner_system
    cx0, px_per_in = _float_attr(tf, "data-center-x"), _float_attr(tf, "data-px-per-in")
    sheaves = [
        el
        for g in top_groups
        if g.get("data-role") == "tensioner"
        for el in g
        if el.get("class") == "sheave"
    ]
    if sheaves and ts is not None and cx0 is not None and px_per_in is not None:
        off = (
            float(ts.sheave_radius_m) / 0.0254 * px_per_in
            if _known(ts.sheave_radius_m)
            else DEFAULT_SHEAVE_OFFSET_PX
        )
        xs = sorted(_fnan(el, "cx") for el in sheaves)
        if len(xs) != 2 or not (
            abs(xs[0] - (cx0 - off)) <= PX_TOL and abs(xs[1] - (cx0 + off)) <= PX_TOL
        ):
            out.append(
                f"sheave radius: sheaves at x={xs} are not at the centre "
                f"{cx0:g} +- {off:.2f} px the spec sheave radius implies"
            )
    return out


def _rendered_text_problems(t: ET.Element, ctx: str, col: Optional[str]) -> list[str]:
    """Numbers, signs and units as the whole <text> reads, across tspans.

    ``col`` is the table column of a cell (its unit is in the header).
    """
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
        if kind(spans[i]) in ("text-field", "dd-id"):
            # verbatim spec text or a design-data ID, checked for exact
            # equality elsewhere; its digits are not a number
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
    if col in CELL_UNITS:
        # a numeric cell: its unit is the column header's
        for sp in spans:
            if kind(sp) == "number" and sp.get("data-unit") != CELL_UNITS[col]:
                out.append(
                    f"{ctx}: {sp.get('data-field')} in column {col} carries unit "
                    f"{sp.get('data-unit')!r}, the header states {CELL_UNITS[col]!r}"
                )
        return out
    # every m/ft/in field is followed by its own unit, or by a joiner and
    # another field of the same unit ("EL a to b m", "OD x WT in")
    for i, sp in enumerate(spans):
        unit = sp.get("data-unit")
        if kind(sp) != "number" or unit not in ("m", "ft", "in"):
            continue
        j = i + 1
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


def _row_key(g: ET.Element) -> Optional[str]:
    """Row key: component id, ``datums`` (drill floor) or ``tensioner_system``."""
    if g.get("data-component-id") is not None:
        return g.get("data-component-id")
    if g.get("data-datum") == "drill_floor_el_m":
        return "datums"
    if g.get("data-row") == "tensioner_system":
        return "tensioner_system"
    return None


def _fmt(v: float, d: int, signed: bool = False) -> str:
    """Independent restatement of the drawing's number format."""
    s = f"{abs(v):,.{d}f}"
    if float(s.replace(",", "")) == 0:
        return s
    if v < 0:
        return "−" + s
    return ("+" + s) if signed else s


def _cell(v: Any, fld: str, dec: Optional[int], signed: bool = False, factor=1.0):
    """(text, field) of a value cell under its column format; field None = "–"."""
    if v is None:
        return (DASH, None)
    if v == NOT_FOUND:
        return (NA, fld)
    x = float(v) * factor
    return (_fmt(x, _min_decimals(x) if dec is None else dec, signed), fld)


def _ft_cell(v: Any, fld: str):
    if v is None:
        return (DASH, None)
    if v == NOT_FOUND:
        return (NA, fld)
    return ("(" + _fmt(float(v) * M2FT, 1, True) + ")", fld)


def _expected_cells(spec: StackupDrawingSpec, key: str) -> dict[str, tuple]:
    """Every cell of a row except the design-data cell: col -> (text, field)."""
    dash = (DASH, None)
    if key == "datums":
        v, f = spec.datums.drill_floor_el_m, "datums.drill_floor_el_m"
        return {
            "component": ("Drill floor (RKB)", None),
            "qty": dash,
            "top": _cell(v, f, 2, True),
            "top_ft": _ft_cell(v, f),
            "bot": dash,
            "od": dash,
            "wall": dash,
            "buoy": dash,
        }
    if key == "tensioner_system":
        ts = spec.tensioner_system
        f = "tensioner_system.sheave_radius_m"
        r_text, r_fld = _cell(ts.sheave_radius_m, f, 2)
        label = (
            "Tensioners, sheave R "
            + r_text
            + (" m" if _known(ts.sheave_radius_m) else "")
        )
        return {
            "component": (label, r_fld),
            "qty": _cell(ts.count, "tensioner_system.count", 0),
            "qty_len": dash,
            "top": _cell(ts.sheave_el_m, "tensioner_system.sheave_el_m", 2, True),
            "top_ft": _ft_cell(ts.sheave_el_m, "tensioner_system.sheave_el_m"),
            "bot": dash,
            "od": dash,
            "wall": dash,
            "buoy": dash,
        }
    c = spec.component(key)
    out = {
        "component": (c.label[:1].upper() + c.label[1:], None),
        "qty": _cell(c.count, "count", 0),
    }
    if c.joint_length_m is None:
        out["qty_len"] = dash
    else:
        dec = 1
        if _known(c.joint_length_m):
            lft = float(c.joint_length_m) * M2FT
            dec = 0 if abs(lft - round(lft)) < 0.05 else 1
        out["qty_x"] = ("×", None)
        out["qty_len"] = _cell(c.joint_length_m, "joint_length_m", dec, factor=M2FT)
    out["top"] = _cell(c.top_el_m, "top_el_m", 2, True)
    out["top_ft"] = _ft_cell(c.top_el_m, "top_el_m")
    out["bot"] = _cell(c.bottom_el_m, "bottom_el_m", 2, True)
    if c.od_in is not None:
        out["od"] = _cell(c.od_in, "od_in", None)
    elif c.envelope_width_in is not None:
        out["od_w"] = ("W", None)
        out["od"] = _cell(c.envelope_width_in, "envelope_width_in", None)
    else:
        out["od"] = dash
    out["wall"] = _cell(c.wall_thickness_in, "wall_thickness_in", 3)
    if c.buoyancy_od_in is None and c.buoyancy_depth_rating_ft is None:
        out["buoy"] = dash
    else:
        out["buoy"] = _cell(c.buoyancy_od_in, "buoyancy_od_in", None)
        out["buoy_sep"] = ("/", None)
        out["buoy_rating"] = _cell(
            c.buoyancy_depth_rating_ft, "buoyancy_depth_rating_ft", 0
        )
    return out


def _prov_for(spec: StackupDrawingSpec, row_key: Optional[str], fld: str):
    """Provenance entry (or ReferenceValue) behind a printed field, or None."""
    if "." not in fld:
        if row_key is None:
            return None
        try:
            return spec.component(row_key).provenance.get(fld)
        except KeyError:
            return None
    head, rest = fld.split(".", 1)
    if head == "datums":
        return spec.datums.provenance.get(rest)
    if head == "tensioner_system" and spec.tensioner_system is not None:
        return spec.tensioner_system.provenance.get(rest)
    if head == "reference_totals":
        return spec.reference_totals.get(rest.rsplit(".", 1)[0])
    return None


def _value_fields(t: ET.Element) -> list[str]:
    """Printed spec values of a text that must reference design data."""
    out = []
    for sp in t.iter(NS + "tspan"):
        fld = sp.get("data-field")
        if (
            fld is None
            or sp.get("data-kind") == "text"
            or (sp.text or "") == NA
            or fld.startswith("derived.")
            or fld == "datums.msl_el_m"  # the zero of the elevation axis
        ):
            continue
        out.append(fld)
    return out


def _dd_printed(t: ET.Element) -> list[tuple[str, Optional[str]]]:
    """(ID, class flag shown after it) for each design-data tspan of a text."""
    spans = list(t.iter(NS + "tspan"))
    out = []
    for i, sp in enumerate(spans):
        if sp.get("data-dd") is None:
            continue
        nxt = spans[i + 1] if i + 1 < len(spans) else None
        flag = None
        if nxt is not None and (nxt.get("class") or "").startswith("ddc-"):
            flag = nxt.text
        out.append((sp.get("data-dd"), flag))
    return out


_NATIVE = {"m": 1.0, "ft": 0.3048, "in": 0.0254}


def _native(key: str) -> tuple[str, float]:
    """(unit, formatting tolerance) of a spec field by name."""
    if key == "count":
        return "count", 0.5
    if key == "joint_length_m":
        return "m", 0.05 * 0.3048  # printed to 0.1 ft
    if key.endswith("_ft"):
        return "ft", 0.5
    if key.endswith("_in"):
        return "in", 0.0005
    return "m", 0.005


def _design_value_problems(spec: StackupDrawingSpec) -> tuple[list[str], list[str]]:
    """Register values against every field they back, in the field's unit.

    Returns ``(failures, open items)``. Each backed field is compared with
    the item's per-field ``values[path]`` when given, else with the item's
    scalar ``value``. A field whose item states neither cannot be verified:
    it is an open item, so the item's class flag is not taken as a checked
    source (review r1 finding 1).
    """
    out: list[str] = []
    opens: list[str] = []
    entries = []
    for key, p in spec.datums.provenance.items():
        entries.append((f"datums.{key}", key, getattr(spec.datums, key, None), p))
    if spec.tensioner_system is not None:
        ts = spec.tensioner_system
        for key, p in ts.provenance.items():
            entries.append((f"tensioner_system.{key}", key, getattr(ts, key, None), p))
    for c in spec.components:
        for key, p in c.provenance.items():
            entries.append((f"components.{c.id}.{key}", key, getattr(c, key, None), p))
    for key, rv in spec.reference_totals.items():
        entries.append(
            (f"reference_totals.{key}", key.removesuffix("_workbook"), rv.value, rv)
        )
    for path, key, value, p in entries:
        item = spec.design_item(getattr(p, "design_data_id", None))
        if item is None or not _known(value):
            continue
        if isinstance(value, bool) or not isinstance(value, (int, float)):
            continue
        stated = (item.values or {}).get(path)
        if isinstance(stated, dict) and "value" in stated:
            reg_value, reg_unit = stated.get("value"), stated.get("unit")
        elif item.value is not None:
            reg_value, reg_unit = item.value, item.unit
        else:
            opens.append(
                f"{path}: {item.id} states no verifiable value for this field "
                "(no values entry and no scalar value); integrity not established"
            )
            continue
        unit, tol = _native(key)
        if reg_unit == unit == "count":
            got = float(reg_value)
        elif reg_unit in _NATIVE and unit in _NATIVE:
            got = float(reg_value) * _NATIVE[reg_unit] / _NATIVE[unit]
        else:
            out.append(
                f"{item.id}: register unit {reg_unit!r} cannot state {path} ({unit})"
            )
            continue
        if abs(got - float(value)) > tol + 1e-12:
            out.append(
                f"{item.id}: register value {reg_value:g} {reg_unit} disagrees with "
                f"{path} = {float(value):.6g} {unit} (tolerance {tol:g} {unit})"
            )
    return out, opens


def _archive_problems(spec: StackupDrawingSpec, root: ET.Element) -> list[str]:
    """Archive citations and provenance source strings anywhere on the drawing."""
    texts = [("aria-label", root.get("aria-label") or "")]
    for el in root.iter():
        tag = _local(el.tag)
        if tag in ("title", "desc"):
            texts.append((tag, el.text or ""))
        elif tag == "text":
            texts.append(("text", "".join(el.itertext())))
    sources = set()
    for _, p in spec._provenance_entries():
        if len(str(p.source)) >= MIN_SOURCE_LEN:
            sources.add(str(p.source))
    for c in spec.components:
        if len(str(c.source)) >= MIN_SOURCE_LEN and c.source != NOT_FOUND:
            sources.add(str(c.source))
    if len(spec.title_block.document_ref) >= 4:
        sources.add(spec.title_block.document_ref)
    out = []
    for where, s in texts:
        for rx, what in ARCHIVE_PATTERNS:
            if rx.search(s):
                out.append(f"archive citation ({what}) on the drawing: {s!r}")
        for src in sources:
            if src in s:
                out.append(f"archive citation (provenance source {src!r}) in {where}")
    return out


def reconcile(spec: StackupDrawingSpec, svg_text: str) -> dict[str, Any]:
    """Check ``svg_text`` against ``spec``.

    ``result`` is ``"pass"``, ``"pass_with_open_items"`` or ``"fail"`` (see
    the module docstring). The report carries one entry per check
    (``a_mapping`` .. ``f_design_data``) with ``status`` (``pass``,
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
    try:
        root = ET.fromstring(svg_text)
    except ET.ParseError as exc:
        res["a_mapping"]["status"] = "fail"
        res["a_mapping"]["failures"].append(f"SVG is not well-formed XML: {exc}")
        return _finish(spec, res)
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
    for el in root.iter():
        v = el.get("data-review")
        if v is not None and not _REVIEW_RE.fullmatch(v):
            fail("a_mapping", f"grammar: data-review={v!r} is not a review key list")
    # digits must be ASCII: Python parses other Unicode decimal digits (e.g. full-width)
    # as numbers, which would let a non-canonical rendering pass the number checks.
    for t in root.iter(NS + "text"):
        rendered = "".join(t.itertext())
        odd = sorted({ch for ch in rendered if ch.isdecimal() and not ch.isascii()})
        if odd:
            fail("c_numbers", f"non-ASCII digit {''.join(odd)!r} in {rendered!r}")
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
    by_role: dict[str, list] = {}
    for g in top_groups:
        by_role.setdefault(g.get("data-role"), []).append(g)

    # sheet: width from the table columns, height from the bands --------------------
    band = by_role.get("review-list", [])
    band_lines = [t for g in band for t in g if t.get("class") in ("rl", "rl-none")]
    band_h = REVIEW_HEADER_BASELINE + len(band_lines) * REVIEW_PITCH + REVIEW_BOTTOM_PAD
    y_review = y_bottom + REVIEW_GAP
    y_title = y_review + band_h + TITLE_GAP
    W, H = SHEET_WIDTH, y_title + TITLE_BLOCK_HEIGHT + SHEET_BOTTOM_MARGIN
    for msg in _viewport_problems(root, top_groups, W, H):
        fail("b_positions", msg)
    for msg in _layer_problems(root, top_groups, W, H):
        fail("a_mapping", msg)
    for msg in _geometry_problems(top_groups, y_bottom, tf, spec):
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
    comp_groups: dict[str, list] = {}
    row_groups: dict[str, list] = {}
    for g in top_groups:
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
        if r == "table-row":
            key = _row_key(g)
            if key is None or (
                key not in comps and key not in ("datums", "tensioner_system")
            ):
                fail("a_mapping", f"table row for unknown item {key!r}")
            else:
                row_groups.setdefault(key, []).append(g)
        if r == "decor":
            for t in g.iter(NS + "text"):
                if re.search(r"\d", "".join(t.itertext())):
                    fail("a_mapping", "decor element carries a number")
    order = [g.get("data-component-id") for g in by_role.get("component", [])]
    for cid, c in comps.items():
        gl = comp_groups.get(cid, [])
        if len(gl) != 1:
            fail("a_mapping", f"{cid}: drawn {len(gl)} times (expected exactly once)")
            continue
        g = gl[0]
        expect = {
            "data-type": c.type.value,
            "data-count": str(c.count),
            "data-od-in": "" if c.od_in is None else str(c.od_in),
            "data-nested-in": c.nested_in,
        }
        for k, v in expect.items():
            if g.get(k) != v:
                fail("a_mapping", f"{cid}: {k}={g.get(k)!r} != spec {v!r}")
        if c.nested_in in order and order.index(cid) < order.index(c.nested_in):
            fail("a_mapping", f"{cid}: drawn before its nested_in host {c.nested_in}")
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
    # rows: one per component, drill floor and tensioner system
    row_keys = list(comps)
    if _known(d_.drill_floor_el_m):
        row_keys.append("datums")
    if spec.tensioner_system is not None:
        row_keys.append("tensioner_system")
    for key in row_keys:
        n = len(row_groups.get(key, []))
        if n != 1:
            fail("a_mapping", f"{key}: {n} table rows (expected exactly one)")
    for key in row_groups:
        if key not in row_keys:
            fail("a_mapping", f"{key}: table row for an item that has none")
    rows = {k: v[0] for k, v in row_groups.items() if len(v) == 1 and k in row_keys}
    # header and legend are frozen text
    _header_problems(by_role.get("table-header", []), zones[0]["y_top"], fail)
    legend = by_role.get("table-legend", [])
    got_legend = tuple(
        "".join(t.itertext()) for g in legend for t in g.iter(NS + "text")
    )
    if len(legend) != 1 or got_legend != LEGEND_TEXTS:
        fail("a_mapping", f"table legend {got_legend!r} is not the frozen legend")
    # review list: one entry per open conflict or gap, keyed by id
    open_entries = open_review_entries(spec)
    open_keys = {f"{k}[{i}]": e for k, i, e in open_entries}
    _review_problems(spec, band, open_keys, y_review, band_h, band_lines, fail)
    # warning marks: each row lists exactly its open entries (by component_id)
    by_cid: dict[Any, list[str]] = {}
    for kind, i, e in open_entries:
        by_cid.setdefault(e.get("component_id"), []).append(f"{kind}[{i}]")
    for key, g in rows.items():
        marks = [el for el in g if el.get("class") == "warn"]
        want = sorted(by_cid.get(key, []))
        got = sorted(k for el in marks for k in (el.get("data-review") or "").split())
        if got != want or len(marks) > 1:
            fail(
                "a_mapping",
                f"{key}: warning mark lists {got} but the open conflicts/gaps "
                f"for this component_id are {want}",
            )
    note(
        "a_mapping",
        f"{len(comps)} spec components, {sum(len(v) for v in comp_groups.values())} "
        f"component groups, {len(rows)} table rows, {len(open_keys)} open review entries",
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
    boxes: dict[str, list[tuple[float, float, float, float]]] = {}
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
            boxes.setdefault(cid, []).append((*xe, *ye))
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
            elif any(not abs(a - b) <= PX_TOL for a, b in zip(seams, exp)):
                fail(
                    "b_positions",
                    f"{cid}: joint seams off T(top - i*L) by > {PX_TOL} px",
                )
        # flex-joint pivot: at its stated elevation, inside the component
        for problem in _pivot_problems(spec, c, g, T):
            fail("b_positions", problem)
    # datums: every known datum drawn exactly once, at its elevation
    datum_groups: dict[str, list] = {}
    for g in by_role.get("datum", []):
        datum_groups.setdefault(g.get("data-datum"), []).append(g)
    for name in datum_groups:
        if name not in DATUM_NAMES:
            fail("a_mapping", f"unknown datum {name!r}")
    datum_lines: dict[str, list] = {}
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
        datum_lines[name] = lines
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
    sheaves = [
        cc
        for g in by_role.get("tensioner", [])
        for cc in g.iter(NS + "circle")
        if cc.get("data-part") == "sheave"
    ]
    if spec.tensioner_system is not None:
        for cc in sheaves:
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
    # rows: geometry, cells on the column edges, anchors and leaders
    anchor_boxes: dict[str, list] = {k: v for k, v in boxes.items()}
    if "drill_floor_el_m" in datum_lines:
        anchor_boxes["datums"] = [
            (*x_ext(ln), *y_ext(ln))
            for ln in datum_lines["drill_floor_el_m"]
            if x_ext(ln) and y_ext(ln)
        ]
    if sheaves:
        anchor_boxes["tensioner_system"] = [
            (
                _fnan(cc, "cx") - _fnan(cc, "r"),
                _fnan(cc, "cx") + _fnan(cc, "r"),
                _fnan(cc, "cy"),
                _fnan(cc, "cy"),
            )
            for cc in sheaves
        ]
    placed = []
    for key, g in rows.items():
        yc, anchor, problems = _row_geometry(key, g, anchor_boxes.get(key))
        for p in problems:
            fail("b_positions", p)
        if yc is not None:
            placed.append((key, yc, anchor))
    lo = zones[0]["y_top"] + HEADER_HEIGHT + ROW_PITCH / 2 - PX_TOL
    hi = y_bottom - LEGEND_HEIGHT - ROW_PITCH / 2 + PX_TOL
    worst = 0.0
    for key, yc, anchor in placed:
        if not lo <= yc <= hi:
            fail("b_positions", f"{key}: row at y={yc:.2f} outside the table band")
        if anchor is not None:
            off = abs(yc - anchor[1])
            worst = max(worst, off)
            if off > MAX_ROW_OFFSET_PX:
                fail(
                    "b_positions",
                    f"{key}: row offset {off:.1f} px from its component exceeds "
                    f"{MAX_ROW_OFFSET_PX:g} px",
                )
    ys = sorted(yc for _, yc, _ in placed)
    for a, b in zip(ys, ys[1:]):
        if b - a < ROW_PITCH - PX_TOL:
            fail("b_positions", f"rows overlap: centres {a:.2f} and {b:.2f}")
    anchored = sorted((a[1], yc, k) for k, yc, a in placed if a is not None)
    for (_, y1, k1), (_, y2, k2) in zip(anchored, anchored[1:]):
        if y2 < y1:
            fail("b_positions", f"rows {k1} and {k2} are not in their anchors' order")
    unanchored = [yc for _, yc, a in placed if a is None]
    if anchored and unanchored and min(unanchored) < max(y for _, y, _ in anchored):
        fail("b_positions", "an undrawn component's row sits among the drawn rows")
    note(
        "b_positions",
        f"max body-piece deviation {max_dev:.3f} px; {n_ticks} axis ticks checked; "
        f"largest row offset {worst:.1f} px",
    )

    # (c) numbers + (e) NOT_FOUND -----------------------------------------------------
    n_checked = 0
    text_fields: dict[str, list[str]] = {}
    for t in texts:
        r, owner = role_of(t)
        cid = owner.get("data-component-id") if r == "table-row" else None
        col = t.get("data-col") if r == "table-row" else None
        for msg in _rendered_text_problems(t, f"{r}:{cid or ''}", col):
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
            if txt == NA:
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
                    if unit == "ft" and (
                        abs(v - round(v)) > 1e-6 or parsed[0] != round(v)
                    ):
                        fail(
                            "f_design_data",
                            f"axis tick {sp.get('data-tick-el-m')} m is {v:.4f} ft, "
                            "not a round ft tick",
                        )
                n_checked += 1
                continue
            if fld is None:
                if re.search(r"\d", txt) and sp.get("data-dd") is None:
                    fail(
                        "c_numbers",
                        f"untraceable number {txt!r} (role={r}, component={cid})",
                    )
                continue
            if sp.get("data-kind") == "text":
                text_fields.setdefault(fld, []).append(txt)
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
    # verbatim text fields: all parts of one field, in order, joined by spaces
    for fld, parts in text_fields.items():
        try:
            exp = _path(sd, fld)
        except KeyError:
            exp = None
        got = " ".join(parts)
        if got != exp:
            fail("c_numbers", f"{fld}: {got!r} != {exp!r}")
        n_checked += 1
    # table cells: each equals its spec field under the column format
    n_cells = 0
    for key, g in rows.items():
        expected = _expected_cells(spec, key)
        cells: dict[str, list] = {}
        for t in g.iter(NS + "text"):
            cells.setdefault(t.get("data-col"), []).append(t)
        want_cols = set(expected) | {"dd"}
        got_cols = set(cells)
        if got_cols != want_cols or any(len(v) != 1 for v in cells.values()):
            fail(
                "a_mapping",
                f"row {key}: cells {sorted(got_cols)} "
                f"(x{max((len(v) for v in cells.values()), default=0)}) != the "
                f"row's cells {sorted(want_cols)}",
            )
        for col, (exp_text, exp_fld) in expected.items():
            if len(cells.get(col, [])) != 1:
                continue
            t = cells[col][0]
            n_cells += 1
            got = "".join(t.itertext())
            flds = [
                sp.get("data-field")
                for sp in t.iter(NS + "tspan")
                if sp.get("data-field") is not None
            ]
            want = [exp_fld] if exp_fld else []
            if flds != want:
                fail(
                    "c_numbers",
                    f"cell {key}.{col}: field {flds} != column field {want}",
                )
            if got == exp_text:
                continue
            if {got, exp_text} & {DASH, NA}:
                meaning = {DASH: "not applicable", NA: "NOT_FOUND"}
                fail(
                    "e_not_found",
                    f"cell {key}.{col}: shows {got!r} but the spec field is "
                    f"{meaning.get(exp_text, repr(exp_text))}",
                )
            else:
                fail(
                    "c_numbers",
                    f"cell {key}.{col}: shows {got!r}, spec gives {exp_text!r}",
                )
    note("c_numbers", f"{n_checked} printed numbers verified; {n_cells} table cells")

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
                    for sp in (rows[cid].iter(NS + "tspan") if cid in rows else [])
                    if sp.get("data-field") == f
                ]
                if any((sp.text or "") != NA for sp in shown):
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

    # (d) totals ---------------------------------------------------------------------------
    _check_totals(spec, res, fail, note, open_item)

    # (f) design data -------------------------------------------------------------
    _check_design_data(spec, root, texts, rows, role_of, fail, note, open_item)
    return _finish(spec, res)


def _header_problems(headers, y0: float, fail) -> None:
    if len(headers) != 1:
        fail("a_mapping", f"{len(headers)} table headers (expected one)")
        return
    g = headers[0]
    rects = [el for el in g if el.tag == NS + "rect"]
    if len(rects) != 1 or any(
        abs(a - b) > PX_TOL
        for a, b in zip(
            (_fnan(rects[0], k) for k in ("x", "y", "width", "height")),
            (TABLE_X0, y0, TABLE_X1 - TABLE_X0, HEADER_HEIGHT),
        )
    ):
        fail("b_positions", "table header is not on the frozen column edges")
    want = []
    for key, h1, h2, w in COLUMNS:
        a = COL_X[key]
        if key == "component":
            want.append((key, h1, a + 6, y0 + 20))
        else:
            want.append((key, h1, a + w / 2, y0 + 13))
            want.append((key, h2, a + w / 2, y0 + 26))
    got = [
        (t.get("data-col"), "".join(t.itertext()), _fnan(t, "x"), _fnan(t, "y"))
        for t in g
        if t.tag == NS + "text"
    ]
    if [(k, s) for k, s, _, _ in got] != [(k, s) for k, s, _, _ in want]:
        fail("a_mapping", "table header text/units are not the frozen column headers")
    elif any(
        not (abs(gx - wx) <= PX_TOL and abs(gy - wy) <= PX_TOL)
        for (_, _, gx, gy), (_, _, wx, wy) in zip(got, want)
    ):
        fail("b_positions", "table header text is off its column")
    seps = sorted(_fnan(ln, "x1") for ln in g if ln.tag == NS + "line")
    if len(seps) != len(COLUMNS) - 1 or any(
        not abs(a - COL_X[k]) <= PX_TOL for a, (k, *_r) in zip(seps, COLUMNS[1:])
    ):
        fail("b_positions", "table header column lines are not on the column edges")


def _row_geometry(key: str, g: ET.Element, boxes):
    """(row centre, anchor or None, problems) of one table row."""
    out = []
    rects = [el for el in g if el.tag == NS + "rect"]
    if len(rects) != 1:
        return None, None, [f"{key}: row needs exactly one background rect"]
    x, y, w, h = (_fnan(rects[0], k) for k in ("x", "y", "width", "height"))
    if not (
        abs(x - TABLE_X0) <= PX_TOL
        and abs(w - (TABLE_X1 - TABLE_X0)) <= PX_TOL
        and abs(h - ROW_PITCH) <= PX_TOL
    ):
        out.append(f"{key}: row background is not the table width x row pitch")
    yc = y + h / 2
    for ln in (el for el in g if el.tag == NS + "line"):
        if not abs(_fnan(ln, "y1") - (y + h)) <= PX_TOL:
            out.append(f"{key}: row rule not at the row's bottom edge")
    for t in g.iter(NS + "text"):
        col = t.get("data-col")
        if col not in CELL_POS:
            continue
        ckey, off, anchor = CELL_POS[col]
        if t.get("class") != CELL_CLASS[col]:
            out.append(f"{key}: cell {col} has class {t.get('class')!r}")
        if not (
            abs(_fnan(t, "x") - (COL_X[ckey] + off)) <= PX_TOL
            and abs(_fnan(t, "y") - (yc + CELL_BASELINE)) <= PX_TOL
            and t.get("text-anchor") == anchor
        ):
            out.append(f"{key}: cell {col} is not at its column position")
    for p in (el for el in g if el.tag == NS + "path"):
        pts = _points(p)
        if pts and not (
            abs(pts[0][0] - (TABLE_X1 - WARN_INSET)) <= PX_TOL
            and abs(pts[0][1] - (yc + 5)) <= PX_TOL
        ):
            out.append(f"{key}: warning mark is not in its row")
    leaders = [el for el in g if el.tag == NS + "polyline"]
    dots = [el for el in g if el.tag == NS + "circle"]
    if boxes is None:
        if leaders or dots:
            out.append(f"{key}: leader on a row whose item is not drawn")
        return yc, None, out
    if len(leaders) != 1 or len(dots) != 1:
        return yc, None, out + [f"{key}: row needs exactly one leader and one dot"]
    pts = _points(leaders[0])
    if not pts or len(pts) != 4:
        return yc, None, out + [f"{key}: leader is not the four-point row leader"]
    (ax, ay), (bx, by), (cx, cy), (ex, ey) = pts
    if not (
        abs(ex - (TABLE_X0 - 1)) <= PX_TOL
        and abs(ey - yc) <= PX_TOL
        and abs(cx - (TABLE_X0 - 10)) <= PX_TOL
        and abs(cy - yc) <= PX_TOL
        and abs(by - ay) <= PX_TOL
    ):
        out.append(f"{key}: leader does not end at the row's left edge and centre")
    if not (
        abs(_fnan(dots[0], "cx") - ax) <= PX_TOL
        and abs(_fnan(dots[0], "cy") - ay) <= PX_TOL
    ):
        out.append(f"{key}: leader dot is not at the leader's anchor")
    on = any(
        x0 - LEADER_TOL_PX <= ax <= x1 + LEADER_TOL_PX
        and y0 - PX_TOL <= ay <= y1 + PX_TOL
        for x0, x1, y0, y1 in boxes
    )
    if not on:
        out.append(f"{key}: leader does not end on its item ({ax:.2f}, {ay:.2f})")
    return yc, (ax, ay), out


def _review_problems(spec, band, open_keys, y0, band_h, lines, fail) -> None:
    if len(band) != 1:
        fail("a_mapping", f"{len(band)} review lists (expected one)")
        return
    g = band[0]
    rects = [el for el in g if el.tag == NS + "rect"]
    if len(rects) != 1 or any(
        abs(a - b) > PX_TOL
        for a, b in zip(
            (_fnan(rects[0], k) for k in ("x", "y", "width", "height")),
            (12.0, y0, SHEET_WIDTH - 24, band_h),
        )
    ):
        fail(
            "b_positions", "review list band is not above the title block at full width"
        )
    heads = [t for t in g if t.get("class") == "rl-hdr"]
    if len(heads) != 1 or "".join(heads[0].itertext()) != REVIEW_HEADER:
        fail("a_mapping", "review list header is not the frozen header")
    for k, t in enumerate(lines):
        want_y = y0 + REVIEW_HEADER_BASELINE + (k + 1) * REVIEW_PITCH
        if not (
            abs(_fnan(t, "y") - want_y) <= PX_TOL
            and abs(_fnan(t, "x") - (12.0 + REVIEW_TEXT_X)) <= PX_TOL
        ):
            fail("b_positions", f"review list line {k + 1} is not at its line position")
    keys = [t.get("data-review") for t in lines if t.get("class") == "rl"]
    none = [t for t in lines if t.get("class") == "rl-none"]
    if set(keys) != set(open_keys):
        fail(
            "a_mapping",
            f"review list entries {sorted(set(keys))} != open conflicts and gaps "
            f"{sorted(open_keys)}",
        )
    if bool(none) == bool(open_keys) or len(none) > 1:
        fail("a_mapping", "review list 'none open' line does not match the spec")
    warns = [el.get("data-review") for el in g if el.get("class") == "warn"]
    if sorted(warns) != sorted(open_keys):
        fail(
            "a_mapping",
            f"review list warning marks {sorted(warns)} != {sorted(open_keys)}",
        )
    for key, e in open_keys.items():
        kind, i = key[:-1].split("[")
        shown = {
            sp.get("data-field")
            for t in lines
            if t.get("data-review") == key
            for sp in t.iter(NS + "tspan")
            if sp.get("data-field")
        }
        need = {
            f"{kind}.{i}.{f}"
            for f in ("component_id", "item", "detail")
            if e.get(f) not in (None, "")
        }
        if shown != need:
            fail(
                "a_mapping",
                f"review entry {key} shows {sorted(shown)}, needs {sorted(need)}",
            )
        # review r1 finding 5: classification, whole text, mark attachment
        entry_lines = [t for t in lines if t.get("data-review") == key]
        if not entry_lines:
            continue
        first = entry_lines[0]
        label = "Conflict" if kind == "data_conflicts" else "Gap"
        spans = list(first.iter(NS + "tspan"))
        if not spans or (spans[0].text or "") != label or spans[0].attrib:
            fail(
                "a_mapping",
                f"review entry {key}: printed classification "
                f"{(spans[0].text if spans else '')!r} != {label!r}",
            )
        want = f"{label} · " + (
            "drawing" if e.get("component_id") is None else str(e["component_id"])
        )
        if e.get("item") not in (None, ""):
            want += f" · {e['item']}"
        if e.get("detail") not in (None, ""):
            want += f": {e['detail']}"
        got = " ".join("".join(t.itertext()) for t in entry_lines)
        if got != want:
            fail("a_mapping", f"review entry {key}: entry text {got!r} != {want!r}")
        marks = [
            el for el in g if el.get("class") == "warn" and el.get("data-review") == key
        ]
        y_line = _fnan(first, "y")
        for m in marks:
            pts = _points(m)
            if not pts or not (
                abs(pts[0][0] - (12.0 + REVIEW_WARN_X)) <= PX_TOL
                and abs(pts[0][1] - (y_line + REVIEW_WARN_DY)) <= PX_TOL
            ):
                fail(
                    "b_positions",
                    f"review entry {key}: warning mark is not beside its entry's "
                    "first line",
                )


def _pivot_problems(spec, c, g, T) -> list[str]:
    key = {
        CT.UPPER_FLEX_JOINT: "ufj_pivot_el_m",
        CT.LOWER_FLEX_JOINT: "lfj_pivot_el_m",
    }.get(c.type)
    pivots = [cc for cc in g.iter(NS + "circle") if cc.get("data-part") == "pivot"]
    rv = spec.reference_totals.get(key) if key else None
    first = key and next(x for x in spec.components if x.type == c.type) is c
    if not (first and rv is not None and _known(rv.value)):
        return (
            [f"{c.id}: pivot drawn without a stated pivot elevation"] if pivots else []
        )
    zp = float(rv.value)
    if len(pivots) != 1:
        return [f"{c.id}: {len(pivots)} pivot symbols for {key} (expected one)"]
    out = []
    attr = _float_attr(pivots[0], "data-pivot-el-m")
    if attr is None or abs(attr - zp) > 1e-9:
        out.append(f"{c.id}: pivot data-pivot-el-m != reference_totals.{key}")
    if not float(c.bottom_el_m) - 1e-9 <= zp <= float(c.top_el_m) + 1e-9:
        out.append(f"{c.id}: pivot EL {zp:.3f} m outside the component")
    cy = _float_attr(pivots[0], "cy")
    if cy is None or abs(cy - T.y(zp)) > PX_TOL:
        out.append(f"{c.id}: pivot symbol not at T({zp:g})")
    return out


def _check_design_data(spec, root, texts, rows, role_of, fail, note, open_item):
    k = "f_design_data"
    has_register = bool(spec.design_data)
    missing: set[str] = set()

    def ids_of(row_key, fields):
        ids, lacking = set(), False
        for fld in fields:
            p = _prov_for(spec, row_key, fld)
            did = getattr(p, "design_data_id", None)
            if did is None:
                lacking = True
                missing.add(
                    f"{row_key + ':' if row_key and '.' not in fld else ''}{fld}"
                )
            else:
                ids.add(did)
        return ids, lacking

    def check_printed(where, printed, want):
        got = {d for d, _ in printed}
        for did, flag in printed:
            item = spec.design_item(did)
            if item is None:
                fail(k, f"{where}: {did} does not resolve in the design-data register")
                continue
            want_flag = SOURCE_CLASSES.get(item.source_class)
            if flag != want_flag:
                fail(
                    k,
                    f"{where}: {did} class flag {flag!r} != {want_flag!r} "
                    f"({item.source_class})",
                )
        if got != want:
            fail(
                k,
                f"{where}: printed IDs {sorted(got)} != IDs of the printed values "
                f"{sorted(want)} (missing {sorted(want - got)}, extra {sorted(got - want)})",
            )

    n_rows = 0
    for key, g in rows.items():
        cells = [t for t in g.iter(NS + "text") if t.get("data-col") != "dd"]
        dd = [t for t in g.iter(NS + "text") if t.get("data-col") == "dd"]
        fields = [f for t in cells for f in _value_fields(t)]
        want, lacking = ids_of(key, fields)
        if len(dd) != 1:
            continue  # reported by the cell set check in (a)
        check_printed(f"row {key}", _dd_printed(dd[0]), want)
        items = sorted(want, key=lambda d: int(d.split("-")[1]))
        parts = [
            d
            + SOURCE_CLASSES.get(getattr(spec.design_item(d), "source_class", ""), "?")
            for d in items
        ] + ([NA] if lacking else [])
        got = "".join(dd[0].itertext())
        if got != ", ".join(parts):
            fail(k, f"row {key}: Design data cell {got!r} != {', '.join(parts)!r}")
        n_rows += 1
    for t in texts:
        r, _ = role_of(t)
        if r in ("table-row", "table-header", "table-legend", "review-list", "axis"):
            continue
        fields = _value_fields(t)
        printed = _dd_printed(t)
        if not fields and not printed:
            continue
        want, _ = ids_of(None, fields)
        check_printed(f"{r} text {''.join(t.itertext())[:40]!r}", printed, want)
    for f in sorted(missing):
        msg = f"{f}: printed value has no design_data_id"
        if has_register:
            fail(k, msg)
        else:
            open_item(k, msg + " (the spec has no design-data register)")
    value_fails, value_opens = _design_value_problems(spec)
    for p in value_fails:
        fail(k, p)
    for p in value_opens:
        open_item(k, p)
    for p in _archive_problems(spec, root):
        fail(k, p)
    note(
        k,
        f"{len(spec.design_data)} design-data items, {len(spec.references)} references; "
        f"{n_rows} Design data cells checked",
    )


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
    stacked = spec.stacked()
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
            gap = float(a.bottom_el_m) - float(b.top_el_m)
            # the only excused discontinuity: an explicit landing relation
            # (a.nested_in == b.id), a's lower part inside its host b
            if (
                a.nested_in == b.id
                and gap < 0
                and _known(b.bottom_el_m)
                and float(b.bottom_el_m) <= float(a.bottom_el_m)
                and float(b.top_el_m) <= float(a.top_el_m)
            ):
                note(k, f"{a.id} lands {-gap:.3f} m inside {b.id} (explicit nested_in)")
                continue
            fail(
                k,
                f"elevation gap/overlap between {a.id} and {b.id}: {gap:+.4f} m",
            )
    for c in spec.components:
        if not c.nested_in:
            continue
        if spec.is_overlay(c):
            note(
                k,
                f"{c.id} is an overlay inside {c.nested_in} (explicit nested_in; "
                "not in the chain or the length sum)",
            )
        elif c not in stacked:
            fail(
                k,
                f"{c.id}: nested_in {c.nested_in} but neither an overlay nor in the "
                "stacked chain",
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
    # flex joints may be overlays (e.g. inside the LMRP): search every component
    ufj = next((c for c in spec.components if c.type == CT.UPPER_FLEX_JOINT), None)
    lfj = next((c for c in spec.components if c.type == CT.LOWER_FLEX_JOINT), None)
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
        value = _path(sd, fld)
    except KeyError:
        return NOT_FOUND
    if fld == "title_block.report_document_no" and value in (None, ""):
        return NOT_FOUND  # optional; absent prints n/a
    return value


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
