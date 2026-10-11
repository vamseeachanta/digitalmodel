"""Read the CC0 Mendeley diagnostic plate without inventing engineering units.

Source version: DOI 10.17632/hnfwm9mjs6.1, retrieved 2026-10-11.
Provenance and independent inventory: testdata/mendeley_plate/manifest.json in
this module's tests. Embedded chart titles establish normalised displacement,
load and power. The source bytes are pinned because those titles are not cells.
"""

from __future__ import annotations

import hashlib
import math
from dataclasses import dataclass
from pathlib import Path

import xlrd

# Published file digest verified against the dataset download metadata.
_SOURCE_SHA256 = "69723091c93269761a2fa79fc774a6ae7bf0eff786af439fb94c40171018058e"

# Literal class-name equivalents only. The existing generator reference maps
# supply insufficiency to fluid pound and sand to abrasion; those mechanisms
# are not established by the source labels and remain unset here.
_LOAD_CLASSES: dict[str, str | None] = {
    "工况正常": "NORMAL",
    "供液不足": None,
    "气体影响": "GAS_INTERFERENCE",
    "抽油杆断脱": "ROD_PARTING",
    "上碰泵": "PUMP_TAGGING_UP",
    "下碰泵": "PUMP_TAGGING_DOWN",
    "游动阀漏失": "VALVE_LEAK_TV",
    "固定阀漏失": "VALVE_LEAK_SV",
    "活塞脱出工作筒": "PLUNGER_OUT_OF_BARREL",
    "出砂": None,
}
_POWER_CLASSES = {"过平衡", "欠平衡", "减速箱故障"}
_EMPTY_TYPES = {xlrd.XL_CELL_EMPTY, xlrd.XL_CELL_BLANK}


@dataclass(frozen=True)
class DiagnosticPlateCurve:
    """A labelled, dimensionless curve; power values are never pump loads."""

    label: str
    block_index: int
    columns: tuple[int, int]
    ordinate: str
    fault_mode: str | None
    position: tuple[float, ...]
    values: tuple[float, ...]


def _column_pairs(sheet: xlrd.sheet.Sheet) -> list[tuple[int, int]]:
    """Find populated adjacent pairs, allowing arbitrary blank-column padding."""
    columns = [c for c in range(sheet.ncols) if any(
        sheet.cell(r, c).ctype not in _EMPTY_TYPES for r in range(sheet.nrows)
    )]
    if len(columns) % 2:
        raise ValueError(f"Unpaired column in plate class {sheet.name}")
    pairs = list(zip(columns[::2], columns[1::2], strict=True))
    if any(y != x + 1 for x, y in pairs):
        raise ValueError(f"Nonadjacent pair in plate class {sheet.name}")
    return pairs


def _read_pair(
    sheet: xlrd.sheet.Sheet, column: int,
) -> tuple[tuple[float, ...], tuple[float, ...]]:
    """Preserve sample order and omit only wholly blank row padding."""
    points = []
    for row in range(sheet.nrows):
        cells = (sheet.cell(row, column), sheet.cell(row, column + 1))
        if all(cell.ctype in _EMPTY_TYPES for cell in cells):
            continue
        if any(cell.ctype != xlrd.XL_CELL_NUMBER for cell in cells):
            raise ValueError(f"Incomplete or nonnumeric pair: {sheet.name}, {row}")
        point = tuple(float(cell.value) for cell in cells)
        if not all(math.isfinite(value) for value in point):
            raise ValueError(f"Nonfinite point: {sheet.name}, {row}")
        # Domain follows the source's normalised chart axes; no rescaling.
        if not all(0 <= value <= 1 for value in point):
            raise ValueError(f"Point outside normalised domain: {sheet.name}, {row}")
        points.append(point)
    if not points:
        raise ValueError(f"Empty curve: {sheet.name}, {column}")
    x, y = zip(*points, strict=True)
    return tuple(x), tuple(y)


def load_diagnostic_plate(path: str | Path) -> list[DiagnosticPlateCurve]:
    """Load all labelled curves from the exact public source-version workbook.

    Raises ValueError for changed source bytes or unexpected plate layout.
    No download, physical-unit conversion or classifier inference is performed.
    """
    raw = Path(path).read_bytes()
    if hashlib.sha256(raw).hexdigest() != _SOURCE_SHA256:
        raise ValueError("Diagnostic plate source digest does not match version 1")
    book = xlrd.open_workbook(file_contents=raw)
    curves = []
    labels = []
    for sheet in book.sheets():
        if sheet.name not in _LOAD_CLASSES and sheet.name not in _POWER_CLASSES:
            raise ValueError(f"Unknown plate class: {sheet.name}")
        labels.append(sheet.name)
        quantities = ("load", "power") if sheet.name in _LOAD_CLASSES else ("power",)
        pairs = _column_pairs(sheet)
        if len(pairs) != len(quantities):
            raise ValueError(f"Unexpected curve blocks: {sheet.name}")
        for index, ((x_col, y_col), quantity) in enumerate(
            zip(pairs, quantities, strict=True)
        ):
            position, values = _read_pair(sheet, x_col)
            mode = _LOAD_CLASSES.get(sheet.name) if quantity == "load" else None
            curves.append(DiagnosticPlateCurve(
                label=sheet.name, block_index=index, columns=(x_col, y_col),
                ordinate=quantity, fault_mode=mode, position=position, values=values,
            ))
    if len(labels) != len(set(labels)) or set(labels) != (
        set(_LOAD_CLASSES) | _POWER_CLASSES
    ):
        raise ValueError("Missing or repeated plate classes")
    return curves
