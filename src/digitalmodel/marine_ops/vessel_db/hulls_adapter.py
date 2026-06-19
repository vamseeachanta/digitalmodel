"""
ABOUTME: Off-repo adapter for REAL model-test / computed RAO workbooks, feeding
the diffraction RAO layer without committing the data into this public repo.

Real RAO datasets (e.g. semisubmersible / OSV motion workbooks) are typically
project-confidential, so they live OFF-REPO. This adapter resolves a library
directory from an env var, parses the RAO workbooks it finds into the repo's
``hydrodynamics.models.RAOData``, and returns them at runtime only. Nothing —
no data, vessel names, or paths — is committed here. Mirrors the OCIMF/WED
"code in repo, data off-repo" pattern.

Set the library with::

    export VESSEL_RAO_LIBRARY_PATH=/path/to/your/rao/library

Supported workbook layout (auto-detected): heading-block tables where each block
starts with a header row carrying the heading in column 0 and the DOF labels
1..6 in the next columns, followed by rows of ``period, amp(1..6), phase(1..6)``.
Workbooks that don't match are skipped (reported), never guessed.
"""

from __future__ import annotations

import logging
import math
import os
from pathlib import Path
from typing import Optional

import numpy as np

logger = logging.getLogger(__name__)

_ENV = "VESSEL_RAO_LIBRARY_PATH"
_DOF_SEQ = [1, 2, 3, 4, 5, 6]  # surge, sway, heave, roll, pitch, yaw


def resolve_rao_library_dir(require: bool = False) -> Optional[Path]:
    """Resolve the off-repo RAO library dir from ``VESSEL_RAO_LIBRARY_PATH``."""
    val = os.environ.get(_ENV)
    if val and Path(val).is_dir():
        return Path(val)
    if require:
        raise FileNotFoundError(
            f"RAO library not found. Set {_ENV} to a directory of RAO workbooks. "
            "Real RAOs are held off-repo (project-confidential); this repo ships "
            "only the reader, not the data."
        )
    return None


def _rows(path: Path) -> list[list]:
    """Return all cells of the first usable sheet as a list of rows (xls/xlsx)."""
    suffix = path.suffix.lower()
    if suffix == ".xls":
        import xlrd
        book = xlrd.open_workbook(str(path))
        for sheet in book.sheets():
            rows = [[sheet.cell_value(r, c) for c in range(sheet.ncols)]
                    for r in range(sheet.nrows)]
            if _find_header_rows(rows):
                return rows
        return []
    if suffix == ".xlsx":
        import openpyxl
        wb = openpyxl.load_workbook(str(path), read_only=True, data_only=True)
        for ws in wb.worksheets:
            rows = [list(r) for r in ws.iter_rows(values_only=True)]
            if _find_header_rows(rows):
                return rows
        return []
    return []


def _is_num(x) -> bool:
    return isinstance(x, (int, float)) and not isinstance(x, bool)


def _find_header_rows(rows: list[list]) -> list[int]:
    """Indices of heading-block header rows (cols 1..6 == 1..6)."""
    out = []
    for i, row in enumerate(rows):
        if len(row) < 7:
            continue
        seq = row[1:7]
        if all(_is_num(v) for v in seq) and [round(v) for v in seq] == _DOF_SEQ:
            if _is_num(row[0]):  # col0 = heading
                out.append(i)
    return out


def parse_heading_block_rao(rows: list[list], vessel_name: str):
    """Parse the heading-block RAO layout into ``RAOData`` (or None)."""
    from digitalmodel.hydrodynamics.models import RAOData

    headers = _find_header_rows(rows)
    if not headers:
        return None

    blocks: dict[float, dict] = {}  # heading -> {periods, amp(list of 6 lists), phase(...)}
    for hi, start in enumerate(headers):
        heading = float(rows[start][0])
        end = headers[hi + 1] if hi + 1 < len(headers) else len(rows)
        periods, amps, phases = [], [], []
        for r in range(start + 1, end):
            row = rows[r]
            if not row or not _is_num(row[0]):
                continue
            if len(row) < 13:
                continue
            amp = row[1:7]
            pha = row[7:13]
            if not all(_is_num(v) for v in amp):
                continue
            periods.append(float(row[0]))
            amps.append([float(v) for v in amp])
            phases.append([float(v) if _is_num(v) else 0.0 for v in pha])
        if periods:
            blocks[heading] = {
                "periods": np.array(periods),
                "amp": np.array(amps),     # [n_period, 6]
                "phase": np.array(phases),
            }
    if not blocks:
        return None

    headings = sorted(blocks)
    # Reference period grid = the longest block; interpolate others onto it.
    ref = max(blocks.values(), key=lambda b: len(b["periods"]))
    ref_T = ref["periods"]
    order = np.argsort(ref_T)  # ascending period
    ref_T = ref_T[order]
    freqs = 2.0 * math.pi / ref_T  # rad/s (ascending period -> descending freq)

    n_freq, n_dir = len(ref_T), len(headings)
    amp = np.zeros((n_freq, n_dir, 6))
    pha = np.zeros((n_freq, n_dir, 6))
    for di, hd in enumerate(headings):
        b = blocks[hd]
        bo = np.argsort(b["periods"])
        bT = b["periods"][bo]
        for dof in range(6):
            amp[:, di, dof] = np.interp(ref_T, bT, b["amp"][bo, dof])
            pha[:, di, dof] = np.interp(ref_T, bT, b["phase"][bo, dof])

    return RAOData(
        frequencies=freqs,
        directions=np.array(headings, dtype=float),
        amplitudes=amp,
        phases=pha,
        vessel_name=vessel_name,
    )


def read_rao_workbook(path: Path):
    """Parse one RAO workbook into RAOData, or None if the layout isn't recognised."""
    rows = _rows(Path(path))
    if not rows:
        return None
    name = Path(path).parent.name or Path(path).stem
    try:
        return parse_heading_block_rao(rows, vessel_name=name)
    except Exception as exc:  # pragma: no cover - defensive on bespoke workbooks
        logger.warning("RAO parse failed for %s: %s", path, exc)
        return None


def real_rao_datasets(base: Optional[Path] = None) -> dict[str, object]:
    """Parse every recognised RAO workbook in the off-repo library.

    Returns ``{dataset_name: RAOData}`` (empty if the library is not configured).
    Unrecognised workbooks are skipped and logged — never guessed.
    """
    d = base or resolve_rao_library_dir()
    if d is None:
        return {}
    out: dict[str, object] = {}
    for path in sorted(list(d.rglob("*.xls")) + list(d.rglob("*.xlsx"))):
        rao = read_rao_workbook(path)
        if rao is not None and len(rao.frequencies) > 1:
            out[rao.vessel_name] = rao
    return out


def library_summary(base: Optional[Path] = None) -> dict:
    """Count parseable RAO datasets in the off-repo library (0 if unset)."""
    d = base or resolve_rao_library_dir()
    if d is None:
        return {"configured": False, "datasets": 0, "names": []}
    ds = real_rao_datasets(d)
    return {"configured": True, "datasets": len(ds), "names": sorted(ds)}


if __name__ == "__main__":
    import json
    print(json.dumps(library_summary(), indent=2))
