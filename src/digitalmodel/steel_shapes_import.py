"""Offline AISC v16 workbook conversion; network access belongs to the CLI."""

from __future__ import annotations

import datetime as dt
import hashlib
from io import BytesIO
from pathlib import Path

from digitalmodel.steel_shapes import validate_catalog

_FAMILY_COLUMNS = {
    "W": {"d": "d", "bf": "bf", "tw": "tw", "tf": "tf", "kdes": "kdes"},
    "HP": {"d": "d", "bf": "bf", "tw": "tw", "tf": "tf", "kdes": "kdes"},
    "S": {"d": "d", "bf": "bf", "tw": "tw", "tf": "tf", "kdes": "kdes"},
    "C": {"d": "d", "bf": "bf", "tw": "tw", "tf": "tf", "kdes": "kdes"},
    "L": {"d": "d", "b": "b", "t": "t", "kdes": "kdes"},
    "HSS_RECT": {"d": "Ht", "b": "B", "t": "tdes", "tnom": "tnom"},
    "HSS_ROUND": {"d": "OD", "t": "tdes", "tnom": "tnom"},
    "PIPE": {"d": "OD", "t": "tdes", "tnom": "tnom"},
}


def _header_indices(header: tuple) -> tuple[dict, dict]:
    """Split duplicate imperial/SI headings without hardcoded column offsets."""
    edi = [i for i, value in enumerate(header) if value == "EDI_Std_Nomenclature"]
    manual = [i for i, value in enumerate(header) if value == "AISC_Manual_Label"]
    if len(edi) != 2 or len(manual) != 2 or not edi[0] < manual[0] < edi[1] < manual[1]:
        raise ValueError("Missing/ambiguous imperial and metric name heading blocks")
    imperial = {
        value: i for i, value in enumerate(header[: edi[1]]) if value is not None
    }
    metric = {"EDI_Std_Nomenclature": edi[1], "AISC_Manual_Label": manual[1]}
    required = {
        "Type",
        "A",
        "d",
        "Ht",
        "OD",
        "bf",
        "B",
        "b",
        "tw",
        "tf",
        "t",
        "tnom",
        "tdes",
        "kdes",
    }
    missing = required - imperial.keys()
    if missing:
        raise ValueError(f"Missing AISC dimension heading(s): {sorted(missing)}")
    return imperial, metric


def _record(row: tuple, number: int, imperial: dict, metric: dict) -> dict | None:
    from openpyxl.utils import get_column_letter

    family = row[imperial["Type"]]
    if family == "HSS":
        diameter = row[imperial["OD"]]
        family = "HSS_ROUND" if isinstance(diameter, (int, float)) else "HSS_RECT"
    if family not in _FAMILY_COLUMNS:
        return None
    mapping = _FAMILY_COLUMNS[family]
    dims = {key: row[imperial[column]] for key, column in mapping.items()}
    names = {
        "name": row[imperial["AISC_Manual_Label"]],
        "metric_name": row[metric["AISC_Manual_Label"]],
    }
    columns = {
        key: get_column_letter(imperial[column] + 1) for key, column in mapping.items()
    }
    columns.update(
        name=get_column_letter(imperial["AISC_Manual_Label"] + 1),
        metric_name=get_column_letter(metric["AISC_Manual_Label"] + 1),
        area=get_column_letter(imperial["A"] + 1),
    )
    return {
        **names,
        "family": family,
        "dimensions": dims,
        "area": row[imperial["A"]],
        "aliases": [row[imperial["EDI_Std_Nomenclature"]]],
        "metric_aliases": [row[metric["EDI_Std_Nomenclature"]]],
        "source": {"sheet": "Database v16.0", "row": number, "columns": columns},
    }


def import_workbook(path: str | Path, *, source_url: str) -> dict:
    """Convert all in-scope rows, preserving metric aliases and source cells.

    Unsupported families are excluded explicitly. Missing/invalid dimensions,
    aliases or source headings fail the complete import; no partial catalogue is
    emitted. No Geometry construction or network access occurs here.
    """
    from openpyxl import load_workbook

    path = Path(path)
    raw = path.read_bytes()
    digest = hashlib.sha256(raw).hexdigest()
    book = load_workbook(BytesIO(raw), read_only=True, data_only=True)
    try:
        if "Database v16.0" not in book.sheetnames:
            raise ValueError("Expected AISC worksheet 'Database v16.0'")
        rows = iter(book["Database v16.0"].iter_rows())
        imperial, metric = _header_indices(tuple(cell.value for cell in next(rows)))
        records = []
        for number, cells in enumerate(rows, start=2):
            item = _record(
                tuple(cell.value for cell in cells), number, imperial, metric
            )
            if item is not None:
                item["source"]["area_number_format"] = cells[
                    imperial["A"]
                ].number_format
                records.append(item)
    finally:
        book.close()
    data = {
        "schema": "aisc-shapes/1",
        "units": {"length": "in", "area": "in^2"},
        "source": {
            "catalog_id": "aisc-shapes",
            "publication": "AISC Shapes Database v16.0, August 2023",
            "url": source_url,
            "sha256": digest,
            "retrieved_on": dt.datetime.now(dt.timezone.utc).date().isoformat(),
            "access_note": "User-supplied workbook; verify source, reuse terms and edition before engineering use.",
        },
        "sections": records,
    }
    validate_catalog(data)
    if not any(item["family"] == "W" for item in records):
        raise ValueError(
            "No W-shapes in imported workbook; full-W coverage is unestablished"
        )
    return data
