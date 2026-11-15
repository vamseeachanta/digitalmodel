#!/usr/bin/env python3
"""Convert vendor fender tables into the fender database schema.

The vendor tables (one CSV per family, a value per row with its catalogue,
page and date read) are held privately because they carry manufacturers'
published values. This script reads them from a caller-supplied directory and
writes, to a caller-supplied output directory:

* ``fender_database.csv``: every row in the schema of
  ``data/fender_design/database_schema.yaml``;
* ``fender_catalogue.csv``: rated rows only, in the column layout that
  :func:`digitalmodel.hydrodynamics.berthing_energy.load_fender_catalogue`
  reads (``FENDER_CATALOGUE_COLUMNS``), ready for the workbook generator.

Nothing is written inside this repository unless the caller points the output
there; vendor values are not committed to the public repository.

Input directory layout (as produced by the private vendor-data work)::

    <in>/solid_rubber.csv  <in>/floating.csv  <in>/sources.csv

Usage::

    python scripts/fender_design/import_vendor_tables.py --in <dir> --out <dir>
"""

from __future__ import annotations

import argparse
import csv
from pathlib import Path

DATABASE_COLUMNS = (
    "record_id",
    "family",
    "fender_type",
    "vendor",
    "product_line",
    "size_designation",
    "diameter_or_height_mm",
    "length_mm",
    "grade_or_pressure",
    "internal_pressure_kpa",
    "rated_deflection_pct",
    "rated_energy_knm",
    "rated_reaction_kn",
    "hull_pressure_kpa",
    "basis",
    "perf_basis",
    "tolerance_pct",
    "weight_kg",
    "float_draft_mm",
    "test_standard",
    "source_id",
    "catalogue",
    "catalogue_edition",
    "page_or_table",
    "value_method",
    "accessed_on",
)

CATALOGUE_COLUMNS = (
    "fender_id",
    "manufacturer",
    "fender_type",
    "diameter_mm",
    "length_mm",
    "internal_pressure_kpa",
    "rated_energy_knm",
    "reaction_kn",
    "hull_pressure_kpa",
    "rated_deflection_pct",
    "source",
)


def _num(text: str) -> str:
    """Return the cell if it parses as a positive number, else ''."""
    text = (text or "").strip()
    if not text:
        return ""
    try:
        value = float(text)
    except ValueError:
        return ""
    return text if value > 0 else ""


def _read(path: Path) -> list[dict]:
    with path.open(newline="", encoding="utf-8") as fh:
        return list(csv.DictReader(fh))


def convert(in_dir: Path) -> list[dict]:
    """Rows of both vendor tables in the database schema."""
    sources = {r["source_id"]: r for r in _read(in_dir / "sources.csv")}
    out: list[dict] = []
    for r in _read(in_dir / "solid_rubber.csv"):
        s = sources.get(r["source_id"], {})
        out.append(
            dict(
                record_id=r["record_id"],
                family="solid",
                fender_type=r["fender_type"],
                vendor=r["vendor"],
                product_line=r["product_line"],
                size_designation=r["size_designation"],
                diameter_or_height_mm=_num(r["height_mm"]),
                length_mm=_num(r["length_mm"]),
                grade_or_pressure=r["compound_grade"],
                internal_pressure_kpa="",
                rated_deflection_pct=_num(r["rated_deflection_pct"]),
                rated_energy_knm=_num(r["rated_energy_knm"]),
                rated_reaction_kn=_num(r["rated_reaction_kn"]),
                hull_pressure_kpa="",
                basis=r["basis"],
                perf_basis=r["perf_basis"],
                tolerance_pct=_num(r["tolerance_pct"]),
                weight_kg=_num(r["weight_kg"]),
                float_draft_mm="",
                test_standard=r["test_standard"],
                source_id=r["source_id"],
                catalogue=s.get("document_title", ""),
                catalogue_edition=s.get("edition_or_date", ""),
                page_or_table=r["page_or_table"],
                value_method=r["value_method"],
                accessed_on=r["accessed_on"],
            )
        )
    for r in _read(in_dir / "floating.csv"):
        s = sources.get(r["source_id"], {})
        out.append(
            dict(
                record_id=r["record_id"],
                family="floating",
                fender_type=r["fender_type"],
                vendor=r["vendor"],
                product_line=r["product_line"],
                size_designation=r["size_designation"],
                diameter_or_height_mm=_num(r["diameter_mm"]),
                length_mm=_num(r["length_mm"]),
                grade_or_pressure=(
                    f"{r['initial_pressure_kpa']} kPa" if r["initial_pressure_kpa"] else ""
                ),
                internal_pressure_kpa=_num(r["initial_pressure_kpa"]),
                rated_deflection_pct=_num(r["rated_deflection_pct"]),
                rated_energy_knm=_num(r["gea_knm"]),
                rated_reaction_kn=_num(r["reaction_kn"]),
                hull_pressure_kpa=_num(r["hull_pressure_kpa"]),
                basis="per-unit",
                perf_basis="GEA",
                tolerance_pct="",
                weight_kg=_num(r["weight_kg"]),
                float_draft_mm="",
                test_standard=r["test_standard"],
                source_id=r["source_id"],
                catalogue=s.get("document_title", ""),
                catalogue_edition=s.get("edition_or_date", ""),
                page_or_table=r["page_or_table"],
                value_method=r["value_method"],
                accessed_on=r["accessed_on"],
            )
        )
    return out


def catalogue_rows(rows: list[dict]) -> list[dict]:
    """Rated per-unit rows in the selection-catalogue layout.

    Rows without a rated energy or reaction, and per-metre rows (extruded and
    arch fenders quoted per metre length), are left out: selection compares
    one fender unit with the energy per fender.
    """
    out = []
    for r in rows:
        if not (r["rated_energy_knm"] and r["rated_reaction_kn"]):
            continue
        if r["basis"] and r["basis"] != "per-unit":
            continue
        out.append(
            dict(
                fender_id=r["record_id"],
                manufacturer=r["vendor"],
                fender_type=r["fender_type"],
                diameter_mm=r["diameter_or_height_mm"],
                length_mm=r["length_mm"],
                internal_pressure_kpa=r["internal_pressure_kpa"],
                rated_energy_knm=r["rated_energy_knm"],
                reaction_kn=r["rated_reaction_kn"],
                hull_pressure_kpa=r["hull_pressure_kpa"],
                rated_deflection_pct=r["rated_deflection_pct"],
                source=(
                    f"{r['source_id']} {r['catalogue']} ({r['catalogue_edition']}), "
                    f"{r['page_or_table']}; read {r['accessed_on']}"
                ),
            )
        )
    return out


def _write(path: Path, columns, rows) -> None:
    with path.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(columns), lineterminator="\n")
        w.writeheader()
        w.writerows(rows)


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--in", dest="in_dir", required=True, type=Path)
    ap.add_argument("--out", dest="out_dir", required=True, type=Path)
    args = ap.parse_args(argv)
    rows = convert(args.in_dir)
    args.out_dir.mkdir(parents=True, exist_ok=True)
    _write(args.out_dir / "fender_database.csv", DATABASE_COLUMNS, rows)
    cat = catalogue_rows(rows)
    _write(args.out_dir / "fender_catalogue.csv", CATALOGUE_COLUMNS, cat)
    print(f"database rows {len(rows)}; catalogue rows {len(cat)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
