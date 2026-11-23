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
  reads (``FENDER_CATALOGUE_COLUMNS``) plus a ``perf_basis`` column, ready
  for the workbook generator.

A catalogue compares rows with each other, so every row of one fender type
must be rated on the same performance basis (RPD, CV or GEA). Where the
vendor tables hold more than one basis for a type, choose one with
``--perf-basis``; otherwise the export stops with the types and bases found.
Floating fenders are rated at GEA.

Nothing is written inside this repository unless the caller points the output
there; vendor values are not committed to the public repository.

Input directory layout (as produced by the private vendor-data work)::

    <in>/solid_rubber.csv  <in>/floating.csv  <in>/sources.csv

Usage::

    python scripts/fender_design/import_vendor_tables.py --in <dir> --out <dir> [--perf-basis RPD]
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
    "perf_basis",
)

PERF_BASES = ("RPD", "CV", "GEA")


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


def catalogue_rows(rows: list[dict], perf_basis: str | None = None) -> list[dict]:
    """Rated per-unit rows in the selection-catalogue layout.

    Rows without a rated energy or reaction, and per-metre rows (extruded and
    arch fenders quoted per metre length), are left out: selection compares
    one fender unit with the energy per fender. ``perf_basis`` keeps only
    solid-fender rows rated on that basis (floating rows are GEA). The
    performance basis is written with every row, and rows of one fender type
    on different bases (or without a basis beside rows with one) raise
    ValueError: they are not comparable.
    """
    if perf_basis is not None and perf_basis not in PERF_BASES:
        raise ValueError(f"perf_basis must be one of {PERF_BASES}")
    out = []
    bases: dict[str, set] = {}
    for r in rows:
        if not (r["rated_energy_knm"] and r["rated_reaction_kn"]):
            continue
        if r["basis"] and r["basis"] != "per-unit":
            continue
        if perf_basis is not None and r["family"] == "solid" and r["perf_basis"] != perf_basis:
            continue
        bases.setdefault(r["fender_type"], set()).add(r["perf_basis"])
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
                    f"{r['page_or_table']}; read {r['accessed_on']}; basis {r['perf_basis'] or 'not stated'}"
                ),
                perf_basis=r["perf_basis"],
            )
        )
    mixed = {t: sorted(b) for t, b in bases.items() if len(b) > 1}
    if mixed:
        raise ValueError(
            "rows of one fender type on different performance bases: "
            + "; ".join(f"{t}: {', '.join(x or '(blank)' for x in b)}" for t, b in sorted(mixed.items()))
            + ". Choose one with --perf-basis."
        )
    return out


SIZE_SUMMARY_COLUMNS = ("vendor", "fender_type", "rows", "size_measure", "rows_with_size", "smallest_size_mm",
                        "largest_size_mm", "source_ids", "accessed_on")


def size_summary(in_dir: Path) -> list[dict]:
    """Coverage of the vendor tables by vendor, then fender type: row count and the smallest
    and largest size held (height of a solid fender, diameter of a floating fender, mm).

    Sizes are catalogue dimensions, not rated values; no energy, reaction or pressure is
    written. The result is the public file ``data/fender_design/catalogue_size_summary.csv``.
    """
    groups: dict[tuple[str, str, str], dict] = {}
    for name, column, measure in (("solid_rubber.csv", "height_mm", "height"), ("floating.csv", "diameter_mm", "diameter")):
        for r in _read(in_dir / name):
            g = groups.setdefault((r["vendor"], r["fender_type"], measure),
                                  dict(rows=0, sizes=[], sources=set(), dates=set()))
            g["rows"] += 1
            g["sources"].add(r["source_id"])
            g["dates"].add(r["accessed_on"])
            if r[column].strip():
                g["sizes"].append(float(r[column]))
    out = []
    for (vendor, kind, measure), g in sorted(groups.items()):
        out.append(dict(
            vendor=vendor, fender_type=kind, rows=g["rows"], size_measure=measure, rows_with_size=len(g["sizes"]),
            smallest_size_mm=f"{min(g['sizes']):.0f}" if g["sizes"] else "",
            largest_size_mm=f"{max(g['sizes']):.0f}" if g["sizes"] else "",
            source_ids="; ".join(sorted(g["sources"])), accessed_on=max(g["dates"])))
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
    ap.add_argument("--perf-basis", choices=PERF_BASES, default=None,
                    help="keep only solid-fender rows rated on this basis in the catalogue")
    ap.add_argument("--size-summary", type=Path, default=None,
                    help="also write the coverage by vendor, type and size (no rated values) to this CSV")
    args = ap.parse_args(argv)
    if args.size_summary:
        summary = size_summary(args.in_dir)
        _write(args.size_summary, SIZE_SUMMARY_COLUMNS, summary)
        print(f"size summary rows {len(summary)}")
    rows = convert(args.in_dir)
    cat = catalogue_rows(rows, args.perf_basis)
    args.out_dir.mkdir(parents=True, exist_ok=True)
    _write(args.out_dir / "fender_database.csv", DATABASE_COLUMNS, rows)
    _write(args.out_dir / "fender_catalogue.csv", CATALOGUE_COLUMNS, cat)
    print(f"database rows {len(rows)}; catalogue rows {len(cat)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
