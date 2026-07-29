#!/usr/bin/env python3
"""Extract safe artificial-lift equipment catalogs into packaged resources."""

from __future__ import annotations

import argparse
from collections import defaultdict
from datetime import date
from decimal import Decimal
import os
from pathlib import Path
import re
import shutil
import tempfile

import yaml

from digitalmodel.marine_ops.artificial_lift._reference_catalog_io import (
    decimal_text as _decimal_text,
    is_numeric as _is_numeric,
    read_rows as _read_rows,
    scan_safe as _scan_safe,
    sha256_file,
    source_path as _source_path,
    text as _text,
    write_csv as _write_csv,
)
from digitalmodel.marine_ops.artificial_lift._reference_catalog_schema import (
    CATALOG_BOOK,
    CONNECTION_BOOK,
    COUPLING_BOOK,
    OUTPUT_DEFINITIONS,
    ROD_DETAIL_BOOK,
    RODPUMP_BOOK,
    RODPUMP_MAP,
    SURFACE_FIELDS,
    SURFACE_MAP,
    TUBING_BOOK,
)


def _parse_rod_label(value) -> tuple[str, str] | None:
    label = _text(value).strip('"')
    match = re.fullmatch(r"(.*?)\s*-\s*(\d+(?:\.\d+)?)", label)
    if not match:
        return None
    grade = re.sub(r"\s+", " ", match.group(1)).strip().upper()
    diameter = _decimal_text(match.group(2))
    if not grade or Decimal(diameter).as_tuple().exponent < -3:
        return None
    return grade, diameter


def _rod_detail_rows(path: Path):
    records, dimensions = _read_rows(path, "Sheet1")
    groups = defaultdict(list)
    quarantine = []
    for source_row, record in records:
        key = _parse_rod_label(record["Rod Grade"])
        values = tuple(
            _decimal_text(record[name])
            for name in (
                "Rod Area", "Unit Weight", "Modulus of Elast",
                "Spd of Sound", "Tensil Strength",
            )
        )
        if key is None:
            quarantine.append(_quarantine_row(source_row, record, "non-keyable label"))
        else:
            groups[key].append((source_row, record["Rod Grade"], values))
    emitted, duplicates = [], 0
    for (grade, diameter), values in sorted(groups.items()):
        distinct = {value[2] for value in values}
        if len(distinct) != 1:
            quarantine.extend(
                _quarantine_row(row, {"Rod Grade": label}, "conflicting normalized key")
                for row, label, _ in values
            )
            continue
        area, weight, modulus_mpsi, velocity_kft_s, tensile = values[0][2]
        modulus_psi = Decimal(modulus_mpsi) * Decimal("1000000")
        density = Decimal(weight) / (Decimal(area) * Decimal("12"))
        velocity = (modulus_psi * Decimal("386.0886") / density).sqrt() / 12
        emitted.append({
            "grade": grade, "diameter_in": diameter, "area_in2": area,
            "unit_weight_lbf_ft": weight, "modulus_psi": _decimal_text(modulus_psi),
            "catalog_sonic_velocity_ft_s": _decimal_text(Decimal(velocity_kft_s) * 1000),
            "weight_derived_velocity_ft_s": format(velocity, ".12f"),
            "tensile_strength_psi": tensile,
            "raw_sonic_velocity_kft_s": velocity_kft_s,
            "source_rows": ";".join(str(value[0]) for value in values),
            "raw_labels": ";".join(_text(value[1]) for value in values),
        })
        duplicates += len(values) - 1
    counts = {
        "source_rows": len(records), "emitted_rows": len(emitted),
        "duplicate_rows": duplicates, "quarantined_rows": len(quarantine),
        "worksheet_rows": dimensions[0], "worksheet_columns": dimensions[1],
    }
    return emitted, quarantine, counts


def _quarantine_row(source_row: int, record: dict, reason: str) -> dict:
    return {
        "source_row": source_row,
        "raw_label": _text(record.get("Rod Grade")),
        "reason": reason,
    }


def _surface_rows(path: Path, sheet: str, catalog: str, mapping: dict):
    records, dimensions = _read_rows(path, sheet)
    rows = []
    for source_row, record in records:
        row = {"source_catalog": catalog, "source_row": source_row}
        for field in SURFACE_FIELDS:
            if field in {"source_catalog", "source_row"}:
                continue
            value = record.get(mapping.get(field, ""))
            if field in {"manufacturer_key", "model_key", "geometry_code"}:
                row[field] = _text(value)
            else:
                row[field] = _decimal_text(value)
        rows.append(row)
    return rows, dimensions


def _simple_catalogs(root: Path):
    catalog_path = _source_path(root, CATALOG_BOOK)
    coupling_path = _source_path(root, COUPLING_BOOK)
    connection_path = _source_path(root, CONNECTION_BOOK)
    rod_records, rod_dims = _read_rows(catalog_path, "Rods Catalog", first_row=3)
    rods = [record for record in rod_records if _is_numeric(record[1]["."])]
    rejected_rods = len(rod_records) - len(rods)
    rods = [
        {
            "source_row": row_number, "catalog_id": _decimal_text(row["."]),
            "description": _text(row["DESC"]), "tensile_strength_psi": _decimal_text(row["TENSILE"]),
            "area_in2": _decimal_text(row["AREA"]), "modulus_mpsi": _decimal_text(row["MOE"]),
            "velocity_kft_s": _decimal_text(row["VELOCITY"]),
            "unit_weight_lbf_ft": _decimal_text(row["DENSITY"]),
            "elastorq_raw": _decimal_text(row["ELASTORQ"]),
        }
        for row_number, row in rods
    ]
    guides, guide_dims = _guide_rows(catalog_path)
    couplings, coupling_dims = _coupling_rows(coupling_path)
    connections, connection_dims = _connection_rows(connection_path)
    return {
        "rods_catalog": (
            rods, rod_dims, catalog_path, "Rods Catalog",
            {
                **_counts(len(rods), rod_dims),
                "source_rows": len(rod_records),
                "rejected_rows": rejected_rods,
            },
        ),
        "rod_guides": (
            guides, guide_dims, catalog_path, "Rods Guide Catalog",
            _counts(len(guides), guide_dims),
        ),
        "couplings": (
            couplings, coupling_dims, coupling_path, "Sheet1",
            _counts(len(couplings), coupling_dims),
        ),
        "rod_connections": (
            connections, connection_dims, connection_path, "Rod ODs",
            _counts(len(connections), connection_dims),
        ),
    }


def _guide_rows(path: Path):
    records, dimensions = _read_rows(path, "Rods Guide Catalog")
    rows, manufacturer, model = [], "", ""
    for source_row, record in records:
        manufacturer = _text(record["Manufacturer"]) or manufacturer
        model = _text(record["Model/Type"]) or model
        rows.append({
            "source_row": source_row, "manufacturer": manufacturer,
            "model_type": model, "material": _text(record["Material"]),
        })
    return rows, dimensions


def _coupling_rows(path: Path):
    records, dimensions = _read_rows(path, "Sheet1")
    columns = {
        "rod_diameter_in": "Rod Diameter (in.)",
        "manufacturer": "Coupling Manufacturer", "size": "Coupling Size",
        "coupling_type": "Coupling Type",
        "coupling_diameter_in": "Coupling Diameter (in.)",
        "coupling_length_in": "Coupling Length (in.)",
        "tensile_strength_psi": "Coupling Tensile (psi)",
        "friction_coefficient": "Friction Coefficient",
    }
    rows = []
    for source_row, record in records:
        row = {"source_row": source_row}
        for field, source in columns.items():
            value = record[source]
            row[field] = _text(value) if field in {
                "manufacturer", "size", "coupling_type"
            } else _decimal_text(value)
        rows.append(row)
    return rows, dimensions


def _connection_rows(path: Path):
    records, dimensions = _read_rows(path, "Rod ODs")
    rows = [{
        "source_row": source_row, "raw_rod_od": _text(record["Rod OD"]),
        "rod_od_in": _decimal_text(record["Rod OD"]),
        "raw_connection_size": _text(record["Rod Connection Size"]),
        "connection_size_in": "",
        "disposition": "quarantined" if record["Rod Connection Size"] else "unmapped",
    } for source_row, record in records]
    return rows, dimensions


def extract_catalogs(source_root, output_dir, extraction_date):
    root, output = Path(source_root), Path(output_dir)
    if output.exists():
        raise FileExistsError(f"catalog version already exists: {output}")
    rod_path = _source_path(root, ROD_DETAIL_BOOK)
    rod_rows, quarantine, rod_counts = _rod_detail_rows(rod_path)
    surface_path = _source_path(root, CATALOG_BOOK)
    pump_path = _source_path(root, RODPUMP_BOOK)
    surface, surface_dims = _surface_rows(
        surface_path, "Surface Unit Catalog", "surface_unit_catalog", SURFACE_MAP
    )
    pumps, pump_dims = _surface_rows(
        pump_path, "Sheet1", "rodpump_units", RODPUMP_MAP
    )
    simple = _simple_catalogs(root)
    rows_by_file = {
        "rod_details.csv": rod_rows, "rod_details_quarantine.csv": quarantine,
        "surface_unit_catalog.csv": surface, "rodpump_units.csv": pumps,
        **{f"{name}.csv": values[0] for name, values in simple.items()},
    }
    for name, rows in rows_by_file.items():
        _scan_safe(rows, name)
    output.parent.mkdir(parents=True, exist_ok=True)
    stage = Path(tempfile.mkdtemp(prefix=f".{output.name}-", dir=output.parent))
    try:
        manifest = _emit_tree(
            stage, extraction_date, rows_by_file, rod_counts, simple,
            rod_path, surface_path, surface_dims, pump_path, pump_dims, root,
        )
        os.replace(stage, output)
    except Exception:
        shutil.rmtree(stage, ignore_errors=True)
        raise
    return manifest


def _emit_tree(stage, extraction_date, rows_by_file, rod_counts, simple,
               rod_path, surface_path, surface_dims, pump_path, pump_dims, root):
    sources = {
        "rod_details": _source_meta(rod_path, "Sheet1", rod_counts),
        "surface_unit_catalog": _source_meta(
            surface_path, "Surface Unit Catalog",
            _counts(len(rows_by_file["surface_unit_catalog.csv"]), surface_dims),
        ),
        "rodpump_units": _source_meta(
            pump_path, "Sheet1",
            _counts(len(rows_by_file["rodpump_units.csv"]), pump_dims),
        ),
    }
    for name, (rows, dims, path, sheet, counts) in simple.items():
        sources[name] = _source_meta(path, sheet, counts)
    tubing = root / TUBING_BOOK
    sources["tubing"] = {
        "relative_workbook": str(TUBING_BOOK), "sheet": "Tubing Stretch Table",
        "availability": "available" if tubing.is_file() else "unavailable",
        "source_rows": 0, "emitted_rows": 0,
    }
    outputs, fields = {}, OUTPUT_DEFINITIONS
    for filename, rows in rows_by_file.items():
        _write_csv(stage / filename, fields[filename], rows, {
            "provenance": "previous project reference",
            "extraction_date": extraction_date,
            "schema": ",".join(fields[filename]),
            "units": "explicit in unit-suffixed fields; surface values unverified_source_unit",
        })
        outputs[filename] = {
            "sha256": sha256_file(stage / filename), "row_count": len(rows),
            "columns": fields[filename],
        }
    manifest = {
        "schema_version": "1.0", "catalog_version": "v1",
        "provenance": "previous project reference",
        "extraction_date": extraction_date, "sources": sources,
        "outputs": outputs,
        "transformations": [
            "NFKC text normalization and outer whitespace stripping",
            "rod-guide manufacturer/model fill-down",
            "exact typed rod duplicate collapse with source-row lineage",
            "conflicting or non-keyable rod detail rows quarantined",
            "surface units retained as unverified_source_unit raw values",
            "operational and audit columns excluded by allowlist",
        ],
    }
    (stage / "manifest.yml").write_text(
        yaml.safe_dump(manifest, sort_keys=False, allow_unicode=True),
        encoding="utf-8",
    )
    return manifest


def _counts(row_count, dimensions):
    return {
        "source_rows": row_count, "emitted_rows": row_count,
        "duplicate_rows": 0, "quarantined_rows": 0,
        "worksheet_rows": dimensions[0], "worksheet_columns": dimensions[1],
    }


def _source_meta(path, sheet, counts):
    return {
        "relative_workbook": str(path.name), "sheet": sheet,
        "availability": "available", "sha256": sha256_file(path), **counts,
    }


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-root", required=True, type=Path)
    parser.add_argument("--output-dir", type=Path, default=Path(
        "src/digitalmodel/marine_ops/artificial_lift/reference_data/v1"
    ))
    parser.add_argument("--extraction-date", default=date.today().isoformat())
    args = parser.parse_args(argv)
    manifest = extract_catalogs(args.source_root, args.output_dir, args.extraction_date)
    for name, source in manifest["sources"].items():
        print(
            f"{name}: source={source.get('source_rows', 0)} "
            f"emitted={source.get('emitted_rows', 0)} "
            f"duplicate={source.get('duplicate_rows', 0)} "
            f"quarantined={source.get('quarantined_rows', 0)} "
            f"availability={source['availability']}"
        )


if __name__ == "__main__":
    main()
