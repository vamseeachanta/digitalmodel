#!/usr/bin/env python3
"""Extract safe artificial-lift equipment catalogs into packaged resources."""

from __future__ import annotations

import argparse
from datetime import date
import os
from pathlib import Path
import shutil
import sys
import tempfile
WORKTREE_SRC = Path(__file__).resolve().parents[2] / "src"
if str(WORKTREE_SRC) not in sys.path:
    sys.path.insert(0, str(WORKTREE_SRC))

from digitalmodel.marine_ops.artificial_lift._reference_catalog_io import (  # noqa: E402
    decimal_text as _decimal_text,
    output_lock,
    read_rows as _read_rows,
    scan_safe as _scan_safe,
    sha256_file,  # noqa: F401 - exposed for extraction integrity checks
    source_path as _source_path,
    text as _text,
)
from digitalmodel.marine_ops.artificial_lift._reference_catalog_manifest import (  # noqa: E402
    counts as _counts,
    emit_tree as _emit_tree,
    validate_stage as _validate_stage,
)
from digitalmodel.marine_ops.artificial_lift._reference_catalog_schema import (  # noqa: E402
    CATALOG_BOOK,
    CONNECTION_BOOK,
    COUPLING_BOOK,
    ROD_DETAIL_BOOK,
    RODPUMP_BOOK,
    RODPUMP_MAP,
    SURFACE_FIELDS,
    SURFACE_MAP,
)
from digitalmodel.marine_ops.artificial_lift._reference_catalog_transform import (  # noqa: E402
    connection_lookup_rows as _connection_lookup_rows,
    connection_rows as _connection_rows,
    rod_detail_rows as _rod_detail_rows,
    rods_catalog_rows as _rods_catalog_rows,
)


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
    rods, rod_dims, rod_counts = _rods_catalog_rows(catalog_path)
    guides, guide_dims = _guide_rows(catalog_path)
    couplings, coupling_dims = _coupling_rows(coupling_path)
    connections, connection_dims = _connection_rows(connection_path)
    lookup, lookup_dims = _connection_lookup_rows(connection_path, couplings)
    connection_quarantine = sum(
        row["disposition"] == "quarantined" for row in connections
    )
    lookup_quarantine = sum(
        row["disposition"] == "quarantined" for row in lookup
    )
    return {
        "rods_catalog": (
            rods, rod_dims, catalog_path, "Rods Catalog",
            rod_counts,
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
            {
                **_counts(len(connections), connection_dims),
                "quarantined_rows": connection_quarantine,
            },
        ),
        "rod_connection_lookup": (
            lookup, lookup_dims, connection_path, "Look-up",
            {
                **_counts(len(lookup), lookup_dims),
                "emitted_rows": len(lookup) - lookup_quarantine,
                "verified_rows": len(lookup) - lookup_quarantine,
                "quarantined_rows": lookup_quarantine,
            },
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


def extract_catalogs(source_root, output_dir, extraction_date):
    _validate_extraction_date(extraction_date)
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
        _validate_stage(stage, manifest)
        with output_lock(output):
            if output.exists():
                raise FileExistsError(f"catalog version already exists: {output}")
            os.rename(stage, output)
    except Exception:
        shutil.rmtree(stage, ignore_errors=True)
        raise
    return manifest


def _validate_extraction_date(value):
    if not isinstance(value, str):
        raise TypeError("extraction date must be an ISO date string")
    try:
        parsed = date.fromisoformat(value)
    except ValueError as exc:
        raise ValueError("extraction date must use YYYY-MM-DD") from exc
    if parsed.isoformat() != value:
        raise ValueError("extraction date must use YYYY-MM-DD")


def check_catalogs(source_root, output_dir, extraction_date):
    output = Path(output_dir)
    if not output.is_dir():
        raise FileNotFoundError(output)
    temporary = Path(tempfile.mkdtemp(prefix=".catalog-check-", dir=output.parent))
    generated = temporary / output.name
    try:
        manifest = extract_catalogs(source_root, generated, extraction_date)
        expected = {
            path.name: path.read_bytes() for path in output.iterdir() if path.is_file()
        }
        actual = {
            path.name: path.read_bytes() for path in generated.iterdir() if path.is_file()
        }
        if expected != actual:
            raise ValueError("packaged catalog differs from deterministic extraction")
        return manifest
    finally:
        shutil.rmtree(temporary, ignore_errors=True)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-root", required=True, type=Path)
    parser.add_argument("--output-dir", type=Path, default=Path(
        "src/digitalmodel/marine_ops/artificial_lift/reference_data/v1"
    ))
    parser.add_argument("--extraction-date", required=True)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args(argv)
    operation = check_catalogs if args.check else extract_catalogs
    manifest = operation(args.source_root, args.output_dir, args.extraction_date)
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
