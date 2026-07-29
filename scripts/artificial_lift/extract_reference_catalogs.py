#!/usr/bin/env python3
"""Extract safe artificial-lift equipment catalogs into packaged resources."""

from __future__ import annotations

import argparse
from decimal import Decimal
import os
from pathlib import Path
import shutil
import sys
import tempfile
import yaml
WORKTREE_SRC = Path(__file__).resolve().parents[2] / "src"
if str(WORKTREE_SRC) not in sys.path:
    sys.path.insert(0, str(WORKTREE_SRC))

from digitalmodel.marine_ops.artificial_lift._reference_catalog_io import (  # noqa: E402
    decimal_text as _decimal_text,
    is_numeric as _is_numeric,
    output_lock,
    read_rows as _read_rows,
    scan_safe as _scan_safe,
    sha256_file,
    source_path as _source_path,
    text as _text,
    write_csv as _write_csv,
)
from digitalmodel.marine_ops.artificial_lift._reference_catalog_schema import (  # noqa: E402
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
    unit_for_field,
)
from digitalmodel.marine_ops.artificial_lift._reference_catalog_transform import (  # noqa: E402
    connection_lookup_rows as _connection_lookup_rows,
    connection_rows as _connection_rows,
    rod_detail_rows as _rod_detail_rows,
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
            {
                **_counts(len(connections), connection_dims),
                "quarantined_rows": connection_quarantine,
            },
        ),
        "rod_connection_lookup": (
            lookup, lookup_dims, connection_path, "Look-up",
            {
                **_counts(len(lookup), lookup_dims),
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


def _emit_tree(stage, extraction_date, rows_by_file, rod_counts, simple,
               rod_path, surface_path, surface_dims, pump_path, pump_dims, root):
    sources = {
        "rod_details": _source_meta(rod_path, "Sheet1", rod_counts, root),
        "surface_unit_catalog": _source_meta(
            surface_path, "Surface Unit Catalog",
            _counts(len(rows_by_file["surface_unit_catalog.csv"]), surface_dims),
            root,
        ),
        "rodpump_units": _source_meta(
            pump_path, "Sheet1",
            _counts(len(rows_by_file["rodpump_units.csv"]), pump_dims),
            root,
        ),
    }
    for name, (rows, dims, path, sheet, counts) in simple.items():
        sources[name] = _source_meta(path, sheet, counts, root)
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
            "units": {
                field: unit_for_field(filename, field) for field in fields[filename]
            },
        }
    manifest = {
        "schema_version": "1.0", "catalog_version": "v1",
        "provenance": "previous project reference",
        "extraction_date": extraction_date, "sources": sources,
        "outputs": outputs,
        "physics_validation": {
            "rod_details": _physics_summary(rows_by_file["rod_details.csv"]),
            "rods_catalog": _physics_summary(rows_by_file["rods_catalog.csv"]),
            "independent_falsification": _independent_falsification(
                rows_by_file["rod_details.csv"]
            ),
        },
        "transformations": [
            "NFKC text normalization and outer whitespace stripping",
            "rod-guide manufacturer/model fill-down",
            "exact typed rod duplicate collapse with source-row lineage",
            "conflicting or non-keyable rod detail rows quarantined",
            "surface units retained as unverified_source_unit raw values",
            "operational and audit columns excluded by allowlist",
        ],
        "policy_deviations": [
            "Real rod-detail normalized-key conflicts are preserved in quarantine "
            "and excluded from strict lookup instead of aborting all catalog output."
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


def _validate_stage(stage, manifest):
    for filename, metadata in manifest["outputs"].items():
        path = stage / filename
        if sha256_file(path) != metadata["sha256"]:
            raise ValueError(f"staged digest mismatch: {filename}")
        data_lines = [
            line for line in path.read_text(encoding="utf-8").splitlines()
            if line and not line.startswith("#")
        ]
        if len(data_lines) - 1 != metadata["row_count"]:
            raise ValueError(f"staged row-count mismatch: {filename}")
        if data_lines[0].split(",") != metadata["columns"]:
            raise ValueError(f"staged schema mismatch: {filename}")


def _source_meta(path, sheet, counts, root):
    return {
        "relative_workbook": str(path.resolve().relative_to(root.resolve())),
        "sheet": sheet,
        "availability": "available", "sha256": sha256_file(path), **counts,
    }


def _physics_summary(rows):
    residuals = []
    for row in rows:
        try:
            area = Decimal(row["area_in2"])
            weight = Decimal(row["unit_weight_lbf_ft"])
            modulus = Decimal(row.get("modulus_psi") or row["modulus_mpsi"])
            velocity = Decimal(
                row.get("catalog_sonic_velocity_ft_s") or row["velocity_kft_s"]
            )
            if "modulus_mpsi" in row:
                modulus *= Decimal("1000000")
                velocity *= Decimal("1000")
            density = weight / (area * 12)
            computed = (modulus * Decimal("386.0886") / density).sqrt() / 12
            residuals.append(abs(computed - velocity) / velocity)
        except (ArithmeticError, KeyError):
            continue
    if not residuals:
        return {
            "complete_rows": 0,
            "minimum_relative_residual": None,
            "maximum_relative_residual": None,
            "mean_relative_residual": None,
        }
    return {
        "complete_rows": len(residuals),
        "minimum_relative_residual": format(min(residuals), ".12f"),
        "maximum_relative_residual": format(max(residuals), ".12f"),
        "mean_relative_residual": format(
            sum(residuals) / len(residuals), ".12f"
        ),
    }


def _independent_falsification(rows):
    row = next((item for item in rows if item["grade"] == "97"
                and item["diameter_in"] == "0.875"), None)
    if row is None:
        return {"availability": "unavailable"}
    computed = Decimal(row["weight_derived_velocity_ft_s"])
    target = Decimal("16300")
    return {
        "grade": "97", "diameter_in": "0.875",
        "target_ft_s": format(target, "f"),
        "computed_ft_s": format(computed, ".12f"),
        "relative_difference": format(abs(computed - target) / target, ".12f"),
    }


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
