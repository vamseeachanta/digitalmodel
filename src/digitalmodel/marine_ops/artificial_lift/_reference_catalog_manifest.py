"""Manifest construction and staged-output validation."""

from decimal import Decimal

import yaml

from ._reference_catalog_io import sha256_file, write_csv
from ._reference_catalog_schema import (
    OUTPUT_DEFINITIONS,
    TUBING_BOOK,
    unit_for_field,
)


def emit_tree(stage, extraction_date, rows_by_file, rod_counts, simple,
              rod_path, surface_path, surface_dims, pump_path, pump_dims, root):
    sources = _build_sources(
        rows_by_file, rod_counts, simple, rod_path, surface_path,
        surface_dims, pump_path, pump_dims, root,
    )
    outputs = _write_outputs(stage, extraction_date, rows_by_file)
    manifest = _manifest_document(
        extraction_date, sources, outputs, rows_by_file
    )
    (stage / "manifest.yml").write_text(
        yaml.safe_dump(manifest, sort_keys=False, allow_unicode=True),
        encoding="utf-8",
    )
    return manifest


def _build_sources(rows_by_file, rod_counts, simple, rod_path, surface_path,
                   surface_dims, pump_path, pump_dims, root):
    sources = {
        "rod_details": source_meta(rod_path, "Sheet1", rod_counts, root),
        "surface_unit_catalog": source_meta(
            surface_path, "Surface Unit Catalog",
            _surface_counts(
                rows_by_file["surface_unit_catalog.csv"], surface_dims
            ),
            root,
        ),
        "rodpump_units": source_meta(
            pump_path, "Sheet1",
            counts(len(rows_by_file["rodpump_units.csv"]), pump_dims), root,
        ),
    }
    for name, (_, _, path, sheet, source_counts) in simple.items():
        sources[name] = source_meta(path, sheet, source_counts, root)
    tubing = root / TUBING_BOOK
    sources["tubing"] = {
        "relative_workbook": str(TUBING_BOOK), "sheet": "Tubing Stretch Table",
        "availability": "available" if tubing.is_file() else "unavailable",
        "source_rows": 0, "emitted_rows": 0,
    }
    return sources


def _write_outputs(stage, extraction_date, rows_by_file):
    outputs = {}
    for filename, rows in rows_by_file.items():
        fields = OUTPUT_DEFINITIONS[filename]
        write_csv(stage / filename, fields, rows, {
            "provenance": "previous project reference",
            "extraction_date": extraction_date,
            "schema": ",".join(fields),
            "units": "explicit in unit-suffixed fields; surface values "
            "unverified_source_unit",
        })
        outputs[filename] = {
            "sha256": sha256_file(stage / filename), "row_count": len(rows),
            "columns": fields,
            "units": {
                field: unit_for_field(filename, field) for field in fields
            },
        }
    return outputs


def _manifest_document(extraction_date, sources, outputs, rows_by_file):
    return {
        "schema_version": "1.0", "catalog_version": "v1",
        "provenance": "previous project reference",
        "extraction_date": extraction_date, "sources": sources,
        "outputs": outputs,
        "physics_validation": {
            "rod_details": physics_summary(rows_by_file["rod_details.csv"]),
            "rods_catalog": physics_summary(rows_by_file["rods_catalog.csv"]),
            "independent_falsification": independent_falsification(
                rows_by_file["rod_details.csv"]
            ),
        },
        "transformations": [
            "NFKC text normalization and outer whitespace stripping",
            "rod-guide manufacturer/model fill-down",
            "exact typed rod duplicate collapse with source-row lineage",
            "conflicting or non-keyable rod detail rows quarantined",
            "surface units retained as unverified_source_unit raw values",
            "prohibited surface manufacturer/model text redacted and lookup-excluded",
            "operational and audit columns excluded by allowlist",
        ],
        "policy_deviations": [
            "Real rod-detail normalized-key conflicts are preserved in quarantine "
            "and excluded from strict lookup instead of aborting all catalog output.",
            "Surface-unit rows with blank manufacturer or model keys remain in the "
            "raw artifact but are excluded from lookup-eligible row counts.",
        ],
    }


def counts(row_count, dimensions):
    return {
        "source_rows": row_count, "emitted_rows": row_count,
        "duplicate_rows": 0, "quarantined_rows": 0,
        "worksheet_rows": dimensions[0], "worksheet_columns": dimensions[1],
    }


def _surface_counts(rows, dimensions):
    lookup_eligible = sum(
        bool(row["manufacturer_key"] and row["model_key"]) for row in rows
    )
    blank_key_rows = sum(
        row["lookup_exclusion_reason"].startswith("blank ") for row in rows
    )
    redacted_key_rows = sum(
        row["lookup_exclusion_reason"].startswith("prohibited ") for row in rows
    )
    return {
        **counts(lookup_eligible, dimensions),
        "source_rows": len(rows),
        "lookup_eligible_rows": lookup_eligible,
        "quarantined_rows": len(rows) - lookup_eligible,
        "blank_key_rows": blank_key_rows,
        "redacted_key_rows": redacted_key_rows,
    }


def validate_stage(stage, manifest):
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


def source_meta(path, sheet, source_counts, root):
    return {
        "relative_workbook": str(path.resolve().relative_to(root.resolve())),
        "sheet": sheet, "availability": "available",
        "sha256": sha256_file(path), **source_counts,
    }


def physics_summary(rows):
    residuals = []
    for row in rows:
        try:
            residuals.append(_row_residual(row))
        except (ArithmeticError, KeyError):
            continue
    if not residuals:
        return {
            "complete_rows": 0, "minimum_relative_residual": None,
            "maximum_relative_residual": None, "mean_relative_residual": None,
        }
    return {
        "complete_rows": len(residuals),
        "minimum_relative_residual": format(min(residuals), ".12f"),
        "maximum_relative_residual": format(max(residuals), ".12f"),
        "mean_relative_residual": format(
            sum(residuals) / len(residuals), ".12f"
        ),
    }


def _row_residual(row):
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
    return abs(computed - velocity) / velocity


def independent_falsification(rows):
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
