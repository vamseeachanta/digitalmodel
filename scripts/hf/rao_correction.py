# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Stage and verify offline RAO dataset corrections; never upload or run solvers.

Policy source: issue #3122 (2026), Decision items 1-3; linked #3121 and #1548.
No numerical scaling is validly inferred from half-model results. Original
values remain intact until an independently supplied replacement passes the
evidence controls. A recorded PASS is evidence supplied by the operator, not
authentication of a reviewer or independent engineering qualification.
"""

from __future__ import annotations

import argparse
from datetime import date
import hashlib
import json
import math
from pathlib import Path
import shutil

import pyarrow as pa
import pyarrow.parquet as pq

# Exact warning and required gates: issue #3122 (2026), Decision items 1-3.
WARNING = "under correction: solved from a half-model mesh; do not use"
GATES = ("buoyancy", "mesh", "solver_agreement", "claims_verifier")
MANIFEST = "rao-correction.json"
STATUS = "correction_status"
NOTICE = "correction_warning"
# Column roles: public dataset card schema 1.1.0 (inspected 2026-10-21).
SAMPLE_COLUMNS = {
    "vessel_id",
    "solver",
    "damping_zeta_pct",
    "dof",
    "freq_rad_s",
    "heading_deg",
}
RAO_COLUMNS = {"magnitude", "phase"}


def digest(path: Path) -> str:
    """Hash retained bytes, with no normalization of numerical observations."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _json(path: Path) -> dict:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
        if not isinstance(value, dict):
            raise ValueError("JSON object required")
        return value
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError("Unreadable JSON evidence") from error


def _files(root: Path) -> dict[str, str]:
    if not root.is_dir() or root.is_symlink():
        raise ValueError("A regular source directory is required")
    files = {}
    for path in sorted(root.rglob("*")):
        if path.is_symlink():
            raise ValueError("Source symlinks are not permitted")
        if path.is_file() and path.relative_to(root).as_posix() != MANIFEST:
            files[path.relative_to(root).as_posix()] = digest(path)
    return files


def _destination(source: Path, target: Path) -> None:
    if (
        target.exists()
        or target.is_symlink()
        or target.resolve().is_relative_to(source.resolve())
    ):
        raise ValueError("Destination must be new and outside the source")


def _affected(policy: dict) -> set[str]:
    if set(policy) != {"installation", "parametric"}:
        raise ValueError("Explicit installation and parametric mappings are required")
    seen = set()
    for values in policy.values():
        if not isinstance(values, list) or not values:
            raise ValueError("Each entry requires a nonempty identifier list")
        for value in values:
            if not isinstance(value, str) or not value.strip() or value in seen:
                raise ValueError("Identifiers must be nonempty and unambiguous")
            seen.add(value)
    return seen


def _table(path: Path) -> pa.Table:
    # ParquetFile avoids interpreting parent directory names as partition keys.
    return pq.ParquetFile(path).read()


def _annotate(
    table: pa.Table, affected: set[str], policy: dict, status: str = "UNDER_CORRECTION"
) -> pa.Table:
    ids = table["vessel_id"].to_pylist()
    warnings = [WARNING if name in affected else None for name in ids]
    states = [status if name in affected else None for name in ids]
    for column, values in ((STATUS, states), (NOTICE, warnings)):
        if column in table.column_names:
            table = table.set_column(
                table.column_names.index(column), column, pa.array(values, pa.string())
            )
        else:
            table = table.append_column(column, pa.array(values, pa.string()))
    metadata = dict(table.schema.metadata or {})
    metadata[b"rao_correction"] = json.dumps(
        {"affected": policy, "status": status, "warning": WARNING}, sort_keys=True
    ).encode()
    return table.replace_schema_metadata(metadata)


def _card(root: Path, policy: dict, issued: str, mode: str) -> str:
    date.fromisoformat(issued)
    original = (root / "README.md").read_text(encoding="utf-8")
    heading = "Half-model warning" if mode == "flag" else "Full-body RAO replacement"
    notice = (
        WARNING
        if mode == "flag"
        else (
            "RAO values replaced using supplied gate evidence. Other table warnings "
            "and prior reliability flags remain in effect."
        )
    )
    return (
        original
        + f"\n\n## {heading}\n\n{notice}\n\n"
        + "Affected identifier mapping: "
        + json.dumps(policy, sort_keys=True)
        + "\n\nEngineering qualification and external claims review remain separate "
        "from byte-integrity verification. No upload is performed.\n\n"
        + "### Revision history\n\n| Date | Change and reason |\n|---|---|\n"
        + f"| {issued} | {heading}; issue #3122. |\n"
    )


def _write(
    source: Path,
    target: Path,
    tables: dict[str, pa.Table],
    card: str,
    record: dict,
    original: dict,
) -> dict:
    # Validate before creating output; an I/O failure may leave a partial bundle.
    # It has no valid manifest and must never be published or reused.
    _destination(source, target)
    if _files(source) != original:
        raise ValueError("Source changed during preparation")
    shutil.copytree(source, target)
    for name, table in tables.items():
        pq.write_table(table, target / name)
    (target / "README.md").write_text(card, encoding="utf-8")
    record.update(
        {
            "input_sha256": original,
            "output_sha256": _files(target),
            "publication": "NOT_EVALUATED",
        }
    )
    (target / MANIFEST).write_text(
        json.dumps(record, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    return record


def flag(source: Path, target: Path, affected: dict, issued: str) -> dict:
    """Annotate both affected entries without changing or omitting observations."""
    _destination(source, target)
    ids = _affected(affected)
    original = _files(source)
    if (source / MANIFEST).exists():
        raise ValueError("Source is already staged; use the original snapshot")
    tables, counts = {}, {}
    if "rao.parquet" not in original:
        raise ValueError("rao.parquet is required")
    for name in original:
        if not name.endswith(".parquet"):
            continue
        table = _table(source / name)
        if "vessel_id" not in table.column_names:
            continue
        if {STATUS, NOTICE} & set(table.column_names):
            raise ValueError(
                "Existing correction columns require explicit reconciliation"
            )
        names = table["vessel_id"].to_pylist()
        if name == "rao.parquet" and not ids.issubset(set(names)):
            raise ValueError(
                "Every supplied affected identifier must occur in the RAO table"
            )
        tables[name] = _annotate(table, ids, affected)
        counts[name] = {
            key: sum(value in aliases for value in names)
            for key, aliases in affected.items()
        }
    if "rao.parquet" not in tables:
        raise ValueError("RAO table requires vessel_id")
    return _write(
        source,
        target,
        tables,
        _card(source, affected, issued, "flag"),
        {"mode": "flag", "affected": affected, "counts": counts},
        original,
    )


def verify(source: Path, bundle: Path) -> dict:
    """Check the complete source and staged inventories against recorded hashes."""
    record = _json(bundle / MANIFEST)
    if record.get("mode") not in {"flag", "correct"}:
        raise ValueError("Unknown correction mode")
    if record.get("input_sha256") != _files(source):
        raise ValueError("Source inventory or digest mismatch")
    if record.get("output_sha256") != _files(bundle):
        raise ValueError("Bundle inventory or digest mismatch")
    # A correction's input is a flagged bundle; bind its manifest too.
    if record["mode"] == "correct" and record.get("flagged_manifest_sha256") != digest(
        source / MANIFEST
    ):
        raise ValueError("Flagged manifest digest mismatch")
    return record


def _gates(candidate: Path, report: Path) -> dict:
    document = _json(report)
    if document.get("candidate_sha256") != digest(candidate):
        raise ValueError("Candidate digest mismatch")
    checks = document.get("checks")
    if not isinstance(checks, dict):
        raise ValueError("Gate checks must be a JSON object")
    evidence = {}
    for gate in GATES:
        check = checks.get(gate, {})
        if not isinstance(check, dict) or check.get("status") != "PASS":
            raise ValueError(f"{gate}: PASS evidence required; otherwise NOT_EVALUATED")
        filename = check.get("file")
        if not isinstance(filename, str):
            raise ValueError(f"{gate}: relative evidence file required")
        relative = Path(filename)
        if relative.is_absolute() or ".." in relative.parts or not relative.parts:
            raise ValueError(f"{gate}: relative evidence file required")
        path = report.parent / relative
        if (
            path.is_symlink()
            or not path.resolve().is_relative_to(report.parent.resolve())
            or not path.is_file()
        ):
            raise ValueError(f"{gate}: evidence file is unavailable")
        if check.get("sha256") != digest(path):
            raise ValueError(f"{gate}: evidence digest mismatch")
        evidence[gate] = check["sha256"]
    return evidence


def _key(row: dict, columns: list[str]) -> tuple:
    values = tuple(row[column] for column in columns)
    if any(
        value is None or isinstance(value, float) and not math.isfinite(value)
        for value in values
    ):
        raise ValueError("Non-finite or null sample key")
    return values


def correct(
    original: Path,
    flagged: Path,
    target: Path,
    candidate: Path,
    report: Path,
    keys: list[str],
    values: list[str],
    issued: str,
) -> dict:
    """Replace selected numeric RAO fields after all supplied gate records pass."""
    record = verify(original, flagged)
    if record["mode"] != "flag":
        raise ValueError("Replacement requires a verified warning bundle")
    _destination(flagged, target)
    evidence = _gates(candidate, report)
    affected = _affected(record["affected"])
    baseline = _table(original / "rao.parquet")
    supplied = _table(candidate)
    if not supplied.schema.equals(baseline.schema, check_metadata=False):
        raise ValueError("Replacement schema must match the original RAO schema")
    fields = set(baseline.column_names)
    if (
        not keys
        or not values
        or "vessel_id" not in keys
        or set(keys) & set(values)
        or len(set(keys)) != len(keys)
        or len(set(values)) != len(values)
        or not set(keys + values).issubset(fields)
    ):
        raise ValueError(
            "Explicit disjoint unique sample keys and numeric value columns are required"
        )
    if not (SAMPLE_COLUMNS & fields).issubset(keys) or set(values) != (
        RAO_COLUMNS & fields
    ):
        raise ValueError(
            "Keys must include every sample coordinate; replace all available RAO value columns"
        )
    if any(
        not (
            pa.types.is_floating(baseline.schema.field(name).type)
            or pa.types.is_integer(baseline.schema.field(name).type)
        )
        for name in values
    ):
        raise ValueError("Replacement columns must be numeric")
    rows = baseline.to_pylist()
    expected = {_key(row, keys): row for row in rows if row["vessel_id"] in affected}
    if len(expected) != sum(row["vessel_id"] in affected for row in rows):
        raise ValueError("Original sample keys are not unique")
    replacements = {}
    for row in supplied.to_pylist():
        key = _key(row, keys)
        if key not in expected or key in replacements:
            raise ValueError("Unexpected or duplicate replacement key")
        if any(row[name] != expected[key][name] for name in fields - set(values)):
            raise ValueError("Replacement changed an undeclared field")
        if any(row[name] is None or not math.isfinite(row[name]) for name in values):
            raise ValueError("Replacement values must be finite and non-null")
        replacements[key] = row
    if replacements.keys() != expected.keys():
        raise ValueError("Incomplete replacement grid")
    updated = [
        replacements[_key(row, keys)] if row["vessel_id"] in affected else row
        for row in rows
    ]
    table = pa.Table.from_pylist(updated, schema=baseline.schema)
    table = _annotate(table, affected, record["affected"], "CORRECTED")
    # The issue requires a do-not-use warning until qualification, not after it.
    table = table.set_column(
        table.column_names.index(NOTICE),
        NOTICE,
        pa.array([None] * table.num_rows, pa.string()),
    )
    metadata = dict(table.schema.metadata or {})
    correction = json.loads(metadata[b"rao_correction"])
    correction["warning"] = None
    metadata[b"rao_correction"] = json.dumps(correction, sort_keys=True).encode()
    table = table.replace_schema_metadata(metadata)
    return _write(
        flagged,
        target,
        {"rao.parquet": table},
        _card(flagged, record["affected"], issued, "correct"),
        {
            "mode": "correct",
            "affected": record["affected"],
            "candidate_sha256": digest(candidate),
            "gate_evidence_sha256": evidence,
            "gate_report_sha256": digest(report),
            "flagged_manifest_sha256": digest(flagged / MANIFEST),
            "key_columns": keys,
            "value_columns": values,
        },
        _files(flagged),
    )


def release_check(source: Path, bundle: Path, claims_report: Path) -> dict:
    """Require an external claims-verifier PASS bound to the final bundle manifest.

    This records a supplied receipt; it does not authenticate the verifier.
    Publishing still requires maintainer review of that evidence and authority.
    """
    verify(source, bundle)
    report = _json(claims_report)
    if (
        report.get("status") != "PASS"
        or report.get("bundle_sha256") != digest(bundle / MANIFEST)
        or not isinstance(report.get("evidence_ref"), str)
        or not report["evidence_ref"].strip()
    ):
        raise ValueError(
            "Bound external claims-verifier PASS required before publication"
        )
    return {"claims_verifier": "PASS_RECORDED", "publication": "NOT_EVALUATED"}


def main() -> None:
    """Run offline staging commands with explicit paths and required mappings."""
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    for name in ("flag", "verify", "correct", "release-check"):
        command = commands.add_parser(name)
        command.add_argument("source", type=Path)
        command.add_argument("bundle", type=Path)
        if name in {"flag", "correct"}:
            command.add_argument("--date", required=True)
        if name == "flag":
            command.add_argument("--affected", required=True, type=Path)
        if name == "correct":
            command.add_argument("--output", required=True, type=Path)
            command.add_argument("--candidate", required=True, type=Path)
            command.add_argument("--gates", required=True, type=Path)
            command.add_argument("--keys", nargs="+", required=True)
            command.add_argument("--values", nargs="+", required=True)
        if name == "release-check":
            command.add_argument("--claims-report", required=True, type=Path)
    args = parser.parse_args()
    try:
        if args.command == "flag":
            result = flag(args.source, args.bundle, _json(args.affected), args.date)
        elif args.command == "correct":
            result = correct(
                args.source,
                args.bundle,
                args.output,
                args.candidate,
                args.gates,
                args.keys,
                args.values,
                args.date,
            )
        elif args.command == "release-check":
            result = release_check(args.source, args.bundle, args.claims_report)
        else:
            result = verify(args.source, args.bundle)
    except (ValueError, OSError, pa.ArrowException) as error:
        parser.exit(1, f"Correction refused: {error}\n")
    print(
        json.dumps(
            {
                "mode": result.get("mode"),
                "publication": "NOT_EVALUATED",
                "claims_verifier": result.get("claims_verifier", "NOT_EVALUATED"),
            },
            sort_keys=True,
        )
    )


if __name__ == "__main__":
    main()
