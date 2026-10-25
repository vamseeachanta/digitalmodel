# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Validate offline scour-table evidence and emit metadata-only gap reports.

The column contract is proposed, not a transcription of uninspected headers.
Source mode checks evidence structure only. Human rights review, actual header
and unit interpretation, and independent source inspection remain external gates;
self-declared PERMITTED and MATCH values cannot authenticate those activities.
Consequently neither mode admits a source or qualifies an engineering result.
Source mode exits 2 even for a structurally complete evidence candidate. Synthetic
mode exits 0 solely for software exercise. Neither mode emits numeric source cells.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import html
import io
import json
import math
from pathlib import Path
import re
import sys
from datetime import datetime

SOURCE_SHA256 = "d6b7f16df361f9ee36cc626886ea130f09d3455e8ec94ca9bfd0b1756e92007b"
SOURCE_DOI = "https://doi.org/10.1016/j.apor.2026.105302"
CSV_HEADER = ("table", "case_id", "source_row", "source_column", "field", "value_text", "unit")
CELL_KEY = CSV_HEADER[:5]
UNITS = frozenset(("m", "mm", "m/s", "Pa", "1", "unit 1"))
REPORT_NAMES = ("scour-table-readback.json", "scour-table-readback.html")
SOURCE_ARGUMENTS = ("inputs_csv", "results_csv", "literature_csv", "source_pdf",
                    "source_sha256", "rights_json", "readback_json")
RIGHTS_KEYS = frozenset(("schema_version", "source_sha256", "licence", "destination",
                         "extraction_status", "literature_clearance", "evidence",
                         "checked_by_role", "checked_at_utc"))
READBACK_KEYS = frozenset(("schema_version", "source_sha256", "csv_sha256",
                           "verification_method", "verified_by_role", "verified_at_utc", "cells"))
GAPS = (
    {
        "id": "checked_scour_transcription", "status": "BLOCKED-IF-MISSING",
        "evidence": ["S: cached table locations only; Tables 2 and 6 contain 58 paired physical cases; Table 7 contains 15 literature cases.",
                     "T: synthetic equation checks do not establish a published fixture."],
        "consequence": "Zero of the 73 published cases is admitted or independently read back in this audit.",
        "owner_role": "source readback reviewer",
        "missing_input": "Digest-verified S, complete scratch CSVs for Tables 2/6/7, actual headers and units, and independent original-cell readback.",
    },
    {
        "id": "public_scour_table_rights", "status": "BLOCKED-IF-MISSING",
        "evidence": ["S: CC BY-NC-ND 4.0 is reported by the cached packet; public derived-fixture permission is UNVERIFIED.",
                     "Separately sourced literature contributions require their own review."],
        "consequence": "Neither scratch extraction nor public numeric redistribution is authorized by this report.",
        "owner_role": "source rights reviewer",
        "missing_input": "Human source-specific rights review backed by digest-verified evidence for extraction, literature contributions and the scratch destination; a separate public grant before redistribution.",
    },
    {
        "id": "native_cyclic_runs", "status": "BLOCKED-IF-MISSING",
        "evidence": ["Y: described normalized curves and loading combinations do not establish supplied native runs."],
        "consequence": "Native cyclic response qualification cannot be calculated from descriptions alone.",
        "owner_role": "cyclic analysis maintainer",
        "missing_input": "Native cyclic input/output runs with stable identifiers, units, digests and versioned acceptance criteria.",
    },
    {
        "id": "clay_log_strain_convention", "status": "BLOCKED-IF-MISSING",
        "evidence": ["Y: equation 8 logarithm base and strain convention remain UNVERIFIED in the cached packet."],
        "consequence": "The cyclic degradation convention cannot be qualified; no additional clay helper is implemented.",
        "owner_role": "constitutive model reviewer",
        "missing_input": "Source-confirmed logarithm base and strain definition for Y equation 8, with applicable units and independent review.",
    },
    {
        "id": "native_cptu_matrix", "status": "BLOCKED-IF-MISSING",
        "evidence": ["C: Table 4 has eight parameter rows, not native benchmark cases; underlying data were not supplied.",
                     "A and Z: archive script, native case count and supporting-source interpretation remain UNVERIFIED."],
        "consequence": "CPTU benchmark qualification is not established by parameter rows or supporting-document listings.",
        "owner_role": "penetration analysis maintainer",
        "missing_input": "Native CPTU case matrix, source inputs and outputs, case counts, units, digests, source rights and acceptance criteria.",
    },
    {
        "id": "retained_scour_cptu_outputs", "status": "BLOCKED-IF-MISSING",
        "evidence": ["N and D: bounded historical index observations found no matching names; current retained-output coverage was not inspected.",
                     "H: one pipeline example does not establish a riprap benchmark."],
        "consequence": "No matching retained scour or CPTU result is admitted; absence across all retained data is not established.",
        "owner_role": "retained data custodian",
        "missing_input": "Matching owner-held source/run manifests with stable case identifiers, versions, units, SHA-256 digests, access rights and qualified native outputs.",
    },
)


class ValidationError(ValueError):
    """Carry fixed public-safe diagnostic codes without input payloads."""


def require(condition, code):
    """Reject an unmet criterion without echoing private evidence."""
    if not condition:
        raise ValidationError(code)


def sha256(path):
    """Hash the exact supplied file bytes, without interpreting their content."""
    digest = hashlib.sha256()
    with Path(path).open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def valid_digest(value):
    """Recognize the declared lowercase SHA-256 format."""
    return isinstance(value, str) and re.fullmatch(r"[0-9a-f]{64}", value) is not None


def exact_keys(value, keys, code):
    """Require an object with precisely the specified schema keys."""
    require(isinstance(value, dict) and set(value) == set(keys), code)


def unique_object(pairs):
    """Reject duplicate JSON object keys before parsing loses the duplicates."""
    result = {}
    for key, value in pairs:
        require(key not in result, "DUPLICATE_JSON_KEY")
        result[key] = value
    return result


def reject_constant(_value):
    """Reject JSON nonfinite constants."""
    raise ValidationError("NONFINITE_JSON")


def load_json(path):
    """Read a strict UTF-8 JSON object without permissive duplicate handling."""
    try:
        doc = json.loads(Path(path).read_text(encoding="utf-8"),
                         object_pairs_hook=unique_object, parse_constant=reject_constant)
    except (OSError, UnicodeError, ValueError) as exc:
        raise ValidationError("EVIDENCE_JSON_INVALID") from exc
    require(isinstance(doc, dict), "EVIDENCE_OBJECT_REQUIRED")
    return doc


def check_role_time(role, timestamp):
    """Validate role-word and real RFC3339 UTC instant syntax only."""
    require(isinstance(role, str) and re.fullmatch(r"[A-Za-z]+(?:[ -][A-Za-z]+)*", role),
            "ROLE_WORDS_REQUIRED")
    require(isinstance(timestamp, str) and re.fullmatch(
        r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:Z|\+00:00)", timestamp),
        "RFC3339_UTC_REQUIRED")
    try:
        datetime.fromisoformat(timestamp.replace("Z", "+00:00"))
    except ValueError as exc:
        raise ValidationError("RFC3339_UTC_REQUIRED") from exc


def read_csv(path, data=None):
    """Read exact cell strings from one byte snapshot with an exact header."""
    try:
        raw = Path(path).read_bytes() if data is None else data
        reader = csv.DictReader(io.StringIO(raw.decode("utf-8"), newline=""), strict=True)
        require(reader.fieldnames == list(CSV_HEADER), "CSV_HEADER_INVALID")
        rows = list(reader)
    except (OSError, UnicodeError, csv.Error) as exc:
        raise ValidationError("CSV_UNREADABLE") from exc
    require(bool(rows), "CSV_EMPTY")
    for row in rows:
        exact_keys(row, CSV_HEADER, "CSV_CELL_COUNT_INVALID")
        require(all(isinstance(value, str) for value in row.values()), "CSV_CELL_MISSING")
    return rows


def cell_key(cell):
    """Keep the complete proposed printed-location identity."""
    return tuple(cell[key] for key in CELL_KEY)


def validate_cell(cell, table):
    """Check proposed cell syntax, finite values and the stated unit vocabulary.

    Membership in this vocabulary does not establish field-specific units or
    actual source headers. Printed strings are never rewritten or inferred.
    Unavailable markers are preserved in scratch but block numerical admission.
    """
    exact_keys(cell, CSV_HEADER, "CELL_SCHEMA_INVALID")
    require(all(isinstance(value, str) and value.strip() for value in cell.values()),
            "CELL_UNAVAILABLE")
    require(cell["table"] == table, "TABLE_GROUP_INVALID")
    require(re.fullmatch(r"[1-9][0-9]*", cell["source_row"]), "SOURCE_ROW_INVALID")
    require(cell["unit"] in UNITS, "UNIT_UNVERIFIED")
    require(re.fullmatch(r"[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?",
                         cell["value_text"]), "VALUE_UNVERIFIED")
    require(math.isfinite(float(cell["value_text"])), "VALUE_NONFINITE")


def validate_table(cells, table):
    """Enforce per-table case/row bijection and uniform original column coverage."""
    require(bool(cells), "TABLE_EMPTY")
    keys, case_rows, row_cases, columns = set(), {}, {}, {}
    for cell in cells:
        validate_cell(cell, table)
        key = cell_key(cell)
        require(key not in keys, "DUPLICATE_CELL")
        keys.add(key)
        case_id, row = cell["case_id"], cell["source_row"]
        require(case_rows.get(case_id, row) == row, "DUPLICATE_CASE_ID")
        require(row_cases.get(row, case_id) == case_id, "DUPLICATE_SOURCE_ROW")
        case_rows[case_id], row_cases[row] = row, case_id
        case_columns = columns.setdefault(case_id, {})
        require(cell["source_column"] not in case_columns, "DUPLICATE_COLUMN")
        case_columns[cell["source_column"]] = (cell["field"], cell["unit"])
    require(set(map(int, row_cases)) == set(range(1, len(case_rows) + 1)), "SOURCE_ROW_SET_INVALID")
    require(all(value == next(iter(columns.values())) for value in columns.values()),
            "COLUMN_COVERAGE_INVALID")
    return set(case_rows)


def validate_tables(inputs, results, literature, *, physical_count=58, literature_count=15):
    """Count group-qualified cases, excluding the separate CFD tables entirely."""
    input_ids = validate_table(inputs, "2")
    result_ids = validate_table(results, "6")
    literature_ids = validate_table(literature, "7")
    require(input_ids == result_ids, "PHYSICAL_JOIN_ID_SET_MISMATCH")
    require(len(input_ids) == physical_count, "PHYSICAL_CASE_COUNT_INVALID")
    require(len(literature_ids) == literature_count, "LITERATURE_CASE_COUNT_INVALID")
    return {"physical": len(input_ids), "literature": len(literature_ids),
            "total": len(input_ids) + len(literature_ids),
            "cells": len(inputs) + len(results) + len(literature)}


def validate_rights(doc, source_digest, evidence_root):
    """Validate declared scratch-rights evidence structure and actual file hashes.

    These checks cannot authenticate permission or establish source-specific
    meaning. A human reviewer must examine the retained evidence separately.
    Evidence scope tokens are exactly extraction and literature; each identifies
    a distinct required review, even when one supplied file covers both.
    """
    exact_keys(doc, RIGHTS_KEYS, "RIGHTS_SCHEMA_INVALID")
    require(doc["schema_version"] == "scour-rights-1", "RIGHTS_VERSION_INVALID")
    require(valid_digest(doc["source_sha256"]) and doc["source_sha256"] == source_digest,
            "RIGHTS_SOURCE_DIGEST_MISMATCH")
    require(doc["licence"] == "CC BY-NC-ND 4.0", "RIGHTS_LICENCE_INVALID")
    require(doc["destination"] == "scratch", "RIGHTS_DESTINATION_INVALID")
    require(doc["extraction_status"] == doc["literature_clearance"] == "PERMITTED",
            "RIGHTS_UNVERIFIED_OR_DENIED")
    check_role_time(doc["checked_by_role"], doc["checked_at_utc"])
    require(isinstance(doc["evidence"], list) and doc["evidence"], "RIGHTS_EVIDENCE_MISSING")
    scopes = set()
    root = Path(evidence_root).resolve()
    for item in doc["evidence"]:
        exact_keys(item, ("locator", "sha256", "scope"), "RIGHTS_EVIDENCE_SCHEMA_INVALID")
        locator = item["locator"]
        require(isinstance(locator, str) and locator.strip(), "RIGHTS_LOCATOR_INVALID")
        relative = Path(locator)
        require(not relative.is_absolute() and ".." not in relative.parts, "RIGHTS_LOCATOR_ESCAPE")
        target = (root / relative).resolve()
        require(target.is_relative_to(root) and target.is_file(), "RIGHTS_EVIDENCE_UNAVAILABLE")
        require(valid_digest(item["sha256"]) and sha256(target) == item["sha256"],
                "RIGHTS_EVIDENCE_DIGEST_MISMATCH")
        require(item["scope"] in ("extraction", "literature"), "RIGHTS_SCOPE_UNVERIFIED")
        scopes.add(item["scope"])
    require(scopes == {"extraction", "literature"}, "RIGHTS_SCOPE_INCOMPLETE")


def validate_readback(doc, source_digest, csv_digests, cells):
    """Bind every declared MATCH to exact original strings and supplied CSV bytes.

    Declared matches are not proof that independent inspection occurred.
    """
    exact_keys(doc, READBACK_KEYS, "READBACK_SCHEMA_INVALID")
    require(doc["schema_version"] == "scour-readback-1", "READBACK_VERSION_INVALID")
    require(valid_digest(doc["source_sha256"]) and doc["source_sha256"] == source_digest,
            "READBACK_SOURCE_DIGEST_MISMATCH")
    exact_keys(doc["csv_sha256"], ("inputs", "results", "literature"), "READBACK_CSV_HASH_SCHEMA_INVALID")
    require(all(valid_digest(value) for value in doc["csv_sha256"].values())
            and doc["csv_sha256"] == csv_digests, "READBACK_CSV_DIGEST_MISMATCH")
    require(doc["verification_method"] == "independent-source-cell-readback", "READBACK_METHOD_INVALID")
    check_role_time(doc["verified_by_role"], doc["verified_at_utc"])
    require(isinstance(doc["cells"], list), "READBACK_CELLS_INVALID")
    expected = {cell_key(cell): cell for cell in cells}
    require(len(expected) == len(cells), "READBACK_INPUT_DUPLICATE")
    observed = set()
    for record in doc["cells"]:
        exact_keys(record, (*CSV_HEADER, "status"), "READBACK_CELL_SCHEMA_INVALID")
        require(all(isinstance(value, str) for value in record.values()), "READBACK_CELL_TYPE_INVALID")
        key = cell_key(record)
        require(key not in observed and key in expected, "READBACK_EXTRA_OR_DUPLICATE")
        observed.add(key)
        require(record["status"] == "MATCH", "READBACK_NOT_MATCHED")
        require({key: record[key] for key in CSV_HEADER} == expected[cell_key(record)],
                "READBACK_ORIGINAL_STRING_MISMATCH")
    require(observed == set(expected), "READBACK_INCOMPLETE")


def validate_source(args):
    """Verify the pinned PDF first, then structurally validate evidence snapshots."""
    require(valid_digest(args.source_sha256) and args.source_sha256 == SOURCE_SHA256
            and sha256(args.source_pdf) == SOURCE_SHA256, "SOURCE_DIGEST_MISMATCH")
    rights = load_json(args.rights_json)
    validate_rights(rights, SOURCE_SHA256, Path(args.rights_json).parent)
    tables, digests = [], {}
    for name in ("inputs", "results", "literature"):
        path = Path(getattr(args, name + "_csv"))
        data = path.read_bytes()
        digests[name] = hashlib.sha256(data).hexdigest()
        tables.append(read_csv(path, data))
    counts = validate_tables(*tables)
    validate_readback(load_json(args.readback_json), SOURCE_SHA256, digests,
                      [cell for table in tables for cell in table])
    return counts


def synthetic_tables():
    """Build visibly invented fixtures inline, with a cross-group ID collision."""
    tables = []
    for table, count in (("2", 2), ("6", 2), ("7", 1)):
        tables.append([
            dict(zip(CSV_HEADER, (table, f"INVENTED-{index}", str(index + 1),
                                  label, label, value, unit)))
            for index in range(count)
            for label, value, unit in (("INVENTED length", "1.230", "m"),
                                       ("INVENTED dimensionless", "0.03700", "1"))
        ])
    return tables


def build_report(mode, counts=None, diagnostic=None):
    """Use fixed metadata only; never include supplied payloads or private locators."""
    return {
        "schema_version": "scour-table-audit-1", "criteria_version": "scour-readback-criteria-1",
        "status": "SYNTHETIC_ONLY" if mode == "synthetic" else "BLOCKED-IF-MISSING",
        "source_status": "SOURCE_NOT_ADMITTED", "source_admitted": False,
        "engineering_qualified": False, "evidence_structure_valid": counts is not None,
        "diagnostic": diagnostic or "HUMAN_SOURCE_REVIEW_REQUIRED",
        "published_cases_expected": 73, "published_cases_admitted": 0,
        "published_cases_blocked": 73, "source_cells_independently_read_back": 0,
        "candidate_counts": counts if mode == "source" and counts is not None else None,
        "synthetic_counts": counts if mode == "synthetic" else None,
        "source_basis": {"key": "S", "doi": SOURCE_DOI, "expected_sha256": SOURCE_SHA256,
                         "declared_version": "2026-10-15", "licence": "CC BY-NC-ND 4.0",
                         "observation": "Cached packet metadata; source bytes and rights not admitted.",
                         "table_roles": {"2": "physical inputs", "6": "physical results", "7": "literature"},
                         "excluded": "Tables 3-5 describe 18 separate CFD cases; no CFD case enters the 73-case count."},
        "summary": "Offline software checks do not establish source admission or geotechnical validation.",
        "revision_history": [{"revision": "0", "date": "2026-10-25",
                              "change": "Initial metadata-only audit; source admission and six missing-input dispositions are made explicit."}],
        "qualification_criteria": [
            "Exact pinned PDF digest before CSV validation.",
            "Human source-specific rights review for scratch extraction and literature contributions.",
            "Exactly 58 paired physical IDs plus 15 literature IDs, qualified by source group.",
            "Actual source headers and field-specific units checked against the paper; printed dimensionless strings preserved.",
            "Every required original cell independently read back with bound CSV digests.",
        ],
        "limits": [
            "SYNTHETIC_ONLY denotes invented software fixtures, with zero admitted published cases.",
            "Source mode checks an evidence candidate; PERMITTED and MATCH declarations alone cannot authenticate human review.",
            "Header semantics and field-specific units remain UNVERIFIED until source inspection; the proposed schema is not an observed source table.",
            "The current CLI has no authenticated human-review channel and always leaves source_admitted and engineering_qualified false.",
            "No source cells, numeric transcriptions, inferred particle properties or optional normalization are emitted.",
            "The cached source domain is steady currents with intact riprap within laboratory calibration conditions; cyclic or damaged protection is excluded.",
            "Public numeric redistribution is not authorized; scratch permission does not establish public fixture rights.",
            "Unsafe output aliases or unreadable existing rights evidence prevent all report writes and return exit 2; supplied evidence is preserved.",
        ],
        "gaps": list(GAPS),
    }


def render_html(report):
    """Render deterministic, escaped metadata with captions below each table."""
    escape = lambda value: html.escape(str(value), quote=True)
    revision = "".join("<tr>" + "".join(f"<td>{escape(row[key])}</td>" for key in
                       ("revision", "date", "change")) + "</tr>" for row in report["revision_history"])
    gaps = "".join("<tr>" + "".join(f"<td>{escape(' '.join(gap[key]) if key == 'evidence' else gap[key])}</td>"
                   for key in ("id", "status", "evidence", "consequence", "owner_role", "missing_input"))
                   + "</tr>" for gap in report["gaps"])
    criteria = "".join(f"<li>{escape(item)}</li>" for item in report["qualification_criteria"])
    limits = "".join(f"<li>{escape(item)}</li>" for item in report["limits"])
    return f'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Scour table readback and qualification gaps</title>
<style>body{{font:16px system-ui,sans-serif;max-width:90rem;margin:2rem auto;padding:0 1rem;color:#162536}}table{{border-collapse:collapse;width:100%}}th,td{{border:1px solid #9aaabb;padding:.5rem;text-align:left;vertical-align:top}}.scroll{{overflow-x:auto}}.status{{font-weight:bold;color:#754400}}h1,h2{{line-height:1.3}}</style></head>
<body><h1>Scour table readback and qualification gaps</h1>
<h2>Summary</h2><p>{escape(report['summary'])}</p>
<p class="status">{escape(report['status'])}; {escape(report['source_status'])}</p>
<p>Published cases admitted: 0 of 73; published cases blocked: 73; source cells independently read back: 0.</p>
<p>Evidence structure valid: {escape(report['evidence_structure_valid'])}; source admitted: false; engineering qualified: false.</p>
<p>Criterion disposition: {escape(report['diagnostic'])}.</p>
<h2>Revision history</h2><div class="scroll"><table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead><tbody>{revision}</tbody></table></div>
<p>Table 1. Audit revisions.</p>
<h2>Gap dispositions</h2><div class="scroll"><table><thead><tr><th>Gap</th><th>Status</th><th>Evidence</th><th>Consequence</th><th>Owner role</th><th>Exact missing input</th></tr></thead><tbody>{gaps}</tbody></table></div>
<p>Table 2. Six independent missing-input dispositions; none is cleared by software exit status.</p>
<h2>Source basis and criteria</h2><p>Source S: <a href="{escape(SOURCE_DOI)}">article DOI</a>; declared version 2026-10-15; CC BY-NC-ND 4.0 per cached packet.</p>
<p>Expected SHA-256: <code>{SOURCE_SHA256}</code>. The packet metadata is historical evidence, not current source inspection.</p>
<p>Tables 2/6: 58 paired physical cases; Table 7: 15 literature cases. Tables 3-5: 18 separate CFD cases, excluded.</p>
<p>Proposed CSV header: <code>{escape(','.join(CSV_HEADER))}</code>. Unit vocabulary: m, mm, m/s, Pa, 1 and unit 1; field-specific semantics remain unverified.</p>
<p>Synthetic counts: {escape(report['synthetic_counts'])}; evidence candidate counts: {escape(report['candidate_counts'])}.</p>
<ol>{criteria}</ol><h2>Known limits</h2><ul>{limits}</ul>
</body></html>
'''


def protected_inputs(args):
    """Collect supplied inputs and evidence targets before any output mutation."""
    paths = [Path(getattr(args, key)).resolve() for key in SOURCE_ARGUMENTS
             if key != "source_sha256" and getattr(args, key)]
    if args.rights_json:
        try:
            doc = load_json(args.rights_json)
            evidence = doc.get("evidence", [])
            if isinstance(evidence, list):
                for item in evidence:
                    if isinstance(item, dict) and isinstance(item.get("locator"), str):
                        paths.append((Path(args.rights_json).parent / item["locator"]).resolve())
        except ValidationError:
            # Existing malformed evidence may conceal protected target paths.
            # Missing evidence has no retained bytes to overwrite.
            require(not Path(args.rights_json).exists(), "RIGHTS_OUTPUT_SAFETY_UNVERIFIED")
    return paths


def output_targets(args):
    """Refuse aliases to evidence, including symlinks and existing hardlinks."""
    targets = [Path(args.out_dir) / name for name in REPORT_NAMES]
    protected = protected_inputs(args)
    require(not (targets[0].exists() and targets[1].exists()
                 and targets[0].samefile(targets[1])), "OUTPUT_REPORT_ALIAS")
    for target in targets:
        require(not target.is_symlink(), "OUTPUT_SYMLINK_REFUSED")
        require(not target.exists() or target.is_file(), "OUTPUT_TYPE_INVALID")
        for source in protected:
            require(target.resolve() != source, "OUTPUT_INPUT_COLLISION")
            require(not (target.exists() and source.exists() and target.samefile(source)),
                    "OUTPUT_INPUT_COLLISION")
    return targets


def emit_reports(targets, report):
    """Write only the two metadata reports into the explicitly requested directory."""
    targets[0].parent.mkdir(parents=True, exist_ok=True)
    targets[0].write_text(json.dumps(report, indent=2, ensure_ascii=False, allow_nan=False)
                          + "\n", encoding="utf-8")
    targets[1].write_text(render_html(report), encoding="utf-8")


def argument_parser():
    """Expose the two bounded modes without implicit source discovery."""
    parser = argparse.ArgumentParser(prog="scour_table_readback.py", description=__doc__)
    parser.add_argument("--synthetic", action="store_true", help="Exercise invented fixtures only.")
    parser.add_argument("--out-dir", type=Path, required=True, help="Metadata-only output directory.")
    for name in SOURCE_ARGUMENTS:
        parser.add_argument("--" + name.replace("_", "-"), type=str if name == "source_sha256" else Path)
    return parser


def main(argv=None):
    """Return 0 for synthetic exercise, 2 for blocked or candidate-only source mode."""
    args = argument_parser().parse_args(argv)
    try:
        targets = output_targets(args)
    except (ValidationError, OSError) as exc:
        print(str(exc) if isinstance(exc, ValidationError) else "OUTPUT_UNAVAILABLE", file=sys.stderr)
        return 2
    mode, counts, diagnostic = "source", None, None
    try:
        supplied = [getattr(args, name) is not None for name in SOURCE_ARGUMENTS]
        if args.synthetic:
            require(not any(supplied), "SYNTHETIC_SOURCE_ARGUMENTS_FORBIDDEN")
            counts = validate_tables(*synthetic_tables(), physical_count=2, literature_count=1)
            mode, diagnostic = "synthetic", "SOFTWARE_EXERCISE_ONLY"
        else:
            require(all(supplied), "SOURCE_ARGUMENTS_REQUIRED")
            counts = validate_source(args)
    except ValidationError as exc:
        diagnostic = str(exc)
    except (OSError, UnicodeError, ValueError):
        diagnostic = "EVIDENCE_UNAVAILABLE_OR_INVALID"
    try:
        emit_reports(targets, build_report(mode, counts, diagnostic))
    except OSError:
        print("OUTPUT_UNAVAILABLE", file=sys.stderr)
        return 2
    return 0 if mode == "synthetic" else 2


if __name__ == "__main__":
    raise SystemExit(main())
