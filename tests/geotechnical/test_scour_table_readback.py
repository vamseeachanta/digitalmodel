# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Invented cell fixtures exercise software, never published scour results."""

import copy
import csv
import importlib.util
import json
from pathlib import Path
from types import SimpleNamespace

import pytest

SCRIPT = Path(__file__).resolve().parents[2] / "scripts/geotechnical/scour_table_readback.py"
spec = importlib.util.spec_from_file_location("scour_table_readback", SCRIPT)
readback = importlib.util.module_from_spec(spec)
spec.loader.exec_module(readback)


def invented_rows(table, count=2):
    """Return invented labels and original strings, including a group collision."""
    return [
        dict(zip(readback.CSV_HEADER, (
            str(table), f"INVENTED-{index}", str(index + 1), label, label, value, unit
        )))
        for index in range(count)
        for label, value, unit in (
            ("INVENTED length", "1.230", "m"),
            ("INVENTED dimensionless", "0.03700", "1"),
        )
    ]


def small_tables():
    return [invented_rows(table) for table in (2, 6, 7)]


def check_tables(tables):
    return readback.validate_tables(*tables, physical_count=2, literature_count=2)


def rights_document(root, digest):
    evidence = root / "invented-rights.txt"
    evidence.write_text("Invented test evidence; no source permission is granted.\n",
                        encoding="utf-8", newline="\n")
    return {
        "schema_version": "scour-rights-1", "source_sha256": digest,
        "licence": "CC BY-NC-ND 4.0", "destination": "scratch",
        "extraction_status": "PERMITTED", "literature_clearance": "PERMITTED",
        "evidence": [
            {"locator": evidence.name, "sha256": readback.sha256(evidence), "scope": scope}
            for scope in ("extraction", "literature")
        ],
        "checked_by_role": "source rights reviewer",
        "checked_at_utc": "2026-10-15T12:00:00Z",
    }


def readback_document(tables, digest="a" * 64, csv_hashes=None):
    return {
        "schema_version": "scour-readback-1", "source_sha256": digest,
        "csv_sha256": csv_hashes or dict.fromkeys(("inputs", "results", "literature"), "b" * 64),
        "verification_method": "independent-source-cell-readback",
        "verified_by_role": "source cell reviewer", "verified_at_utc": "2026-10-15T12:00:00Z",
        "cells": [{**row, "status": "MATCH"} for table in tables for row in table],
    }


def check_readback(doc, tables):
    readback.validate_readback(doc, "a" * 64, dict.fromkeys(
        ("inputs", "results", "literature"), "b" * 64), sum(tables, []))


def test_group_collision_and_printed_values_survive():
    tables = small_tables()
    original = copy.deepcopy(tables)
    counts = check_tables(tables)
    # 2 physical + 2 literature = 4 cases; 3 tables * 2 cases * 2 cells = 12 cells.
    assert counts == {"physical": 2, "literature": 2, "total": 4, "cells": 12}
    assert tables == original
    assert tables[0][1]["value_text"] == "0.03700"
    assert tables[0][0]["case_id"] == tables[2][0]["case_id"]


@pytest.mark.parametrize("change", ["duplicate_cell", "duplicate_case_row", "extra", "missing", "unequal_ids", "cfd", "mixed", "unequal_columns"])
def test_table_identity_and_join_fail_closed(change):
    tables = small_tables()
    if change == "duplicate_cell":
        tables[0].append(dict(tables[0][0]))
    elif change == "duplicate_case_row":
        tables[0].extend({**row, "source_row": "3"} for row in tables[0][:2])
    elif change == "extra":
        tables[1].extend({**row, "case_id": "INVENTED-extra", "source_row": "3"} for row in tables[1][:2])
    elif change == "missing":
        tables[1] = tables[1][:2]
    elif change == "unequal_ids":
        tables[1][0]["case_id"] = tables[1][1]["case_id"] = "INVENTED-other"
    elif change == "cfd":
        tables[0][0]["table"] = "3"
    elif change == "mixed":
        tables[2][0]["table"] = "6"
    else:
        tables[0].pop()
    with pytest.raises(readback.ValidationError):
        check_tables(tables)


@pytest.mark.parametrize("field,value", [
    ("unit", "ft"), ("unit", ""), ("unit", "UNVERIFIED"),
    ("value_text", "NaN"), ("value_text", "-inf"), ("value_text", "Infinity"),
    ("value_text", "1e999999"), ("value_text", ""), ("value_text", "—"),
    ("value_text", "n/a"), ("value_text", "not a number"),
    ("source_row", "0"), ("source_row", "1.0"), ("case_id", ""),
    ("field", ""), ("source_column", ""),
])
def test_unverified_cells_cannot_be_numerical_inputs(field, value):
    tables = small_tables()
    tables[0][0][field] = value
    with pytest.raises(readback.ValidationError):
        check_tables(tables)


def test_production_counts_are_58_plus_15():
    tables = [invented_rows(2, 58), invented_rows(6, 58), invented_rows(7, 15)]
    assert readback.validate_tables(*tables)["total"] == 73
    with pytest.raises(readback.ValidationError):
        readback.validate_tables(*small_tables())


def test_zero_remains_a_true_zero():
    tables = small_tables()
    tables[0][0]["value_text"] = "0"
    assert check_tables(tables)["total"] == 4
    assert tables[0][0]["value_text"] == "0"


def test_readback_accepts_exact_original_strings_only():
    tables = small_tables()
    check_readback(readback_document(tables), tables)


@pytest.mark.parametrize("change", ["missing", "extra", "duplicate", "mismatch", "unavailable", "rewritten", "digest", "source", "method", "time", "role", "extra_key"])
def test_readback_rejects_unbound_evidence(change):
    tables = small_tables()
    doc = readback_document(tables)
    if change == "missing":
        doc["cells"].pop()
    elif change in ("extra", "duplicate"):
        row = dict(doc["cells"][0])
        if change == "extra":
            row["case_id"] = "INVENTED-extra"
        doc["cells"].append(row)
    elif change == "mismatch":
        doc["cells"][0]["status"] = "MISMATCH"
    elif change == "unavailable":
        doc["cells"][0]["status"] = "SOURCE_UNAVAILABLE"
    elif change == "rewritten":
        doc["cells"][0]["value_text"] = "1.23"
    elif change == "digest":
        doc["csv_sha256"]["inputs"] = "c" * 64
    elif change == "source":
        doc["source_sha256"] = "c" * 64
    elif change == "method":
        doc["verification_method"] = "copied"
    elif change == "time":
        doc["verified_at_utc"] = "2026-99-99T99:99:99Z"
    elif change == "role":
        doc["verified_by_role"] = ""
    else:
        doc["extra"] = True
    with pytest.raises(readback.ValidationError):
        check_readback(doc, tables)


def test_rights_checks_evidence_bytes_not_only_permitted(tmp_path):
    doc = rights_document(tmp_path, "a" * 64)
    readback.validate_rights(doc, "a" * 64, tmp_path)
    (tmp_path / "invented-rights.txt").write_text("changed", encoding="utf-8", newline="\n")
    with pytest.raises(readback.ValidationError):
        readback.validate_rights(doc, "a" * 64, tmp_path)


@pytest.mark.parametrize("change", ["absent", "denied", "unverified", "no_evidence", "scope", "wrong_source", "destination", "licence", "time", "role", "extra_key", "escape", "symlink"])
def test_rights_fail_closed(tmp_path, change):
    doc = rights_document(tmp_path, "a" * 64)
    if change == "absent":
        doc = {}
    elif change in ("denied", "unverified"):
        doc["extraction_status"] = change.upper()
    elif change == "no_evidence":
        doc["evidence"] = []
    elif change == "scope":
        doc["evidence"] = doc["evidence"][:1]
    elif change == "wrong_source":
        doc["source_sha256"] = "c" * 64
    elif change == "destination":
        doc["destination"] = "public"
    elif change == "licence":
        doc["licence"] = "CC BY 4.0"
    elif change == "time":
        doc["checked_at_utc"] = "2026-10-15"
    elif change == "role":
        doc["checked_by_role"] = "reviewer123"
    elif change == "extra_key":
        doc["grants_authority"] = True
    elif change == "escape":
        doc["evidence"][0]["locator"] = "../invented-rights.txt"
    else:
        outside = tmp_path.parent / "invented-outside.txt"
        outside.write_text("Invented external evidence", encoding="utf-8", newline="\n")
        (tmp_path / "escape-link").symlink_to(outside)
        doc["evidence"][0].update(locator="escape-link", sha256=readback.sha256(outside))
    with pytest.raises(readback.ValidationError):
        readback.validate_rights(doc, "a" * 64, tmp_path)


@pytest.mark.parametrize("text", ['{"key":1,"key":2}', '{"key":NaN}', '[]'])
def test_json_rejects_duplicate_keys_nonfinite_and_wrong_root(tmp_path, text):
    path = tmp_path / "invalid.json"
    path.write_text(text, encoding="utf-8", newline="\n")
    with pytest.raises(readback.ValidationError):
        readback.load_json(path)


def write_source_candidate(tmp_path, monkeypatch):
    source = tmp_path / "invented.pdf"
    source.write_bytes(b"INVENTED source, not a paper or a source-rights grant")
    digest = readback.sha256(source)
    monkeypatch.setattr(readback, "SOURCE_SHA256", digest)
    tables = [invented_rows(2, 58), invented_rows(6, 58), invented_rows(7, 15)]
    paths = {}
    for name, rows in zip(("inputs", "results", "literature"), tables):
        paths[name] = tmp_path / f"{name}.csv"
        with paths[name].open("w", newline="\n", encoding="utf-8") as handle:
            writer = csv.DictWriter(handle, fieldnames=readback.CSV_HEADER, lineterminator="\n")
            writer.writeheader()
            writer.writerows(rows)
    rights = tmp_path / "rights.json"
    rights.write_text(json.dumps(rights_document(tmp_path, digest)), encoding="utf-8", newline="\n")
    record = tmp_path / "readback.json"
    record.write_text(json.dumps(readback_document(tables, digest, {
        key: readback.sha256(path) for key, path in paths.items()
    })), encoding="utf-8", newline="\n")
    return ["--source-pdf", str(source), "--source-sha256", digest,
            "--inputs-csv", str(paths["inputs"]), "--results-csv", str(paths["results"]),
            "--literature-csv", str(paths["literature"]), "--rights-json", str(rights),
            "--readback-json", str(record)]


def load_report(out):
    return json.loads((out / "scour-table-readback.json").read_text(encoding="utf-8"))


def test_complete_declarations_are_only_a_candidate(tmp_path, monkeypatch):
    args = write_source_candidate(tmp_path, monkeypatch)
    out = tmp_path / "out"
    assert readback.main(args + ["--out-dir", str(out)]) == 2
    report = load_report(out)
    assert report["evidence_structure_valid"] is True
    assert report["candidate_counts"]["total"] == 73
    assert report["source_admitted"] is False
    assert report["engineering_qualified"] is False
    assert report["published_cases_admitted"] == 0
    assert report["source_cells_independently_read_back"] == 0
    text = (out / "scour-table-readback.json").read_text(encoding="utf-8")
    for secret in ("INVENTED-0", "1.230", "0.03700", "invented.pdf", "source cell reviewer"):
        assert secret not in text
    assert "density" not in report


def test_source_digest_is_checked_before_csv_open(tmp_path, monkeypatch):
    source = tmp_path / "invented.pdf"
    source.write_bytes(b"changed")
    calls = []
    monkeypatch.setattr(readback, "read_csv", lambda *args: calls.append(args))
    args = SimpleNamespace(source_pdf=source, source_sha256="0" * 64)
    with pytest.raises(readback.ValidationError, match="SOURCE_DIGEST"):
        readback.validate_source(args)
    assert not calls


def test_self_consistent_unpinned_source_is_rejected(tmp_path):
    source = tmp_path / "invented.pdf"
    source.write_bytes(b"INVENTED unpinned source")
    args = SimpleNamespace(source_pdf=source, source_sha256=readback.sha256(source))
    with pytest.raises(readback.ValidationError, match="SOURCE_DIGEST"):
        readback.validate_source(args)


def test_csv_header_and_extra_cells_are_rejected(tmp_path):
    path = tmp_path / "invented.csv"
    for text in ("wrong,header\na,b\n", ",".join(readback.CSV_HEADER) + "\n2,INVENTED,1,A,A,1,m,extra\n"):
        path.write_text(text, encoding="utf-8", newline="\n")
        with pytest.raises(readback.ValidationError):
            readback.read_csv(path)


def test_synthetic_output_is_deterministic_metadata_only(tmp_path):
    first, second = tmp_path / "first", tmp_path / "second"
    assert readback.main(["--synthetic", "--out-dir", str(first)]) == 0
    assert readback.main(["--synthetic", "--out-dir", str(second)]) == 0
    names = {"scour-table-readback.json", "scour-table-readback.html"}
    assert {p.name for p in first.iterdir()} == names
    for name in names:
        assert (first / name).read_bytes() == (second / name).read_bytes()
    report = load_report(first)
    # 2 physical + 1 literature = 3 cases; (2 + 2 + 1) table rows * 2 cells = 10 cells.
    assert report["synthetic_counts"] == {"physical": 2, "literature": 1, "total": 3, "cells": 10}
    assert report["status"] == "SYNTHETIC_ONLY"
    assert report["source_status"] == "SOURCE_NOT_ADMITTED"
    assert report["published_cases_admitted"] == 0
    assert report["published_cases_blocked"] == 73
    assert report["source_cells_independently_read_back"] == 0
    assert report["source_admitted"] is report["engineering_qualified"] is False
    assert len(report["gaps"]) == 6
    assert len({gap["id"] for gap in report["gaps"]}) == 6
    for gap in report["gaps"]:
        assert gap["status"] == "BLOCKED-IF-MISSING"
        assert all(gap[key] for key in ("evidence", "consequence", "owner_role", "missing_input"))
    html = (first / "scour-table-readback.html").read_text(encoding="utf-8")
    assert html.index("Summary") < html.index("Revision history") < html.index("Gap dispositions")
    assert "INVENTED-" not in html


def test_missing_source_evidence_emits_blocked_reports(tmp_path):
    assert readback.main(["--out-dir", str(tmp_path / "out")]) == 2
    assert load_report(tmp_path / "out")["status"] == "BLOCKED-IF-MISSING"


def test_synthetic_mode_rejects_source_arguments(tmp_path):
    assert readback.main(["--synthetic", "--source-sha256", "a" * 64,
                          "--out-dir", str(tmp_path / "out")]) == 2
    assert load_report(tmp_path / "out")["evidence_structure_valid"] is False


def test_printed_row_cannot_identify_two_cases():
    tables = small_tables()
    for row in tables[0][2:]:
        row["source_row"] = "1"
    with pytest.raises(readback.ValidationError):
        check_tables(tables)


def test_failed_rerun_replaces_previous_reports(tmp_path):
    out = tmp_path / "out"
    assert readback.main(["--synthetic", "--out-dir", str(out)]) == 0
    assert readback.main(["--out-dir", str(out)]) == 2
    assert load_report(out)["status"] == "BLOCKED-IF-MISSING"
    assert "BLOCKED-IF-MISSING; SOURCE_NOT_ADMITTED</p>" in (out / "scour-table-readback.html").read_text(encoding="utf-8")


@pytest.mark.parametrize("input_kind", ["inputs-csv", "results-csv", "literature-csv", "source-pdf", "rights-json", "readback-json"])
def test_output_never_overwrites_supplied_input(tmp_path, input_kind):
    out = tmp_path / "out"
    out.mkdir()
    protected = out / "scour-table-readback.json"
    protected.write_text("INVENTED protected input", encoding="utf-8", newline="\n")
    before = protected.read_bytes()
    assert readback.main([f"--{input_kind}", str(protected), "--out-dir", str(out)]) == 2
    assert protected.read_bytes() == before


def test_output_never_overwrites_rights_evidence(tmp_path):
    out = tmp_path / "out"
    out.mkdir()
    protected = out / "scour-table-readback.html"
    protected.write_text("INVENTED protected evidence", encoding="utf-8", newline="\n")
    rights = tmp_path / "rights.json"
    doc = rights_document(tmp_path, "a" * 64)
    doc["evidence"][0].update(locator="out/scour-table-readback.html", sha256=readback.sha256(protected))
    rights.write_text(json.dumps(doc), encoding="utf-8", newline="\n")
    before = protected.read_bytes()
    assert readback.main(["--rights-json", str(rights), "--out-dir", str(out)]) == 2
    assert protected.read_bytes() == before


def test_output_symlink_is_not_followed(tmp_path):
    protected = tmp_path / "protected.txt"
    protected.write_text("INVENTED protected input", encoding="utf-8", newline="\n")
    out = tmp_path / "out"
    out.mkdir()
    (out / "scour-table-readback.json").symlink_to(protected)
    assert readback.main(["--synthetic", "--out-dir", str(out)]) == 2
    assert protected.read_text(encoding="utf-8") == "INVENTED protected input"


def test_hardlinked_output_reports_are_preserved(tmp_path):
    out = tmp_path / "out"
    out.mkdir()
    first = out / "scour-table-readback.json"
    second = out / "scour-table-readback.html"
    first.write_text("INVENTED protected output", encoding="utf-8", newline="\n")
    second.hardlink_to(first)
    before = first.read_bytes()
    assert readback.main(["--synthetic", "--out-dir", str(out)]) == 2
    assert first.read_bytes() == second.read_bytes() == before


def test_malformed_rights_cannot_hide_evidence_targets(tmp_path):
    out = tmp_path / "out"
    out.mkdir()
    protected = out / "scour-table-readback.html"
    protected.write_text("INVENTED protected evidence", encoding="utf-8", newline="\n")
    rights = tmp_path / "rights.json"
    rights.write_text('{"duplicate":1,"duplicate":2,"evidence":'
                      '[{"locator":"out/scour-table-readback.html"}]}',
                      encoding="utf-8", newline="\n")
    before = protected.read_bytes()
    assert readback.main(["--rights-json", str(rights), "--out-dir", str(out)]) == 2
    assert protected.read_bytes() == before


@pytest.mark.parametrize("shape", [
    "object", "string", "null", "number", "boolean", "empty", "missing",
    "entry_string", "entry_null", "entry_list", "entry_missing_key",
    "entry_extra_key", "locator_null", "locator_empty", "mixed_entries",
])
def test_malformed_rights_shapes_preserve_all_reports(tmp_path, capsys, shape):
    out = tmp_path / "out"
    out.mkdir()
    reports = [out / name for name in readback.REPORT_NAMES]
    for path in reports:
        path.write_text("INVENTED retained evidence\n", encoding="utf-8", newline="\n")
    doc = rights_document(tmp_path, "a" * 64)
    item = {"locator": "out/scour-table-readback.html",
            "sha256": readback.sha256(reports[1]), "scope": "extraction"}
    shapes = {
        "object": item, "string": item["locator"], "null": None,
        "number": 1, "boolean": True, "empty": [],
        "entry_string": [item["locator"]], "entry_null": [None], "entry_list": [[]],
        "entry_missing_key": [{"locator": item["locator"]}],
        "entry_extra_key": [{**item, "extra": True}],
        "locator_null": [{**item, "locator": None}],
        "locator_empty": [{**item, "locator": " "}],
        "mixed_entries": [doc["evidence"][0], None],
    }
    if shape == "missing":
        del doc["evidence"]
    else:
        doc["evidence"] = shapes[shape]
    rights = tmp_path / "rights.json"
    rights.write_text(json.dumps(doc), encoding="utf-8", newline="\n")
    retained = {path: path.read_bytes() for path in [*reports, rights]}
    assert readback.main(["--rights-json", str(rights), "--out-dir", str(out)]) == 2
    assert {path: path.read_bytes() for path in retained} == retained
    assert capsys.readouterr().err.strip() == "RIGHTS_OUTPUT_SAFETY_UNVERIFIED"


def test_output_junction_is_not_followed(tmp_path, monkeypatch, capsys):
    out = tmp_path / "out"
    out.mkdir()
    protected = out / "scour-table-readback.json"
    protected.write_text("INVENTED retained evidence\n", encoding="utf-8", newline="\n")
    monkeypatch.setattr(Path, "is_junction", lambda path: path == protected, raising=False)
    before = protected.read_bytes()
    assert readback.main(["--synthetic", "--out-dir", str(out)]) == 2
    assert protected.read_bytes() == before
    assert capsys.readouterr().err.strip() == "OUTPUT_SYMLINK_REFUSED"
