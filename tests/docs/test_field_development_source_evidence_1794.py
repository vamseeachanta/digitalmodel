# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline structural and document checks for the issue research note pack."""

import json
import re
from collections import Counter
from copy import deepcopy
from pathlib import Path
from urllib.parse import urlsplit

import pytest


ROOT = Path(__file__).resolve().parents[2]
DIRECTORY = ROOT / "docs/research"
BASE = "field-development-source-evidence-1794"
FIELDS = ("num_trees", "num_manifolds", "tieback_distance_km", "cost_usd_bn")
RECORDS = ["Whale", "Appomattox", "Mad Dog Phase 2 / Argos", "Perdido", "Stones",
           "Lucius", "Thunder Horse", "Atlantis", "Mars / Mars B Olympus", "Na Kika"]
GATE_RECORDS = ["Perdido", "Appomattox", "Whale", "Atlantis", "Thunder Horse",
                "Mars / Olympus", "Mad Dog Phase 2", "Stones", "Lucius", "Na Kika"]
# The research note orders the provisional table differently and uses two aliases.
# Known note anomalies: none in the class-count table; absence paragraphs for
# Mad Dog Phase 2, Perdido, Lucius and Mars contain narrative/preamble wording.
KNOWN_NOTE_ANOMALIES = []


def load_pack():
    """Load parent and every declared record part without network access."""
    parent = json.loads((DIRECTORY / (BASE + ".json")).read_text())
    records = []
    for name in parent["record_files"]:
        part = json.loads((DIRECTORY / name).read_text())
        assert part["schema_version"] == parent["schema_version"]
        assert part["issue"] == 1794
        assert part["parent_file"] == BASE + ".json"
        records.extend(part["records"])
    return parent, records


def cited_rows(finding):
    """Parse singular/plural row locators from note findings."""
    return [int(n) for group in re.findall(r"\brows? ([\d, ]+)", finding)
            for n in re.findall(r"\d+", group)]


def validate_evidence_entry(entry, source_refs):
    """Reject incomplete or internally inconsistent evidence objects."""
    required = {"row", "quantity", "value_as_stated", "quote_cell", "ref", "class",
                "unverified"}
    assert required <= entry.keys()
    assert type(entry["row"]) is int and entry["row"] > 0
    assert entry["ref"] is None or entry["ref"] in source_refs
    assert type(entry["unverified"]) is bool
    assert entry["unverified"] == (entry["class"] == "unverified")
    for field in ("quantity", "value_as_stated", "quote_cell", "class"):
        assert isinstance(entry[field], str) and entry[field]


def test_sources_and_parts():
    parent, records = load_pack()
    assert parent["issue"] == 1794 and parent["schema_version"] == 1
    assert parent["source_of_entries"] == "research note on the issue"
    assert parent["checked"] == "2026-10-11"
    assert set(parent["record_files"]) == {
        p.name for p in DIRECTORY.glob(BASE + ".records-*.json")}
    assert len(set(parent["record_files"])) == len(parent["record_files"])
    assert [s["ref"] for s in parent["sources"]] == [f"S{i}" for i in range(1, 19)]
    assert [r["record"] for r in records] == RECORDS
    for source in parent["sources"]:
        assert {"ref", "title", "publisher", "date", "class",
                "reuse_terms_as_stated", "url"} <= source.keys()
        assert all(isinstance(source[k], str) and source[k] for k in source)
        parsed = urlsplit(source["url"])
        assert parsed.scheme in {"http", "https"} and parsed.hostname
        assert not parsed.username and not parsed.password
    assert len(parent["access_limits"]) == 5
    assert len(parent["gap_list"]) == 8
    assert set(parent["context_fields"]) == {"confirmed", "corrected", "unsupported"}
    assert parent["source_acceptance_rule"]["tieback_distance_required"] is False
    assert parent["source_acceptance_rule"]["contractor_award_scope_label"] == "as-contracted"


def test_evidence_rows_and_class_counts():
    parent, records = load_pack()
    evidence = [e for r in records for e in r["evidence"]]
    assert [e["row"] for e in evidence] == list(range(1, 83))
    refs = {s["ref"] for s in parent["sources"]}
    for record in records:
        assert {"record", "host", "evidence", "no_primary_source_found"} <= record.keys()
        assert record["host"] and record["no_primary_source_found"]
        for entry in record["evidence"]:
            validate_evidence_entry(entry, refs)
    counts = Counter(e["class"] for e in evidence)
    printed = parent["row_counts_by_class"]
    for label, count in printed.items():
        key = label.split(" (")[0]
        assert (len(evidence) if key == "Total" else counts[key]) == count
    assert counts == {"operator": 21, "contractor": 21, "filing": 16,
                      "secondary": 10, "unverified": 14}


def test_gate_review_and_empty_acceptance():
    parent, records = load_pack()
    assert [g["record"] for g in parent["gate_review"]] == GATE_RECORDS
    evidence = {e["row"]: e for r in records for e in r["evidence"]}
    contracted = set()
    for gate in parent["gate_review"]:
        assert set(gate) == {"record", *FIELDS}
        for field in FIELDS:
            cell = gate[field]
            assert set(cell) == {"provisional_value_as_given", "finding", "cell_as_given"}
            value, finding = cell["cell_as_given"].split(":", 1)
            assert value == cell["provisional_value_as_given"]
            assert finding.strip() == cell["finding"]
            for row in cited_rows(cell["finding"]):
                assert row in evidence
                if field in FIELDS[:2] and evidence[row]["class"] == "contractor":
                    contracted.add(row)
    assert parent["as_contracted_rows"] == sorted(contracted) == [13, 14, 28, 52, 53, 67]
    assert parent["accepted_records"] == []
    for candidate in parent["accepted_records"]:
        assert acceptance_reasons(candidate, parent["sources"], list(evidence.values())) == []


def markdown_cells(line):
    """Read the note's simple unescaped-pipe table format."""
    return [cell.strip() for cell in line.strip().strip("|").split("|")]


def test_document_fidelity():
    parent, records = load_pack()
    document = (DIRECTORY / (BASE + ".md")).read_text()
    lines = document.splitlines()
    source_rows = [markdown_cells(l) for l in lines if re.match(r"^\| S\d+ \|", l)]
    assert [r[0] for r in source_rows] == [s["ref"] for s in parent["sources"]]
    assert [r[-1] for r in source_rows] == [s["url"] for s in parent["sources"]]
    tables = re.split(r"\n### \d+\. ", document)[1:]
    assert len(tables) == len(records)
    for table, record in zip(tables, records):
        rows = [markdown_cells(l) for l in table.splitlines()
                if re.match(r"^\| \d+ \|", l)]
        assert len(rows) == len(record["evidence"])
        for row, entry in zip(rows, record["evidence"]):
            assert row == [str(entry["row"]), entry["quantity"], entry["value_as_stated"],
                           entry.get("as_written_in_document", entry["quote_cell"]),
                           entry["ref"] or "-", entry["class"]]
        assert sum("UNVERIFIED" in row[3] for row in rows) == sum(
            e["unverified"] for e in record["evidence"])
    for gate in parent["gate_review"]:
        for field in FIELDS:
            assert gate[field]["provisional_value_as_given"] in document
            assert gate[field]["cell_as_given"] in document
    # Four passages restated: definition, access limit, S2 title, Whale absence line.
    assert len(parent["as_written_in_document"]) + sum(
        "as_written_in_document" in s for s in parent["sources"]) + sum(
        "as_written_in_document" in r for r in records) == 4
    assert all(v in document for v in parent["as_written_in_document"].values())
    for source in parent["sources"]:
        assert source.get("as_written_in_document", source["title"]) in document


def test_pack_line_caps_and_json_loading():
    files = list(DIRECTORY.glob(BASE + "*")) + list(
        (ROOT / "tests/docs").glob("test_field_development_source_evidence_1794*.py"))
    for path in files:
        assert len(path.read_text().splitlines()) <= 400, path.name
        if path.suffix == ".json":
            json.loads(path.read_text())


@pytest.mark.parametrize("defect", ["missing-quantity", "bad-ref", "wrong-unverified"])
def test_evidence_validator_rejects_synthetic_entry(defect):
    entry = {"row": 1, "quantity": "synthetic quantity",
             "value_as_stated": "synthetic value", "quote_cell": "synthetic fragment",
             "ref": "synthetic-source", "class": "operator", "unverified": False}
    if defect == "missing-quantity":
        del entry["quantity"]
    elif defect == "bad-ref":
        entry["ref"] = "missing-source"
    else:
        entry["unverified"] = True
    with pytest.raises(AssertionError):
        validate_evidence_entry(deepcopy(entry), {"synthetic-source"})


def synthetic_candidate():
    """Return a synthetic candidate without real development values."""
    field = {"value": "synthetic value", "definition": "synthetic definition",
             "evidence_rows": [1]}
    record = {name: deepcopy(field) for name in
              ("num_trees", "num_manifolds", "cost_usd_bn")}
    record["record"] = "synthetic candidate"
    record["cost_usd_bn"].update(year="synthetic year", currency="synthetic currency",
                               scope="synthetic scope")
    record["tieback_distance_km"] = None
    return record


def synthetic_inputs():
    """Return synthetic source and evidence objects only."""
    return [{"ref": "synthetic-source"}], [
        {"row": 1, "ref": "synthetic-source", "class": "operator",
         "unverified": False}]


def test_synthetic_complete_and_null_tieback():
    record = synthetic_candidate()
    sources, evidence = synthetic_inputs()
    assert acceptance_reasons(record, sources, evidence) == []
    record["tieback_distance_km"] = deepcopy(record["num_trees"])
    assert acceptance_reasons(record, sources, evidence) == []


@pytest.mark.parametrize("defect", [
    "missing-row", "unverified", "secondary", "contractor-label",
    "cost-year", "cost-scope", "cost-currency", "tieback-definition",
    "missing-value", "missing-definition", "missing-source",
])
def test_synthetic_rejected(defect):
    record = synthetic_candidate()
    sources, evidence = synthetic_inputs()
    if defect == "missing-row":
        record["num_trees"]["evidence_rows"] = [2]
    elif defect in {"unverified", "secondary", "contractor-label"}:
        if defect == "unverified":
            evidence[0]["unverified"] = True
        else:
            evidence[0]["class"] = (
                "secondary" if defect == "secondary" else "contractor")
    elif defect.startswith("cost-"):
        del record["cost_usd_bn"][defect.removeprefix("cost-")]
    elif defect == "tieback-definition":
        record["tieback_distance_km"] = deepcopy(record["num_trees"])
        del record["tieback_distance_km"]["definition"]
    elif defect == "missing-source":
        sources.clear()
    else:
        del record["num_trees"][defect.removeprefix("missing-")]
    assert acceptance_reasons(record, sources, evidence)


def test_synthetic_contractor_label():
    record = synthetic_candidate()
    sources, evidence = synthetic_inputs()
    evidence[0]["class"] = "contractor"
    for name in ("num_trees", "num_manifolds"):
        record[name]["label"] = "as-contracted"
    assert acceptance_reasons(record, sources, evidence) == []


def field_evidence_reasons(name, field, source_refs, evidence_by_row):
    """Check evidence provenance and count labels without fetching sources."""
    reasons = []
    valid = []
    for number in field.get("evidence_rows", []):
        row = evidence_by_row.get(number)
        if row is None:
            reasons.append(f"{name}: missing evidence row")
        elif row.get("unverified") is not False or row.get("class") == "unverified":
            reasons.append(f"{name}: unverified evidence")
        elif row.get("class") == "secondary":
            reasons.append(f"{name}: secondary evidence")
        elif row.get("class") not in {"operator", "contractor", "filing", "regulator", "paper"}:
            reasons.append(f"{name}: unstated evidence class")
        elif row.get("ref") not in source_refs:
            reasons.append(f"{name}: unresolved source")
        else:
            valid.append(row)
    if valid:
        reasons = []
    else:
        reasons.append(f"{name}: no eligible evidence")
    if name in {"num_trees", "num_manifolds"} and any(
        r.get("class") == "contractor" for r in valid
    ) and field.get("label") != "as-contracted":
        reasons.append(f"{name}: contractor count lacks as-contracted label")
    return reasons


def acceptance_reasons(candidate, sources, evidence):
    """Return missing-evidence reasons under the recorded acceptance choice."""
    reasons = []
    source_refs = {s["ref"] for s in sources}
    evidence_by_row = {e["row"]: e for e in evidence}
    for name in ("num_trees", "num_manifolds", "cost_usd_bn", "tieback_distance_km"):
        field = candidate.get(name)
        if name == "tieback_distance_km" and (
            field is None or isinstance(field, dict) and field.get("value") is None
        ):
            continue
        if not isinstance(field, dict):
            reasons.append(f"{name}: missing field")
            continue
        if field.get("value") is None or field.get("value") == "":
            reasons.append(f"{name}: missing value")
        if not isinstance(field.get("definition"), str) or not field["definition"].strip():
            reasons.append(f"{name}: missing stated definition")
        reasons.extend(field_evidence_reasons(name, field, source_refs, evidence_by_row))
        if name == "cost_usd_bn":
            for key in ("year", "currency", "scope"):
                if not field.get(key):
                    reasons.append(f"{name}: missing {key}")
    return reasons
