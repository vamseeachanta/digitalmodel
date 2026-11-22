# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline evidence contracts; synthetic mutations are never standards evidence."""

import ast
from collections import Counter
from copy import deepcopy
import json
from pathlib import Path
import re

import pytest

ROOT = Path(__file__).resolve().parents[2]
AUDIT = ROOT / "docs/standards/riser-citation-audit.json"
INPUTS = ["src/digitalmodel/codes.py", "docs/domains/codes-register.md"] + [
    "src/digitalmodel/structural/analysis/wall_thickness_codes/" + name
    for name in ("api_rp_2rd.py", "api_std_2rd.py", "dnv_st_f201.py", "manifest.yaml")
] + ["src/digitalmodel/structural/fatigue/worked_examples.py"]
CATEGORIES = {"VERSION CONFLICT", "IMPLAUSIBLE CLAUSE", "MISSING VERSION", "DIVERGENT THRESHOLD"}
CITATION = re.compile(
    r"\b(?:DNV(?:GL)?-(?:RP|ST|OS|RU)-[A-Z0-9-]+|DNV CN \d+(?:\.\d+)*|"
    r"API(?:(?: RP| STD| TR)? |-(?:RP|STD)-)\d[A-Z0-9]*(?:-\d+)?|"
    r"ASME[- ](?:B\d+[A-Z]?(?:\.\d+)*|FFS-\d+)|BS \d+|EN \d+(?:-\d+)*|"
    r"ISO \d+(?:-\d+)*|PD \d+(?:-\d+)*|IACS (?:UR [A-Z]\d+[A-Z]?|CSR)|"
    r"Eurocode 3|OTC \d+|(?:RP|STD|Std) 2RD|(?:ST|OS)-F201)\b"
)
CLAUSE = re.compile(r"(?:Sec\.|Cl\.|Table|Fig\.)\s+\d+(?:[.-]\d+)*|S403\.2(?:\.\d+)?")
KEYS = ("designation", "edition", "parameter", "units", "limit_state", "condition", "method")


def canonical(literal):
    aliases = {"RP 2RD": "API RP 2RD", "STD 2RD": "API STD 2RD", "Std 2RD": "API STD 2RD",
               "ST-F201": "DNV-ST-F201", "OS-F201": "DNV-OS-F201",
               "Eurocode 3": "EN 1993-1-1"}
    normal = literal.replace("ASME-", "ASME ").replace("DNVGL-", "DNV-")
    return aliases.get(literal, normal.replace("-", " ") if normal.startswith("API-") else normal)


def validate(data):
    """Reject stale evidence, missing dispositions and unlike threshold claims."""
    assert data["inputs"] == INPUTS, "inputs"
    register = {}
    for node in ast.walk(ast.parse((ROOT / INPUTS[0]).read_text(encoding="utf-8"))):
        if isinstance(node, ast.Assign) and isinstance(node.value, ast.Call):
            if isinstance(node.value.func, ast.Name) and node.value.func.id == "CodeReference":
                for literal in CITATION.findall(ast.literal_eval(node.value.args[0])):
                    register[canonical(literal)] = node.targets[0].id
    observed = Counter()
    for index, path in enumerate(INPUTS):
        assert data["clauses"][str(index)] == sorted(set(CLAUSE.findall((ROOT / path).read_text(encoding="utf-8")))), "clause coverage"
        observed.update((index, match) for match in CITATION.findall((ROOT / path).read_text(encoding="utf-8")))
    rows = data["occurrences"]
    assert Counter((r["input"], r["literal"]) for r in rows) == observed, "occurrence coverage"
    assert len({r["id"] for r in rows}) == len(rows), "occurrence IDs"
    for row in rows:
        text = (ROOT / INPUTS[row["input"]]).read_text(encoding="utf-8")
        assert row["anchor"] and row["anchor"] in text, "anchor"
        assert row["literal"] in row["anchor"], "literal"
        assert row["designation"] == canonical(row["literal"]), "alias"
        assert row["register_key"] == register.get(row["designation"]), "register"
        assert row["register_key"] or row["unmatched"].strip(), "unmatched disposition"
        assert row["status"] in {"reference", "implementation", "historical", "example"}, "status"
        assert isinstance(row["line"], int) and row["line"] > 0, "line"
        assert row["edition"] is None or re.fullmatch(r"\d{4}", row["edition"]), "edition"
        assert row["edition"] is None or row["edition"] in row["anchor"], "edition evidence"
        assert row["clause"] is None or row["clause"] in text, "clause evidence"
    designations = {r["designation"] for r in rows}
    matched = {r["designation"] for r in rows if r["register_key"]}
    assert data["coverage"] == {"matched": len(matched), "total": len(designations),
                                "unmatched": sorted(designations - matched)}, "coverage"
    findings = data["findings"]
    assert {f["category"] for f in findings} == CATEGORIES, "categories"
    assert len({f["id"] for f in findings}) == len(findings), "finding IDs"
    for finding in findings:
        assert finding["evidence"] and set(finding["evidence"]) <= {r["id"] for r in rows}, "finding evidence"
        assert finding["action"].strip() and finding["rationale"].strip(), "disposition"
    missing = {r["id"] for r in rows if r["edition"] is None}
    assert missing <= set().union(*(set(f["evidence"]) for f in findings if f["category"] == "MISSING VERSION")), "missing editions"
    for comparison in data["comparisons"]:
        left, right = comparison["left"], comparison["right"]
        assert all(left[k] and right[k] for k in KEYS), "comparison keys"
        comparable = all(left[k] == right[k] for k in KEYS) and bool(re.fullmatch(r"\d{4}", left["edition"]))
        assert comparison["comparable"] == comparable, "comparability"
        assert comparison["rationale"].strip(), "comparison rationale"


@pytest.fixture
def inventory():
    return json.loads(AUDIT.read_text(encoding="utf-8"))


def test_inventory(inventory):
    validate(inventory)


def test_report():
    report = AUDIT.with_suffix(".md").read_text(encoding="utf-8")
    inventory = json.loads(AUDIT.read_text(encoding="utf-8"))
    for category in CATEGORIES:
        assert "## " + category in report
    for finding in inventory["findings"]:
        assert "| " + finding["id"] + " |" in report
    assert "Revision history" in report
    assert "{matched}/{total}".format(**inventory["coverage"]) in report
    assert "transfer-ledger coverage is not established" in report


@pytest.mark.parametrize("mutation, expected", [
    ("anchor", "anchor"), ("input", "inputs"), ("clause", "clause coverage"), ("disposition", "disposition"),
    ("alias", "alias"), ("coverage", "coverage"), ("omission", "occurrence coverage"),
    ("units", "comparability"), ("condition", "comparability"), ("unverified", "comparability"),
    ("historical", "alias"),
])
def test_synthetic_tampering_is_rejected(inventory, mutation, expected):
    data = deepcopy(inventory)
    if mutation == "anchor":
        data["occurrences"][0]["anchor"] = "synthetic absent anchor"
    elif mutation == "input":
        data["inputs"].pop()
    elif mutation == "clause":
        data["clauses"]["5"].pop()
    elif mutation == "disposition":
        data["findings"][0]["action"] = ""
    elif mutation in ("alias", "historical"):
        before, after = ("API RP 2RD", "API STD 2RD") if mutation == "alias" else ("DNV-OS-F201", "DNV-ST-F201")
        row = next(r for r in data["occurrences"] if r["designation"] == before)
        row["designation"] = after
    elif mutation == "coverage":
        data["coverage"]["total"] -= 1
    elif mutation == "omission":
        data["occurrences"].pop()
    else:
        # Synthetic equal cases first, then a single unlike key.
        comparison = data["comparisons"][0]
        comparison["left"]["edition"] = "2000"
        comparison["right"] = deepcopy(comparison["left"])
        if mutation == "unverified": comparison["left"]["edition"] = comparison["right"]["edition"] = "unverified"
        else: comparison["right"][mutation] = "synthetic different " + mutation
        comparison["comparable"] = True
    with pytest.raises(AssertionError, match=expected):
        validate(data)
