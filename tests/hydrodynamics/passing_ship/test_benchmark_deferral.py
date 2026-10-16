# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Keep missing published references distinct from a benchmark result."""

import hashlib
import json
from pathlib import Path
import re

from digitalmodel.hydrodynamics.passing_ship.preparation_report import build


ROOT = Path(__file__).resolve().parents[3]
DATA = ROOT / "docs/domains/passing_ship/preparation"
STATEMENT = (
    "Benchmark check (i) not run: the published reference gives plots only; "
    "a numeric reference or a digitising ruling is pending."
)


def test_benchmark_deferral_is_generated_from_record(tmp_path):
    """Both generated records carry the data status and its provenance digest."""
    manifest = build(DATA, tmp_path)
    record = json.loads((DATA / "benchmark_status.json").read_text())
    assert record == {
        "check": "i", "status": "not_run", "statement": STATEMENT, "issue": 2017,
    }
    assert manifest["benchmark_check"] == record
    name = "docs/domains/passing_ship/preparation/benchmark_status.json"
    assert manifest["inputs"][name] == hashlib.sha256((ROOT / name).read_bytes()).hexdigest()
    assert STATEMENT in (tmp_path / "report.html").read_text()
    revisions = json.loads((DATA / "revisions.json").read_text())
    assert revisions["revision"] == revisions["history"][-1]["revision"] == "3"
    assert len(revisions["history"]) == 3


def test_unrun_benchmarks_claim_no_pass_count_or_agreement(tmp_path):
    """Input candidates remain unqualified and outputs make no success claim."""
    manifest = build(DATA, tmp_path)
    assert manifest["benchmark_check"]["status"] == "not_run"
    candidates = json.loads((DATA / "benchmark_candidates.json").read_text())
    assert len(candidates["cases"]) == 6
    assert all(row["reference"] is None and row["status"].startswith("NOT QUALIFIED")
               for row in candidates["cases"])
    assert manifest["qualified_benchmarks"] == 0

    def check_keys(value):
        if isinstance(value, dict):
            for key, child in value.items():
                assert not re.search(r"pass(ed)?(_count)?|agreement|deviation", key, re.I)
                check_keys(child)
        elif isinstance(value, list):
            for child in value:
                check_keys(child)

    for key in manifest:
        assert not re.search(r"benchmark.*(?:pass|agreement|deviation)|(?:pass|agreement|deviation).*benchmark", key, re.I)
    check_keys(manifest["benchmark_check"])
    check_keys(candidates)
    page = (tmp_path / "report.html").read_text()
    assert not re.search(r"\b\d+\s+(?:benchmarks?\s+)?passed\b", page, re.I)
    assert not re.search(r"\b(?:reproduces?|matches?|agrees? with)\s+(?:the\s+)?published", page, re.I)
