# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Guard the public evidence boundary required by issue #2539 (Acceptance).

Expected identities and unevaluated checks come from issue #2539 and the
diffraction knowledge pack, section 10, U5. These are documentation contract
tests, not numerical validation or inspection of private benchmark evidence.
"""

import json
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[2]
DOCS = ROOT / "src/digitalmodel/hydrodynamics/diffraction/docs"
RECORD = DOCS / "benchmark-evidence-readiness.json"
NOTE = DOCS / "benchmark-evidence-readiness.md"

# Source: issue #2539, Acceptance; no engineering acceptance limits are added.
REQUIRED_CHECKS = {
    "source_identity",
    "manifest_identity",
    "revision_identity",
    "sha256_digest_agreement",
    "independent_review",
    "intended_use_eligibility",
}


@pytest.fixture
def record():
    return json.loads(RECORD.read_text(encoding="utf-8"))


def test_public_record_has_only_identity_and_disposition_fields(record):
    """Private evidence belongs in its owning repository, per #2539 Scope."""
    assert set(record) == {
        "dataset_id", "revision", "readiness_disposition", "checks",
    }
    assert set(record["checks"]) == REQUIRED_CHECKS
    assert all(isinstance(status, str) for status in record["checks"].values())


def test_r2_is_unverified_without_private_acceptance_evidence(record):
    """An earlier issue's closure cannot qualify r2; #2539 Acceptance says so."""
    assert record["dataset_id"] == "diffraction-matched-benchmark"
    assert record["revision"] == "r2"
    assert record["readiness_disposition"] == "UNVERIFIED"


@pytest.mark.parametrize("check", sorted(REQUIRED_CHECKS))
def test_missing_private_evidence_is_not_evaluated(record, check):
    """Unknown evidence must not become a passing check or a measured failure."""
    assert record["checks"][check] == "NOT_EVALUATED"


def test_acceptance_table_matches_public_check_dispositions(record):
    """The human acceptance table must not contradict the public record."""
    rows = [
        line.split("|") for line in NOTE.read_text(encoding="utf-8").splitlines()
        if line.startswith("| `")
    ]
    keys = [row[1].strip().strip("`") for row in rows]
    assert len(keys) == len(set(keys)), "Duplicate acceptance checks"
    assert {key: row[2].strip() for key, row in zip(keys, rows)} == record["checks"]
    assert all(row[3].strip() for row in rows), "Missing private evidence requirement"


def test_pack_routes_unverified_u5_to_the_acceptance_record():
    """Readers must reach the acceptance record from the existing open item."""
    pack = (DOCS / "agent-pack.md").read_text(encoding="utf-8")
    u5 = next(line for line in pack.splitlines() if line.startswith("| U5 "))
    assert "UNVERIFIED" in u5
    assert "#2539" in u5
    assert "(benchmark-evidence-readiness.md)" in u5
    note = NOTE.read_text(encoding="utf-8")
    assert "(benchmark-evidence-readiness.json)" in note
    assert "digitalmodel-data" in note
    assert "NOT_EVALUATED" in note
    assert "UNVERIFIED" in note
