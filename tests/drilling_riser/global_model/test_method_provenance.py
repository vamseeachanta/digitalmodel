"""Provenance qualification is independent of numerical agreement."""

from __future__ import annotations

import inspect
import json
from pathlib import Path

import pytest

from digitalmodel.citations.schema import CitationResolutionError

from digitalmodel.drilling_riser.global_model import hand_checks


def test_sidecar_covers_every_public_hand_check_without_claiming_compliance():
    """The inventory is checked against the public calculations actually shipped.

    Empty citations mean no standards-derived method has been qualified; they
    must not be interpreted as a successful standards-resolution check.
    """
    sidecar = hand_checks.hand_check_provenance()
    functions = {
        name for name, fn in inspect.getmembers(hand_checks, inspect.isfunction)
        if not name.startswith("_") and fn.__module__ == hand_checks.__name__
    }
    assert set(sidecar["methods"]) == functions
    assert sidecar["standards_status"] == "NOT_EVALUATED"
    assert sidecar["citations"] == []
    assert json.loads(json.dumps(sidecar)) == sidecar
    for row in sidecar["methods"].values():
        assert row["equation"]
        assert row["units"]
        assert row["assumptions"]
        assert row["reference"]
        assert row["expected_value_source"]
        assert row["standards_status"] == "NOT_EVALUATED"


def test_missing_standard_metadata_prevents_a_qualified_request():
    """No edition/clause applicability evidence exists for these mechanics methods."""
    sidecar = hand_checks.hand_check_provenance()
    for candidate in sidecar["standard_candidates"]:
        assert candidate["revision"] is None
        assert candidate["clause_title"] is None
        assert candidate["section"] is None
        assert candidate["status"] == "NOT_EVALUATED"
    with pytest.raises(CitationResolutionError, match="NOT_EVALUATED.*clause title"):
        hand_checks.hand_check_provenance(require_standard=True)


def test_sidecar_references_resolve_to_the_shipped_method_note():
    """Source locators must resolve independently of a private wiki installation."""
    root = Path(__file__).resolve().parents[3]
    for row in hand_checks.hand_check_provenance()["methods"].values():
        for field in ("reference", "expected_value_source"):
            path, anchor = row[field].split("#")
            assert f'id="{anchor}"' in (root / path).read_text(encoding="utf-8")


def test_callers_cannot_mutate_later_provenance_results():
    first = hand_checks.hand_check_provenance()
    first["methods"]["submerged_weight_n"]["standards_status"] = "PASS"
    first["standard_candidates"][0]["revision"] = "unverified"
    assert hand_checks.hand_check_provenance()["standards_status"] == "NOT_EVALUATED"
    assert hand_checks.hand_check_provenance()["standard_candidates"][0]["revision"] is None
