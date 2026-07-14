"""Regression tests for the Step 5 result-policy rereview."""

import json
from dataclasses import replace
from pathlib import Path
from types import MappingProxyType
from urllib.parse import quote

import pytest

from digitalmodel.workflows import openfoam_batch_results as results

EXPECTED_PROJECTED = {
    "index": 1, "name": "case-a", "status": "completed",
    "solver": "simpleFoam", "mock": True, "wall_seconds": 1.25,
    "courant_number": 0.5, "solver_app": "simpleFoam",
    "mpi_plan": "<redacted>", "args": "<redacted>",
    "error_message": "<redacted>", "standard_error": "<redacted>",
    "solver_log": "<redacted>", "mixed_percent": "<redacted>",
    "mixed_unicode": "<redacted>", "partial_encoding": "<redacted>",
    "double_encoding": "<redacted>", "ambiguous_encoding": "<redacted>",
}


def test_external_row_projection_drops_nested_and_redacts_all_diagnostic_aliases():
    root = Path("/private/operator/openfoam")
    mixed_percent = "%2Fprivate%2foperator%2Fopenfoam%2frun"
    mixed_unicode = r"\u002fprivate\u002Foperator\u002fopenfoam\u002Frun"
    partial = "%2Fprivate/operator/openfoam/run"
    double_encoded = quote(quote(str(root / "run"), safe=""), safe="")
    ambiguous = quote(quote(quote(quote(str(root), safe=""), safe=""), safe=""), safe="")
    row = {
        "index": 1,
        "name": "case-a",
        "status": "completed",
        "solver": "simpleFoam",
        "mock": True,
        "wall_seconds": 1.25,
        "courant_number": 0.5,
        "solver_app": "simpleFoam",
        "mpi_plan": ["mpirun", "--token=secret"],
        "args": ["--token=secret"],
        "error_message": "private failure diagnostic",
        "standard_error": "private stderr diagnostic",
        "solver_log": "private log diagnostic",
        "unknown_nested": {"safe-looking": 1, "path": str(root)},
        "mixed_percent": mixed_percent,
        "mixed_unicode": mixed_unicode,
        "partial_encoding": partial,
        "double_encoding": double_encoded,
        "ambiguous_encoding": ambiguous,
    }

    projected = results.redact_external_row(row, root)
    encoded = json.dumps(projected, sort_keys=True)

    assert projected == EXPECTED_PROJECTED
    for secret in (
        str(root), "private", "--token=secret", mixed_percent, mixed_unicode,
        partial, double_encoded, ambiguous,
    ):
        assert secret not in encoded


@pytest.mark.parametrize(
    "changes",
    [
        {"basename": r"..\escape.json"},
        {"basename": "../escape.json"},
        {"basename": ".artifact.json"},
        {"basename": "artifact..json"},
        {"max_bytes": -1},
        {"max_bytes": 0},
        {"max_bytes": True},
        {"max_bytes": 1.0},
        {"policy_version": "wrong-policy"},
        {"schema_id": "wrong-schema-v1"},
        {"media_type": "text/plain"},
        {"extension_id": "wrong-extension-v1"},
        {"active": 1},
    ],
)
def test_registry_validates_every_inactive_record_field(monkeypatch, changes):
    record = results.RESULT_EXTENSION_REGISTRY["openfoam-artifact-index-v1"]
    invalid = replace(record, **changes)
    monkeypatch.setattr(
        results,
        "RESULT_EXTENSION_REGISTRY",
        MappingProxyType({"openfoam-artifact-index-v1": invalid}),
    )
    with pytest.raises(ValueError, match="result extension registry"):
        results.active_result_extensions()


@pytest.mark.parametrize(
    "basename, media_type",
    [
        ("artifact.txt", None),
        ("artifact.JSON", None),
        ("artifact.JSON", "application/json"),
        ("artifact", None),
    ],
)
def test_registry_requires_supported_lowercase_suffix_and_exact_media(
    monkeypatch, basename, media_type
):
    record = results.RESULT_EXTENSION_REGISTRY["openfoam-artifact-index-v1"]
    invalid = replace(record, basename=basename, media_type=media_type)
    monkeypatch.setattr(
        results,
        "RESULT_EXTENSION_REGISTRY",
        MappingProxyType({record.extension_id: invalid}),
    )
    with pytest.raises(ValueError, match="result extension registry"):
        results.active_result_extensions()


@pytest.mark.parametrize("basename", ["Cases.csv", "Batch_Summary.json"])
def test_registry_rejects_casefold_collision_with_mandatory(monkeypatch, basename):
    record = results.RESULT_EXTENSION_REGISTRY["openfoam-artifact-index-v1"]
    media = "text/csv" if basename.endswith(".csv") else "application/json"
    invalid = replace(record, basename=basename, media_type=media)
    monkeypatch.setattr(
        results,
        "RESULT_EXTENSION_REGISTRY",
        MappingProxyType({record.extension_id: invalid}),
    )
    with pytest.raises(ValueError, match="result extension registry"):
        results.active_result_extensions()


def test_registry_rejects_casefold_duplicate_basenames(monkeypatch):
    first = results.RESULT_EXTENSION_REGISTRY["openfoam-artifact-index-v1"]
    first = replace(first, basename="Artifact.json")
    second = replace(
        first,
        extension_id="openfoam-secondary-index-v1",
        schema_id="openfoam-secondary-index-v1",
        basename="artifact.json",
    )
    monkeypatch.setattr(
        results,
        "RESULT_EXTENSION_REGISTRY",
        MappingProxyType({first.extension_id: first, second.extension_id: second}),
    )
    with pytest.raises(ValueError, match="result extension registry"):
        results.active_result_extensions()
