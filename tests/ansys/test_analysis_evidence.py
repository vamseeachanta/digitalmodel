"""Offline package boundary tests; fixtures are synthetic, never qualifications."""
import copy
import json
from pathlib import Path

import pytest

from digitalmodel.ansys.analysis_evidence import (
    build_package, canonical_bytes, digest_bytes, load_package,
    parse_digest, publish_package,
)


@pytest.fixture
def intake(tmp_path):
    raw = tmp_path / "capture.csv"
    raw.write_text("stress,12.5,displacement,0.0")
    evidence = {"id": "capture", "sha256": digest_bytes(raw.read_bytes()),
                "role": "output", "required": True}
    case = {
        "case_id": "case-1", "component_id": "plate", "model_revision": "model-1",
        "parameters": {"thickness_mm": "10"}, "author": "fixture-author",
        "author_status": "recorded", "superseded_by": [], "capture_role": "fixture",
        "source_kind": "synthetic", "execution_status": "completed",
        "retention_rights": "approved", "use_rights": "unresolved",
        "generated_at": "2026-09-13T00:00:00Z",
        "input_descriptor": {"load_basis": "test only", "source_revision": "fixture-1",
                             "solver": "none", "frame": "fixture", "dependencies": []},
        "evidence": [evidence],
        "responses": [{"name": "stress", "definition": "test scalar",
                       "location": "fixture", "unit": "MPa", "value": "12.5",
                       "calculation_status": "computed", "limitations": ["synthetic"],
                       "inherited_findings": [],
                       "evidence_ids": ["capture"]}],
    }
    study = {"analysis_id": "analysis-1", "dataset_id": "dataset-1", "revision": "r1",
             "criteria_revision": "ENG-ANALYSIS-CRITERIA:1", "code_revision": "code-1",
             "method_revision": "method-1", "expected_cases": ["case-1"],
             "intended_uses": ["screening"], "cases": [case]}
    ref = {"id": "capture", "sha256": evidence["sha256"]}
    study.update(finding_ledger=[], criteria_reference=ref, intake_reference=ref, review_sources=[])
    return study, {"capture": raw}


@pytest.mark.parametrize("text", ["", "a,", "a,1,a,2", "a,1,A,2",
                                      "a,nan", "a,inf", "a,1,,2", "a,1,b"])
def test_digest_rejects_ambiguous_or_nonfinite_input(text):
    with pytest.raises(ValueError):
        parse_digest(text, {"a"})


def test_digest_requires_fields_and_preserves_true_zero():
    assert parse_digest("a,0,b,2.", {"a", "b"}) == {"a": "0", "b": "2"}
    with pytest.raises(ValueError):
        parse_digest("a,1", {"a", "b"})


def test_canonical_encoding_separates_types_and_field_boundaries():
    inputs = [{}, {"x": None}, {"x": ""}, {"x": "0"}, {"x": 0},
              {"x": "ab", "y": "c"}, {"x": "a", "y": "bc"}]
    assert len({canonical_bytes(x) for x in inputs}) == len(inputs)
    with pytest.raises(ValueError):
        canonical_bytes({"x": float("nan")})


@pytest.mark.parametrize("damage", ["missing", "duplicate", "offgrid", "nonfinite",
                                     "empty_response", "duplicate_response", "rights"])
def test_package_rejects_incomplete_or_ambiguous_cases(intake, damage):
    study, resolver = intake
    case = study["cases"][0]
    if damage == "missing":
        study["expected_cases"].append("missing-case")
    elif damage == "duplicate":
        study["cases"].append(copy.deepcopy(case))
    elif damage == "offgrid":
        case["parameters"]["thickness_mm"] = "10.0"
    elif damage == "nonfinite":
        case["responses"][0]["value"] = "NaN"
    elif damage == "empty_response":
        case["responses"] = []
    elif damage == "duplicate_response":
        case["responses"] *= 2
    else:
        case["retention_rights"] = "unresolved"
    with pytest.raises(ValueError):
        build_package(study, resolver)


@pytest.mark.parametrize("damage", ["changed", "missing"])
def test_import_binds_actual_retained_bytes(intake, damage):
    study, resolver = intake
    if damage == "changed":
        resolver["capture"].write_text("changed")
    else:
        resolver["capture"].unlink()
    with pytest.raises(ValueError):
        build_package(study, resolver)


def test_roundtrip_is_immutable_and_metadata_changes_hash(intake, tmp_path):
    study, resolver = intake
    first = build_package(study, resolver)
    study["cases"][0]["input_descriptor"]["load_basis"] = "different basis"
    second = build_package(study, resolver)
    assert first["cases"][0]["row_hash"] != second["cases"][0]["row_hash"]
    output = publish_package(first, tmp_path / "datasets")
    assert load_package(output) == first
    assert json.loads(output.read_text()) == first
    with pytest.raises(FileExistsError):
        publish_package(second, tmp_path / "datasets")


def test_size_failure_publishes_nothing(intake, tmp_path):
    package = build_package(*intake)
    root = tmp_path / "datasets"
    with pytest.raises(ValueError, match="size"):
        publish_package(package, root, revision_limit=5)
    assert not list(root.rglob("*.json"))


def test_tampered_package_cannot_be_loaded(intake, tmp_path):
    path = publish_package(build_package(*intake), tmp_path / "datasets")
    data = json.loads(path.read_text())
    data["cases"][0]["responses"][0]["value"] = "0"
    path.write_text(json.dumps(data))
    with pytest.raises(ValueError, match="digest"):
        load_package(path)


def test_csv_roundtrip_preserves_null_zero_and_literal_text(intake):
    from digitalmodel.ansys.analysis_evidence import response_csv, read_response_csv
    study, resolver = intake
    response = study["cases"][0]["responses"][0]
    response["value"] = None
    response["calculation_status"] = "not_evaluated"
    response["limitations"] = ["NULL", "missing source"]
    package = build_package(study, resolver)
    rows = read_response_csv(response_csv(package))
    assert rows[0]["response"] == response
    response["value"] = "0"
    response["calculation_status"] = "computed"
    rows = read_response_csv(response_csv(build_package(study, resolver)))
    assert rows[0]["response"]["value"] == "0"


def test_case_identity_cannot_duplicate_under_different_ids(intake):
    study, resolver = intake
    second = copy.deepcopy(study["cases"][0])
    second["case_id"] = "second"
    study["cases"].append(second)
    study["expected_cases"].append("second")
    with pytest.raises(ValueError, match="identity"):
        build_package(study, resolver)


def test_published_revision_requires_portable_ids(intake, tmp_path):
    study, resolver = intake
    study["revision"] = "../escape"
    with pytest.raises(ValueError):
        build_package(study, resolver)


def test_rehashed_incomplete_package_still_fails_schema(intake, tmp_path):
    package = build_package(*intake)
    package["expected_cases"].append("missing")
    del package["package_hash"]
    package["package_hash"] = digest_bytes(canonical_bytes(package))
    with pytest.raises(ValueError, match="cases"):
        publish_package(package, tmp_path / "datasets")


def test_digest_preserves_high_precision_without_context_rounding():
    number = "1.123456789012345678901234567890123456789"
    assert parse_digest("a," + number, {"a"})["a"] == number
