"""Defect regressions from independent Claude Stage A review."""
import copy
import hashlib
import csv
import io
import json

import pytest

from digitalmodel.ansys.analysis_intake import _make_case
from digitalmodel.ansys.analysis_evidence import build_package, publish_package, response_csv, read_response_csv, parse_digest
from digitalmodel.ansys.analysis_lookup import lookup
from digitalmodel.ansys.analysis_records import safe_id
from tests.ansys.test_analysis_evidence import intake
from tests.ansys.test_analysis_qualification import protocol
from tests.ansys.test_analysis_lookup import query


@pytest.fixture
def candidate(tmp_path):
    source = tmp_path / "capture.csv"
    source.write_bytes(b"stress,12")
    refs = [{"id": "capture", "sha256": hashlib.sha256(source.read_bytes()).hexdigest(),
             "role": "output", "required": True}]
    record = {"candidate_id": "ansys-fixture", "family": "test",
        "source_kind": "synthetic", "execution_status": "dry_run", "author": "unknown", "author_status": "unknown",
        "retention_rights": "approved", "use_rights": "unresolved", "role": "test",
        "superseded_by": [], "inherited_findings": [],
        "model_basis": {"parameters": {"x": "1"}, "load": "test", "idealization": "test"},
        "disposition_reason": "isolated synthetic test"}
    return record, refs, {"capture": source}


def test_id_prefix_does_not_determine_source_or_execution(candidate):
    record, refs, resolver = candidate
    case = _make_case(record, refs, {"stress": "MPa"}, "capture", resolver, "test")
    assert case["source_kind"] == "synthetic"
    assert case["execution_status"] == "dry_run"
    assert case["author"] == "unknown"


@pytest.mark.parametrize("field", ["source_kind", "execution_status", "author", "retention_rights"])
def test_missing_intake_classification_refuses(candidate, field):
    record, refs, resolver = candidate
    del record[field]
    with pytest.raises(ValueError):
        _make_case(record, refs, {"stress": "MPa"}, "capture", resolver, "test")


def test_unresolved_retention_cannot_be_imported(candidate):
    record, refs, resolver = candidate
    record["retention_rights"] = "unresolved"
    with pytest.raises(ValueError, match="retention"):
        _make_case(record, refs, {"stress": "MPa"}, "capture", resolver, "test")


@pytest.mark.parametrize("damage", ["unknown_author", "superseded", "open_finding"])
def test_unknown_author_supersession_and_open_findings_refuse(protocol, damage):
    study, resolver, _, ledger, authority = protocol
    case = study["cases"][0]
    if damage == "unknown_author":
        case["author"] = "unknown"
    elif damage == "superseded":
        case["superseded_by"] = ["corrected"]
    else:
        case["responses"][0]["inherited_findings"] = ["unmapped-finding"]
    package = build_package(study, resolver)
    authority["revisions"] = {"r1": package["package_hash"]}
    # Failure must be the new provenance/finding gate, before stale row verification.
    with pytest.raises(ValueError, match="author|superseded|finding"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)


def test_cumulative_failure_preserves_prior_revision_and_cleans_lock(intake, tmp_path):
    study, resolver = intake
    package = build_package(study, resolver)
    path = publish_package(package, tmp_path)
    saved = path.read_bytes()
    study["revision"] = "r2"
    next_package = build_package(study, resolver)
    with pytest.raises(ValueError, match="cumulative"):
        publish_package(next_package, tmp_path, cumulative_limit=len(saved))
    assert list(tmp_path.rglob("*.json")) == [path]
    assert path.read_bytes() == saved
    assert not list(tmp_path.rglob("*.pending"))
    assert not list(tmp_path.rglob(".publish.lock"))


@pytest.mark.parametrize("name", ["COM1", "COM9.txt", "LPT1", "LPT9.json"])
def test_portable_ids_exclude_windows_device_names(name):
    with pytest.raises(ValueError):
        safe_id(name)


def test_import_verifies_top_level_criteria_reference(intake):
    study, resolver = intake
    study["criteria_reference"] = {"id": "capture", "sha256": "0" * 64}
    with pytest.raises(ValueError, match="changed"):
        build_package(study, resolver)


@pytest.mark.parametrize("damage", ["header", "short_row", "duplicate", "noncanonical", "value"])
def test_csv_refuses_damaged_export(intake, damage):
    rows = list(csv.reader(io.StringIO(response_csv(build_package(*intake)))))
    if damage == "header":
        rows[0][0] = "unsupported"
    elif damage == "short_row":
        rows[1].pop()
    elif damage == "duplicate":
        rows.append(rows[1])
    elif damage == "noncanonical":
        rows[1][-1] = " " + rows[1][-1]
    else:
        rows[1][3] = "99"
    stream = io.StringIO()
    csv.writer(stream).writerows(rows)
    with pytest.raises(ValueError):
        read_response_csv(stream.getvalue())


def test_authority_must_bind_exact_revision_hash(protocol):
    _, resolver, package, ledger, authority = protocol
    authority["revisions"]["r1"] = "0" * 64
    with pytest.raises(ValueError, match="bound"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)


def test_code_fingerprint_is_portable_across_checkout_newlines(tmp_path):
    from digitalmodel.ansys.analysis_intake import code_fingerprint
    names = ["analysis_records.py", "analysis_evidence.py", "analysis_lookup.py", "analysis_intake.py"]
    for name in names:
        (tmp_path / name).write_bytes(b"first\nsecond\n")
    lf = code_fingerprint(tmp_path)
    for name in names:
        (tmp_path / name).write_bytes(b"first\r\nsecond\r\n")
    assert code_fingerprint(tmp_path) == lf


@pytest.mark.parametrize("field", ["finding_ledger", "criteria_reference", "intake_reference",
    "review_sources", "superseded_by", "capture_role", "inherited_findings", "author_status"])
def test_missing_provenance_constraints_refuse(intake, field):
    study, resolver = intake
    target = study if field in study else study["cases"][0]
    if field == "inherited_findings":
        target = study["cases"][0]["responses"][0]
    del target[field]
    with pytest.raises(ValueError):
        build_package(study, resolver)


def test_finding_resolution_requires_live_response_specific_evidence(protocol):
    study, resolver, _, ledger, authority = protocol
    study["cases"][0]["responses"][0]["inherited_findings"] = ["finding-1"]
    study["finding_ledger"] = [{"finding": "finding-1", "disposition": "open", "affected_responses": ["*"]}]
    preliminary = build_package(study, resolver)
    register = json.loads(resolver["register"].read_text())
    register["entries"]["case-1:stress"]["row_hash"] = preliminary["cases"][0]["row_hash"]
    resolver["register"].write_text(json.dumps(register))
    ref = {"id": "register", "sha256": hashlib.sha256(resolver["register"].read_bytes()).hexdigest()}
    study["verification_reference"] = ref
    package = build_package(study, resolver)
    authority.update(verification_reference=ref, revisions={"r1": package["package_hash"]})
    with pytest.raises(ValueError, match="finding"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)
    authority["finding_dispositions"] = {"case-1:stress": {"finding-1": {
        "disposition": "resolved", "justification": "isolated protocol fixture",
        "evidence": [study["criteria_reference"]]}}}
    assert lookup(package, query(), resolver, ledger=ledger, authority=authority)["qualified"]
    authority["finding_dispositions"]["case-1:stress"]["finding-1"]["evidence"] = []
    with pytest.raises(ValueError, match="evidence"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)


def test_declared_physical_input_requires_matching_evidence(candidate):
    record, refs, resolver = candidate
    record["input_sha256"] = "0" * 64
    with pytest.raises(ValueError, match="input"):
        _make_case(record, refs, {"stress": "MPa"}, "capture", resolver, "test")


def test_underscore_is_not_a_numeric_digest_literal():
    with pytest.raises(ValueError):
        parse_digest("a,1_0", {"a"})
