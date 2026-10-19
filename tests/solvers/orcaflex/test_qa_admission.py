"""Admission truth tables from the OrcaFlex agent pack, section 5 (AQG r0).

Each malformed request must be refused once, with no invalid admission; an
unchanged signed control is admitted once. These are logical contract tests,
not calculated engineering values or claims of native qualification.
"""

from dataclasses import replace
import json
from pathlib import Path
from unittest.mock import Mock

import pytest

from digitalmodel.solvers.orcaflex import parallel_runner as runner
from digitalmodel.solvers.orcaflex.qa_admission import AdmissionRefused
from tests.solvers.orcaflex.qa_fixtures import control_fingerprint, make_qa


@pytest.fixture
def control(tmp_path):
    case = {"case_id": "control", "analysis": "statics"}
    qa = make_qa([case], tmp_path)
    return case, qa, qa.requests["control"].record


def admit(case, qa):
    return runner.check_cases([case], qa=qa, expected_dll="test-version")[0]


def test_valid_control_is_admitted_once_without_mutation(control):
    case, qa, _ = control
    assert admit(case, qa) == [case]


@pytest.mark.parametrize("refusal", ("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9"))
def test_each_refusal_condition_rejects_one_request(control, refusal):
    """B1–B9 expected refusals are specified by agent-pack.md section 5."""
    case, qa, record = control
    if refusal == "B1":
        qa = replace(qa, requests={})
    elif refusal == "B2":
        record["input_sha256"] = "changed"
    elif refusal == "B3":
        record["gates"]["G2"]["status"] = "NOT_RUN"
    elif refusal == "B4":
        record["program_version"] = "different-version"
    elif refusal == "B5":
        qa.requests[case["case_id"]].images["static"].write_bytes(b"changed image")
    elif refusal == "B6":
        record["signature"]["reviewer"] = record["producer"]
    elif refusal == "B7":
        record["deviations"] = [{"status": "OPEN"}]
    elif refusal == "B8":
        record["criteria_revision"] = "superseded"
    else:
        record["representatives"] = []
    with pytest.raises(AdmissionRefused) as exc:
        admit(case, qa)
    assert exc.value.condition == refusal


@pytest.mark.parametrize("status", ("FAIL", "NOT_RUN", "NOT_EVALUATED", "", None, True))
def test_required_gate_never_treats_an_unqualified_status_as_pass(control, status):
    case, qa, record = control
    record["gates"]["G1"]["status"] = status
    with pytest.raises(AdmissionRefused, match="B3"):
        admit(case, qa)


@pytest.mark.parametrize("field", ("G0", "G1", "G2", "G3", "G4", "G5"))
def test_no_required_gate_can_be_omitted(control, field):
    case, qa, record = control
    del record["gates"][field]
    with pytest.raises(AdmissionRefused, match="B3"):
        admit(case, qa)


def test_unexplained_not_applicable_is_refused(control):
    case, qa, record = control
    record["gates"]["G4"] = {"status": "NOT_APPLICABLE", "reason": "  "}
    with pytest.raises(AdmissionRefused, match="B3"):
        admit(case, qa)


@pytest.mark.parametrize("target", ("actual-file", "record-digest", "role-register"))
def test_missing_image_evidence_is_refused(control, target):
    case, qa, record = control
    if target == "actual-file":
        # Retain the original control file while requesting an absent one.
        images = {"static": qa.requests[case["case_id"]].images["static"].with_name("absent")}
        request = replace(qa.requests[case["case_id"]], images=images)
        qa = replace(qa, requests={case["case_id"]: request})
    elif target == "record-digest":
        record["images"] = {}
    else:
        qa = replace(qa, required_images=())
    with pytest.raises(AdmissionRefused, match="B5"):
        admit(case, qa)


@pytest.mark.parametrize("field", ("signature", "producer", "deviations", "criteria_revision"))
def test_required_register_fields_cannot_be_omitted(control, field):
    case, qa, record = control
    del record[field]
    expected = {"signature": "B6", "producer": "B6", "deviations": "B7",
                "criteria_revision": "B8"}[field]
    with pytest.raises(AdmissionRefused, match=expected):
        admit(case, qa)


def test_signed_explained_not_applicable_can_be_admitted(control):
    case, qa, record = control
    record["gates"]["G4"] = {"status": "NOT_APPLICABLE", "reason": "No imported data"}
    verifier = Mock(return_value=True)
    qa = replace(qa, verify_signature=verifier)
    assert admit(case, qa) == [case]
    assert b"No imported data" in verifier.call_args.args[0]


def test_unapproved_signature_is_refused(control):
    case, qa, record = control
    record["signature"]["approval"] = "unapproved"
    with pytest.raises(AdmissionRefused, match="B6"):
        admit(case, qa)


def test_unapproved_reviewer_is_refused(control):
    case, qa, record = control
    record["signature"]["reviewer"] = "unapproved-reviewer"
    with pytest.raises(AdmissionRefused, match="B6"):
        admit(case, qa)


def test_record_tampering_invalidates_exact_content_approval(control):
    case, qa, record = control
    record["gates"]["G2"]["note"] = "changed after approval"
    with pytest.raises(AdmissionRefused, match="B6"):
        admit(case, qa)


def test_verifier_error_fails_closed(control):
    case, qa, _ = control
    qa = replace(qa, verify_signature=Mock(side_effect=OSError("unavailable")))
    with pytest.raises(AdmissionRefused, match="B6"):
        admit(case, qa)


@pytest.mark.parametrize("value", (None, "PASS", 1))
def test_signature_verifier_must_return_boolean_true(control, value):
    case, qa, _ = control
    qa = replace(qa, verify_signature=Mock(return_value=value))
    with pytest.raises(AdmissionRefused, match="B6"):
        admit(case, qa)


@pytest.mark.parametrize("field,value", (
    ("configuration", "other"), ("sample", "other"), ("status", "FAIL"),
    ("criteria_revision", "old"), ("program_version", "old"),
    ("evidence_ref", ""), ("evidence_sha256", "invalid"),
))
def test_representative_must_match_requested_qualification(control, field, value):
    case, qa, record = control
    record["representatives"][0][field] = value
    with pytest.raises(AdmissionRefused, match="B9"):
        admit(case, qa)


@pytest.mark.parametrize("value", (None, [], "record", {}))
def test_malformed_record_is_refused(control, value):
    case, qa, _ = control
    request = replace(qa.requests[case["case_id"]], record=value)
    qa = replace(qa, requests={case["case_id"]: request})
    with pytest.raises(AdmissionRefused, match="B1"):
        admit(case, qa)


def test_missing_context_cannot_admit_legacy_requests(control):
    case, _, _ = control
    with pytest.raises(AdmissionRefused, match="B1"):
        runner.check_cases([case])


@pytest.mark.parametrize("case_id", ("trailing-newline\n", None, True))
def test_case_identifier_remains_strict(control, case_id):
    case, qa, _ = control
    with pytest.raises(ValueError, match="case_id"):
        admit({**case, "case_id": case_id}, qa)


def test_nonfinite_case_parameters_are_not_json_evidence(control):
    case, qa, _ = control
    with pytest.raises(ValueError):
        admit({**case, "params": {"value": float("nan")}}, qa)


def test_empty_batch_needs_no_admission_records():
    assert runner.check_cases([]) == ([], {})


def test_whole_batch_is_checked_before_any_output_or_worker(control, tmp_path, monkeypatch):
    case, qa, _ = control
    pool = Mock(side_effect=AssertionError("worker must not start"))
    monkeypatch.setattr(runner, "_make_pool", pool)
    output = tmp_path / "not-created"
    with pytest.raises(AdmissionRefused, match="B1"):
        runner.run_cases([case, {"case_id": "missing", "analysis": "statics"}],
                         adapter="unused:adapter", out_dir=output,
                         expected_dll="test-version", qa=qa, max_workers=1)
    pool.assert_not_called()
    assert not output.exists()


def test_worker_refuses_changed_fingerprint_before_solver_access(control, tmp_path, monkeypatch):
    case, qa, _ = control

    class Adapter:
        def build(self, case, directory):
            directory.mkdir(parents=True)
            master = directory / "master.yml"
            master.write_text("synthetic: changed\n", encoding="utf-8")
            return master

    api = Mock(side_effect=AssertionError("native access is forbidden"))
    monkeypatch.setattr(runner, "_api", api)
    monkeypatch.setattr(runner, "_adapter", lambda _: Adapter())
    record = runner.run_one(
        case, adapter="unused:adapter", out_dir=str(tmp_path / "worker"),
        admitted_input_sha256=qa.requests[case["case_id"]].input_sha256,
        qa_evidence_sha256=qa.validate(case, "test-version"),
    )
    assert record["status"] == "admission_failed"
    assert record["phase"] == "admission"
    assert record["retry"] is False
    assert "B2" in record["message"]
    api.assert_not_called()


def test_unadmitted_worker_preserves_previous_files(control, tmp_path, monkeypatch):
    case, _, _ = control
    previous = tmp_path / "work" / case["case_id"] / "model" / "previous.yml"
    previous.parent.mkdir(parents=True)
    previous.write_text("previous evidence", encoding="utf-8")
    api = Mock(side_effect=AssertionError("native access is forbidden"))
    monkeypatch.setattr(runner, "_api", api)
    record = runner.run_one(case, adapter="unused:adapter", out_dir=str(tmp_path))
    assert record["status"] == "admission_failed"
    assert previous.read_text(encoding="utf-8") == "previous evidence"
    api.assert_not_called()


def test_worker_matches_admission_before_reopen_and_keeps_evidence_identity(control, tmp_path, monkeypatch):
    """Exact bytes produce identical digests; readback is still required.

    The independent control constructs SHA256(``master.yml:SHA256(file)`` plus
    newline plus canonical case JSON), as written out in control_fingerprint.
    No numeric engineering output is asserted by this test.
    """
    case, _, _ = control
    from types import SimpleNamespace

    class Adapter:
        def build(self, case, directory):
            directory.mkdir(parents=True)
            master = directory / "master.yml"
            master.write_text("synthetic: unchanged\n", encoding="utf-8")
            return master

        def extract(self, model, case):
            assert model.reopened is True
            return {"synthetic": "read back"}

    class Model:
        def __init__(self, path):
            self.reopened = Path(path).suffix == ".sim"
            self.state = "static"

        def CalculateStatics(self):  # noqa: N802
            pass

        def SaveSimulation(self, path):  # noqa: N802
            Path(path).write_bytes(b"synthetic saved state")

    master = Adapter().build(case, tmp_path / "independent")
    fingerprint = control_fingerprint(master, case)
    qa = make_qa([case], tmp_path / "qa", {case["case_id"]: fingerprint})
    evidence_digest = qa.validate(case, "test-version")
    api = SimpleNamespace(Model=Model, ModelState=SimpleNamespace(InStaticState="static"))
    monkeypatch.setattr(runner, "_api", lambda *_: (api, "test-version"))
    monkeypatch.setattr(runner, "_adapter", lambda _: Adapter())
    record = runner.run_one(
        case, adapter="unused:adapter", out_dir=str(tmp_path / "worker"),
        expected_dll="test-version", admitted_input_sha256=fingerprint,
        qa_evidence_sha256=evidence_digest, keep_sim=True,
    )
    assert record["status"] == "ok"
    assert record["qa_evidence_sha256"] == evidence_digest
    assert record["input_sha256"] == fingerprint
    result = json.loads((tmp_path / "worker" / record["results_path"]).read_text())
    assert result["qa_evidence_sha256"] == evidence_digest
    repeated = runner.reextract(
        case, adapter="unused:adapter", out_dir=tmp_path / "worker",
        ledger_record=record, expected_dll="test-version",
    )
    assert repeated["status"] == "ok"
    assert repeated["qa_evidence_sha256"] == evidence_digest
    result = json.loads((tmp_path / "worker" / repeated["results_path"]).read_text())
    assert result["qa_evidence_sha256"] == evidence_digest


@pytest.mark.parametrize("bad_state", ("unrun", "unstable", "unknown"))
def test_existing_static_readback_rejects_unqualified_states(bad_state):
    from types import SimpleNamespace

    api = SimpleNamespace(ModelState=SimpleNamespace(InStaticState="static"))
    with pytest.raises(RuntimeError):
        runner._verify(api, SimpleNamespace(state=bad_state), "statics")
