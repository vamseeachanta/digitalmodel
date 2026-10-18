"""Seed a violation for each externally evidenced AQWA QA gate."""
import hashlib
import json
from types import SimpleNamespace

import pytest

from digitalmodel.hydrodynamics.aqwa.aqwa_utilities import AqwaUtilities
from digitalmodel.hydrodynamics.aqwa import qa_gates


@pytest.mark.parametrize("gate", [f"AQ-{number:02}" for number in range(5, 11)])
def test_external_gate_requires_qualified_evidence(gate, tmp_path):
    with pytest.raises(ValueError, match=gate):
        qa_gates.validate_external_gate(gate, None)
    evidence = tmp_path / "metrics.json"
    evidence.write_text('{"inspection":"synthetic"}')
    record = {"tool": qa_gates.EXTERNAL_TOOLS[gate], "tool_version": "synthetic-1",
              "reviewer_role": "maintainer", "decision": "PASS",
              "criteria_revision": "AQG-r0", "artifact": str(evidence),
              "sha256": hashlib.sha256(evidence.read_bytes()).hexdigest(),
              "input_sha256": {"dat": "a"*64},
              "checks": {key: True for key in qa_gates.EXTERNAL_CHECKS[gate]}}
    assert qa_gates.validate_external_gate(gate, record)["decision"] == "PASS"
    record["checks"][next(iter(record["checks"]))] = False
    with pytest.raises(ValueError, match=gate):
        qa_gates.validate_external_gate(gate, record)


def test_native_gate_rejects_exit_messages_and_missing_evidence(tmp_path):
    result = {"returncode": 0, "mes_status": "PASS", "errors": [], "warnings": [],
              "body_count": 1, "expected_body_count": 1, "input_sha256": {"dat": "a"*64}}
    assert qa_gates.validate_native_result(result)["decision"] == "PASS"
    for key, value in (("returncode", 1), ("mes_status", "MISSING"), ("warnings", ["warning"]),
                       ("errors", ["error"]), ("body_count", 2)):
        with pytest.raises(ValueError, match="AQ-04"):
            qa_gates.validate_native_result(dict(result, **{key: value}))


def test_runner_requires_explicit_execution_authority():
    with pytest.raises(ValueError, match="execution"):
        AqwaUtilities().run_aqwa_analysis_as_subprocess({}, "unused.dat")


def test_runner_returns_exit_and_mes_without_solver(monkeypatch, tmp_path):
    module = AqwaUtilities()
    monkeypatch.setattr(module, "get_aqwa_exe", lambda cfg: "unused")
    input_file = tmp_path / "input.dat"
    input_file.write_text("synthetic")
    def simulated_run(*args, **kwargs):
        assert kwargs["capture_output"]
        input_file.with_suffix(".MES").write_text("**** TEST ERROR **** seeded violation")
        return SimpleNamespace(returncode=7, stdout="captured", stderr="failure")
    monkeypatch.setattr("digitalmodel.hydrodynamics.aqwa.aqwa_utilities.subprocess.run", simulated_run)
    result = module.run_aqwa_analysis_as_subprocess({"execution": {"allow_solver": True}}, str(input_file))
    assert result["returncode"] == 7
    assert result["stdout"] == "captured"
    assert result["mes_status"] == "FAIL"


def test_gate_appendix_and_digest_manifest(tmp_path):
    source = tmp_path / "input.dat"
    source.write_text("synthetic")
    manifest = qa_gates.input_manifest({"dat": source})
    assert manifest["dat"]["sha256"] == hashlib.sha256(b"synthetic").hexdigest()
    qa_gates.verify_manifest(manifest, {"dat": source})
    source.write_text("changed")
    with pytest.raises(ValueError, match="digest"):
        qa_gates.verify_manifest(manifest, {"dat": source})
    with pytest.raises(ValueError, match="AQ-11"):
        qa_gates.validate_report_gates({})


def test_report_writes_gate_appendix_and_manifest(tmp_path):
    from tests.hydrodynamics.aqwa.test_audit_workflow import case
    module, dataset, config = case(tmp_path)
    result = module._process_case(dataset, config)
    module._generate_case_reports(result, tmp_path)
    text = (tmp_path / "case/case_viscous_damping.html").read_text(encoding="utf-8")
    assert "QA gate appendix" in text
    assert "NOT_RUN" in text
    manifest = json.loads((tmp_path / "case/data/record_manifest.json").read_text())
    assert manifest["inputs"]["dat"]["sha256"]
    assert manifest["linearisation"]["wave_amplitude"]["unit"] == "m"
    assert manifest["citations"]["citations"][0]["type"] == "Citation"
