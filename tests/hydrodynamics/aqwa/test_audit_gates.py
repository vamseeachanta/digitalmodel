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
    with pytest.raises(ValueError, match="signature"):
        qa_gates.validate_external_gate(gate, record)
    record["signature"] = signature_record(
        record["input_sha256"], dict(record, artifact=evidence.name))
    assert qa_gates.validate_external_gate(gate, record)["decision"] == "PASS"
    import yaml
    record["signature"].update(yaml.safe_load("signed_at: 2026-10-01T12:00:00+00:00"))
    assert json.loads(json.dumps(qa_gates.validate_external_gate(gate, record)))["decision"] == "PASS"
    for key, value in (("identity", ""), ("signed_at", "invalid"),
                       ("manifest_sha256", "b"*64), ("gates_sha256", "c"*64)):
        changed = dict(record, signature=dict(record["signature"], **{key: value}))
        with pytest.raises(ValueError, match="signature"):
            qa_gates.validate_external_gate(gate, changed)
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


def test_launch_only_returns_handle_and_requires_opt_in(monkeypatch, tmp_path):
    utility = AqwaUtilities()
    handle = SimpleNamespace(pid=123)
    calls = []
    monkeypatch.setattr(utility, "get_aqwa_exe", lambda cfg: "solver")
    monkeypatch.setattr(
        "digitalmodel.hydrodynamics.aqwa.aqwa_utilities.subprocess.Popen",
        lambda *args, **kwargs: calls.append(args) or handle,
    )
    source = tmp_path / "input.dat"
    source.write_text("synthetic")
    with pytest.raises(ValueError, match="execution"):
        utility.run_aqwa_analysis_as_subprocess({}, source, process="launch-only")
    assert not calls
    assert utility.run_aqwa_analysis_as_subprocess(
        {"execution": {"allow_solver": True}}, source, process="launch-only"
    ) is handle
    assert calls[0][0] == ["solver", "/nowind", str(source)]


@pytest.mark.parametrize("configuration_error", [False, True])
def test_ef_router_launches_before_serving_and_propagates_errors(monkeypatch, tmp_path, configuration_error):
    from digitalmodel.hydrodynamics.aqwa import aqwa_analysis_ef_server as module

    events = []
    cfg = {"Analysis": {"file_management_input_directory": str(tmp_path)},
           "file_management": {"input_files": {"DAT": ["input.dat"]}},
           "analysis_settings": {"ef_input": {"number_of_runs": 1,
               "run_function": "UF1", "closing_function": None}}}
    def launch(config, source, process):
        assert process == "launch-only"
        events.append("launch")
        if configuration_error:
            raise ValueError("execution disabled")
        return SimpleNamespace(wait=lambda: events.append("wait") or 0)
    monkeypatch.setattr(module, "fm", SimpleNamespace(router=lambda value: value))
    monkeypatch.setattr(module, "_HAS_AQWA_SERVER", True)
    monkeypatch.setattr(module, "au", SimpleNamespace(run_aqwa_analysis_as_subprocess=launch))
    monkeypatch.setattr(module, "AqwaUserForceServer", lambda **kwargs: SimpleNamespace(
        Run=lambda *args: events.append("serve")), raising=False)
    router = module.AqwaEFServer()
    monkeypatch.setattr(router, "define_result_df", lambda cfg: events.append("define"))
    monkeypatch.setattr(router, "save_result_df", lambda *args: events.append("save"))
    if configuration_error:
        with pytest.raises(ValueError, match="execution disabled"):
            router.ef_server_router(cfg)
        assert events == ["launch"]
    else:
        router.ef_server_router(cfg)
        assert events == ["launch", "define", "serve", "wait", "save"]


def test_ef_router_requires_optional_server(monkeypatch):
    from digitalmodel.hydrodynamics.aqwa import aqwa_analysis_ef_server as module
    monkeypatch.setattr(module, "_HAS_AQWA_SERVER", False)
    with pytest.raises(RuntimeError, match="dependencies unavailable"):
        module.AqwaEFServer().ef_server_router({})


@pytest.mark.parametrize("failure", ["define", "serve"])
def test_ef_router_reaps_solver_on_callback_failure(monkeypatch, tmp_path, failure):
    from digitalmodel.hydrodynamics.aqwa import aqwa_analysis_ef_server as module
    events = []
    cfg = {"Analysis": {"file_management_input_directory": str(tmp_path)},
           "file_management": {"input_files": {"DAT": ["input.dat"]}},
           "analysis_settings": {"ef_input": {"number_of_runs": 1,
               "run_function": "UF1", "closing_function": None}}}
    handle = SimpleNamespace(poll=lambda: None,
        terminate=lambda: events.append("terminate"), wait=lambda: events.append("wait"))
    def callback(stage):
        if stage == failure:
            raise RuntimeError("callback failed")
    monkeypatch.setattr(module, "_HAS_AQWA_SERVER", True)
    monkeypatch.setattr(module, "fm", SimpleNamespace(router=lambda value: value))
    monkeypatch.setattr(module, "au", SimpleNamespace(run_aqwa_analysis_as_subprocess=lambda *a, **k: handle))
    monkeypatch.setattr(module, "AqwaUserForceServer", lambda **kwargs: SimpleNamespace(
        Run=lambda *args: callback("serve")), raising=False)
    router = module.AqwaEFServer()
    monkeypatch.setattr(router, "define_result_df", lambda cfg: callback("define"))
    monkeypatch.setattr(router, "save_result_df", lambda *args: events.append("save"))
    with pytest.raises(RuntimeError, match="callback failed"):
        router.ef_server_router(cfg)
    assert events == ["terminate", "wait"]


def test_saved_external_force_results_disclose_unqualified_completion(tmp_path):
    import pandas as pd
    from digitalmodel.hydrodynamics.aqwa.aqwa_analysis_ef_server import AqwaEFServer
    router = AqwaEFServer()
    router.result_df_array = [pd.DataFrame({"time": [0.0]})]
    cfg = {"Analysis": {"result_folder": str(tmp_path)},
           "analysis_settings": {"ef_input": {"def_pos": [{}]}}}
    router.save_result_df(cfg, "input.dat")
    saved = pd.read_csv(tmp_path / "input_0_py_inputs.csv")
    assert saved["aq_04_decision"].tolist() == ["NOT_RUN"]


def test_signature_accepts_yaml_datetime_and_rejects_naive_dates():
    import yaml
    gates = {f"AQ-{n:02}": {"decision": "PASS"} for n in range(1, 11)}
    manifest = {"dat": {"sha256": "a" * 64}}
    signature = signature_record(manifest, gates)
    signature.update(yaml.safe_load("signed_at: 2026-10-01T12:00:00+00:00"))
    result = qa_gates.validate_report_gates(gates, signature, manifest=manifest)
    assert json.loads(json.dumps(result))["decision"] == "PASS"
    signature.update(yaml.safe_load("signed_at: 2026-10-01T12:00:00"))
    with pytest.raises(ValueError, match="signature"):
        qa_gates.validate_report_gates(gates, signature, manifest=manifest)


def test_native_heading_scan_accepts_non_utf8_listing(monkeypatch, tmp_path):
    utility = AqwaUtilities()
    monkeypatch.setattr(utility, "get_aqwa_exe", lambda cfg: "unused")
    source = tmp_path / "input.dat"
    source.write_text("synthetic")
    def run(*args, **kwargs):
        source.with_suffix(".MES").write_text("Completed successfully")
        source.with_suffix(".LIS").write_bytes(b"\xff\nHYDRODYNAMIC PARAMETERS FOR STRUCTURE 1\n")
        return SimpleNamespace(returncode=0, stdout="", stderr="")
    monkeypatch.setattr("digitalmodel.hydrodynamics.aqwa.aqwa_utilities.subprocess.run", run)
    result = utility.run_aqwa_analysis_as_subprocess(
        {"execution": {"allow_solver": True, "expected_body_count": 1}}, source)
    assert qa_gates.validate_native_result(result)["decision"] == "PASS"
    assert result["input_sha256"]["lis"] == hashlib.sha256(source.with_suffix(".LIS").read_bytes()).hexdigest()


def signature_record(manifest, gates):
    def digest(value):
        return hashlib.sha256(json.dumps(value, sort_keys=True, separators=(",", ":"),
                                         allow_nan=False).encode()).hexdigest()
    return {"identity": "reviewer-123", "role": "maintainer",
            "signed_at": "2026-10-01T12:00:00+00:00",
            "manifest_sha256": digest(manifest), "gates_sha256": digest(gates)}


def test_quality_signature_rejects_bare_role_and_stale_bindings():
    gates = {f"AQ-{n:02}": {"decision": "PASS", "evidence": {"check": n}} for n in range(1, 11)}
    manifest = {"dat": {"file": "input.dat", "sha256": "a"*64}}
    with pytest.raises(ValueError, match="signature"):
        qa_gates.validate_report_gates(gates, "maintainer")
    signature = signature_record(manifest, gates)
    assert qa_gates.validate_report_gates(gates, signature, manifest=manifest)["decision"] == "PASS"
    for key, value in (("identity", ""), ("signed_at", "yesterday"),
                       ("manifest_sha256", "b"*64), ("gates_sha256", "c"*64)):
        with pytest.raises(ValueError, match="signature"):
            qa_gates.validate_report_gates(gates, dict(signature, **{key: value}), manifest=manifest)
    gates["AQ-03"]["evidence"]["check"] = "changed"
    with pytest.raises(ValueError, match="signature"):
        qa_gates.validate_report_gates(gates, signature, manifest=manifest)


def test_runner_output_satisfies_native_gate_and_binds_inputs(monkeypatch, tmp_path):
    utility = AqwaUtilities()
    monkeypatch.setattr(utility, "get_aqwa_exe", lambda cfg: "unused")
    source = tmp_path / "input.dat"
    source.write_text("synthetic")
    lis = source.with_suffix(".LIS")
    def run(*args, **kwargs):
        source.with_suffix(".MES").write_text("Completed successfully")
        lis.write_text("H Y D R O D Y N A M I C   P A R A M E T E R S   F O R   S T R U C T U R E   1\n")
        return SimpleNamespace(returncode=0, stdout="", stderr="")
    monkeypatch.setattr("digitalmodel.hydrodynamics.aqwa.aqwa_utilities.subprocess.run", run)
    result = utility.run_aqwa_analysis_as_subprocess(
        {"execution": {"allow_solver": True, "expected_body_count": 1}}, source)
    bindings = {"dat": hashlib.sha256(source.read_bytes()).hexdigest(),
                "lis": hashlib.sha256(lis.read_bytes()).hexdigest()}
    assert qa_gates.validate_native_result(result, expected_inputs=bindings)["decision"] == "PASS"
    result["body_count"] = 2
    with pytest.raises(ValueError, match="AQ-04"):
        qa_gates.validate_native_result(result, expected_inputs=bindings)


def test_local_gates_require_individual_evidence(tmp_path):
    gates = qa_gates.record_gates()
    assert all(gates[f"AQ-{n:02}"]["decision"] == "NOT_RUN" for n in range(1, 4))
    from tests.hydrodynamics.aqwa.test_audit_workflow import case
    module, dataset, config = case(tmp_path)
    record = module._process_case(dataset, config)["records"][0]
    for n in range(1, 4):
        gate = record["qa_gates"][f"AQ-{n:02}"]
        assert gate["decision"] == "PASS"
        assert gate["checks"] and all(value is True for value in gate["checks"].values())
        assert gate["input_sha256"] == {key: value["sha256"] for key, value in record["input_manifest"].items()}
        assert gate["evidence"]
