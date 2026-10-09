"""Strict, local evidence records. Metadata claims do not authenticate provenance."""
from __future__ import annotations

import hashlib
import json
import re
from decimal import Decimal, InvalidOperation
from pathlib import Path
from typing import Any, Mapping


def canonical_bytes(value: Any) -> bytes:
    """Version 1: typed JSON, sorted keys, UTF-8, no binary floats."""
    if isinstance(value, float):
        raise ValueError("binary floats are not canonical evidence values")
    if isinstance(value, dict):
        if any(not isinstance(key, str) for key in value):
            raise ValueError("object keys must be strings")
        for child in value.values():
            canonical_bytes(child)
    elif isinstance(value, list):
        for child in value:
            canonical_bytes(child)
    elif value is not None and not isinstance(value, (str, int, bool)):
        raise ValueError("unsupported canonical type")
    return json.dumps(value, sort_keys=True, separators=(",", ":"),
                      ensure_ascii=False, allow_nan=False).encode("utf-8")


def digest_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def read_json(path: Path) -> dict:
    return parse_json(Path(path).read_bytes())


def parse_json(data: bytes) -> dict:
    def unique(pairs):
        result = {}
        for key, value in pairs:
            if key in result:
                raise ValueError(f"duplicate JSON field: {key}")
            result[key] = value
        return result

    result = json.loads(data, object_pairs_hook=unique)
    if not isinstance(result, dict):
        raise ValueError("expected JSON object")
    canonical_bytes(result)
    return result


def decimal_text(value: str) -> str:
    if not isinstance(value, str) or len(value) > 128:
        raise ValueError("numeric values require bounded decimal strings")
    if not re.fullmatch(r"[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?", value):
        raise ValueError("invalid decimal literal")
    try:
        number = Decimal(value)
    except InvalidOperation as error:
        raise ValueError("invalid decimal") from error
    if not number.is_finite() or abs(number.adjusted()) > 100:
        raise ValueError("nonfinite or excessive decimal")
    result = format(number, "f")
    return "0" if number == 0 else result.rstrip("0").rstrip(".") if "." in result else result


def require_fields(record: dict, fields: set[str], label: str) -> None:
    if not isinstance(record, dict):
        raise ValueError(f"{label} must be an object")
    if not fields.issubset(record):
        raise ValueError(f"missing {label} fields: {sorted(fields - set(record))}")


def nonempty(value: Any, label: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"empty {label}")
    return value


def safe_id(value: str) -> str:
    reserved = {"CON", "PRN", "AUX", "NUL"} | {f"{prefix}{i}" for prefix in ("COM", "LPT") for i in range(1, 10)}
    if (not isinstance(value, str) or len(value) > 100 or value.endswith(".")
            or not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_.-]*", value)
            or value.upper().split(".")[0] in reserved):
        raise ValueError("invalid portable identifier")
    return value


def verify_reference(reference: dict, resolver: Mapping[str, Path]) -> bytes:
    require_fields(reference, {"id", "sha256"}, "evidence")
    identity = nonempty(reference["id"], "evidence id")
    expected = reference["sha256"]
    if not isinstance(expected, str) or not re.fullmatch(r"[0-9a-f]{64}", expected):
        raise ValueError("invalid evidence digest")
    if identity not in resolver:
        raise ValueError(f"evidence unavailable: {identity}")
    try:
        data = Path(resolver[identity]).read_bytes()
    except OSError as error:
        raise ValueError(f"evidence unavailable: {identity}") from error
    if digest_bytes(data) != expected:
        raise ValueError(f"evidence changed: {identity}")
    return data


def validate_response(response: dict, evidence_ids: set[str]) -> None:
    require_fields(response, {"name", "definition", "location", "unit", "value",
        "calculation_status", "limitations", "evidence_ids", "inherited_findings"}, "response")
    string_list(response["inherited_findings"], "inherited findings")
    for field in ("name", "definition", "location", "unit"):
        nonempty(response[field], field)
    status = response["calculation_status"]
    if status not in {"computed", "failed", "not_evaluated", "not_applicable"}:
        raise ValueError("invalid calculation status")
    if status == "computed":
        if decimal_text(response["value"]) != response["value"]:
            raise ValueError("noncanonical response value")
    elif response["value"] is not None or not response["limitations"]:
        raise ValueError("noncomputed response requires null and reason")
    if not isinstance(response["limitations"], list):
        raise ValueError("limitations must be a list")
    if (not isinstance(response["evidence_ids"], list)
            or not response["evidence_ids"]
            or not set(response["evidence_ids"]).issubset(evidence_ids)):
        raise ValueError("missing response evidence reference")


def validate_case(case: dict) -> None:
    required = {"case_id", "component_id", "model_revision", "parameters", "author",
        "source_kind", "execution_status", "retention_rights", "use_rights",
        "generated_at", "input_descriptor", "evidence", "responses", "superseded_by", "capture_role", "author_status"}
    require_fields(case, required, "case")
    string_list(case["superseded_by"], "supersession")
    nonempty(case["capture_role"], "capture role")
    if case["author_status"] not in {"recorded", "unknown", "unverified"}:
        raise ValueError("invalid author status")
    for key in ("case_id", "component_id", "model_revision", "author", "generated_at"):
        nonempty(case[key], key)
    if case["source_kind"] not in {"native", "synthetic", "unverified"}:
        raise ValueError("invalid source kind")
    if case["execution_status"] not in {"completed", "failed", "dry_run", "unknown"}:
        raise ValueError("invalid execution status")
    if case["retention_rights"] != "approved":
        raise ValueError("retention rights unresolved: use metadata-only intake ledger")
    if case["use_rights"] not in {"approved", "unresolved", "withdrawn"}:
        raise ValueError("invalid use rights")
    if not isinstance(case["parameters"], dict) or not case["parameters"]:
        raise ValueError("missing parameter identity")
    for name, value in case["parameters"].items():
        nonempty(name, "parameter")
        if decimal_text(value) != value:
            raise ValueError("noncanonical parameter key")
    require_fields(case["input_descriptor"], {"load_basis", "source_revision",
                   "solver", "frame", "dependencies"}, "input descriptor")
    validate_case_evidence(case)
    validate_attempt_state(case)


def validate_attempt_state(case: dict) -> None:
    """Absent attempt metadata stays unknown; pending metadata cannot imply a run."""
    if case['capture_role'] == 'diagnostic_capture':
        from digitalmodel.ansys.analysis_pressure_observed import validate_pressure_observed_case
        validate_pressure_observed_case(case)
        return
    if case['capture_role'] == 'diagnostic_replay':
        from digitalmodel.ansys.analysis_replay import validate_replay_record
        validate_replay_record(case)
        return
    if case['capture_role'] == 'diagnostic_observation':
        validate_observed_attempt(case)
        return
    present = {key for key in ('attempt_consumed', 'native_attempt_count') if key in case}
    if present:
        if len(present) != 2:
            raise ValueError('attempt metadata requires both fields')
        consumed, count = case['attempt_consumed'], case['native_attempt_count']
        if type(consumed) is not bool or type(count) is not int or count < 0:
            raise ValueError('invalid typed attempt metadata')
        if consumed != (count > 0):
            raise ValueError('attempt flag and count disagree')
    if case['capture_role'] != 'pending_native':
        return
    if not present or case['attempt_consumed'] or case['native_attempt_count'] != 0:
        raise ValueError('pending case requires explicit unattempted state')
    if case['source_kind'] != 'unverified' or case['execution_status'] != 'unknown':
        raise ValueError('pending case cannot claim native execution')
    if case['author'] != 'unverified' or case['author_status'] != 'unverified':
        raise ValueError('pending native author must remain unverified')
    for row in case['responses']:
        if (row['value'] is not None or row['calculation_status'] != 'not_evaluated'
                or 'observed_value' in row
                or 'native-not-attempted' not in row['limitations']):
            raise ValueError('pending response requires null, unattempted evidence')


def validate_observed_attempt(case: dict) -> None:
    validate_observed_metadata(case)
    count = case.get('native_attempt_count')
    if (case.get('attempt_consumed') is not True or 'native_attempt_count' not in case
            or (count is not None and type(count) is not int) or count not in (0, 1, None)):
        raise ValueError('invalid typed diagnostic attempt metadata')
    if (case.get('assessment_status') not in ('incomplete', 'failed')
            or case['author'] != 'SOLVERS' or case['author_status'] != 'recorded'):
        raise ValueError('diagnostic assessment or author differs')
    reference = case.get('observation_reference')
    if not isinstance(reference, dict) or set(reference) != {'id', 'sha256'}:
        raise ValueError('diagnostic observation reference required')
    if not any(r['id'] == reference['id'] and r['sha256'] == reference['sha256']
               and r['role'] == 'diagnostic_intake' for r in case['evidence']):
        raise ValueError('diagnostic receipt absent from case evidence')
    if count == 1:
        if case['source_kind'] != 'native' or case['execution_status'] != 'completed':
            raise ValueError('diagnostic execution state differs')
    else:
        ref = case.get('attempt_boundary_reference')
        if (case['source_kind'] != 'unverified' or case['execution_status'] != 'unknown'
                or not isinstance(ref, dict) or set(ref) != {'id', 'sha256'}
                or not any(r['id'] == ref['id'] and r['sha256'] == ref['sha256']
                           for r in case['evidence'])):
            raise ValueError('claim or uncertainty evidence required')
    if any(row['value'] is not None or row['calculation_status'] != 'failed'
           or 'diagnostic-only' not in row['limitations'] for row in case['responses']):
        raise ValueError('diagnostic observation requires failed null responses')


def validate_observed_metadata(case: dict) -> None:
    execution = case.get('observed_execution')
    if (case.get('input_descriptor_scope') != 'historical-prelaunch-basis'
            or 'observed_solver_profile' not in case or case['observed_solver_profile'] is not None
            or case.get('observed_solver_profile_status') != 'not-established-from-native-header'
            or case.get('observed_retention') != dict(status='retained-local-source-evidence',
                                                     private_git_backup='not-established')
            or not isinstance(execution, dict)
            or set(execution) != {'return_code', 'duration_seconds'}):
        raise ValueError('observed metadata differs')
    if case.get('native_attempt_count') == 1:
        if (type(execution['return_code']) is not int or execution['return_code'] != 0
                or not isinstance(execution['duration_seconds'], str)
                or decimal_text(execution['duration_seconds']) != execution['duration_seconds']
                or Decimal(execution['duration_seconds']) <= 0):
            raise ValueError('observed metadata execution differs')
    elif execution != dict(return_code=None, duration_seconds=None):
        raise ValueError('observed metadata without execution differs')


def string_list(value: list, label: str) -> None:
    if not isinstance(value, list):
        raise ValueError(label + " must be a list")
    for item in value:
        nonempty(item, label)
    if len(set(value)) != len(value):
        raise ValueError("duplicate " + label)


def validate_case_evidence(case: dict) -> None:
    evidence = case["evidence"]
    if not isinstance(evidence, list) or not evidence:
        raise ValueError("empty evidence")
    for item in evidence:
        require_fields(item, {"id", "sha256", "role", "required"}, "evidence")
        if type(item["required"]) is not bool:
            raise ValueError("required must be boolean")
        nonempty(item["role"], "evidence role")
    ids = [nonempty(item["id"], "evidence id") for item in evidence]
    if len(set(ids)) != len(ids):
        raise ValueError("duplicate evidence ids")
    dependencies = case["input_descriptor"]["dependencies"]
    if not isinstance(dependencies, list) or not set(dependencies).issubset(ids):
        raise ValueError("missing input dependency")
    if not isinstance(case["responses"], list) or not case["responses"]:
        raise ValueError("missing required responses")
    names = [item["name"] for item in case["responses"]]
    if len(set(names)) != len(names):
        raise ValueError("duplicate response")
    for response in case["responses"]:
        validate_response(response, set(ids))
