"""Evidence checks for AQG r0 sections 5, 8 and 9; never launch a solver.

External checks attest manual work, not computed hydrostatic qualification.
The named tool's retained artifact and a maintainer decision are required.
"""
import hashlib
import json
import re
from pathlib import Path
from datetime import datetime, timezone
from typing import Any


EXTERNAL_TOOLS = {f"AQ-{n:02}": "AQWA Workbench hydrostatics and mesh inspection" for n in range(5, 11)}
EXTERNAL_CHECKS = {
    "AQ-05": ("weight_residual_le_1_percent", "volume_residual_le_0_5_percent"),
    "AQ-06": ("cob_horizontal_le_0_5_percent_length", "cob_vertical_le_1_percent_draft", "gm_positive", "gm_difference_le_2_percent"),
    "AQ-07": ("panel_count_within_cited_version_limit", "aspect_ratio_le_3", "panel_dimension_le_wavelength_over_6"),
    "AQ-08": ("waterline_residual_le_1_mm", "draft_residual_le_0_01_m", "no_reversed_normals", "no_zero_area_panels", "no_duplicate_symmetry", "positive_signed_volume"),
    "AQ-09": ("irregular_frequency_estimate_retained", "lid_present_if_max_frequency_ge_0_9_irregular_frequency"),
    "AQ-10": ("long_period_heave_within_2_percent", "nonnegative_diagonal_damping", "added_mass_asymmetry_le_2_percent", "no_unexplained_irregular_frequency_spikes"),
}


def validate_external_gate(gate, evidence, *, expected_inputs=None):
    """Reject absent, unsigned, changed or failed manual gate evidence."""
    if gate not in EXTERNAL_CHECKS or not isinstance(evidence, dict):
        raise ValueError(f"{gate}: external manual evidence required; solver validation NOT_RUN")
    if (evidence.get("tool") != EXTERNAL_TOOLS[gate]
            or not evidence.get("tool_version")
            or evidence.get("reviewer_role") != "maintainer"
            or evidence.get("criteria_revision") != "AQG-r0"
            or evidence.get("decision") != "PASS"):
        raise ValueError(f"{gate}: qualified manual tool/version/maintainer decision required")
    checks = evidence.get("checks", {})
    if any(checks.get(key) is not True for key in EXTERNAL_CHECKS[gate]):
        raise ValueError(f"{gate}: missing or failed manual criterion")
    validate_input_binding(gate, evidence, expected_inputs)
    path = Path(evidence.get("artifact", ""))
    if not path.is_file() or hashlib.sha256(path.read_bytes()).hexdigest() != evidence.get("sha256"):
        raise ValueError(f"{gate}: external evidence digest mismatch")
    signed_evidence = {key: value for key, value in evidence.items() if key != "signature"}
    signed_evidence["artifact"] = path.name
    validate_signature(gate, evidence.get("signature"), evidence["input_sha256"], signed_evidence)
    return dict(evidence, artifact=path.name, verification="manual attestation; no package solver execution")


def validate_input_binding(gate, evidence, expected_inputs):
    digests = evidence.get("input_sha256")
    if not isinstance(digests, dict) or not digests or any(
        not isinstance(value, str) or not re.fullmatch(r"[0-9a-f]{64}", value)
        for value in digests.values()
    ):
        raise ValueError(f"{gate}: input SHA-256 binding required")
    if expected_inputs is not None and digests != expected_inputs:
        raise ValueError(f"{gate}: input digest binding does not match this calculation")


def validate_native_result(result, *, expected_inputs=None):
    """AQ-04: native completion, MES status and declared body counts all agree."""
    if (not isinstance(result, dict) or result.get("returncode") != 0
            or result.get("mes_status") != "PASS" or result.get("errors") != []
            or result.get("warnings") != [] or not isinstance(result.get("body_count"), int)
            or result["body_count"] <= 0 or result["body_count"] != result.get("expected_body_count")):
        raise ValueError("AQ-04: successful native exit, clean MES and matching positive body count required")
    validate_input_binding("AQ-04", result, expected_inputs)
    return {"decision": "PASS", "evidence": result}


def input_manifest(paths):
    return {key: {"file": Path(path).name,
                  "sha256": hashlib.sha256(Path(path).read_bytes()).hexdigest()}
            for key, path in paths.items()}


def verify_manifest(manifest, paths):
    if manifest != input_manifest(paths):
        raise ValueError("AQ-11: input digest mismatch")


def evidence_digest(value: Any) -> str:
    """Canonical JSON digest used by the retained attestation record."""
    return hashlib.sha256(json.dumps(value, sort_keys=True, separators=(",", ":"),
                                     allow_nan=False).encode("utf-8")).hexdigest()


def validate_signature(gate: str, signature: Any, manifest: Any, gates: Any) -> None:
    """Validate a dated identity attestation and its content bindings.

    This record is a manual attestation, not cryptographic identity verification.
    The maintainers must establish the reviewer's authority separately.
    """
    if not isinstance(signature, dict):
        raise ValueError(f"{gate}: identity/date/digest signature record required")
    identity = signature.get("identity")
    try:
        signed_at = datetime.fromisoformat(signature.get("signed_at", "").replace("Z", "+00:00"))
    except (TypeError, ValueError, AttributeError) as exc:
        raise ValueError(f"{gate}: signature date must be an ISO timestamp") from exc
    if (not isinstance(identity, str) or not identity.strip() or identity.strip() == "maintainer"
            or signature.get("role") != "maintainer" or signed_at.tzinfo is None
            or signed_at > datetime.now(timezone.utc)
            or not isinstance(manifest, dict) or not manifest
            or signature.get("manifest_sha256") != evidence_digest(manifest)
            or signature.get("gates_sha256") != evidence_digest(gates)):
        raise ValueError(f"{gate}: signature identity, date or digest binding invalid")


def validate_report_gates(
    gates: dict[str, Any], signature: Any = None, *, manifest: Any = None,
) -> dict[str, Any]:
    required = {f"AQ-{n:02}" for n in range(1, 11)}
    if any(gates.get(key, {}).get("decision") != "PASS" for key in required):
        raise ValueError("AQ-11: applicable gate evidence and maintainer quality signature required")
    validate_signature("AQ-11", signature, manifest, gates)
    return {"decision": "PASS", "quality_signature": signature}


LOCAL_CHECKS = {
    "AQ-01": ("ldrg_present", "configured_cog_matches"),
    "AQ-02": ("radiation_grid_matches", "added_mass_grid_matches"),
    "AQ-03": ("required_inertia_present", "required_restoring_present", "resonance_solved"),
}


def record_gates(
    external: Any = None, *, expected_inputs: Any = None, local: Any = None,
) -> dict[str, Any]:
    """Called only after DAT, grid and hydrostatic coefficient validation."""
    external = external or {}
    gates = {}
    for key, required in LOCAL_CHECKS.items():
        evidence = (local or {}).get(key)
        if evidence is None:
            gates[key] = {"decision": "NOT_RUN", "reason": "per-gate validation evidence required"}
            continue
        if (evidence.get("decision") != "PASS" or not evidence.get("evidence")
                or any(evidence.get("checks", {}).get(check) is not True for check in required)):
            raise ValueError(f"{key}: per-gate validation evidence invalid")
        validate_input_binding(key, evidence, expected_inputs)
        gates[key] = evidence
    for number in range(4, 11):
        key = f"AQ-{number:02}"
        if key not in external:
            gates[key] = {"decision": "NOT_RUN", "reason": "external manual/native evidence required"}
        elif key == "AQ-04":
            gates[key] = validate_native_result(external[key], expected_inputs=expected_inputs)
        else:
            gates[key] = validate_external_gate(key, external[key], expected_inputs=expected_inputs)
    return gates


def write_report_evidence(record, data_dir):
    verify_manifest(record["input_manifest"], record["input_paths"])
    gates = dict(record["qa_gates"])
    try:
        gates["AQ-11"] = validate_report_gates(
            gates, record.get("quality_signature"), manifest=record["input_manifest"])
    except ValueError as exc:
        gates["AQ-11"] = {"decision": "NOT_RUN", "reason": str(exc)}
    payload = {"criteria_revision": "AQG-r0", "inputs": record["input_manifest"],
               "gates": gates, "linearisation": record["viscous"]["linearisation"],
               "citations": record["viscous"]["citation_sidecar"]}
    path = data_dir / f"{record['record_id']}_manifest.json"
    path.write_text(json.dumps(payload, indent=2, allow_nan=False), encoding="utf-8")
    if json.loads(path.read_text(encoding="utf-8")) != payload:
        raise ValueError("AQ-11: evidence readback mismatch")
    return payload
