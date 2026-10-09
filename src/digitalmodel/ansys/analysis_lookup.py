"""Exact lookup with live evidence checks and separately trusted withdrawal state.

Authority and resolver inputs are supplied by the engineering owner outside the
published dataset. They are audit claims, not authenticated identities. An actor
controlling both those inputs and the dataset is outside this local trust model.
"""
from __future__ import annotations

import copy
from decimal import Decimal, localcontext
from pathlib import Path

from digitalmodel.ansys.analysis_evidence import validate_package
from digitalmodel.ansys.analysis_records import (
    canonical_bytes, decimal_text, digest_bytes, nonempty, require_fields,
    verify_reference, parse_json,
)


def create_ledger(dataset_id: str) -> tuple[dict, dict]:
    nonempty(dataset_id, "dataset id")
    ledger = {"dataset_id": dataset_id, "entries": []}
    return ledger, {"count": 0, "sha256": digest_bytes(canonical_bytes(ledger))}


def validate_ledger(ledger: dict, head: dict) -> None:
    require_fields(ledger, {"dataset_id", "entries"}, "ledger")
    require_fields(head, {"count", "sha256"}, "authority head")
    _, previous = create_ledger(ledger["dataset_id"])
    for index, entry in enumerate(ledger["entries"], 1):
        data = copy.deepcopy(entry)
        digest = data.pop("sha256", None)
        if data.get("sequence") != index or data.get("previous") != previous["sha256"]:
            raise ValueError("ledger sequence/chain mismatch")
        if digest_bytes(canonical_bytes(data)) != digest:
            raise ValueError("ledger digest mismatch")
        if data.get("action") != "withdraw":
            raise ValueError("unsupported ledger action")
        for field in ("revision", "case_id", "response_name", "reason", "authority",
                      "evidence_id", "timestamp"):
            nonempty(data.get(field), field)
        previous = {"count": index, "sha256": digest}
    if head != previous:
        raise ValueError("ledger head unavailable, inconsistent or rolled back")


def append_withdrawal(ledger: dict, head: dict, **event: str) -> tuple[dict, dict]:
    validate_ledger(ledger, head)
    fields = {"revision", "case_id", "response_name", "reason", "authority",
              "evidence_id", "timestamp"}
    if set(event) != fields:
        raise ValueError("withdrawal event fields do not match schema")
    for name, value in event.items():
        nonempty(value, name)
    entry = {**event, "action": "withdraw", "sequence": head["count"] + 1,
             "previous": head["sha256"]}
    entry["sha256"] = digest_bytes(canonical_bytes(entry))
    result = copy.deepcopy(ledger)
    result["entries"].append(entry)
    updated = {"count": entry["sequence"], "sha256": entry["sha256"]}
    validate_ledger(result, updated)
    return result, updated


def _find_response(package: dict, query: dict) -> tuple[dict, dict]:
    require_fields(query, {"dataset_id", "revision", "analysis_id", "component_id",
        "model_revision", "parameters", "response_name", "definition", "location",
        "unit", "intended_use"}, "query")
    for key in ("dataset_id", "revision", "analysis_id"):
        if query[key] != package[key]:
            raise ValueError(f"incompatible {key}")
    if query["intended_use"] not in package["intended_uses"]:
        raise ValueError("unsupported intended use")
    matches = []
    for case in package["cases"]:
        if any(case[key] != query[key] for key in ("component_id", "model_revision", "parameters")):
            continue
        for response in case["responses"]:
            if response["name"] != query["response_name"]:
                continue
            if any(response[key] != query[key] for key in ("definition", "location", "unit")):
                raise ValueError("incompatible response definition/location/unit")
            matches.append((case, response))
    if len(matches) != 1:
        raise ValueError("exact query must match exactly one response")
    return matches[0]


def _check_authority(package: dict, authority: dict | None, ledger: dict | None,
                     resolver: dict[str, Path]) -> None:
    if authority is None or ledger is None:
        raise ValueError("current publication authority and ledger required")
    if authority.get("dataset_id") != package["dataset_id"]:
        raise ValueError("incompatible authority dataset")
    if ledger.get("dataset_id") != package["dataset_id"]:
        raise ValueError("incompatible ledger dataset")
    validate_ledger(ledger, authority.get("head", {}))
    if authority.get("criteria_revision") != package["criteria_revision"]:
        raise ValueError("criteria transition requires recorded requalification")
    criteria = package.get("criteria_reference")
    if not criteria or authority.get("criteria_reference") != criteria:
        raise ValueError("current criteria evidence required")
    verify_reference(criteria, resolver)
    verify_reference(package["intake_reference"], resolver)
    refs = authority.get("review_sources")
    if not isinstance(refs, list) or not refs:
        raise ValueError("current review sources required")
    for reference in refs:
        verify_reference(reference, resolver)
    pinned = package.get("review_sources")
    if pinned != refs:
        raise ValueError("review sources changed; requalification required")
    if authority.get("revisions", {}).get(package["revision"]) != package["package_hash"]:
        raise ValueError("package revision is not bound to current authority")
    if authority.get("blocking_findings") is not False:
        raise ValueError("unresolved or unmapped review findings")


def _verification(package: dict, case: dict, response: dict, authority: dict,
                  resolver: dict[str, Path]) -> dict:
    reference = package.get("verification_reference")
    if not reference or authority.get("verification_reference") != reference:
        raise ValueError("frozen verification reference required")
    raw = verify_reference(reference, resolver)
    register = parse_json(raw)
    key = case["case_id"] + ":" + response["name"]
    entry = register.get("entries", {}).get(key, {})
    fields = {"checker", "competence", "authority", "date", "row_hash", "intended_uses",
        "expected", "absolute_tolerance", "criterion_id", "reference_evidence",
        "required_roles", "checks", "allowed_limitations"}
    require_fields(entry, fields, "verification entry")
    for field in ("intended_uses", "required_roles", "allowed_limitations"):
        if not isinstance(entry[field], list):
            raise ValueError(f"{field} must be a list")
        for value in entry[field]:
            nonempty(value, field)
    for field in ("checker", "competence", "authority", "date", "criterion_id"):
        nonempty(entry[field], field)
    if entry["checker"].strip().casefold() == case["author"].strip().casefold() or entry["row_hash"] != case["row_hash"]:
        raise ValueError("verification is not independent or bound to this row")
    for ref in entry["reference_evidence"]:
        verify_reference(ref, resolver)
    if not entry["reference_evidence"]:
        raise ValueError("independent reference evidence missing")
    if any(entry["checks"].get(check) != "passed" for check in
           ("source_rights", "equilibrium", "numerical_quality", "independent_method")):
        raise ValueError("required verification incomplete")
    return entry


def _qualify(package: dict, case: dict, response: dict, intended_use: str,
             authority: dict, resolver: dict[str, Path]) -> dict:
    # Preserve historical roles as evidence; only explicit qualification claims
    # may enter the independent rights, provenance and verification checks below.
    if (case.get("engineering_qualified") is not True
            or case.get("capture_role") != "qualified_native"
            or case.get("campaign_assessment_status") != "PASS"):
        raise ValueError("diagnostic or incomplete campaign evidence cannot qualify")
    _check_provenance(package, case, response, authority, resolver)
    if case["source_kind"] != "native" or case["execution_status"] != "completed":
        raise ValueError("synthetic, failed or dry-run evidence cannot qualify")
    if case["use_rights"] != "approved" or response["calculation_status"] != "computed":
        raise ValueError("response or intended-use rights unqualified")
    for reference in case["evidence"]:
        verify_reference(reference, resolver)
    entry = _verification(package, case, response, authority, resolver)
    roles = {ref["role"] for ref in case["evidence"] if ref["required"]}
    required = {"input", "output", "log", "native", "source"} | set(entry["required_roles"])
    if not required.issubset(roles):
        raise ValueError("required native evidence incomplete")
    if intended_use not in entry["intended_uses"]:
        raise ValueError("verification not qualified for intended use")
    if not set(response["limitations"]).issubset(entry["allowed_limitations"]):
        raise ValueError("disqualifying readiness limitation")
    expected = Decimal(decimal_text(entry["expected"]))
    tolerance = Decimal(decimal_text(entry["absolute_tolerance"]))
    # Decimal input bounds are 128 characters and adjusted exponents +/-100.
    # 512 digits cover their exact subtraction without ambient-context rounding.
    with localcontext() as context:
        context.prec = 512
        if tolerance < 0 or abs(Decimal(response["value"]) - expected) > tolerance:
            raise ValueError("independent comparison criterion failed")
    return entry


def _check_provenance(package: dict, case: dict, response: dict, authority: dict, resolver: dict) -> None:
    if case["author_status"] != "recorded" or case["author"].strip().casefold() in {"unknown", "unverified", "historical-neutral-capture"}:
        raise ValueError("capture author is unknown; independent verification cannot be established")
    if case["superseded_by"]:
        raise ValueError("superseded capture cannot qualify")
    findings = response["inherited_findings"]
    if not isinstance(findings, list):
        raise ValueError("inherited findings must be a list")
    # An unmapped inherited finding conservatively affects every response.
    findings = set(findings) | {item["finding"] for item in package["finding_ledger"]}
    key = case["case_id"] + ":" + response["name"]
    dispositions = authority.get("finding_dispositions", {}).get(key, {})
    for identity in findings:
        nonempty(identity, "finding id")
        item = dispositions.get(identity, {})
        if item.get("disposition") != "resolved":
            raise ValueError("open or unmapped finding: " + identity)
        nonempty(item.get("justification"), "finding justification")
        refs = item.get("evidence")
        if not isinstance(refs, list) or not refs:
            raise ValueError("finding disposition evidence required")
        for reference in refs:
            verify_reference(reference, resolver)


def lookup(package: dict, query: dict, resolver: dict[str, Path], *,
           diagnostic: bool = False, ledger: dict | None = None,
           authority: dict | None = None) -> dict:
    """Diagnostic access never claims qualification; engineering access rechecks live state."""
    validate_package(package)
    case, response = _find_response(package, query)
    answer = {**copy.deepcopy(response), "source_kind": case["source_kind"],
              "qualified": False, "dataset_id": package["dataset_id"],
              "revision": package["revision"], "case_id": case["case_id"],
              "row_hash": case["row_hash"], "mode": "diagnostic"}
    if diagnostic:
        return answer
    _check_authority(package, authority, ledger, resolver)
    for event in ledger["entries"]:
        if (event["revision"] in {"*", package["revision"]}
                and event["case_id"] in {"*", case["case_id"]}
                and event["response_name"] in {"*", response["name"]}):
            raise ValueError("withdrawn: " + event["reason"])
    entry = _qualify(package, case, response, query["intended_use"], authority, resolver)
    answer.update(qualified=True, mode="exact", criterion_id=entry["criterion_id"],
                  verification_reference=package["verification_reference"])
    return answer
