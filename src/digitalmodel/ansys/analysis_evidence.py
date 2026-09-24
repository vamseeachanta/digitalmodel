"""Offline ANSYS evidence packaging; never executes a solver or infers qualification."""
from __future__ import annotations

import copy
import csv
import io
import json
import os
from pathlib import Path
import tempfile

from digitalmodel.ansys.analysis_records import (
    canonical_bytes, decimal_text, digest_bytes, nonempty, read_json,
    require_fields, safe_id, validate_case, verify_reference,
)

SCHEMA_VERSION = "ansys-evidence-1"


def parse_digest(text: str, required: set[str]) -> dict[str, str]:
    """Guard the existing digest reader before information can be discarded."""
    if not text.strip():
        raise ValueError("empty digest")
    tokens = [token.strip() for token in text.strip().replace("\n", ",").split(",")]
    if len(tokens) % 2 or any(not token for token in tokens):
        raise ValueError("incomplete digest field")
    labels = tokens[::2]
    if len({name.casefold() for name in labels}) != len(labels):
        raise ValueError("duplicate digest label")
    if not required.issubset(labels):
        raise ValueError("missing required response")
    values = {name: decimal_text(value) for name, value in zip(labels, tokens[1::2])}
    # Preserve decimal strings; the existing float-based extractor is unsuitable here.
    return values


def build_package(study: dict, resolver: dict[str, Path]) -> dict:
    """Bind actual retained evidence bytes; all records start diagnostic-only."""
    package = copy.deepcopy(study)
    _validate_study(package)
    for key in ("criteria_reference", "intake_reference", "verification_reference"):
        if key in package:
            verify_reference(package[key], resolver)
    for reference in package.get("review_sources", []):
        verify_reference(reference, resolver)
    for case in package["cases"]:
        for reference in case["evidence"]:
            verify_reference(reference, resolver)
        case["row_hash"] = digest_bytes(canonical_bytes(case))
    package["schema_version"] = SCHEMA_VERSION
    package["canonicalization"] = "typed-json-utf8-v1-decimal-strings"
    package["package_hash"] = digest_bytes(canonical_bytes(package))
    return package


def _validate_study(package: dict) -> None:
    require_fields(package, {"analysis_id", "dataset_id", "revision", "criteria_revision",
        "code_revision", "method_revision", "expected_cases", "intended_uses", "cases",
        "finding_ledger", "criteria_reference", "intake_reference", "review_sources"},
        "study")
    if not isinstance(package["finding_ledger"], list) or not isinstance(package["review_sources"], list):
        raise ValueError("finding ledger and review sources require lists")
    for finding in package["finding_ledger"]:
        require_fields(finding, {"finding", "disposition", "affected_responses"}, "finding")
        nonempty(finding["finding"], "finding id")
    for key in ("analysis_id", "dataset_id", "revision"):
        safe_id(package[key])
    for key in ("criteria_revision", "code_revision", "method_revision"):
        nonempty(package[key], key)
    if not package["intended_uses"] or not isinstance(package["intended_uses"], list):
        raise ValueError("missing intended uses")
    cases = package["cases"]
    ids = [case["case_id"] for case in cases]
    expected = package["expected_cases"]
    if not ids or len(set(ids)) != len(ids) or len(set(expected)) != len(expected):
        raise ValueError("empty or duplicate case membership")
    if set(ids) != set(expected):
        raise ValueError("missing or unexpected cases")
    identities = [canonical_bytes({key: case[key] for key in
                  ("component_id", "model_revision", "parameters")}) for case in cases]
    if len(set(identities)) != len(identities):
        raise ValueError("duplicate query identity")
    for case in cases:
        validate_case(case)


def validate_package(package: dict) -> None:
    if package.get("schema_version") != SCHEMA_VERSION:
        raise ValueError("unsupported package schema")
    _validate_study(package)
    data = copy.deepcopy(package)
    expected = data.pop("package_hash", None)
    if digest_bytes(canonical_bytes(data)) != expected:
        raise ValueError("package digest mismatch")
    for case in data["cases"]:
        row = copy.deepcopy(case)
        digest = row.pop("row_hash", None)
        if digest_bytes(canonical_bytes(row)) != digest:
            raise ValueError("row digest mismatch")
        validate_case(row)


def publish_package(package: dict, root: Path, *, revision_limit: int = 5_000_000,
                    cumulative_limit: int = 25_000_000) -> Path:
    """Atomic no-overwrite file publication; limits apply per dataset owner root."""
    validate_package(package)
    data = canonical_bytes(package)
    if len(data) > revision_limit:
        raise ValueError("revision size requires owner placement review")
    directory = Path(root).resolve() / safe_id(package["dataset_id"])
    directory.mkdir(parents=True, exist_ok=True)
    if not directory.resolve().is_relative_to(Path(root).resolve()):
        raise ValueError("dataset residence escapes configured root")
    target = directory / (safe_id(package["revision"]) + ".json")
    lock = directory / ".publish.lock"
    descriptor = os.open(lock, os.O_CREAT | os.O_EXCL | os.O_WRONLY)
    try:
        os.close(descriptor)
        if target.exists():
            raise FileExistsError(target)
        total = sum(path.stat().st_size for path in directory.glob("*.json"))
        if total + len(data) > cumulative_limit:
            raise ValueError("cumulative dataset size requires owner placement review")
        _write_atomic(target, data)
        return target
    finally:
        lock.unlink(missing_ok=True)


def _write_atomic(target: Path, data: bytes) -> None:
    with tempfile.NamedTemporaryFile(dir=target.parent, delete=False, suffix=".pending") as stream:
        temporary = Path(stream.name)
        stream.write(data)
        stream.flush()
        os.fsync(stream.fileno())
    try:
        if temporary.read_bytes() != data:
            raise ValueError("readback failed before publication")
        # link is no-replace: even a noncooperating writer cannot be overwritten.
        os.link(temporary, target)
    finally:
        temporary.unlink(missing_ok=True)


def load_package(path: Path) -> dict:
    package = read_json(Path(path))
    validate_package(package)
    return package


def response_csv(package: dict) -> str:
    """Typed CSV with canonical response payload; no NULL text coercion."""
    validate_package(package)
    stream = io.StringIO(newline="")
    writer = csv.writer(stream, lineterminator="\n")
    writer.writerow(["case_id", "response_name", "value_type", "value", "record_json"])
    for case in package["cases"]:
        for response in case["responses"]:
            kind = "null" if response["value"] is None else "decimal"
            if kind == "decimal" and case["capture_role"].startswith("diagnostic_"):
                kind = "diagnostic_decimal"
            record = {"case_id": case["case_id"], "row_hash": case["row_hash"],
                      "dataset_id": package["dataset_id"], "revision": package["revision"],
                      "capture_role": case["capture_role"], "response": response}
            writer.writerow([case["case_id"], response["name"], kind,
                             "" if kind == "null" else response["value"],
                             canonical_bytes(record).decode("utf-8")])
    return stream.getvalue()


def read_response_csv(text: str) -> list[dict]:
    reader = csv.DictReader(io.StringIO(text))
    if reader.fieldnames != ["case_id", "response_name", "value_type", "value", "record_json"]:
        raise ValueError("unsupported response CSV schema")
    records, seen = [], set()
    for row in reader:
        if None in row or any(value is None for value in row.values()):
            raise ValueError("malformed CSV row")
        key = (row["case_id"], row["response_name"])
        if key in seen:
            raise ValueError("duplicate CSV response")
        seen.add(key)
        record = json.loads(row["record_json"])
        if canonical_bytes(record).decode("utf-8") != row["record_json"]:
            raise ValueError("noncanonical CSV payload")
        response = record["response"]
        kind = "null" if response["value"] is None else "decimal"
        if kind == "decimal" and record.get("capture_role", "").startswith("diagnostic_"):
            kind = "diagnostic_decimal"
        value = "" if kind == "null" else decimal_text(response["value"])
        if (record["case_id"] != key[0] or response["name"] != key[1]
                or kind != row["value_type"] or value != row["value"]):
            raise ValueError("CSV typed columns disagree with payload")
        records.append(record)
    if not records:
        raise ValueError("empty response CSV")
    return records
