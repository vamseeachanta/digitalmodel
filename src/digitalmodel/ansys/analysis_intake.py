"""Reconstruct the frozen issue-2119 diagnostic intake without invoking ANSYS."""
from __future__ import annotations

import argparse
import copy
import json
from pathlib import Path, PurePosixPath
import re
import subprocess

from digitalmodel.ansys.analysis_evidence import build_package, parse_digest, publish_package, load_package
from digitalmodel.ansys.analysis_records import (
    canonical_bytes, decimal_text, digest_bytes, verify_reference, read_json, require_fields, validate_case,
)


def _relative(value: str) -> PurePosixPath:
    path = PurePosixPath(value)
    if not value or path.is_absolute() or ".." in path.parts or "\\" in value or ":" in value:
        raise ValueError("evidence reference must be a bounded logical relative path")
    return path


def resolve_native(root: Path, identity: str) -> Path:
    if not identity.startswith("retained-native/"):
        raise ValueError("unexpected native reference namespace")
    relative = _relative(identity.removeprefix("retained-native/"))
    root = Path(root).resolve()
    path = (root / str(relative)).resolve()
    if not path.is_relative_to(root):
        raise ValueError("evidence residence escapes configured root")
    return path


def _git_evidence(repo: Path, cache: Path, commit: str, name: str) -> tuple[str, Path, str]:
    if not re.fullmatch(r"[0-9a-f]{40}", commit):
        raise ValueError("full source commit required")
    _relative(name)
    data = subprocess.check_output(["git", "-C", str(repo), "cat-file", "blob", commit + ":" + name])
    digest = digest_bytes(data)
    cache.mkdir(parents=True, exist_ok=True)
    path = cache / digest
    if path.exists() and path.read_bytes() != data:
        raise ValueError("source cache digest collision or corruption")
    path.write_bytes(data)
    if path.read_bytes() != data:
        raise ValueError("source cache readback failed")
    return "repository/" + name, path, digest


def _role(name: str) -> str:
    suffix = PurePosixPath(name).suffix.lower()
    return {".inp": "input", ".csv": "output", ".out": "log", ".rst": "native",
            ".db": "native", ".cdb": "model", ".mntr": "monitor"}.get(suffix, "capture")


def _collect(candidate: dict, baseline: str, repo: Path, native: Path,
             cache: Path, resolver: dict) -> tuple[list, dict, str]:
    refs, units, csv_id = [], {}, ""
    for item in candidate.get("artifacts", []):
        identity = item["logical_source"]
        resolver[identity] = resolve_native(native, identity)
        refs.append({"id": identity, "sha256": item["expected_sha256"],
                     "role": _role(identity), "required": True})
        if identity.endswith(".csv"):
            csv_id, units = identity, item["response_units"]
    if not refs:
        name = candidate["logical_source"]
        identity, path, digest = _git_evidence(repo, cache, baseline, name)
        if digest != candidate["sha256"]:
            raise ValueError("frozen repository source changed")
        resolver[identity] = path
        refs.append({"id": identity, "sha256": digest, "role": "output", "required": True})
        csv_id, units = identity, candidate["response_units"]
    basis = candidate.get("basis_document", candidate["model_basis"]["source"])
    identity, path, digest = _git_evidence(repo, cache, baseline, basis)
    resolver[identity] = path
    refs.append({"id": identity, "sha256": digest, "role": "source", "required": True})
    return refs, units, csv_id


def _make_case(candidate: dict, refs: list, units: dict, csv_id: str,
               resolver: dict, observed_at: str) -> dict:
    reference = next(ref for ref in refs if ref["id"] == csv_id)
    values = parse_digest(verify_reference(reference, resolver).decode("utf-8"), set(units))
    require_fields(candidate, {"source_kind", "execution_status", "author", "retention_rights",
        "use_rights", "role", "superseded_by", "inherited_findings", "author_status"}, "intake classification")
    if set(values) != set(units):
        raise ValueError("unexpected response field without declared units")
    invalid = candidate.get("invalid_response_fields", [])
    responses = [_response(name, value, units[name], invalid, csv_id) for name, value in values.items()]
    for response in responses:
        response["inherited_findings"] = copy.deepcopy(candidate["inherited_findings"])
    basis = candidate["model_basis"]
    dependencies = [ref["id"] for ref in refs if ref["role"] in {"input", "model"}]
    case = {
        "case_id": candidate["candidate_id"], "component_id": candidate["family"],
        "model_revision": candidate["candidate_id"],
        "parameters": {key: decimal_text(str(value)) for key, value in basis["parameters"].items()},
        **{key: candidate[key] for key in ("author", "source_kind", "execution_status",
                                            "retention_rights", "use_rights", "superseded_by", "author_status")},
        "capture_role": candidate["role"], "generated_at": observed_at,
        "input_descriptor": {"load_basis": basis["load"], "source_revision": candidate.get("source_head_claim", "unknown"),
            "solver": candidate.get("profile_claim", "historical provenance; unqualified"),
            "frame": basis["idealization"], "dependencies": dependencies,
            "physical_input_sha256": _physical_input(candidate, refs),
            "capture_identity_note": "model_revision includes capture/extraction identity; not an independent physical case",
            "retention_basis": candidate["disposition_reason"]},
        "evidence": refs, "responses": responses,
    }
    validate_case(case)
    return case


def _physical_input(candidate: dict, refs: list) -> str:
    basis = candidate["model_basis"]
    expected = candidate.get("input_sha256", basis.get("baseline_input_sha256", "unknown"))
    if expected != "unknown":
        matches = [ref for ref in refs if ref["role"] == "input" or
                   ref["id"] == "repository/" + basis.get("source", "")]
        if not matches or any(ref["sha256"] != expected for ref in matches):
            raise ValueError("declared physical input digest does not match evidence")
    return expected


def _response(name: str, value: str, unit: str, invalid: list, csv_id: str) -> dict:
    return {"name": name, "definition": "historical APDL digest field: " + name,
            "location": "source digest; spatial qualification unresolved", "unit": unit,
            "value": None if name in invalid else value, "observed_value": value,
            "calculation_status": "failed" if name in invalid else "computed",
            "limitations": ["diagnostic-only", "inherited-review-unresolved"] +
                (["invalid-original-force-channel"] if name in invalid else []),
            "evidence_ids": [csv_id]}


def _intake_references(intake: dict, repo: Path, cache: Path, resolver: dict) -> dict:
    sources = []
    for item in intake["source_records"]:
        identity, path, digest = _git_evidence(repo, cache, intake["baseline_commit"], item["path"])
        if digest != item["sha256"]:
            raise ValueError("frozen intake source-record digest mismatch")
        resolver[identity] = path
        sources.append({"id": identity, "sha256": digest})
    identity, path, digest = _git_evidence(repo, cache, intake["baseline_commit"],
                                         "docs/standards/analysis-parametric-lookup-criteria.html")
    if digest != intake["criteria_sha256"]:
        raise ValueError("frozen criteria digest mismatch")
    resolver[identity] = path
    criteria = {"id": identity, "sha256": digest}
    raw = canonical_bytes(intake)
    digest = digest_bytes(raw)
    path = cache / digest
    path.write_bytes(raw)
    identity = "intake/" + digest
    resolver[identity] = path
    return {"criteria_reference": criteria, "intake_reference": {"id": identity, "sha256": digest},
            "review_sources": sources}


def code_fingerprint(directory: Path) -> dict:
    """Version 1 hashes UTF-8 source with universal newlines normalized to LF."""
    names = ["analysis_records.py", "analysis_evidence.py", "analysis_lookup.py", "analysis_intake.py"]
    return {name: digest_bytes((directory / name).read_text(encoding="utf-8").encode("utf-8")) for name in names}


def build_intake_package(intake: dict, repo_root: Path, native_root: Path,
                         cache_root: Path) -> tuple[dict, dict]:
    eligible = [case for case in intake["candidates"] if case["disposition"] == "eligible"]
    if not eligible or not any(case.get("artifacts") for case in eligible):
        raise ValueError("no eligible retained real capture: stage blocked")
    cases, resolver = [], {}
    for candidate in eligible:
        refs, units, csv_id = _collect(candidate, intake["baseline_commit"], Path(repo_root),
                                      Path(native_root), Path(cache_root), resolver)
        if not csv_id:
            raise ValueError("eligible capture has no response CSV")
        cases.append(_make_case(candidate, refs, units, csv_id, resolver, intake["observed_utc"]))
    code = code_fingerprint(Path(__file__).parent)
    references = _intake_references(intake, Path(repo_root), Path(cache_root), resolver)
    study = {"analysis_id": "ansys-2094-diagnostic-intake", "dataset_id": "ansys-retained-evidence",
        **references,
        "revision": "r1", "criteria_revision": "ENG-ANALYSIS-CRITERIA:1",
        "criteria_sha256": intake["criteria_sha256"], "code_revision": digest_bytes(canonical_bytes(code)),
        "code_files": code, "code_canonicalization": "utf8-lf-v1", "method_revision": "offline-digest-import-1",
        "source_revision": intake["baseline_commit"], "intake_digest": digest_bytes(canonical_bytes(intake)),
        "finding_ledger": copy.deepcopy(intake["finding_ledger"]),
        "expected_cases": [case["case_id"] for case in cases], "intended_uses": ["screening"],
        "coverage": {"intake_candidates": len(intake["candidates"]),
                     "native_captures": sum(case["source_kind"] == "native" for case in cases),
                     "qualified_responses": 0, "physical_parametric_coverage": "not established"},
        "cases": cases}
    return build_package(study, resolver), resolver


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("intake", "repo-root", "native-root", "cache-root", "output-root"):
        parser.add_argument("--" + name, required=True, type=Path)
    args = parser.parse_args()
    intake = read_json(args.intake)
    package, _ = build_intake_package(intake, args.repo_root, args.native_root, args.cache_root)
    path = publish_package(package, args.output_root)
    if load_package(path) != package:
        raise ValueError("published package readback mismatch")
    print(json.dumps({"case_records": len(package["cases"]), "qualified_responses": 0,
                      "package_hash": package["package_hash"]}))


if __name__ == "__main__":
    main()
