"""Write frozen B1 inputs offline; no solver, checker or authority capability.

The complete proposal at ba2ac7f4 is pinned semantically, including every fixed
criterion, budget and recovery rule. Its historical pending-approval fields are
retained as source text; neither those fields nor this helper grant authority.
Only basis decimals and case pressures normalize under decimal_text. A different
proposal requires reviewed source revision, not a caller-supplied expected hash.
"""
from copy import deepcopy
from pathlib import Path

from digitalmodel.ansys.analysis_records import canonical_bytes, decimal_text, digest_bytes
from digitalmodel.ansys.cylinder_benchmark import build_case, frozen_basis


PROPOSAL_PATH = "docs/plans/evidence/2026-09-13-issue-2121-canary-proposal.json"
PROPOSAL_GIT_REVISION = "ba2ac7f4"
PROPOSAL_GIT_BYTES_SHA256 = "c765ad316307b36090595ec265e71b4b0459aaa8f82adf9ce8b94f27c9e69423"
PROPOSAL_CANONICAL_SHA256 = "117340d4e72b502dfa5e3a116a8019f572f08cb16e220ef5cb6a381e1287972d"
DECIMAL_BASIS = ("inner_radius_mm", "wall_thickness_mm", "axial_length_mm",
                 "youngs_modulus_mpa", "poisson_ratio", "delta_temperature_C", "external_pressure_mpa")


class PreparationError(OSError):
    """A partial output is preserved; failure-receipt persistence is best effort."""

    def __init__(self, output, failed_path, completed_files, cause):
        super().__init__(f"partial preparation at {output}; failed {failed_path}: {cause}")
        self.output = output
        self.failed_path = failed_path
        self.completed_files = deepcopy(completed_files)


def validate_proposal(proposal: dict) -> dict:
    """Validate the whole frozen proposal, never a loose caller-approved hash."""
    canonical_bytes(proposal)  # Reject binary floats and unsupported nested types first.
    normalized = deepcopy(proposal)
    try:
        for key in DECIMAL_BASIS:
            normalized["basis"][key] = decimal_text(normalized["basis"][key])
        for case in normalized["cases"]:
            case["pressure_mpa"] = decimal_text(case["pressure_mpa"])
    except (KeyError, TypeError) as error:
        raise ValueError("proposal lacks the frozen basis/case structure") from error
    if digest_bytes(canonical_bytes(normalized)) != PROPOSAL_CANONICAL_SHA256:
        raise ValueError("proposal differs from the complete frozen B1 contract")
    return normalized


def _cases(proposal):
    basis = frozen_basis()
    for key in (*DECIMAL_BASIS, "element", "keyopts", "nlgeom"):
        if canonical_bytes(basis[key]) != canonical_bytes(proposal["basis"][key]):
            raise ValueError("builtin basis differs from pinned proposal")
    cases = []
    for expected in proposal["cases"]:
        case = build_case(expected["case_id"])
        for key in ("pressure_mpa", "radial_divisions", "axial_divisions", "expected_nodes", "expected_elements"):
            if canonical_bytes(case[key]) != canonical_bytes(expected[key]):
                raise ValueError("builtin case differs from pinned proposal")
        if len(case["nodes"]) != expected["expected_nodes"] or len(case["elements"]) != expected["expected_elements"]:
            raise ValueError("generated mesh counts differ from pinned proposal")
        if proposal["recovery"]["record_format"]["case_tokens"].get(case["case_token"]) != case["case_id"]:
            raise ValueError("generated case token differs from pinned proposal")
        cases.append(case)
    return cases


def _write_verified(root, name, raw):
    path = root / name
    with path.open("xb") as stream:
        stream.write(raw)
        stream.flush()
    saved = path.read_bytes()
    if saved != raw:
        raise OSError("saved artifact readback differs")
    return {"path": name, "bytes": len(saved), "sha256": digest_bytes(saved)}


def _artifacts(proposal, cases, provenance):
    yield "proposal.json", canonical_bytes(proposal)
    yield "basis-criteria.json", canonical_bytes({"basis": proposal["basis"],
            "criteria": proposal["criteria"], "provenance": provenance})
    for case in cases:
        yield case["case_id"] + ".inp", case["deck_bytes"]
        metadata = {key: value for key, value in case.items() if key != "deck_bytes"}
        yield case["case_id"] + ".json", canonical_bytes(metadata)


def _preserve_failure(root, failed_path, files, error):
    failure = {"status": "partial_preparation", "native_execution": False,
               "failed_path": failed_path, "completed_files": files,
               "exception_type": type(error).__name__, "reason": str(error),
               "limitation": "Partial or unverified failed-path bytes may remain; no automatic cleanup or retry."}
    try:
        _write_verified(root, "preparation-failure.json", canonical_bytes(failure))
    except OSError:
        pass  # The raised exception still names the preserved output and failed path.


def prepare_inputs(output, proposal: dict) -> dict:
    """Create four input/metadata pairs and verified relative-file provenance.

    Existing output raises FileExistsError. Filesystem failure preserves partial
    output and raises PreparationError. This is not the final execution manifest;
    independent reference, source binding and B2 approval are separate work.
    """
    normalized = validate_proposal(proposal)
    cases = _cases(normalized)
    provenance = {"proposal_path": PROPOSAL_PATH, "proposal_git_revision": PROPOSAL_GIT_REVISION,
                  "proposal_git_bytes_sha256": PROPOSAL_GIT_BYTES_SHA256,
                  "proposal_canonical_sha256": PROPOSAL_CANONICAL_SHA256,
                  "numeric_representation": "exact decimal strings; no binary float",
                  "authority": "historical source identity only; no approval asserted"}
    receipt = {"schema": "cylinder-offline-preparation-1", "status": "prepared_not_executed",
               "case_order": [case["case_id"] for case in cases], "provenance": provenance,
               "native_execution": False, "independent_reference_established": False, "files": []}
    root = Path(output)
    root.mkdir(parents=True, exist_ok=False)
    name = "proposal.json"
    try:
        for name, raw in _artifacts(normalized, cases, provenance):
            receipt["files"].append(_write_verified(root, name, raw))
        name = "preparation.json"
        _write_verified(root, name, canonical_bytes(receipt))
    except (OSError, ValueError) as error:
        _preserve_failure(root, name, receipt["files"], error)
        raise PreparationError(root, name, receipt["files"], error) from error
    return receipt
