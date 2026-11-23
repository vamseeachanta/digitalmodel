"""Frozen four-case cylinder preparation; no launch or qualification capability.

Basis: docs/plans/evidence/2026-09-13-issue-2121-canary-proposal.json,
approved B1 baseline ba2ac7f4. Numeric inputs are demonstration case inputs,
not standards-derived material limits. Native command sources are recorded in
the companion deck modules and the proposal's vendor-source metadata register.
"""
from copy import deepcopy

from digitalmodel.ansys.analysis_records import canonical_bytes, decimal_text, digest_bytes
from digitalmodel.ansys.cylinder_deck import render_deck
from digitalmodel.ansys.cylinder_mesh import mapped_mesh


_BASIS = {"inner_radius_mm": "750", "wall_thickness_mm": "60", "axial_length_mm": "240",
          "youngs_modulus_mpa": "200000", "poisson_ratio": "0.3",
          "delta_temperature_C": "0", "external_pressure_mpa": "0",
          "length_unit": "mm", "pressure_unit": "MPa", "element": "PLANE183",
          "keyopts": {"1": 0, "3": 1, "6": 0}, "nlgeom": "OFF"}
_CASES = {"ocv-zero-t60-n16": ("CTRL16", "0", 16, 4833, 1536),
          "ocv-t60-p10-n4": ("P10N4", "10", 4, 345, 96),
          "ocv-t60-p10-n8": ("P10N8", "10", 8, 1265, 384),
          "ocv-t60-p10-n16": ("P10N16", "10", 16, 4833, 1536)}
_DECIMALS = set(list(_BASIS)[:7])


def frozen_basis() -> dict:
    return deepcopy(_BASIS)


def validate_basis(basis: dict) -> dict:
    """Refuse changed fields/units; normalize exact decimal strings only."""
    if not isinstance(basis, dict) or set(basis) != set(_BASIS):
        raise ValueError("basis fields differ from frozen canary")
    normalized = deepcopy(basis)
    for field in _DECIMALS:
        normalized[field] = decimal_text(normalized[field])
    if canonical_bytes(normalized) != canonical_bytes(_BASIS):
        raise ValueError("basis differs from frozen canary")
    return normalized


def case_definition(case_id: str) -> dict:
    if not isinstance(case_id, str) or case_id not in _CASES:
        raise ValueError("case is outside four-case B1 scope")
    token, pressure, radial, nodes, elements = _CASES[case_id]
    return {"case_id": case_id, "case_token": token, "pressure_mpa": pressure,
            "radial_divisions": radial, "axial_divisions": 6 * radial,
            "expected_nodes": nodes, "expected_elements": elements}


def validate_case(case: dict) -> dict:
    if not isinstance(case, dict):
        raise ValueError("case must be an object")
    expected = case_definition(case.get("case_id"))
    normalized = deepcopy(case)
    normalized["pressure_mpa"] = decimal_text(case.get("pressure_mpa"))
    if canonical_bytes(normalized) != canonical_bytes(expected):
        raise ValueError("case differs from frozen divisions, pressure or identity")
    return normalized


def build_case(case_id: str) -> dict:
    """Return deterministic ASCII/LF deck bytes and exact-decimal mesh metadata."""
    case = validate_case(case_definition(case_id))
    mesh = mapped_mesh(case["radial_divisions"], case["axial_divisions"])
    result = {**case, "basis": validate_basis(frozen_basis()), **mesh}
    result["pressure_faces"] = [] if case["pressure_mpa"] == "0" else [
        {"element_id": 1 + j * case["radial_divisions"], "face": 4, "pressure_mpa": "10"}
        for j in range(case["axial_divisions"])]
    result["artifact_filenames"] = ["station_values.txt", "state_values.txt",
                                    "precision_witness.txt", "support_reactions.txt", "model.cdb"]
    result["deck_bytes"] = render_deck(result)
    result["deck_sha256"] = digest_bytes(result["deck_bytes"])
    return result


def validate_deck(case_id: str, deck_bytes: bytes) -> None:
    """Require the exact deterministic approved command stream, including audit windows.

    This input identity guard is not a parser for native execution evidence.
    Geometry tests independently inspect the resulting connectivity and loads.
    """
    if not isinstance(deck_bytes, bytes) or deck_bytes != build_case(case_id)["deck_bytes"]:
        raise ValueError("deck differs from frozen generated command stream")
