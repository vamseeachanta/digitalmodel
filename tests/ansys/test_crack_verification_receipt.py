"""P0a verification receipt, validated in CI without a licence (#2157).

None of these tests skip. A missing receipt, a stale deck hash, a guard that is
not ``pass`` or a K outside the Newman-Raju band fails the suite. The licensed
solve that produces the receipt is ``test_solve_verification_deck`` in
``test_crack_verification.py``.
"""

from __future__ import annotations

import json
from pathlib import Path

from digitalmodel.ansys import cint_parser
from digitalmodel.ansys.crack_verification import (
    CrackPlateSpec,
    deck_sha256,
    generate_crack_verification_apdl,
)

RECEIPT = (
    Path(__file__).resolve().parents[2]
    / "examples"
    / "workflows"
    / "crack-fe-weldolet"
    / "fe_states"
    / "p0a_verification.receipt.json"
)

# Frozen comparator values (plan #2157, TDD list P0), fixed before the run.
NR_DEEPEST = 7.2896  # MPa*sqrt(m), phi = 90 deg
NR_SURFACE = 5.7422  # MPa*sqrt(m), phi = 0 deg (and 180 deg by symmetry)
NR_BAND = 0.05  # +/- 5 %, the stated accuracy of the Newman-Raju fit


def _receipt() -> dict:
    assert RECEIPT.is_file(), f"P0a verification receipt missing: {RECEIPT.name}"
    return json.loads(RECEIPT.read_text(encoding="utf-8"))


def test_receipt_present_and_schema_valid():
    assert cint_parser.validate_receipt_schema(_receipt()) == []


def test_receipt_deck_hash_matches_generator():
    receipt = _receipt()
    for mesh in receipt["meshes"]:
        spec = CrackPlateSpec(**receipt["spec"], mesh_level=mesh["level"])
        current = deck_sha256(generate_crack_verification_apdl(spec))
        assert current == mesh["deck_sha256"], (
            f"stale receipt: level {mesh['level']} deck hash differs from the generator"
        )


def test_receipt_guards_all_pass():
    receipt = _receipt()
    for name in cint_parser.GUARD_NAMES:
        assert receipt["guards"][name]["status"] == "pass", receipt["guards"][name]
    # re-evaluate from the recorded solved data, not only the stored verdicts
    for name, result in cint_parser.evaluate_receipt_guards(receipt).items():
        assert result.status == "pass", f"recomputed guard {name}: {result}"


def test_receipt_is_host_free():
    text = RECEIPT.read_text(encoding="utf-8")
    assert cint_parser.find_host_tokens(text) == []
    assert _receipt()["run"]["platform"] in {"windows", "linux", "darwin"}


def _front_k(receipt: dict, phi: float) -> list[float]:
    primary = next(
        m for m in receipt["meshes"] if m["level"] == receipt["primary_level"]
    )
    vals = [
        n["K1_reported"] for n in primary["front"] if abs(n["phi_deg"] - phi) < 1e-6
    ]
    assert vals, f"no front node at phi = {phi}"
    return vals


def test_verification_receipt_newman_raju_deepest():
    (k_deep,) = _front_k(_receipt(), 90.0)
    assert abs(k_deep / NR_DEEPEST - 1.0) <= NR_BAND, k_deep


def test_verification_receipt_newman_raju_surface():
    receipt = _receipt()
    for k_surf in _front_k(receipt, 0.0) + _front_k(receipt, 180.0):
        assert abs(k_surf / NR_SURFACE - 1.0) <= NR_BAND, k_surf
