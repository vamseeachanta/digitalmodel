# ABOUTME: Keeps docs/domains/codes-register.md in sync with codes.REGISTER —
# ABOUTME: every registered CodeReference standard must be documented.
from pathlib import Path

from digitalmodel import codes

_REGISTER_DOC = (
    Path(__file__).resolve().parents[2] / "docs" / "domains" / "codes-register.md"
)


def test_register_doc_exists():
    assert _REGISTER_DOC.is_file(), f"missing {_REGISTER_DOC}"


def test_every_registered_code_is_documented():
    text = _REGISTER_DOC.read_text(encoding="utf-8")
    missing = [
        ref.standard
        for ref in codes.REGISTER.values()
        if ref.standard not in text
    ]
    assert not missing, f"codes missing from codes-register.md: {missing}"


def test_register_is_nonempty_and_well_formed():
    assert codes.REGISTER
    assert all(isinstance(r, codes.CodeReference) for r in codes.REGISTER.values())
    assert all(r.standard for r in codes.REGISTER.values())


def test_session_codes_present():
    # The tubular-product and ship-structural codes added with the register.
    for name in ("API_5CT", "API_5C3", "API_11B", "IACS_S11", "IACS_S11A",
                 "DNV_RU_SHIP", "IACS_CSR"):
        assert name in codes.REGISTER, f"{name} not registered"
