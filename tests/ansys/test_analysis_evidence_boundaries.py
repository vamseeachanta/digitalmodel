"""Synthetic boundary tests; passing fixtures establish no engineering authority."""

import json
from decimal import Overflow, Subnormal, localcontext
from pathlib import Path

import pytest

from digitalmodel.ansys.analysis_evidence import (
    build_package,
    load_package,
    read_response_csv,
    response_csv,
)
from digitalmodel.ansys.analysis_intake import _make_case
from digitalmodel.ansys.analysis_lookup import lookup
from digitalmodel.ansys.analysis_records import digest_bytes
from tests.ansys.test_analysis_evidence import intake as intake
from tests.ansys.test_analysis_lookup import query
from tests.ansys.test_analysis_qualification import protocol as protocol
from tests.ansys.test_analysis_review_regressions import candidate as candidate


def _repin(protocol, **entry_changes):
    """Bind deliberate fixture changes without weakening any production gate."""
    study, resolver, _, ledger, authority = protocol
    preliminary = build_package(study, resolver)
    register = json.loads(resolver["register"].read_bytes())
    entry = register["entries"]["case-1:stress"]
    entry.update(entry_changes, row_hash=preliminary["cases"][0]["row_hash"])
    resolver["register"].write_text(json.dumps(register), encoding="utf-8")
    ref = {"id": "register", "sha256": digest_bytes(resolver["register"].read_bytes())}
    study["verification_reference"] = ref
    package = build_package(study, resolver)
    authority.update(verification_reference=ref, revisions={"r1": package["package_hash"]})
    return package, resolver, ledger, authority


@pytest.mark.parametrize("actual,expected,tolerance,passes", [
    ("1000", "0", "1000", True),
    ("1000", "-1", "1000", False),
    ("0.00001", "0", "0.00001", True),
    ("0.00001", "0", "0", False),
    ("12.5", "-0.00000000000000000000000000001", "12.5", False),
])
def test_exact_comparison_ignores_ambient_decimal_context(
    protocol, actual, expected, tolerance, passes,
):
    """Hand calculations for AC-07 (criteria revision 1, 2026), not design limits.

    |1000 - 0| = 1000 <= 1000; |1000 - (-1)| = 1001 > 1000.
    |0.00001 - 0| = 0.00001, equal to its tolerance and greater than zero.
    |12.5 - (-1e-29)| = 12.5 + 1e-29 > 12.5, with no rounding permitted.
    Context settings are adversarial test inputs, not engineering coefficients.
    """
    protocol[0]["cases"][0]["responses"][0]["value"] = actual
    package, resolver, ledger, authority = _repin(
        protocol, expected=expected, absolute_tolerance=tolerance,
    )
    with localcontext() as context:
        # Python 3.11 decimal.Context: force overflow and trapped subnormal paths.
        context.prec = 2
        context.Emax = 2
        context.Emin = -2
        context.traps[Overflow] = False
        context.traps[Subnormal] = True
        before = context.copy()
        if passes:
            assert lookup(package, query(), resolver, ledger=ledger,
                          authority=authority)["qualified"]
        else:
            with pytest.raises(ValueError, match="comparison"):
                lookup(package, query(), resolver, ledger=ledger, authority=authority)
        assert context.flags == before.flags
        assert context.traps == before.traps
        assert (context.prec, context.Emax, context.Emin) == (
            before.prec, before.Emax, before.Emin,
        )


@pytest.mark.parametrize("checker", [
    "unknown", " UNVERIFIED ", "Historical-Neutral-Capture",
])
def test_placeholder_checker_cannot_establish_independence(protocol, checker):
    """AC-07 requires an identified non-author checker, not a placeholder label."""
    package, resolver, ledger, authority = _repin(protocol, checker=checker)
    with pytest.raises(ValueError, match="checker"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)


def test_intake_parses_the_single_verified_snapshot(candidate, monkeypatch):
    """AC-02/AC-10: a replacement after read must not change extracted tokens."""
    record, refs, resolver = candidate
    source = resolver["capture"]
    original_read = Path.read_bytes
    reads = []

    def replace_after_read(path):
        raw = original_read(path)
        if path == source:
            reads.append(path)
            path.write_bytes(b"stress,99")
        return raw

    monkeypatch.setattr(Path, "read_bytes", replace_after_read)
    case = _make_case(record, refs, {"stress": "MPa"}, "capture", resolver, "test")
    # The synthetic source literal in the candidate fixture is stress,12.
    assert case["responses"][0]["value"] == "12"
    assert reads == [source]
    assert case["evidence"] == refs


def test_committed_example_manifest_and_csv_remain_bound():
    """AC-09/AC-10: the committed manifest is the independent export oracle.

    Compare every retained record and typed field; no engineering value is
    invented or qualified here. Newline normalization follows utf8-lf-v1 in
    analysis_intake.code_fingerprint, so LF and CRLF checkouts are equivalent.
    """
    root = Path(__file__).resolve().parents[2] / "examples/ansys/analysis-evidence"
    package = load_package(root / "manifest.json")
    committed = (root / "responses.csv").read_text(encoding="utf-8")
    assert response_csv(package) == committed
    expected = [
        {"case_id": case["case_id"], "row_hash": case["row_hash"],
         "dataset_id": package["dataset_id"], "revision": package["revision"],
         "capture_role": case["capture_role"], "response": response}
        for case in package["cases"] for response in case["responses"]
    ]
    assert read_response_csv(committed) == expected
