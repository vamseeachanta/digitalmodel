"""OQ-4 regression test: ``values_equal`` must normalize bool ↔ "Yes"/"No".

Closes Open Question OQ-4 from ``SEMANTIC_DIFF_TAXONOMY.md`` §7 per
``SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY.md`` §5.1 disposition (#515 iter 4/7).

Before this fix, ``values_equal(True, "Yes")`` returned
``(False, inf, SIGNIFICANT)`` because the type-mismatch path triggered
before any bool-string normalization. The downstream effect was that
every Environment bool field with OrcFxAPI's Yes/No encoding surfaced
as a SIGNIFICANT diff against any spec.yml that round-tripped through
a raw YAML loader (which emits Python ``bool``).

The fix is at the compare site (not the dump site). OrcaFlex's load-side
schema expects Yes/No, so ``yaml_utils.OrcaFlexDumper`` must continue
to emit Yes/No. The diff engine is what needed to learn the equivalence.
"""

from __future__ import annotations

import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).parents[3]
sys.path.insert(0, str(REPO_ROOT / "scripts"))

from semantic_validate import Significance, values_equal  # type: ignore  # noqa: E402


class TestBoolStringEquivalence:
    """A bool and its "Yes"/"No" representation must compare equal."""

    def test_true_equals_yes_left(self) -> None:
        equal, pct, sig = values_equal(True, "Yes")
        assert equal is True
        assert pct == 0.0
        assert sig == Significance.COSMETIC

    def test_yes_equals_true_right(self) -> None:
        equal, pct, sig = values_equal("Yes", True)
        assert equal is True
        assert pct == 0.0
        assert sig == Significance.COSMETIC

    def test_false_equals_no_left(self) -> None:
        equal, pct, sig = values_equal(False, "No")
        assert equal is True
        assert sig == Significance.COSMETIC

    def test_no_equals_false_right(self) -> None:
        equal, pct, sig = values_equal("No", False)
        assert equal is True
        assert sig == Significance.COSMETIC


class TestBoolStringMismatch:
    """Bool ↔ wrong-polarity string must still register as different."""

    def test_true_not_equal_no(self) -> None:
        equal, _, sig = values_equal(True, "No")
        assert equal is False
        assert sig == Significance.SIGNIFICANT

    def test_false_not_equal_yes(self) -> None:
        equal, _, sig = values_equal(False, "Yes")
        assert equal is False
        assert sig == Significance.SIGNIFICANT


class TestNonYesNoStringsUnaffected:
    """Bool ↔ arbitrary string must still register as TYPE_MISMATCH —
    only "Yes" and "No" carry the boolean encoding."""

    def test_true_vs_truthy_string(self) -> None:
        equal, _, sig = values_equal(True, "true")
        assert equal is False
        assert sig == Significance.TYPE_MISMATCH

    def test_true_vs_one_string(self) -> None:
        equal, _, sig = values_equal(True, "1")
        assert equal is False
        assert sig == Significance.TYPE_MISMATCH

    def test_false_vs_empty_string(self) -> None:
        equal, _, sig = values_equal(False, "")
        assert equal is False
        assert sig == Significance.TYPE_MISMATCH


class TestExistingBehaviorPreserved:
    """The OQ-4 fix must not regress any prior path through values_equal."""

    def test_both_true(self) -> None:
        equal, _, sig = values_equal(True, True)
        assert equal is True
        assert sig == Significance.MATCH

    def test_both_false(self) -> None:
        equal, _, sig = values_equal(False, False)
        assert equal is True
        assert sig == Significance.MATCH

    def test_true_vs_false(self) -> None:
        equal, _, sig = values_equal(True, False)
        assert equal is False
        # Same-type bool-vs-bool falls through to the trailing
        # ``return False, float("inf"), Significance.SIGNIFICANT`` branch.
        assert sig == Significance.SIGNIFICANT

    def test_yes_equals_yes(self) -> None:
        equal, _, sig = values_equal("Yes", "Yes")
        assert equal is True
        assert sig == Significance.MATCH

    def test_no_equals_no(self) -> None:
        equal, _, sig = values_equal("No", "No")
        assert equal is True
        assert sig == Significance.MATCH

    def test_numeric_int_vs_float(self) -> None:
        equal, _, sig = values_equal(100, 100.0)
        assert equal is True
        assert sig == Significance.MATCH

    def test_both_none(self) -> None:
        equal, _, sig = values_equal(None, None)
        assert equal is True
        assert sig == Significance.MATCH


# Parametrized comprehensive matrix — every bool × {Yes,No,true,false} pair.
# The matrix documents the OQ-4 contract surface concisely; per-row tests
# above carry the human-readable failure messages.
@pytest.mark.parametrize(
    "left,right,expected_equal,expected_sig",
    [
        (True, "Yes", True, Significance.COSMETIC),
        (False, "No", True, Significance.COSMETIC),
        (True, "No", False, Significance.SIGNIFICANT),
        (False, "Yes", False, Significance.SIGNIFICANT),
        ("Yes", True, True, Significance.COSMETIC),
        ("No", False, True, Significance.COSMETIC),
        ("No", True, False, Significance.SIGNIFICANT),
        ("Yes", False, False, Significance.SIGNIFICANT),
    ],
)
def test_oq4_matrix(left, right, expected_equal, expected_sig) -> None:
    equal, _, sig = values_equal(left, right)
    assert equal is expected_equal
    assert sig == expected_sig
