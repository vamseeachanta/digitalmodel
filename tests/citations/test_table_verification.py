# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exact comparison tests with synthetic quantities, not standard values.

The independent example is a length formed by two segments of three metres:
3 m + 3 m = 6 m. Six millimetres has the same numeral but a different unit.
All numbers below are deliberate test inputs, not engineering coefficients
or acceptance limits. Source identity is synthetic fixture metadata.
"""

from dataclasses import replace
import os
from pathlib import Path
import subprocess
import sys

import pytest

from digitalmodel.citations import Citation, CitedValue, CitationResolutionError
from digitalmodel.citations.table_verification import (
    TableVerificationError,
    verify_table_value,
)


@pytest.fixture
def reference(tmp_path):
    citation = Citation(
        code_id="synthetic-length",
        publisher="synthetic",
        revision="example",
        section="Example length",
        wiki_path="wikis/synthetic/length.md",
        note="two segments of three metres",
    )
    page = tmp_path / citation.wiki_path
    page.parent.mkdir(parents=True)
    page.write_text(
        "---\ncode_id: synthetic-length\npublisher: synthetic\n"
        "revision: example\n---\nSynthetic arithmetic only.\n",
        encoding="utf-8",
    )
    return CitedValue(value=6, citation=citation, units="m"), page


def test_exact_independent_sum(reference, tmp_path):
    """Independent hand calculation: 3 + 3 = 6, with both sides in m."""
    expected, _ = reference
    actual = replace(expected, value=3 + 3)
    assert verify_table_value(actual, expected, repo_root=tmp_path) is None
    verify_table_value(replace(actual, value=6.0), expected, repo_root=tmp_path)


@pytest.mark.parametrize("units", ["mm", "s", "dimensionless", "", None])
def test_unchanged_numeral_cannot_hide_unit_mutation(reference, tmp_path, units):
    expected, _ = reference
    with pytest.raises(TableVerificationError, match="units"):
        verify_table_value(replace(expected, units=units), expected, repo_root=tmp_path)


@pytest.mark.parametrize(
    ("field", "value"),
    [
        ("code_id", "other-source"),
        ("publisher", "other-publisher"),
        ("revision", "other-edition"),
        ("section", "Other table"),
        ("wiki_path", "wikis/synthetic/other.md"),
        ("note", "different row basis"),
        ("source_sibling", "other-scope"),
        ("source_project", "other-scope"),
    ],
)
def test_unchanged_numeral_cannot_hide_source_mutation(
    reference, tmp_path, field, value
):
    expected, _ = reference
    citation = replace(expected.citation, **{field: value})
    with pytest.raises(TableVerificationError, match=field):
        verify_table_value(
            replace(expected, citation=citation), expected, repo_root=tmp_path
        )


@pytest.mark.parametrize("side", ["actual", "expected"])
@pytest.mark.parametrize("units", ["", " ", None])
def test_empty_unit_contract_is_rejected(reference, tmp_path, side, units):
    expected, _ = reference
    actual = expected
    if side == "actual":
        actual = replace(actual, units=units)
    else:
        expected = replace(expected, units=units)
    with pytest.raises(TableVerificationError, match="units"):
        verify_table_value(actual, expected, repo_root=tmp_path)


@pytest.mark.parametrize("side", ["actual", "expected"])
@pytest.mark.parametrize(
    "value", [True, False, float("nan"), float("inf"), -float("inf"), "6", None]
)
def test_nonfinite_or_nonnumeric_values_are_rejected(reference, tmp_path, side, value):
    expected, _ = reference
    actual = expected
    if side == "actual":
        actual = replace(actual, value=value)
    else:
        expected = replace(expected, value=value)
    with pytest.raises(TableVerificationError, match="finite numeric"):
        verify_table_value(actual, expected, repo_root=tmp_path)


def test_numeric_difference_is_rejected(reference, tmp_path):
    """Hand calculation: 3 + 3 = 6; 3 + 3 + 1 = 7, so exact equality fails."""
    expected, _ = reference
    with pytest.raises(TableVerificationError, match="value"):
        verify_table_value(replace(expected, value=7), expected, repo_root=tmp_path)


@pytest.mark.parametrize("side", ["actual", "expected"])
@pytest.mark.parametrize("defect", ["quantity", "citation"])
def test_required_metadata_objects_are_rejected_when_absent(
    reference, tmp_path, side, defect
):
    expected, _ = reference
    invalid = None if defect == "quantity" else replace(expected, citation=None)
    actual, expected = (invalid, expected) if side == "actual" else (expected, invalid)
    with pytest.raises(TableVerificationError, match="CitedValue|Citation"):
        verify_table_value(actual, expected, repo_root=tmp_path)


@pytest.mark.parametrize("mutation", ["missing", "code_id", "publisher", "revision"])
def test_matching_citation_labels_must_resolve(reference, tmp_path, mutation):
    expected, page = reference
    # A matching label is insufficient; its backing page must still resolve.
    if mutation == "missing":
        page.rename(page.with_suffix(".unavailable"))
    else:
        text = page.read_text(encoding="utf-8")
        text = text.replace(f"{mutation}: ", f"{mutation}: changed-")
        page.write_text(text, encoding="utf-8")
    with pytest.raises(CitationResolutionError):
        verify_table_value(expected, expected, repo_root=tmp_path)


def test_metadata_is_checked_before_numbers(reference, tmp_path):
    expected, page = reference
    invalid_number = replace(expected, value=float("nan"))
    with pytest.raises(TableVerificationError, match="units"):
        verify_table_value(
            replace(invalid_number, units="mm"), expected, repo_root=tmp_path
        )
    with pytest.raises(TableVerificationError, match="section"):
        verify_table_value(
            replace(invalid_number, citation=replace(expected.citation, section="Other")),
            expected,
            repo_root=tmp_path,
        )
    page.rename(page.with_suffix(".unavailable"))
    with pytest.raises(CitationResolutionError):
        verify_table_value(invalid_number, expected, repo_root=tmp_path)


@pytest.mark.parametrize("mutation", ["match", "unit", "source", "unresolved", "value"])
def test_checks_survive_optimized_python(reference, tmp_path, mutation):
    """Use explicit exit paths in a real optimized interpreter, without assert."""
    expected, _ = reference
    program = """
from dataclasses import replace
from pathlib import Path
import sys
from digitalmodel.citations import Citation, CitedValue, CitationResolutionError
from digitalmodel.citations.table_verification import (
    TableVerificationError, verify_table_value,
)
root, mutation, wiki_path = sys.argv[1:]
citation = Citation(code_id='synthetic-length', publisher='synthetic',
    revision='example', section='Example length', wiki_path=wiki_path,
    note='two segments of three metres')
expected = CitedValue(value=6, citation=citation, units='m')
actual = expected
if mutation == 'unit':
    actual = replace(actual, units='mm')
elif mutation == 'source':
    actual = replace(actual, citation=replace(citation, section='Other table'))
elif mutation == 'unresolved':
    expected = replace(expected, citation=replace(citation, revision='unavailable'))
    actual = expected
elif mutation == 'value':
    actual = replace(actual, value=7)
try:
    verify_table_value(actual, expected, repo_root=Path(root))
except (TableVerificationError, CitationResolutionError):
    sys.exit(0 if mutation != 'match' else 1)
sys.exit(0 if mutation == 'match' else 1)
"""
    env = dict(os.environ)
    env["PYTHONPATH"] = os.pathsep.join(sys.path)
    completed = subprocess.run(
        [sys.executable, "-O", "-c", program, str(tmp_path), mutation,
         expected.citation.wiki_path],
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )
    assert completed.returncode == 0, completed.stderr


@pytest.mark.parametrize("field", ["code_id", "publisher", "revision"])
@pytest.mark.parametrize("optimized", [False, True], ids=["normal", "optimized"])
def test_matching_pair_rejects_stale_source(tmp_path, field, optimized):
    """Reuse the same pair after a successful comparison and a source edit."""
    env = dict(os.environ)
    env["PYTHONPATH"] = os.pathsep.join(sys.path)
    probe = Path(__file__).with_name("table_stale_source_probe.py")
    completed = subprocess.run(
        [
            sys.executable,
            *(["-O"] if optimized else []),
            str(probe),
            str(tmp_path),
            field,
        ],
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )
    assert completed.returncode == 0, completed.stdout + completed.stderr
