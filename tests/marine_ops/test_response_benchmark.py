# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reference intake tests; no physical response qualification is asserted."""

from dataclasses import fields, replace

import pytest

from digitalmodel.marine_ops.response_benchmark import (
    ResponseReference,
    inspect_response_reference,
)

# RFC 6234 (2011), section 8.5, SHA256 test 1: literal bytes b"abc".
# This published digest tests byte identity only, not engineering accuracy.
ABC_SHA256 = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"


def reference():
    """Supply synthetic declarations to exercise metadata, never a benchmark."""
    return ResponseReference(
        citation="Synthetic intake fixture; not a physical reference",
        source_locator="https://example.org/reference",
        reuse_basis="Synthetic test data created for this test",
        independence_basis="Synthetic declaration, not authenticated",
        frequency_dependence_basis="Synthetic declaration, not authenticated",
        dof="heave",
        frequency_unit="rad/s",
        significant_amplitude_unit="m",
        acceptance_criteria="Required comparison limits supplied externally",
        declared_sha256=ABC_SHA256,
    )


def test_published_digest_and_complete_metadata_do_not_qualify_response():
    """RFC 6234 sec. 8.5 supplies the hash; no response value is invented."""
    result = inspect_response_reference(reference(), artifact=b"abc")
    assert result.observed_sha256 == ABC_SHA256
    assert result.intake_complete is True
    assert result.gaps == ()
    assert result.benchmark_status == "NOT_EVALUATED"


@pytest.mark.parametrize("field", [f.name for f in fields(ResponseReference)])
@pytest.mark.parametrize("missing", [None, "", " \n "])
def test_every_evidence_field_is_required(field, missing):
    result = inspect_response_reference(
        replace(reference(), **{field: missing}), artifact=b"abc"
    )
    assert result.intake_complete is False
    assert f"missing:{field}" in result.gaps
    assert result.benchmark_status == "NOT_EVALUATED"


def test_empty_intake_lists_evidence_gaps_without_a_pass():
    result = inspect_response_reference(ResponseReference())
    assert result.gaps == tuple(
        f"missing:{f.name}" for f in fields(ResponseReference)
    ) + ("missing:artifact",)
    assert result.observed_sha256 is None
    assert result.intake_complete is False
    assert result.benchmark_status == "NOT_EVALUATED"


@pytest.mark.parametrize(
    "artifact, gap", [(None, "missing:artifact"), (b"", "empty:artifact")]
)
def test_no_reference_bytes_cannot_complete_intake(artifact, gap):
    result = inspect_response_reference(reference(), artifact=artifact)
    assert gap in result.gaps
    assert result.intake_complete is False


def test_changed_bytes_fail_identity_check():
    """Appending a newline changes the RFC test message, hence its identity."""
    result = inspect_response_reference(reference(), artifact=b"abc\n")
    assert "mismatch:artifact_sha256" in result.gaps
    assert result.intake_complete is False


@pytest.mark.parametrize(
    "digest", ["not-a-digest", ABC_SHA256[:-1], ABC_SHA256.upper()]
)
def test_digest_requires_canonical_lowercase_sha256(digest):
    result = inspect_response_reference(
        replace(reference(), declared_sha256=digest), artifact=b"abc"
    )
    assert "invalid:declared_sha256" in result.gaps
    assert result.intake_complete is False


@pytest.mark.parametrize(
    "dof, unit",
    [
        ("surge", "m"),
        ("sway", "m"),
        ("heave", "m"),
        ("roll", "deg"),
        ("pitch", "deg"),
        ("yaw", "deg"),
    ],
)
def test_canonical_units_follow_envelope_basis_b5(dof, unit):
    result = inspect_response_reference(
        replace(reference(), dof=dof, significant_amplitude_unit=unit), artifact=b"abc"
    )
    assert result.intake_complete is True
    assert result.benchmark_status == "NOT_EVALUATED"


@pytest.mark.parametrize(
    "changes, gap",
    [
        ({"dof": "Heave"}, "unsupported:dof"),
        ({"frequency_unit": "Hz"}, "unsupported:frequency_unit"),
        (
            {"significant_amplitude_unit": "m/m"},
            "incompatible:significant_amplitude_unit",
        ),
        (
            {"dof": "roll", "significant_amplitude_unit": "rad"},
            "incompatible:significant_amplitude_unit",
        ),
    ],
)
def test_units_are_not_inferred_or_converted(changes, gap):
    result = inspect_response_reference(
        replace(reference(), **changes), artifact=b"abc"
    )
    assert gap in result.gaps
    assert result.intake_complete is False


@pytest.mark.parametrize("bad_value", [False, [], {}])
def test_evidence_is_text_not_truthiness(bad_value):
    result = inspect_response_reference(
        replace(reference(), acceptance_criteria=bad_value), artifact=b"abc"
    )
    assert "missing:acceptance_criteria" in result.gaps


def test_inspection_is_read_only():
    evidence = reference()
    inspect_response_reference(evidence, artifact=b"abc")
    assert evidence == reference()
