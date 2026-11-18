# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline comparison contract with independent synthetic hand calculations.

All numerical inputs are defined synthetic examples, not engineering defaults.
Expected differences and tolerance bounds are calculated in the tests below.
Zero denotes the additive identity; unit relative tolerance denotes equality of
error and reference magnitude. Invalid-value cases exercise domain boundaries.
"""

from dataclasses import replace
from fractions import Fraction
import runpy
from pathlib import Path

from numpy import int64, uint64
import pytest

from digitalmodel.structural.result_comparison import (
    ReferenceMetadata,
    ResultComparison,
    ResultKey,
    ResultValue,
    Tolerance,
)


META = ReferenceMetadata(
    model_id="synthetic-frame",
    load_case="static-case",
    state="undeformed-static",
    reference="common-origin-and-node-member-map",
    convention="right-handed; member i-to-j; positive-end-actions",
)
REACTION = ResultKey("node-a", "force", "fy", "node")
DISPLACEMENT = ResultKey("node-b", "translation", "uy", "node")
MEMBER = ResultKey("member-a", "force", "axial", "i")


def pair(reference, candidate, *, family="reactions", metadata=META):
    """Register two already-extracted synthetic datasets."""
    comparison = ResultComparison()
    comparison.add_result("reference", metadata=META, **{family: reference})
    comparison.add_result("candidate", metadata=metadata, **{family: candidate})
    return comparison


def test_reaction_difference_by_hand():
    # -12 - (-10) = -2 N; |-2| = 2 N; 2 / |-10| = 0.2.
    comparison = pair(
        {REACTION: ResultValue(-10, "N", "global")},
        {REACTION: ResultValue(-12, "N", "global")},
    )
    report = comparison.compare_reactions(
        "reference", "candidate", tolerances={REACTION: Tolerance(2, 0, "N")}
    )
    (row,) = report.differences
    assert row.key == REACTION
    assert (row.reference, row.candidate) == (-10, -12)
    assert (row.signed_difference, row.absolute_difference) == (-2, 2)
    assert row.relative_difference == pytest.approx(0.2)
    assert row.allowed_difference == 2
    assert (row.unit, row.coordinate_frame) == ("N", "global")
    assert row.status == report.status == "PASS"
    assert report.metadata == META
    assert (report.reference_name, report.candidate_name) == ("reference", "candidate")
    failed = comparison.compare_reactions(
        "reference", "candidate", tolerances={REACTION: Tolerance(1, 0, "N")}
    )
    assert failed.status == failed.differences[0].status == "FAIL"


def test_displacement_components():
    rotation = ResultKey("node-b", "rotation", "rz", "node")
    # uy: 0.014 - 0.010 = 0.004 m. rz: -0.03 - (-0.02) = -0.01 rad.
    comparison = pair(
        {DISPLACEMENT: ResultValue(0.010, "m", "global"),
         rotation: ResultValue(-0.02, "rad", "global")},
        {rotation: ResultValue(-0.03, "rad", "global"),
         DISPLACEMENT: ResultValue(0.014, "m", "global")},
        family="displacements",
    )
    report = comparison.compare_deflections(
        "reference", "candidate",
        tolerances={DISPLACEMENT: Tolerance(0.005, 0, "m"),
                    rotation: Tolerance(0.005, 0, "rad")},
    )
    rows = {row.key: row for row in report.differences}
    assert rows[DISPLACEMENT].signed_difference == pytest.approx(0.004)
    assert rows[rotation].signed_difference == pytest.approx(-0.01)
    assert rows[DISPLACEMENT].status == "PASS"
    assert rows[rotation].status == report.status == "FAIL"


def test_member_force_identity():
    end_j = replace(MEMBER, location="j")
    moment = ResultKey("member-a", "moment", "mz", "i")
    # End i: 11-10=1 N; end j: -9-(-10)=1 N; moment: 7-5=2 N*m.
    reference = {MEMBER: ResultValue(10, "N", "member-local"),
                 end_j: ResultValue(-10, "N", "member-local"),
                 moment: ResultValue(5, "N*m", "member-local")}
    candidate = {moment: ResultValue(7, "N*m", "member-local"),
                 end_j: ResultValue(-9, "N", "member-local"),
                 MEMBER: ResultValue(11, "N", "member-local")}
    comparison = pair(reference, candidate, family="member_forces")
    report = comparison.compare_member_forces(
        "reference", "candidate",
        tolerances={MEMBER: Tolerance(1, 0, "N"), end_j: Tolerance(1, 0, "N"),
                    moment: Tolerance(2, 0, "N*m")},
    )
    assert {row.key: row.signed_difference for row in report.differences} == {
        MEMBER: 1, end_j: 1, moment: 2,
    }
    assert report.status == "PASS"


def test_empty_overlap_refused():
    other = replace(REACTION, identifier="node-c")
    comparison = pair({REACTION: ResultValue(0, "N", "global")},
                      {other: ResultValue(0, "N", "global")})
    with pytest.raises(ValueError, match="overlap"):
        comparison.compare_reactions("reference", "candidate", tolerances={})


@pytest.mark.parametrize("missing_side", ["reference", "candidate"])
def test_missing_node_refused(missing_side):
    other = replace(REACTION, identifier="node-c")
    comparison = ResultComparison()
    for name in ("reference", "candidate"):
        values = {REACTION: ResultValue(0, "N", "global")}
        if name != missing_side:
            values[other] = ResultValue(0, "N", "global")
        comparison.add_result(name, metadata=META, reactions=values)
    with pytest.raises(ValueError, match="identit"):
        comparison.compare_reactions(
            "reference", "candidate", tolerances={REACTION: Tolerance(0, 0, "N")}
        )


@pytest.mark.parametrize("changed", ["identifier", "quantity", "component", "location"])
def test_member_identity_mismatch_refused(changed):
    altered = replace(MEMBER, **{changed: "moment" if changed == "quantity" else "different"})
    # A matching entry plus a mismatching member action must not be truncated.
    shared = replace(MEMBER, identifier="member-b")
    candidate = {shared: ResultValue(0, "N", "local"),
                 altered: ResultValue(0, "N*m" if changed == "quantity" else "N", "local")}
    comparison = pair(
        {shared: ResultValue(0, "N", "local"), MEMBER: ResultValue(0, "N", "local")},
        candidate, family="member_forces",
    )
    with pytest.raises(ValueError, match="identit"):
        comparison.compare_member_forces("reference", "candidate", tolerances={})


@pytest.mark.parametrize("candidate,absolute,relative,status", [
    (0, 0, 0, "PASS"), (2, 2, 0, "PASS"), (2, 0, 1, "FAIL"),
])
def test_zero_reference(candidate, absolute, relative, status):
    # Reference is zero: relative error is undefined; limit is absolute + r*0.
    comparison = pair({REACTION: ResultValue(0, "N", "global")},
                      {REACTION: ResultValue(candidate, "N", "global")})
    report = comparison.compare_reactions(
        "reference", "candidate",
        tolerances={REACTION: Tolerance(absolute, relative, "N")},
    )
    assert report.differences[0].relative_difference is None
    assert report.status == status


def test_combined_tolerance_by_hand():
    # |13-10|=3 N; 1 N + 0.2*|10 N| = 3 N, inclusive boundary passes.
    comparison = pair({REACTION: ResultValue(10, "N", "global")},
                      {REACTION: ResultValue(13, "N", "global")})
    report = comparison.compare_reactions(
        "reference", "candidate", tolerances={REACTION: Tolerance(1, 0.2, "N")}
    )
    assert report.differences[0].allowed_difference == 3
    assert report.status == "PASS"


@pytest.mark.parametrize("field", list(ReferenceMetadata.__dataclass_fields__))
def test_incompatible_reference_metadata_refused(field):
    comparison = pair({REACTION: ResultValue(0, "N", "global")},
                      {REACTION: ResultValue(0, "N", "global")},
                      metadata=replace(META, **{field: "different"}))
    with pytest.raises(ValueError, match="metadata"):
        comparison.compare_reactions("reference", "candidate", tolerances={})


@pytest.mark.parametrize("unit,frame", [("kN", "global"), ("N", "local")])
def test_incompatible_units_or_frames_refused(unit, frame):
    comparison = pair({REACTION: ResultValue(0, "N", "global")},
                      {REACTION: ResultValue(0, unit, frame)})
    with pytest.raises(ValueError, match="unit|frame"):
        comparison.compare_reactions(
            "reference", "candidate", tolerances={REACTION: Tolerance(0, 0, "N")}
        )


@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -float("inf"), True, "0"])
def test_nonfinite_or_nonnumeric_value_refused(bad):
    with pytest.raises(ValueError, match="finite"):
        ResultComparison().add_result(
            "reference", metadata=META, reactions={REACTION: ResultValue(bad, "N", "global")}
        )


@pytest.mark.parametrize("family,key,unit", [
    ("reactions", REACTION, "N"),
    ("displacements", DISPLACEMENT, "m"),
    ("member_forces", MEMBER, "N"),
])
@pytest.mark.parametrize("bad", [
    2**53 + 1, -(2**53 + 1), Fraction(1, 10),
    int64(2**53 + 1), int64(2**63 - 1), uint64(2**64 - 1),
])
def test_lossy_result_conversion_refused(family, key, unit, bad):
    # (2**53 + 1) - 2**53 = 1; binary floats would erase this difference.
    # 1/10 also has no exact finite binary representation.
    comparison = ResultComparison()
    with pytest.raises(ValueError, match="precision"):
        comparison.add_result(
            "candidate", metadata=META,
            **{family: {key: ResultValue(bad, unit, "global")}},
        )
    # A refused dataset must not reserve its name or retain any result table.
    comparison.add_result(
        "candidate", metadata=META,
        **{family: {key: ResultValue(0, unit, "global")}},
    )


def test_adjacent_large_integers_cannot_false_pass():
    # The exact candidate-reference difference is 1 N, above the 0 N limit.
    with pytest.raises(ValueError, match="precision"):
        comparison = pair(
            {REACTION: ResultValue(2**53, "N", "global")},
            {REACTION: ResultValue(2**53 + 1, "N", "global")},
        )
        comparison.compare_reactions(
            "reference", "candidate", tolerances={REACTION: Tolerance(0, 0, "N")}
        )


@pytest.mark.parametrize("field", ["absolute", "relative"])
@pytest.mark.parametrize("bad", [
    2**53 + 1, Fraction(1, 10),
    int64(2**53 + 1), int64(2**63 - 1), uint64(2**64 - 1),
])
def test_lossy_tolerance_conversion_refused(field, bad):
    comparison = pair(
        {REACTION: ResultValue(1, "N", "global")},
        {REACTION: ResultValue(2, "N", "global")},
    )
    tolerance = replace(Tolerance(0, 0, "N"), **{field: bad})
    with pytest.raises(ValueError, match="precision"):
        comparison.compare_reactions(
            "reference", "candidate", tolerances={REACTION: tolerance}
        )


@pytest.mark.parametrize("reference,candidate,difference", [
    (2**53 - 1, 2**53, 1),
    (2**53, 2**53 + 2, 2),
    (Fraction(1, 2), Fraction(3, 4), 0.25),
    (int64(2**53), int64(2**53 + 2), 2),
    (uint64(2**63), uint64(2**63 + 2048), 2048),
])
def test_exact_numeric_conversion_retains_difference(reference, candidate, difference):
    # Boundary integers differ by 1, 2 and 2048 N; 3/4 - 1/2 = 1/4 N.
    comparison = pair(
        {REACTION: ResultValue(reference, "N", "global")},
        {REACTION: ResultValue(candidate, "N", "global")},
    )
    report = comparison.compare_reactions(
        "reference", "candidate", tolerances={REACTION: Tolerance(0, 0, "N")}
    )
    (row,) = report.differences
    assert row.reference == reference
    assert row.candidate == candidate
    assert row.signed_difference == pytest.approx(difference)
    assert row.allowed_difference == 0
    assert row.status == report.status == "FAIL"


@pytest.mark.parametrize("field", list(ReferenceMetadata.__dataclass_fields__))
def test_missing_reference_metadata_refused(field):
    with pytest.raises(ValueError, match="metadata"):
        ResultComparison().add_result(
            "reference", metadata=replace(META, **{field: " "}),
            reactions={REACTION: ResultValue(0, "N", "global")},
        )


@pytest.mark.parametrize("field", list(ResultKey.__dataclass_fields__))
def test_missing_identifier_or_identity_refused(field):
    with pytest.raises(ValueError, match="identity"):
        ResultComparison().add_result(
            "reference", metadata=META,
            reactions={replace(REACTION, **{field: " "}): ResultValue(0, "N", "global")},
        )


@pytest.mark.parametrize("unit,frame", [("", "global"), ("kg", "global"), ("N", " ")])
def test_missing_or_wrong_dimension_metadata_refused(unit, frame):
    with pytest.raises(ValueError, match="unit|frame"):
        ResultComparison().add_result(
            "reference", metadata=META, reactions={REACTION: ResultValue(0, unit, frame)}
        )


@pytest.mark.parametrize("family,key,unit", [
    ("reactions", DISPLACEMENT, "m"),
    ("displacements", REACTION, "N"),
    ("member_forces", DISPLACEMENT, "m"),
])
def test_wrong_quantity_for_family_refused(family, key, unit):
    with pytest.raises(ValueError, match="quantity"):
        ResultComparison().add_result(
            "reference", metadata=META, **{family: {key: ResultValue(0, unit, "global")}}
        )


def test_duplicate_identifiers_refused():
    entry = (REACTION, ResultValue(0, "N", "global"))
    with pytest.raises(ValueError, match="duplicate"):
        ResultComparison().add_result("reference", metadata=META, reactions=[entry, entry])
    comparison = ResultComparison()
    comparison.add_result("reference", metadata=META, reactions=[entry])
    with pytest.raises(ValueError, match="duplicate"):
        comparison.add_result("reference", metadata=META, reactions=[entry])


def test_empty_result_and_absent_family_refused():
    with pytest.raises(ValueError, match="empty"):
        ResultComparison().add_result("reference", metadata=META)
    comparison = pair({REACTION: ResultValue(0, "N", "global")},
                      {REACTION: ResultValue(0, "N", "global")})
    with pytest.raises(ValueError, match="empty"):
        comparison.compare_deflections("reference", "candidate", tolerances={})


def test_unknown_result_refused():
    with pytest.raises(ValueError, match="unknown"):
        ResultComparison().compare_reactions("absent", "candidate", tolerances={})


@pytest.mark.parametrize("tolerances", [
    {}, {REACTION: Tolerance(0, 0, "kN")},
    {REACTION: Tolerance(-1, 0, "N")}, {REACTION: Tolerance(0, -1, "N")},
    {REACTION: Tolerance(float("inf"), 0, "N")},
    {REACTION: Tolerance(0, float("nan"), "N")},
    {REACTION: Tolerance(True, 0, "N")},
    {REACTION: Tolerance(0, 0, "N"), DISPLACEMENT: Tolerance(0, 0, "m")},
])
def test_invalid_or_incomplete_tolerances_refused(tolerances):
    comparison = pair({REACTION: ResultValue(0, "N", "global")},
                      {REACTION: ResultValue(0, "N", "global")})
    with pytest.raises(ValueError, match="tolerance"):
        comparison.compare_reactions("reference", "candidate", tolerances=tolerances)


def test_registration_snapshots_input_mapping():
    reference = {REACTION: ResultValue(0, "N", "global")}
    comparison = pair(reference, reference)
    reference.clear()
    assert comparison.compare_reactions(
        "reference", "candidate", tolerances={REACTION: Tolerance(0, 0, "N")}
    ).status == "PASS"


@pytest.mark.parametrize("reference,candidate,absolute,relative", [
    (-1e308, 1e308, 0, 0), (1e308, 1e308, 0, 1e308),
])
def test_arithmetic_overflow_refused(reference, candidate, absolute, relative):
    # Finite extreme inputs can produce nonfinite differences or limits.
    comparison = pair({REACTION: ResultValue(reference, "N", "global")},
                      {REACTION: ResultValue(candidate, "N", "global")})
    with pytest.raises(ValueError, match="finite"):
        comparison.compare_reactions(
            "reference", "candidate",
            tolerances={REACTION: Tolerance(absolute, relative, "N")},
        )


def test_synthetic_example(capsys):
    example = Path(__file__).resolve().parents[2] / "examples/structural/result_comparison.py"
    runpy.run_path(str(example), run_name="__main__")
    output = capsys.readouterr().out
    assert "reactions: PASS" in output
    assert "displacements: FAIL" in output
    assert "member_forces: PASS" in output
