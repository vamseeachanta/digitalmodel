# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent geometry examples for the starting-shape admission screen.

Synthetic dimensions below are test inputs, not design recommendations.
The two straight segments (0,0,0)->(3,4,0)->(6,8,0) each have length
sqrt(3**2 + 4**2) = 5, hence total length 10 and three nodes.
Distance basis: OpenStax, Calculus Volume 3 (2016), section 2.2.
Acceptance basis: AQG r0 section 4 G0.6, as transcribed in
src/digitalmodel/solvers/orcaflex/docs/agent-pack.md (C06).

Repository examples also characterize the current admission policy: every
starting-shape FAIL becomes an ERROR, including on hand-authored models.
Arc length is compared with unstretched section lengths; a mismatch alone
does not establish physical invalidity or native load failure. The severity
policy for hand-authored models remains a maintainer decision.
"""

from copy import deepcopy
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.starting_shape_validation import (
    validate_starting_shapes,
)
from digitalmodel.solvers.orcaflex.yaml_validator import OrcaFlexYAMLValidator

SHAPE = "StartingShapeX, StartingShapeY, StartingShapeZ"
SECTIONS = "LineType, Length, NumberOfSegments"
CONNECTIONS = "Connection, ConnectionX, ConnectionY, ConnectionZ"


@pytest.fixture
def model():
    return {
        "General": {"UnitsSystem": "SI"},
        "LineTypes": [{"Name": "pipe", "OD": 0.2}],
        "Lines": [{
            "Name": "test line", "StaticsStep1": "User specified",
            SHAPE: [[0, 0, 0], [3, 4, 0], [6, 8, 0]],
            SECTIONS: [["pipe", 5, 1], ["pipe", 5, 1]],
            CONNECTIONS: [["Fixed", 0, 0, 0], ["Fixed", 6, 8, 0]],
        }],
    }


def findings(model):
    return {f.check: f for f in validate_starting_shapes(model)}


def test_hand_calculated_geometry(model):
    result = findings(model)
    for name in ("table", "arc_length", "end_a", "end_b", "sections"):
        assert result[name].status == "PASS"
    assert result["arc_length"].actual == pytest.approx(0)
    # XYZ coordinates contain no evidence of the source line's diameter.
    assert result["dimensions"].status == "NOT_EVALUATED"


def test_repository_d04_example_passes_file_validation():
    """The existing library example passes the offline geometry screen.

    The source file is used directly so its sections, table encodings and
    coordinate frames are exercised together through the public validator.
    Passing this screen does not establish native statics acceptance.
    """
    root = Path(__file__).resolve().parents[3]
    path = root / "docs/domains/orcaflex/examples/yml/D04/D04 J-tube pull in.yml"
    result = OrcaFlexYAMLValidator().validate_file(path)
    assert result.valid, [str(issue) for issue in result.errors]
    lengths = [f for f in result.starting_shapes if f.check == "arc_length"]
    assert lengths
    assert all(f.status == "PASS" for f in lengths)
    assert not any(f.status == "FAIL" for f in result.starting_shapes)


def test_repository_installation_model_is_rejected_only_for_arc_length():
    """An existing installation model flips validity under the C06 screen.

    This records the existing ERROR policy while its applicability to
    stretched starting shapes awaits a maintainer decision. No solver
    acceptance or physical defect is inferred from the geometric mismatch.
    """
    root = Path(__file__).resolve().parents[3]
    path = root / (
        "docs/domains/orcaflex/installation/pipeline/route_a/monolithic/Step1.yml"
    )
    result = OrcaFlexYAMLValidator().validate_file(path)
    assert not result.valid
    assert result.errors
    assert all(issue.property == "StartingShape" for issue in result.errors)
    failures = [f for f in result.starting_shapes if f.status == "FAIL"]
    assert [f.check for f in failures] == ["arc_length"]
    assert failures[0].actual > failures[0].limit
    assert failures[0].limit == pytest.approx(0.001)


def test_missing_table_is_distinct_from_bad_table(model):
    del model["Lines"][0][SHAPE]
    assert findings(model)["table"].message.startswith("Missing")
    assert findings(model)["table"].status == "FAIL"


@pytest.mark.parametrize("table", [[], [[0, 0, 0]], [[0, 0]],
                                    [[0, 0, 0], [True, 1, 1]],
                                    [[0, 0, 0], [float("nan"), 1, 1]],
                                    [[0, 0, 0], [float("inf"), 1, 1]],
                                    [[0, 0, 0], ["1", 1, 1]]])
def test_malformed_table(model, table):
    model["Lines"][0][SHAPE] = table
    assert findings(model)["table"].status == "FAIL"


def test_arc_length_boundary_and_excess(model):
    """Straight x-axis: L=1000, shape=1001 => |1001-1000|/1000=.001.

    AQG r0 G0.6 permits 0.1 percent including equality. At 1001.1 the
    independently calculated fraction is .0011, which must fail.
    """
    line = model["Lines"][0]
    line[SECTIONS] = [["pipe", 1000, 1]]
    line[SHAPE] = [[0, 0, 0], [1001, 0, 0]]
    assert findings(model)["arc_length"].status == "PASS"
    line[SHAPE][-1][0] = 1001.1
    assert findings(model)["arc_length"].status == "FAIL"


@pytest.mark.parametrize("end,index", [("end_a", 0), ("end_b", -1)])
def test_endpoint_boundary_and_excess(model, end, index):
    """A z-only displacement gives distance |dz|: .01 passes, .011 fails.

    AQG r0 G0.6 endpoint limit is .01 m, inclusive.
    """
    line = model["Lines"][0]
    line[SHAPE][index][2] = 0.01
    assert findings(model)[end].status == "PASS"
    line[SHAPE][index][2] = 0.011
    assert findings(model)[end].status == "FAIL"


@pytest.mark.parametrize("translation", [0.0, 1.0, 1_000_000.0])
@pytest.mark.parametrize("axis", [0, 1, 2])
@pytest.mark.parametrize("direction", [-1, 1])
def test_translated_length_limits_are_inclusive(model, translation, axis, direction):
    """Decimal lengths .4004 and .3996 differ from .4 by exactly .1%."""
    line = model["Lines"][0]
    line[SECTIONS] = [["pipe", 0.4, 1]]
    start = [translation] * 3
    end = start.copy()
    end[axis] = translation + 0.4 + direction * 0.0004
    line[SHAPE] = [start, end]
    result = findings(model)
    assert result["arc_length"].status == "PASS"
    assert result["sections"].status == "PASS"
    # The added .000001 m is far above binary64 roundoff at each origin.
    end[axis] += direction * 0.000001
    result = findings(model)
    assert result["arc_length"].status == "FAIL"
    assert result["sections"].status == "FAIL"


@pytest.mark.parametrize("translation", [0.0, 1.0, 1_000_000.0])
@pytest.mark.parametrize("axis", [0, 1, 2])
@pytest.mark.parametrize("end,index", [("end_a", 0), ("end_b", -1)])
@pytest.mark.parametrize("direction", [-1, 1])
def test_translated_endpoint_limits_are_inclusive(
    model, translation, axis, end, index, direction,
):
    """One coordinate displaced by .01 m is exactly at the endpoint limit."""
    line = model["Lines"][0]
    target = [translation] * 3
    point = target.copy()
    point[axis] += direction * 0.01
    line[CONNECTIONS][index] = ["Fixed", *target]
    line[SHAPE][index] = point
    assert findings(model)[end].status == "PASS"
    point[axis] += direction * 0.000001
    assert findings(model)[end].status == "FAIL"


@pytest.mark.parametrize("translation", [0.0, 1.0, 1_000_000.0])
@pytest.mark.parametrize("direction", [-1, 1])
def test_translated_section_boundary_with_matching_total(model, translation, direction):
    """Chords .4004 and .3996 sum to .8; each .4 section is at its limit."""
    line = model["Lines"][0]
    line[SECTIONS] = [["pipe", 0.4, 1], ["pipe", 0.4, 1]]
    line[SHAPE] = [
        [translation, 0, 0],
        [translation + 0.4 + direction * 0.0004, 0, 0],
        [translation + 0.8, 0, 0],
    ]
    result = findings(model)
    assert result["arc_length"].status == "PASS"
    assert result["sections"].status == "PASS"
    line[SHAPE][1][0] += direction * 0.000001
    result = findings(model)
    assert result["arc_length"].status == "PASS"
    assert result["sections"].status == "FAIL"


def test_small_resolvable_length_excess_is_rejected(model):
    """A 1e-12 m excess exceeds roundoff without a broad absolute epsilon."""
    line = model["Lines"][0]
    line[SECTIONS] = [["pipe", 0.4, 1]]
    line[SHAPE] = [[0, 0, 0], [0.4004 + 1e-12, 0, 0]]
    result = findings(model)
    assert result["arc_length"].status == "FAIL"
    assert result["sections"].status == "FAIL"


def test_small_resolvable_endpoint_excess_is_rejected(model):
    line = model["Lines"][0]
    line[CONNECTIONS][0] = ["Fixed", 0, 0, 0]
    line[SHAPE][0] = [0.01 + 1e-12, 0, 0]
    assert findings(model)["end_a"].status == "FAIL"


def test_coordinates_too_coarse_for_length_limit_are_not_accepted(model):
    """At 10**15, one binary64 coordinate step is .125 m."""
    line = model["Lines"][0]
    line[SECTIONS] = [["pipe", 0.4, 1]]
    line[SHAPE] = [[1e15, 0, 0], [1e15 + 0.5, 0, 0]]
    result = findings(model)
    assert result["arc_length"].status == "NOT_EVALUATED"
    assert result["sections"].status == "NOT_EVALUATED"
    line[SHAPE][-1][0] = 1e15 + 2
    result = findings(model)
    assert result["arc_length"].status == "FAIL"
    assert result["sections"].status == "FAIL"


def test_coordinates_too_coarse_for_endpoint_limit_are_not_accepted(model):
    """Coordinate uncertainty must never turn a resolvable excess into PASS."""
    line = model["Lines"][0]
    line[CONNECTIONS][0] = ["Fixed", 1e15, 0, 0]
    line[SHAPE][0] = [1e15 + 0.125, 0, 0]
    assert findings(model)["end_a"].status == "NOT_EVALUATED"
    line[SHAPE][0][0] = 1e15 + 2
    assert findings(model)["end_a"].status == "FAIL"


def test_wrong_line_table_cannot_hide_behind_other_line_geometry(model):
    """A substituted 6-8-10 segment pair totals 20, vs this line's 10.

    Relative error = |20-10|/10 = 1; end B error = sqrt(6**2+8**2)=10.
    """
    other = deepcopy(model["Lines"][0])
    other["Name"] = "other line"
    other[SHAPE] = [[0, 0, 0], [6, 8, 0], [12, 16, 0]]
    other[SECTIONS] = [["pipe", 10, 1], ["pipe", 10, 1]]
    other[CONNECTIONS][-1] = ["Fixed", 12, 16, 0]
    model["Lines"].append(other)
    model["Lines"][0][SHAPE] = deepcopy(other[SHAPE])
    results = validate_starting_shapes(model)
    wrong_line = next(f for f in results if f.line == "test line"
                      and f.check == "arc_length")
    assert wrong_line.actual == pytest.approx(1)
    assert wrong_line.status == "FAIL"
    assert next(f for f in results if f.line == "other line"
                and f.check == "arc_length").status == "PASS"


def test_section_mismatch_despite_correct_total_and_endpoints(model):
    """The first section expects 2, second 8; actual section chords are 5,5.

    Total 10 and end coordinates still match; per-section errors are 3/2
    and 3/8, so zero section size mismatches is not satisfied.
    """
    model["Lines"][0][SECTIONS] = [["pipe", 2, 1], ["pipe", 8, 1]]
    result = findings(model)
    assert result["arc_length"].status == "PASS"
    assert result["sections"].status == "FAIL"


def test_wrong_node_count(model):
    model["Lines"][0][SECTIONS][0][2] = 2
    assert findings(model)["sections"].status == "FAIL"


@pytest.mark.parametrize("connection", ["Anchored", "Free", "floating object"])
def test_unresolved_endpoint_frame(model, connection):
    model["Lines"][0][CONNECTIONS][0][0] = connection
    assert findings(model)["end_a"].status == "NOT_EVALUATED"
    assert findings(model)["end_b"].status == "PASS"


@pytest.mark.parametrize("parallel", [False, True])
@pytest.mark.parametrize("end,index", [("end_a", 0), ("end_b", -1)])
@pytest.mark.parametrize("relative_to", [None, "~", "Seabed", 0])
def test_fixed_endpoint_requires_global_vertical_reference(model, parallel, end, index, relative_to):
    line = model["Lines"][0]
    rows = [row + [None] for row in line.pop(CONNECTIONS)]
    rows[index][-1] = relative_to
    header = CONNECTIONS + ", ConnectionzRelativeTo"
    if parallel:
        for i, column in enumerate(header.split(", ")):
            line[column] = [row[i] for row in rows]
    else:
        line[header] = rows
    result = findings(model)
    expected = "PASS" if relative_to in (None, "~") else "NOT_EVALUATED"
    assert result[end].status == expected
    other = "end_b" if end == "end_a" else "end_a"
    assert result[other].status == "PASS"


def test_parallel_vertical_references_must_match_connection_row_count(model):
    line = model["Lines"][0]
    rows = line.pop(CONNECTIONS)
    for i, column in enumerate(CONNECTIONS.split(", ")):
        line[column] = [row[i] for row in rows]
    line["ConnectionzRelativeTo"] = [None]
    result = findings(model)
    assert result["end_a"].status == "FAIL"
    assert result["end_b"].status == "FAIL"


@pytest.mark.parametrize("units", [None, "US", "User"])
def test_missing_or_non_si_units_are_not_assumed(model, units):
    model["General"]["UnitsSystem"] = units
    result = findings(model)
    assert result["arc_length"].status == "NOT_EVALUATED"
    assert result["end_a"].status == "NOT_EVALUATED"


def test_target_segment_rounding_is_deferred(model):
    line = model["Lines"][0]
    del line[SECTIONS]
    line["LineType, Length, TargetSegmentLength"] = [["pipe", 10, 5]]
    result = findings(model)
    assert result["arc_length"].status == "PASS"
    assert result["sections"].status == "NOT_EVALUATED"


@pytest.mark.parametrize("dependency", ["BaseFile", "includefile"])
def test_unresolved_inheritance_does_not_report_missing_shape(model, dependency):
    model[dependency] = "base.yml"
    del model["Lines"][0][SHAPE]
    assert findings(model)["table"].status == "NOT_EVALUATED"


def test_non_user_specified_lines_are_ignored(model):
    model["Lines"][0]["StaticsStep1"] = "Catenary"
    assert validate_starting_shapes(model) == []


def test_parallel_columns_and_extra_orientation_columns(model):
    line = model["Lines"][0]
    rows = line.pop(SHAPE)
    for i, column in enumerate(SHAPE.split(", ")):
        line[column] = [row[i] for row in rows]
    assert findings(model)["table"].status == "PASS"
    for column in SHAPE.split(", "):
        del line[column]
    line[SHAPE + ", StartingShapeAzm"] = [row + [0] for row in rows]
    assert findings(model)["arc_length"].status == "PASS"


def test_yaml_validator_rejects_missing_and_mismatched_shapes(tmp_path, model):
    path = tmp_path / "model.yml"
    line = model["Lines"][0]
    del line[SHAPE]
    path.write_text(yaml.safe_dump(model))
    result = OrcaFlexYAMLValidator().validate_file(path)
    assert not result.valid
    assert any("Missing" in i.message for i in result.errors)
    assert result.starting_shapes
    line[SHAPE] = [[0, 0, 0], [6, 8, 0], [12, 16, 0]]
    path.write_text(yaml.safe_dump(model))
    assert not OrcaFlexYAMLValidator().validate_file(path).valid


def test_parallel_section_and_shape_columns_do_not_share_row_counts(model):
    line = model["Lines"][0]
    for key in (SECTIONS, SHAPE, CONNECTIONS):
        rows = line.pop(key)
        for i, column in enumerate(key.split(", ")):
            line[column] = [row[i] for row in rows]
    result = findings(model)
    assert result["table"].status == "PASS"
    assert result["sections"].status == "PASS"


@pytest.mark.parametrize("length", [0, -1, True, float("nan"), float("inf"), "5"])
def test_invalid_section_lengths_cannot_pass(model, length):
    model["Lines"][0][SECTIONS][0][1] = length
    assert findings(model)["sections"].status == "FAIL"


@pytest.mark.parametrize("count", [0, -1, True, 1.5, "1"])
def test_invalid_explicit_segment_counts_cannot_pass(model, count):
    model["Lines"][0][SECTIONS][0][2] = count
    assert findings(model)["sections"].status == "FAIL"


def test_missing_section_lengths_are_unevaluated(model):
    del model["Lines"][0][SECTIONS]
    assert findings(model)["arc_length"].status == "NOT_EVALUATED"


@pytest.mark.parametrize("length", [None, "~", 0, "calculated"])
def test_calculated_section_lengths_ignore_inactive_placeholders(model, length):
    line = model["Lines"][0]
    line["LengthAndEndOrientations"] = "Calculated from end positions"
    line[SECTIONS][0][1] = length
    result = findings(model)
    assert result["arc_length"].status == "NOT_EVALUATED"
    assert result["sections"].status == "NOT_EVALUATED"
    assert result["end_a"].status == "PASS"
    assert result["end_b"].status == "PASS"


def test_equal_geometry_cannot_certify_diameter_provenance(model):
    """Synthetic OD changes from .2 to .4; XYZ and chord lengths do not change.

    No distance equation can recover the original OD, so provenance must stay
    unevaluated even though both section chords still measure 5.
    """
    model["LineTypes"][0]["OD"] = 0.4
    result = findings(model)
    assert result["arc_length"].status == "PASS"
    assert result["dimensions"].status == "NOT_EVALUATED"


def test_duplicate_shape_encodings_fail(model):
    line = model["Lines"][0]
    for i, column in enumerate(SHAPE.split(", ")):
        line[column] = [row[i] for row in line[SHAPE]]
    assert findings(model)["table"].status == "FAIL"


def test_reordered_headers_use_column_names(model):
    line = model["Lines"][0]
    rows = line.pop(SHAPE)
    line["StartingShapeZ, StartingShapeX, StartingShapeY"] = [
        [z, x, y] for x, y, z in rows
    ]
    assert findings(model)["arc_length"].status == "PASS"


def test_directory_preserves_geometry_findings(tmp_path, model):
    (tmp_path / "model.yml").write_text(yaml.safe_dump(model))
    result = OrcaFlexYAMLValidator().validate_directory(tmp_path)
    assert result.valid
    assert len(result.starting_shapes) == len(validate_starting_shapes(model))
    assert any(f.status == "NOT_EVALUATED" for f in result.starting_shapes)
    assert any("NOT_EVALUATED" in i.message for i in result.warnings)


def test_malformed_fixed_coordinate_is_failure(model):
    model["Lines"][0][CONNECTIONS][0][1] = "unknown"
    assert findings(model)["end_a"].status == "FAIL"


def test_shape_distance_overflow_is_a_diagnostic(model):
    model["Lines"][0][SHAPE] = [[-1e308, 0, 0], [1e308, 0, 0]]
    assert findings(model)["arc_length"].status == "FAIL"


@pytest.mark.parametrize("violation", ["end_a", "end_b", "node_count", "section_size"])
def test_geometry_violations_invalidate_yaml_result(tmp_path, model, violation):
    """Distances .02 exceed .01 m; node count 4 differs from 3.

    The 2+8 section split still totals 10 but disagrees with the 5+5 chords.
    These are the same independent calculations exercised above, through the
    public file validator rather than only the geometry helper.
    """
    line = model["Lines"][0]
    if violation == "end_a":
        line[CONNECTIONS][0][3] = 0.02
    elif violation == "end_b":
        line[CONNECTIONS][-1][3] = 0.02
    elif violation == "node_count":
        line[SECTIONS][0][2] = 2
    else:
        line[SECTIONS] = [["pipe", 2, 1], ["pipe", 8, 1]]
    path = tmp_path / "model.yml"
    path.write_text(yaml.safe_dump(model))
    result = OrcaFlexYAMLValidator().validate_file(path)
    assert not result.valid
    assert any(f.status == "FAIL" for f in result.starting_shapes)


def test_three_dimensional_distance(model):
    """One 3-4-12 segment has sqrt(9+16+144)=13 length (OpenStax 2.2)."""
    line = model["Lines"][0]
    line[SHAPE] = [[0, 0, 0], [3, 4, 12]]
    line[SECTIONS] = [["pipe", 13, 1]]
    line[CONNECTIONS][-1] = ["Fixed", 3, 4, 12]
    result = findings(model)
    assert result["arc_length"].status == "PASS"
    assert result["end_b"].status == "PASS"


def test_target_length_takes_precedence_over_inactive_explicit_count(model):
    line = model["Lines"][0]
    rows = line.pop(SECTIONS)
    line[SECTIONS + ", TargetSegmentLength"] = [row + [5] for row in rows]
    # The explicit count is inactive when a target length is supplied.
    line[SECTIONS + ", TargetSegmentLength"][0][2] = 0
    assert findings(model)["sections"].status == "NOT_EVALUATED"
