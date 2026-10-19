# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for the unit-square bilinear map.

Formula basis: Coreform Cubit 2026.8, Quadrilateral Metrics; the open
Introduction to Solid Mechanics, Quadrilateral Elements, shape functions
(online edition accessed 2026-10-19). Derivations are recorded per fixture;
fixture dimensions and criteria are synthetic geometric inputs, not solver limits.
"""

from dataclasses import asdict
import json
import math

import numpy as np
import pytest

from digitalmodel.hydrodynamics.quad_mesh_quality import (
    QuadQualityCriteria,
    assess_quad_mesh,
)


def assess(points, *, criteria=None, tolerance=0.0):
    """Synthetic fixtures declare exact planarity unless stated otherwise."""
    return assess_quad_mesh(
        np.asarray(points, dtype=float),
        np.array([[0, 1, 2, 3]]),
        planarity_tolerance=tolerance,
        criteria=criteria,
    )


RECTANGLE = [[0, 0, 0], [2, 0, 0], [2, 1, 0], [0, 1, 0]]


def test_rectangle_metrics_and_missing_limits():
    """x=2u, y=v gives J=2; edges 2,1,2,1 and all dot products zero."""
    report = assess(RECTANGLE)
    panel = report.panels[0]
    assert panel.edge_ratio == pytest.approx(2)
    assert panel.corner_angles_deg == pytest.approx([90] * 4)
    assert panel.convex is True
    assert panel.min_projected_jacobian == pytest.approx(2)
    assert panel.max_plane_distance == 0
    for key in ("convexity", "mapping_jacobian", "planarity"):
        assert panel.checks[key].status == "PASS"
    for key in ("edge_ratio", "corner_angles"):
        assert panel.checks[key].status == "NOT_EVALUATED"
    assert report.status == "NOT_EVALUATED"
    assert report.solver_qualification.status == "NOT_EVALUATED"


def test_explicit_geometric_limits_and_serialization():
    """Rectangle values above lie exactly on these synthetic inclusive bounds."""
    criteria = QuadQualityCriteria(
        basis="Synthetic rectangle hand calculation in this test",
        max_edge_ratio=2,
        min_angle_deg=90,
        max_angle_deg=90,
        min_jacobian=2,
    )
    report = assess(RECTANGLE, criteria=criteria)
    assert report.status == "PASS"
    assert all(check.status == "PASS" for check in report.panels[0].checks.values())
    assert report.solver_qualification.status == "NOT_EVALUATED"
    assert json.loads(json.dumps(asdict(report), allow_nan=False))["criteria"]["basis"]


@pytest.mark.parametrize(
    "limits, failed",
    [
        ({"max_edge_ratio": 1.5}, "edge_ratio"),
        ({"min_angle_deg": 91}, "corner_angles"),
        ({"max_angle_deg": 89}, "corner_angles"),
        ({"min_jacobian": 3}, "mapping_jacobian"),
    ],
)
def test_limits_are_independent(limits, failed):
    """Synthetic bounds deliberately exclude one rectangle metric at a time."""
    panel = assess(
        RECTANGLE, criteria=QuadQualityCriteria(basis="Synthetic exclusion", **limits)
    ).panels[0]
    assert panel.checks[failed].status == "FAIL"
    assert panel.status == "FAIL"
    assert panel.checks["convexity"].status == "PASS"


def test_equal_edges_do_not_hide_angular_skew():
    """Rhombus edges (1,0), (3/5,4/5): length 1, J=4/5.

    Dot product 3/5 gives angle acos(3/5); opposite angles supplement it.
    Thus edge ratio 1 passes while a synthetic 60-degree minimum fails.
    """
    panel = assess(
        [[0, 0, 0], [1, 0, 0], [1.6, 0.8, 0], [0.6, 0.8, 0]],
        tolerance=0,
        criteria=QuadQualityCriteria(
            basis="Synthetic rhombus hand calculation", max_edge_ratio=1.1,
            min_angle_deg=60,
        ),
    ).panels[0]
    acute = math.degrees(math.acos(3 / 5))
    assert panel.edge_ratio == pytest.approx(1)
    assert panel.corner_angles_deg == pytest.approx([acute, 180 - acute] * 2)
    assert panel.min_projected_jacobian == pytest.approx(4 / 5)
    assert panel.checks["edge_ratio"].status == "PASS"
    assert panel.checks["corner_angles"].status == "FAIL"


def test_concave_quad_has_reflex_angle_and_negative_jacobian():
    """p2=(1/2,1/2): x=2u-3uv/2, y=2v-3uv/2.

    J=4-3u-3v has corner minimum -2. Edges at p2 have dot -3/2,
    lengths sqrt(5/2), giving reflex angle 360-acos(-3/5).
    """
    panel = assess([[0, 0, 0], [2, 0, 0], [0.5, 0.5, 0], [0, 2, 0]]).panels[0]
    assert panel.min_projected_jacobian == pytest.approx(-2)
    assert panel.corner_angles_deg[2] == pytest.approx(
        360 - math.degrees(math.acos(-3 / 5))
    )
    assert panel.convex is False
    assert panel.checks["convexity"].status == "FAIL"
    assert panel.checks["mapping_jacobian"].status == "FAIL"


@pytest.mark.parametrize(
    "points",
    [
        [[0, 0, 0], [1, 1, 0], [0, 1, 0], [1, 0, 0]],
        [[0, 0, 0], [1, 0, 0], [2, 0, 0], [3, 0, 0]],
        [[0, 0, 0]] * 4,
    ],
)
def test_folded_flat_and_collapsed_maps_fail(points):
    """Bow tie x=u+v-2uv,y=u has J=2u-1, zero at centre.

    Collinear points and coincident points have zero cross products everywhere.
    None can define a nonsingular, consistently signed planar map.
    """
    report = assess(points)
    assert report.status == "FAIL"
    assert report.panels[0].convex is False
    assert report.panels[0].checks["mapping_jacobian"].status == "FAIL"
    json.dumps(asdict(report), allow_nan=False)


def test_flat_triangle_encoded_with_four_distinct_nodes_fails():
    """p0,p1,p2 collinear: x=2u+2uv, y=v-uv, J=2-2u+2v.

    Minimum is zero at (1,0), although the polygon encloses nonzero area.
    """
    panel = assess(
        [[0, 0, 0], [2, 0, 0], [4, 0, 0], [0, 1, 0]],
        criteria=QuadQualityCriteria(basis="Synthetic zero bound", min_jacobian=0),
    ).panels[0]
    assert panel.min_projected_jacobian == pytest.approx(0)
    assert panel.corner_angles_deg[1] == pytest.approx(180)
    assert panel.convex is False
    assert panel.checks["mapping_jacobian"].status == "FAIL"


def test_warped_quad_is_not_qualified_by_projection():
    """x=u,y=v,z=uv: centre normal (-1,-1,2)/sqrt(6).

    Relative to the plane through p0, only p1,p3 have nonzero distance,
    1/sqrt(6); projected J=(u+v+2)/sqrt(6), minimum 2/sqrt(6).
    The positive projected minimum alone does not qualify the surface map.
    """
    report = assess([[0, 0, 0], [1, 0, 0], [1, 1, 1], [0, 1, 0]])
    panel = report.panels[0]
    assert panel.max_plane_distance == pytest.approx(1 / math.sqrt(6))
    assert panel.min_projected_jacobian == pytest.approx(2 / math.sqrt(6))
    assert panel.convex is None
    assert panel.corner_angles_deg is None
    assert panel.checks["planarity"].status == "FAIL"
    for key in ("convexity", "mapping_jacobian", "corner_angles"):
        assert panel.checks[key].status == "NOT_EVALUATED"
    assert report.status == "FAIL"
    assert report.solver_qualification.status == "NOT_EVALUATED"


@pytest.mark.parametrize("reverse", [False, True])
def test_rigid_transform_scale_and_winding(reverse):
    """Rotate rectangle into yz plane and scale by 3: J=2*3^2=18.

    Translation has no effect; reversing winding reverses the local reference
    normal too, preserving geometric validity without certifying outward normals.
    """
    points = np.asarray(RECTANGLE, dtype=float)[:, [2, 0, 1]] * 3 + [8, -7, 9]
    if reverse:
        points = points[::-1]
    panel = assess(points).panels[0]
    assert panel.edge_ratio == pytest.approx(2)
    assert panel.corner_angles_deg == pytest.approx([90] * 4)
    assert panel.min_projected_jacobian == pytest.approx(18)
    assert panel.convex is True


def test_mesh_report_keeps_panel_indices_and_failures():
    """Two translated rectangles followed by the folded bow tie above."""
    points = np.concatenate([
        np.asarray(RECTANGLE), np.asarray(RECTANGLE) + [3, 0, 0],
        [[0, 0, 0], [1, 1, 0], [0, 1, 0], [1, 0, 0]],
    ])
    report = assess_quad_mesh(
        points, np.arange(12).reshape(3, 4), planarity_tolerance=0,
    )
    assert [panel.panel_index for panel in report.panels] == [0, 1, 2]
    assert [panel.status for panel in report.panels] == [
        "NOT_EVALUATED", "NOT_EVALUATED", "FAIL",
    ]
    assert report.status == "FAIL"


def test_empty_mesh_is_not_evaluated():
    """No panels provide no evidence of geometric or solver validity."""
    report = assess_quad_mesh(np.empty((0, 3)), np.empty((0, 4), dtype=int),
                              planarity_tolerance=0)
    assert report.panels == ()
    assert report.status == "NOT_EVALUATED"


@pytest.mark.parametrize("tolerance", [-1, math.inf, math.nan])
def test_invalid_planarity_tolerance(tolerance):
    with pytest.raises(ValueError, match="planarity_tolerance"):
        assess(RECTANGLE, tolerance=tolerance)


@pytest.mark.parametrize(
    "nodes, panels",
    [
        ([[0, 0]], [[0, 1, 2, 3]]),
        (RECTANGLE, [[0, 1, 2]]),
        (RECTANGLE, [[0, 1, 2, -1]]),
        (RECTANGLE, [[0, 1, 2, 4]]),
        (RECTANGLE, [[0.0, 1.0, 2.0, 3.0]]),
        (RECTANGLE, [[0, 1, 2, 2]]),
        ([[math.nan, 0, 0]] * 4, [[0, 1, 2, 3]]),
    ],
)
def test_invalid_mesh_inputs(nodes, panels):
    with pytest.raises(ValueError):
        assess_quad_mesh(nodes, panels, planarity_tolerance=0)


@pytest.mark.parametrize(
    "kwargs",
    [
        {"basis": ""}, {"basis": "   "},
        {"max_edge_ratio": 0.5}, {"max_edge_ratio": math.inf},
        {"min_angle_deg": -1}, {"max_angle_deg": 361},
        {"min_angle_deg": 100, "max_angle_deg": 90},
        {"min_jacobian": -1}, {"min_jacobian": math.nan},
    ],
)
def test_invalid_criteria(kwargs):
    with pytest.raises(ValueError):
        QuadQualityCriteria(**({"basis": "Synthetic input"} | kwargs))


def test_trapezoid_minimum_is_not_the_centre_value():
    """x=3u-uv,y=v gives J=3-v: corner minimum 2, centre value 5/2.

    Edges have lengths 3,sqrt(2),2,1; the slanted edge makes angles 45,135
    degrees at p1,p2, with right angles at the other two corners.
    """
    panel = assess([[0, 0, 0], [3, 0, 0], [2, 1, 0], [0, 1, 0]]).panels[0]
    assert panel.edge_ratio == pytest.approx(3)
    assert panel.corner_angles_deg == pytest.approx([90, 45, 135, 90])
    assert panel.min_projected_jacobian == pytest.approx(2)
    assert panel.convex is True


def test_fold_with_nonsingular_centre_still_fails_at_corner():
    """x=2u+v-3uv,y=v gives J=2-3v, centre 1/2 but minimum -1."""
    panel = assess([[0, 0, 0], [2, 0, 0], [0, 1, 0], [1, 1, 0]]).panels[0]
    assert panel.min_projected_jacobian == pytest.approx(-1)
    assert panel.convex is False
    assert panel.checks["mapping_jacobian"].status == "FAIL"


def test_zero_edge_with_nonsingular_centre_is_reported():
    """x=uv,y=v gives J=v, centre 1/2 but zero on the collapsed edge."""
    report = assess([[0, 0, 0], [0, 0, 0], [1, 1, 0], [0, 1, 0]])
    panel = report.panels[0]
    assert panel.edge_ratio is None
    assert panel.corner_angles_deg is None
    assert panel.min_projected_jacobian == 0
    assert panel.checks["edge_ratio"].status == "FAIL"
    assert panel.checks["corner_angles"].status == "FAIL"
    json.dumps(asdict(report), allow_nan=False)


def test_planarity_tolerance_is_explicit_and_inclusive():
    """For x=u,y=v,z=uv the distance is 1/sqrt(6), as derived above.

    A declared distance of 1 permits its planar projection; this explicitly
    relaxed geometric criterion still supplies no surface or solver eligibility.
    """
    report = assess([[0, 0, 0], [1, 0, 0], [1, 1, 1], [0, 1, 0]], tolerance=1)
    assert report.panels[0].checks["planarity"].status == "PASS"
    assert report.panels[0].convex is True
    assert report.solver_qualification.status == "NOT_EVALUATED"
    distance = report.panels[0].max_plane_distance
    assert assess([[0, 0, 0], [1, 0, 0], [1, 1, 1], [0, 1, 0]],
                  tolerance=distance).panels[0].checks["planarity"].status == "PASS"


@pytest.mark.parametrize("scale", [1e-100, 1e100])
def test_representable_scale_extremes(scale):
    """Rectangle J scales quadratically: 2*s^2; angles and ratio are invariant."""
    panel = assess(np.asarray(RECTANGLE) * scale).panels[0]
    assert panel.min_projected_jacobian == pytest.approx(2 * scale * scale, abs=0)
    assert panel.edge_ratio == pytest.approx(2)
    assert panel.convex is True


@pytest.mark.parametrize("scale", [1e-200, 1e200])
def test_unrepresentable_jacobian_is_explicit(scale):
    """J=2*s^2 lies outside finite nonzero binary64 range for these inputs.

    An undefined numeric result must raise, rather than classify a valid
    rectangle as a collapsed map or emit a nonfinite JSON diagnostic.
    """
    with pytest.raises(ValueError, match="floating-point range"):
        assess(np.asarray(RECTANGLE) * scale)
