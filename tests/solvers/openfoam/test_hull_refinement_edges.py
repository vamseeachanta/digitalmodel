"""Independent geometry examples for horizontal refinement transitions.

Basis: OpenFOAM Foundation, OpenFOAM 13 (2025),
meshCheck::faceOrthogonality and boxToCell::combine. The former defines the
angle using the face normal and neighbouring cell centres; the latter selects
cell centres. Synthetic dimensions below are hand-calculation inputs, not
recommended mesh sizes or acceptance limits.
"""

import math

import pytest

from digitalmodel.solvers.openfoam.hull_refinement_edges import (
    RefinementZEdgePolicy,
    place_refinement_z_edges,
    xy_transition_angle_deg,
)


def test_rectangular_face_with_unequal_vertical_neighbours():
    """A 12 by 16 face has sub-cell offsets 3 and 4: lateral distance 5.

    Heights 2 and 8 put centres (2 + 8)/2 = 5 apart vertically.
    tan(theta) = 5/5 = 1, hence the isosceles right-triangle angle is 45 deg.
    """
    assert xy_transition_angle_deg(12, 16, 2, 8) == pytest.approx(45)


def test_thin_cells_increase_angle():
    """Offsets 3, 4 give distance 5; dz=1 gives theta=atan(5).

    The oracle uses acos of the independently constructed vector (3,4,1)
    against the unit vertical normal, as in the cited OpenFOAM definition.
    """
    expected = math.degrees(math.acos(1 / math.sqrt(26)))
    assert xy_transition_angle_deg(12, 16, 1, 1) == pytest.approx(expected)
    assert expected > 45  # Compared with the preceding hand calculation.


@pytest.mark.parametrize("bad", [0, -1, math.inf, -math.inf, math.nan])
@pytest.mark.parametrize("index", range(4))
def test_angle_requires_finite_positive_cell_dimensions(bad, index):
    """Zero, negative and non-finite lengths do not define these cells."""
    dimensions = [12, 16, 2, 8]
    dimensions[index] = bad
    with pytest.raises(ValueError, match="finite and positive"):
        xy_transition_angle_deg(*dimensions)


def test_edges_move_to_nearest_safe_actual_cell_faces():
    """dx=dy=4 gives offset sqrt(2). With a caller limit of 45 deg,

    faces at +/-1 have dz=1 and fail (offset > dz). At +/-2 the adjacent
    heights are 1 and 2, so dz=1.5 > sqrt(2) and they pass. Requested edges
    +/-0.5 therefore move outward to +/-2, not to a nominal block boundary.
    """
    policy = RefinementZEdgePolicy(
        z_faces=(-10, -4, -2, -1, 0, 1, 2, 4, 10),
        background_dx=4, background_dy=4, max_angle_deg=45,
    )
    original = [((-2, -2, -0.5), (2, 0, 0.5))]
    assert place_refinement_z_edges(original, policy) == [
        ((-2, -2, -2), (2, 0, 2))
    ]
    assert original[0][0][2] == -0.5


@pytest.mark.parametrize("limit", [45, 80])
def test_nested_stages_leave_a_parent_cell_beyond_each_inner_edge(limit):
    """Inner parent dx=dy=2 gives offset sqrt(0.5), safe at +/-1.

    Outer parent dx=dy=4 needs +/-2 as above. Even with a looser limit,
    the outer edge must include one further row so a z-face never jumps
    several refinement levels at once. All requested z extents are +/-0.5.
    """
    boxes = [((-4, -4, -0.5), (4, 0, 0.5)),
             ((-2, -2, -0.5), (2, 0, 0.5))]
    policy = RefinementZEdgePolicy(
        z_faces=(-10, -4, -2, -1, 0, 1, 2, 4, 10),
        background_dx=4, background_dy=4, max_angle_deg=limit,
    )
    result = place_refinement_z_edges(boxes, policy)
    assert [lo[2] for lo, _ in result] == [-2, -1]
    assert [hi[2] for _, hi in result] == [2, 1]


def test_domain_walls_have_no_internal_z_transition():
    """Faces -1,0,1 bound two cells: a box straddling 0 selects both.

    There is no internal coarse neighbour at either domain wall. This
    remains true for all nested stages, irrespective of the supplied limit.
    """
    policy = RefinementZEdgePolicy(
        z_faces=(-1, 0, 1), background_dx=12, background_dy=16,
        max_angle_deg=1,
    )
    boxes = [((-2, -2, -0.5), (2, 0, 0.5))] * 2
    assert all((lo[2], hi[2]) == (-1, 1)
               for lo, hi in place_refinement_z_edges(boxes, policy))


def test_exact_limit_is_accepted():
    """dx=12,dy=16, dz=5 gives exactly 45 degrees (3-4-5 triangle).

    Requested edges already lie at the two safe internal faces +/-2.5.
    """
    policy = RefinementZEdgePolicy(
        z_faces=(-7.5, -2.5, 2.5, 7.5),
        background_dx=12, background_dy=16, max_angle_deg=45,
    )
    boxes = [((-1, -1, -2.5), (1, 0, 2.5))]
    assert place_refinement_z_edges(boxes, policy) == boxes


@pytest.mark.parametrize("faces", [(), (0,), (0, 0), (1, 0), (0, math.nan),
                                   (0, math.inf)])
def test_invalid_cell_face_ladder_is_rejected(faces):
    """A cell ladder needs finite, strictly ascending bounding faces."""
    with pytest.raises(ValueError, match="z_faces"):
        RefinementZEdgePolicy(faces, 4, 4, 45)


@pytest.mark.parametrize("limit", [0, -1, 90, math.nan, math.inf])
def test_invalid_angle_limit_is_rejected(limit):
    """A positive centre separation gives an angle in [0,90) degrees."""
    with pytest.raises(ValueError, match="max_angle_deg"):
        RefinementZEdgePolicy((-1, 0, 1), 4, 4, limit)


@pytest.mark.parametrize("dx,dy", [(0, 1), (1, -1), (math.nan, 1), (1, math.inf)])
def test_invalid_background_cell_sizes_are_rejected(dx, dy):
    """Background lengths must define finite non-degenerate cells."""
    with pytest.raises(ValueError, match="finite and positive"):
        RefinementZEdgePolicy((-1, 0, 1), dx, dy, 45)


def test_out_of_domain_box_is_rejected():
    """A requested lower edge -2 cannot be covered by a ladder starting -1."""
    policy = RefinementZEdgePolicy((-1, 0, 1), 4, 4, 45)
    with pytest.raises(ValueError, match="inside"):
        place_refinement_z_edges([((-1, -1, -2), (1, 0, 0.5))], policy)


def test_non_nested_boxes_are_rejected():
    """An inner x edge at 2 extends past the outer edge at 1."""
    policy = RefinementZEdgePolicy((-1, 0, 1), 4, 4, 45)
    with pytest.raises(ValueError, match="nested"):
        place_refinement_z_edges([
            ((-1, -1, -0.5), (1, 0, 0.5)),
            ((-1, -1, -0.5), (2, 0, 0.5)),
        ], policy)


def test_empty_staging_is_empty():
    """No refinement boxes create no transitions."""
    policy = RefinementZEdgePolicy((-1, 0, 1), 4, 4, 45)
    assert place_refinement_z_edges([], policy) == []


def test_hull_domain_applies_explicit_policy(manifest_dict):
    """The shared synthetic hull uses Lpp=6 and draft=0.3 metres.

    Supply 1-metre background cells and z faces at the two domain walls:
    every stage must reach those walls, which have no internal transition.
    This isolates builder wiring from the angle and face-search calculations
    already checked above. All x-y coordinates must remain those of the
    existing hull-box interpolation.
    """
    from digitalmodel.solvers.openfoam.hull_domain import (
        build_hull_domain,
        refinement_boxes,
    )
    from digitalmodel.solvers.openfoam.hull_manifest import HullManifest

    manifest = HullManifest.from_dict(manifest_dict)
    domain = build_hull_domain(manifest, base_cell_size=1)
    policy = RefinementZEdgePolicy(
        (domain.z_levels[0], domain.z_levels[-1]), 1, 1, 45,
    )
    original = refinement_boxes(manifest, domain)
    adjusted = refinement_boxes(manifest, domain, z_edge_policy=policy)
    assert len(adjusted) == len(original)
    for (lo, hi), (old_lo, old_hi) in zip(adjusted, original):
        assert lo[:2] == old_lo[:2]
        assert hi[:2] == old_hi[:2]
        assert (lo[2], hi[2]) == (domain.z_levels[0], domain.z_levels[-1])


def test_hull_domain_rejects_policy_for_a_different_vertical_domain(manifest_dict):
    """A ladder bounded by +/-1 cannot represent this hull's full domain."""
    from digitalmodel.solvers.openfoam.hull_domain import (
        HullDomainError,
        build_hull_domain,
        refinement_boxes,
    )
    from digitalmodel.solvers.openfoam.hull_manifest import HullManifest

    manifest = HullManifest.from_dict(manifest_dict)
    domain = build_hull_domain(manifest)
    policy = RefinementZEdgePolicy((-1, 0, 1), 4, 4, 45)
    with pytest.raises(HullDomainError, match="domain walls"):
        refinement_boxes(manifest, domain, z_edge_policy=policy)
