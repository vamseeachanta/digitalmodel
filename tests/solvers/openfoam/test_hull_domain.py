"""
ABOUTME: Domain, refinement-box and background-division derivation for an
arbitrary hull (#2023). Everything here must come out of Lpp/beam/draft; a
constant that survives from the KCS/DTC port is a scale bomb.

The KCS resistance path sizes its domain by scaling a FIXED DTC box by an LOA
ratio. That is correct for KCS and wrong for anything else, because the ratio
carries DTC's proportions -- its beam/length, its draft/length -- into a hull
that does not have them. These tests pin the replacement: proportions read off
the hull in front of us, arithmetic delegated to ``DomainBuilder``.
"""

from __future__ import annotations

import pytest

from digitalmodel.solvers.openfoam.hull_domain import (
    BACKGROUND_CELLS_PER_LPP,
    DEPTH_LPP,
    DOWNSTREAM_LPP,
    FREEBOARD_LPP,
    LATERAL_LPP,
    UPSTREAM_LPP,
    HullDomainError,
    block_divisions,
    build_hull_domain,
    refinement_boxes,
)
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest

from .conftest import scaled_manifest_dict


@pytest.fixture
def manifest(manifest_dict) -> HullManifest:
    return HullManifest.from_dict(manifest_dict)


@pytest.fixture
def domain(manifest: HullManifest):
    return build_hull_domain(manifest)


# --------------------------------------------------------------------------- #
#  Extents
# --------------------------------------------------------------------------- #

def test_extents_are_multiples_of_lpp_not_of_a_ported_box(manifest, domain) -> None:
    lpp = manifest.lpp_m
    assert domain.x_inlet == pytest.approx(UPSTREAM_LPP * lpp)
    assert domain.x_outlet == pytest.approx(-DOWNSTREAM_LPP * lpp)
    assert domain.y_side == pytest.approx(-LATERAL_LPP * lpp)
    assert domain.z_levels[0] == pytest.approx(-DEPTH_LPP * lpp)
    assert domain.z_levels[-1] == pytest.approx(FREEBOARD_LPP * lpp)


def test_domain_comes_from_domainbuilder_from_hull_dims(manifest, domain) -> None:
    """The box arithmetic is delegated, not reimplemented.

    ``DomainBuilder.from_hull_dims`` already exists and is already wired into
    ``openfoam setup``. This lane wires it into the resistance path too, so the
    emitted DomainConfig must be exactly what it returns.
    """
    from digitalmodel.solvers.openfoam.domain_builder import DomainBuilder

    expected = DomainBuilder.from_hull_dims(
        length=manifest.lpp_m,
        beam=manifest.beam_m,
        draft=manifest.draft_m,
        upstream_factor=UPSTREAM_LPP,
        downstream_factor=DOWNSTREAM_LPP,
        lateral_factor=LATERAL_LPP * manifest.lpp_m / manifest.beam_m,
        depth_factor=DEPTH_LPP * manifest.lpp_m / manifest.draft_m,
        freeboard_factor=FREEBOARD_LPP,
        base_cell_size=domain.base_cell_size,
    )
    assert domain.config.min_coords == pytest.approx(expected.min_coords)
    assert domain.config.max_coords == pytest.approx(expected.max_coords)


def test_x_is_mirrored_so_the_inlet_faces_the_bow(manifest, domain) -> None:
    """Two conventions meet here and the mismatch is silent if unhandled.

    ``DomainBuilder`` puts the flow in +x with the bow upstream at -x. The
    template inherited from the DTC tutorial puts the inlet at +x with an
    internal field of -Umean, so the flow runs in -x and the bow must be at +x.
    The manifest states x = forward, so the hull's bow is already at +x and the
    DOMAIN is what gets mirrored. Get this wrong and the hull is towed
    stern-first: it solves, it converges, and it answers a different question.
    """
    assert domain.x_inlet > 0 > domain.x_outlet
    assert domain.x_inlet == pytest.approx(-domain.config.min_coords[0])
    assert domain.x_outlet == pytest.approx(-domain.config.max_coords[0])
    assert domain.flow_direction == (-1.0, 0.0, 0.0)


def test_half_domain_keeps_the_negative_y_side(domain) -> None:
    """The template's midPlane symmetry sits at y = 0."""
    assert domain.y_side < 0
    assert domain.y_centreplane == 0.0
    assert domain.half_domain is True


def test_waterline_is_the_draft_because_the_keel_is_the_origin(manifest, domain):
    assert domain.waterline == pytest.approx(manifest.draft_m)
    assert domain.z_levels[3] == pytest.approx(manifest.draft_m)


def test_z_levels_are_strictly_increasing_and_straddle_the_waterline(domain) -> None:
    levels = domain.z_levels
    assert len(levels) == 7, "blockMeshDict has six stacked blocks"
    assert all(a < b for a, b in zip(levels, levels[1:])), levels
    assert levels[2] < domain.waterline < levels[4]


def test_location_in_mesh_is_inside_the_domain_and_outside_the_hull(
    manifest, domain
) -> None:
    x, y, z = domain.location_in_mesh
    assert domain.x_outlet < x < domain.x_inlet
    assert domain.y_side < y < 0.0
    assert domain.z_levels[0] < z < domain.z_levels[-1]
    # clear of the placed hull in x, so snappy keeps the outside region
    assert x < manifest.bbox_min_m[0] - manifest.midship_x_m


def test_a_hull_whose_draft_exceeds_the_domain_is_refused(manifest_dict) -> None:
    """Fail closed rather than emit a blockMeshDict with inverted blocks."""
    manifest_dict["draft_m"] = 0.30
    m = HullManifest.from_dict(manifest_dict)
    with pytest.raises(HullDomainError):
        build_hull_domain(m, keel_clearance_drafts=1.0e4)


# --------------------------------------------------------------------------- #
#  Refinement boxes
# --------------------------------------------------------------------------- #

def test_six_refinement_boxes_matching_the_six_toposet_stages(manifest, domain):
    boxes = refinement_boxes(manifest, domain)
    assert len(boxes) == 6, "the template ships topoSetDict.1 .. topoSetDict.6"


def test_refinement_boxes_are_nested(manifest, domain) -> None:
    """Each stage refines inside the previous one.

    A box that escapes its parent refines cells the previous stage never
    touched, which produces a 4:1 face jump the mesher will either smear over
    with buffer layers or fail on.
    """
    boxes = refinement_boxes(manifest, domain)
    for outer, inner in zip(boxes, boxes[1:]):
        (ox0, oy0, oz0), (ox1, oy1, oz1) = outer
        (ix0, iy0, iz0), (ix1, iy1, iz1) = inner
        assert ox0 <= ix0 and ix1 <= ox1
        assert oy0 <= iy0 and iy1 <= oy1
        assert oz0 <= iz0 and iz1 <= oz1


def test_innermost_box_contains_the_placed_hull(manifest, domain) -> None:
    lo, hi = refinement_boxes(manifest, domain)[-1]
    hx0 = manifest.bbox_min_m[0] - manifest.midship_x_m
    hx1 = manifest.bbox_max_m[0] - manifest.midship_x_m
    assert lo[0] < hx0 and hx1 < hi[0]
    assert lo[1] < manifest.bbox_min_m[1]
    assert lo[2] < manifest.bbox_min_m[2]
    assert hi[2] > manifest.bbox_max_m[2]


def test_every_box_straddles_the_waterline(manifest, domain) -> None:
    """The free surface is the feature the refinement exists to resolve."""
    for lo, hi in refinement_boxes(manifest, domain):
        assert lo[2] < domain.waterline < hi[2]


def test_every_box_lies_inside_the_domain(manifest, domain) -> None:
    for lo, hi in refinement_boxes(manifest, domain):
        assert domain.x_outlet <= lo[0] and hi[0] <= domain.x_inlet
        assert domain.y_side <= lo[1] and hi[1] <= 0.0
        assert domain.z_levels[0] <= lo[2] and hi[2] <= domain.z_levels[-1]


def test_boxes_stop_at_the_centreplane(manifest, domain) -> None:
    for _lo, hi in refinement_boxes(manifest, domain):
        assert hi[1] == pytest.approx(0.0)


def test_a_beamy_hull_gets_a_wider_inner_box(manifest_dict) -> None:
    """Containment is read off the hull, not assumed from a ship-like B/L."""
    manifest_dict["beam_m"] = 2.4
    manifest_dict["bbox_min_m"] = [-0.05, -1.2, 0.0]
    manifest_dict["bbox_max_m"] = [6.15, 1.2, 0.55]
    manifest_dict["displacement_m3"] = 1.9
    barge = HullManifest.from_dict(manifest_dict)
    lo, _hi = refinement_boxes(barge, build_hull_domain(barge))[-1]
    assert lo[1] < -1.2


# --------------------------------------------------------------------------- #
#  Background divisions
# --------------------------------------------------------------------------- #

def test_background_divisions_follow_the_base_cell_size(manifest, domain) -> None:
    div = block_divisions(domain)
    assert div["nx"] == pytest.approx(
        round(domain.length / domain.base_cell_size), abs=1
    )
    assert div["ny"] == pytest.approx(
        round(domain.width / domain.base_cell_size), abs=1
    )
    for key in ("nx", "ny", "nza", "nzb", "nzc", "nzd"):
        assert div[key] >= 1


def test_default_background_resolution_is_stated_not_inherited(manifest, domain):
    assert domain.base_cell_size == pytest.approx(
        manifest.lpp_m / BACKGROUND_CELLS_PER_LPP
    )


def test_free_surface_band_gets_the_finest_vertical_cells(manifest, domain) -> None:
    div = block_divisions(domain)
    z = domain.z_levels
    dz_band = min(z[3] - z[2], z[4] - z[3]) / div["nzb"]
    dz_outer = (z[5] - z[4]) / div["nzc"]
    dz_far = (z[6] - z[5]) / div["nzd"]
    assert dz_band < dz_outer < dz_far


# --------------------------------------------------------------------------- #
#  Scale sanity -- a scale-invariance bug is invisible on one hull
# --------------------------------------------------------------------------- #

def test_a_ten_times_larger_hull_gets_a_ten_times_larger_domain(manifest_dict):
    small = HullManifest.from_dict(manifest_dict)
    big = HullManifest.from_dict(scaled_manifest_dict(10.0))
    ds, db = build_hull_domain(small), build_hull_domain(big)

    assert db.length == pytest.approx(ds.length * 10.0)
    assert db.width == pytest.approx(ds.width * 10.0)
    assert db.height == pytest.approx(ds.height * 10.0)
    assert db.volume == pytest.approx(ds.volume * 1000.0)
    for a, b in zip(ds.z_levels, db.z_levels):
        assert b == pytest.approx(a * 10.0)


def test_a_ten_times_larger_hull_gets_ten_times_larger_boxes(manifest_dict) -> None:
    small = HullManifest.from_dict(manifest_dict)
    big = HullManifest.from_dict(scaled_manifest_dict(10.0))
    bs = refinement_boxes(small, build_hull_domain(small))
    bb = refinement_boxes(big, build_hull_domain(big))
    for (slo, shi), (blo, bhi) in zip(bs, bb):
        assert blo == pytest.approx([c * 10.0 for c in slo])
        assert bhi == pytest.approx([c * 10.0 for c in shi])


def test_lpp_proportional_cells_keep_the_division_counts_invariant(manifest_dict):
    """With the default cell size the MESH DESIGN is scale-free.

    This is the property that makes the snappy settings scale-invariant, and it
    is worth pinning separately from the domain test: if the divisions moved
    with the hull, the two tests would still both pass while the case silently
    changed resolution class.
    """
    small = HullManifest.from_dict(manifest_dict)
    big = HullManifest.from_dict(scaled_manifest_dict(10.0))
    assert block_divisions(build_hull_domain(small)) == block_divisions(
        build_hull_domain(big)
    )
