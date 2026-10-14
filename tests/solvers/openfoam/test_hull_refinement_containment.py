#!/usr/bin/env python3
"""
ABOUTME: The refinement staging must contain the hull the case is ACTUALLY
built from (#2033).

WHAT WENT WRONG. ``refinement_boxes`` interpolated every box as a symmetric
interval about x = 0 and built its innermost box from the hull's bounding box
TRANSLATED by the bounding-box centre, on the strength of a docstring claiming
"the hull is translated by -this". Nothing translates the hull: the surfaces
are copied into ``constant/triSurface`` byte for byte, so the case's x is the
manifest's x, and the manifest's origin is the aft perpendicular. The staging
was therefore centred on the aft perpendicular instead of on the hull: levels
were spent on open water astern and the finest box stopped short of the bow.

On a hull measured 0.021 Lpp aft of the AP to 1.002 Lpp forward, the finest
box reached 0.571 Lpp -- 42 % of the hull was never refined, and the bow was
meshed at 16x the intended cell. The hull carried ``level (0 0)``, so nothing
else put cells there either. The mesh converged, the solve converged, and
0.7 % of the hull patch carried 122 % of the net pressure drag.

Every test here is a property of the PLACED geometry, never of an assumed
origin convention. A test written in the translated frame is what let the
defect through: the suite asserted containment of a hull that does not exist.
"""

from __future__ import annotations

import pytest

from digitalmodel.solvers.openfoam.hull_case_regions import (
    HULL_REFINEMENT_LEVEL,
    hull_region,
)
from digitalmodel.solvers.openfoam.hull_domain import (
    build_hull_domain,
    refinement_boxes,
)
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest
from digitalmodel.solvers.openfoam.hull_placement import (
    HullPlacementError,
    assert_boxes_contain_hull,
    hull_x_centre,
    meshed_hull_bbox,
)


@pytest.fixture
def manifest(manifest_dict) -> HullManifest:
    """The shared synthetic hull. Its bbox runs -0.05 .. 6.15 m against an
    Lpp of 6 m: OFF-ORIGIN, keel-up, aft perpendicular at x = 0, exactly as
    the ingestion lane's declared origin says it must be."""
    return HullManifest.from_dict(manifest_dict)


@pytest.fixture
def domain(manifest: HullManifest):
    return build_hull_domain(manifest)


def _shifted(manifest_dict, dx: float):
    """The same hull, moved forward. Still a legal manifest: the origin
    declaration constrains z (keel on 0), not x."""
    d = dict(manifest_dict)
    d["bbox_min_m"] = [manifest_dict["bbox_min_m"][0] + dx,
                       *manifest_dict["bbox_min_m"][1:]]
    d["bbox_max_m"] = [manifest_dict["bbox_max_m"][0] + dx,
                       *manifest_dict["bbox_max_m"][1:]]
    return HullManifest.from_dict(d)


# --------------------------------------------------------------------------- #
#  The builder
# --------------------------------------------------------------------------- #

def test_every_refinement_box_contains_the_off_origin_hull(manifest, domain):
    """THE REGRESSION. Not just the innermost box -- every one of them.

    A box that stops short of the bow does not merely fail to refine there; it
    leaves a face of the previous level's size on a stagnation surface, and
    that face integrates a pressure it is far too coarse to represent.
    """
    hx0, hx1 = manifest.bbox_min_m[0], manifest.bbox_max_m[0]
    for stage, (lo, hi) in enumerate(refinement_boxes(manifest, domain), 1):
        assert lo[0] <= hx0, (
            f"box {stage} starts at x={lo[0]:.4g}, aft of the hull's aft end "
            f"x={hx0:.4g}"
        )
        assert hi[0] >= hx1, (
            f"box {stage} reaches x={hi[0]:.4g}, short of the bow x={hx1:.4g}"
        )


def test_the_finest_box_clears_the_hull_with_a_margin(manifest, domain):
    """Containment is not enough: a box that ends ON the bow puts the
    refinement transition in the stagnation region."""
    lo, hi = refinement_boxes(manifest, domain)[-1]
    assert lo[0] < manifest.bbox_min_m[0]
    assert hi[0] > manifest.bbox_max_m[0]


def test_the_staging_follows_the_hull_when_the_hull_moves(manifest_dict):
    """Move the hull, and the boxes move with it.

    This is the property the defect violated most starkly: the emitted boxes
    were a function of Lpp and of nothing about WHERE the hull was. A builder
    that keys off the origin passes every containment test written on a hull
    that happens to straddle it, and fails on every real one.
    """
    dx = 2.0
    here, there = HullManifest.from_dict(manifest_dict), _shifted(manifest_dict, dx)
    a = refinement_boxes(here, build_hull_domain(here))[-1]
    b = refinement_boxes(there, build_hull_domain(there))[-1]
    assert b[0][0] == pytest.approx(a[0][0] + dx)
    assert b[1][0] == pytest.approx(a[1][0] + dx)


def test_the_reference_point_is_the_hull_centre_not_the_origin(manifest):
    """The geometric interpolation between the outer and inner box is taken
    about a reference. That reference is the hull, read off the manifest."""
    centre = 0.5 * (manifest.bbox_min_m[0] + manifest.bbox_max_m[0])
    assert hull_x_centre(manifest) == pytest.approx(centre)
    assert hull_x_centre(manifest) != 0.0, "the fixture hull is off-origin"


def test_boxes_are_still_nested_and_still_stop_at_the_centreplane(manifest, domain):
    """The transverse and vertical staging was correct and must stay so: this
    fix moves x, and a fix that quietly re-derived y or z would be a second
    change hiding inside the first."""
    boxes = refinement_boxes(manifest, domain)
    for outer, inner in zip(boxes, boxes[1:]):
        for axis in range(3):
            assert outer[0][axis] <= inner[0][axis]
            assert inner[1][axis] <= outer[1][axis]
    for _lo, hi in boxes:
        assert hi[1] == pytest.approx(0.0)
    for lo, hi in boxes:
        assert lo[2] < domain.waterline < hi[2]


# --------------------------------------------------------------------------- #
#  The pre-mesh assertion
# --------------------------------------------------------------------------- #

def test_the_meshed_hull_is_the_hull_clipped_to_the_domain(manifest, domain):
    """What must be contained is the part of the hull the domain holds.

    The half domain cuts the hull at the centreplane and the double-body
    domain cuts it at the waterline. Asserting containment of the WHOLE
    bounding box would refuse both, correctly-built.
    """
    lo, hi = meshed_hull_bbox(manifest, domain)
    assert lo[0] == pytest.approx(manifest.bbox_min_m[0])
    assert hi[0] == pytest.approx(manifest.bbox_max_m[0])
    assert hi[1] == pytest.approx(0.0), "the meshed half stops at y = 0"
    assert lo[1] == pytest.approx(manifest.bbox_min_m[1])


def test_the_assertion_passes_on_the_builders_own_staging(manifest, domain):
    assert_boxes_contain_hull(manifest, domain, refinement_boxes(manifest, domain))


def test_the_assertion_rejects_a_box_that_stops_short_of_the_bow(manifest, domain):
    """The exact defect, reduced to its one-line check.

    The message has to name the box, its extent and the hull's, because the
    person reading it is looking at a build that refused and has to decide
    whether the staging or the geometry is wrong.
    """
    boxes = refinement_boxes(manifest, domain)
    lo, hi = boxes[-1]
    truncated = (lo, (manifest.bbox_max_m[0] - 0.5, hi[1], hi[2]))
    with pytest.raises(HullPlacementError) as exc:
        assert_boxes_contain_hull(manifest, domain, [*boxes[:-1], truncated])
    message = str(exc.value)
    assert "6" in message, "the failing box is named"
    assert "x" in message
    assert f"{manifest.bbox_max_m[0]:.4g}" in message, "the hull extent is quoted"


def test_the_assertion_rejects_the_symmetric_about_origin_staging(manifest, domain):
    """The staging the defective builder emitted, handed to the guard.

    This is the whole point of the guard: it is cheap, it runs before any
    compute, and it would have refused the campaign at build time.
    """
    symmetric = [
        ((-hi[0], lo[1], lo[2]), (hi[0], hi[1], hi[2]))
        for lo, hi in [
            ((0.0, -0.6, -0.2), (3.46, 0.0, 0.5)),
        ]
    ]
    with pytest.raises(HullPlacementError):
        assert_boxes_contain_hull(manifest, domain, symmetric)


def test_a_hull_outside_the_domain_is_refused_before_any_box_is_checked(
    manifest_dict,
):
    far = _shifted(manifest_dict, 500.0)
    domain = build_hull_domain(HullManifest.from_dict(manifest_dict))
    with pytest.raises(HullPlacementError):
        meshed_hull_bbox(far, domain)


# --------------------------------------------------------------------------- #
#  The hull's own surface refinement
# --------------------------------------------------------------------------- #

def test_the_hull_carries_a_non_zero_surface_refinement_level():
    """``level (0 0)`` made the hull's resolution a pure function of where the
    boxes happened to land. It is a floor now, so a staging error degrades the
    mesh instead of deleting the bow from it."""
    assert HULL_REFINEMENT_LEVEL[0] >= 1
    assert hull_region("hull.stl").refinement_level == HULL_REFINEMENT_LEVEL


def test_the_hull_level_sits_below_the_appendage_levels():
    """The ladder is monotone in size: the smaller the body, the more levels.
    A rudder takes 2 and a shaft boss 3 on the case this was found on, both
    derived from cells-across-the-smallest-dimension; the hull is the largest
    body in the domain and takes the lowest non-zero rung."""
    assert 1 <= HULL_REFINEMENT_LEVEL[0] < 2
    assert HULL_REFINEMENT_LEVEL[0] == HULL_REFINEMENT_LEVEL[1]
