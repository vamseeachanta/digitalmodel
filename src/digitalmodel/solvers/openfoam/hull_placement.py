#!/usr/bin/env python3
"""
ABOUTME: Where the hull actually IS in the emitted case, and the pre-mesh
assertion that the refinement staging contains it (#2033).

THE CASE FRAME IS THE MANIFEST FRAME. Nothing translates the hull. The case
builder copies every region's STL into ``constant/triSurface`` byte for byte
(``hull_case_regions.copy_region_surfaces``), so a vertex at x = 155.4 in the
source surface is at x = 155.4 in the mesh. The manifest declares its origin
``aft_perpendicular_keel``, so the hull runs from x ~ 0 FORWARD and its
bounding-box centre is roughly half an Lpp ahead of the origin.

WHAT THAT COST. ``hull_domain`` used to subtract the bounding-box centre when
it placed the refinement boxes, on the strength of a docstring saying "the
hull is translated by -this". It is not, and never was. The subtraction, plus
a geometric interpolation taken about x = 0, produced boxes that were
symmetric intervals about the AFT PERPENDICULAR. On a 155 m hull the finest
box reached x = 88.6 m: 42 % of the hull -- the whole bow -- was left at 8 to
16 times the intended cell, and because the hull carried ``level (0 0)``
nothing else refined it either. The mesh passed ``checkMesh``, the solve
converged, and 112 faces out of 16,106 carried 122 % of the net pressure drag.

WHY THE ASSERTION IS SEPARATE FROM THE DERIVATION. The staging is one
derivation among several and could be replaced. The property -- every box
contains the hull -- is what the mesh has to satisfy no matter who computed
the boxes, so it is checked against the geometry rather than against the
arithmetic that produced it. It costs microseconds and it runs before a single
core-hour is spent.
"""

from __future__ import annotations

import math
from typing import TYPE_CHECKING, List, Sequence, Tuple

if TYPE_CHECKING:  # pragma: no cover - typing only
    from .hull_domain import HullDomain
    from .hull_manifest import HullManifest

__all__ = [
    "HullPlacementError",
    "assert_boxes_contain_hull",
    "assert_inner_inside_outer",
    "domain_bbox",
    "hull_bbox",
    "hull_x_centre",
    "meshed_hull_bbox",
    "offset_interp",
]

Vec3 = Tuple[float, float, float]
Box = Tuple[Vec3, Vec3]

#: Slack on a containment comparison, relative to the extent being compared.
#: Boxes are CLIPPED to the domain (the double-body staging is cut at the
#: waterline), so a correct box touches the clip plane exactly and a strict
#: comparison would refuse it on the last bit of a float.
_REL_TOL = 1.0e-9

_AXES = ("x", "y", "z")


class HullPlacementError(ValueError):
    """A case whose refinement staging does not contain the hull it meshes."""


def hull_bbox(manifest: "HullManifest") -> Box:
    """The hull's bounding box IN THE CASE'S OWN COORDINATES.

    Identity, and deliberately a named function rather than two attribute
    reads at each call site. The bug this module exists for was a translation
    applied at one call site and not at the geometry; a single place that
    says "this is where the hull is" is what makes the next such divergence a
    conflict rather than a silence.
    """
    return (tuple(manifest.bbox_min_m), tuple(manifest.bbox_max_m))


def hull_x_centre(manifest: "HullManifest") -> float:
    """x of the hull's bounding-box centre, in case coordinates.

    This is what the refinement staging is built about. It is NOT zero on any
    hull whose manifest states an aft-perpendicular origin, which is every
    hull this lane accepts.
    """
    return 0.5 * (manifest.bbox_min_m[0] + manifest.bbox_max_m[0])


def domain_bbox(domain: "HullDomain") -> Box:
    """The computational domain as a box, in the same coordinates."""
    return (
        (domain.x_outlet, domain.y_side, domain.z_levels[0]),
        (domain.x_inlet, domain.y_centreplane, domain.z_levels[-1]),
    )


def meshed_hull_bbox(manifest: "HullManifest", domain: "HullDomain") -> Box:
    """The part of the hull the domain actually holds: bbox INTERSECT domain.

    Two cuts make this different from the hull's own bounding box, and both
    are deliberate features of the case rather than errors to refuse:

    * the half domain stops at the centreplane, so the starboard half of the
      hull is not meshed;
    * the double-body domain stops at the waterline, so the topsides are not
      meshed.

    Asserting containment of the WHOLE bounding box would refuse both of those
    cases while they were built correctly, and a guard that cries wolf gets
    switched off.
    """
    (hlo, hhi), (dlo, dhi) = hull_bbox(manifest), domain_bbox(domain)
    lo = tuple(max(hlo[i], dlo[i]) for i in range(3))
    hi = tuple(min(hhi[i], dhi[i]) for i in range(3))
    for axis in range(3):
        if not lo[axis] < hi[axis]:
            raise HullPlacementError(
                f"the hull does not intersect the domain in "
                f"{_AXES[axis]}: hull [{hlo[axis]:.4g}, {hhi[axis]:.4g}] vs "
                f"domain [{dlo[axis]:.4g}, {dhi[axis]:.4g}]. The surface is "
                f"placed outside the box the case meshes; nothing downstream "
                f"would say so -- snappyHexMesh meshes open water and reports "
                f"a force of zero."
            )
    return lo, hi


def assert_boxes_contain_hull(
    manifest: "HullManifest",
    domain: "HullDomain",
    boxes: Sequence[Box],
) -> None:
    """PRE-MESH GATE: every refinement box must contain the meshed hull.

    Every box, not only the innermost. A box that stops short of the bow does
    not merely decline to refine there -- it leaves the PREVIOUS level's cell
    on a stagnation surface, and the face that results integrates a pressure
    it is orders of magnitude too coarse to represent. The defect this guards
    was worst at the coarsest surviving level, not at the finest.

    Raised, never warned. A warning in a mesh log is read by nobody: the log
    that carried this defect also carried "Mesh OK" and a 95 % layer-coverage
    figure, and the campaign ran for weeks.
    """
    target = meshed_hull_bbox(manifest, domain)
    if not boxes:
        raise HullPlacementError(
            "no refinement boxes were derived; the hull would be meshed at "
            "the background cell size everywhere"
        )
    for stage, box in enumerate(boxes, start=1):
        _assert_box_contains(box, target, stage=stage)


def _assert_box_contains(box: Box, target: Box, *, stage: int) -> None:
    for axis in range(3):
        tol = _REL_TOL * max(1.0, abs(target[1][axis] - target[0][axis]))
        short_lo = box[0][axis] - target[0][axis]
        short_hi = target[1][axis] - box[1][axis]
        if short_lo <= tol and short_hi <= tol:
            continue
        raise HullPlacementError(
            f"refinement box {stage} does not contain the hull in "
            f"{_AXES[axis]}: box [{box[0][axis]:.4g}, {box[1][axis]:.4g}] vs "
            f"hull [{target[0][axis]:.4g}, {target[1][axis]:.4g}] "
            f"(short by {max(short_lo, 0.0):.4g} at the low end and "
            f"{max(short_hi, 0.0):.4g} at the high end). "
            f"Refinement boxes are built from the hull's bounding box plus a "
            f"margin; a box that misses it leaves that part of the hull at a "
            f"coarser cell, and the coarse faces land on the surface whose "
            f"pressure the case exists to integrate."
        )


def assert_inner_inside_outer(inner: Box, outer: Box) -> None:
    """The innermost box must fit inside the outermost one.

    Distinct from :func:`assert_boxes_contain_hull`: this one says the STAGING
    is nested, that one says the staging is in the right place. A hull that is
    too large a fraction of its domain fails here, and the message names that
    rather than leaving a reader to infer it from an escaped box.
    """
    for axis in range(3):
        if outer[0][axis] <= inner[0][axis] and inner[1][axis] <= outer[1][axis]:
            continue
        raise HullPlacementError(
            f"the innermost refinement box escapes the outermost one in "
            f"{_AXES[axis]}: inner [{inner[0][axis]:.4g}, {inner[1][axis]:.4g}]"
            f" vs outer [{outer[0][axis]:.4g}, {outer[1][axis]:.4g}]. The hull "
            f"is too large a fraction of the domain for this staging."
        )


def offset_interp(outer: float, inner: float, t: float, ref: float) -> float:
    """Geometric interpolation of the OFFSET from ``ref``.

    Geometric rather than linear because the offsets span two orders of
    magnitude and a linear ramp would put every intermediate box out near the
    far field, leaving a 4:1 jump at the hull.

    ``ref`` is the whole point: it is what the staging is centred on, and the
    defect this module is named for was this function being handed 0.0 for the
    x axis on a hull that does not sit on the origin.
    """
    a, b = outer - ref, inner - ref
    if a == 0.0 or b == 0.0 or (a > 0) != (b > 0):
        return outer + (inner - outer) * t
    return ref + math.copysign(abs(a) ** (1.0 - t) * abs(b) ** t, a)


def boxes_provenance(boxes: Sequence[Box]) -> List[dict]:
    """The staging, in the form the case provenance records it."""
    return [{"lo": list(lo), "hi": list(hi)} for lo, hi in boxes]
