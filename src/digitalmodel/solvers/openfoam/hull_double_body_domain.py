#!/usr/bin/env python3
"""
ABOUTME: Domain, refinement staging and vertical stack for the DOUBLE-BODY
hull case (#2023). Three things differ from the free-surface case and nothing
else does, which is why they live here and the rest is called rather than
copied:

* the domain is TRUNCATED at the waterline, so it has three block levels
  instead of seven and no air column;
* the refinement staging is the free-surface staging CLIPPED to the water
  column -- the part of each box above the waterline enclosed air;
* the vertical cell in the near field is the free-surface ladder's own, taken
  from that ladder rather than re-derived, so the two case types resolve the
  boundary layer identically.

The last is the load-bearing one. A form factor is a RATIO whose numerator is
computed here and which is then applied to a free-surface result computed on
the other mesh. Any difference in near-wall resolution between the two lands
in k and is indistinguishable from hull form.
"""

from __future__ import annotations

import dataclasses
import math
from typing import Dict, List, Optional, Sequence, Tuple

from .hull_case_physics import Block
from .hull_domain import (
    FREEBOARD_LPP,
    Box,
    HullDomain,
    HullDomainError,
    block_divisions,
    build_hull_domain,
    refinement_boxes,
)
from .hull_manifest import HullManifest
from .hull_placement import assert_boxes_contain_hull

__all__ = [
    "build_double_body_domain",
    "clip_boxes_at_waterline",
    "double_body_divisions",
    "double_body_refinement_boxes",
    "double_body_vertical_blocks",
]


def build_double_body_domain(
    manifest: HullManifest,
    *,
    base_cell_size: Optional[float] = None,
    upstream_lpp: float,
    downstream_lpp: float,
    lateral_lpp: float,
    depth_lpp: float,
    keel_clearance_drafts: float,
) -> Tuple[HullDomain, HullDomain]:
    """``(double_body_domain, free_surface_reference)``.

    The reference is built by ``build_hull_domain`` from the same ITTC factors,
    so the x and y extents, the floor, the background cell and the point in the
    free stream are identical to the free-surface case at the same level. The
    double-body domain is that box with everything above the waterline removed
    and the free-surface band collapsed, leaving three levels:

        floor -> keel clearance -> waterline

    ``freeboard_lpp`` is not a parameter of this function. It sizes the air
    column, which is exactly what is being removed, so the reference is built
    with the library default and the blocks above the waterline are discarded.
    """
    reference = build_hull_domain(
        manifest,
        base_cell_size=base_cell_size,
        upstream_lpp=upstream_lpp,
        downstream_lpp=downstream_lpp,
        lateral_lpp=lateral_lpp,
        depth_lpp=depth_lpp,
        freeboard_lpp=FREEBOARD_LPP,
        keel_clearance_drafts=keel_clearance_drafts,
    )
    z = (reference.z_levels[0], reference.z_levels[1], reference.waterline)
    if not z[0] < z[1] < z[2]:
        raise HullDomainError(
            f"the double-body z-levels are not strictly increasing: "
            f"{[round(v, 6) for v in z]}. The keel-clearance level must sit "
            f"below the waterline and above the floor; an inverted block is a "
            f"negative-volume cell blockMesh refuses."
        )
    return dataclasses.replace(reference, z_levels=z), reference


def double_body_refinement_boxes(
    manifest: HullManifest, reference: HullDomain, domain: HullDomain
) -> List[Box]:
    """The staging for this case: generated, then clipped.

    Generated against the UNTRUNCATED reference on purpose -- see
    :func:`clip_boxes_at_waterline` -- so the two steps are kept together and
    nobody has to remember which domain the first one takes.

    The containment guard runs TWICE, and the second run is the load-bearing
    one (#2033): ``refinement_boxes`` checks the staging against the reference
    domain, and the check below re-runs it against the TRUNCATED domain the
    case is actually meshed in, on the boxes that were actually emitted. A
    guard that only ever sees the pre-clip boxes certifies something the mesher
    never receives.
    """
    boxes = clip_boxes_at_waterline(refinement_boxes(manifest, reference), domain)
    assert_boxes_contain_hull(manifest, domain, boxes)
    return boxes


def clip_boxes_at_waterline(
    boxes: Sequence[Box], domain: HullDomain
) -> List[Box]:
    """The free-surface refinement staging, clipped to the water column.

    The boxes are generated against the UNTRUNCATED domain so the containment
    check in ``refinement_boxes`` sees the real geometry -- the innermost box
    is the hull's own bounding box, which reaches above the waterline on any
    hull with freeboard, and would be reported as escaping a domain that stops
    there. Clipping afterwards is safe because clamping is monotone: a nested
    family stays nested.
    """
    floor, top = domain.z_levels[0], domain.z_levels[-1]
    clipped: List[Box] = []
    for lo, hi in boxes:
        z_lo, z_hi = max(lo[2], floor), min(hi[2], top)
        if not z_lo < z_hi:
            raise HullDomainError(
                f"refinement box z-range [{lo[2]:.4g}, {hi[2]:.4g}] does not "
                f"intersect the water column [{floor:.4g}, {top:.4g}]"
            )
        clipped.append(((lo[0], lo[1], z_lo), (hi[0], hi[1], z_hi)))
    return clipped


# --------------------------------------------------------------------------- #
#  The vertical stack
# --------------------------------------------------------------------------- #

def double_body_divisions(
    reference: HullDomain, domain: HullDomain
) -> Dict[str, int]:
    """Cell counts for the two stacked blocks, and the in-plane counts.

    ``nx`` and ``ny`` are the free-surface ladder's, UNREFINED. The in-plane
    criterion that would raise them scores cells per wavelength, and there is
    no wave here; spending the square of a linear factor on every cell in the
    case to resolve a wave that has been replaced by a symmetry plane would be
    a cost with nothing on the other side of it.

    The vertical cell in the near field is the free-surface ladder's near-field
    cell, taken from the ladder itself rather than re-derived from the
    constants that produced it. That is the load-bearing choice in this module.
    A form factor is a RATIO of the double-body result to a correlation line,
    and it is applied to a free-surface result computed on the other mesh; if
    the two meshes resolve the boundary layer differently, the difference lands
    in k and is indistinguishable from hull form.
    """
    ladder = block_divisions(reference)
    zr = reference.z_levels
    near_cell = (zr[2] - zr[1]) / ladder["nza"]
    span = domain.z_levels[2] - domain.z_levels[1]
    return {
        "nx": ladder["nx"],
        "ny": ladder["ny"],
        # The floor-to-keel-clearance block is the ladder's own, unchanged:
        # same z-range, same count, same grading in the emitted dictionary.
        "nzdeep": ladder["nza"],
        "nznear": max(1, math.ceil(span / near_cell)),
    }


def double_body_vertical_blocks(
    domain: HullDomain, divisions: Dict[str, int]
) -> Tuple[Block, ...]:
    """The two stacked blocks, in the form the cell-budget estimate walks."""
    z = domain.z_levels
    return (
        (z[0], z[1], divisions["nzdeep"]),
        (z[1], z[2], divisions["nznear"]),
    )
