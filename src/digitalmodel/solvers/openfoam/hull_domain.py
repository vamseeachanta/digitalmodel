#!/usr/bin/env python3
"""
ABOUTME: Domain extents, refinement boxes and background block divisions for an
arbitrary hull (#2023). Every quantity is a multiple of Lpp, beam or draft, so a
hull ten times larger gets a domain ten times larger and nothing else changes.

WHY NOT THE KCS ROUTE. The KCS resistance path scales a FIXED DTC box by the
ratio of the two hulls' overall lengths. That is exact for a geometrically
similar hull and wrong for anything else, because the ratio carries DTC's
proportions -- its beam/length and draft/length -- into a hull that does not
have them. Here the proportions are read off the hull in front of us and the
box arithmetic is delegated to ``DomainBuilder.from_hull_dims``, which already
existed and was already wired into ``openfoam setup``.

TWO CONVENTIONS MEET HERE. ``DomainBuilder`` puts the flow in +x with the bow
upstream at -x. The template inherited from the DTC tutorial puts the inlet at
+x with an internal field of -Umean, so the flow runs in -x and the bow must be
at +x. The manifest states x = forward, so the hull is already correct and the
DOMAIN is what gets mirrored. Unhandled, this tows the hull stern-first: it
meshes, it converges, and it answers a different question.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Dict, List, Optional, Sequence, Tuple

from .domain_builder import DomainBuilder
from .hull_manifest import HullManifest
from .models import DomainConfig

__all__ = [
    "BACKGROUND_CELLS_PER_LPP",
    "DEPTH_LPP",
    "DOWNSTREAM_LPP",
    "FREEBOARD_LPP",
    "LATERAL_LPP",
    "UPSTREAM_LPP",
    "HullDomain",
    "HullDomainError",
    "block_divisions",
    "build_hull_domain",
    "refinement_boxes",
]

Vec3 = Tuple[float, float, float]
Box = Tuple[Vec3, Vec3]


class HullDomainError(ValueError):
    """A domain that would emit an invalid or inverted blockMeshDict."""


# --------------------------------------------------------------------------- #
#  Extents, in multiples of Lpp measured from MIDSHIP.
#
#  These reproduce the DTC tutorial's own proportions (whose domain the KCS
#  validation case uses) to within rounding, and they satisfy the ITTC
#  7.5-03-02-03 minimums measured from the hull ENDS: 1.5 Lpp ahead of the bow,
#  4.0 Lpp astern, 3.0 Lpp to the side, 2.5 Lpp below the keel.
# --------------------------------------------------------------------------- #

UPSTREAM_LPP = 2.0
DOWNSTREAM_LPP = 4.5
LATERAL_LPP = 3.0
DEPTH_LPP = 2.5
FREEBOARD_LPP = 0.65

#: Background cell size = Lpp / this. The DTC tutorial's background resolution,
#: which the KCS validation solved on. ``DomainBuilder``'s own default of L/20
#: is deliberately NOT used: it is three times finer in each direction, which
#: is 27x the background cells before the six refinement stages multiply it.
BACKGROUND_CELLS_PER_LPP = 6.3

#: Vertical staging, as multiples of the draft (near the keel) or of Lpp (the
#: free-surface band, whose job is to resolve the wave and whose amplitude
#: scales with Lpp and Froude number, not with draft).
KEEL_CLEARANCE_DRAFTS = 4.0
FS_BAND_BELOW_LPP = 0.0095
FS_BAND_ABOVE_LPP = 0.0090
FS_OUTER_LPP = 0.216

#: Free-surface band cell HEIGHT as a fraction of the background cell size, and
#: the stretching of each block above and below it relative to that height.
#: DTC's own ratios (its band cell is 0.01475 m against a 1 m background).
#:
#: Every vertical count is driven by ``base_cell_size`` rather than by Lpp, so
#: "hold the target cell size and grow the hull" grows the cell count with the
#: VOLUME in all three directions. Anchoring the band to Lpp instead would hold
#: the vertical counts fixed and the budget would grow by 100x, not 1000x --
#: which reads like a working derivation right up until it silently under-sizes
#: a full-scale case.
FREE_SURFACE_CELL_FRACTION = 0.0148
NEAR_FIELD_STRETCH = 1.6
OUTER_STRETCH = 2.2
FAR_STRETCH = 8.1

#: Refinement staging. Six stages, matching topoSetDict.1 .. topoSetDict.6.
REFINEMENT_STAGES = 6
#: Outermost box, from midship / keel, in Lpp. DTC's box 1.
OUTER_BOX_LPP = {
    "x_lo": -2.075,
    "x_hi": 1.112,
    "y_lo": -0.956,
    "z_lo": -0.478,
    "z_hi": 0.478,
}
#: Innermost box: the hull's own bounding box, expanded by these margins.
INNER_MARGIN_X_LPP = 0.060
INNER_MARGIN_Y_BEAM = 0.30
INNER_MARGIN_Z_LPP = 0.024


@dataclass(frozen=True)
class HullDomain:
    """The computational domain, in the emitted case's own coordinates.

    ``config`` is exactly what ``DomainBuilder.from_hull_dims`` returned, kept
    so a reader can see the delegation. ``x_inlet``/``x_outlet`` are that box
    mirrored into the template's flow direction.
    """

    config: DomainConfig
    x_inlet: float
    x_outlet: float
    y_side: float
    z_levels: Tuple[float, float, float, float, float, float, float]
    waterline: float
    base_cell_size: float
    location_in_mesh: Vec3
    half_domain: bool = True

    y_centreplane: float = 0.0
    flow_direction: Vec3 = (-1.0, 0.0, 0.0)

    @property
    def length(self) -> float:
        return self.x_inlet - self.x_outlet

    @property
    def width(self) -> float:
        return self.y_centreplane - self.y_side

    @property
    def height(self) -> float:
        return self.z_levels[-1] - self.z_levels[0]

    @property
    def volume(self) -> float:
        return self.length * self.width * self.height


def build_hull_domain(
    manifest: HullManifest,
    *,
    base_cell_size: Optional[float] = None,
    upstream_lpp: float = UPSTREAM_LPP,
    downstream_lpp: float = DOWNSTREAM_LPP,
    lateral_lpp: float = LATERAL_LPP,
    depth_lpp: float = DEPTH_LPP,
    freeboard_lpp: float = FREEBOARD_LPP,
    keel_clearance_drafts: float = KEEL_CLEARANCE_DRAFTS,
) -> HullDomain:
    """Size the domain from the hull's principal dimensions.

    ``DomainBuilder`` scales its lateral factor by the BEAM and its depth
    factor by the DRAFT, so the Lpp-based extents above are converted into that
    parameterisation rather than the box being recomputed here.
    """
    lpp, beam, draft = manifest.lpp_m, manifest.beam_m, manifest.draft_m
    if base_cell_size is None:
        base_cell_size = lpp / BACKGROUND_CELLS_PER_LPP
    if base_cell_size <= 0:
        raise HullDomainError(f"base_cell_size must be positive, got {base_cell_size}")

    config = DomainBuilder.from_hull_dims(
        length=lpp,
        beam=beam,
        draft=draft,
        upstream_factor=upstream_lpp,
        downstream_factor=downstream_lpp,
        lateral_factor=lateral_lpp * lpp / beam,
        depth_factor=depth_lpp * lpp / draft,
        freeboard_factor=freeboard_lpp,
        base_cell_size=base_cell_size,
    )

    # Mirror x: DomainBuilder's upstream (-x) is the template's inlet (+x).
    x_inlet = -config.min_coords[0]
    x_outlet = -config.max_coords[0]
    z_levels = _z_levels(config, lpp, draft, keel_clearance_drafts)

    return HullDomain(
        config=config,
        x_inlet=x_inlet,
        x_outlet=x_outlet,
        y_side=config.min_coords[1],
        z_levels=z_levels,
        waterline=draft,
        base_cell_size=base_cell_size,
        location_in_mesh=_location_in_mesh(
            manifest, x_outlet, config.min_coords[1], z_levels, draft
        ),
    )


def _z_levels(
    config: DomainConfig, lpp: float, draft: float, keel_clearance_drafts: float
) -> Tuple[float, float, float, float, float, float, float]:
    """The seven levels bounding blockMesh's six stacked blocks.

    Near the keel the staging scales with the DRAFT; the free-surface band
    scales with Lpp, because what it exists to resolve is the wave, whose
    amplitude follows Lpp and Froude number rather than the draft.
    """
    levels = (
        config.min_coords[2],
        -keel_clearance_drafts * draft,
        draft - FS_BAND_BELOW_LPP * lpp,
        draft,
        draft + FS_BAND_ABOVE_LPP * lpp,
        draft + FS_OUTER_LPP * lpp,
        config.max_coords[2],
    )
    _check_levels(levels)
    return levels


def _location_in_mesh(
    manifest: HullManifest,
    x_outlet: float,
    y_side: float,
    z_levels: Sequence[float],
    draft: float,
) -> Vec3:
    """A point in the free stream: aft of the placed hull's stern and below
    the free surface, so snappy keeps the region OUTSIDE the hull."""
    stern_x = manifest.bbox_min_m[0] - manifest.midship_x_m
    return (
        (stern_x + x_outlet) / 2.0,
        y_side / 2.0,
        (z_levels[1] + draft) / 2.0,
    )


def _check_levels(levels: Sequence[float]) -> None:
    for i, (a, b) in enumerate(zip(levels, levels[1:])):
        if not a < b:
            raise HullDomainError(
                f"blockMesh z-levels are not strictly increasing at index {i}: "
                f"{a:.6g} >= {b:.6g}. Levels: "
                f"{[round(v, 6) for v in levels]}. An inverted block is a "
                f"negative-volume cell blockMesh will refuse."
            )


# --------------------------------------------------------------------------- #
#  Refinement boxes
# --------------------------------------------------------------------------- #

def refinement_boxes(
    manifest: HullManifest,
    domain: HullDomain,
    stages: int = REFINEMENT_STAGES,
) -> List[Box]:
    """The nested topoSet boxes, outermost first.

    The OUTER box is Lpp-scaled far field. The INNER box is the hull's own
    bounding box plus a margin -- read off the geometry rather than assumed
    from a ship-like beam/length, so a barge gets a box wide enough to hold it.
    The stages between interpolate geometrically, which is the character of the
    tutorial's own staging (its successive boxes shrink by roughly 0.77 per
    stage) without inheriting its numbers.
    """
    if stages < 1:
        raise HullDomainError(f"stages must be >= 1, got {stages}")

    lpp = manifest.lpp_m
    ref_x, ref_z = 0.0, domain.waterline  # hull centre: midship, free surface

    outer = _outer_box(lpp, domain)
    inner = _inner_box(manifest, domain)
    _check_containment(inner, outer)

    boxes: List[Box] = []
    for k in range(stages):
        t = k / (stages - 1) if stages > 1 else 1.0
        lo = (
            _interp(outer[0][0], inner[0][0], t, ref_x),
            _interp(outer[0][1], inner[0][1], t, 0.0),
            _interp(outer[0][2], inner[0][2], t, ref_z),
        )
        hi = (
            _interp(outer[1][0], inner[1][0], t, ref_x),
            0.0,
            _interp(outer[1][2], inner[1][2], t, ref_z),
        )
        boxes.append((lo, hi))
    return boxes


def _outer_box(lpp: float, domain: HullDomain) -> Box:
    lo = (
        max(OUTER_BOX_LPP["x_lo"] * lpp, domain.x_outlet),
        max(OUTER_BOX_LPP["y_lo"] * lpp, domain.y_side),
        max(OUTER_BOX_LPP["z_lo"] * lpp, domain.z_levels[0]),
    )
    hi = (
        min(OUTER_BOX_LPP["x_hi"] * lpp, domain.x_inlet),
        0.0,
        min(domain.waterline + OUTER_BOX_LPP["z_hi"] * lpp, domain.z_levels[-1]),
    )
    return lo, hi


def _inner_box(manifest: HullManifest, domain: HullDomain) -> Box:
    """The hull's bounding box, placed, expanded, and forced to straddle the
    free surface even for a hull whose topsides sit below it."""
    lpp = manifest.lpp_m
    shift = manifest.midship_x_m
    mx = INNER_MARGIN_X_LPP * lpp
    mz = INNER_MARGIN_Z_LPP * lpp
    lo = (
        manifest.bbox_min_m[0] - shift - mx,
        min(
            manifest.bbox_min_m[1] * (1.0 + INNER_MARGIN_Y_BEAM),
            -manifest.half_beam_m * (1.0 + INNER_MARGIN_Y_BEAM),
        ),
        min(manifest.bbox_min_m[2], domain.waterline) - mz,
    )
    hi = (
        manifest.bbox_max_m[0] - shift + mx,
        0.0,
        max(manifest.bbox_max_m[2], domain.waterline) + mz,
    )
    return lo, hi


def _check_containment(inner: Box, outer: Box) -> None:
    for axis, name in enumerate("xyz"):
        if not (outer[0][axis] <= inner[0][axis] and inner[1][axis] <= outer[1][axis]):
            raise HullDomainError(
                f"the innermost refinement box escapes the outermost one in "
                f"{name}: inner [{inner[0][axis]:.4g}, {inner[1][axis]:.4g}] vs "
                f"outer [{outer[0][axis]:.4g}, {outer[1][axis]:.4g}]. The hull "
                f"is too large a fraction of the domain for this staging."
            )


def _interp(outer: float, inner: float, t: float, ref: float) -> float:
    """Geometric interpolation of the OFFSET from ``ref``.

    Geometric rather than linear because the offsets span two orders of
    magnitude and a linear ramp would put every intermediate box out near the
    far field, leaving a 4:1 jump at the hull.
    """
    a, b = outer - ref, inner - ref
    if a == 0.0 or b == 0.0 or (a > 0) != (b > 0):
        return outer + (inner - outer) * t
    return ref + math.copysign(abs(a) ** (1.0 - t) * abs(b) ** t, a)


# --------------------------------------------------------------------------- #
#  Background block divisions
# --------------------------------------------------------------------------- #

def block_divisions(
    domain: HullDomain,
    free_surface_cell_fraction: float = FREE_SURFACE_CELL_FRACTION,
) -> Dict[str, int]:
    """Cell counts for the six stacked blockMesh blocks.

    The free-surface band sets the finest vertical cell and every other block
    is a stated multiple of it. All six counts, and both horizontal counts,
    come from ``base_cell_size``, which is what makes ``relativeSizes true`` in
    snappyHexMeshDict mean the same thing at every hull scale: with the default
    Lpp-proportional cell size the counts are invariant, and with a fixed cell
    size they track the domain volume.
    """
    if free_surface_cell_fraction <= 0:
        raise HullDomainError("free_surface_cell_fraction must be positive")

    h = domain.base_cell_size
    z = domain.z_levels
    band = min(z[3] - z[2], z[4] - z[3])
    nzb = max(1, math.ceil(band / (h * free_surface_cell_fraction)))
    dz_band = band / nzb

    return {
        "nx": max(1, round(domain.length / h)),
        "ny": max(1, round(domain.width / h)),
        "nza": max(1, math.ceil((z[2] - z[1]) / (dz_band * NEAR_FIELD_STRETCH))),
        "nzb": nzb,
        "nzc": max(1, math.ceil((z[5] - z[4]) / (dz_band * OUTER_STRETCH))),
        "nzd": max(1, math.ceil((z[6] - z[5]) / (dz_band * FAR_STRETCH))),
    }
