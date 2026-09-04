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
from .hull_placement import (
    assert_boxes_contain_hull,
    assert_inner_inside_outer,
    hull_x_centre,
    offset_interp as _offset_interp,
)
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


# --- Extents, in multiples of Lpp measured from MIDSHIP --------------------- #
#
#  DEFAULTS: every one is also a ``HullCaseConfig`` field, so a case can trim
#  its own far field. These reproduce the DTC tutorial's own proportions (whose
#  domain the KCS validation case uses) to within rounding.
#
#  MEASURED FROM THE ORIGIN OF THE CASE FRAME = the hull's AFT PERPENDICULAR,
#  not midship and not the hull's ends. This claimed the ITTC 7.5-03-02-03
#  minimums measured from the ends, which assumed a translation the builder
#  does not perform: true forward clearance is ``upstream_lpp - LOA/Lpp``.
#  Far-field sizing, deliberately left alone by #2033, which fixed resolution.

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
#: DTC's own ratios (its band cell is 0.01475 m against a 1 m background). Every
#: vertical count is driven by ``base_cell_size`` rather than by Lpp, so "hold
#: the target cell size and grow the hull" grows the count with the VOLUME in
#: all three directions; anchoring the band to Lpp would hold the vertical
#: counts fixed and grow the budget by 100x, not 1000x -- which reads like a
#: working derivation right up until it silently under-sizes a full-scale case.
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
    #: Level boundaries of the stacked blockMesh blocks, ascending. SEVEN for
    #: the free-surface ladder ``build_hull_domain`` builds; the double-body
    #: variant truncates the same domain at the waterline and carries three.
    z_levels: Tuple[float, ...]
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
    """The seven levels bounding blockMesh's six stacked blocks."""
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
    the free surface, so snappy keeps the region OUTSIDE the hull.

    ``stern_x`` is read in CASE coordinates (#2033); subtracting the
    bounding-box centre put the keep-point half an Lpp further aft than meant.
    """
    stern_x = manifest.bbox_min_m[0]
    return (
        (stern_x + x_outlet) / 2.0,
        y_side / 2.0,
        (z_levels[1] + draft) / 2.0,
    )


#: What binds each z-level junction, so a refusal names the knob rather than an
#: index. Index 5 -- FS_OUTER_LPP against ``freeboard_lpp`` -- is the one a
#: trimmed freeboard hits: freeboard_lpp must exceed FS_OUTER_LPP + draft / Lpp.
_LEVEL_CONSTRAINTS = (
    "DEPTH_LPP/KEEL_CLEARANCE_DRAFTS KEEL_CLEARANCE_DRAFTS/FS_BAND_BELOW_LPP "
    "FS_BAND_BELOW_LPP FS_BAND_ABOVE_LPP FS_BAND_ABOVE_LPP/FS_OUTER_LPP "
    "FS_OUTER_LPP/freeboard_lpp"
).split()


def _check_levels(levels: Sequence[float]) -> None:
    for i, (a, b) in enumerate(zip(levels, levels[1:])):
        if not a < b:
            raise HullDomainError(
                f"blockMesh z-levels are not strictly increasing at index {i}: "
                f"{a:.6g} >= {b:.6g}. Levels: {[round(v, 6) for v in levels]}. "
                f"The binding constraint is {_LEVEL_CONSTRAINTS[i]}. An "
                f"inverted block is a negative-volume cell blockMesh refuses."
            )


# --- Refinement boxes ------------------------------------------------------- #

def refinement_boxes(
    manifest: HullManifest,
    domain: HullDomain,
    stages: int = REFINEMENT_STAGES,
    inner_margin_x_lpp: float = INNER_MARGIN_X_LPP,
    inner_margin_y_beam: float = INNER_MARGIN_Y_BEAM,
    inner_margin_z_lpp: float = INNER_MARGIN_Z_LPP,
) -> List[Box]:
    """The nested topoSet boxes, outermost first.

    The OUTER box is Lpp-scaled far field. The INNER box is the hull's own
    bounding box plus a margin -- read off the geometry rather than assumed
    from a ship-like beam/length, so a barge gets a box wide enough to hold it.
    The stages between interpolate geometrically, which is the character of the
    tutorial's own staging (its successive boxes shrink by roughly 0.77 per
    stage) without inheriting its numbers.

    EVERY x IS MEASURED FROM THE HULL, NOT THE ORIGIN (#2033). These were
    symmetric intervals about x = 0 over a bounding box translated by its own
    centre. Nothing translates the hull, so the staging sat half an Lpp aft of
    it: levels went on open water and the finest box stopped short of the bow.
    """
    if stages < 1:
        raise HullDomainError(f"stages must be >= 1, got {stages}")

    lpp = manifest.lpp_m
    # The hull's own centre, in the coordinates the surfaces are placed in.
    ref_x, ref_z = hull_x_centre(manifest), domain.waterline

    outer = _outer_box(lpp, domain, ref_x)
    inner = _inner_box(manifest, domain, inner_margin_x_lpp, inner_margin_y_beam, inner_margin_z_lpp)
    assert_inner_inside_outer(inner, outer)

    boxes: List[Box] = []
    for k in range(stages):
        t = k / (stages - 1) if stages > 1 else 1.0
        lo = (
            _offset_interp(outer[0][0], inner[0][0], t, ref_x),
            _offset_interp(outer[0][1], inner[0][1], t, 0.0),
            _offset_interp(outer[0][2], inner[0][2], t, ref_z),
        )
        hi = (
            _offset_interp(outer[1][0], inner[1][0], t, ref_x),
            0.0,
            _offset_interp(outer[1][2], inner[1][2], t, ref_z),
        )
        boxes.append((lo, hi))
    assert_boxes_contain_hull(manifest, domain, boxes)
    return boxes


def _outer_box(lpp: float, domain: HullDomain, ref_x: float) -> Box:
    """Far field. DTC's x offsets, taken about the HULL's centre rather than
    about the origin, then clamped to the domain."""
    lo = (
        max(ref_x + OUTER_BOX_LPP["x_lo"] * lpp, domain.x_outlet),
        max(OUTER_BOX_LPP["y_lo"] * lpp, domain.y_side),
        max(OUTER_BOX_LPP["z_lo"] * lpp, domain.z_levels[0]),
    )
    hi = (
        min(ref_x + OUTER_BOX_LPP["x_hi"] * lpp, domain.x_inlet),
        0.0,
        min(domain.waterline + OUTER_BOX_LPP["z_hi"] * lpp, domain.z_levels[-1]),
    )
    return lo, hi


def _inner_box(
    manifest: HullManifest,
    domain: HullDomain,
    margin_x_lpp: float = INNER_MARGIN_X_LPP,
    margin_y_beam: float = INNER_MARGIN_Y_BEAM,
    margin_z_lpp: float = INNER_MARGIN_Z_LPP,
) -> Box:
    """The hull's bounding box, expanded, and forced to straddle the free
    surface even for a hull whose topsides sit below it. NO TRANSLATION: y and
    z were already read off the geometry, and x no longer subtracts the centre.

    The margins are GEOMETRY-scaled, so in cells they shrink as the mesh
    coarsens. snappyHexMesh needs its nCellsBetweenLevels transition (3 cells)
    plus a cell or two between the hull surface and the box face, or its
    dangling-cell pass reaches the hanging-node cells the x-y ladder leaves on
    that face and hexRef8 aborts ("uses more than 8 points"). Measured on the
    Wigley 44-cells-per-wavelength case: 2.4 cells above the deck, 1.5 beside
    the hull -> abort; a convergence triplet therefore states margins sized
    for its COARSEST member and uses them for every member, so the box
    extents are identical and only the cell changes.
    """
    lpp = manifest.lpp_m
    mx = margin_x_lpp * lpp
    mz = margin_z_lpp * lpp
    lo = (
        manifest.bbox_min_m[0] - mx,
        min(
            manifest.bbox_min_m[1] * (1.0 + margin_y_beam),
            -manifest.half_beam_m * (1.0 + margin_y_beam),
        ),
        min(manifest.bbox_min_m[2], domain.waterline) - mz,
    )
    hi = (
        manifest.bbox_max_m[0] + mx,
        0.0,
        max(manifest.bbox_max_m[2], domain.waterline) + mz,
    )
    return lo, hi


# --- Background block divisions --------------------------------------------- #

def block_divisions(
    domain: HullDomain,
    free_surface_cell_fraction: float = FREE_SURFACE_CELL_FRACTION,
) -> Dict[str, int]:
    """Cell counts for the six stacked blockMesh blocks.

    The free-surface band sets the finest vertical cell and every other block
    is a stated multiple of it. All six counts, and both horizontal counts,
    come from ``base_cell_size``, which is what makes ``relativeSizes true`` in
    snappyHexMeshDict mean the same thing at every hull scale.

    The IN-PLANE free-surface requirement -- cells per wavelength, which moves
    with the SPEED -- is the analogue in ``hull_free_surface``; it raises nx and
    ny on top of these counts and leaves the vertical staging alone.
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
