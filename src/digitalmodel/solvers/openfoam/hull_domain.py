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
from typing import Any, Dict, List, Optional, Sequence, Tuple

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
    "HullWakeCoverage",
    "HullWakeExtent",
    "HullWakeExtentError",
    "preflight_wake_extent",
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


@dataclass(frozen=True)
class HullWakeCoverage:
    """Available horizontal coverage in one domain or refinement box."""

    name: str
    aft_extent_m: float
    lateral_half_width_m: float
    wake_wavelengths: float
    meets_requirement: bool

    def to_provenance(self) -> Dict[str, Any]:
        """Return named coverage quantities with explicit units."""
        return {
            "name": self.name,
            "aft_extent_m": self.aft_extent_m,
            "lateral_half_width_m": self.lateral_half_width_m,
            "wake_wavelengths": self.wake_wavelengths,
            "meets_requirement": self.meets_requirement,
        }


@dataclass(frozen=True)
class HullWakeExtent:
    """Geometric preflight result, also retained on a coverage refusal."""

    speed_m_s: float
    gravity_m_s2: float
    required_wake_wavelengths: float
    wedge_half_angle_deg: float
    wavelength_m: float
    stern_x_m: float
    bow_x_m: float
    hull_half_beam_m: float
    required_aft_extent_m: float
    required_aft_x_m: float
    required_lateral_half_width_m: float
    domain: HullWakeCoverage
    boxes: Tuple[HullWakeCoverage, ...]

    @property
    def meets_requirement(self) -> bool:
        """Whether the domain and every supplied stage cover the envelope."""
        return self.domain.meets_requirement and all(
            box.meets_requirement for box in self.boxes
        )

    def to_provenance(self) -> Dict[str, Any]:
        """Return JSON-compatible required and achieved geometric coverage."""
        coverage = (self.domain, *self.boxes)
        return {
            "status": "PASS" if self.meets_requirement else "FAIL",
            "speed_m_s": self.speed_m_s,
            "gravity_m_s2": self.gravity_m_s2,
            "wedge_half_angle_deg": self.wedge_half_angle_deg,
            "wavelength_m": self.wavelength_m,
            "stern_x_m": self.stern_x_m,
            "bow_x_m": self.bow_x_m,
            "hull_half_beam_m": self.hull_half_beam_m,
            "required": {
                "wake_wavelengths": self.required_wake_wavelengths,
                "aft_extent_m": self.required_aft_extent_m,
                "aft_x_m": self.required_aft_x_m,
                "lateral_half_width_m": self.required_lateral_half_width_m,
            },
            "achieved": {
                "aft_extent_m": min(c.aft_extent_m for c in coverage),
                "lateral_half_width_m": min(c.lateral_half_width_m for c in coverage),
                "wake_wavelengths": min(c.wake_wavelengths for c in coverage),
            },
            "domain": self.domain.to_provenance(),
            "boxes": [box.to_provenance() for box in self.boxes],
            "basis": (
                "lambda = 2*pi*speed^2/gravity; aft extent starts at the actual "
                "stern; the wedge starts at the bow with the hull half beam; "
                "aft half width = half_beam + (bow_x - aft_x)*tan(half_angle). "
                "Achieved wavelengths are limited by both aft and lateral reach."
            ),
            "limits": (
                "geometry only, assuming linear deep-water dispersion; no depth "
                "validity, cell resolution, wave amplitude, mesh quality, "
                "reflection or CFD accuracy assessment; boxes are not resized "
                "and the solver builder is not changed"
            ),
        }


class HullWakeExtentError(HullDomainError):
    """Unsupported inputs or inadequate coverage; a shortfall retains its report."""

    def __init__(self, message: str, report: Optional[HullWakeExtent] = None) -> None:
        """Store the report when valid geometry fails the requested coverage."""
        super().__init__(message)
        self.report = report


def preflight_wake_extent(
    manifest: HullManifest,
    domain: HullDomain,
    boxes: Sequence[Box],
    *,
    speed_m_s: float,
    gravity_m_s2: float,
    required_wake_wavelengths: float,
    wedge_half_angle_deg: float,
) -> HullWakeExtent:
    """Check supplied boxes without changing geometry or writing case files.

    Every criterion is mandatory. This API supports the existing half domain:
    flow in negative x, centreplane y=0, waterline strictly inside every box.
    Each stage must contain the hull and the entire bow-origin wedge through
    N wavelengths aft of the stern. Nested axis-aligned boxes cover that wedge
    exactly when they contain its bounding rectangle at the waterline.

    Basis: linear deep-water dispersion omega**2=g*k and U=omega/k imply
    lambda=2*pi*U**2/g (also used by hull_free_surface.deep_water_wavelength).
    The widening is right-triangle geometry, not a prescribed wake angle.
    The open angular interval (0, 90 degrees) excludes a degenerate wedge and
    an unbounded tangent. No engineering acceptance threshold is inferred.
    """
    # Local import avoids the existing free-surface module's constant import.
    from .hull_free_surface import deep_water_wavelength

    for name, value in (
        ("speed_m_s", speed_m_s),
        ("gravity_m_s2", gravity_m_s2),
        ("required_wake_wavelengths", required_wake_wavelengths),
    ):
        if not math.isfinite(value) or value <= 0:
            raise HullWakeExtentError(f"{name} must be finite and positive")
    if not math.isfinite(wedge_half_angle_deg) or not 0 < wedge_half_angle_deg < 90:
        raise HullWakeExtentError("wedge_half_angle_deg must be finite and between 0 and 90")

    bounds = _wake_domain_bounds(domain)
    if not boxes:
        raise HullWakeExtentError("at least one refinement box is required; boxes are empty")
    for stage, (lo, hi) in enumerate(boxes, start=1):
        if (len(lo) != 3 or len(hi) != 3
                or not all(math.isfinite(v) for v in (*lo, *hi))
                or not all(a < b for a, b in zip(lo, hi))):
            raise HullWakeExtentError(f"box {stage} must have finite, ordered 3D bounds")
        if not all(bounds[0][i] <= lo[i] < hi[i] <= bounds[1][i] for i in range(3)):
            raise HullWakeExtentError(f"box {stage} lies outside domain bounds")
        if not lo[2] < domain.waterline < hi[2]:
            raise HullWakeExtentError(f"box {stage} must straddle the waterline")
    assert_boxes_contain_hull(manifest, domain, boxes)
    for outer, inner in zip(boxes, boxes[1:]):
        assert_inner_inside_outer(inner, outer)

    stern, bow = manifest.bbox_min_m[0], manifest.bbox_max_m[0]
    half_beam = max(manifest.half_beam_m, -manifest.bbox_min_m[1])
    if (not all(math.isfinite(v) for v in (stern, bow, half_beam))
            or not stern < bow or half_beam <= 0):
        raise HullWakeExtentError("hull must have finite ordered x bounds and positive half beam")
    if not bounds[0][0] <= stern < bow <= bounds[1][0] or -half_beam < bounds[0][1]:
        raise HullWakeExtentError("hull horizontal envelope lies outside domain bounds")

    for stage, (_, hi) in enumerate(boxes, start=1):
        if hi[0] < bow or hi[1] != domain.y_centreplane:
            raise HullWakeExtentError(f"box {stage} must contain the bow and centreplane")

    wavelength = deep_water_wavelength(speed_m_s, gravity=gravity_m_s2)
    reach = required_wake_wavelengths * wavelength
    aft_x = stern - reach
    slope = math.tan(math.radians(wedge_half_angle_deg))
    if not math.isfinite(slope) or slope <= 0:
        raise HullWakeExtentError("wedge_half_angle_deg must produce a finite positive tangent")
    width = half_beam + (bow - aft_x) * slope
    if (not all(math.isfinite(v) for v in (wavelength, reach, aft_x, width))
            or wavelength <= 0 or reach <= 0 or aft_x >= stern):
        raise HullWakeExtentError("inputs must produce finite, positive, representable wake extents")

    failures = []

    def coverage(name: str, box: Box) -> HullWakeCoverage:
        lo, _ = box
        aft = stern - lo[0]
        lateral = -lo[1]
        # Invert b+(bow-stern+reach)*tan(angle) <= lateral for reach.
        lateral_reach = (lateral - half_beam) / slope - (bow - stern)
        supported = max(0.0, min(aft, lateral_reach)) / wavelength
        if not all(math.isfinite(v) for v in (aft, lateral, supported)):
            raise HullWakeExtentError(f"{name} coverage must be finite and serializable")
        aft_ok, lateral_ok = lo[0] <= aft_x, lateral >= width
        if not aft_ok:
            failures.append(f"{name} aft extent is insufficient")
        if not lateral_ok:
            failures.append(f"{name} lateral envelope is insufficient")
        return HullWakeCoverage(name, aft, lateral, supported, aft_ok and lateral_ok)

    report = HullWakeExtent(
        speed_m_s, gravity_m_s2, required_wake_wavelengths, wedge_half_angle_deg,
        wavelength, stern, bow, half_beam, reach, aft_x, width, coverage("domain", bounds),
        tuple(coverage(f"box {stage}", box) for stage, box in enumerate(boxes, start=1)),
    )
    if failures:
        raise HullWakeExtentError("; ".join(failures), report)
    return report


def _wake_domain_bounds(domain: HullDomain) -> Box:
    """Refuse unsupported coordinates and invalid free-surface domains."""
    z = domain.z_levels
    if (not domain.half_domain or domain.y_centreplane != 0
            or domain.flow_direction != (-1.0, 0.0, 0.0)):
        raise HullWakeExtentError("domain must be a y=0 half domain with flow in negative x")
    if (len(z) < 2 or not all(math.isfinite(v) for v in (
            domain.x_outlet, domain.x_inlet, domain.y_side, domain.waterline, *z))
            or not domain.x_outlet < domain.x_inlet or not domain.y_side < 0
            or not all(a < b for a, b in zip(z, z[1:]))
            or not z[0] < domain.waterline < z[-1]):
        raise HullWakeExtentError("domain must have finite ordered bounds and straddle the waterline")
    return ((domain.x_outlet, domain.y_side, z[0]),
            (domain.x_inlet, domain.y_centreplane, z[-1]))


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
