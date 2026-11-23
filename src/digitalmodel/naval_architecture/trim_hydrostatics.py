#!/usr/bin/env python3
"""
ABOUTME: Hydrostatics of a tessellated hull at a MEAN DRAFT AND A TRIM -- the
form a loading-condition matrix is actually quoted in -- plus the draft sweep
that produces a hydrostatic table, both over one integration.

WHY TRIM CANNOT BE APPLIED AS A CORRECTION
------------------------------------------
Waterline length, wetted surface and displacement at a trimmed attitude are not
the even-keel values scaled by anything. A hull with a cut-up run carries a
transom that is CLEAR of the water at one trim and immersed at the next, so the
waterline length steps rather than creeps, and the block coefficient -- whose
denominator is that length -- steps with it. The only honest way to get those
numbers is to put the hull in the attitude and integrate again, which is what
this module does.

HOW THE ATTITUDE IS APPLIED
---------------------------
By rotating the hull about the midship point ON the waterline, then keeping the
water horizontal. That pivot is not incidental: rotating about any other point
changes the immersion at midship, so a row of trims quoted at "6.55 m mean
draft" would silently be three different drafts and would not compare with
itself. This pivot leaves the water plane passing through it, so the vertical
immersion at the midship station is exactly the quoted mean draft at any trim.

This is the same geometry as ``appendage_submergence.Attitude``, which tilts the
water plane over a fixed hull instead: that module needs the triangles to stay
put so a clearance is measured against the surface that was ingested, this one
needs the water horizontal so the existing clipped integrators are reused
unchanged. The two views are a rigid rotation apart, and this module builds an
``Attitude`` from the same inputs so the convention is shared, not restated.

SIGN CONVENTION, ONCE
---------------------
``trim = draft_fwd - draft_aft``. NEGATIVE is trim by the stern -- the stern is
deeper. The bow is +x, matching the ship axes ``hull_ingest`` produces. Getting
this backwards does not raise; it produces a complete, plausible table for the
mirror image of the condition asked for, so the tests assert which end went
down by reading the ROTATED TRIANGLES, not the label arithmetic.

WATERLINE LENGTH IS MEASURED ON THE CLIPPED WATERPLANE
------------------------------------------------------
Not on the bounding box of every triangle that happens to dip below the water.
That cheap version is wrong by whatever the longest straddling triangle spans,
and those are the skinny ones on a transom overhang. On a real 158 m hull at
3 m of stern trim it overstated the waterline length by 2.6 m and so
understated Cb by 1.7%, at the one condition where the transom is the story.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Sequence, Tuple

from digitalmodel.naval_architecture.appendage_submergence import Attitude
from digitalmodel.naval_architecture.kcs_geometry import (
    enclosed_volume,
    wetted_surface_area,
)

__all__ = [
    "DEFAULT_WATER_DENSITY_T_M3",
    "FloatingCondition",
    "HullFrame",
    "attitude_for",
    "condition_matrix",
    "end_drafts",
    "hydrostatic_table",
    "hydrostatics_at",
    "rotate_for_trim",
    "trim_angle_rad",
]

Vec3 = Tuple[float, float, float]
Tri = Tuple[Vec3, Vec3, Vec3]

#: Standard seawater. A parameter everywhere rather than folded into the
#: integration: a table quoted in tonnes is only as good as this.
DEFAULT_WATER_DENSITY_T_M3 = 1.025


# --------------------------------------------------------------------------- #
#  The frame the attitude is applied in
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class HullFrame:
    """Where the keel, the midship station and the ends of the hull are.

    Read off the geometry rather than supplied, so a barge, a hull whose origin
    is amidships and a hull whose origin is at the transom all work without the
    caller restating what the file says. ``length_m`` is the hull's own
    longitudinal extent -- the DEFAULT reference length for trim, not a claim
    that it equals Lpp; see :func:`hydrostatics_at`.
    """

    keel_z: float
    x_aft: float
    x_fwd: float

    @classmethod
    def from_triangles(cls, tris: Sequence[Tri]) -> "HullFrame":
        xs = [v[0] for t in tris for v in t]
        zs = [v[2] for t in tris for v in t]
        if not xs:
            raise ValueError("cannot read a hull frame from zero triangles")
        x_aft, x_fwd = min(xs), max(xs)
        if x_fwd - x_aft <= 0.0:
            raise ValueError(
                "the geometry has no longitudinal extent; it is not a hull in "
                "x-forward ship axes"
            )
        if max(zs) - min(zs) <= 0.0:
            raise ValueError("the geometry has no vertical extent")
        return cls(keel_z=min(zs), x_aft=x_aft, x_fwd=x_fwd)

    @property
    def x_midship(self) -> float:
        return 0.5 * (self.x_aft + self.x_fwd)

    @property
    def length_m(self) -> float:
        return self.x_fwd - self.x_aft


# --------------------------------------------------------------------------- #
#  The attitude
# --------------------------------------------------------------------------- #

def end_drafts(draft_m: float, trim_m: float) -> Tuple[float, float]:
    """``(draft_fwd, draft_aft)`` for a mean draft and a trim.

    ``trim = draft_fwd - draft_aft``, so a NEGATIVE trim puts the stern deeper
    and must return an aft draft LARGER than the forward one. Writing this the
    other way round leaves every integrated quantity correct and every quoted
    end draft wrong, which is why it has a test of its own.
    """
    half = 0.5 * trim_m
    return draft_m + half, draft_m - half


def trim_angle_rad(trim_m: float, reference_length_m: float) -> float:
    """Rotation angle for ``trim_m`` measured over ``reference_length_m``.

    Positive is bow-down, which follows from the convention in
    :func:`end_drafts`. ``atan`` rather than the small-angle ratio: the cost is
    nothing and it keeps the end drafts exact at the large trims a ballast
    condition carries.
    """
    if reference_length_m <= 0.0:
        raise ValueError(
            f"reference_length_m must be positive, got {reference_length_m}"
        )
    return math.atan(trim_m / reference_length_m)


def attitude_for(
    *,
    draft_m: float,
    trim_m: float,
    reference_length_m: float,
    x_aft_m: float = 0.0,
    name: str = "",
) -> Attitude:
    """The same condition as an :class:`Attitude` -- the tilted-water view.

    Provided so a condition can be handed to ``appendage_submergence`` without
    anyone re-deriving the end drafts and getting the sign wrong on the way.
    """
    fwd, aft = end_drafts(draft_m, trim_m)
    return Attitude(
        name=name,
        draft_fwd_m=fwd,
        draft_aft_m=aft,
        reference_length_m=reference_length_m,
        x_aft_m=x_aft_m,
    )


def rotate_for_trim(
    tris: Sequence[Tri], *, angle_rad: float, x_pivot: float, z_pivot: float
) -> List[Tri]:
    """Rotate ``tris`` in the x-z plane about ``(x_pivot, z_pivot)``.

    A positive angle takes the bow (+x) DOWN. The pivot point itself is fixed
    exactly, which is the invariant that makes the quoted mean draft mean
    something: put the pivot on the waterline at midship and the water plane
    still passes through it afterwards.
    """
    if angle_rad == 0.0:
        return [tuple(t) for t in tris]  # type: ignore[misc]
    cos_a, sin_a = math.cos(angle_rad), math.sin(angle_rad)
    out: List[Tri] = []
    for tri in tris:
        out.append(
            tuple(  # type: ignore[arg-type]
                (
                    x_pivot + (x - x_pivot) * cos_a + (z - z_pivot) * sin_a,
                    y,
                    z_pivot - (x - x_pivot) * sin_a + (z - z_pivot) * cos_a,
                )
                for x, y, z in tri
            )
        )
    return out


# --------------------------------------------------------------------------- #
#  The waterplane extent
# --------------------------------------------------------------------------- #

def waterplane_extent(
    tris: Sequence[Tri], waterline_z: float
) -> Tuple[float, float]:
    """``(length, beam)`` of the submerged geometry at ``waterline_z``.

    Measured on the geometry CLIPPED at the waterline: vertices at or below it,
    plus the interpolated crossing point of every straddling edge. The module
    docstring records what the unclipped version costs.
    """
    xs: List[float] = []
    ys: List[float] = []
    for tri in tris:
        for i in range(3):
            p, q = tri[i], tri[(i + 1) % 3]
            p_in, q_in = p[2] <= waterline_z, q[2] <= waterline_z
            if p_in:
                xs.append(p[0])
                ys.append(p[1])
            if p_in != q_in:
                u = (waterline_z - p[2]) / (q[2] - p[2])
                xs.append(p[0] + u * (q[0] - p[0]))
                ys.append(p[1] + u * (q[1] - p[1]))
    if not xs:
        raise ValueError(
            f"no geometry lies at or below z = {waterline_z:g}; the hull is "
            "not in the water at this draft"
        )
    return max(xs) - min(xs), max(ys) - min(ys)


# --------------------------------------------------------------------------- #
#  The result
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class FloatingCondition:
    """One (draft, trim) condition and everything integrated from it."""

    name: str
    draft_m: float
    trim_m: float
    draft_fwd_m: float
    draft_aft_m: float
    reference_length_m: float
    trim_angle_rad: float
    waterline_z: float
    lwl_m: float
    beam_wl_m: float
    wetted_surface_m2: float
    displaced_volume_m3: float
    water_density_t_m3: float = DEFAULT_WATER_DENSITY_T_M3

    @property
    def displacement_t(self) -> float:
        return self.displaced_volume_m3 * self.water_density_t_m3

    @property
    def block_coefficient(self) -> float:
        """Cb on the WATERLINE length and beam at this attitude.

        Not on Lpp and not on the moulded beam: at trim the waterline length is
        the quantity that moved, and quoting Cb on a fixed length would hide
        exactly the effect this module exists to expose.
        """
        box = self.lwl_m * self.beam_wl_m * self.draft_m
        if box <= 0.0:
            raise ValueError(
                "block coefficient is undefined: the waterplane has no extent"
            )
        return self.displaced_volume_m3 / box

    def to_dict(self) -> Dict[str, object]:
        return {
            "name": self.name,
            "draft_m": self.draft_m,
            "trim_m": self.trim_m,
            "draft_fwd_m": self.draft_fwd_m,
            "draft_aft_m": self.draft_aft_m,
            "reference_length_m": self.reference_length_m,
            "trim_angle_deg": math.degrees(self.trim_angle_rad),
            "lwl_m": self.lwl_m,
            "beam_wl_m": self.beam_wl_m,
            "wetted_surface_m2": self.wetted_surface_m2,
            "displaced_volume_m3": self.displaced_volume_m3,
            "displacement_t": self.displacement_t,
            "block_coefficient": self.block_coefficient,
        }


# --------------------------------------------------------------------------- #
#  The core, and the two sweeps over it
# --------------------------------------------------------------------------- #

def hydrostatics_at(
    tris: Sequence[Tri],
    *,
    draft_m: float,
    trim_m: float = 0.0,
    frame: Optional[HullFrame] = None,
    reference_length_m: Optional[float] = None,
    water_density_t_m3: float = DEFAULT_WATER_DENSITY_T_M3,
    name: str = "",
) -> FloatingCondition:
    """Integrate the hull at one mean draft and trim.

    ``draft_m`` is the MEAN (midship) draft, measured from the lowest point of
    the geometry. ``trim_m`` is ``draft_fwd - draft_aft``; negative is by the
    stern.

    ``reference_length_m`` is the length the trim is quoted over. It defaults to
    the hull's own longitudinal extent, which is what a client sheet quoting a
    trim in metres over the whole ship means; pass Lpp explicitly when the trim
    was quoted between perpendiculars. The two differ by the overhangs, so the
    resulting angle differs, and which was used is recorded on the result.
    ``frame`` lets a sweep read the frame once instead of once per row.
    """
    if draft_m <= 0.0:
        raise ValueError(f"draft_m must be positive, got {draft_m}")
    frame = frame or HullFrame.from_triangles(tris)
    length = frame.length_m if reference_length_m is None else reference_length_m
    angle = trim_angle_rad(trim_m, length)
    waterline = frame.keel_z + draft_m

    attitude_tris = rotate_for_trim(
        tris, angle_rad=angle, x_pivot=frame.x_midship, z_pivot=waterline
    )
    lwl, beam_wl = waterplane_extent(attitude_tris, waterline)
    fwd, aft = end_drafts(draft_m, trim_m)
    return FloatingCondition(
        name=name,
        draft_m=draft_m,
        trim_m=trim_m,
        draft_fwd_m=fwd,
        draft_aft_m=aft,
        reference_length_m=length,
        trim_angle_rad=angle,
        waterline_z=waterline,
        lwl_m=lwl,
        beam_wl_m=beam_wl,
        wetted_surface_m2=wetted_surface_area(attitude_tris, waterline),
        displaced_volume_m3=enclosed_volume(attitude_tris, waterline),
        water_density_t_m3=water_density_t_m3,
    )


def hydrostatic_table(
    tris: Sequence[Tri],
    drafts_m: Iterable[float],
    *,
    trim_m: float = 0.0,
    **options: object,
) -> List[FloatingCondition]:
    """A hydrostatic table: the same integration swept over draft.

    Deliberately a loop over :func:`hydrostatics_at` and not a second
    implementation. A table and a single point that disagree is a defect that
    only ever shows up as an argument between two reports.
    """
    frame = options.pop("frame", None) or HullFrame.from_triangles(tris)
    return [
        hydrostatics_at(
            tris, draft_m=draft, trim_m=trim_m, frame=frame, **options  # type: ignore[arg-type]
        )
        for draft in drafts_m
    ]


def condition_matrix(
    tris: Sequence[Tri],
    conditions: Sequence[Tuple[str, float, Sequence[float]]],
    **options: object,
) -> List[FloatingCondition]:
    """A named loading matrix: ``(name, mean draft, trims)`` expanded to rows.

    One row per (condition, trim) pair, in the order given, because each row is
    a distinct geometry and therefore a distinct mesh and a distinct solve.
    """
    frame = options.pop("frame", None) or HullFrame.from_triangles(tris)
    rows: List[FloatingCondition] = []
    for name, draft, trims in conditions:
        for trim in trims:
            rows.append(
                hydrostatics_at(
                    tris, draft_m=draft, trim_m=trim, frame=frame, name=name,
                    **options,  # type: ignore[arg-type]
                )
            )
    return rows
