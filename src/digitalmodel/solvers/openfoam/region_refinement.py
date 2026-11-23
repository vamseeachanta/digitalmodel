#!/usr/bin/env python3
"""
ABOUTME: Derive each appendage's snappyHexMesh refinement level from its own
bounding box, the background cell size and the number of refinement stages,
instead of leaving every region at the default ``level (0 0)``.

WHAT GOING WITHOUT THIS COSTS
-----------------------------
Nothing visible, which is the problem. An appendage that inherits the hull's
near-field cell is resolved to whatever that cell happens to be, and the hull's
cell is sized for a ship. On a real case a 6 m appendage inside a 158 m domain
came out of the mesher with 179 faces: three cells across its thinnest
dimension. It meshed, it solved, and it reported 1.8 N of viscous force against
13,921 N of pressure -- a split that is only possible when there is no resolved
boundary layer on the body at all. The run said nothing. The number arrived,
went into a report, and looked like an answer.

Three cells across is not a threshold anybody has to argue about. What DOES
need arguing about is the number that replaces it, so it is a named parameter.

THE HEURISTIC, AND WHY 14
-------------------------
A body needs enough cells across its SMALLEST dimension to carry two distinct
things: the form (the pressure field around it, which needs the section shape
to exist in the mesh at all) and the boundary layer (which lives in a fraction
of that thickness). ``DEFAULT_TARGET_CELLS_ACROSS = 14`` is a judgement, not a
derivation: it is roughly the point where a wall-function boundary layer on a
thin lifting section has a handful of cells inside it while the section still
has a recognisable nose and tail, and it is close to what hand-tuned appendage
meshes in this family of cases end up at. It is deliberately quoted as a
smallest-dimension target rather than a y+ target, because y+ depends on the
layer addition that runs afterwards and this decision has to be made before it.

Raise it for a case whose answer is the appendage's own drag; lower it for a
screening run where the appendage only needs to displace water. Either way the
number is in the call and shows up in the record, so the next reader can see
what was judged rather than inferring it from a level.

WHAT IS ASSUMED
---------------
That the region sits inside the INNERMOST refinement box, so the cell it meets
is ``base_cell_size / 2**stages``. That holds for stern and bow appendages,
which is what this is for, and it is the optimistic direction: a region outside
the innermost box meets a coarser cell than assumed and would need MORE
refinement, not less. ``cells_across_before`` is on the record so the
assumption can be checked against the mesh that comes out.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Any, Dict, List, Mapping, Optional, Sequence, Tuple

from .hull_case_regions import HULL_PATCH
from .hull_domain import REFINEMENT_STAGES

__all__ = [
    "DEFAULT_MAX_LEVEL",
    "DEFAULT_TARGET_CELLS_ACROSS",
    "RegionRefinement",
    "derive_region_levels",
    "finest_in_plane_cell",
    "near_hull_cell_size",
    "refinement_for_bbox",
    "refinement_for_extent",
]

Vec3 = Tuple[float, float, float]

#: Cells across the region's smallest dimension the derivation aims for. A
#: JUDGEMENT -- see the module docstring for what it is trying to buy and when
#: to move it. Passed through to every entry point as a parameter so that
#: changing the judgement is a change at the call site, not an edit here.
DEFAULT_TARGET_CELLS_ACROSS = 14

#: Levels are cumulative on top of the hull's own staging and each one is 8x
#: the cells in the box it covers, so an unbounded derivation applied to a
#: small feature produces a mesh nobody can afford and no warning. The cap
#: turns that into a recorded, inspectable compromise.
DEFAULT_MAX_LEVEL = 5


def near_hull_cell_size(
    base_cell_size_m: float, stages: int = REFINEMENT_STAGES
) -> float:
    """Cell size inside the innermost refinement box.

    Each stage halves the cell in every direction, so ``stages`` of them divide
    the background cell by ``2**stages``.
    """
    if base_cell_size_m <= 0.0:
        raise ValueError(
            f"base_cell_size_m must be positive, got {base_cell_size_m}"
        )
    if stages < 0:
        raise ValueError(f"stages must be >= 0, got {stages}")
    return base_cell_size_m / 2**stages


def finest_in_plane_cell(
    domain: Any, divisions: Mapping[str, int], stages: int = REFINEMENT_STAGES
) -> float:
    """The in-plane cell the EMITTED mesh carries inside the innermost box.

    Distinct from :func:`near_hull_cell_size`, which divides the requested
    ``base_cell_size``. blockMesh takes an integer count, so the cell the case
    actually gets is ``extent / round(extent / base)`` and differs from the
    request by a few per cent. The post-mesh gate (#2033) compares a measured
    face area against this number, so it has to be the one on the mesh and not
    the one that was asked for.

    The LARGER of the two in-plane cells: a face is bounded by the coarser of
    the directions spanning it.
    """
    if stages < 0:
        raise ValueError(f"stages must be >= 0, got {stages}")
    nx, ny = int(divisions["nx"]), int(divisions["ny"])
    if nx < 1 or ny < 1:
        raise ValueError(f"block divisions must be positive, got nx={nx}, ny={ny}")
    return max(domain.length / nx, domain.width / ny) / 2**stages


@dataclass(frozen=True)
class RegionRefinement:
    """One region's derived level, with the working that produced it."""

    name: str
    extent_m: Vec3
    min_extent_m: float
    base_cell_size_m: float
    stages: int
    near_cell_size_m: float
    cells_across_before: float
    target_cells_across: int
    level_required: int
    level: int
    max_level: int

    @property
    def clamped(self) -> bool:
        return self.level_required > self.level

    @property
    def cells_across_after(self) -> float:
        """What the mesh will ACTUALLY deliver, clamp included."""
        return self.cells_across_before * 2**self.level

    @property
    def refinement_level(self) -> Tuple[int, int]:
        """``(min, max)`` in the form ``SurfaceRegion`` takes.

        Both ends equal: a band would let the mesher settle anywhere inside it,
        and the whole point of deriving a number is that the answer is not left
        to the mesher's discretion.
        """
        return self.level, self.level

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "extent_m": list(self.extent_m),
            "min_extent_m": self.min_extent_m,
            "base_cell_size_m": self.base_cell_size_m,
            "stages": self.stages,
            "near_cell_size_m": self.near_cell_size_m,
            "cells_across_before": self.cells_across_before,
            "target_cells_across": self.target_cells_across,
            "level_required": self.level_required,
            "level": self.level,
            "max_level": self.max_level,
            "cells_across_after": self.cells_across_after,
            "clamped": self.clamped,
        }


def refinement_for_extent(
    name: str,
    extent_m: Sequence[float],
    *,
    base_cell_size_m: float,
    stages: int = REFINEMENT_STAGES,
    target_cells_across: int = DEFAULT_TARGET_CELLS_ACROSS,
    max_level: int = DEFAULT_MAX_LEVEL,
) -> RegionRefinement:
    """Levels needed to put ``target_cells_across`` cells across the thinnest
    dimension of a region whose bounding box measures ``extent_m``.

    The SMALLEST dimension, not the largest and not the mean: a fin is a metre
    thick and eight metres tall, and it is the metre that decides whether there
    is a boundary layer on it.
    """
    if len(extent_m) != 3:
        raise ValueError(f"extent_m must have three components, got {extent_m}")
    if target_cells_across < 1:
        raise ValueError(
            f"target_cells_across must be >= 1, got {target_cells_across}"
        )
    if max_level < 0:
        raise ValueError(f"max_level must be >= 0, got {max_level}")
    smallest = min(float(e) for e in extent_m)
    if smallest <= 0.0:
        raise ValueError(
            f"region {name!r} has a bounding box with a non-positive "
            f"dimension {tuple(extent_m)}; it is not a solid body"
        )

    near = near_hull_cell_size(base_cell_size_m, stages)
    across = smallest / near
    required = max(0, math.ceil(math.log2(target_cells_across / across)))
    return RegionRefinement(
        name=name,
        extent_m=(float(extent_m[0]), float(extent_m[1]), float(extent_m[2])),
        min_extent_m=smallest,
        base_cell_size_m=base_cell_size_m,
        stages=stages,
        near_cell_size_m=near,
        cells_across_before=across,
        target_cells_across=target_cells_across,
        level_required=required,
        level=min(required, max_level),
        max_level=max_level,
    )


def refinement_for_bbox(
    name: str,
    bbox_min_m: Sequence[float],
    bbox_max_m: Sequence[float],
    **options: Any,
) -> RegionRefinement:
    """:func:`refinement_for_extent` from the corners a manifest records."""
    if len(bbox_min_m) != 3 or len(bbox_max_m) != 3:
        raise ValueError(
            f"region {name!r} bbox corners must have three components"
        )
    extent = tuple(
        float(bbox_max_m[i]) - float(bbox_min_m[i]) for i in range(3)
    )
    return refinement_for_extent(name, extent, **options)


def derive_region_levels(
    regions: Sequence[Mapping[str, Any]],
    *,
    base_cell_size_m: float,
    stages: int = REFINEMENT_STAGES,
    target_cells_across: int = DEFAULT_TARGET_CELLS_ACROSS,
    max_level: int = DEFAULT_MAX_LEVEL,
    hull_patch: str = HULL_PATCH,
) -> List[RegionRefinement]:
    """Derive a level for every appendage region a hull manifest records.

    The hull itself is SKIPPED by this derivation, which is written against a
    smallest-dimension target and would score the largest body in the domain
    against a number meant for the smallest. It is NOT left unrefined: the
    hull carries ``hull_case_regions.HULL_REFINEMENT_LEVEL`` as a floor.
    "Its resolution is already set by the staged refinement boxes" is what
    this docstring used to say, and #2033 is what that assumption cost -- the
    staging was mis-placed and there was no floor under it.

    A region with no bounding box on record is skipped rather than guessed at;
    a level derived from an assumed size is exactly the guess this module
    exists to remove.
    """
    derived: List[RegionRefinement] = []
    for region in regions:
        name = str(region.get("name", ""))
        role = region.get("role")
        if role == "hull" or (role is None and name == hull_patch):
            continue
        lo: Optional[Sequence[float]] = region.get("bbox_min_m")
        hi: Optional[Sequence[float]] = region.get("bbox_max_m")
        if lo is None or hi is None:
            continue
        derived.append(
            refinement_for_bbox(
                name or "region", lo, hi,
                base_cell_size_m=base_cell_size_m,
                stages=stages,
                target_cells_across=target_cells_across,
                max_level=max_level,
            )
        )
    return derived
