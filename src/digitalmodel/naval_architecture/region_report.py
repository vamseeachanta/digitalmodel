#!/usr/bin/env python3
"""
ABOUTME: Measuring a placed set of hull regions (#2023) into the ``regions``
block of ``hull_manifest.json``: per-region wetted areas split by occlusion,
per-region centreplane cuts, and the union figure that ``Aref`` is built from.

Separate from ``hull_regions``, which PREPARES and emits the regions, for the
same reason ``hull_case_dicts`` is separate from ``hull_case``: doing and
describing are different jobs, and the description is the part every downstream
consumer reads.

THE ONE NUMBER THIS MODULE EXISTS FOR
-------------------------------------
``wetted_surface_external_m2``. Summing the per-region wetted areas is the
obvious thing and it is wrong: an appendage interpenetrates the hull, so part
of each lies inside the other and is wetted by nothing. The sum is an UPPER
BOUND. It is also, if used, the denominator of every reported force
coefficient -- which is why the naive figure is published under a name that
says what it is, beside an estimate that carries its own error bar.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Dict, List, Mapping, Sequence, Tuple

from digitalmodel.naval_architecture.kcs_geometry import (
    check_surface,
    enclosed_volume,
)
from digitalmodel.naval_architecture.solid_occlusion import (
    DEFAULT_SUBDIVISION_DEPTH,
    AreaSplit,
    SolidIndex,
    classify_wetted_area,
)
from digitalmodel.naval_architecture.symmetry_cut import plane_section

if TYPE_CHECKING:  # pragma: no cover - annotations only
    from digitalmodel.naval_architecture.hull_regions import PreparedRegion

__all__ = ["ACCOUNTING_NOTE", "RegionSet", "region_set"]

Vec3 = Tuple[float, float, float]

#: Stated in the manifest, not only in this file: the reader of the JSON is the
#: one who has to decide whether the number is fit for their purpose.
ACCOUNTING_NOTE = (
    "wetted_surface_naive_sum_m2 is the sum of the per-region wetted areas and "
    "is an UPPER BOUND, not the union's area: the part of an appendage inside "
    "the hull is not wetted, and neither is the hull under the appendage. "
    "wetted_surface_external_m2 classifies every wetted triangle as outside or "
    "inside the other regions, subdividing the ones that straddle a boundary; "
    "wetted_surface_external_uncertainty_m2 is the area still straddling at the "
    "finest subdivision, and bounds the error. Per-region areas are reported "
    "separately so neither figure has to be taken on trust."
)


@dataclass(frozen=True)
class RegionSet:
    """Every region, measured against every other one."""

    regions: Tuple["PreparedRegion", ...]
    waterline_z: float
    splits: Mapping[str, AreaSplit]
    volumes: Mapping[str, float]
    sections: Mapping[str, object]
    merged_nonmanifold_edges: int
    subdivision_depth: int
    outside_hull_bbox: Mapping[str, List[str]] = field(default_factory=dict)
    notes: Tuple[str, ...] = ()

    @property
    def naive_sum_m2(self) -> float:
        """What summing the per-region wetted areas would report."""
        return sum(split.total_m2 for split in self.splits.values())

    @property
    def external_m2(self) -> float:
        """The estimate of the union's true external wetted area."""
        return sum(split.external_m2 for split in self.splits.values())

    @property
    def uncertainty_m2(self) -> float:
        return sum(split.undecided_m2 for split in self.splits.values())

    @property
    def double_counted_m2(self) -> float:
        return self.naive_sum_m2 - self.external_m2

    def to_dict(self) -> Dict[str, object]:
        return {
            "n_regions": len(self.regions),
            "waterline_z_m": self.waterline_z,
            "regions": [self._region_dict(r) for r in self.regions],
            "union": self._union_dict(),
        }

    def _region_dict(self, region: "PreparedRegion") -> Dict[str, object]:
        lo, hi = region.bounds
        split = self.splits[region.name]
        return {
            "name": region.name,
            "role": region.role,
            "stl_file": region.stl_file,
            "n_triangles": len(region.triangles),
            "watertight": region.watertight,
            "open_edge_count": region.check.open_edge_count,
            "nonmanifold_edge_count": region.check.nonmanifold_edge_count,
            "bbox_min_m": list(lo),
            "bbox_max_m": list(hi),
            "displaced_volume_m3": self.volumes[region.name],
            "outside_hull_bbox": self.outside_hull_bbox.get(region.name, []),
            "centreplane_section": self.sections[region.name],
            **split.to_dict(),
        }

    def _union_dict(self) -> Dict[str, object]:
        return {
            "wetted_surface_naive_sum_m2": self.naive_sum_m2,
            "wetted_surface_external_m2": self.external_m2,
            "wetted_surface_external_uncertainty_m2": self.uncertainty_m2,
            "double_counted_m2": self.double_counted_m2,
            "merged_nonmanifold_edge_count": self.merged_nonmanifold_edges,
            "subdivision_depth": self.subdivision_depth,
            "accounting": ACCOUNTING_NOTE,
            "notes": list(self.notes),
        }


def region_set(
    regions: Sequence["PreparedRegion"],
    waterline_z: float,
    *,
    subdivision_depth: int = DEFAULT_SUBDIVISION_DEPTH,
    centreplane_y: float = 0.0,
) -> RegionSet:
    """Measure a placed set of regions: areas, volumes and centreplane cuts."""
    indices = {
        region.name: SolidIndex(region.triangles)
        for region in regions
        if region.watertight
    }
    splits: Dict[str, AreaSplit] = {}
    volumes: Dict[str, float] = {}
    sections: Dict[str, object] = {}
    for region in regions:
        others = [
            index for name, index in indices.items() if name != region.name
        ]
        splits[region.name] = classify_wetted_area(
            region.triangles, waterline_z, others, max_depth=subdivision_depth
        )
        volumes[region.name] = (
            enclosed_volume(region.triangles, waterline_z)
            if region.watertight
            else 0.0
        )
        sections[region.name] = plane_section(
            region.triangles, centreplane_y, axis=1
        ).to_dict()

    outside = _outside_hull_bbox(regions)
    merged = _merged_nonmanifold_edges(regions)
    return RegionSet(
        regions=tuple(regions),
        waterline_z=waterline_z,
        splits=splits,
        volumes=volumes,
        sections=sections,
        merged_nonmanifold_edges=merged,
        subdivision_depth=subdivision_depth,
        outside_hull_bbox=outside,
        notes=tuple(_notes(regions, outside, sections, merged)),
    )


def _merged_nonmanifold_edges(regions: Sequence["PreparedRegion"]) -> int:
    """What concatenating the regions into one soup WOULD cost.

    Measured rather than asserted. It is the evidence for the whole design:
    on the client hull it is 520, on two boxes that pass through each other
    without sharing a vertex it is 0. Reporting the number lets a reader see
    which case they have instead of taking the choice on trust.
    """
    if len(regions) < 2:
        return 0
    soup = [tri for region in regions for tri in region.triangles]
    return check_surface(soup).nonmanifold_edge_count


def _notes(
    regions: Sequence["PreparedRegion"],
    outside: Mapping[str, List[str]],
    sections: Mapping[str, object],
    merged: int,
) -> List[str]:
    notes: List[str] = []
    for name, axes in sorted(outside.items()):
        # The domain and every refinement box downstream are sized from the
        # HULL's bounding box, because that is the only box the manifest
        # publishes. An appendage reaching beyond it is meshed at background
        # resolution where it protrudes -- the mesher does not complain, and
        # the shortfall shows up only as a force that is slightly wrong for a
        # reason nobody can see.
        notes.append(
            f"region {name!r} reaches OUTSIDE the hull bounding box on "
            f"{axes}; the refinement boxes are derived from the hull box and "
            "will not cover it there"
        )
    for name in sorted(sections):
        section = sections[name]
        if not section.get("cut_is_clean", True):  # type: ignore[union-attr]
            notes.append(
                f"region {name!r} is NOT cut cleanly by the centreplane: the "
                "half the domain keeps is not a closed solid, so "
                "snappyHexMesh cannot tell its inside from its outside there"
            )
    if merged:
        notes.append(
            f"merging these {len(regions)} regions into one triangle soup "
            f"would produce {merged} non-manifold edges, which is why they "
            "are emitted and meshed separately"
        )
    return notes


def _outside_hull_bbox(
    regions: Sequence["PreparedRegion"], *, tolerance: float = 1e-9
) -> Dict[str, List[str]]:
    """Which appendages leave the hull's box, and on which axes."""
    hulls = [r for r in regions if r.role == "hull"]
    if not hulls:
        return {}
    lo, hi = hulls[0].bounds
    out: Dict[str, List[str]] = {}
    for region in regions:
        if region.role == "hull":
            continue
        r_lo, r_hi = region.bounds
        axes = [
            f"{'xyz'[k]}{sign}"
            for k in range(3)
            for sign, beyond in (
                ("-", r_lo[k] < lo[k] - tolerance),
                ("+", r_hi[k] > hi[k] + tolerance),
            )
            if beyond
        ]
        if axes:
            out[region.name] = axes
    return out
