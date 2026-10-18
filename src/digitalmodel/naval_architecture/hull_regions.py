#!/usr/bin/env python3
"""
ABOUTME: Multi-region hull ingestion (#2023): appendages that INTERPENETRATE
the hull are carried to the mesher as separate closed STL regions, and this
module places them on the hull's frame, gates each on its own closure, and
emits one STL per region. Measuring the set is ``region_report``.

WHY SEPARATE REGIONS AND NOT ONE SURFACE
----------------------------------------
A client rudder and propeller boss arrive as their own closed bodies that
overlap the hull. Concatenating the three triangle soups is not a union; it is
a soup with hundreds of NON-MANIFOLD edges where one body's skin passes through
another's, and the whole ingestion lane exists to refuse exactly that.

The boolean union that WOULD produce one clean surface needs a CAD kernel
operation on tessellated geometry -- slow, fragile on near-tangential
intersections, and capable of returning a plausible surface that has quietly
lost a fillet. snappyHexMesh does not need it. It performs a per-surface
inside/outside test and forms the union implicitly, provided that

  * every surface is INDIVIDUALLY closed, and
  * ``locationInMesh`` lies outside all of them.

So the design is: keep them separate, gate each one on its own closure, and
verify the two premises above rather than assume them.

WHAT THE HULL'S CAP HAS TO DO WITH THIS
---------------------------------------
Nothing, and that is the point. ``hull_cap`` closes a hull surface that stops
at deck level. The appendages are already closed, so capping them would either
be a no-op or would add a lid across a real opening that is not there. Capping
is a HULL step and is applied to the hull region only.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import (
    TYPE_CHECKING,
    Dict,
    List,
    Mapping,
    Optional,
    Sequence,
    Tuple,
)

from digitalmodel.naval_architecture.kcs_geometry import (
    SurfaceCheck,
    check_surface,
    orient_consistently,
    write_stl,
)
from digitalmodel.naval_architecture.region_report import region_set
from digitalmodel.naval_architecture.solid_occlusion import (
    DEFAULT_SUBDIVISION_DEPTH,
)

if TYPE_CHECKING:  # pragma: no cover - annotations only
    from digitalmodel.naval_architecture.hull_ingest import HullTransform

__all__ = [
    "APPENDAGE",
    "HULL",
    "PreparedRegion",
    "ingest_appendages",
    "prepare_appendage",
    "region_set",
    "write_region_stls",
]

Vec3 = Tuple[float, float, float]
Tri = Tuple[Vec3, Vec3, Vec3]

HULL = "hull"
APPENDAGE = "appendage"


@dataclass(frozen=True)
class PreparedRegion:
    """One closed body, already in the hull's placed frame."""

    name: str
    role: str
    triangles: Tuple[Tri, ...]
    check: SurfaceCheck
    stl_file: str = ""

    @property
    def watertight(self) -> bool:
        return self.check.closed

    @property
    def bounds(self) -> Tuple[Vec3, Vec3]:
        return _bounds(self.triangles)


def prepare_appendage(
    name: str,
    triangles: Sequence[Tri],
    *,
    scale_to_m: float,
    forward: str,
    offset: Vec3,
    weld_tolerance: float,
    force: bool = False,
) -> PreparedRegion:
    """Place ONE appendage in the hull's frame and gate it on closure.

    The placement is the hull's, not its own. An appendage inferred
    independently would be scaled by its own longest dimension, rotated onto
    its own principal axis and translated onto its own keel -- three
    inferences that are individually reasonable and jointly put a rudder
    somewhere amidships. Only the hull's decision is used, which is why this
    takes the components rather than re-deriving them.

    ``force`` mirrors the hull gate: an appendage that is not closed is
    refused, because snappyHexMesh's inside/outside test on an open surface
    silently keeps the interior.
    """
    # Imported here, not at module scope: ``hull_ingest`` imports this module,
    # so a top-level import would close the cycle.
    from digitalmodel.naval_architecture import hull_ingest  # noqa: PLC0415

    tris = [tuple(tri) for tri in triangles]  # type: ignore[misc]
    if not tris:
        raise hull_ingest.HullIngestError(f"appendage {name!r} has no triangles")
    if scale_to_m != 1.0:
        tris = [
            tuple(tuple(c * scale_to_m for c in p) for p in tri) for tri in tris
        ]  # type: ignore[misc]
    tris = hull_ingest.weld_vertices(tris, weld_tolerance)
    tris = hull_ingest._rotate_to_ship_axes(tris, forward)
    tris = [tri for tri in tris if _area(*tri) > 0.0]
    if not tris:
        raise hull_ingest.HullIngestError(
            f"every triangle of appendage {name!r} was degenerate after welding"
        )

    check = check_surface(tris)
    if not check.closed and not force:
        raise hull_ingest.NotWatertightError(
            f"appendage {name!r} is not closed: {check.open_edge_count} open "
            f"edges and {check.nonmanifold_edge_count} non-manifold edges "
            f"across {check.triangle_count} triangles (weld tolerance "
            f"{weld_tolerance:g} m).\n"
            "Appendages are handed to snappyHexMesh as SEPARATE closed "
            "surfaces and the mesher decides what is solid from a per-surface "
            "inside/outside test; on an open surface that test keeps the "
            "interior and the appendage becomes a fluid-filled shell.\n"
            "Options: raise weld_tolerance= if the gaps are numerical; repair "
            "the body in CAD; or drop the layer from the ingest."
        )
    if check.closed:
        tris = orient_consistently(tris)
    tris = [
        tuple(
            (p[0] + offset[0], p[1] + offset[1], p[2] + offset[2]) for p in tri
        )
        for tri in tris
    ]  # type: ignore[misc]
    return PreparedRegion(
        name=name, role=APPENDAGE, triangles=tuple(tris), check=check
    )


def write_region_stls(
    regions: Sequence[PreparedRegion], out_dir: Path | str
) -> Tuple[PreparedRegion, ...]:
    """Emit one STL per region and return the regions with their file names.

    The hull region is NOT written here; ``ingest_triangles`` already wrote it
    under the caller's chosen ``stl_name`` and re-writing it would give two
    files a chance to differ.
    """
    out = Path(out_dir)
    out.mkdir(parents=True, exist_ok=True)
    placed: List[PreparedRegion] = []
    for region in regions:
        if region.role == HULL:
            placed.append(region)
            continue
        path = write_stl(
            list(region.triangles), out / f"{region.name}.stl", name=region.name
        )
        placed.append(
            PreparedRegion(
                name=region.name,
                role=region.role,
                triangles=region.triangles,
                check=region.check,
                stl_file=path.name,
            )
        )
    return tuple(placed)


def ingest_appendages(
    appendages: Mapping[str, Sequence[Tri]],
    *,
    out_dir: Path | str,
    transform: "HullTransform",
    hull_triangles: Sequence[Tri],
    hull_name: str,
    hull_stl_file: str,
    hull_check: SurfaceCheck,
    waterline_z: float,
    weld_tolerance: float,
    force: bool = False,
    subdivision_depth: Optional[int] = None,
) -> Dict[str, object]:
    """Place, gate, emit and measure every appendage. Returns the manifest block.

    Called from ``hull_ingest.ingest_triangles`` once the hull is placed, so
    the hull triangles handed in are already in their final frame and the
    hull's own occlusion by the appendages is measured on the same geometry
    the STL carries.
    """
    depth = (
        DEFAULT_SUBDIVISION_DEPTH if subdivision_depth is None
        else int(subdivision_depth)
    )
    prepared: List[PreparedRegion] = [
        PreparedRegion(
            name=hull_name,
            role=HULL,
            triangles=tuple(hull_triangles),
            check=hull_check,
            stl_file=hull_stl_file,
        )
    ]
    for name in sorted(appendages):
        if name == hull_name:
            raise ValueError(
                f"appendage {name!r} collides with the hull region name; the "
                "two would write the same STL and the second would win"
            )
        prepared.append(
            prepare_appendage(
                name,
                appendages[name],
                scale_to_m=transform.scale_to_m,
                forward=transform.forward,
                offset=transform.offset,
                weld_tolerance=weld_tolerance,
                force=force,
            )
        )
    placed = write_region_stls(prepared, out_dir)
    return region_set(placed, waterline_z, subdivision_depth=depth).to_dict()


def _bounds(tris: Sequence[Tri]) -> Tuple[Vec3, Vec3]:
    xs = [p[0] for t in tris for p in t]
    ys = [p[1] for t in tris for p in t]
    zs = [p[2] for t in tris for p in t]
    return (min(xs), min(ys), min(zs)), (max(xs), max(ys), max(zs))


def _area(a: Vec3, b: Vec3, c: Vec3) -> float:
    ux, uy, uz = b[0] - a[0], b[1] - a[1], b[2] - a[2]
    vx, vy, vz = c[0] - a[0], c[1] - a[1], c[2] - a[2]
    nx, ny, nz = uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx
    return 0.5 * (nx * nx + ny * ny + nz * nz) ** 0.5
