"""
ABOUTME: Client hull geometry ingestion for CFD (#2023). Converts a Rhino
``.3dm`` hull into a normalised, watertight, OpenFOAM-ready STL plus a
``hull_manifest.json`` describing exactly what was done to it.

WHY THIS STAGE EXISTS
---------------------
The repository can already solve ship resistance for a benchmark hull whose
geometry it generates itself. A CLIENT hull arrives instead as a CAD file in
unknown units, in an unknown orientation, about an unknown origin, and is
frequently not closed. Every one of those unknowns has a failure mode that is
silent: the case meshes, the solver converges, and the answer is wrong.

    unit wrong        -> hull is the wrong SIZE -> wrong Reynolds number
    direction wrong   -> hull is towed stern-first
    origin wrong      -> refinement boxes land somewhere else
    surface leaky     -> snappyHexMesh meshes it anyway

So this module infers each of those, STATES the inference in the manifest,
lets the caller override it, and refuses to proceed when the evidence is
ambiguous. Failing loudly is the entire point: a plausible wrong answer is
more expensive than a stopped pipeline.

WHAT rhino3dm CAN AND CANNOT DO
-------------------------------
``rhino3dm`` is a file-I/O library, NOT a geometry kernel. It cannot tessellate
a NURBS surface. What it CAN do is hand back the RENDER MESH that Rhino cached
in the file when the object was last shaded.

That distinction decides the whole reader. A ``.3dm`` full of trimmed NURBS
Breps with no cached render meshes cannot be ingested here, and the honest
response is to say so and name the fix (re-save from Rhino with render meshes,
or export a tessellated format). The alternative -- grid-evaluating each
trimmed surface over its full parameter domain -- ignores the trim curves and
produces overlapping patches that are guaranteed not to be watertight. That
would replace a clear failure with exactly the kind of confident wrong answer
this module exists to prevent.

Note also that render-mesh vertices are stored as **float32**. Adjacent patches
therefore meet at coordinates differing in the last bits, and the raw soup
reads as leaky. Welding to a tolerance is mandatory, not cosmetic -- see
``weld_vertices``.

The geometry primitives (closure counting, hydrostatics, STL writing,
orientation repair) are reused from ``kcs_geometry`` rather than reimplemented.
"""

from __future__ import annotations

import hashlib
import json
import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence, Tuple

from digitalmodel.naval_architecture.kcs_geometry import (
    check_surface,
    enclosed_volume,
    orient_consistently,
    wetted_surface_area,
    write_stl,
)

Vec3 = Tuple[float, float, float]
Tri = Tuple[Vec3, Vec3, Vec3]

__all__ = [
    "CAD_EXTRA",
    "DEFAULT_WELD_TOLERANCE_M",
    "MANIFEST_CONTRACT_KEYS",
    "NO_MESH_REMEDIES",
    "PLAUSIBLE_LENGTH_M",
    "RHINO_UNIT_CODES",
    "UNIT_SCALE_TO_M",
    "AmbiguousOrientationError",
    "AmbiguousUnitsError",
    "HullIngestError",
    "HullManifest",
    "ImplausibleScaleError",
    "MissingCadDependencyError",
    "NoMeshGeometryError",
    "OrientationDecision",
    "Rhino3dmGeometry",
    "UnitDecision",
    "NotWatertightError",
    "infer_orientation",
    "infer_units",
    "ingest_3dm",
    "ingest_triangles",
    "read_3dm_triangles",
    "weld_vertices",
]

#: Name of the optional dependency group that carries the CAD readers.
CAD_EXTRA = "cad"

#: Exact conversions to metres. ``in`` and ``ft`` are exact by definition of
#: the international inch (25.4 mm), so none of these is an approximation.
UNIT_SCALE_TO_M: Dict[str, float] = {
    "mm": 0.001,
    "cm": 0.01,
    "m": 1.0,
    "in": 0.0254,
    "ft": 0.3048,
}

#: Rhino's ``UnitSystem`` enumeration, restricted to the values a hull is
#: plausibly modelled in. Anything else is treated as "the file did not say".
RHINO_UNIT_CODES: Dict[int, str] = {
    2: "mm",
    3: "cm",
    4: "m",
    8: "in",
    9: "ft",
}

#: The window a ship hull's overall length must fall in, in metres. The lower
#: bound admits towing-tank models; the upper bound sits above the largest ship
#: ever built. Used ONLY to reject readings, never to choose between two that
#: both fit -- that case raises instead.
PLAUSIBLE_LENGTH_M: Tuple[float, float] = (2.0, 500.0)

#: Vertex weld tolerance in metres. 0.1 mm is far above the float32 spacing of
#: a render mesh at ship scale (about 0.03 mm at 300 m) and far below any gap
#: that represents a real hole in the surface.
DEFAULT_WELD_TOLERANCE_M = 1e-4

#: What to tell a caller whose ``.3dm`` carries no cached render meshes. Kept
#: as a constant because it is the first wall most client files hit, and the
#: list has to stay ordered by how little it costs the client: re-saving from
#: Rhino or exporting an STL takes them two minutes and involves no
#: approximation, whereas rebuilding the trimmed Breps in a CAD kernel is our
#: approximation to defend. That ordering is the point, so it is not a docstring
#: that can drift away from the code.
NO_MESH_REMEDIES = (
    "Fixes, in order of preference:\n"
    "  1. In Rhino, shade the hull (so render meshes are built) and re-save, "
    "with 'Save geometry only' OFF.\n"
    "  2. Export the hull as STL/OBJ and ingest that instead.\n"
    "  3. Check the layer filter: the hull may be on a layer this call "
    "excluded.\n"
    "  4. Pass tessellate_breps=True to rebuild the trimmed NURBS in the "
    "OpenCASCADE kernel (needs the 'cad' extra). This is opt-in because it "
    "approximates the surface and its trim fidelity has to be inspected, not "
    "assumed."
)

#: The manifest keys another lane consumes to size its computational domain.
#: Renaming any of these is a breaking change to that interface.
MANIFEST_CONTRACT_KEYS: Tuple[str, ...] = (
    "source_file",
    "source_sha256",
    "units_in",
    "scale_to_m",
    "orientation",
    "origin",
    "lpp_m",
    "beam_m",
    "draft_m",
    "wetted_surface_m2",
    "displacement_m3",
    "watertight",
    "n_triangles",
    "bbox_min_m",
    "bbox_max_m",
)

#: The orientation the emitted STL is normalised TO. Constant by construction:
#: the stage rotates every hull into it, so a consumer never has to branch.
OUTPUT_ORIENTATION: Dict[str, str] = {"x": "forward", "y": "port", "z": "up"}

#: The origin the emitted STL is normalised TO.
OUTPUT_ORIGIN = "aft_perpendicular_keel"

_AXIS_NAMES = ("x", "y", "z")


# --------------------------------------------------------------------------- #
#  Errors - each names the action that resolves it
# --------------------------------------------------------------------------- #

class HullIngestError(Exception):
    """Base class for every failure this stage raises deliberately."""


class MissingCadDependencyError(HullIngestError):
    """The optional CAD reader is not installed."""


class NoMeshGeometryError(HullIngestError):
    """The file holds no tessellated geometry this reader can use."""


class AmbiguousUnitsError(HullIngestError):
    """More than one unit reading gives a plausible hull."""


class ImplausibleScaleError(HullIngestError):
    """No unit reading, or the stated one, gives a plausible hull."""


class AmbiguousOrientationError(HullIngestError):
    """The hull is too symmetric for its forward direction to be derived."""


class NotWatertightError(HullIngestError):
    """The surface is not closed and the caller did not force the issue."""


def _raise_missing_cad(exc: Optional[BaseException] = None) -> None:
    raise MissingCadDependencyError(
        "Reading Rhino .3dm geometry requires the optional 'rhino3dm' package, "
        "which is not installed.\n"
        f"    pip install 'digitalmodel[{CAD_EXTRA}]'\n"
        f"    uv pip install rhino3dm\n"
        "Everything else in digitalmodel works without it; only .3dm ingestion "
        "needs this extra."
    ) from exc


def _import_rhino3dm():
    """Import ``rhino3dm`` lazily.

    Kept inside a function on purpose. A module-level import would make the
    CAD reader a hard runtime dependency of the whole package, which is both
    wrong for users who never touch CAD and a violation of the dependency
    contract enforced by ``tests/contracts/test_runtime_dependencies.py``.
    """
    try:
        import rhino3dm  # noqa: PLC0415  (deliberately lazy - see docstring)
    except ImportError as exc:  # pragma: no cover - exercised via monkeypatch
        _raise_missing_cad(exc)
    return rhino3dm


# --------------------------------------------------------------------------- #
#  Units
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class UnitDecision:
    """How the source units were settled, and on what evidence."""

    units: str
    scale_to_m: float
    source: str          # caller | file_header | inferred_extent
    note: str = ""


def _plausible(length_m: float) -> bool:
    low, high = PLAUSIBLE_LENGTH_M
    return low <= length_m <= high


def infer_units(
    longest_extent: float,
    *,
    declared: Optional[str] = None,
    override: Optional[str] = None,
    force: bool = False,
) -> UnitDecision:
    """Settle the source units, preferring evidence over guesswork.

    Precedence is caller, then the file's own header, then the bounding box.
    The bounding box is the weakest evidence and is used only when it points
    at exactly ONE answer: a hull measuring 180 units is 180 m or 54.9 m
    depending on whether the file is in metres or feet, and both are ordinary
    ships. Picking the likelier one there would be a coin flip whose loss is a
    Reynolds number wrong by a factor of three, so that case raises.

    ``force`` skips the plausibility gate for a caller who genuinely has a
    model-scale or otherwise unusual hull.
    """
    if override is not None:
        chosen, source = override, "caller"
    elif declared is not None:
        chosen, source = declared, "file_header"
    else:
        chosen, source = None, "inferred_extent"

    if chosen is not None:
        if chosen not in UNIT_SCALE_TO_M:
            raise HullIngestError(
                f"unknown unit {chosen!r}; expected one of "
                f"{sorted(UNIT_SCALE_TO_M)}"
            )
        scale = UNIT_SCALE_TO_M[chosen]
        length_m = longest_extent * scale
        if not force and not _plausible(length_m):
            raise ImplausibleScaleError(
                f"units {chosen!r} ({source}) make the longest extent "
                f"{longest_extent:g} read as {length_m:g} m, outside the "
                f"plausible hull range {PLAUSIBLE_LENGTH_M[0]:g}-"
                f"{PLAUSIBLE_LENGTH_M[1]:g} m. The file's declared unit is "
                "often wrong; pass units_in= explicitly, or force=True if the "
                "hull really is this size."
            )
        return UnitDecision(chosen, scale, source, f"longest extent {length_m:g} m")

    candidates = [
        unit
        for unit, scale in UNIT_SCALE_TO_M.items()
        if _plausible(longest_extent * scale)
    ]
    if not candidates:
        raise ImplausibleScaleError(
            f"no unit reading makes a longest extent of {longest_extent:g} a "
            f"plausible hull ({PLAUSIBLE_LENGTH_M[0]:g}-"
            f"{PLAUSIBLE_LENGTH_M[1]:g} m). Pass units_in= explicitly, or "
            "force=True."
        )
    if len(candidates) > 1:
        readings = ", ".join(
            f"{u} -> {longest_extent * UNIT_SCALE_TO_M[u]:.4g} m"
            for u in candidates
        )
        raise AmbiguousUnitsError(
            f"a longest extent of {longest_extent:g} is a plausible hull in "
            f"more than one unit ({readings}). The file did not declare its "
            "units, so this cannot be settled from the geometry. Pass "
            "units_in= explicitly."
        )
    unit = candidates[0]
    return UnitDecision(
        unit,
        UNIT_SCALE_TO_M[unit],
        "inferred_extent",
        f"only plausible reading; longest extent "
        f"{longest_extent * UNIT_SCALE_TO_M[unit]:g} m",
    )


# --------------------------------------------------------------------------- #
#  Orientation
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class OrientationDecision:
    """Which way the hull points in its SOURCE coordinates."""

    forward: str             # "+x" | "-x" | "+y" | "-y"
    source: str              # caller | inferred_taper
    longitudinal_axis: str = ""
    taper_ratio: float = float("nan")
    note: str = ""


def _bounds(tris: Sequence[Tri]) -> Tuple[Vec3, Vec3]:
    if not tris:
        raise HullIngestError("no triangles: nothing to measure")
    lo = [math.inf] * 3
    hi = [-math.inf] * 3
    for tri in tris:
        for p in tri:
            for k in range(3):
                if p[k] < lo[k]:
                    lo[k] = p[k]
                if p[k] > hi[k]:
                    hi[k] = p[k]
    return (lo[0], lo[1], lo[2]), (hi[0], hi[1], hi[2])


#: Above this ratio of end half-beams the hull is treated as symmetric and its
#: forward direction is refused rather than guessed.
_TAPER_AMBIGUITY_RATIO = 0.9

#: Fractions of the length, measured in from each end, used to compare how fine
#: the two ends are.
#:
#: All are SMALL on purpose. Bow-versus-stern fineness is a property of the
#: extremities; by 20% of the length a hull with any parallel midbody is at
#: full beam at both ends, so a wide band no longer compares the bow with the
#: stern, it compares where the midbody happens to start. That is not a
#: direction signal, and it is actively misleading: on a real full-form tanker
#: a 20% band reported the bow at the WRONG end while every band at or below
#: 10% agreed on the right one (confirmed against the file's own bow
#: construction curves).
_TAPER_BANDS: Tuple[float, ...] = (0.01, 0.02, 0.05)

#: A band holding fewer point samples than this is discarded as too sparse to
#: measure, rather than allowed to contribute a reading built on two vertices.
_MIN_BAND_POINTS = 3


def infer_orientation(
    tris: Sequence[Tri], *, bands: Sequence[float] = _TAPER_BANDS
) -> OrientationDecision:
    """Derive which way the bow points from the hull's own taper.

    Two of the three axes are settled by geometry rather than convention: the
    longitudinal axis is simply the longest one, and once forward and up are
    fixed the transverse direction FOLLOWS from right-handedness (port is
    up x forward), so it is never guessed.

    Only the SIGN of the longitudinal axis needs evidence, and the evidence is
    that a displacement hull is finer at the bow than at the stern -- a stern
    carries its beam aft to a transom, a bow tapers to a stem.

    That comparison is made at SEVERAL bands and every band must agree. A
    single band is not trustworthy: measured on a real tanker, bands from 1% to
    10% agreed on the bow and a 20% band pointed the other way. Requiring
    agreement turns that from a silent wrong answer into a refusal, which is
    the only acceptable outcome given that the wrong choice tows the hull
    stern-first through a case that otherwise looks perfectly healthy.

    Raises ``AmbiguousOrientationError`` when the bands disagree or when the
    ends are too alike to separate (a barge has no finer end).

    ``z`` is assumed to be up. That assumption is checked, not silent: a model
    whose longest axis is ``z`` is rejected rather than reinterpreted.
    """
    lo, hi = _bounds(tris)
    extents = [hi[k] - lo[k] for k in range(3)]
    axis_long = max(range(3), key=lambda k: extents[k])
    if axis_long == 2:
        raise AmbiguousOrientationError(
            "the longest axis of this geometry is z, but this stage assumes "
            "z is up. Either the model is not a hull, or it is not z-up. "
            "Rotate it before ingestion, or pass forward= explicitly."
        )
    axis_tran = 1 if axis_long == 0 else 0

    length = extents[axis_long]
    if length <= 0.0:
        raise AmbiguousOrientationError("degenerate hull: zero length")
    centre_tran = 0.5 * (lo[axis_tran] + hi[axis_tran])
    axis_name = _AXIS_NAMES[axis_long]

    readings: List[Tuple[float, float, str, float, float]] = []
    for band in bands:
        cut = band * length
        half_lo = half_hi = 0.0
        n_lo = n_hi = 0
        for tri in tris:
            for p in tri:
                offset = abs(p[axis_tran] - centre_tran)
                if p[axis_long] <= lo[axis_long] + cut:
                    half_lo = max(half_lo, offset)
                    n_lo += 1
                if p[axis_long] >= hi[axis_long] - cut:
                    half_hi = max(half_hi, offset)
                    n_hi += 1
        if n_lo < _MIN_BAND_POINTS or n_hi < _MIN_BAND_POINTS:
            continue
        wider = max(half_lo, half_hi)
        if wider <= 0.0:
            continue
        ratio = min(half_lo, half_hi) / wider
        forward = f"+{axis_name}" if half_hi < half_lo else f"-{axis_name}"
        readings.append((band, ratio, forward, half_lo, half_hi))

    if not readings:
        raise AmbiguousOrientationError(
            "the ends of this hull hold too few points to compare; its "
            "forward direction cannot be derived. Pass forward= explicitly, "
            f"one of '+{axis_name}' or '-{axis_name}'."
        )

    directions = {reading[2] for reading in readings}
    detail = ", ".join(
        f"{band:.0%} band: {half_lo:.4g} vs {half_hi:.4g} -> {forward}"
        for band, _ratio, forward, half_lo, half_hi in readings
    )
    if len(directions) > 1:
        raise AmbiguousOrientationError(
            "the ends of this hull disagree about which is finer depending on "
            f"how far in they are measured ({detail}), so the forward "
            "direction cannot be derived. Getting this wrong tows the hull "
            f"stern-first. Pass forward= explicitly, one of '+{axis_name}' or "
            f"'-{axis_name}'."
        )

    band, ratio, forward, half_lo, half_hi = min(readings, key=lambda r: r[1])
    if ratio > _TAPER_AMBIGUITY_RATIO:
        raise AmbiguousOrientationError(
            f"the two ends of this hull are equally full ({detail}; best "
            f"ratio {ratio:.3f}), so which end is the bow cannot be derived "
            "from the geometry. Getting this wrong tows the hull stern-first. "
            f"Pass forward= explicitly, one of '+{axis_name}' or "
            f"'-{axis_name}'."
        )

    return OrientationDecision(
        forward=forward,
        source="inferred_taper",
        longitudinal_axis=axis_name,
        taper_ratio=ratio,
        note=(
            f"the finer end is the bow; {detail}; all "
            f"{len(readings)} bands agree"
        ),
    )


def _forward_vector(forward: str) -> Vec3:
    if len(forward) != 2 or forward[0] not in "+-" or forward[1] not in "xyz":
        raise HullIngestError(
            f"forward must look like '+x', '-x', '+y' or '-y', got {forward!r}"
        )
    axis = _AXIS_NAMES.index(forward[1])
    if axis == 2:
        raise HullIngestError(
            "forward cannot be the z axis: this stage assumes z is up"
        )
    sign = 1.0 if forward[0] == "+" else -1.0
    vector = tuple(sign if k == axis else 0.0 for k in range(3))
    return vector  # type: ignore[return-value]


def _rotate_to_ship_axes(tris: Sequence[Tri], forward: str) -> List[Tri]:
    """Express the hull in x-forward, y-port, z-up axes.

    Built as an orthonormal change of basis with ``port = up x forward``, so
    the result is a rotation (determinant +1) and triangle winding is carried
    through unchanged. That matters: a transform that quietly mirrored the
    hull would invert every normal and turn the solid inside out.
    """
    ex = _forward_vector(forward)
    ez = (0.0, 0.0, 1.0)
    ey = (
        ez[1] * ex[2] - ez[2] * ex[1],
        ez[2] * ex[0] - ez[0] * ex[2],
        ez[0] * ex[1] - ez[1] * ex[0],
    )

    def project(p: Vec3) -> Vec3:
        return (
            p[0] * ex[0] + p[1] * ex[1] + p[2] * ex[2],
            p[0] * ey[0] + p[1] * ey[1] + p[2] * ey[2],
            p[0] * ez[0] + p[1] * ez[1] + p[2] * ez[2],
        )

    return [tuple(project(p) for p in tri) for tri in tris]  # type: ignore[misc]


# --------------------------------------------------------------------------- #
#  Welding
# --------------------------------------------------------------------------- #

def weld_vertices(
    tris: Sequence[Tri], tolerance: float = DEFAULT_WELD_TOLERANCE_M
) -> List[Tri]:
    """Snap near-coincident vertices onto a shared value.

    Necessary because Rhino render meshes store vertices as float32. Two
    patches that meet exactly in the NURBS model meet only to about 1e-7
    relative in the cached mesh, so the raw soup has thousands of edges used by
    one triangle each and reads as leaky when it is not.

    Uses a spatial hash and checks the 27 neighbouring cells rather than simply
    rounding coordinates. Rounding puts a bin boundary somewhere, and two
    points a nanometre apart that straddle one would never merge -- which is
    precisely the failure being fixed, reintroduced by the fix.
    """
    if tolerance <= 0.0:
        return [tuple(tri) for tri in tris]  # type: ignore[misc]

    cell = tolerance
    grid: Dict[Tuple[int, int, int], List[Vec3]] = {}
    exact: Dict[Vec3, Vec3] = {}

    def canonical(p: Vec3) -> Vec3:
        hit = exact.get(p)
        if hit is not None:
            return hit
        base = (
            int(math.floor(p[0] / cell)),
            int(math.floor(p[1] / cell)),
            int(math.floor(p[2] / cell)),
        )
        for di in (-1, 0, 1):
            for dj in (-1, 0, 1):
                for dk in (-1, 0, 1):
                    bucket = grid.get((base[0] + di, base[1] + dj, base[2] + dk))
                    if not bucket:
                        continue
                    for q in bucket:
                        if (
                            abs(q[0] - p[0]) <= tolerance
                            and abs(q[1] - p[1]) <= tolerance
                            and abs(q[2] - p[2]) <= tolerance
                        ):
                            exact[p] = q
                            return q
        grid.setdefault(base, []).append(p)
        exact[p] = p
        return p

    return [tuple(canonical(p) for p in tri) for tri in tris]  # type: ignore[misc]


def _triangle_area(a: Vec3, b: Vec3, c: Vec3) -> float:
    ux, uy, uz = b[0] - a[0], b[1] - a[1], b[2] - a[2]
    vx, vy, vz = c[0] - a[0], c[1] - a[1], c[2] - a[2]
    nx, ny, nz = uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx
    return 0.5 * math.sqrt(nx * nx + ny * ny + nz * nz)


# --------------------------------------------------------------------------- #
#  Waterline measurements
# --------------------------------------------------------------------------- #

def _waterline_points(tris: Sequence[Tri], waterline_z: float) -> List[Vec3]:
    """Every point of the surface at or below the waterline.

    Includes the interpolated crossings on edges that straddle the plane, not
    just the vertices below it. On a coarse mesh the difference is real: the
    widest point of a section often sits between two vertices, and taking only
    vertices would under-report the beam.
    """
    out: List[Vec3] = []
    for tri in tris:
        for k in range(3):
            a, b = tri[k], tri[(k + 1) % 3]
            if a[2] <= waterline_z:
                out.append(a)
            if (a[2] <= waterline_z) != (b[2] <= waterline_z):
                t = (waterline_z - a[2]) / (b[2] - a[2])
                out.append(
                    (
                        a[0] + t * (b[0] - a[0]),
                        a[1] + t * (b[1] - a[1]),
                        waterline_z,
                    )
                )
    return out


# --------------------------------------------------------------------------- #
#  The manifest
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class HullManifest:
    """What the stage produced, and every choice it made getting there.

    The fields above ``units_source`` are the interface another lane consumes
    to size its computational domain; everything below is provenance, and is
    written under a ``provenance`` key so the top level of the JSON stays
    exactly the agreed contract.
    """

    source_file: str
    source_sha256: str
    units_in: str
    scale_to_m: float
    orientation: Dict[str, str]
    origin: str
    lpp_m: float
    beam_m: float
    draft_m: float
    wetted_surface_m2: float
    displacement_m3: float
    watertight: bool
    n_triangles: int
    bbox_min_m: List[float]
    bbox_max_m: List[float]

    units_source: str = ""
    units_note: str = ""
    orientation_source: str = ""
    forward_axis_in: str = ""
    orientation_note: str = ""
    origin_method: str = ""
    draft_source: str = ""
    length_measure: str = ""
    open_edge_count: int = 0
    nonmanifold_edge_count: int = 0
    degenerate_removed: int = 0
    weld_tolerance_m: float = 0.0
    forced: bool = False
    stl_file: str = ""
    source_layers: List[str] = field(default_factory=list)
    notes: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, object]:
        contract: Dict[str, object] = {
            "source_file": self.source_file,
            "source_sha256": self.source_sha256,
            "units_in": self.units_in,
            "scale_to_m": self.scale_to_m,
            "orientation": dict(self.orientation),
            "origin": self.origin,
            "lpp_m": self.lpp_m,
            "beam_m": self.beam_m,
            "draft_m": self.draft_m,
            "wetted_surface_m2": self.wetted_surface_m2,
            "displacement_m3": self.displacement_m3,
            "watertight": self.watertight,
            "n_triangles": self.n_triangles,
            "bbox_min_m": list(self.bbox_min_m),
            "bbox_max_m": list(self.bbox_max_m),
        }
        contract["provenance"] = {
            "units_source": self.units_source,
            "units_note": self.units_note,
            "orientation_source": self.orientation_source,
            "forward_axis_in": self.forward_axis_in,
            "orientation_note": self.orientation_note,
            "origin_method": self.origin_method,
            "draft_source": self.draft_source,
            "length_measure": self.length_measure,
            "open_edge_count": self.open_edge_count,
            "nonmanifold_edge_count": self.nonmanifold_edge_count,
            "degenerate_removed": self.degenerate_removed,
            "weld_tolerance_m": self.weld_tolerance_m,
            "forced": self.forced,
            "stl_file": self.stl_file,
            "source_layers": list(self.source_layers),
            "notes": list(self.notes),
        }
        return contract

    def write(self, path: Path | str) -> Path:
        out = Path(path)
        out.parent.mkdir(parents=True, exist_ok=True)
        out.write_text(json.dumps(self.to_dict(), indent=2) + "\n")
        return out


def _sha256_of_file(path: Path) -> str:
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for chunk in iter(lambda: handle.read(1 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _sha256_of_triangles(tris: Sequence[Tri]) -> str:
    digest = hashlib.sha256()
    for tri in tris:
        for p in tri:
            digest.update(f"{p[0]!r},{p[1]!r},{p[2]!r};".encode())
    return digest.hexdigest()


# --------------------------------------------------------------------------- #
#  The Rhino reader
# --------------------------------------------------------------------------- #

@dataclass
class Rhino3dmGeometry:
    """Raw tessellated geometry recovered from a ``.3dm``, in file units."""

    triangles: List[Tri]
    declared_units: Optional[str]
    layer_triangles: Dict[str, int]
    objects_total: int = 0
    objects_meshed: int = 0
    objects_without_mesh: int = 0
    layers_available: List[str] = field(default_factory=list)


def _meshes_of(geometry, rhino3dm) -> List[object]:
    """Every cached render mesh reachable from one object.

    ``GetMesh`` lives on ``BrepFace``, not on ``Brep`` -- a Brep is asked face
    by face. Missing that is why a first pass at this reader concluded, wrongly,
    that a perfectly good 88-face solid hull carried no mesh at all.
    """
    kind = type(geometry).__name__
    if kind == "Mesh":
        return [geometry]

    brep = None
    if kind == "Brep":
        brep = geometry
    elif kind == "Extrusion":
        try:
            brep = geometry.ToBrep(True)
        except Exception:  # pragma: no cover - defensive against kernel quirks
            brep = None
    if brep is None:
        return []

    out: List[object] = []
    try:
        faces = brep.Faces
    except Exception:  # pragma: no cover
        return []
    for face in faces:
        try:
            mesh = face.GetMesh(rhino3dm.MeshType.Any)
        except Exception:  # pragma: no cover
            mesh = None
        if mesh is not None and len(mesh.Faces) > 0:
            out.append(mesh)
    return out


def _triangles_of_mesh(mesh) -> List[Tri]:
    verts = [(v.X, v.Y, v.Z) for v in mesh.Vertices]
    tris: List[Tri] = []
    faces = mesh.Faces
    for i in range(len(faces)):
        face = faces[i]
        a, b, c, d = face[0], face[1], face[2], face[3]
        tris.append((verts[a], verts[b], verts[c]))
        if d != c:                       # a quad; Rhino repeats c for triangles
            tris.append((verts[a], verts[c], verts[d]))
    return tris


def _normalise_layer_filter(layers: Optional[Iterable[str]]) -> Optional[set]:
    """An empty or blank-only filter means "every layer", not "no layers".

    ``os.environ.get("X", "").split(",")`` yields ``['']``, which is truthy and
    would otherwise silently select nothing and report an empty hull.
    """
    if layers is None:
        return None
    wanted = {name.strip().lower() for name in layers if name and name.strip()}
    return wanted or None


def read_3dm_triangles(
    path: Path | str,
    *,
    layers: Optional[Iterable[str]] = None,
    tessellate_breps: bool = False,
    linear_deflection: Optional[float] = None,
) -> Rhino3dmGeometry:
    """Recover tessellated geometry and the declared unit system from a ``.3dm``.

    Only cached render meshes are used -- see the module docstring for why
    evaluating trimmed NURBS here would be worse than failing.

    ``tessellate_breps`` opts in to the CAD-kernel fallback in
    ``brep_tessellate``, which rebuilds the trimmed NURBS Breps in OpenCASCADE
    and meshes them. It is OFF by default and never fires implicitly. Two
    reasons, both about not lying to the caller:

    1. It is a modelling decision, not a format detail. The caller is choosing
       a chordal deflection and accepting a trim reconstruction, and the
       resulting surface is an approximation whose fidelity has to be reported
       (``BrepTessellation.fidelity``) rather than assumed.
    2. It needs a dependency -- an OpenCASCADE binding -- that the render-mesh
       path does not. Falling back silently would turn a clear "this file has
       no meshes, here is how to fix it" into an intermittent import error
       whose cause depends on the machine.
    """
    rhino3dm = _import_rhino3dm()
    source = Path(path)
    if not source.is_file():
        raise HullIngestError(f"no such file: {source}")

    model = rhino3dm.File3dm.Read(str(source))
    if model is None:
        raise HullIngestError(
            f"rhino3dm could not read {source.name}; it may be corrupt or "
            "written by a Rhino version this reader does not support"
        )

    declared: Optional[str] = None
    try:
        declared = RHINO_UNIT_CODES.get(int(model.Settings.ModelUnitSystem))
    except Exception:  # pragma: no cover - older files may lack settings
        declared = None

    layer_names = {i: layer.Name for i, layer in enumerate(model.Layers)}
    wanted = _normalise_layer_filter(layers)

    triangles: List[Tri] = []
    per_layer: Dict[str, int] = {}
    total = meshed = unmeshed = 0

    for obj in model.Objects:
        try:
            name = layer_names.get(obj.Attributes.LayerIndex, "")
        except Exception:  # pragma: no cover
            name = ""
        if wanted is not None and name.strip().lower() not in wanted:
            continue
        total += 1
        got: List[Tri] = []
        for mesh in _meshes_of(obj.Geometry, rhino3dm):
            got.extend(_triangles_of_mesh(mesh))
        if got:
            meshed += 1
            triangles.extend(got)
            per_layer[name] = per_layer.get(name, 0) + len(got)
        else:
            unmeshed += 1

    if not triangles and tessellate_breps:
        return _tessellate_breps_of(
            source,
            layers=layers,
            declared=declared,
            objects_total=total,
            objects_without_mesh=unmeshed,
            layers_available=[layer.Name for layer in model.Layers],
            linear_deflection=linear_deflection,
        )

    if not triangles:
        raise NoMeshGeometryError(
            f"{source.name} yielded no tessellated geometry "
            f"({total} objects examined, {unmeshed} with no cached mesh).\n"
            "rhino3dm is a file reader, not a geometry kernel: it cannot "
            "tessellate NURBS surfaces, it can only return render meshes that "
            "Rhino already stored in the file.\n" + NO_MESH_REMEDIES
        )

    return Rhino3dmGeometry(
        triangles=triangles,
        declared_units=declared,
        layer_triangles=per_layer,
        objects_total=total,
        objects_meshed=meshed,
        objects_without_mesh=unmeshed,
        layers_available=[layer.Name for layer in model.Layers],
    )


def _tessellate_breps_of(
    source: Path,
    *,
    layers: Optional[Iterable[str]],
    declared: Optional[str],
    objects_total: int,
    objects_without_mesh: int,
    layers_available: List[str],
    linear_deflection: Optional[float],
) -> Rhino3dmGeometry:
    """The opt-in CAD-kernel path, imported lazily.

    Kept behind a function-level import so that ``hull_ingest`` still has
    exactly one optional dependency from the point of view of anyone who never
    asks for this, and so the kernel's several-hundred-megabyte import cost is
    not paid by the render-mesh path.

    The fidelity report is copied into the geometry's notes rather than
    discarded: a Brep tessellation that quietly ignored its trims is the single
    failure this whole route has to be defended against, and the evidence for
    "it did not" belongs in the manifest the caller keeps.
    """
    from digitalmodel.naval_architecture import brep_tessellate  # noqa: PLC0415

    result = brep_tessellate.tessellate_3dm(
        source, layers=layers, linear_deflection=linear_deflection
    )
    fidelity = result.fidelity
    if fidelity is not None and not fidelity.within_reference_box:
        raise NotWatertightError(
            f"the Brep tessellation of {source.name} reaches "
            f"{fidelity.max_overshoot:g} file units OUTSIDE the Brep's own "
            "bounding box, which means trim boundaries were not honoured and "
            "the hull is the wrong size:\n" + fidelity.describe()
        )
    layer_triangles = {}
    if result.layer_faces:
        # Face counts, not triangle counts, are what the kernel path knows per
        # layer; recorded under the same key so the manifest stays one shape.
        share = len(result.triangles) / max(sum(result.layer_faces.values()), 1)
        layer_triangles = {
            name: int(round(faces * share))
            for name, faces in result.layer_faces.items()
        }
    return Rhino3dmGeometry(
        triangles=result.triangles,
        declared_units=declared or result.declared_units,
        layer_triangles=layer_triangles,
        objects_total=objects_total,
        objects_meshed=result.faces_converted,
        objects_without_mesh=objects_without_mesh,
        layers_available=layers_available or result.layers_available,
    )


# --------------------------------------------------------------------------- #
#  The stage
# --------------------------------------------------------------------------- #

def ingest_triangles(
    triangles: Sequence[Tri],
    out_dir: Path | str,
    *,
    source_name: Optional[str] = None,
    source_path: Optional[Path | str] = None,
    declared_units: Optional[str] = None,
    units_in: Optional[str] = None,
    force_units: bool = False,
    forward: Optional[str] = None,
    draft_m: Optional[float] = None,
    weld_tolerance: Optional[float] = None,
    force: bool = False,
    stl_name: str = "hull",
    source_layers: Optional[Sequence[str]] = None,
    notes: Optional[Sequence[str]] = None,
) -> HullManifest:
    """Normalise a triangle soup into an OpenFOAM-ready STL plus its manifest.

    The CAD-free half of the stage, so the whole pipeline downstream of the
    reader is testable without ``rhino3dm`` and without a client file.

    Order matters and is deliberate: scale to metres, weld, rotate onto ship
    axes, drop degenerates, gate on closure, orient, translate onto the origin,
    then measure. Measuring before the gate would put numbers derived from a
    leaky surface into a manifest another lane treats as authoritative.
    """
    tris: List[Tri] = [tuple(tri) for tri in triangles]  # type: ignore[misc]
    if not tris:
        raise HullIngestError("no triangles to ingest")

    note_list: List[str] = list(notes or [])

    if source_path is not None:
        source_path = Path(source_path)
        resolved_name = source_name or source_path.name
        digest = _sha256_of_file(source_path)
    else:
        resolved_name = source_name or "<in-memory>"
        digest = _sha256_of_triangles(tris)
        note_list.append(
            "source_sha256 is the digest of the ingested triangles, not of a "
            "source file: no source file was supplied"
        )

    # --- units ------------------------------------------------------------ #
    lo, hi = _bounds(tris)
    longest = max(hi[k] - lo[k] for k in range(3))
    unit_decision = infer_units(
        longest, declared=declared_units, override=units_in, force=force_units
    )
    scale = unit_decision.scale_to_m
    if scale != 1.0:
        tris = [
            tuple(tuple(c * scale for c in p) for p in tri) for tri in tris
        ]  # type: ignore[misc]

    # --- weld -------------------------------------------------------------- #
    tolerance = (
        DEFAULT_WELD_TOLERANCE_M if weld_tolerance is None else weld_tolerance
    )
    tris = weld_vertices(tris, tolerance)

    # --- orientation ------------------------------------------------------- #
    if forward is not None:
        orientation = OrientationDecision(
            forward=forward, source="caller", longitudinal_axis=forward[1]
        )
    else:
        orientation = infer_orientation(tris)
    tris = _rotate_to_ship_axes(tris, orientation.forward)

    # --- degenerates ------------------------------------------------------- #
    before = len(tris)
    tris = [tri for tri in tris if _triangle_area(*tri) > 0.0]
    dropped = before - len(tris)
    if not tris:
        raise HullIngestError("every triangle was degenerate after welding")

    # --- the watertightness gate ------------------------------------------- #
    check = check_surface(tris)
    watertight = check.closed
    if not watertight and not force:
        raise NotWatertightError(
            f"the surface is not closed: {check.open_edge_count} open edges "
            f"and {check.nonmanifold_edge_count} non-manifold edges across "
            f"{check.triangle_count} triangles (weld tolerance "
            f"{tolerance:g} m).\n"
            "snappyHexMesh will mesh a leaky hull without complaint and the "
            "solver will return a confident wrong answer, so this stage stops "
            "here.\n"
            "Options: raise weld_tolerance= if the gaps are numerical; repair "
            "the surface in CAD if they are real; or pass force=True to emit "
            "the STL anyway with watertight=false recorded in the manifest."
        )

    if watertight:
        tris = orient_consistently(tris)
    else:
        note_list.append(
            "normals were NOT made consistent: that repair assumes a closed "
            "manifold, which this surface is not"
        )

    # --- origin ------------------------------------------------------------ #
    lo, hi = _bounds(tris)
    keel_z = lo[2]
    centre_y = 0.5 * (lo[1] + hi[1])
    tris = [
        tuple((p[0], p[1] - centre_y, p[2] - keel_z) for p in tri) for tri in tris
    ]  # type: ignore[misc]

    lo, hi = _bounds(tris)
    depth = hi[2] - lo[2]
    if draft_m is None:
        waterline_z = hi[2]
        draft_source = "assumed_full_hull_depth"
        note_list.append(
            "no draft was supplied, so the waterline was placed at the top of "
            "the hull and the hull treated as fully submerged; "
            "wetted_surface_m2 and displacement_m3 are for that condition"
        )
    else:
        waterline_z = float(draft_m)
        draft_source = "caller"
        if waterline_z > depth + tolerance:
            note_list.append(
                f"the requested draft {waterline_z:g} m exceeds the hull depth "
                f"{depth:g} m; the hull is fully submerged at this draft"
            )

    wet = _waterline_points(tris, waterline_z)
    if wet:
        x_ap = min(p[0] for p in wet)
        origin_method = "aft_extremity_of_the_wetted_surface"
    else:  # pragma: no cover - only for a waterline below the keel
        x_ap = lo[0]
        origin_method = "aft_extremity_of_the_hull"
        note_list.append(
            "nothing lies below the waterline; the origin fell back to the "
            "aft extremity of the whole hull"
        )
    tris = [
        tuple((p[0] - x_ap, p[1], p[2]) for p in tri) for tri in tris
    ]  # type: ignore[misc]

    # --- measure ----------------------------------------------------------- #
    wet = _waterline_points(tris, waterline_z)
    if wet:
        lpp = max(p[0] for p in wet) - min(p[0] for p in wet)
        beam = max(p[1] for p in wet) - min(p[1] for p in wet)
    else:  # pragma: no cover
        lpp = hi[0] - lo[0]
        beam = hi[1] - lo[1]

    wetted = wetted_surface_area(tris, waterline_z)
    displacement = enclosed_volume(tris, waterline_z)
    lo, hi = _bounds(tris)

    # --- emit -------------------------------------------------------------- #
    out = Path(out_dir)
    out.mkdir(parents=True, exist_ok=True)
    stl_path = write_stl(tris, out / f"{stl_name}.stl", name=stl_name)

    manifest = HullManifest(
        source_file=resolved_name,
        source_sha256=digest,
        units_in=unit_decision.units,
        scale_to_m=unit_decision.scale_to_m,
        orientation=dict(OUTPUT_ORIENTATION),
        origin=OUTPUT_ORIGIN,
        lpp_m=lpp,
        beam_m=beam,
        draft_m=min(waterline_z, depth),
        wetted_surface_m2=wetted,
        displacement_m3=displacement,
        watertight=watertight,
        n_triangles=len(tris),
        bbox_min_m=[lo[0], lo[1], lo[2]],
        bbox_max_m=[hi[0], hi[1], hi[2]],
        units_source=unit_decision.source,
        units_note=unit_decision.note,
        orientation_source=orientation.source,
        forward_axis_in=orientation.forward,
        orientation_note=orientation.note,
        origin_method=origin_method,
        draft_source=draft_source,
        length_measure=(
            "waterline length at the stated draft, used as a proxy for Lpp"
        ),
        open_edge_count=check.open_edge_count,
        nonmanifold_edge_count=check.nonmanifold_edge_count,
        degenerate_removed=dropped,
        weld_tolerance_m=tolerance,
        forced=bool(force and not watertight),
        stl_file=stl_path.name,
        source_layers=list(source_layers or []),
        notes=note_list,
    )
    manifest.write(out / f"{stl_name}_manifest.json")
    return manifest


def ingest_3dm(
    path: Path | str,
    out_dir: Path | str,
    *,
    layers: Optional[Iterable[str]] = None,
    units_in: Optional[str] = None,
    force_units: bool = False,
    forward: Optional[str] = None,
    draft_m: Optional[float] = None,
    weld_tolerance: Optional[float] = None,
    force: bool = False,
    stl_name: str = "hull",
    tessellate_breps: bool = False,
    linear_deflection: Optional[float] = None,
) -> HullManifest:
    """Ingest a Rhino ``.3dm`` hull into a normalised STL plus its manifest.

    ``layers`` restricts which Rhino layers contribute geometry. Real client
    files routinely carry the hull alongside cranes, accommodation blocks and
    traced 2D drawings, and ingesting all of it produces a "hull" that is not
    one; the layer report on ``Rhino3dmGeometry`` is there to make choosing
    easy.

    ``tessellate_breps`` opts in to the CAD-kernel fallback for files that
    carry no cached render meshes. See :func:`read_3dm_triangles` for why it is
    never automatic.
    """
    geometry = read_3dm_triangles(
        path,
        layers=layers,
        tessellate_breps=tessellate_breps,
        linear_deflection=linear_deflection,
    )
    used = sorted(geometry.layer_triangles)
    notes = [
        f"read {len(geometry.triangles)} triangles from "
        f"{geometry.objects_meshed}/{geometry.objects_total} objects; "
        f"{geometry.objects_without_mesh} carried no cached render mesh",
    ]
    if tessellate_breps:
        notes.append(
            "trimmed NURBS Breps were rebuilt in the OpenCASCADE kernel "
            "(brep_tessellate); triangles are an approximation of the NURBS "
            "surface, not the modeller's own tessellation"
        )
    if geometry.declared_units is None:
        notes.append("the file did not declare a usable unit system")
    return ingest_triangles(
        geometry.triangles,
        out_dir,
        source_path=Path(path),
        declared_units=geometry.declared_units,
        units_in=units_in,
        force_units=force_units,
        forward=forward,
        draft_m=draft_m,
        weld_tolerance=weld_tolerance,
        force=force,
        stl_name=stl_name,
        source_layers=used,
        notes=notes,
    )
