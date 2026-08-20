#!/usr/bin/env python3
"""
ABOUTME: N-surface support for the arbitrary-hull resistance case (#2023).
Turns a list of closed STL regions -- hull plus appendages -- into the
snappyHexMeshDict, surfaceFeatureExtractDict and controlDict blocks that
declare all of them, and decides how forces are reported over them.

WHY snappyHexMesh NEEDS NOTHING MORE THAN THIS
----------------------------------------------
A rudder and a boss interpenetrate the hull. Their union is not available as a
single watertight surface without a boolean this lane refuses to run, and it
does not have to be: snappyHexMesh takes any number of closed
``triSurfaceMesh`` entries, tests each cell against each of them, and keeps the
region reachable from ``locationInMesh``. The union is formed by that
reachability test. The requirements are exactly two -- every surface closed,
and the keep-point outside all of them -- and both are verified in the
ingestion lane before a case is ever built.

FORCES: PER PATCH, *AND* OVER THE UNION
---------------------------------------
Both, and the split is not a hedge.

``forceCoeffs`` divides by ``0.5 rho U^2 Aref``. A coefficient is only
comparable against another coefficient when both share a reference area, so
there is exactly ONE meaningful ``Aref`` for this case: the external wetted
area of the whole union (halved for the half domain). Per-patch COEFFICIENTS
would each need their own area, and the three would then not sum to the total
-- an appendage Cd against the rudder's own 79 m2 is a number nobody can add
to the hull's. So ``forceCoeffs`` integrates the UNION of patches, and its
Aref is the union's external area.

The appendage SHARE is still the thing the client is paying for, and it is
recovered without any reference-area bookkeeping at all: a ``forces`` function
object per patch reports force in NEWTONS. Newtons sum. The rudder's share of
total resistance is then a division the reader performs on two honest numbers
rather than a coefficient ratio that hides an area convention.
"""

from __future__ import annotations

import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Sequence, Tuple

__all__ = [
    "HULL_PATCH",
    "SurfaceRegion",
    "check_region_surfaces",
    "copy_region_surfaces",
    "hull_region",
    "region_provenance",
    "region_tokens",
]

#: The hull's patch name is FROZEN. ``controlDict``'s forces and forceCoeffs
#: blocks name it, ``addLayersControls`` names it, and the report lane parses
#: ``postProcessing/forces*/`` by it. Renaming it would leave a case that
#: meshes and solves and reports nothing.
HULL_PATCH = "hull"

#: Prefix for the per-patch breakdown function objects. Distinct from the
#: union's plain ``forces``/``forceCoeffs`` so a reader parsing
#: ``postProcessing/`` cannot mistake a share for the total.
PATCH_FORCES_PREFIX = "forces_"


@dataclass(frozen=True)
class SurfaceRegion:
    """One closed STL that snappyHexMesh will meet as its own surface."""

    name: str
    stl_path: Path
    role: str = "appendage"
    refinement_level: Tuple[int, int] = (0, 0)
    n_surface_layers: int = 3

    def __post_init__(self) -> None:
        if not self.name or not self.name.replace("_", "").isalnum():
            raise ValueError(
                f"region name {self.name!r} is not a usable OpenFOAM patch "
                "name; it becomes a patch, a geometry key and a file stem"
            )
        lo, hi = self.refinement_level
        if lo < 0 or hi < lo:
            raise ValueError(
                f"refinement_level {self.refinement_level} must be "
                "(min, max) with 0 <= min <= max"
            )
        if self.n_surface_layers < 0:
            raise ValueError(
                f"n_surface_layers must be >= 0, got {self.n_surface_layers}"
            )

    @property
    def stl_name(self) -> str:
        return Path(self.stl_path).name

    @property
    def emesh_name(self) -> str:
        """What ``surfaceFeatureExtract`` writes for this surface.

        Derived from the STL name for the same reason the single-surface path
        derives it: snappyHexMesh reads the name back, and a literal that has
        drifted aborts the mesher only after blockMesh and the extraction have
        already run.
        """
        return Path(self.stl_name).with_suffix(".eMesh").name


def hull_region(stl_path: Path | str, **options: Any) -> SurfaceRegion:
    """The hull region, with its patch name pinned to :data:`HULL_PATCH`."""
    return SurfaceRegion(
        name=HULL_PATCH, stl_path=Path(stl_path), role="hull", **options
    )


def _validate(regions: Sequence[SurfaceRegion]) -> None:
    if not regions:
        raise ValueError("a case needs at least one surface region")
    names = [r.name for r in regions]
    if len(set(names)) != len(names):
        raise ValueError(f"duplicate region name(s) in {names}")
    stls = [r.stl_name for r in regions]
    if len(set(stls)) != len(stls):
        raise ValueError(
            f"two regions share an STL file name in {stls}; both would be "
            "copied to constant/triSurface and one would overwrite the other"
        )
    if names[0] != HULL_PATCH:
        raise ValueError(
            f"the first region must be the hull, patch {HULL_PATCH!r}, got "
            f"{names[0]!r}"
        )


# --------------------------------------------------------------------------- #
#  Dictionary blocks
# --------------------------------------------------------------------------- #

def region_tokens(
    regions: Sequence[SurfaceRegion], *, c_of_r: str
) -> Dict[str, str]:
    """Every ``@TOKEN@`` that depends on HOW MANY surfaces there are.

    ``c_of_r`` is passed in ALREADY FORMATTED rather than left as ``@COFR@``
    inside the generated text. A token value that itself contains a token is
    substituted or not depending on dictionary order, and this one would land
    on the wrong side of it: ``COFR`` is produced before ``PERPATCHFORCES``, so
    the injected ``@COFR@`` would survive to the leftover-token check and fail
    the build. Values are inert here, by construction.
    """
    _validate(regions)
    return {
        "GEOMETRY": _geometry_block(regions),
        "FEATURES": _features_block(regions),
        "REFINEMENTSURFACES": _refinement_block(regions),
        "LAYERS": _layers_block(regions),
        "FEATUREEXTRACT": _feature_extract_block(regions),
        "FORCEPATCHES": " ".join(r.name for r in regions),
        "PERPATCHFORCES": _per_patch_forces(regions, c_of_r),
    }


def _indent(text: str, spaces: int) -> str:
    pad = " " * spaces
    return "\n".join(pad + line if line else line for line in text.splitlines())


def _geometry_block(regions: Sequence[SurfaceRegion]) -> str:
    parts = [
        f"{r.stl_name}\n"
        "{\n"
        "    type triSurfaceMesh;\n"
        f"    name {r.name};\n"
        "\n"
        "    patchInfo\n"
        "    {\n"
        "        type wall;\n"
        "    }\n"
        "}"
        for r in regions
    ]
    return _indent("\n\n".join(parts), 4).lstrip()


def _features_block(regions: Sequence[SurfaceRegion]) -> str:
    parts = [
        "{\n" f'    file "{r.emesh_name}";\n' "    level 0;\n" "}"
        for r in regions
    ]
    return _indent("\n".join(parts), 9).lstrip()


def _refinement_block(regions: Sequence[SurfaceRegion]) -> str:
    parts = [
        f"{r.name}\n"
        "{\n"
        "    // Surface-wise min and max refinement level\n"
        f"    level ({r.refinement_level[0]} {r.refinement_level[1]});\n"
        "}"
        for r in regions
    ]
    return _indent("\n".join(parts), 8).lstrip()


def _layers_block(regions: Sequence[SurfaceRegion]) -> str:
    parts = [
        f"{r.name}\n" "{\n" f"    nSurfaceLayers {r.n_surface_layers};\n" "}"
        for r in regions
        if r.n_surface_layers > 0
    ]
    return _indent("\n".join(parts), 8).lstrip()


def _feature_extract_block(regions: Sequence[SurfaceRegion]) -> str:
    parts = [
        f"{r.stl_name}\n"
        "{\n"
        "    extractionMethod    extractFromSurface;\n"
        "    includedAngle       150;\n"
        "\n"
        "    subsetFeatures\n"
        "    {\n"
        "        nonManifoldEdges       yes;\n"
        "        openEdges       yes;\n"
        "    }\n"
        "\n"
        "    writeObj            yes;\n"
        "}"
        for r in regions
    ]
    return "\n\n".join(parts)


def _per_patch_forces(regions: Sequence[SurfaceRegion], c_of_r: str) -> str:
    """One ``forces`` object per patch, in NEWTONS, and only when it adds.

    Empty for a single-region case: the union's own ``forces`` block already
    is the hull's, and a second identical function object would write a second
    ``postProcessing`` directory holding the same numbers under a different
    name -- an invitation to report the same force twice.
    """
    if len(regions) < 2:
        return ""
    parts = [
        f"{PATCH_FORCES_PREFIX}{r.name}\n"
        "    {\n"
        "        type            forces;\n"
        "        libs            (forces);\n"
        f"        patches         ({r.name});\n"
        "        // The VOF density FIELD, never a constant: a constant\n"
        "        // integrates the above-water surface at water density.\n"
        "        rho             rho;\n"
        "        log             on;\n"
        "        writeControl    timeStep;\n"
        "        writeInterval   1;\n"
        f"        CofR            ({c_of_r});\n"
        "    }"
        for r in regions
    ]
    return "\n\n    ".join(parts)


# --------------------------------------------------------------------------- #
#  Emission and provenance
# --------------------------------------------------------------------------- #

def check_region_surfaces(regions: Sequence[SurfaceRegion]) -> None:
    """Every region well-formed and every STL present, writing nothing.

    Separate from the copy so the builder can run it BEFORE it renders the
    case tree: a case missing one of three surfaces meshes perfectly well.
    snappy simply never hears about the rudder, and nothing downstream says
    so -- not the log, not the mesh check, not the force report.
    """
    _validate(regions)
    missing = [str(r.stl_path) for r in regions if not Path(r.stl_path).is_file()]
    if missing:
        raise FileNotFoundError(f"surface(s) not found: {missing}")


def copy_region_surfaces(
    regions: Sequence[SurfaceRegion], tri_surface: Path
) -> List[Path]:
    """Copy every region STL into ``constant/triSurface``."""
    check_region_surfaces(regions)
    tri_surface.mkdir(parents=True, exist_ok=True)
    out: List[Path] = []
    for region in regions:
        dst = tri_surface / region.stl_name
        shutil.copyfile(region.stl_path, dst)
        out.append(dst)
    return out


def region_provenance(regions: Sequence[SurfaceRegion]) -> Dict[str, Any]:
    """What was placed, under what patch name, and how forces are reported."""
    return {
        "n_regions": len(regions),
        "regions": [
            {
                "name": r.name,
                "role": r.role,
                "stl": r.stl_name,
                "emesh": r.emesh_name,
                "refinement_level": list(r.refinement_level),
                "n_surface_layers": r.n_surface_layers,
            }
            for r in regions
        ],
        "union_note": (
            "the regions are separate closed surfaces; snappyHexMesh forms "
            "their union from per-surface inside/outside tests, which is valid "
            "only because each is closed and locationInMesh is outside all of "
            "them"
        ),
        "forces_note": (
            "forceCoeffs integrates the UNION of the patches against ONE Aref "
            "(the union's external wetted area, halved for the half domain), "
            "because coefficients are only comparable at a shared reference "
            "area. The per-patch breakdown is reported as "
            f"{PATCH_FORCES_PREFIX}<patch> in NEWTONS, which sum, rather than "
            "as per-patch coefficients, which do not."
        ),
    }


def assert_regions_agree(manifest, appendages) -> None:
    """Refuse an appendage-inclusive Aref on a hull-only mesh.

    The two facts arrive by different routes: Aref comes from the MANIFEST's
    reference wetted surface, which counts every region, while the geometry
    comes from the case config's ``appendages``. Nothing connects them, so
    supplying one without the other builds cleanly, meshes cleanly, and
    divides every coefficient by an area covering surfaces the mesher never
    met -- a silent 1.4% error on the hull this was found on.
    """
    declared = {r.get("name") for r in manifest.appendage_regions}
    declared.discard("hull")
    supplied = {r.name for r in appendages}
    if declared and not supplied:
        raise ValueError(
            f"the manifest declares appendage regions {sorted(declared)} and its "
            f"reference wetted surface counts them, but no appendages were "
            f"supplied to the case: Aref would include surfaces the mesh never "
            f"sees. Pass appendages=..., or build from a hull-only manifest."
        )
    missing = declared - supplied
    if missing:
        raise ValueError(
            f"appendage regions {sorted(missing)} are declared in the manifest "
            f"but not supplied to the case"
        )
