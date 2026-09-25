"""
ABOUTME: Curvature screening of hull panel meshes through HullProd (Serani & Maki 2026).
ABOUTME: Optional dependency (``digitalmodel[curvature]``); returns the HullProd signature plus per-vertex K, H.

HullProd (CNR-INM, BSD-3-Clause, https://github.com/cnr-inm-mao/hullprod) computes a
dimensionless curvature signature of a surface: ``I_D`` (area-averaged
``|K| L_ref^2``), its elliptic and saddle parts, and the area fractions of flat, singly
curved, elliptic and saddle regions. This module adapts a ``PanelMesh`` to that pipeline
so catalog hulls and parametric variants can carry a fairness fingerprint next to their
RAOs (#2170, decisions D1/D2/D6).

Interpretation limits (evaluation of 2026-09-25, docs/domains/hull_library/
hullprod-curvature-screening-evaluation.md):

- Mesh results are representation-sensitive; compare hulls at matched panel density and
  always pass ``lref`` (``length_bp``) explicitly.
- For pontoon-column hulls (semi-sub, spar, TLP, cylinder) the signature is dominated by
  creases at column/pontoon junctions and by facet edges, so it measures paneling, not plate
  curvature. ``CREASE_DOMINATED_HULL_TYPES`` marks those; the signature is still computed
  but annotated, and no saddle warning threshold applies.
- The descriptors are geometry screening values, not fabrication-cost or forming predictors.

Citation: Serani, A. & Maki, K. J. (2026). Geometry-Based Metrics for Early-Stage
Hull-Form Producibility Screening. arXiv:2609.27544.
"""

from __future__ import annotations

import importlib
import importlib.util
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Optional

import numpy as np
from numpy.typing import NDArray
from pydantic import BaseModel, Field

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh

from .profile_schema import HullProfile, HullType

HULLPROD_REQUIREMENT = "hullprod>=1.0.1,<2"
HULLPROD_CITATION = (
    "Serani, A. & Maki, K. J. (2026). Geometry-Based Metrics for Early-Stage "
    "Hull-Form Producibility Screening. arXiv:2609.27544."
)

#: Hull types whose HullProd signature is crease-dominated (decision D6): the value is a
#: paneling-quality metric for these, not a plate-curvature or producibility metric.
CREASE_DOMINATED_HULL_TYPES: frozenset[HullType] = frozenset(
    {
        HullType.SEMI_PONTOON,
        HullType.SPAR,
        HullType.TLP,
        HullType.CYLINDER,
        HullType.LID,
    }
)

#: Monohull saddle-fraction warning thresholds (decision D4, warn-only). Seeded from the
#: 2026-09-25 evaluation table: FPSO 0.03, L01 vessel 0.12, Bokalift 0.15, drillship 0.21
#: on coarse BEM meshes. A generator artefact (ruled lofting) read 0.60.
_SADDLE_WARNING_THRESHOLDS: dict[HullType, float] = {
    HullType.TANKER: 0.35,
    HullType.SHIP: 0.35,
    HullType.FPSO: 0.35,
    HullType.LNGC: 0.35,
    HullType.BARGE: 0.35,
}


def hullprod_available() -> bool:
    """Return True when the optional ``hullprod`` package can be imported."""
    return importlib.util.find_spec("hullprod") is not None


def require_hullprod() -> Any:
    """Import and return the ``hullprod`` module or raise an actionable ImportError."""
    try:
        return importlib.import_module("hullprod")
    except ImportError as exc:  # pragma: no cover - exercised only without the extra
        raise ImportError(
            "Curvature screening needs the optional HullProd dependency: "
            f"pip install 'digitalmodel[curvature]' (or '{HULLPROD_REQUIREMENT}')."
        ) from exc


def saddle_warning_threshold(hull_type: HullType | str | None) -> float | None:
    """Return the warn-only ``a_C_saddle`` threshold for a hull type, or None.

    None means no saddle threshold applies (crease-dominated or unclassified types).
    """
    if hull_type is None:
        return None
    try:
        key = HullType(hull_type)
    except ValueError:
        return None
    return _SADDLE_WARNING_THRESHOLDS.get(key)


class CurvatureSignature(BaseModel):
    """HullProd 1.0 recommended signature with the provenance needed to compare values."""

    I_D: float = Field(
        ..., description="Area-averaged |K| L_ref^2 over the valid surface"
    )
    I_D_plus: float = Field(..., description="Elliptic (synclastic) part of I_D")
    I_D_minus: float = Field(..., description="Saddle (anticlastic) part of I_D")
    a_flat: float = Field(
        ..., description="Area fraction with both principal curvatures ~0"
    )
    a_single: float = Field(
        ..., description="Area fraction singly curved (developable)"
    )
    a_elliptic: float = Field(
        ..., description="Area fraction elliptic double curvature"
    )
    a_saddle: float = Field(
        ..., description="Area fraction saddle/reverse double curvature"
    )
    lref: float = Field(..., description="Reference length in mesh units")
    lref_mode: str = Field(
        ..., description="'explicit_user' or HullProd automatic mode"
    )
    reliability: str = Field(
        ..., description="HullProd mesh curvature reliability: good/caution/poor"
    )
    status: str = Field(..., description="HullProd validity status of I_D")
    valid_area_fraction: float = Field(
        ..., description="Fraction of surface area with valid K"
    )
    panel_count: int = Field(
        ..., description="Triangles assessed after symmetry expansion"
    )
    vertex_count: int = Field(
        ..., description="Vertices assessed after symmetry expansion"
    )
    hullprod_version: str
    hull_type: str | None = None
    crease_dominated: bool = Field(
        False,
        description="Hull type is crease-dominated; treat as paneling metric only (D6)",
    )
    notes: list[str] = Field(default_factory=list)

    def as_vector(self) -> list[float]:
        """Return ``[I_D, I_D_plus, I_D_minus, a_flat, a_single, a_elliptic, a_saddle]``."""
        return [
            self.I_D,
            self.I_D_plus,
            self.I_D_minus,
            self.a_flat,
            self.a_single,
            self.a_elliptic,
            self.a_saddle,
        ]

    def saddle_warning(self) -> str | None:
        """Return a warning line when ``a_saddle`` exceeds the hull-type threshold."""
        threshold = saddle_warning_threshold(self.hull_type)
        if threshold is None or self.a_saddle <= threshold:
            return None
        return (
            f"saddle (reverse-curvature) area fraction {self.a_saddle:.2f} exceeds "
            f"{threshold:.2f} for hull type '{self.hull_type}'; check station lofting "
            "or fairness before diffraction"
        )


@dataclass
class CurvatureFields:
    """Per-vertex curvature fields on the assessed (symmetry-expanded) triangle mesh."""

    vertices: NDArray[np.float64]
    K: NDArray[np.float64]
    H: NDArray[np.float64]
    class_id: NDArray[np.int64]
    valid: NDArray[np.bool_]


@dataclass
class CurvatureScreenResult:
    """Signature plus optional fields and the raw HullProd provenance dictionary."""

    signature: CurvatureSignature
    fields: CurvatureFields | None
    provenance: dict[str, Any]


# ---------------------------------------------------------------------------
# Mesh conversion
# ---------------------------------------------------------------------------


def _triangles_from_panels(panels: NDArray[np.int32]) -> list[list[int]]:
    """Split quads into two triangles; collapsed quads and -1 padded panels become one."""
    faces: list[list[int]] = []
    for panel in np.asarray(panels):
        idx = [int(i) for i in panel if int(i) >= 0]
        if len(idx) == 4 and idx[2] != idx[3]:
            faces.append([idx[0], idx[1], idx[2]])
            faces.append([idx[0], idx[2], idx[3]])
        elif len(idx) >= 3:
            faces.append(idx[:3])
    return faces


def panel_mesh_to_trimesh(mesh: PanelMesh, *, expand_symmetry: bool = True) -> Any:
    """Convert a ``PanelMesh`` to a merged ``trimesh.Trimesh``.

    ``symmetry_plane`` letters name mirrored axes in the WAMIT ISX/ISY sense (``'y'`` is
    the half hull with y >= 0, as ``HullMeshGenerator`` produces). Mirrored copies get
    reversed winding so normals stay outward.
    """
    trimesh = importlib.import_module("trimesh")
    vertices = np.asarray(mesh.vertices, dtype=float)
    faces = np.asarray(_triangles_from_panels(mesh.panels), dtype=np.int64)
    if len(faces) == 0:
        raise ValueError("PanelMesh has no usable panels for curvature screening")

    if expand_symmetry and mesh.symmetry_plane:
        for axis_name, column in (("x", 0), ("y", 1), ("z", 2)):
            if axis_name in mesh.symmetry_plane.lower():
                mirrored = vertices.copy()
                mirrored[:, column] *= -1.0
                offset = len(vertices)
                vertices = np.vstack([vertices, mirrored])
                faces = np.vstack([faces, faces[:, ::-1] + offset])

    tri = trimesh.Trimesh(vertices, faces, process=True)
    tri.merge_vertices()
    tri.update_faces(tri.nondegenerate_faces())
    return tri


# ---------------------------------------------------------------------------
# Screening
# ---------------------------------------------------------------------------


def _metric_status(metadata: dict[str, Any]) -> str:
    """Pull the I_D validity status out of HullProd metadata across key spellings."""
    container = metadata.get("metric_validity") or {}
    for key in ("developability_deviation", "I_D"):
        entry = container.get(key)
        if isinstance(entry, dict) and entry.get("status"):
            return str(entry["status"])
        if isinstance(entry, str):
            return entry
    return "unknown"


def _fields_from_result(result: Any, tri: Any) -> CurvatureFields | None:
    local = getattr(result, "local_fields", None) or {}
    if "K" not in local or "H" not in local:
        return None
    K = np.asarray(local["K"], dtype=float)
    if len(K) != len(tri.vertices):
        return None
    class_id = np.asarray(
        local.get("curvature_class_id", np.full(len(K), -1)), dtype=np.int64
    )
    valid = np.asarray(local.get("K_valid", np.isfinite(K)), dtype=bool)
    return CurvatureFields(
        vertices=np.asarray(tri.vertices, dtype=float),
        K=K,
        H=np.asarray(local["H"], dtype=float),
        class_id=class_id,
        valid=valid,
    )


def screen_trimesh(
    tri: Any,
    *,
    lref: float | None,
    hull_type: HullType | str | None = None,
    keep_fields: bool = True,
    workdir: str | Path | None = None,
) -> CurvatureScreenResult:
    """Run HullProd on an in-memory ``trimesh.Trimesh`` and return the signature."""
    hullprod = require_hullprod()

    with tempfile.TemporaryDirectory(dir=workdir) as tmp:
        # OBJ keeps vertex order, so per-vertex fields map back onto ``tri.vertices``.
        path = Path(tmp) / "hull.obj"
        tri.export(path)
        result = hullprod.assess(path, lref=lref)

    sig = result.signature
    md = dict(result.metadata)
    ref = md.get("reference_length") or {}
    resolved_type = None
    if hull_type is not None:
        try:
            resolved_type = HullType(hull_type)
        except ValueError:
            resolved_type = None

    notes: list[str] = []
    crease = resolved_type in CREASE_DOMINATED_HULL_TYPES
    if crease:
        notes.append(
            "crease-dominated hull type: signature reflects column/pontoon junctions and "
            "facet edges, use as a paneling metric only (decision D6)"
        )
    if lref is None:
        notes.append(
            "automatic reference length used; pass lref=length_bp for comparability"
        )
    reliability = str(md.get("curvature_reliability_status", "unknown"))
    if reliability == "poor":
        notes.append(
            "HullProd mesh reliability is poor; curvature values are not trustworthy"
        )

    signature = CurvatureSignature(
        I_D=float(sig["I_D"]),
        I_D_plus=float(sig["I_D_plus"]),
        I_D_minus=float(sig["I_D_minus"]),
        a_flat=float(sig["a_C"]["flat"]),
        a_single=float(sig["a_C"]["single"]),
        a_elliptic=float(sig["a_C"]["elliptic"]),
        a_saddle=float(sig["a_C"]["saddle"]),
        lref=float(ref.get("value", lref if lref is not None else float("nan"))),
        lref_mode=str(
            ref.get("mode", "explicit_user" if lref is not None else "unknown")
        ),
        reliability=reliability,
        status=_metric_status(md),
        valid_area_fraction=float(
            md.get("developability_valid_area_fraction", float("nan"))
        ),
        panel_count=int(md.get("n_faces", len(tri.faces))),
        vertex_count=int(md.get("n_vertices", len(tri.vertices))),
        hullprod_version=str(
            md.get("hullprod_version", getattr(hullprod, "__version__", ""))
        ),
        hull_type=resolved_type.value if resolved_type else None,
        crease_dominated=crease,
        notes=notes,
    )
    fields = _fields_from_result(result, tri) if keep_fields else None
    # Arrays are large and already surfaced through ``fields``.
    provenance = {k: v for k, v in md.items() if not isinstance(v, np.ndarray)}
    provenance["citation"] = HULLPROD_CITATION
    return CurvatureScreenResult(
        signature=signature, fields=fields, provenance=provenance
    )


def screen_panel_mesh(
    mesh: PanelMesh,
    *,
    lref: float | None,
    hull_type: HullType | str | None = None,
    keep_fields: bool = True,
    expand_symmetry: bool = True,
    workdir: str | Path | None = None,
) -> CurvatureScreenResult:
    """Screen a ``PanelMesh`` (quads or triangles, optional symmetry) with HullProd.

    Args:
        mesh: Panel mesh, typically from ``HullMeshGenerator`` or a GDF loader.
        lref: Reference length in mesh units; use ``length_bp`` for catalog hulls.
        hull_type: Drives the crease-dominated annotation and saddle threshold.
        keep_fields: Keep per-vertex K, H, class and validity arrays.
        expand_symmetry: Mirror the mesh across its ``symmetry_plane`` first.
        workdir: Directory for the temporary OBJ handed to HullProd.
    """
    tri = panel_mesh_to_trimesh(mesh, expand_symmetry=expand_symmetry)
    return screen_trimesh(
        tri, lref=lref, hull_type=hull_type, keep_fields=keep_fields, workdir=workdir
    )


def screen_profile(
    profile: HullProfile,
    config: Any = None,
    *,
    keep_fields: bool = False,
) -> CurvatureScreenResult:
    """Generate the profile's mesh with ``HullMeshGenerator`` and screen it.

    ``lref`` is ``profile.length_bp`` so signatures of catalog hulls and parametric
    variants are comparable.
    """
    from .mesh_generator import HullMeshGenerator

    mesh = HullMeshGenerator().generate(profile, config)
    return screen_panel_mesh(
        mesh,
        lref=float(profile.length_bp),
        hull_type=profile.hull_type,
        keep_fields=keep_fields,
    )


__all__ = [
    "CREASE_DOMINATED_HULL_TYPES",
    "HULLPROD_CITATION",
    "HULLPROD_REQUIREMENT",
    "CurvatureFields",
    "CurvatureScreenResult",
    "CurvatureSignature",
    "hullprod_available",
    "panel_mesh_to_trimesh",
    "require_hullprod",
    "saddle_warning_threshold",
    "screen_panel_mesh",
    "screen_profile",
    "screen_trimesh",
]
