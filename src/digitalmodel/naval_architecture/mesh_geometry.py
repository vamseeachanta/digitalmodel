"""Mesh geometry for the mesh hydrostatics adapter."""
from __future__ import annotations

import numpy as np
from typing import Optional, Sequence


SCHEMA_VERSION = "mesh_hydrostatics/2"



UNIT_SCALE = {"m": 1.0, "mm": 1e-3}



_AXIS_UNIT = {
    "forward": (1.0, 0.0, 0.0),
    "aft": (-1.0, 0.0, 0.0),
    "port": (0.0, 1.0, 0.0),
    "starboard": (0.0, -1.0, 0.0),
    "up": (0.0, 0.0, 1.0),
    "down": (0.0, 0.0, -1.0),
}



CANONICAL_AXES = ("forward", "port", "up")



_DEGENERATE_AREA_REL = 1e-12



_SNAP_REL = 1e-12



_BASELINE_REL = 1e-9



_OVERLAP_REL = 1e-9



_LENGTH_DEGENERATE_REL = 1e-8



_VOLUME_DEGENERATE_REL = 1e-12



_CLOSURE_VOLUME_REL = 1e-9



class MeshContractError(ValueError):
    """The mesh or loading condition violates the geometry contract (refused, not repaired)."""



def _axes_matrix(axes: Optional[Sequence[str]]) -> np.ndarray:
    if axes is None:
        raise MeshContractError(
            "axes must be declared, e.g. ('forward', 'port', 'up'); undeclared axes are refused"
        )
    axes = tuple(axes)
    if len(axes) != 3 or any(a not in _AXIS_UNIT for a in axes):
        raise MeshContractError(f"axes must be three of {sorted(_AXIS_UNIT)}, got {axes!r}")
    r = np.column_stack([_AXIS_UNIT[a] for a in axes])
    if not np.allclose(np.abs(r).sum(axis=0), 1.0) or not np.allclose(np.abs(r).sum(axis=1), 1.0):
        raise MeshContractError(f"axes {axes!r} do not form a permutation of the canonical axes")
    if np.linalg.det(r) < 0:
        raise MeshContractError(
            f"axes {axes!r} declare a left-handed frame; the contract requires right-handed axes"
        )
    return r



def _face_area_vectors(vertices: np.ndarray, faces: np.ndarray) -> np.ndarray:
    a = vertices[faces[:, 0]]
    return 0.5 * np.cross(vertices[faces[:, 1]] - a, vertices[faces[:, 2]] - a)



def _read_only(a: np.ndarray) -> np.ndarray:
    """Snapshot numeric data on immutable storage, including every ndarray base."""
    if isinstance(a, np.ndarray) and type(a) is not np.ndarray:
        raise TypeError("immutable arrays do not support ndarray subclasses")
    a = np.asarray(a)
    if a.dtype.hasobject:
        raise TypeError("immutable arrays cannot contain object references")
    return np.frombuffer(a.tobytes(order="C"), dtype=a.dtype).reshape(a.shape)



def volume_divergence(vertices: np.ndarray, faces: np.ndarray) -> float:
    """Enclosed volume by the divergence theorem with F = (0, 0, z): V = sum A_z * z_centroid."""
    vertices = np.asarray(vertices, float)
    faces = np.asarray(faces)
    av = _face_area_vectors(vertices, faces)
    zc = vertices[faces][:, :, 2].mean(axis=1)
    return float((av[:, 2] * zc).sum())



def _tetra_moments(vertices: np.ndarray, faces: np.ndarray, origin: Optional[np.ndarray]):
    vertices = np.asarray(vertices, float)
    faces = np.asarray(faces)
    if origin is None:
        origin = vertices[np.unique(faces)].mean(axis=0)
    o = np.asarray(origin, float)
    a = vertices[faces[:, 0]] - o
    b = vertices[faces[:, 1]] - o
    c = vertices[faces[:, 2]] - o
    six_v = np.einsum("ij,ij->i", a, np.cross(b, c))
    vol = six_v.sum() / 6.0
    centroid = o + (six_v[:, None] * (a + b + c)).sum(axis=0) / (4.0 * six_v.sum())
    return float(vol), centroid



def volume_tetra(vertices: np.ndarray, faces: np.ndarray, origin: Optional[np.ndarray] = None) -> float:
    """Enclosed volume as a sum of signed tetrahedra (origin, v0, v1, v2)."""
    return _tetra_moments(vertices, faces, origin)[0]


MeshContractError.__module__ = "digitalmodel.naval_architecture.mesh_hydrostatics"
