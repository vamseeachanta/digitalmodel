"""Mesh validation for the mesh hydrostatics adapter."""
from __future__ import annotations

import numpy as np
import hashlib
import json
from typing import Optional, Sequence
from .mesh_geometry import (MeshContractError, UNIT_SCALE, _axes_matrix, _face_area_vectors,
    _read_only, _DEGENERATE_AREA_REL, _BASELINE_REL)
from .mesh_topology import _check_topology


class TriMesh:
    """Closed, oriented triangle mesh in the canonical SI frame after contract checks.

    ``vertices`` (N, 3) and ``faces`` (M, 3) are given in the caller's declared ``units`` and
    ``axes``; ``self.vertices`` / ``self.faces`` are read-only copies in metres in the
    canonical frame.
    """

    def __init__(
        self,
        vertices,
        faces,
        *,
        units: Optional[str],
        axes: Optional[Sequence[str]],
        flip_normals: bool = False,
    ) -> None:
        if units not in UNIT_SCALE:
            raise MeshContractError(
                f"units must be declared as one of {sorted(UNIT_SCALE)}, got {units!r}"
            )
        r = _axes_matrix(axes)
        v = np.array(vertices, dtype=float, copy=True)
        f = np.array(faces, copy=True)
        if v.ndim != 2 or v.shape[1] != 3 or v.shape[0] < 4:
            raise MeshContractError(f"vertices must be an (N>=4, 3) array, got shape {v.shape}")
        if f.ndim != 2 or f.shape[1] != 3 or f.shape[0] < 4:
            raise MeshContractError(f"faces must be an (M>=4, 3) array, got shape {f.shape}")
        if not np.issubdtype(f.dtype, np.integer):
            raise MeshContractError("faces must be integer vertex indices")
        f = f.astype(np.int64)
        if f.min() < 0 or f.max() >= v.shape[0]:
            raise MeshContractError("face vertex index out of range")
        if not np.all(np.isfinite(v)):
            raise MeshContractError("mesh has non-finite vertex coordinates")

        canon = (v @ r.T) * UNIT_SCALE[units]
        used = np.unique(f)
        extent = canon[used].max(axis=0) - canon[used].min(axis=0)
        diag = float(np.linalg.norm(extent))
        if diag <= 0:
            raise MeshContractError("mesh has zero extent")

        repeated = (f[:, 0] == f[:, 1]) | (f[:, 1] == f[:, 2]) | (f[:, 2] == f[:, 0])
        areas = np.linalg.norm(_face_area_vectors(canon, f), axis=1)
        degenerate = repeated | (areas <= _DEGENERATE_AREA_REL * diag**2)
        if np.any(degenerate):
            raise MeshContractError(
                f"mesh has {int(degenerate.sum())} degenerate (zero-area or repeated-index) faces, "
                f"first at index {int(np.nonzero(degenerate)[0][0])}"
            )

        if flip_normals:
            f = f[:, ::-1].copy()
        _check_topology(canon, f, diag)

        zmin = float(canon[used, 2].min())
        if abs(zmin) > _BASELINE_REL * diag:
            raise MeshContractError(
                f"baseline (lowest vertex) must be at canonical z = 0 (draft datum); found z = {zmin!r} m"
            )

        self._vertices = _read_only(canon)
        self._faces = _read_only(f)
        digest = hashlib.sha256()
        digest.update(self._vertices.tobytes())
        digest.update(self._faces.tobytes())
        digest.update(json.dumps([units, list(axes), bool(flip_normals)]).encode())
        self._source_digest = digest.hexdigest()
        self._units = units
        self._axes = tuple(axes)
        self._scale_diag = diag

    def __setstate__(self, state) -> None:
        self.__dict__.update(state)
        self._vertices = _read_only(self._vertices)
        self._faces = _read_only(self._faces)

    @property
    def vertices(self) -> np.ndarray:
        return self._vertices.view()

    @property
    def faces(self) -> np.ndarray:
        return self._faces.view()

    @property
    def source_digest(self) -> str:
        return self._source_digest

    @property
    def units(self) -> str:
        return self._units

    @property
    def axes(self) -> tuple[str, ...]:
        return self._axes

    @property
    def scale_diag(self) -> float:
        return self._scale_diag

    @property
    def bounds(self) -> tuple[np.ndarray, np.ndarray]:
        used = self._vertices[np.unique(self._faces)]
        return _read_only(used.min(axis=0)), _read_only(used.max(axis=0))


TriMesh.__module__ = "digitalmodel.naval_architecture.mesh_hydrostatics"
