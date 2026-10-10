"""Mesh topology for the mesh hydrostatics adapter."""
from __future__ import annotations

import numpy as np
from .mesh_geometry import MeshContractError, _face_area_vectors, _OVERLAP_REL


def _check_edges(faces: np.ndarray) -> None:
    directed = faces[:, [0, 1, 1, 2, 2, 0]].reshape(-1, 2)
    undirected = np.sort(directed, axis=1)
    _, counts = np.unique(undirected, axis=0, return_counts=True)
    if np.any(counts > 2):
        raise MeshContractError(
            f"mesh is non-manifold: {int((counts > 2).sum())} edges are shared by more than two faces"
        )
    if np.any(counts == 1):
        raise MeshContractError(
            f"mesh is open (not watertight): {int((counts == 1).sum())} boundary edges"
        )
    uniq_directed = np.unique(directed, axis=0)
    if uniq_directed.shape[0] != directed.shape[0]:
        raise MeshContractError(
            "mesh has inconsistent face orientation: a directed edge is used by two faces"
        )



def _check_topology(canon: np.ndarray, faces: np.ndarray, diag: float) -> None:
    """Edge, vertex-fan, per-component orientation and component-overlap checks."""
    from scipy.sparse import coo_matrix
    from scipy.sparse.csgraph import connected_components

    _check_edges(faces)
    m = faces.shape[0]
    n = np.int64(canon.shape[0])
    directed = faces[:, [0, 1, 1, 2, 2, 0]].reshape(-1, 2)
    key = directed[:, 0] * n + directed[:, 1]
    rkey = directed[:, 1] * n + directed[:, 0]
    order = np.argsort(key)
    twin = order[np.searchsorted(key[order], rkey)]  # every reverse exists exactly once here

    e = np.arange(3 * m)
    face_e, k_e = e // 3, e % 3
    face_t, k_t = twin // 3, twin % 3
    # edge (a -> b) of face f and its twin (b -> a) of face g are neighbours in the fan of a
    # and in the fan of b: link the corresponding corners (corner id = 3 * face + slot).
    rows = np.concatenate([3 * face_e + k_e, 3 * face_e + (k_e + 1) % 3])
    cols = np.concatenate([3 * face_t + (k_t + 1) % 3, 3 * face_t + k_t])
    graph = coo_matrix((np.ones(rows.size), (rows, cols)), shape=(3 * m, 3 * m))
    _, labels = connected_components(graph, directed=False)
    pairs = np.unique(np.column_stack([faces.ravel(), labels]), axis=0)
    fans = np.bincount(pairs[:, 0])
    if np.any(fans > 1):
        bad = np.nonzero(fans > 1)[0]
        raise MeshContractError(
            f"mesh has {bad.size} non-manifold vertex/vertices (several face fans meet at one "
            f"vertex), first at index {int(bad[0])}"
        )

    _check_components(canon, faces, diag, face_e, face_t)


def _check_components(canon, faces, diag, face_e, face_t):
    from scipy.sparse import coo_matrix
    from scipy.sparse.csgraph import connected_components

    m = faces.shape[0]
    fgraph = coo_matrix((np.ones(3 * m), (face_e, face_t)), shape=(m, m))
    ncomp, flabels = connected_components(fgraph, directed=False)
    av = _face_area_vectors(canon, faces)
    zc = canon[faces][:, :, 2].mean(axis=1)
    vols = np.bincount(flabels, weights=av[:, 2] * zc, minlength=ncomp)
    if np.any(vols <= 0):
        raise MeshContractError(
            f"{int((vols <= 0).sum())} of {ncomp} mesh component(s) have inward normals "
            "(signed volume <= 0: reversed body or internal cavity); pass flip_normals=True only "
            "if the whole mesh is reversed"
        )
    if ncomp > 1:
        lo = np.full((ncomp, 3), np.inf)
        hi = np.full((ncomp, 3), -np.inf)
        pts = canon[faces]  # (m, 3, 3)
        np.minimum.at(lo, flabels, pts.min(axis=1))
        np.maximum.at(hi, flabels, pts.max(axis=1))
        tol = _OVERLAP_REL * diag
        for i in range(ncomp):
            ov = np.minimum(hi[i], hi[i + 1:]) - np.maximum(lo[i], lo[i + 1:])
            clash = np.all(ov > tol, axis=1)
            if np.any(clash):
                j = i + 1 + int(np.nonzero(clash)[0][0])
                raise MeshContractError(
                    f"mesh components {i} and {j} have overlapping bounding boxes (overlapping or "
                    "nested shells are refused)"
                )

