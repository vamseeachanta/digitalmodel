"""Analytic Wigley parabolic hull as a watertight STL, for solver verification.

The Wigley hull is the standard analytic test hull for wave resistance: its
offsets are a closed-form function, so the geometry carries no CAD or
tessellation uncertainty and a resistance result can be compared against the
1983 cooperative towing experiments (Kajitani et al.) and decades of
published computations.

Half-breadth below the waterline, with ``xi = 2 x' / L`` measured from
midships and ``zeta = depth below the waterline``::

    y(x', zeta) = (B/2) * (1 - xi**2) * (1 - (zeta / T)**2)

Above the waterline the waterline section is extruded vertically to a
freeboard so the hull has a deck to cap, which is what the hull-resistance
case builder expects (a hull that ends at the waterline has no surface for
the air side of the free surface to meet).

Coordinates follow the ``hull_manifest`` convention ``aft_perpendicular_keel``:
x from the aft perpendicular forward (0 .. L), y to port, z up from the keel
(0 .. T + freeboard); the design waterline is at ``z = T``.
"""

from __future__ import annotations

import hashlib
import json
import struct
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator

import numpy as np

__all__ = [
    "WigleyHull",
    "wigley_offsets",
    "wigley_stl",
    "wigley_manifest",
    "write_wigley",
]


@dataclass(frozen=True)
class WigleyHull:
    """Principal dimensions. The classic model is ``B/L = 0.1``, ``T/L = 0.0625``."""

    length: float = 3.014
    beam: float = 0.3014
    draft: float = 0.1884
    freeboard: float = 0.0942
    nx: int = 200
    nz_wet: int = 40
    nz_dry: int = 10

    def __post_init__(self) -> None:
        for name in ("length", "beam", "draft", "freeboard"):
            if getattr(self, name) <= 0:
                raise ValueError(f"{name} must be positive")
        if self.nx < 4 or self.nz_wet < 2 or self.nz_dry < 1:
            raise ValueError("station counts too small to close the surface")

    @property
    def height(self) -> float:
        return self.draft + self.freeboard

    # -- analytic properties, used to check the tessellation --------------------
    @property
    def wetted_surface(self) -> float:
        """Numerical integral of the exact surface below the waterline (both sides)."""
        x = np.linspace(0.0, self.length, 2001)
        z = np.linspace(0.0, self.draft, 401)
        X, Z = np.meshgrid(x, z, indexing="ij")
        Y = wigley_offsets(self, X, Z)
        dydx = np.gradient(Y, x, axis=0)
        dydz = np.gradient(Y, z, axis=1)
        integrand = np.sqrt(1.0 + dydx**2 + dydz**2)
        return 2.0 * float(np.trapz(np.trapz(integrand, z, axis=1), x))

    @property
    def displacement(self) -> float:
        """Exact: 2 * integral of y over x and zeta = (8/9) * (B/2) * L * T."""
        return (8.0 / 9.0) * (self.beam / 2.0) * self.length * self.draft


def wigley_offsets(hull: WigleyHull, x: np.ndarray, z: np.ndarray) -> np.ndarray:
    """Port half-breadth at (x from AP, z from keel). Extruded above ``z = T``."""
    xi = 2.0 * (np.asarray(x, dtype=float) - hull.length / 2.0) / hull.length
    zeta = np.clip(hull.draft - np.asarray(z, dtype=float), 0.0, hull.draft)
    y = (hull.beam / 2.0) * (1.0 - xi**2) * (1.0 - (zeta / hull.draft) ** 2)
    return np.clip(y, 0.0, None)


def _grid(hull: WigleyHull) -> tuple[np.ndarray, np.ndarray]:
    x = np.linspace(0.0, hull.length, hull.nx + 1)
    # cluster z near the waterline where curvature is highest
    zw = hull.draft * (1.0 - np.cos(np.linspace(0.0, np.pi / 2, hull.nz_wet + 1)))
    zd = hull.draft + hull.freeboard * np.linspace(0.0, 1.0, hull.nz_dry + 1)[1:]
    return x, np.concatenate([zw, zd])


def _triangles(hull: WigleyHull) -> Iterator[tuple[np.ndarray, np.ndarray, np.ndarray]]:
    """Outward-facing triangles: port side, starboard side, deck cap.

    Bow and stern have zero breadth, and the keel line has zero breadth, so the
    two sides meet there and the surface is closed without extra faces.
    Degenerate (zero-area) triangles at those seams are skipped.
    """
    x, z = _grid(hull)
    X, Z = np.meshgrid(x, z, indexing="ij")
    Y = wigley_offsets(hull, X, Z)

    def emit(a, b, c):
        # Skip zero-area seams, and skip panels lying entirely in the centre
        # plane (y == 0 at bow, stern and keel): those are where port and
        # starboard meet, and emitting them from both sides would place two
        # coincident, opposite-facing interior triangles on the seam.
        if max(abs(a[1]), abs(b[1]), abs(c[1])) < 1e-12:
            return
        if np.linalg.norm(np.cross(b - a, c - a)) > 1e-14:
            yield a, b, c

    ni, nk = X.shape
    for i in range(ni - 1):
        for k in range(nk - 1):
            p00 = np.array([X[i, k], Y[i, k], Z[i, k]])
            p10 = np.array([X[i + 1, k], Y[i + 1, k], Z[i + 1, k]])
            p01 = np.array([X[i, k + 1], Y[i, k + 1], Z[i, k + 1]])
            p11 = np.array([X[i + 1, k + 1], Y[i + 1, k + 1], Z[i + 1, k + 1]])
            # port side (+y), normal must point to +y: order so that
            # (p10-p00) x (p01-p00) has positive y for the port panel
            yield from emit(p00, p01, p10)
            yield from emit(p10, p01, p11)
            # starboard: mirror y and reverse winding
            m = np.array([1.0, -1.0, 1.0])
            yield from emit(p00 * m, p10 * m, p01 * m)
            yield from emit(p10 * m, p11 * m, p01 * m)
    # deck cap at z = height: fan between the port and starboard deck curves
    top = nk - 1
    for i in range(ni - 1):
        a = np.array([X[i, top], Y[i, top], Z[i, top]])
        b = np.array([X[i + 1, top], Y[i + 1, top], Z[i + 1, top]])
        am = a * np.array([1.0, -1.0, 1.0])
        bm = b * np.array([1.0, -1.0, 1.0])
        # normal +z: counter-clockwise seen from above
        yield from emit(am, bm, b)
        yield from emit(am, b, a)


def wigley_stl(hull: WigleyHull, path: Path, solid_name: str = "hull") -> int:
    """Write a binary STL; returns the triangle count."""
    tris = list(_triangles(hull))
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("wb") as fh:
        header = f"wigley L={hull.length} B={hull.beam} T={hull.draft} {solid_name}".encode()[:80]
        fh.write(header.ljust(80, b"\0"))
        fh.write(struct.pack("<I", len(tris)))
        for a, b, c in tris:
            n = np.cross(b - a, c - a)
            n = n / (np.linalg.norm(n) or 1.0)
            fh.write(struct.pack("<3f", *n))
            for p in (a, b, c):
                fh.write(struct.pack("<3f", *p))
            fh.write(b"\0\0")
    return len(tris)


def wigley_manifest(hull: WigleyHull, stl_path: Path, n_triangles: int) -> dict:
    """A ``hull_manifest.json`` for the analytic hull (see ``hull_manifest.py``)."""
    stl_path = Path(stl_path)
    return {
        "source_file": stl_path.name,
        "source_sha256": hashlib.sha256(stl_path.read_bytes()).hexdigest(),
        "units_in": "m",
        "scale_to_m": 1.0,
        "orientation": {"x": "forward", "y": "port", "z": "up"},
        "origin": "aft_perpendicular_keel",
        "lpp_m": hull.length,
        "beam_m": hull.beam,
        "draft_m": hull.draft,
        "wetted_surface_m2": hull.wetted_surface,
        "displacement_m3": hull.displacement,
        "watertight": True,
        "n_triangles": n_triangles,
        "bbox_min_m": [0.0, -hull.beam / 2.0, 0.0],
        "bbox_max_m": [hull.length, hull.beam / 2.0, hull.height],
        "provenance": {
            "generator": "digitalmodel.naval_architecture.wigley_geometry",
            "definition": "y = (B/2)(1-(2x'/L)^2)(1-(zeta/T)^2); waterline section extruded to the freeboard; deck capped",
            "stations": {"nx": hull.nx, "nz_wet": hull.nz_wet, "nz_dry": hull.nz_dry},
            "freeboard_m": hull.freeboard,
        },
    }


def write_wigley(out_dir: Path, hull: WigleyHull = WigleyHull()) -> tuple[Path, Path]:
    """Write ``hull.stl`` and ``hull_manifest.json`` into *out_dir*."""
    out_dir = Path(out_dir)
    stl = out_dir / "hull.stl"
    n = wigley_stl(hull, stl)
    manifest = out_dir / "hull_manifest.json"
    manifest.write_text(json.dumps(wigley_manifest(hull, stl, n), indent=2) + "\n")
    return stl, manifest
