# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Generate reproducible full wet hulls and evaluate DF2 without solver imports.

The equal end breadth is reconstructed from the specified full displacement,
length, beam, draft and taper lengths. This is not recovery of an original mesh.
No symmetry reflection is applied when integrating the supplied surface.
"""

import argparse
import hashlib
import json
from pathlib import Path

import numpy as np

LENGTH, BEAM, DRAFT, TAPER = 231.0, 49.0, 11.0, 16.5
VOLUME, MASS, RHO, GRAVITY = 116149.2, 119052930.0, 1025.0, 9.80665
END_HALF_BREADTH = (VOLUME / DRAFT - BEAM * (LENGTH - TAPER)) / (2 * TAPER)
# (end intervals, middle intervals, transverse intervals, vertical intervals).
# Even transverse counts split the bottom and transoms at the centreline.
LEVELS = {"coarse": (5, 59, 14, 3), "medium": (7, 80, 20, 5), "fine": (10, 118, 28, 6)}
TARGET_SIZES = {"coarse": 3.5, "medium": 2.47, "fine": 1.75}


def panels_for_level(level):
    """Return outward planar quads with matching edges and an open waterline."""
    ne, nm, ny, nz = LEVELS[level]
    x = np.concatenate(
        (
            np.linspace(0, TAPER, ne + 1)[:-1],
            np.linspace(TAPER, LENGTH - TAPER, nm + 1)[:-1],
            np.linspace(LENGTH - TAPER, LENGTH, ne + 1),
        )
    )
    width = END_HALF_BREADTH + (BEAM / 2 - END_HALF_BREADTH) * np.minimum(
        np.minimum(x, LENGTH - x) / TAPER, 1
    )
    y = np.linspace(-1, 1, ny + 1)
    z = np.linspace(-DRAFT, 0, nz + 1)

    def point(i, j, k):
        return [x[i], width[i] * y[j], z[k]]

    panels = []
    for i in range(len(x) - 1):
        for j in range(ny):
            panels.append(
                [
                    point(i, j, 0),
                    point(i, j + 1, 0),
                    point(i + 1, j + 1, 0),
                    point(i + 1, j, 0),
                ]
            )
        for k in range(nz):
            panels.append(
                [
                    point(i, 0, k),
                    point(i + 1, 0, k),
                    point(i + 1, 0, k + 1),
                    point(i, 0, k + 1),
                ]
            )
            panels.append(
                [
                    point(i, ny, k + 1),
                    point(i + 1, ny, k + 1),
                    point(i + 1, ny, k),
                    point(i, ny, k),
                ]
            )
    for j in range(ny):
        for k in range(nz):
            panels.append(
                [
                    point(0, j, k),
                    point(0, j, k + 1),
                    point(0, j + 1, k + 1),
                    point(0, j + 1, k),
                ]
            )
            i = len(x) - 1
            panels.append(
                [
                    point(i, j, k),
                    point(i, j + 1, k),
                    point(i, j + 1, k + 1),
                    point(i, j, k + 1),
                ]
            )
    return np.asarray(panels)


def encode_gdf(panels, symmetry=(0, 0)):
    """Serialize metres, gravity and explicit symmetry flags with fixed precision."""
    lines = [
        "Reconstructed full-body diffraction hull",
        "1.0 9.80665",
        f"{symmetry[0]} {symmetry[1]}",
        str(len(panels)),
    ]
    for vertex in np.asarray(panels).reshape(-1, 3):
        lines.append(" ".join(f"{0.0 if abs(v) < 0.5e-9 else v:.9f}" for v in vertex))
    return ("\n".join(lines) + "\n").encode("ascii")


def gdf_bytes(level):
    """Generate a named level without writing any file."""
    return encode_gdf(panels_for_level(level))


def read_gdf(path):
    """Read finite WAMIT quads, scaling coordinates by ULEN without reflection."""
    lines = Path(path).read_text(encoding="ascii").splitlines()
    if len(lines) < 4:
        raise ValueError("incomplete GDF header")
    ulen, gravity = map(float, lines[1].split())
    symmetry = tuple(map(int, lines[2].split()))
    count = int(lines[3].strip())
    if len(symmetry) != 2 or any(s not in (0, 1) for s in symmetry) or count <= 0:
        raise ValueError("invalid GDF count or symmetry flags")
    if not np.isfinite([ulen, gravity]).all() or min(ulen, gravity) <= 0:
        raise ValueError("invalid GDF units or gravity")
    tokens = " ".join(lines[4:]).replace("D", "E").replace("d", "e").split()
    if len(tokens) != count * 12:
        raise ValueError("GDF panel count differs from coordinate count")
    panels = np.array(list(map(float, tokens))).reshape(count, 4, 3) * ulen
    if not np.isfinite(panels).all():
        raise ValueError("nonfinite GDF coordinates")
    return dict(panels=panels, symmetry=symmetry, ulen=ulen, gravity=gravity)


def check_symmetry(mesh, declared):
    """Require header planes to match the campaign declaration in both directions.

    GDF ISX reflects x=0 (yz); ISY reflects y=0 (xz). This campaign contract
    is deliberately stricter than backend full-mesh symmetry optimizations.
    """
    flags = read_gdf(mesh)["symmetry"]
    expected = {"none": (0, 0), "xz": (0, 1), "yz": (1, 0), "xz+yz": (1, 1)}
    if declared not in expected or flags != expected[declared]:
        raise ValueError("mesh header symmetry disagrees with specification symmetry")
    return flags


def _integrate(parsed):
    """Integrate signed tetrahedra; close only boundaries on zero-coordinate planes."""
    panels = parsed["panels"]
    if np.max(panels[:, :, 2]) > 1e-8:
        raise ValueError("DF2 requires a wet surface at waterline z=0")
    edges = {}
    for panel in panels:
        vertices = [tuple(v) for v in panel.round(9)]
        if len(set(vertices)) != 4:
            raise ValueError("degenerate quad")
        for a, b in zip(vertices, vertices[1:] + vertices[:1]):
            edges.setdefault(tuple(sorted((a, b))), []).append((a, b))
    area = 0.0
    for occurrences in edges.values():
        if len(occurrences) == 2:
            if occurrences[0] != occurrences[1][::-1]:
                raise ValueError("inconsistent panel orientation")
            continue
        if len(occurrences) != 1:
            raise ValueError("nonmanifold or duplicated surface")
        a, b = occurrences[0]
        if abs(a[2]) < 1e-8 and abs(b[2]) < 1e-8:
            area -= (a[0] * b[1] - b[0] * a[1]) / 2
        elif not any(
            flag and abs(a[axis]) < 1e-8 and abs(b[axis]) < 1e-8
            for axis, flag in enumerate(parsed["symmetry"])
        ):
            raise ValueError("open boundary below waterline")
    triangles = np.concatenate((panels[:, [0, 1, 2]], panels[:, [0, 2, 3]]))
    a, b, c = triangles[:, 0], triangles[:, 1], triangles[:, 2]
    normal = np.cross(b - a, c - a)
    if np.any(np.linalg.norm(normal, axis=1) < 1e-12):
        raise ValueError("zero-area panel")
    # Warped quad faces cannot be integrated as a declared planar panel.
    if np.any(
        abs(
            np.einsum(
                "ij,ij->i",
                np.cross(panels[:, 1] - panels[:, 0], panels[:, 2] - panels[:, 0]),
                panels[:, 3] - panels[:, 0],
            )
        )
        > 1e-6
    ):
        raise ValueError("nonplanar quad")
    tetra = np.einsum("ij,ij->i", a, np.cross(b, c)) / 6
    volume = float(np.sum(tetra))
    if volume <= 0 or area <= 0:
        raise ValueError("positive signed volume and waterplane area required")
    centre = np.sum(tetra[:, None] * (a + b + c) / 4, axis=0) / volume
    return volume, float(area), centre.tolist()


def check_df2_buoyancy(mesh, mass_kg, rho=1025, tolerance=0.01):
    """Return PASS/FAIL/NOT_EVALUATED for |rho*V-M|/M, using raw geometry only.

    Gravity cancels in the force ratio. Symmetry flags allow planar closure
    for integration but never multiply volume. No weight report is inferred.
    """
    result = dict(
        decision="NOT_EVALUATED",
        volume_m3=None,
        waterplane_area_m2=None,
        centre_of_buoyancy_m=None,
        relative_error=None,
    )
    try:
        if (
            any(
                isinstance(v, bool) or not np.isfinite(v) or v <= 0
                for v in (mass_kg, rho, tolerance)
            )
            or tolerance >= 1
        ):
            raise ValueError(
                "positive finite mass, density and tolerance below one required"
            )
        volume, area, centre = _integrate(read_gdf(mesh))
        error = abs(rho * volume - mass_kg) / mass_kg
        result.update(
            decision="PASS" if error <= tolerance else "FAIL",
            volume_m3=volume,
            waterplane_area_m2=area,
            centre_of_buoyancy_m=centre,
            relative_error=error,
        )
    except (ValueError, OSError, TypeError, OverflowError) as exc:
        result["reason"] = (
            str(exc) if not isinstance(exc, OSError) else "mesh unavailable"
        )
    return result


def generate(output):
    """Write levels and a deterministic manifest; resume only matching files."""
    output = Path(output)
    output.mkdir(parents=True, exist_ok=True)
    manifest = dict(
        method="numpy structured planar quads",
        ulen=1.0,
        gravity=GRAVITY,
        symmetry=[0, 0],
        length_m=LENGTH,
        beam_m=BEAM,
        draft_m=DRAFT,
        taper_length_m=TAPER,
        end_half_breadth_m=END_HALF_BREADTH,
        displacement_m3=VOLUME,
        basis="base spec of the codenamed vessel; equal end breadth reconstructed from displacement",
        limitation="Reconstructed geometry; original surface and panel quality are not qualified.",
        meshes={},
    )
    for level in LEVELS:
        data = gdf_bytes(level)
        path = output / f"{level}.gdf"
        sha = hashlib.sha256(data).hexdigest()
        if path.exists():
            if hashlib.sha256(path.read_bytes()).hexdigest() != sha:
                raise ValueError("existing mesh digest mismatch")
        else:
            path.write_bytes(data)
        report = check_df2_buoyancy(path, MASS)
        if report["decision"] != "PASS":
            raise ValueError("generated mesh fails DF2")
        manifest["meshes"][level] = dict(
            file=path.name,
            sha256=sha,
            bytes=len(data),
            panels=len(panels_for_level(level)),
            target_size_m=TARGET_SIZES[level],
        )
    data = (json.dumps(manifest, indent=2, allow_nan=False) + "\n").encode()
    path = output / "full-body-meshes.json"
    if path.exists() and path.read_bytes() != data:
        raise ValueError("existing manifest digest mismatch")
    if not path.exists():
        path.write_bytes(data)
    return manifest


def main():
    """Generate offline meshes in the requested destination."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    print(json.dumps(generate(args.output), indent=2))


if __name__ == "__main__":
    main()
