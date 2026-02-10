#!/usr/bin/env python
"""Convert Rhino-exported GDF meshes to standard WAMIT GDF format.

Problem:
    Rhino->WAMIT export uses non-standard format:
    - Header line 4 contains number of vertex LINES (not panel count)
    - Each panel is a triangle defined by 3 vertex lines
    - Triangle pairs form quads but normals point inward (wrong)

Standard WAMIT GDF format:
    - Header line 4 contains NPAN (number of panels)
    - Each panel has 4 vertex lines (quad)
    - Panel normals point outward from body (into fluid)

Usage:
    python convert_rhino_gdf.py input.gdf [output.gdf]
    python convert_rhino_gdf.py input.gdf output.gdf --mode tri
"""

import argparse
import sys
from pathlib import Path


def parse_args():
    parser = argparse.ArgumentParser(
        description="Convert Rhino-format GDF to standard WAMIT GDF"
    )
    parser.add_argument("input", type=Path, help="Input Rhino GDF file")
    parser.add_argument(
        "output",
        type=Path,
        nargs="?",
        help="Output WAMIT GDF file (default: input_wamit.gdf)",
    )
    parser.add_argument(
        "--mode",
        choices=["quad", "tri"],
        default="quad",
        help="quad: pair triangles into quads (default); tri: keep as degenerate quads",
    )
    parser.add_argument(
        "--no-flip",
        action="store_true",
        help="Do NOT reverse winding order (skip normal flip)",
    )
    return parser.parse_args()


def read_rhino_gdf(filepath):
    """Read Rhino-format GDF file.

    Returns:
        tuple: (header_lines, triangles)
            header_lines: list of 4 header strings
            triangles: list of [(x,y,z), (x,y,z), (x,y,z)] triangles
    """
    with open(filepath, "r") as f:
        lines = [line.rstrip("\n") for line in f]

    if len(lines) < 4:
        raise ValueError("Invalid GDF file: less than 4 header lines")

    header_lines = lines[:4]
    vertex_lines = lines[4:]

    vertex_count = int(header_lines[3].strip())

    if len(vertex_lines) != vertex_count:
        raise ValueError(
            f"Vertex count mismatch: header says {vertex_count}, "
            f"but found {len(vertex_lines)} vertex lines"
        )

    if vertex_count % 3 != 0:
        raise ValueError(
            f"Vertex count {vertex_count} is not divisible by 3. "
            f"Expected triangular panels with 3 vertices each."
        )

    # Parse vertex coordinates into triangles
    triangles = []
    for i in range(0, vertex_count, 3):
        tri = []
        for j in range(3):
            parts = vertex_lines[i + j].split()
            tri.append((float(parts[0]), float(parts[1]), float(parts[2])))
        triangles.append(tri)

    return header_lines, triangles


def _fmt_vertex(v):
    """Format vertex as GDF coordinate line."""
    return f"  {v[0]:.5f} {v[1]:.5f} {v[2]:.5f}"


def pair_triangles_into_quads(triangles, inward_normals=True):
    """Pair consecutive triangle pairs into proper quads.

    Rhino exports structured meshes where each pair of triangles forms a quad:
        Tri1: (A, B, C)
        Tri2: (A, C, D)
        Quad: (A, B, C, D)

    If inward_normals=True, ensure all normals point into the body
    (toward centroid). This is the WAMIT/OrcaWave convention.
    """
    import numpy as np

    if len(triangles) % 2 != 0:
        raise ValueError(
            f"Odd number of triangles ({len(triangles)}), cannot pair into quads"
        )

    # Compute body centroid from all vertices
    all_verts = [v for tri in triangles for v in tri]
    centroid = np.mean(all_verts, axis=0)
    print(f"  Body centroid: ({centroid[0]:.1f}, {centroid[1]:.1f}, {centroid[2]:.1f})")

    quads = []
    n_flipped = 0
    for i in range(0, len(triangles), 2):
        tri1 = triangles[i]  # (A, B, C)
        tri2 = triangles[i + 1]  # (A, C, D) or similar

        a, b, c = tri1
        _, _, d = tri2

        quad = [a, b, c, d]

        if inward_normals:
            # Check normal direction relative to centroid
            va = np.array(a)
            vb = np.array(b)
            vc = np.array(c)
            normal = np.cross(vb - va, vc - va)
            panel_center = (va + vb + vc + np.array(d)) / 4
            # Vector from panel center to centroid
            to_centroid = centroid - panel_center
            # Normal should point toward centroid (inward)
            if np.dot(normal, to_centroid) < 0:
                # Wrong direction: reverse winding
                quad = [d, c, b, a]
                n_flipped += 1

        quads.append(quad)

    print(f"  Flipped {n_flipped}/{len(quads)} panels for inward normals")
    return quads


def triangles_to_degenerate_quads(triangles, inward_normals=True):
    """Convert triangles to degenerate quads (V4=V3)."""
    import numpy as np

    all_verts = [v for tri in triangles for v in tri]
    centroid = np.mean(all_verts, axis=0)

    quads = []
    n_flipped = 0
    for tri in triangles:
        a, b, c = tri
        quad = [a, b, c, c]

        if inward_normals:
            va, vb, vc = np.array(a), np.array(b), np.array(c)
            normal = np.cross(vb - va, vc - va)
            panel_center = (va + vb + vc) / 3
            to_centroid = centroid - panel_center
            if np.dot(normal, to_centroid) < 0:
                quad = [c, b, a, a]
                n_flipped += 1

        quads.append(quad)

    print(f"  Flipped {n_flipped}/{len(quads)} panels for inward normals")
    return quads


def write_wamit_gdf(filepath, header_lines, quads):
    """Write standard WAMIT GDF file."""
    with open(filepath, "w") as f:
        # Header lines 1-3
        for h in header_lines[:3]:
            f.write(h + "\n")
        # Line 4: panel count
        f.write(f"{len(quads):>12}\n")
        # Panel vertices (4 per panel)
        for quad in quads:
            for v in quad:
                f.write(_fmt_vertex(v) + "\n")


def main():
    args = parse_args()

    if not args.input.exists():
        print(f"Error: Input file not found: {args.input}", file=sys.stderr)
        sys.exit(1)

    if args.output is None:
        output_path = args.input.parent / f"{args.input.stem}_wamit{args.input.suffix}"
    else:
        output_path = args.output

    try:
        print(f"Reading Rhino GDF: {args.input}")
        header_lines, triangles = read_rhino_gdf(args.input)
        print(f"  Found {len(triangles)} triangular panels")

        fix_normals = not args.no_flip

        if args.mode == "quad":
            print(f"  Pairing into quads (inward_normals={fix_normals})...")
            quads = pair_triangles_into_quads(triangles, inward_normals=fix_normals)
            print(f"  Created {len(quads)} quadrilateral panels")
        else:
            print(f"  Converting to degenerate quads (inward_normals={fix_normals})...")
            quads = triangles_to_degenerate_quads(triangles, inward_normals=fix_normals)
            print(f"  Created {len(quads)} degenerate quad panels")

        print(f"Writing WAMIT GDF: {output_path}")
        write_wamit_gdf(output_path, header_lines, quads)

        expected_lines = 4 + len(quads) * 4
        print(f"  Panels: {len(quads)}")
        print(f"  Output lines: {expected_lines}")
        print("Conversion successful!")

    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
