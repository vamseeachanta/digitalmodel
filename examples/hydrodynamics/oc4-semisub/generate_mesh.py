"""Generate a wetted OC4 mesh using open-source Gmsh, without a solver.

Physical dimensions: NREL/TP-5000-60601 (2014), Tables 3-1 and 3-2,
pp. 7-8; https://www.nlr.gov/docs/fy14osti/60601.pdf. See README.md
for the complete dimension and provenance table. Other numbers below
are numerical discretization choices, coordinate algebra or GDF syntax.
"""

import argparse
import math
from pathlib import Path

import numpy as np


# Table 3-2, column centres (m); retain the published coordinate rounding.
CENTRES = ((14.43, 25.0), (-28.87, 0.0), (14.43, -25.0))
# Table 3-2, lower delta pontoons, lower Y pontoons and cross braces (m).
MEMBERS = (
    ((4, 19, -17), (-18.47, 6, -17)),
    ((-18.47, -6, -17), (4, -19, -17)),
    ((14.43, -13, -17), (14.43, 13, -17)),
    ((1.625, 2.815, -17), (8.4, 14.6, -17)),
    ((-3.25, 0, -17), (-16.87, 0, -17)),
    ((1.625, -2.815, -17), (8.4, -14.6, -17)),
    ((1.625, 2.815, -16.2), (11.43, 19.81, 9.13)),
    ((-3.25, 0, -16.2), (-22.87, 0, 9.13)),
    ((1.625, -2.815, -16.2), (11.43, -19.81, 9.13)),
)


def build_geometry(gmsh):
    """Fuse exterior solids and clip at SWL; extensions are buried in columns."""
    occ = gmsh.model.occ
    # Table 3-1: MC diameter 6.5, draft 20, freeboard 10 m.
    solids = [(3, occ.addCylinder(0, 0, -20, 0, 0, 30, 6.5 / 2))]
    for x, y in CENTRES:
        # Table 3-1: BC diameter 24, height 6; UC diameter 12, length 26 m.
        solids.append((3, occ.addCylinder(x, y, -20, 0, 0, 6, 24 / 2)))
        solids.append((3, occ.addCylinder(x, y, -14, 0, 0, 26, 12 / 2)))
    for start, end in MEMBERS:
        start, end = np.array(start, float), np.array(end, float)
        unit = (end - start) / np.linalg.norm(end - start)
        # Numerical overlap only: bury end caps in adjacent columns so union
        # removes them. Extension exceeds the Table 3-1 member diameter 1.6 m.
        extension = 1.6 * 2
        a, b = start - extension * unit, end + extension * unit
        solids.append((3, occ.addCylinder(*a, *(b - a), 1.6 / 2)))
    united, _ = occ.fuse(solids[:1], solids[1:])
    # Clipping box bounds derive from the published offset spacing and draft.
    clip = occ.addBox(-50, -50, -20, 100, 100, 20)
    wetted, _ = occ.intersect(united, [(3, clip)])
    occ.synchronize()
    if len(wetted) != 1:
        raise ValueError("Expected one connected submerged solid")
    return wetted


def generate_mesh(output, *, gravity, mesh_size=1.25):
    """Write low-order GDF in metres; gravity is an explicit header input."""
    if not math.isfinite(gravity) or gravity <= 0:
        raise ValueError("gravity must be finite and positive")
    if not math.isfinite(mesh_size) or mesh_size <= 0:
        raise ValueError("mesh_size must be finite and positive")
    import gmsh

    gmsh.initialize()
    try:
        gmsh.option.setNumber("General.Terminal", 0)
        gmsh.model.add("oc4_public_definition")
        wetted = build_geometry(gmsh)
        # Permit refinement at cylinder intersections and on slender members.
        gmsh.option.setNumber("Mesh.MeshSizeMin", 0)
        gmsh.option.setNumber("Mesh.MeshSizeMax", mesh_size)
        gmsh.option.setNumber("Mesh.MeshSizeFromCurvature", 16)
        gmsh.option.setNumber("Mesh.Algorithm", 6)
        gmsh.model.mesh.generate(2)
        tags, xyz, _ = gmsh.model.mesh.getNodes()
        nodes = dict(zip(tags, np.array(xyz).reshape(-1, 3)))
        panels = []
        for _, surface in gmsh.model.getBoundary(wetted, oriented=True):
            # The artificial planar SWL cap is for CAD closure only.
            bounds = gmsh.model.getBoundingBox(2, abs(surface))
            if abs(bounds[2]) < 1e-6 and abs(bounds[5]) < 1e-6:
                continue
            kinds, _, connections = gmsh.model.mesh.getElements(2, abs(surface))
            for kind, connection in zip(kinds, connections):
                if kind != 2:  # Gmsh linear triangle element code
                    raise ValueError("Expected linear triangle elements")
                for tri in np.array(connection).reshape(-1, 3):
                    points = np.array([nodes[tag] for tag in tri])
                    # Gmsh's volume-boundary mesh already has outward winding;
                    # signed OCC surface tags must not reverse it a second time.
                    points[np.abs(points[:, 2]) < 1e-7, 2] = 0
                    panels.append(points[[0, 1, 2, 2]])
        write_gdf(output, np.array(panels), gravity)
    finally:
        gmsh.finalize()


def write_gdf(output, panels, gravity):
    """Export outward normals; repeated last vertex denotes a triangle."""
    with Path(output).open("w", encoding="ascii", newline="\n") as stream:
        stream.write("OC4 public definition; NREL/TP-5000-60601 Tables 3-1,3-2\n")
        stream.write(f"1.0 {gravity:.10g}\n0 0\n{len(panels)}\n")
        for panel in panels:
            for x, y, z in panel:
                stream.write(f"{x:.10f} {y:.10f} {z:.10f}\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    parser.add_argument(
        "--gravity", required=True, type=float, help="m/s2; GDF header input"
    )
    parser.add_argument(
        "--mesh-size", default=1.25, type=float, help="numerical target edge length (m)"
    )
    args = parser.parse_args()
    generate_mesh(args.output, gravity=args.gravity, mesh_size=args.mesh_size)


if __name__ == "__main__":
    main()
