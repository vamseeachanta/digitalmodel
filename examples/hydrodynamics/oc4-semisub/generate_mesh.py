# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Generate a wetted OC4 mesh using open-source Gmsh, without a solver.

Physical dimensions: NREL/TP-5000-60601 (2014), Tables 3-1 and 3-2,
pp. 7-8; https://www.nlr.gov/docs/fy14osti/60601.pdf. See README.md
for the complete dimension and provenance table. Other numbers below
are numerical discretization choices, coordinate algebra or GDF syntax.
"""

import argparse
import hashlib
import importlib.util
import json
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


def enforce_span(panels, max_span):
    """Uniform midpoint subdivision preserves conformity and surface winding."""
    if not math.isfinite(max_span) or max_span <= 0:
        raise ValueError("max_span must be finite and positive")
    panels = np.asarray(panels, dtype=float)
    if (panels.ndim != 3 or panels.shape[1:] != (4, 3) or not len(panels)
            or not np.isfinite(panels).all()
            or not np.array_equal(panels[:, 2], panels[:, 3])):
        raise ValueError("Expected finite nonempty triangular panels")
    while max(np.max(np.linalg.norm(panels[:, a] - panels[:, b], axis=1))
              for a, b in ((0, 1), (1, 2), (2, 0))) > max_span - 1e-9:
        a, b, c = panels[:, 0], panels[:, 1], panels[:, 2]
        ab, bc, ca = (a + b) / 2, (b + c) / 2, (c + a) / 2
        panels = np.concatenate([np.stack(tri, axis=1) for tri in
                                 ((a, ab, ca, ca), (ab, b, bc, bc),
                                  (ca, bc, c, c), (ab, bc, ca, ca))])
    return panels


def _checker():
    spec = importlib.util.spec_from_file_location(
        "oc4_mesh_checks", Path(__file__).with_name("check_mesh.py"))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def generate_mesh(output, *, gravity, mesh_size=0.55,
                  max_span=0.8383265196279277, lid_output=None):
    """Write low-order GDF in metres; gravity is an explicit header input."""
    if not math.isfinite(gravity) or gravity <= 0:
        raise ValueError("gravity must be finite and positive")
    if not math.isfinite(mesh_size) or mesh_size <= 0:
        raise ValueError("mesh_size must be finite and positive")
    import gmsh

    gmsh.initialize()
    try:
        gmsh.option.setNumber("General.Terminal", 0)
        gmsh.option.setNumber("General.NumThreads", 4)
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
        panels, lid = [], []
        for _, surface in gmsh.model.getBoundary(wetted, oriented=True):
            # The artificial planar SWL cap is for CAD closure only.
            bounds = gmsh.model.getBoundingBox(2, abs(surface))
            is_lid = abs(bounds[2]) < 1e-6 and abs(bounds[5]) < 1e-6
            kinds, _, connections = gmsh.model.mesh.getElements(2, abs(surface))
            for kind, connection in zip(kinds, connections):
                if kind != 2:  # Gmsh linear triangle element code
                    raise ValueError("Expected linear triangle elements")
                for tri in np.array(connection).reshape(-1, 3):
                    points = np.array([nodes[tag] for tag in tri])
                    # Gmsh's volume-boundary mesh already has outward winding;
                    # signed OCC surface tags must not reverse it a second time.
                    points[np.abs(points[:, 2]) < 1e-7, 2] = 0
                    (lid if is_lid else panels).append(points[[0, 1, 2, 2]])
        # Refine hull and cap together, so all shared waterline nodes match.
        all_panels = np.concatenate([panels, lid])
        refined = enforce_span(all_panels, max_span)
        is_lid = np.all(abs(refined[:, :, 2]) < 1e-8, axis=1)
        write_gdf(output, refined[~is_lid], gravity)
        if lid_output is not None:
            write_gdf(lid_output, refined[is_lid], gravity)
        # Check serialized coordinates, including their decimal rounding.
        check = _checker()
        for path in (output, lid_output):
            if path is not None:
                exported = check.read_gdf(path)
                span = max(np.max(np.linalg.norm(exported[:, a] - exported[:, b], axis=1))
                           for a, b in ((0, 1), (1, 2), (2, 0)))
                if span > max_span:
                    raise ValueError("Exported panel exceeds maximum span")
    finally:
        gmsh.finalize()


def generate_pair(directory, *, gravity=9.80665, depth=1500, omega=3.5):
    """Generate resumable coarse/fine hulls and matching reference lids."""
    directory = Path(directory)
    directory.mkdir(parents=True, exist_ok=True)
    check = _checker()
    limit = check.panel_span_limit(omega, depth, gravity)
    sources = {name: hashlib.sha256(Path(__file__).with_name(name).read_bytes()).hexdigest()
               for name in ("generate_mesh.py", "check_mesh.py")}
    for level, size in (("coarse", 0.55), ("fine", 0.40)):
        manifest_file = directory / f"{level}-manifest-v2.json"
        inputs = {"gravity_m_s2": gravity, "depth_m": depth,
                  "omega_max_rad_s": omega, "panels_per_wavelength": 6,
                  "rule_status": "ASSUMED study criterion", "max_span_m": limit,
                  "mesh_size_m": size, "source_sha256": sources,
                  "gmsh_version": "4.15.1"}
        if manifest_file.exists():
            old = json.loads(manifest_file.read_text())
            if old["inputs"] != inputs:
                raise ValueError("Existing generation inputs differ; retain and use a new output directory")
            for artifact in old["artifacts"]:
                if hashlib.sha256((directory / artifact["file"]).read_bytes()).hexdigest() != artifact["sha256"]:
                    raise ValueError("Retained mesh digest mismatch")
            continue
        hull_path, lid_path = (directory / f"{level}-{kind}-v2.gdf" for kind in ("hull", "lid"))
        if hull_path.exists() or lid_path.exists():
            raise ValueError("Partial case retained; inspect before choosing a new output directory")
        generate_mesh(hull_path, gravity=gravity, mesh_size=size,
                      max_span=limit, lid_output=lid_path)
        hull, lid = check.read_gdf(hull_path), check.read_gdf(lid_path)
        lid_result = check.validate_lid(hull, lid)
        hydrostatics = check.measure(hull)
        if any(row[3] == "FAIL" for row in check.checks(hydrostatics)):
            raise ValueError("Generated hull fails historical mesh criteria")
        artifacts = []
        for path, panels, kind in ((hull_path, hull, "hull"), (lid_path, lid, "lid")):
            result = check.df3(panels, [omega], depth=depth, gravity=gravity)
            if not result["pass"]:
                raise ValueError("DF3 rejected generated mesh")
            artifacts.append({"file": path.name, "kind": kind, "panels": len(panels),
                              "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
                              "bytes": path.stat().st_size, "df3": result})
        manifest = {"schema_version": 2, "level": level, "inputs": inputs,
                    "source_id": "nrel-iea-reference-turbines", "owner": "digitalmodel",
                    "status": "OFFLINE_CHECKED_NATIVE_ACCEPTANCE_OPEN",
                    "artifacts": artifacts, "lid_check": lid_result,
                    "hydrostatics": {k: float(v) for k, v in hydrostatics.items()}}
        manifest_file.write_text(json.dumps(manifest, indent=2, allow_nan=False) + "\n")


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
    parser.add_argument("--pair", action="store_true", help="output is the coarse/fine packet directory")
    parser.add_argument(
        "--gravity", required=True, type=float, help="m/s2; GDF header input"
    )
    parser.add_argument(
        "--mesh-size", default=0.55, type=float, help="numerical target edge length (m)"
    )
    args = parser.parse_args()
    if args.pair:
        generate_pair(args.output, gravity=args.gravity)
    else:
        generate_mesh(args.output, gravity=args.gravity, mesh_size=args.mesh_size)


if __name__ == "__main__":
    main()
