# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Bind retained public hull bytes to two staged decks and a conforming lid."""

import argparse
from collections import Counter
import hashlib
import importlib.util
import json
from pathlib import Path

import numpy as np

HERE = Path(__file__).resolve().parent
SOURCES = ("prepare_staged_v3.py", "mesh_band_v3.py", "build_datafile_v3.py",
           "input_checks_v3.py", "input-specification-v3.json", "expected-v2.json",
           "build_datafile_v2.py", "check_mesh.py", "generate_mesh.py", "native_estimate_v3.py")


def module(name):
    """Load only adjacent, offline preparation code."""
    spec = importlib.util.spec_from_file_location(name, HERE / f"{name}.py")
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


def digest(path):
    """Identify retained bytes, without interpreting their contents."""
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def source_hashes():
    """Bind all preparation code and the immutable physical basis."""
    return {name: digest(HERE / name) for name in SOURCES}


def mesh_record(path, *, declared_upper=None):
    """Measure spans from actual GDF bytes; do not substitute benchmark counts."""
    check, band = module("check_mesh"), module("mesh_band_v3")
    panels = check.read_gdf(path)
    gravity = float(Path(path).read_text().splitlines()[1].split()[1])
    if gravity != 9.80665:
        raise ValueError("Mesh gravity differs from the declared input basis")
    span = float(max(band.maximum_span(panels)))
    upper = band.valid_band(span, depth=1500)
    return {"file": Path(path).name, "sha256": digest(path), "panels": len(panels),
            "bytes": Path(path).stat().st_size, "max_span_m": span, "valid_band_rad_s": upper,
            "declared_df3": band.df3_declared(panels, upper if declared_upper is None else declared_upper, depth=1500),
            "historical_3_50_df3": check.df3(panels, [3.5], depth=1500)}


def _waterline_loops(hull):
    directed = Counter()
    for panel in hull:
        for a, b in zip(panel, np.roll(panel, -1, axis=0)):
            if abs(a[2]) < 1e-8 and abs(b[2]) < 1e-8 and not np.array_equal(a, b):
                directed[(tuple(b), tuple(a))] += 1
    successors = {}
    for (a, b), count in directed.items():
        if count != 1 or a in successors:
            raise ValueError("Non-simple or duplicated hull waterline")
        successors[a] = b
    loops = []
    while successors:
        start = next(iter(successors))
        loop, point = [], start
        while point in successors:
            loop.append(point)
            point = successors.pop(point)
        if point != start or len(loop) < 3:
            raise ValueError("Open hull waterline")
        polygon = np.asarray(loop)
        if np.sum(np.cross(polygon, np.roll(polygon, -1, axis=0))[:, 2]) <= 0:
            raise ValueError("Nested or reversed waterline requires explicit geometry review")
        loops.append(loop)
    if not loops:
        raise ValueError("No closed waterline")
    return loops


def generate_lid(hull_path, output, *, max_span):
    """Preserve every hull boundary segment; mesh only the interior surface."""
    check = module("check_mesh")
    hull = check.read_gdf(hull_path)
    loops = _waterline_loops(hull)
    import gmsh

    gmsh.initialize()
    try:
        gmsh.option.setNumber("General.Terminal", 0)
        gmsh.option.setNumber("General.NumThreads", 4)
        gmsh.model.add("public_waterplane")
        geo = gmsh.model.geo
        for loop in loops:
            points = [geo.addPoint(*point, max_span / 3) for point in loop]
            lines = [geo.addLine(a, b) for a, b in zip(points, points[1:] + points[:1])]
            geo.addPlaneSurface([geo.addCurveLoop(lines)])
        geo.synchronize()
        for _, line in gmsh.model.getEntities(1):
            gmsh.model.mesh.setTransfiniteCurve(line, 2)
        gmsh.option.setNumber("Mesh.MeshSizeMax", max_span / 3)
        gmsh.model.mesh.generate(2)
        tags, xyz, _ = gmsh.model.mesh.getNodes()
        nodes = dict(zip(tags, np.asarray(xyz).reshape(-1, 3)))
        panels = []
        for kind, _, elements in zip(*gmsh.model.mesh.getElements(2)):
            if kind != 2:
                raise ValueError("Expected triangular interior panels")
            for tri in np.asarray(elements).reshape(-1, 3):
                panels.append(np.array([nodes[tag] for tag in tri])[[0, 1, 2, 2]])
        module("generate_mesh").write_gdf(output, panels, 9.80665)
    finally:
        gmsh.finalize()
    lid = check.read_gdf(output)
    result = check.validate_lid(hull, lid)
    if max(module("mesh_band_v3").maximum_span(lid)) > max_span:
        raise ValueError("Reference lid exceeds the hull panel scale")
    return result


def prepare_packet(directory, l1_source, *, l2_source=None):
    """Create once; reuse only after checking every source and output digest."""
    directory, l1_source = Path(directory), Path(l1_source)
    if directory.is_symlink():
        raise ValueError("Output directory must not be a symlink")
    manifest_path = directory / "manifest-v3.json"
    if manifest_path.exists():
        manifest = json.loads(manifest_path.read_text())
        for level, source in (("L1", l1_source), ("L2", l2_source)):
            if (source is None) != (level not in manifest["meshes"]):
                raise ValueError("Retained input set differs")
            if source is not None and digest(source) != manifest["meshes"][level]["sha256"]:
                raise ValueError("Retained source digest mismatch")
        module("input_checks_v3").df0(directory)
        return manifest
    if directory.exists() and any(directory.iterdir()):
        raise ValueError("Partial output retained; use a new empty directory")
    directory.mkdir(parents=True, exist_ok=True)
    meshes = {}
    for level, source in (("L1", l1_source), ("L2", l2_source)):
        if source is not None:
            output = directory / f"{level.lower()}-hull-v3.gdf"
            output.write_bytes(Path(source).read_bytes())
            meshes[level] = mesh_record(output)
    specification = module("build_datafile_v3").resolved_specification(meshes["L1"]["valid_band_rad_s"])
    lid = None
    if specification["stages"]["F"]["interior_lid"]:
        generate_lid(directory / "l1-hull-v3.gdf", directory / "l1-lid-v3.gdf",
                     max_span=meshes["L1"]["max_span_m"])
        lid = mesh_record(directory / "l1-lid-v3.gdf", declared_upper=meshes["L1"]["valid_band_rad_s"])
    module("build_datafile_v3").build_datafiles(directory, specification)
    if l2_source is not None:
        import yaml

        (directory / "l2-probes-model-v3.yml").write_text(yaml.safe_dump(
            module("build_datafile_v3").probe_model(specification), sort_keys=False))
    (directory / "resolved-specification-v3.json").write_text(json.dumps(specification, indent=2) + "\n")
    manifest = {"schema_version": 3, "source_id": "nrel-iea-reference-turbines", "owner": "digitalmodel",
                "status": "OFFLINE_INPUT_INTEGRITY_ONLY_NATIVE_ACCEPTANCE_OPEN",
                "source_sha256": source_hashes(), "meshes": meshes, "lid": lid,
                "estimates": packet_estimates(meshes, lid, specification),
                "files": {p.name: digest(p) for p in directory.iterdir()}, "native_acceptance": "OPEN"}
    manifest_path.write_text(json.dumps(manifest, indent=2) + "\n")
    module("input_checks_v3").df0(directory)
    return manifest


def packet_estimates(meshes, lid, specification):
    """Compare both L1 lid scenarios and the four-frequency L2 probe."""
    estimate = module("native_estimate_v3").estimate
    results = {}
    for stage in ("S", "F"):
        count = specification["stages"][stage]["frequency_count"]
        results[f"{stage}_without_lid"] = estimate(meshes["L1"]["panels"], count)
        results[f"{stage}_with_lid"] = (None if lid is None else estimate(
            meshes["L1"]["panels"], count, lid_panels=lid["panels"]))
    if "L2" in meshes:
        results["L2_probes"] = estimate(meshes["L2"]["panels"], 4)
    return results


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path)
    parser.add_argument("l1_mesh", type=Path)
    parser.add_argument("--l2-mesh", type=Path)
    args = parser.parse_args()
    print(json.dumps(prepare_packet(args.directory, args.l1_mesh, l2_source=args.l2_mesh), indent=2))


if __name__ == "__main__":
    main()
