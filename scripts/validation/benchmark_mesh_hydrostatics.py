#!/usr/bin/env python3
"""Fresh-process analytic Wigley qualification for sr2314 criteria v1.

Invoke once per implementation with PYTHONPATH=src uv run python this_script.
Optional --legacy-file loads a locally retained baseline; its path is not output.
Peak RSS is resource.ru_maxrss (KiB on Linux), covering the complete process.
"""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import platform
import sys
import time

try:
    import resource
except ImportError:
    resource = None

import numpy as np
import scipy
from digitalmodel.naval_architecture import mesh_hydrostatics as current
from digitalmodel.naval_architecture.hull_fixtures import wigley_mesh


CRITERIA = {"revision": "sr2314-v1", "minimum_faces": 1_000_000,
            "volume_relative_error_max": 1e-4, "area_relative_error_max": 5e-4,
            "elapsed_seconds_max_exclusive": 120, "peak_rss_bytes_max_exclusive": 2 * 1024**3}
COMPARISON_CRITERIA = {"revision": "sr2314-comparison-v1", "wall_time_ratio_max": 1.10,
                       "rss_ratio_max": 1.05}


def is_qualified(record):
    return (record["source_faces"] >= CRITERIA["minimum_faces"]
        and record["volume_relative_error"] <= CRITERIA["volume_relative_error_max"]
        and record["area_relative_error"] <= CRITERIA["area_relative_error_max"]
        and record["elapsed_seconds_validate_hydrostatics"] < CRITERIA["elapsed_seconds_max_exclusive"]
        and record["peak_rss_bytes"] is not None
        and record["peak_rss_bytes"] < CRITERIA["peak_rss_bytes_max_exclusive"])


def compare_records(baseline, revised):
    fields = ("numpy", "scipy", "python", "platform", "nx", "nz", "draft_m", "grid_stations_m",
              "source_faces", "station_count", "benchmark_script_sha256", "result_sha256")
    mismatches = [key for key in fields if baseline.get(key) != revised.get(key)]
    if baseline["source_sha256"]["hull_fixtures.py"] != revised["source_sha256"]["hull_fixtures.py"]:
        mismatches.append("hull_fixtures.py")
    if baseline["implementation"] != "legacy" or revised["implementation"] != "current":
        mismatches.append("implementation_roles")
    if baseline["implementation_sha256"] == revised["implementation_sha256"]:
        mismatches.append("identical_implementation")
    for role, record in (("baseline", baseline), ("current", revised)):
        if canonical_digest(record["result"]) != record["result_sha256"]:
            mismatches.append(f"{role}_result_integrity")
    if baseline["criteria"] != revised["criteria"] or baseline["criteria"] != CRITERIA:
        mismatches.append("criteria")
    elapsed = revised["elapsed_seconds_validate_hydrostatics"] / baseline["elapsed_seconds_validate_hydrostatics"]
    memory = (revised["peak_rss_bytes"] / baseline["peak_rss_bytes"]
              if baseline["peak_rss_bytes"] and revised["peak_rss_bytes"] is not None else None)
    return {"criteria": COMPARISON_CRITERIA, "mismatches": mismatches,
            "compared_record_sha256": {"baseline": canonical_digest(baseline), "current": canonical_digest(revised)},
            "record_digest_scope": "canonical-json",
            "wall_time_ratio": elapsed, "rss_ratio": memory,
            "qualified": not mismatches and is_qualified(baseline) and is_qualified(revised)
                and elapsed <= COMPARISON_CRITERIA["wall_time_ratio_max"] and memory is not None
                and memory <= COMPARISON_CRITERIA["rss_ratio_max"]}


def canonical_digest(value):
    encoded = json.dumps(value, sort_keys=True, separators=(",", ":"), allow_nan=False).encode()
    return hashlib.sha256(encoded).hexdigest()


def partial_references(draft):
    """Integrate the design-T=6 m Wigley surface below the requested draft."""
    from scipy import integrate
    midship_area = 10 * (draft**2 / 6 - draft**3 / (3 * 6**2))

    def integrand(z, x):
        xi, zeta = 2 * x / 100, (z - 6) / 6
        y_x, y_z = -0.2 * xi * (1 - zeta**2), -10 / 6 * (1 - xi**2) * zeta
        return np.sqrt(1 + y_x**2 + y_z**2)

    quarter, _ = integrate.dblquad(integrand, 0, 50, 0, draft, epsabs=0, epsrel=1e-12)
    return 2 * 100 / 3 * midship_area, 4 * quarter, midship_area


def peak_rss_raw():
    return resource.getrusage(resource.RUSAGE_SELF).ru_maxrss if resource else None


def load_implementation(legacy_file=None):
    if legacy_file is None:
        return current
    spec = importlib.util.spec_from_file_location("sr2314_legacy_mesh", legacy_file)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def area_summary(vertices, faces):
    a = vertices[faces[:, 0]]
    areas = 0.5 * np.linalg.norm(np.cross(vertices[faces[:, 1]] - a,
                                         vertices[faces[:, 2]] - a), axis=1)
    return {"count": len(areas), "min": float(areas.min()), "mean": float(areas.mean())}


def source_digests(implementation, legacy_file):
    directory = Path(current.__file__).parent
    paths = [directory / "hull_fixtures.py"]
    if legacy_file:
        paths.append(Path(implementation.__file__))
    else:
        paths.extend(sorted(directory.glob("mesh_*.py")))
    return {path.name: hashlib.sha256(path.read_bytes()).hexdigest() for path in paths}


def evaluate_sections(implementation, mesh, stations, legacy_file, draft):
    target = implementation if legacy_file else importlib.import_module(
        "digitalmodel.naval_architecture.mesh_sections")
    original, evaluated = target._section, []

    def counted(*args, **kwargs):
        evaluated.append(float(args[1]))
        return original(*args, **kwargs)

    target._section = counted
    try:
        result = implementation.compute_hydrostatics(mesh, draft=draft,
            grid_stations=stations, grid_waterlines=[1.0, 3.0, draft])
    finally:
        target._section = original
    return result, len(evaluated), len(set(evaluated))


def run_case(nx=1000, nz=250, legacy_file=None, draft=5.973):
    implementation = load_implementation(legacy_file)
    reference_volume, reference_area, reference_midship = partial_references(draft)
    vertices, faces = wigley_mesh(100.0, 10.0, 6.0, nx=nx, nz=nz)
    source_summary = area_summary(vertices, faces)
    stations = np.linspace(-44.95, 45.05, 10)
    rss_before = peak_rss_raw()
    started = time.perf_counter()
    mesh = implementation.TriMesh(vertices, faces, units="m", axes=("forward", "port", "up"))
    result, evaluations, unique = evaluate_sections(implementation, mesh, stations, legacy_file, draft)
    elapsed = time.perf_counter() - started
    rss_after = peak_rss_raw()
    clipped = implementation.clip_at_waterline(mesh, draft)
    generated_summary = area_summary(clipped.vertices, clipped.faces[~clipped.artificial])
    volume_error = abs(result["V"].value / reference_volume - 1)
    area_error = abs(result["S"].value / reference_area - 1)
    rss_raw = peak_rss_raw()
    rss_bytes = rss_raw * 1024 if rss_raw is not None and sys.platform.startswith("linux") else None
    record = {"criteria": dict(CRITERIA), "implementation": "legacy" if legacy_file else "current",
        "implementation_sha256": hashlib.sha256(Path(implementation.__file__).read_bytes()).hexdigest(),
        "benchmark_script_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        "source_sha256": source_digests(implementation, legacy_file),
        "python": platform.python_version(), "platform": sys.platform, "numpy": np.__version__, "scipy": scipy.__version__,
        "draft_m": draft, "grid_stations_m": stations.tolist(),
        "nx": nx, "nz": nz, "source_faces": len(faces), "station_count": len(stations),
        "section_evaluation_count": evaluations, "unique_section_stations": unique,
        "volume_m3": result["V"].value, "reference_volume_m3": reference_volume,
        "midship_area_m2": result["A_M"].value, "reference_midship_area_m2": reference_midship,
        "C_M": result["C_M"].value, "C_P": result["C_P"].value,
        "result": result.to_dict(), "result_sha256": canonical_digest(result.to_dict()),
        "wetted_area_m2": result["S"].value, "reference_area_m2": reference_area,
        "volume_relative_error": volume_error, "area_relative_error": area_error,
        "source_area_m2": source_summary, "generated_physical_area_m2": generated_summary,
        "elapsed_seconds_validate_hydrostatics": elapsed, "peak_rss_raw": rss_raw,
        "peak_rss_raw_before_timed_region": rss_before, "peak_rss_raw_after_timed_region": rss_after,
        "peak_rss_raw_unit": "KiB" if sys.platform.startswith("linux") else "platform-dependent",
        "peak_rss_bytes": rss_bytes}
    record["qualified"] = is_qualified(record)
    return record


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--legacy-file", type=Path)
    parser.add_argument("--compare", nargs=2, type=Path, metavar=("BASELINE_JSON", "CURRENT_JSON"))
    parser.add_argument("--nx", type=int, default=1000)
    parser.add_argument("--nz", type=int, default=250)
    args = parser.parse_args()
    report = (compare_records(*(json.loads(path.read_text()) for path in args.compare))
              if args.compare else run_case(args.nx, args.nz, args.legacy_file))
    if args.compare:
        report["compared_file_sha256"] = {role: hashlib.sha256(path.read_bytes()).hexdigest()
            for role, path in zip(("baseline", "current"), args.compare)}
    print(json.dumps(report, indent=2, allow_nan=False))


if __name__ == "__main__":
    main()
