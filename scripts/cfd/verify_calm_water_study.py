# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Verify committed indicative evidence and regenerate its report without solves."""

from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
import math
import statistics
from pathlib import Path

BASE = Path("data/cfd/calm_water_study")
MESHES = ("coarse", "medium", "fine")


def load_builder(repo: Path):
    """Load report helpers without importing the solver package."""
    spec = importlib.util.spec_from_file_location(
        "retained_study_builder", repo / "scripts/cfd/build_calm_water_study.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def require_close(actual: float, expected: float) -> None:
    """Reject changed calculations, allowing only coefficient text precision."""
    if not math.isclose(actual, expected, rel_tol=1e-10, abs_tol=1e-14):
        raise ValueError("retained arithmetic mismatch")


def verify_files(repo: Path, manifest: dict) -> None:
    """Constrain evidence paths before reading them and verify bytes and sizes."""
    expected = {str(BASE / "run-log.jsonl")}
    for run in manifest["runs"]:
        if run["mesh"] not in MESHES:
            raise ValueError("unknown mesh")
        for suffix in ("force.dat", "coefficients.csv", "face-resolution.txt"):
            expected.add(str(BASE / "raw" / f"{run['mesh']}-{suffix}"))
    for run in manifest["full_runs"]:
        if run["mesh"] not in MESHES:
            raise ValueError("unknown full-run mesh")
        for suffix in ("force.dat", "coefficients.csv"):
            expected.add(str(BASE / "raw" / f"{run['mesh']}-full-window-{suffix}"))
    if set(manifest["files"]) != expected:
        raise ValueError("retained file census mismatch")
    for relative, item in manifest["files"].items():
        path = repo / relative
        if not path.resolve().is_relative_to(repo.resolve()):
            raise ValueError("evidence path escapes repository")
        data = path.read_bytes()
        size = item.get("size_bytes", item.get("bytes"))
        if len(data) != size or hashlib.sha256(data).hexdigest() != item["sha256"]:
            raise ValueError("retained digest or size mismatch")


def verify_history(repo: Path, run: dict, builder, *, full: bool = False) -> list:
    """Recompute coefficients from the retained signed native forces."""
    stem = run["mesh"] + ("-full-window" if full else "")
    force = (repo / BASE / "raw" / f"{stem}-force.dat").read_text()
    csv = (repo / BASE / "raw" / f"{stem}-coefficients.csv").read_text()
    norm = run["normalisation"]
    if norm["half_domain"] is not True or norm["full_hull_force_multiplier"] != 2:
        raise ValueError("unqualified force normalisation")
    reduced = builder.coefficient_history(
        force, norm["density_kg_per_m3"], norm["area_m2"],
        norm["speed_m_per_s"], half_domain=True,
    )
    if reduced != csv:
        raise ValueError("coefficient reduction mismatch")
    return [[float(value) for value in line.split(",")] for line in csv.splitlines()[1:]]


def verify_full_window(repo: Path, run: dict, builder, criteria: dict) -> None:
    """Check descriptive statistics without certifying temporal convergence."""
    rows = verify_history(repo, run, builder, full=True)
    last, count = criteria["full_iterations"], criteria["averaging_iterations"]
    if run["status"] != "complete" or run["return_code"] != 0:
        raise ValueError("unexpected full-run outcome")
    if [row[0] for row in rows] != list(range(last - count + 1, last + 1)):
        raise ValueError("incomplete averaging window")
    stats = run["statistics"]
    for key, value in (("samples", count), ("first_iteration", last-count+1),
                       ("last_iteration", last)):
        require_close(stats[key], value)
    for index, name in enumerate(("ct", "cp", "cv"), 1):
        values = [row[index] for row in rows]
        require_close(stats[f"mean_{name}"], statistics.mean(values))
        require_close(stats[f"standard_deviation_{name}"], statistics.pstdev(values))
    norm = run["normalisation"]
    denominator = 0.5 * norm["density_kg_per_m3"] * norm["area_m2"] * norm["speed_m_per_s"]**2
    require_close(stats["mean_resistance_n"], stats["mean_ct"] * denominator)
    if run["last_iteration"] != last or run["requested_end_iteration"] != last:
        raise ValueError("full-run endpoint mismatch")


def verify_retained(repo: Path, manifest: dict | None = None) -> dict:
    """Read back all committed evidence; external archived bytes are not checked."""
    if manifest is None:
        manifest = json.loads((repo / BASE / "manifest.json").read_text())
    if manifest["qualified_results"] != 0 or manifest["readiness_status"] != "partial":
        raise ValueError("evidence does not establish qualified resistance")
    criteria_bytes = (repo / "config/cfd/calm_water_study.json").read_bytes()
    criteria = json.loads(criteria_bytes)
    if manifest["criteria"] != criteria:
        raise ValueError("criterion snapshot mismatch")
    verify_files(repo, manifest)
    builder = load_builder(repo)
    names = [r["mesh"] for r in manifest["runs"]]
    if sorted(names) != sorted(MESHES):
        raise ValueError("timed-start mesh census mismatch")
    full_names = [r["mesh"] for r in manifest["full_runs"]]
    if len(set(full_names)) != len(full_names):
        raise ValueError("duplicate full-run mesh")
    for run in manifest["runs"]:
        builder.verify_criterion(run, criteria_bytes)
        rows = verify_history(repo, run, builder)
        count = criteria["timed_start_iterations"]
        if run["return_code"] != 0 or run["completed_iterations"] != count:
            raise ValueError("incomplete timed start")
        if [row[0] for row in rows] != list(range(1, count+1)):
            raise ValueError("timed-start iteration mismatch")
        elapsed = run["measured_wall_seconds"] / count
        require_close(run["seconds_per_iteration"], elapsed)
        require_close(run["projected_full_seconds"], elapsed * criteria["full_iterations"])
        face = repo / BASE / "raw" / f"{run['mesh']}-face-resolution.txt"
        if builder.read_face_check(face) != run["face_resolution"]:
            raise ValueError("face-resolution record mismatch")
    for run in manifest["full_runs"]:
        builder.verify_criterion(run, criteria_bytes)
        verify_full_window(repo, run, builder, criteria)
    if builder.render_run_log(manifest) != (repo / BASE / "run-log.jsonl").read_text():
        raise ValueError("run-log readback mismatch")
    return manifest


def render_retained(repo: Path, manifest: dict) -> str:
    """Regenerate the complete page using only the committed averaging windows."""
    builder = load_builder(repo)
    series = {
        run["mesh"]: (repo / BASE / "raw" / f"{run['mesh']}-full-window-coefficients.csv").read_text()
        for run in manifest["full_runs"]
    }
    figure = builder.render_history_plot(series, manifest["criteria"]["averaging_iterations"])
    return builder.render_report(manifest, figure)


def main() -> int:
    """Check by default; write the generated page only when explicitly requested."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--write-report", action="store_true")
    args = parser.parse_args()
    manifest = verify_retained(args.repo)
    page = render_retained(args.repo, manifest)
    path = args.repo / "docs/domains/openfoam/calm-water-resistance-study.html"
    if args.write_report:
        path.write_text(page)
    elif path.read_text() != page:
        raise ValueError("committed indicative report differs from retained evidence")
    print(f"indicative study evidence OK: {len(manifest['files'])} files; "
          f"{len(manifest['runs'])} starts; {len(manifest['full_runs'])} continuations; "
          "0 qualified results; external archives not checked")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
