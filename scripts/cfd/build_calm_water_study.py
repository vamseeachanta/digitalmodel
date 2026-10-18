# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Retain native cost measurements and render an indicative resistance record."""

from __future__ import annotations

import argparse
import hashlib
import html
import json
import math
import re
from datetime import datetime
from pathlib import Path


def verify_criterion(record: dict, criteria: bytes) -> None:
    """Reject changed criteria or criteria recorded after computation began."""
    if record["criterion_sha256"] != hashlib.sha256(criteria).hexdigest():
        raise ValueError("criterion digest mismatch")
    recorded = datetime.fromisoformat(record["criterion_recorded_at"])
    started = datetime.fromisoformat(record["timed_start_started_at"])
    if recorded >= started:
        raise ValueError("criterion was not recorded before native computation")


def executed_velocity(field: Path) -> float:
    """Read the rendered speed and verify the case's negative flow direction."""
    text = field.read_text()
    mean = re.search(r"^Umean\s+([0-9eE.+-]+);", text, re.MULTILINE)
    negative = re.search(r"^mUmean\s+([0-9eE.+-]+);", text, re.MULTILINE)
    if not mean or not negative:
        raise ValueError("rendered velocity declarations are missing")
    speed = float(mean[1])
    if not math.isfinite(speed) or speed <= 0 or float(negative[1]) != -speed:
        raise ValueError("rendered flow direction or speed is invalid")
    return speed


def normalise_force_header(text: str) -> str:
    """Apply whitespace hooks to headers while preserving numerical rows."""
    return "".join(
        line.rstrip(" \t\r\n") + "\n" if line.lstrip().startswith("#") else line
        for line in text.splitlines(keepends=True)
    )


def coefficient_history(
    text: str, density: float, area: float, speed: float, *, half_domain: bool = False
) -> str:
    """Reduce signed native forces to positive resistance coefficients."""
    if not all(math.isfinite(x) and x > 0 for x in (density, area, speed)):
        raise ValueError("positive finite normalisation inputs are required")
    denominator = 0.5 * density * area * speed**2
    force_multiplier = 2.0 if half_domain else 1.0
    rows = ["iteration,ct,cp,cv"]
    previous = -math.inf
    for line in text.splitlines():
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        values = [float(x) for x in line.split()]
        if len(values) != 10 or not all(math.isfinite(x) for x in values):
            raise ValueError("native force row must contain ten finite values")
        if values[0] <= previous:
            raise ValueError("native iterations must increase strictly")
        previous = values[0]
        coefficients = [-values[index] * force_multiplier / denominator for index in (1, 4, 7)]
        rows.append(
            f"{values[0]:g}," + ",".join(f"{value:.12g}" for value in coefficients)
        )
    if len(rows) == 1:
        raise ValueError("native force history is empty")
    return "\n".join(rows) + "\n"


def verify_retained(manifest: dict, repo: Path) -> None:
    """Verify committed evidence without importing or launching a solver."""
    _verify_status(manifest)
    criteria = (repo / "config/cfd/calm_water_study.json").read_bytes()
    if manifest["criteria"] != json.loads(criteria):
        raise ValueError("retained criterion snapshot mismatch")
    expected = set()
    for run in manifest["runs"]:
        verify_criterion(run, criteria)
        if run["mesh"] not in ("coarse", "medium", "fine"):
            raise ValueError("unknown mesh")
        base = f"data/cfd/calm_water_study/raw/{run['mesh']}"
        force_path, coefficients_path = f"{base}-force.dat", f"{base}-coefficients.csv"
        if force_path in expected:
            raise ValueError("duplicate mesh")
        expected.update((force_path, coefficients_path))
        norm = run["normalisation"]
        coefficients = coefficient_history(
            (repo / force_path).read_text(), norm["density_kg_per_m3"],
            norm["area_m2"], norm["speed_m_per_s"], half_domain=norm["half_domain"],
        )
        if coefficients != (repo / coefficients_path).read_text():
            raise ValueError("coefficient reduction mismatch")
        count = len(coefficients.splitlines()) - 1
        if run["return_code"] != 0 or count != run["completed_iterations"]:
            raise ValueError("incomplete force history")
        if count != manifest["criteria"]["timed_start_iterations"]:
            raise ValueError("not a timed start")
        per_iteration = run["measured_wall_seconds"] / count
        projected = per_iteration * manifest["criteria"]["full_iterations"]
        if not (math.isclose(per_iteration, run["seconds_per_iteration"])
                and math.isclose(projected, run["projected_full_seconds"])):
            raise ValueError("cost arithmetic mismatch")
    if not expected or expected != set(manifest["files"]):
        raise ValueError("retained file census mismatch")
    for relative, evidence in manifest["files"].items():
        data = (repo / relative).read_bytes()
        if (len(data) != evidence["size_bytes"]
                or hashlib.sha256(data).hexdigest() != evidence["sha256"]):
            raise ValueError("retained file digest mismatch")


def _verify_status(manifest: dict) -> None:
    """Short starts cannot certify resistance or validation."""
    if manifest["qualified_results"] != 0 or manifest["readiness_status"] != "partial":
        raise ValueError("short starts cannot be qualified resistance results")


def _cost_rows(manifest: dict) -> str:
    """Format recorded measurements and arithmetic projections."""
    rows = []
    for run in manifest["runs"]:
        rows.append(
            "<tr>"
            f"<td>{html.escape(run['mesh'])}</td><td>{run['cells']}</td>"
            f"<td>{run['completed_iterations']}</td><td>{run['ranks']}</td>"
            f"<td>{run['measured_wall_seconds']:.1f}</td>"
            f"<td>{run['peak_aggregate_rss_bytes'] / 1e9:.1f}</td>"
            f"<td>{run['seconds_per_iteration']:.2f}</td>"
            f"<td>{run['projected_full_seconds'] / 3600:.1f}</td></tr>"
        )
    return "\n".join(rows)


def render_report(manifest: dict) -> str:
    """Render indicative cost evidence with explicit gaps and evidence links."""
    _verify_status(manifest)
    c = manifest["criteria"]
    comparison, grid = c["comparison"], c["grid_uncertainty"]
    limits = c["resource_limits"]
    return f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<title>Calm-water resistance study (indicative)</title>
<style>body{{font:16px/1.5 system-ui,sans-serif;max-width:1100px;margin:2rem auto;padding:1rem;color:#182f40}}table{{border-collapse:collapse;width:100%}}td,th{{border:1px solid #bbc;padding:.4rem;text-align:left}}caption{{caption-side:bottom;text-align:left}}h2{{margin-top:2rem}}@media print{{body{{font-size:11pt}}}}</style>
</head><body>
<h1>Calm-water resistance study (indicative)</h1>
<p>Readiness: partial. Qualified resistance results: {manifest['qualified_results']}</p>
<h2>Summary</h2>
<p>The retained short starts provide indicative computation-cost evidence only. Resistance prediction cannot yet support an engineering decision. Settled runs, numerical uncertainty and experimental agreement are not established.</p>
<h2>Revision history</h2>
<p>The retained starts were recorded on {html.escape(manifest['runs'][0]['timed_start_started_at'][:10])}. This report is regenerated from the committed manifest and its recorded criterion snapshot.</p>
<h2>Study basis and criteria</h2>
<p>A generic benchmark hull is fixed at even keel without appendages in calm water at a requested Froude number of {c['froude_number']}. Geometry and fluid particulars are inherited case inputs, not independently verified experimental inputs. Each run records the geometry digest and coefficient normalisation inputs; the rendered flow speed, rather than the unrounded request, is used.</p>
<p>The planned mesh multipliers are {', '.join(str(x) for x in c['mesh_scales'])}. Only the coarse and medium starts are retained; a fine-mesh start is not retained. A constant division-count ratio does not demonstrate uniform realised mesh refinement or asymptotic grid convergence.</p>
<p>The proposed total-resistance comparison tolerance is {comparison['relative_tolerance_percent']:.1f} percent and the medium-mesh grid uncertainty target is {grid['medium_mesh_target_percent']:.1f} percent. These are assumed study criteria recorded before the starts, not a standard requirement or a demonstrated validation threshold. An opened experimental table and page are required; no experimental comparison value is retained.</p>
<h2>Method and resource measurements</h2>
<p>The OpenFOAM force histories contain {c['timed_start_iterations']} iterations per start at {c['ranks']} ranks. The intended full length is {c['full_iterations']} iterations, with the final {c['averaging_iterations']} iterations proposed for averaging. These lengths do not establish convergence.</p>
<p>Peak memory is the recorded sampled aggregate resident memory of the launcher and descendants at the time of measurement. Sampling implementation and native solver logs are not retained here; transient peaks, kernel memory and independent log verification are not established. Solver version is not established by the retained files.</p>
<p>Projected duration is the measured short-start wall time divided by completed iterations and multiplied by intended full iterations. Mesh construction is excluded. Projections are indicative; steady-run performance is not established.</p>
<table><tr><th>Mesh</th><th>Cells</th><th>Iterations</th><th>Ranks</th><th>Wall time (s)</th><th>Sampled peak memory (GB)</th><th>Time per iteration (s)</th><th>Projected full run (h)</th></tr>
{_cost_rows(manifest)}
<caption>Recorded short-start measurements and indicative arithmetic projections; decimal GB is used.</caption></table>
<h2>Findings and limitations</h2>
<p>The coarse streamwise force history becomes unstable and changes sign. The medium history still drifts; neither history establishes settled resistance. A successful process return code does not establish a physical solution. Full-duration results and a fine-mesh start are absent from this evidence set.</p>
<p>Resistance comparison: not available. Grid uncertainty: not established; {grid['required_completed_meshes']} completed settled meshes with a justified refinement relationship are required. No accepted resistance coefficient, numerical uncertainty or experimental validation is reported.</p>
<h2>Next evidence required</h2>
<p>The unstable coarse history requires diagnosis before extending it. Settled force histories, complete mesh evidence and an opened tabulated experimental reference are needed before the proposed criteria can be evaluated. Resource policy calls for {limits['memory_reserve_gb']} GB memory reserve, {limits['minimum_disk_free_gb']} GB free disk and no more than {limits['total_projected_solver_hours']} projected solver hours in total. The incomplete mesh set cannot establish a total resource projection.</p>
<h2>Evidence and sources</h2>
<ul><li><a href="../../../data/cfd/calm_water_study/manifest.json">Run records and SHA-256 digest manifest</a>: measured resource values, timestamps, geometry identity, coefficient normalisation and frozen study criteria.</li>
<li><a href="../../../config/cfd/calm_water_study.json">Original study criteria</a>: the exact bytes are bound by each run's criterion digest. Resource limits and tolerance values are declared policy, not measurements.</li>
<li><a href="../../../data/cfd/calm_water_study/raw/coarse-force.dat">Coarse native force history</a> and <a href="../../../data/cfd/calm_water_study/raw/medium-force.dat">medium native force history</a>.</li>
<li><a href="../../../data/cfd/calm_water_study/raw/coarse-coefficients.csv">Coarse derived coefficients</a> and <a href="../../../data/cfd/calm_water_study/raw/medium-coefficients.csv">medium derived coefficients</a>.</li></ul>
<p>Force comment-header trailing whitespace is normalised; numerical force rows are unchanged according to the import record. Original source digests are retained, but original unnormalised files, solver logs, meshes and fields are not committed. Independent reproduction of the solves or measurement procedure is not established.</p>
<p>Full-hull coefficients double half-domain streamwise forces and reverse the streamwise sign, using the density, area and executed speed declared in each run. The coefficient histories are initialization evidence, not accepted resistance estimates. No public-source experimental numbers are used.</p>
</body></html>
"""


def build_record(cases: Path, repo: Path, output: Path) -> dict:
    """Retain completed short starts and verify numerical history integrity."""
    criteria = (repo / "config/cfd/calm_water_study.json").read_bytes()
    from digitalmodel.solvers.openfoam.validation.ship_resistance import ShipResistanceConfig

    basis = ShipResistanceConfig()
    manifest = {"schema_version": 1, "readiness_status": "partial", "intended_use": "Native start cost only; settled resistance is not established.", "qualified_results": 0, "criteria": json.loads(criteria), "runs": [], "files": {}}
    raw = output / "data/cfd/calm_water_study/raw"
    raw.mkdir(parents=True, exist_ok=True)
    for name in ("coarse", "medium", "fine"):
        record_path = cases / name / "timed-start.json"
        if not record_path.is_file():
            continue
        record = json.loads(record_path.read_text())
        verify_criterion(record, criteria)
        if record["return_code"] != 0 or record["completed_iterations"] != 100:
            raise ValueError(f"{name}: incomplete timed start")
        record["requested_velocity_m_per_s"] = record["velocity_m_per_s"]
        record["velocity_m_per_s"] = executed_velocity(cases / name / "0.orig/U")
        force = (cases / name / "postProcessing/forces/0/force.dat").read_bytes()
        source_force_digest = hashlib.sha256(force).hexdigest()
        text = normalise_force_header(force.decode("utf-8"))
        force = text.encode("utf-8")
        coefficients = coefficient_history(text, basis.density, basis.wetted_surface, record["velocity_m_per_s"], half_domain=True)
        if len(coefficients.splitlines()) - 1 != record["completed_iterations"]:
            raise ValueError(f"{name}: force history count disagrees with native log")
        record["normalisation"] = {"half_domain": True, "full_hull_force_multiplier": 2.0, "density_kg_per_m3": basis.density, "area_m2": basis.wetted_surface, "speed_m_per_s": record["velocity_m_per_s"], "basis": "assumed, based on previous experience; existing case configuration"}
        for suffix, data in (("force.dat", force), ("coefficients.csv", coefficients.encode())):
            path = raw / f"{name}-{suffix}"
            path.write_bytes(data)
            manifest["files"][path.relative_to(output).as_posix()] = {"sha256": hashlib.sha256(data).hexdigest(), "size_bytes": len(data), "header_redaction": "none", "source_force_sha256": source_force_digest, "normalisation": "Trailing whitespace is removed from comment headers; numerical rows are unchanged."}
        manifest["runs"].append(record)
    target = output / "data/cfd/calm_water_study/manifest.json"
    target.write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n")
    report = output / "docs/domains/openfoam/calm-water-resistance-study.html"
    report.parent.mkdir(parents=True, exist_ok=True)
    report.write_text(render_report(manifest))
    return manifest


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cases", type=Path)
    parser.add_argument("--report-only", action="store_true")
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--repo", type=Path, default=Path(__file__).resolve().parents[2])
    arguments = parser.parse_args()
    if arguments.report_only:
        source = arguments.repo / "data/cfd/calm_water_study/manifest.json"
        manifest = json.loads(source.read_text())
        verify_retained(manifest, arguments.repo)
        report = arguments.output / "docs/domains/openfoam/calm-water-resistance-study.html"
        report.parent.mkdir(parents=True, exist_ok=True)
        report.write_text(render_report(manifest))
    elif arguments.cases is None:
        parser.error("--cases is required unless --report-only is supplied")
    else:
        build_record(arguments.cases, arguments.repo, arguments.output)
