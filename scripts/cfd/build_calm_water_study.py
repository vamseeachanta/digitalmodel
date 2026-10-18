# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Retain native cost measurements and render an indicative resistance record.

This docs/domains report is outside the docs/api freshness census; its dedicated
committed-evidence checker verifies the report and retained numerical histories.

Restore large case files with scripts/data/fetch.py using an exact path or a
quoted glob such as 'data/cfd/calm_water_study/cases/*/constant/triSurface/*'.
The repository manifest indexes only files larger than 1 MiB. For each smaller
entry in the study manifest's store.cases block, copy
${ACE_DATA_ROOT}/data/digitalmodel/<store_id> to its relative path and check its
size and SHA-256 before use. These entries also retain native logs and inputs.
Pass the restored cases folder with --cases; --output selects the rebuilt tree.
Full histories stay in the store; only final averaging windows are written.
"""

from __future__ import annotations

import argparse
import hashlib
import html
import json
import math
import re
import statistics
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from digitalmodel.solvers.openfoam.validation.ship_resistance import ShipResistanceConfig
from datetime import datetime
from pathlib import Path


def verify_criterion(record: dict, criteria: bytes) -> None:
    """Reject changed criteria or criteria recorded after computation began."""
    if record["criterion_sha256"] != hashlib.sha256(criteria).hexdigest():
        raise ValueError("criterion digest mismatch")
    recorded = datetime.fromisoformat(record["criterion_recorded_at"])
    start_key = "started_at" if "started_at" in record else "timed_start_started_at"
    started = datetime.fromisoformat(record[start_key])
    if recorded >= started:
        raise ValueError("criterion was not recorded before native computation")


def verify_full_run(native_log: str, requested_end: int, return_code: int) -> int:
    """Require native completion and the requested final iteration."""
    iterations = [float(value) for value in re.findall(r"^Time = ([0-9eE.+-]+)$", native_log, re.MULTILINE)]
    if return_code != 0 or not iterations or iterations[-1] != requested_end or "\nEnd\n" not in native_log:
        raise ValueError("incomplete native full run")
    return requested_end


def verify_force_iterations(coefficients: str, requested_end: int) -> None:
    """Reject gaps, duplicate iterations and truncated full histories."""
    iterations = [float(line.split(",", 1)[0]) for line in coefficients.splitlines()[1:]]
    if iterations != list(range(1, requested_end + 1)):
        raise ValueError("missing or repeated force iterations")


def full_run_statistics(coefficients: str, requested_end: int, window: int) -> dict:
    """Compute final-window descriptive statistics without certifying convergence."""
    verify_force_iterations(coefficients, requested_end)
    if not 1 <= window <= requested_end:
        raise ValueError("invalid averaging window")
    rows = [[float(value) for value in line.split(",")] for line in coefficients.splitlines()[1:]]
    tail = [row for row in rows if row[0] > requested_end - window]
    result = {"samples": len(tail), "first_iteration": tail[0][0], "last_iteration": tail[-1][0]}
    for index, name in enumerate(("ct", "cp", "cv"), 1):
        values = [row[index] for row in tail]
        result[f"mean_{name}"] = statistics.mean(values)
        result[f"standard_deviation_{name}"] = statistics.pstdev(values)
    return result


def join_force_histories(folder: Path) -> tuple[str, dict]:
    """Join restart segments in iteration order without discarding overlaps."""
    segments = sorted(folder.glob("*/force.dat"), key=lambda path: float(path.parent.name))
    rows = ["# Native signed half-domain forces in N; restart segments joined in order.\n", "# iteration total_x_N total_y_N total_z_N pressure_x_N pressure_y_N pressure_z_N viscous_x_N viscous_y_N viscous_z_N\n"]
    digests = {}
    for path in segments:
        data = path.read_bytes()
        digests[path.parent.name] = hashlib.sha256(data).hexdigest()
        rows.extend(line + "\n" for line in data.decode().splitlines() if line.strip() and not line.lstrip().startswith("#"))
    return "".join(rows), digests


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


def averaging_window(text: str, first: int, last: int, *, csv: bool = False) -> str:
    """Retain the declared iteration window with the original fixed headers."""
    lines = text.splitlines(keepends=True)
    headers = lines[:1] if csv else [line for line in lines if line.startswith("#")]
    rows = lines[1:] if csv else [line for line in lines if line.strip() and not line.startswith("#")]
    selected = [line for line in rows if first <= float(line.split("," if csv else None)[0]) <= last]
    if len(selected) != last - first + 1:
        raise ValueError("incomplete averaging window")
    return "".join(headers + selected)


def collect_full_run(case: Path, record: dict, normalisation: dict, criteria: dict, output: Path, files: dict) -> dict:
    """Retain completed or failed native continuations with explicit status."""
    if record["mesh"] not in ("coarse", "medium", "fine"):
        raise ValueError("unknown or unsafe mesh designation")
    native = (case / "log.interFoam.full").read_bytes()
    if hashlib.sha256(native).hexdigest() != record["native_log_sha256"]:
        raise ValueError("native full-run log digest mismatch")
    if record["status"] == "complete":
        verify_full_run(native.decode(), criteria["full_iterations"], record["return_code"])
    elif record["status"] != "failed" or record["return_code"] == 0:
        raise ValueError("invalid full-run status")
    text, sources = join_force_histories(case / "postProcessing/forces")
    coefficients = coefficient_history(text, normalisation["density_kg_per_m3"], normalisation["area_m2"], normalisation["speed_m_per_s"], half_domain=True)
    result = dict(record)
    result["normalisation"] = normalisation
    result["source_force_digests"] = sources
    if record["status"] == "complete":
        result["statistics"] = full_run_statistics(coefficients, criteria["full_iterations"], criteria["averaging_iterations"])
        cutoff = criteria["full_iterations"] - criteria["averaging_iterations"]
        native_rows = [line.split() for line in text.splitlines() if line.strip() and not line.startswith("#")]
        result["statistics"]["mean_resistance_n"] = statistics.mean(-2 * float(row[1]) for row in native_rows if float(row[0]) > cutoff)
    raw = output / "data/cfd/calm_water_study/raw"
    raw.mkdir(parents=True, exist_ok=True)
    last = criteria["full_iterations"]
    first = last - criteria["averaging_iterations"] + 1
    for suffix, history in (("force.dat", text), ("coefficients.csv", coefficients)):
        data = averaging_window(history, first, last, csv=suffix.endswith(".csv")).encode()
        path = raw / f"{record['mesh']}-full-window-{suffix}"
        path.write_bytes(data)
        files[path.relative_to(output).as_posix()] = {
            "sha256": hashlib.sha256(data).hexdigest(), "bytes": len(data),
            "first_iteration": first, "last_iteration": last,
            "row_count": last - first + 1,
            "normalisation": "Restart segments are joined in order; comment headers are replaced by a fixed header; numerical rows are unchanged.",
            "header_redaction": "fixed header",
        }
    return result


def plan_envelope(points: list[tuple[float, float]]) -> list[list[float]]:
    """Return a deterministic convex envelope of the projected input surface."""
    points = sorted(set(points))
    if len(points) < 3:
        raise ValueError("geometry projection has fewer than three points")
    def cross(a: tuple[float, float], b: tuple[float, float], c: tuple[float, float]) -> float:
        return (b[0]-a[0])*(c[1]-a[1])-(b[1]-a[1])*(c[0]-a[0])
    halves: list[tuple[float, float]] = []
    for sequence in (points, list(reversed(points))):
        half: list[tuple[float, float]] = []
        for point in sequence:
            while len(half) >= 2 and cross(half[-2], half[-1], point) <= 0:
                half.pop()
            half.append(point)
        halves.extend(half[:-1])
    return [list(point) for point in halves]


def read_fluid_inputs(case: Path) -> dict:
    """Preserve both fluid phases and a true zero surface-tension input."""
    text = (case / "constant/transportProperties").read_text()
    result = {}
    for phase, prefix in (("water", ""), ("air", "air_")):
        block = re.search(r"\b" + phase + r"\s*\{([^}]+)\}", text, re.S)
        if not block:
            raise ValueError("fluid phase inputs are missing")
        for token, key in (("nu", "kinematic_viscosity_m2_per_s"), ("rho", "density_kg_per_m3")):
            match = re.search(r"\b" + token + r"\s+([0-9eE.+-]+);", block[1])
            value = float(match[1]) if match else math.nan
            if not math.isfinite(value) or value <= 0:
                raise ValueError("invalid fluid phase input")
            result[prefix + key] = value
    sigma = re.search(r"\bsigma\s+([0-9eE.+-]+);", text)
    value = float(sigma[1]) if sigma else math.nan
    if not math.isfinite(value) or value < 0:
        raise ValueError("surface-tension input is missing or invalid")
    result["surface_tension_n_per_m"] = value
    return result


def read_face_check(path: Path) -> dict:
    """Read the retained geometric gate without promoting it to flow validation."""
    data = path.read_bytes()
    number = r"([0-9eE.+-]+)"
    pattern = r"FACE RESOLUTION VERDICT: PASS .*?(\d+) faces, max " + number + r" m2 = " + number + r"x .*?\(" + number + r" m\)\^2, limit " + number + r"x; mean face " + number + r" m2"
    match = re.search(pattern, data.decode())
    if not match:
        raise ValueError("a passing hull-face record is not established")
    faces = int(match[1])
    maximum, ratio, target, limit, mean = (float(match[index]) for index in range(2, 7))
    if faces <= 0 or not all(math.isfinite(v) and v > 0 for v in (maximum, ratio, target, limit, mean)):
        raise ValueError("invalid hull-face metrics")
    if ratio > limit or maximum / target**2 > limit or mean > maximum:
        raise ValueError("hull-face metrics exceed their geometric limit")
    return {"faces": faces, "maximum_face_area_m2": maximum, "target_cell_length_m": target, "area_ratio_1": ratio, "limit_1": limit, "mean_face_area_m2": mean, "source_sha256": hashlib.sha256(data).hexdigest()}


def read_model_basis(case: Path, config: "ShipResistanceConfig", criteria: dict) -> dict:
    """Read physical inputs and geometry without substituting requested speed."""
    vertices = []
    for line in (case / "constant/triSurface/kcs.stl").read_text().splitlines():
        if line.strip().startswith("vertex "):
            values = tuple(float(value) for value in line.split()[1:])
            if len(values) != 3 or not all(math.isfinite(value) for value in values):
                raise ValueError("invalid input geometry vertex")
            vertices.append(values)
    if not vertices:
        raise ValueError("input geometry vertices are missing")
    mesh = (case / "log.checkMesh").read_text()
    bounds = re.search(r"Overall domain bounding box\s*:?\s*\(([^)]+)\)\s*\(([^)]+)\)", mesh)
    if not bounds:
        raise ValueError("executed domain bounds are missing")
    lower, upper = ([float(value) for value in bounds[group].split()] for group in (1, 2))
    if len(lower) != 3 or len(upper) != 3 or not all(math.isfinite(value) for value in lower + upper) or any(lo >= hi for lo, hi in zip(lower, upper)):
        raise ValueError("invalid executed domain bounds")
    if lower[1] >= 0 or upper[1] != 0:
        raise ValueError("executed domain does not match the negative-y half-domain basis")
    gravity = re.search(r"\bvalue\s*\(([^)]+)\)", (case / "constant/g").read_text())
    if gravity is None:
        raise ValueError("gravity vector is missing")
    g = [float(value) for value in gravity[1].split()]
    if len(g) != 3 or g[0] != 0 or g[1] != 0 or not math.isfinite(g[2]) or g[2] >= 0:
        raise ValueError("gravity does not establish an upward vertical axis")
    fluid = read_fluid_inputs(case)
    nu = fluid["kinematic_viscosity_m2_per_s"]
    density = fluid["density_kg_per_m3"]
    if density != config.density:
        raise ValueError("executed fluid density disagrees with the normalization basis")
    speed = executed_velocity(case / "0.orig/U")
    selected = [(x, y) for x, y, z in vertices if lower[1] <= y <= upper[1]]
    return {
        "lpp_m": config.lpp, "beam_m": config.beam, "draft_m": config.draft,
        "area_m2": config.wetted_surface, "density_kg_per_m3": density,
        "speed_m_per_s": speed, "kinematic_viscosity_m2_per_s": nu,
        "case_gravity_m_per_s2": -g[2], "nominal_froude": criteria["froude_number"],
        "executed_froude": speed / math.sqrt(-g[2] * config.lpp),
        "executed_reynolds": speed * config.lpp / nu,
        "geometry_bounds_m": {axis: [min(v[index] for v in vertices), max(v[index] for v in vertices)] for index, axis in enumerate("xyz")},
        "domain_bounds_m": {axis: [lower[index], upper[index]] for index, axis in enumerate("xyz")},
        "plan_envelope_xy_m": plan_envelope(selected),
        "input_basis": "assumed, based on previous experience; retained model geometry and case configuration",
        **fluid,
    }


def render_history_plot(series: dict[str, str], window: int) -> str:
    """Plot final-window bin means and extrema with a fixed SVG identity."""
    if not series:
        return ""
    import io
    import os
    import tempfile

    previous = os.environ.get("MPLCONFIGDIR")
    os.environ["MPLCONFIGDIR"] = str(Path(os.environ.get("TMPDIR", tempfile.gettempdir())) / "matplotlib")
    try:
        import matplotlib
        from matplotlib.figure import Figure
    finally:
        if previous is None:
            os.environ.pop("MPLCONFIGDIR", None)
        else:
            os.environ["MPLCONFIGDIR"] = previous
    with matplotlib.rc_context({"svg.hashsalt": "calm-water-history", "font.family": "DejaVu Sans"}):
        figure = Figure(figsize=(8, 3.5), layout="constrained")
        axis = figure.subplots()
        for name, data in sorted(series.items()):
            rows = [[float(value) for value in line.split(",")[:2]] for line in data.splitlines()[1:]]
            tail = [row for row in rows if row[0] > rows[-1][0] - window]
            step = max(1, math.ceil(len(tail) / 200))
            bins = [tail[index:index+step] for index in range(0, len(tail), step)]
            x = [statistics.mean(row[0] for row in block) for block in bins]
            y = [1000 * statistics.mean(row[1] for row in block) for block in bins]
            low = [1000 * min(row[1] for row in block) for block in bins]
            high = [1000 * max(row[1] for row in block) for block in bins]
            line, = axis.plot(x, y, label=name)
            axis.fill_between(x, low, high, alpha=0.18, color=line.get_color())
        axis.set_xlabel("Iteration (1)")
        axis.set_ylabel("Total resistance coefficient (×10⁻³)")
        axis.grid(alpha=0.25)
        axis.legend()
        buffer = io.StringIO()
        figure.savefig(buffer, format="svg", metadata={"Date": None, "Creator": None})
        figure.clear()
    svg = buffer.getvalue()
    svg = svg[svg.index("<svg"):]
    svg = re.sub(r"<metadata>.*?</metadata>\s*", "", svg, flags=re.S)
    return "\n".join(line.rstrip() for line in svg.splitlines()).strip() + "\n"


def mesh_difference_text(runs: list[dict]) -> str:
    """State a two-mesh difference without presenting it as grid uncertainty."""
    values = {run["mesh"]: run["statistics"]["mean_ct"] for run in runs if "statistics" in run}
    if not {"coarse", "medium"} <= values.keys():
        return "A cross-mesh resistance difference is not available until two completed histories exist; grid uncertainty is not established."
    reference = values["coarse"]
    if abs(reference) < 0.5 * 10**-4:
        return f"The absolute coefficient difference is {values['medium']-reference:.4g}; the coarse reference is below half the printed coefficient increment. This is not grid uncertainty."
    difference = 100 * (values["medium"] - reference) / reference
    return f"The medium final-window mean total coefficient differs by {difference:+.1f}% from the coarse final-window mean, which is the reference. This difference is not a grid-uncertainty estimate; three completed meshes are required."


def report_table(headers: list[str], rows: list[list[str]], caption: str, identity: str) -> str:
    """Place the readable caption below a table and expose its citation target."""
    head = "".join(f"<th>{value}</th>" for value in headers)
    body = "".join("<tr>" + "".join(f"<td>{value}</td>" for value in row) + "</tr>" for row in rows)
    return f'<div class="table-wrap"><table aria-describedby="{identity}"><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table><p class="caption" id="{identity}">{caption}</p></div>'


def report_number(value: float | None, *, percent: bool = False) -> str:
    """Apply four-significant-figure precision without inventing missing data."""
    return "not established" if value is None else format(value, ".1f" if percent else ".4g")


def model_schematic(basis: dict) -> str:
    """Show the half-domain surface envelope, with explicit projection limits."""
    points = basis.get("plan_envelope_xy_m")
    if not points:
        return "<p>The geometry envelope is not established.</p>"
    xmin = min(point[0] for point in points)
    xmax = max(point[0] for point in points)
    ymin = min(point[1] for point in points)
    ymax = max(point[1] for point in points)
    if xmax <= xmin or ymax <= ymin:
        raise ValueError("geometry envelope is degenerate")
    outline = " ".join(f"{80+640*(x-xmin)/(xmax-xmin):.3f},{240-150*(y-ymin)/(ymax-ymin):.3f}" for x, y in points)
    return f'''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 800 320" role="img" aria-label="Plan projection envelope of the analysed half hull">
<defs><marker id="hull-arrow" markerWidth="8" markerHeight="8" refX="7" refY="4" orient="auto"><path d="M0 0 L8 4 L0 8 Z" fill="#245b77"/></marker></defs>
<path d="M650 40 L150 40" stroke="#245b77" stroke-width="2" marker-end="url(#hull-arrow)"/><text x="290" y="28">Flow in negative x</text>
<polygon points="{outline}" fill="#e0edf2" stroke="#245b77" stroke-width="2"/>
<path d="M70 90 L730 90" stroke="#556" stroke-dasharray="5 4"/><text x="290" y="78">Symmetry plane y = 0</text>
<text x="65" y="275">x = {xmin:.4g} m</text><text x="625" y="275">x = {xmax:.4g} m</text>
<text x="180" y="305">Convex plan envelope; transverse scale is enlarged for readability.</text></svg>'''


def report_design_data(manifest: dict) -> str:
    """Describe physical inputs, assumptions, criteria and case definitions."""
    b = manifest.get("model_basis", {})
    fields = [("Length between perpendiculars", "lpp_m", "m"), ("Nominal full beam", "beam_m", "m"), ("Nominal draft", "draft_m", "m"), ("Reference wetted area", "area_m2", "m²"), ("Executed inlet speed", "speed_m_per_s", "m/s"), ("Water density", "density_kg_per_m3", "kg/m³"), ("Water kinematic viscosity", "kinematic_viscosity_m2_per_s", "m²/s"), ("Air density", "air_density_kg_per_m3", "kg/m³"), ("Air kinematic viscosity", "air_kinematic_viscosity_m2_per_s", "m²/s"), ("Surface tension", "surface_tension_n_per_m", "N/m"), ("Case gravity", "case_gravity_m_per_s2", "m/s²"), ("Nominal Froude number", "nominal_froude", "1"), ("Executed Reynolds number", "executed_reynolds", "1")]
    rows = [[label, report_number(b.get(key)), unit, '<a href="#ref-i1">[I1]</a>'] for label, key, unit in fields]
    inputs = report_table(["Quantity", "Value", "Unit", "Basis"], rows, "Table 3.1. Defined model and normalization inputs; source qualification is recorded in the assumption ledger.", "table-3-1")
    ledger = report_table(["Assumption", "Basis and confirmation", "Affected result and sensitivity"], [
        ["Geometry and model particulars", "ASSUMED - to be confirmed: assumed, based on previous experience; retained geometry and configuration. Confirm against an opened primary geometry source.", "Resistance and physical model identity; source-error sensitivity is not bounded."],
        ["Fluid properties and normalization", "ASSUMED - to be confirmed: assumed, based on previous experience; existing case configuration. Confirm the intended experimental condition and reference area.", "Absolute coefficients and any future experimental error; property sensitivity was not calculated."],
        ["Comparison and uncertainty targets", "Assumed, based on previous experience; predeclared targets in <a href='#ref-i2'>[I2]</a>, without a claimed standard clause.", "Acceptance thresholds; no empirical acceptance is inferred without a matched reference."],
        ["Gravity convention", "Executed gravity and Froude number are retained in the manifest model_basis; primary input qualification remains open.", "The reported executed speed is used for normalization; this difference is retained, not silently matched."],
        ["Inherited domain placement", "The executed extents are retained; a domain-size sensitivity investigation is required.", "Domain-truncation influence is not bounded."],
    ], "Table 3.2. Assumption ledger and affected conclusions.", "table-3-2")
    c = manifest.get("criteria", {})
    criteria = report_table(["Quantity", "Criterion", "Rationale and source"], [
        ["Experimental total-coefficient difference", report_number(c.get("comparison", {}).get("relative_tolerance_percent"), percent=True) + " %", "Comparison target; assumed, based on previous experience. An opened printed number and matched condition are required. <a href='#ref-i2'>[I2]</a>"],
        ["Medium-grid numerical uncertainty", report_number(c.get("grid_uncertainty", {}).get("medium_mesh_target_percent"), percent=True) + " %", "Target GCI; three completed meshes with nominal linear refinement ratio r = 2 are required. <a href='#ref-i2'>[I2]</a>"],
        ["Force-history scope", "25000 iterations; final 4000 averaged", "Declared execution and averaging scope; this is not a temporal-stationarity tolerance. <a href='#ref-i2'>[I2]</a>"],
    ], "Table 3.3. Predeclared comparison criteria and their applicability.", "table-3-3")
    cases = []
    for r in manifest["runs"]:
        if "cells" not in r:
            continue
        target = r.get("face_resolution", {}).get("target_cell_length_m")
        length = report_number(1000 * target) if target else "not established"
        relative = f"{100 * target / b['lpp_m']:.1f}" if target and b.get("lpp_m") else "not established"
        cases.append([html.escape(r["mesh"]), str(r["cells"]), str(r["mesh_scale"]), length, relative])
    case_table = report_table(["Mesh designation", "Cells", "Density multiplier (1)", "Target hull cell length (mm)", "Target length/Lpp (%)"], cases, "Table 3.4. Alternative discretizations of one fixed even-keel, bare-hull calm-water case; the stated targets are not measurements of minimum cell-edge length. The geometric gate is reported in <a href='#table-a-4'>Table A.4</a>.", "table-3-4")
    return f'''<h2 id="design-data">3 Design Data</h2><p>The defined model inputs and their qualification are presented in <a href="#table-3-1">Table 3.1</a> and <a href="#table-3-2">Table 3.2</a>.</p>
<h3>3.1 Site or field data</h3><p>Not applicable: the configuration is a laboratory-scale benchmark without a site.</p>
<h3>3.2 Structure or equipment data</h3><p>Not applicable: no supporting structure or equipment is analysed.</p>
<h3>3.3 Environmental data</h3><p>Calm water without incident waves is modelled; surface tension is set to zero. Fluid inputs and case gravity are stated in <a href="#table-3-1">Table 3.1</a>.</p>
<h3>3.4 Vessel data</h3><p>The hull is fixed at even keel without appendages. Flow is in negative x, z is upward, and the computational half-domain occupies y ≤ 0 with symmetry at y = 0.</p>{inputs}{ledger}
<figure id="figure-3-1">{model_schematic(b)}<figcaption>Figure 3.1. Convex envelope of the executed surface plan projection in the computational half-domain; cell faces and vertical geometry are not shown.</figcaption></figure>
<h3>3.5 Criteria and source clauses</h3><p>The targets in <a href="#table-3-3">Table 3.3</a> are assumed limits, not values attributed to a standard clause.</p>{criteria}
<h3>3.6 Method</h3><p>The governing method is given in <a href="#methodology">Section 4</a>.</p>
<h3>3.7 Load cases</h3><p>The mesh labels in <a href="#table-3-4">Table 3.4</a> identify the discretizations used in the subsequent results.</p>{case_table}'''


def report_results(manifest: dict, history_figure: str) -> str:
    """Keep calculated means separate from validation and uncertainty claims."""
    full = manifest.get("full_runs", [])
    rows = []
    for run in full:
        if "statistics" in run:
            s = run["statistics"]
            rows.append([html.escape(run["mesh"]), report_number(s.get("mean_resistance_n")), *(report_number(1000*s['mean_'+name]) for name in ("ct", "cp", "cv")), f"{s['standard_deviation_ct']:.1e}", str(s["samples"])])
    table = report_table(["Mesh", "Mean resistance (N)", "Mean Ct (×10⁻³)", "Mean Cp (×10⁻³)", "Mean Cv (×10⁻³)", "Ct dispersion (1)", "Samples"], rows, "Table 5.1. Descriptive final-window coefficients; dispersion is population standard deviation, not numerical uncertainty.", "table-5-1")
    plot = history_figure or "<p>The final-window figure is not available until a completed force history is retained.</p>"
    return f'''<h2 id="results">5 Detailed Results</h2><p>The final-window force coefficients are summarized in <a href="#table-5-1">Table 5.1</a>. Native completion and descriptive averages do not establish experimental agreement.</p>{table}
<p>{mesh_difference_text(full)}</p><figure id="figure-5-1">{plot}<figcaption>Figure 5.1. Total coefficient against iteration in the final declared window; lines show bin means and shading preserves bin minima and maxima. No acceptance band is drawn because an opened experimental target is unavailable.</figcaption></figure>
<p>Validation is planned against publicly tabulated towing-tank resistance at nominal Froude number 0.26 for the benchmark geometry, with attitude, appendage, Reynolds-number and reference-area matching required by <a href="#table-3-3">Table 3.3</a>. The institution archive addresses <a href="#ref-e1">[E1]</a> could not be opened; experimental comparison is not available. Grid uncertainty is not established because three completed full meshes are required.</p><p>{referent_reading()}</p>{component_reading(manifest)}'''


def referent_reading() -> str:
    """Do not promote an unopened fixture into experimental evidence."""
    return (
        "The in-repository experimental fixture has no opened, matched table "
        "and page retained with this study. No numerical experimental comparison is emitted. "
        "The predeclared source criterion remains unchanged. "
        "<a href='#ref-i7'>[I7]</a> records this evidence limit."
    )


def component_reading(manifest: dict) -> str:
    """Keep unavailable experimental and component comparisons explicit."""
    return (
        "<p>Experimental component offsets are not available: the reference is "
        "unqualified under the recorded source criterion. Descriptive CFD means "
        "remain traceable to retained force windows in <a href='#ref-i8'>[I8]</a>.</p>"
    )


def report_references(manifest: dict | None = None) -> str:
    """Identify committed numeric evidence and state archive verification limits."""
    return '''<h2 id="references">9 References</h2>
<h3>9.1 Published references</h3><p id="ref-e1">[E1] No opened primary experimental table at a matched condition is retained. Experimental validation is not established. Earlier archive-access claims are not independently verified by the committed evidence checker.</p>
<p id="ref-e2">[E2] No numerical experimental or standards-derived comparison is used in this indicative report.</p>
<h3>9.2 Internal references</h3><ul>
<li id="ref-i1">[I1] Model/data basis: <a href="../../../data/cfd/calm_water_study/manifest.json">retained manifest</a>, model_basis and runs. Geometry-source qualification and input sensitivity are not established.</li>
<li id="ref-i2">[I2] <a href="../../../config/cfd/calm_water_study.json">Original study criteria</a>: policy values are assumed study choices, bound by each run's criterion digest.</li>
<li id="ref-i3">[I3] Solver/program identity and method: manifest case_evidence includes recorded version banners, dictionary digests and method readback. These are retained records; archived native logs are not independently opened by this checker. Generator revision and a complete launch sequence are not recorded.</li>
<li id="ref-i4">[I4] Execution evidence: <a href="../../../data/cfd/calm_water_study/run-log.jsonl">run log</a> and <a href="../../../data/cfd/calm_water_study/raw/">retained force and coefficient windows</a>. The manifest store block indexes archived inputs and native logs by relative identifiers and digests. Archived bytes are not checked here. Full histories, meshes and fields are not needed for this page's retained-window recomputation.</li>
<li id="ref-i5">[I5] Reduction and report: scripts/cfd/build_calm_water_study.py. Independent committed-evidence check and regeneration: scripts/cfd/verify_calm_water_study.py. Grid method: src/digitalmodel/solvers/openfoam/grid_convergence.py; no uncertainty result is calculated.</li>
<li id="ref-i6">[I6] Historical published-reference candidates are not adopted as numeric evidence.</li>
<li id="ref-i7">[I7] The experimental fixture does not satisfy the opened-source, table and page criterion. No fixture number is promoted into this report.</li>
<li id="ref-i8">[I8] Descriptive means and dispersions: manifest full_runs statistics, independently recomputed from each retained final-window force and coefficient pair. A two-mesh descriptive difference is not numerical uncertainty.</li></ul>'''


def report_appendix(manifest: dict) -> str:
    """Retain cost evidence and explanatory caveats outside numbered findings."""
    rows = []
    for r in manifest["runs"]:
        if "cells" in r:
            rows.append([html.escape(r["mesh"]), str(r["cells"]), str(r["ranks"]), str(r["completed_iterations"]), report_number(r["measured_wall_seconds"]), f"{r['peak_aggregate_rss_bytes']/1e9:.1f}", f"{r['seconds_per_iteration']:.1f}", f"{r['projected_full_seconds']/3600:.1f}"])
    starts = report_table(["Mesh", "Cells", "Ranks", "Start iterations", "Wall time (s)", "Aggregate RSS (GB)", "Time per iteration (s)", "Projected full run (h)"], rows, "Table A.1. Measured short-start cost; measured resident memory uses decimal GB. Projection excludes mesh construction.", "table-a-1")
    rows = [[html.escape(r["mesh"]), html.escape(r["status"]), str(r["last_iteration"]), report_number(r["measured_wall_seconds"])] for r in manifest.get("full_runs", [])]
    full = report_table(["Mesh", "Native outcome", "Last iteration", "Continuation wall time (s)"], rows, "Table A.2. Native continuation evidence; return code, native end marker and force-history completeness are checked separately.", "table-a-2")
    basis = manifest.get("model_basis", {})
    bounds = [[axis, report_number(pair[0]), report_number(pair[1])] for axis, pair in basis.get("domain_bounds_m", {}).items()]
    domain = report_table(["Axis", "Lower bound (m)", "Upper bound (m)"], bounds, "Table A.3. Executed outer domain bounds; a domain-size sensitivity study was not performed.", "table-a-3")
    face_rows = []
    for r in manifest["runs"]:
        if "face_resolution" in r:
            f = r["face_resolution"]
            face_rows.append([html.escape(r["mesh"]), str(f["faces"]), report_number(1e6 * f["maximum_face_area_m2"]), f"{f['area_ratio_1']:.1f}", f"{f['limit_1']:.1f}"])
    face_table = report_table(["Mesh", "Hull faces", "Maximum face area (mm²)", "Area/target area (1)", "Limit (1)"], face_rows, "Table A.4. Retained static-mesh hull-face inspection; the area ratio is maximum face area divided by the squared target length. Meeting this geometric limit does not establish resistance convergence.", "table-a-4")
    return f'''<h2 id="appendix-a">Appendix A – Native execution and model detail</h2><p>The short-start measurements in <a href="#table-a-1">Table A.1</a> define the computation-cost projection, not resistance convergence.</p>{starts}<h3>Native continuation</h3><p>The completed or failed continuations are distinguished by their native outcomes and final iterations.</p>{full}{domain}{face_table}
<p>Peak memory is sampled aggregate resident memory of the launcher and descendants at a cadence not recorded in the retained manifest. Shared pages may be counted repeatedly; unsampled transient peaks and kernel memory are excluded. Memory reserve and minimum free disk are declared study policy in the criterion record; their runtime implementation is not independently checked here. Full-run peak RSS was not sampled.</p>
<p>The projection is measured short-start wall time per iteration multiplied by 25000. The finest projection exceeds the 80-hour total allowance; a full finest run is therefore excluded. A larger resource or duration allowance is required before a three-mesh result can be obtained. The manifest identifies archived native case data; archive availability is not established by the committed-file checks.</p>
<h2 id="appendix-b">Appendix B – Interpretation and missing evidence</h2><p>The following details explain the limitations that affect engineering use.</p><ul><li>Primary geometry-source access and a matched printed experimental table remain unavailable. Recalculation of the retained inputs does not establish original input qualification.</li><li>Final-window averages and dispersion describe the computed histories. No independent temporal-stationarity tolerance was predeclared; neither dispersion nor the two-mesh difference supplies a numerical uncertainty bound.</li><li>The nominal linear refinement ratio is two. The retained starts record the common lateral reference division count; intended block division ratios remain constant. Surface clipping and local refinement mean that total cell-count ratios need not be exact cubes of two.</li><li>Domain placement is inherited and was not varied. Its influence on absolute resistance is not bounded by the available runs.</li><li>The history plot groups at most 200 bins in the final 4000-iteration window. Every value is used in the tabulated mean and dispersion; plotted shading preserves each bin's extrema. The half printed coefficient increment, 5 × 10⁻⁵, defines the near-zero display threshold for relative mesh differences, not an acceptance tolerance.</li></ul>'''


def render_report(manifest: dict, history_figure: str = "") -> str:
    """Render an indicative analysis without certifying missing evidence."""
    if manifest["qualified_results"] != 0 or manifest.get("readiness_status", "partial") != "partial":
        raise ValueError("retained histories do not establish qualified resistance")
    count = sum(r.get("status") == "complete" for r in manifest.get("full_runs", []))
    revision_rows = [["0", "2026-10-14", "Native start evidence and criteria are retained to distinguish computation cost from settled resistance."]]
    if manifest.get("full_runs"):
        revision_rows.append(["1", "2026-10-15", "Native continuation outcomes are added to distinguish completion from resistance qualification."])
    if manifest.get("store"):
        revision_rows.append(["2", "2026-10-15", "Case inputs and native logs become retrievable; averaging windows replace split histories, and the referent and component limitations are identified."])
    revisions = report_table(["Revision", "Date", "Change and reason"], revision_rows, "Table 0.1. Revision history.", "revision-table")
    sections = [("summary", "1 Executive Summary"), ("introduction", "2 Introduction"), ("design-data", "3 Design Data"), ("methodology", "4 Analysis Methodology"), ("results", "5 Detailed Results"), ("findings", "6 Key Findings"), ("limitations", "7 Limitations"), ("recommendations", "8 Recommendations"), ("references", "9 References"), ("symbols", "10 Acronyms and Symbols"), ("appendix-a", "Appendix A – Native execution and model detail"), ("appendix-b", "Appendix B – Interpretation and missing evidence")]
    contents = "".join(f'<li><a href="#{identity}">{title}</a></li>' for identity, title in sections)
    summary = f'''<h2 id="summary">1 Executive Summary</h2><p>The results support computational workflow development only. Engineering resistance acceptance remains on hold until a matched experimental reference and a three-mesh uncertainty bound are available.</p><p>Primary geometry-source access and the required experimental table are unavailable, and numerical uncertainty is not established; the affected absolute resistance values remain unqualified.</p><ul><li>Completed full native continuations: {count}; three completed meshes are required for the declared grid-uncertainty method.</li><li>Mean coefficients are descriptive values from the final 4000 iterations, not experimental validation.</li><li>The next action is to obtain a matched published table and the additional full mesh identified in <a href="#recommendations">Section 8</a>.</li></ul><p>Qualified resistance results: {manifest['qualified_results']}</p><h2>Revision history</h2>{revisions}<nav aria-label="Contents"><h2>Contents</h2><ol>{contents}</ol></nav>'''
    introduction = '''<h2 id="introduction">2 Introduction</h2><p>The analysis concerns one laboratory-scale bare-hull calm-water condition at nominal Froude number 0.26. The hull is fixed at even keel; sinkage, trim response, appendages, incident waves and full-scale extrapolation are excluded.</p>'''
    method = '''<h2 id="methodology">4 Analysis Methodology</h2><p>The free surface and hull forces are calculated with OpenFOAM v2312 using a two-phase volume-of-fluid model, the k–ω SST turbulence model and local pseudo-time stepping; these are read from the retained case, including the native solver banner and phase, turbulence and time-discretization dictionaries <a href="#ref-i3">[I3]</a>. The following boundary description is stated, not read by this report renderer: the inlet prescribes negative-x velocity, the outlet uses a phase-mean velocity condition, the atmosphere permits pressure-driven inflow or outflow, and the fixed hull has zero wall motion.</p><p>The executed inlet speed and water properties in <a href="#table-3-1">Table 3.1</a> define the normalization. Half-domain forces are converted to full-hull resistance by <a href="#equation-1">Equation (1)</a>.</p><p class="equation" id="equation-1">R<sub>T</sub> = −2 F<sub>x,half</sub> &nbsp; (1)</p><p class="equation" id="equation-2">C<sub>T</sub> = R<sub>T</sub> / (½ ρ S U²) = C<sub>P</sub> + C<sub>V</sub> &nbsp; (2)</p><p class="equation" id="equation-3">Fr = U / √(g L<sub>pp</sub>), &nbsp; Re = U L<sub>pp</sub> / ν &nbsp; (3)</p><p class="equation" id="equation-4">Mean C<sub>T</sub> = Σ C<sub>T,i</sub> / N; &nbsp; σ<sub>Ct</sub> = √[Σ(C<sub>T,i</sub> − Mean C<sub>T</sub>)² / N] &nbsp; (4)</p><p>The force window contains iterations 21001–25000 after completion at iteration 25000. Dispersion in <a href="#equation-4">Equation (4)</a> is descriptive; a separate temporal-stationarity limit is not established. Three full meshes at nominal refinement ratio r = 2 are required before the grid-convergence-index method <a href="#ref-i5">[I5]</a> can be applied. Validation is planned against the matched towing-tank data described in <a href="#results">Section 5</a>.</p>'''
    findings = f'''<h2 id="findings">6 Key Findings</h2><p>The governing conclusions separate completed numerical histories from engineering qualification.</p><h3>6.1 Resistance histories</h3><p>{count} full continuations reached the required final iteration where a complete record is shown in <a href="#table-a-2">Table A.2</a>. The final-window coefficients are stated in <a href="#table-5-1">Table 5.1</a>, with no experimental acceptance inferred.</p><h3>6.2 Numerical and experimental qualification</h3><p>Grid uncertainty is not established because the declared criterion requires three completed full meshes. Experimental comparison is not available because an opened, matched printed table is missing; the 3.0 percent comparison target cannot therefore be evaluated.</p><p>{referent_reading()}</p>'''
    limitations = '''<h2 id="limitations">7 Limitations</h2><p>The following evidence gaps govern use of the calculated coefficients.</p><ul><li>The missing third full mesh prevents a three-grid uncertainty estimate; the cost basis is given in <a href="#appendix-a">Appendix A</a>.</li><li>The missing matched experimental table prevents validation; source access and condition matching are explained in <a href="#appendix-b">Appendix B</a>.</li><li>Original geometry qualification, fluid-property sensitivity, domain-size influence and independent temporal stationarity are not established; their affected conclusions are described in <a href="#appendix-b">Appendix B</a>.</li></ul>'''
    recommendations = '''<h2 id="recommendations">8 Recommendations</h2><p>Engineering resistance use should remain on hold while the uncertainty and validation quantities required by <a href="#table-3-3">Table 3.3</a> remain unevaluated.</p><ol><li>Obtain a primary geometry source and a printed towing-tank coefficient at the matched physical condition before evaluating experimental error using <a href="#equation-2">Equation (2)</a>.</li><li>Provide additional duration or resources for the missing full mesh, using the measured projection in <a href="#table-a-1">Table A.1</a> as the scheduling basis; then evaluate the declared three-mesh uncertainty target.</li><li>Quantify domain-size and temporal sensitivities before qualifying absolute coefficients; the present final-window calculation <a href="#equation-4">Equation (4)</a> does not supply those bounds.</li></ol>'''
    symbols = report_table(["Symbol or acronym", "Meaning", "Unit"], [["Rₜ", "Full-hull total resistance", "N"], ["Fₓ,half", "Signed half-domain streamwise force", "N"], ["ρ", "Water density", "kg/m³"], ["S", "Reference wetted area", "m²"], ["U", "Inlet speed magnitude", "m/s"], ["Lpp", "Length between perpendiculars", "m"], ["g", "Gravity magnitude", "m/s²"], ["ν", "Water kinematic viscosity", "m²/s"], ["Ct, Cp, Cv", "Total, pressure and viscous resistance coefficients", "1"], ["Fr, Re", "Froude and Reynolds numbers", "1"], ["N", "Number of samples in the averaging window", "count"], ["r", "Nominal linear mesh-refinement ratio", "1"], ["σCt", "Population dispersion of Ct, not uncertainty", "1"], ["hₜ", "Declared hull-face target length; hₜ/Lpp is a defined geometric ratio", "m"], ["STL, SST", "Triangulated surface format; shear-stress transport", "not applicable"], ["CFD, GCI, RSS", "Computational fluid dynamics; grid convergence index; aggregate resident memory", "not applicable"]], "Table 10.1. Symbols and acronyms used in the analysis.", "table-10-1")
    style = 'body{font:16px/1.5 system-ui,sans-serif;max-width:1050px;margin:2rem auto;padding:1rem;color:#182f40}table{border-collapse:collapse;width:100%;table-layout:fixed}td,th{border:1px solid #bbc;padding:.4rem;text-align:left;overflow-wrap:anywhere}h2{margin-top:2rem}p,li{font-weight:400}.caption,figcaption{font-size:.9em;text-align:left;margin:.4rem 0 1rem}.table-wrap{max-width:100%}figure{margin:1rem 0}svg{width:100%;height:auto}code{overflow-wrap:anywhere}.equation{text-align:center}@media print{body{font-size:10pt;margin:0;max-width:none}h2,h3{break-after:avoid}figure,.table-wrap{break-inside:avoid}}'
    body = summary + introduction + report_design_data(manifest) + method + report_results(manifest, history_figure) + findings + limitations + recommendations + report_references(manifest) + '<h2 id="symbols">10 Acronyms and Symbols</h2><p>The symbols and units used in the governing equations are defined below.</p>' + symbols + report_appendix(manifest)
    return '<!doctype html>\n<html lang="en"><head><meta charset="utf-8"><title>Calm-water resistance study (indicative)</title><style>' + style + '</style></head><body><h1>Calm-water resistance study (indicative)</h1><p>Indicative computational record</p>' + body + '\n</body></html>\n'


def render_run_log(manifest: dict) -> str:
    """Render measured native costs and their iteration scopes as JSON lines."""
    rows: list[str] = []
    for key, kind in (("runs", "timed_start"), ("full_runs", "full_continuation")):
        rows.extend(json.dumps(dict(record, kind=kind), sort_keys=True) for record in manifest.get(key, []))
    return "\n".join(rows) + "\n"


def read_case_evidence(case: Path) -> dict:
    """Read solver identity, dictionary digests and regeneration evidence."""
    dictionaries = {path.name: hashlib.sha256(path.read_bytes()).hexdigest()
                    for path in sorted((case / "system").iterdir()) if path.is_file()}
    records = [case / name for name in
               ("study-input.json", "timed-start.json", "full-run.json", "run.log")]
    revisions = []
    for path in records:
        if path.is_file():
            revisions.extend(re.findall(
                r'"(?:code_revision|git_revision|git_commit|revision|commit)"\s*:\s*"([^"]+)"',
                path.read_text()))
    native = case / "log.interFoam.full"
    banner = []
    if native.is_file():
        banner = [line for line in native.read_text().splitlines()[:60]
                  if re.match(r"^\|.*Version:", line) or line.startswith("Build  :")]
    commands = []
    for log in sorted(case.glob("log.*")):
        if log.name in ("log.face-resolution",):
            continue
        match = re.search(r"^Exec\s*:\s*(.+)$", log.read_text(), re.M)
        if match:
            # Case destination is caller-selected, never an execution-host path.
            command = re.sub(r" -case \S+", "", match[1])
            commands.append(command)
    schemes = (case / "system/fvSchemes").read_text()
    turbulence = (case / "constant/turbulenceProperties").read_text()
    transport = (case / "constant/transportProperties").read_text()
    log_steps = re.findall(r"^BEGIN (.+)$", (case / "run.log").read_text(), re.M)
    meshing = []
    for step in log_steps:
        utility = step.split(".")[0]
        expected = utility + (" -dict system/topoSetDict." + step.split(".")[1]
                              if utility == "topoSet" else "")
        matched_command = next((command for command in commands if command.startswith(expected)), None)
        meshing.append(matched_command or "not recorded")
    return {
        "generator_revision": revisions[0] if revisions else "not recorded in the retained case",
        "solver_version_banner": banner,
        "dictionary_sha256": dictionaries,
        "method_readback": {
            "local_time_stepping": bool(re.search(r"default\s+localEuler;", schemes)),
            "turbulence_model": "kOmegaSST" if re.search(r"RASModel\s+kOmegaSST;", turbulence) else "not recorded",
            "two_phase_vof": "phases (water air)" if re.search(r"phases\s*\(water air\)", transport) else "not recorded",
        },
        "regeneration": {
            "working_directory": "restored case folder",
            "configuration": json.loads((case / "study-input.json").read_text()),
            "logged_steps": log_steps,
            "mesh_commands_in_logged_order": meshing,
            "native_commands": commands,
            "command_sequence": "not recorded: run.log records utility order; native log Exec lines record individual commands, not a complete launch script",
            "fields": "not recorded: initial-field setup and complete launch sequence are absent from run.log",
            "mesh_and_fields_retained": False,
        },
    }


def build_record(cases: Path, repo: Path, output: Path) -> dict:
    """Retain completed short starts and verify numerical history integrity."""
    criteria = (repo / "config/cfd/calm_water_study.json").read_bytes()
    from digitalmodel.solvers.openfoam.validation.ship_resistance import ShipResistanceConfig

    basis = ShipResistanceConfig()
    manifest: dict[str, Any] = {"schema_version": 1, "readiness_status": "partial", "intended_use": "Native execution cost and descriptive force histories; experimental validation and grid uncertainty are not established.", "qualified_results": 0, "runs": [], "full_runs": [], "files": {}}
    manifest["criteria"] = json.loads(criteria)
    retained = json.loads((repo / "data/cfd/calm_water_study/manifest.json").read_text())
    manifest["store"] = retained["store"]
    manifest["case_evidence"] = {}
    raw = output / "data/cfd/calm_water_study/raw"
    raw.mkdir(parents=True, exist_ok=True)
    for name in ("coarse", "medium", "fine"):
        record_path = cases / name / "timed-start.json"
        if not record_path.is_file():
            continue
        record = json.loads(record_path.read_text())
        if record["mesh"] != name:
            raise ValueError("timed-start mesh identity disagrees with its case")
        verify_criterion(record, criteria)
        if record["return_code"] != 0 or record["completed_iterations"] != 100:
            raise ValueError(f"{name}: incomplete timed start")
        manifest["case_evidence"][name] = read_case_evidence(cases / name)
        model = read_model_basis(cases / name, basis, json.loads(criteria))
        record["model_basis"] = model
        if "model_basis" not in manifest:
            manifest["model_basis"] = model
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
        face_path = cases / name / "log.face-resolution"
        if face_path.is_file():
            record["face_resolution"] = read_face_check(face_path)
            face_output = raw / f"{name}-face-resolution.txt"
            face_bytes = normalise_force_header(face_path.read_text()).encode()
            face_output.write_bytes(face_bytes)
            manifest["files"][face_output.relative_to(output).as_posix()] = {"sha256": hashlib.sha256(face_bytes).hexdigest(), "size_bytes": len(face_bytes), "kind": "native hull-face inspection"}
        manifest["runs"].append(record)
        full_path = cases / name / "full-run.json"
        if full_path.is_file():
            full = json.loads(full_path.read_text())
            if full["status"] != "running":
                if full["mesh"] != name:
                    raise ValueError("full-run mesh identity disagrees with its case")
                full["control_dict_sha256"] = manifest["case_evidence"][name]["dictionary_sha256"]["controlDict"]
                verify_criterion(full, criteria)
                manifest["full_runs"].append(collect_full_run(cases / name, full, record["normalisation"], json.loads(criteria), output, manifest["files"]))
    run_log = output / "data/cfd/calm_water_study/run-log.jsonl"
    log_bytes = render_run_log(manifest).encode()
    run_log.write_bytes(log_bytes)
    manifest["files"][run_log.relative_to(output).as_posix()] = {"sha256": hashlib.sha256(log_bytes).hexdigest(), "size_bytes": len(log_bytes), "kind": "native run log"}
    target = output / "data/cfd/calm_water_study/manifest.json"
    target.write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n")
    report = output / "docs/domains/openfoam/calm-water-resistance-study.html"
    report.parent.mkdir(parents=True, exist_ok=True)
    series = {}
    for run in manifest["full_runs"]:
        if run["status"] == "complete":
            names = sorted(name for name in manifest["files"] if Path(name).name.startswith(run["mesh"] + "-full-window-coefficients"))
            series[run["mesh"]] = "".join((output / name).read_text() for name in names)
    report.write_text(render_report(manifest, render_history_plot(series, json.loads(criteria)["averaging_iterations"])))
    return manifest


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cases", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--repo", type=Path, default=Path(__file__).resolve().parents[2])
    arguments = parser.parse_args()
    build_record(arguments.cases, arguments.repo, arguments.output)
