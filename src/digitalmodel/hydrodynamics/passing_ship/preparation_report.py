# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build a deterministic passing-ship preparation record and report skeleton."""

from __future__ import annotations

import argparse
import hashlib
from html import escape
import json
from pathlib import Path

from .input_schemas import PassingShipSpec
from .preparation import execute_cases, prepare_campaign
from .formulations import finite_depth_correction


INPUT_NAMES = ("model.json", "matrix.json", "benchmark_candidates.json", "criteria.json",
               "sources.json", "revisions.json", "execution.json", "benchmark_status.json")


def _read(path: Path) -> dict:
    return json.loads(path.read_text())


def _digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _write(path: Path, value: object) -> None:
    path.write_text(json.dumps(value, sort_keys=True, indent=2, allow_nan=False) + "\n")


def _revision(data: dict) -> str:
    if not data["history"] or data["history"][-1]["revision"] != data["revision"]:
        raise ValueError("declared revision must match the newest revision row")
    rows = "".join("<tr>" + "".join(f"<td>{escape(str(row[key]))}</td>"
                                  for key in ("revision", "date", "change")) + "</tr>"
                   for row in data["history"])
    return ('<section id="revisions"><h2>Revision history</h2><table>'
            '<tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr>'
            + rows + '</table></section>')


def build(data: Path, output: Path) -> dict:
    """Write deterministic force samples, a manifest template and one HTML file.

    The native force samples are computational examples, not reproduced paper
    values. The template deliberately contains no solver-ready or approval state.
    """
    root = Path(__file__).resolve().parents[4]
    model_record = _read(data / "model.json")
    model = PassingShipSpec.model_validate(model_record)
    matrix_record = _read(data / "matrix.json")
    matrix = prepare_campaign(model, matrix_record, mooring_layout=model_record["mooring_layout"])
    candidates = _read(data / "benchmark_candidates.json")
    criteria = _read(data / "criteria.json")
    benchmark_status = _read(data / "benchmark_status.json")
    revisions = _read(data / "revisions.json")
    receipt = _read(data / "execution.json")
    if receipt["criteria_sha256"] != _digest(data / "criteria.json"):
        raise ValueError("criteria digest mismatch")
    from datetime import datetime
    if datetime.fromisoformat(receipt["recorded_at_utc"]) >= datetime.fromisoformat(receipt["first_run_started_at_utc"]):
        raise ValueError("criteria must precede the first force run")
    comparison_started = receipt["first_benchmark_comparison_started_at_utc"]
    if comparison_started is not None and datetime.fromisoformat(receipt["recorded_at_utc"]) >= datetime.fromisoformat(comparison_started):
        raise ValueError("criteria must precede the first benchmark comparison")
    output.mkdir(parents=True, exist_ok=True)
    samples = []
    for spec in matrix:
        half_length = spec.moored_vessel.hull.loa / 2
        rows = execute_cases(spec, staggers_m=[-half_length, 0, half_length])
        rows[0]["depth_to_draft"] = spec.environment.water_depth / spec.moored_vessel.effective_draft()
        samples.extend(rows)
    _write(output / "force_samples.json", {"status": "unqualified force examples; depth trend entirely placeholder-driven; no coupled response",
                                           "base_commit": receipt["base_commit"], "cases": samples})
    _write(output / "campaign.json", {"cases": [spec.model_dump() for spec in matrix]})
    candidate_rows = "".join(f'<tr><td>{escape(row["id"])}</td><td>Processes 2021, 9, 558; input citation in benchmark_candidates.json</td>'
                             f'<td>{escape(row["status"])}</td></tr>' for row in candidates["cases"])
    sources = _read(data / "sources.json")
    source_rows = "".join(f'<li><a href="{escape(row["url"], quote=True)}">{escape(row["id"])}</a>'
                          f'; retrieved {escape(row["retrieval_date"])}; {escape(row["use"])}</li>'
                          for row in sources["sources"])
    force_rows = "".join(f'<tr><td>{row["name"]}</td><td>{row["depth_to_draft"]:.3f}</td><td>{row["surge_n"][1]:.1f}</td>'
                         f'<td>{row["sway_n"][1]:.1f}</td><td>{row["yaw_nm"][1]:.1f}</td></tr>'
                         for row in samples)
    factor_ranges = []
    for ratio in matrix_record["depth_to_draft"]:
        factors = [float(finite_depth_correction(
            h=ratio * model.moored_vessel.effective_draft(),
            T=(model.moored_vessel.effective_draft() + model.passing_ship.dimensions.draft) / 2,
            L=model.moored_vessel.hull.loa, y=separation, mode="sway"))
            for separation in matrix_record["separation_m"]]
        low, high = f"{min(factors):.3f}", f"{max(factors):.3f}"
        factor_ranges.append(f"{low if low == high else low + ' to ' + high} at h/D {ratio:.3f}")
    depth_limit = (
        "<p>The existing finite-depth correction in formulations.py is an uncited placeholder. "
        "Its source labels the implementation simplified and says the actual formulation is still needed. "
        "The shallow-water multiplier is an uncited linear factor: "
        "1 + 0.5 * (1 - (h/D)/1.5) for h/D &lt; 1.5, otherwise 1. "
        "Recomputed sway factors over the three separations are " + "; ".join(factor_ranges) + ". "
        "The existing correction factor supplies the whole depth trend of all 18 force cases; "
        "this trend does not qualify shallow-water behaviour. #2017 remains open.</p>"
    )
    layout = model_record["mooring_layout"]
    depth_lengths = {spec.environment.water_depth: spec.moored_vessel.mooring_lines[0].length for spec in matrix}
    lengths = ", ".join(f"{depth_lengths[depth]:.1f}" for depth in sorted(depth_lengths))
    inner_edge = min(matrix_record["separation_m"]) - model.passing_ship.dimensions.beam / 2
    layout_limit = (
        "<p>Four assumed inextensible lines retain 600 kN fairlead pretension. "
        f"A submerged weight of {layout['submerged_weight_n_m']:.1f} N/m and fairleads at water level are assumed, based on previous experience. "
        "The repository touchdown catenary sizes each independent depth layout: line lengths are "
        f"{lengths} m at the three increasing depths. Anchors at lateral offsets of plus or minus "
        f"{layout['anchor_abs_y_m']:.1f} m lie inside the closest passing hull edge ({inner_edge:.1f} m); longitudinal anchor positions vary with depth. "
        "These isolated static lines pass the excess-length and swept-track checks. "
        "Elastic extension, seabed properties and whole-vessel equilibrium remain unqualified before #2016.</p>"
    )
    sections = [
        ("Summary", f"<p>Preparation only: {len(matrix)} force cases; {len(candidates['cases'])} published input candidates; zero qualified published benchmarks. No vessel response has been computed.</p><p>{escape(benchmark_status['statement'])}</p>"),
        ("Scope", "<p>Parallel passing-ship forces are sampled with the existing native calculator. Tension, motion and fender response remain open under #2016. Published benchmark qualification remains open under #2017.</p>"),
        ("Input basis", "<p>Public hull dimensions and coefficients: paper Table 3. Separation and depth ratios: Table 2. Speed and density remain assumed inputs. The layout assumptions and sizing results are recorded in model.json.</p>" + layout_limit),
        ("Method", "<p>Existing surge, sway and yaw integrals are used unchanged. Samples at half a hull length astern, abeam and half a hull length ahead are not coefficient peaks. The sectional-area approximation is unvalidated. There is no time-domain response integration. Wind is excluded unless an explicit force and moment vector is supplied.</p>" + depth_limit),
        ("Case matrix", f"<p>{len(matrix)} Cartesian entries vary passing speed, centreline separation and water depth divided by moored draft. The case data are recorded in campaign.json.</p>"),
        ("Force examples", '<p>Computed abeam force samples below are numerical examples only. Forces are in N; yaw moment is in N m. The depth trend is entirely placeholder-driven by the uncited correction described in section 4. They carry no measured-data agreement verdict.</p><table><tr><th>Case</th><th>Depth/draft (placeholder-driven)</th><th>Surge</th><th>Sway</th><th>Yaw</th></tr>' + force_rows + '</table>'),
        ("Mooring response", "<p>NOT COMPUTED: tensions, excursions, wind physics, fender loads and response plots. Mass, damping, hydrostatics, mooring stiffness and fender characteristics require a qualified response model. Requested outputs are recorded explicitly as uncomputed. #2016 remains open.</p>"),
        ("Benchmark criteria", '<p>Equation (9) defines Cx and Cy as force divided by 0.5 rho D L V^2, and Cm as moment divided by 0.5 rho D L^2 V^2. Positive maxima are compared with the model-test series. The full peak-search stagger window is not established from the opened text. Fixed force samples cannot supply these peaks. Proposed relative tolerance: ' + f'{100 * criteria["relative_tolerance"]:.1f}%' + '. The dimensionless absolute floors and their SI scaling basis are frozen in criteria.json. These limits do not represent experimental uncertainty. Numerical paper references remain unavailable; no benchmark comparison has been made.</p><table><tr><th>Candidate</th><th>Citation</th><th>Status</th></tr>' + candidate_rows + '</table>'),
        ("Way forward", "<p>Obtain tabulated numerical force and moment data covering distinct speed, separation, depth and size conditions. Establish response inputs and qualify an execution adapter. Launch remains refused until both a qualified adapter and an approved manifest digest exist. #2016 and #2017 remain open.</p>"),
        ("Sources and limitations", '<p>Plot digitisation, licensed solver execution and standards acceptance are excluded. Numerical reference peaks and their full stagger window are not established; #2017 remains open.</p><h3>Known limits</h3>' + depth_limit + layout_limit + '<ul>' + source_rows + '</ul>'),
    ]
    content = []
    for number, (title, body) in enumerate(sections, start=1):
        content.append(f'<section id="section-{number}"><h2>{number}. {title}</h2>{body}</section>')
        if number == 1:
            content.append(_revision(revisions))
    page = ('<!doctype html>\n<html lang="en"><head><meta charset="utf-8">'
            '<meta name="viewport" content="width=device-width, initial-scale=1">'
            '<title>Passing-ship mooring analysis preparation</title>'
            '<style>body{font-family:sans-serif;max-width:72em;margin:2em auto;padding:1em}table{border-collapse:collapse}td,th{border:1px solid #888;padding:.4em;text-align:left}section{margin:2em 0}</style>'
            '</head><body><h1>Passing-ship mooring analysis preparation</h1>'
            f'<p>Revision: {escape(revisions["revision"])}</p>' + ''.join(content) + '</body></html>\n')
    (output / "report.html").write_text(page)
    inputs = {str((data / name).relative_to(root)): _digest(data / name) for name in INPUT_NAMES}
    for module in (Path(__file__), Path(__file__).with_name("preparation.py"),
                   Path(__file__).with_name("calculator.py"), Path(__file__).with_name("formulations.py"),
                   Path(__file__).with_name("configuration.py"), Path(__file__).with_name("input_schemas.py")):
        inputs[str(module.relative_to(root))] = _digest(module)
    catenary = root / "src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py"
    inputs[str(catenary.relative_to(root))] = _digest(catenary)
    manifest = {"revision": revisions["revision"], "base_commit": receipt["base_commit"],
                "execution": receipt, "launch_allowed": False, "approved_manifest_sha256": None,
                "adapter": None, "qualified_benchmarks": 0, "matrix_entries": len(matrix),
                "benchmark_check": benchmark_status,
                "response_cases": 0, "header_replacements": "none", "inputs": inputs,
                "outputs": {name: _digest(output / name) for name in ("report.html", "force_samples.json", "campaign.json")}}
    counts = {"model.json": 1, "matrix.json": len(matrix),
              "benchmark_candidates.json": len(candidates["cases"]),
              "revisions.json": len(revisions["history"]), "sources.json": len(sources["sources"])}
    manifest["input_provenance"] = {
        name: {"sha256": digest, "record_count": counts.get(Path(name).name, 1),
               "retrieval_date": revisions["history"][0]["date"],
               "source_url": sources["sources"][0]["url"] if Path(name).name in
               ("model.json", "matrix.json", "benchmark_candidates.json") else
               "repository-relative:" + name,
               "basis": "Public input facts plus explicit study assumptions; not measured response data."
               if Path(name).name in ("model.json", "matrix.json", "benchmark_candidates.json") else
               "Preparation record or implementation; first issue created with this change."}
        for name, digest in inputs.items()
    }
    _write(output / "manifest.template.json", manifest)
    (output / "manifest.sha256").write_text(_digest(output / "manifest.template.json") + "  manifest.template.json\n")
    return manifest


def main() -> None:
    """Build the preparation record at an explicit output destination."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--data-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    arguments = parser.parse_args()
    build(arguments.data_dir.resolve(), arguments.output_dir.resolve())


if __name__ == "__main__":
    main()
