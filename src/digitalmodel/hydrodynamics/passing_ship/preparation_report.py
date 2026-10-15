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


INPUT_NAMES = ("model.json", "matrix.json", "benchmark_candidates.json", "criteria.json",
               "sources.json", "revisions.json", "execution.json")


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
    model = PassingShipSpec.model_validate(_read(data / "model.json"))
    matrix = prepare_campaign(model, _read(data / "matrix.json"))
    candidates = _read(data / "benchmark_candidates.json")
    criteria = _read(data / "criteria.json")
    revisions = _read(data / "revisions.json")
    receipt = _read(data / "execution.json")
    if receipt["criteria_sha256"] != _digest(data / "criteria.json"):
        raise ValueError("criteria digest mismatch")
    from datetime import datetime
    if datetime.fromisoformat(receipt["recorded_at_utc"]) >= datetime.fromisoformat(receipt["first_run_started_at_utc"]):
        raise ValueError("criteria must precede the first force run")
    output.mkdir(parents=True, exist_ok=True)
    samples = []
    for spec in matrix:
        half_length = spec.moored_vessel.hull.loa / 2
        rows = execute_cases(spec, staggers_m=[-half_length, 0, half_length])
        rows[0]["depth_to_draft"] = spec.environment.water_depth / spec.moored_vessel.effective_draft()
        samples.extend(rows)
    _write(output / "force_samples.json", {"status": "unqualified force examples; no coupled response",
                                           "base_commit": receipt["base_commit"], "cases": samples})
    _write(output / "campaign.json", {"cases": [spec.model_dump() for spec in matrix]})
    candidate_rows = "".join(f'<tr><td>{escape(row["id"])}</td><td>{escape(row["source"])}</td>'
                             f'<td>{escape(row["status"])}</td></tr>' for row in candidates["cases"])
    sources = _read(data / "sources.json")
    source_rows = "".join(f'<li><a href="{escape(row["url"], quote=True)}">{escape(row["id"])}</a>'
                          f'; retrieved {escape(row["retrieval_date"])}; {escape(row["use"])}</li>'
                          for row in sources["sources"])
    force_rows = "".join(f'<tr><td>{row["name"]}</td><td>{row["surge_n"][1]:.1f}</td>'
                         f'<td>{row["sway_n"][1]:.1f}</td><td>{row["yaw_nm"][1]:.1f}</td></tr>'
                         for row in samples)
    sections = [
        ("Summary", f"<p>Preparation only: {len(matrix)} force cases; {len(candidates['cases'])} published input candidates; zero qualified published benchmarks. No vessel response has been computed.</p>"),
        ("Scope", "<p>Parallel passing-ship forces are sampled with the existing native calculator. Tension, motion and fender response remain open under #2016. Published benchmark qualification remains open under #2017.</p>"),
        ("Input basis", "<p>Hull dimensions and coefficients: Park, Nam and Kim (2021), Table 3. Separation and depth ratios: Table 2. Mooring geometry, line length, pretension, speed and density are assumed inputs; these do not define a measured mooring system. Mooring inputs are assumed, based on previous experience.</p>"),
        ("Method", "<p>Existing surge, sway and yaw integrals and finite-depth correction are used unchanged. Samples are taken at half a hull length astern, abeam and half a hull length ahead. The sectional-area approximation and finite-depth correction have not been validated against the opened papers. There is no time-domain response integration. Wind is excluded unless an explicit force and moment vector is supplied.</p>"),
        ("Case matrix", f"<p>{len(matrix)} Cartesian entries vary passing speed, centreline separation and water depth divided by moored draft. The case data are recorded in campaign.json.</p>"),
        ("Force examples", '<p>Computed abeam force samples below are numerical examples only. Forces are in N; yaw moment is in N m. They carry no measured-data agreement verdict.</p><table><tr><th>Case</th><th>Surge</th><th>Sway</th><th>Yaw</th></tr>' + force_rows + '</table>'),
        ("Mooring response", "<p>NOT COMPUTED: tensions, excursions, wind physics, fender loads and response plots. Mass, damping, hydrostatics, mooring stiffness and fender characteristics require a qualified response model. Requested outputs are recorded explicitly as uncomputed. #2016 remains open.</p>"),
        ("Benchmark criteria", '<p>Proposed relative tolerance: ' + f'{100 * criteria["relative_tolerance"]:.1f}%' + '. Absolute floors: 1.0 N for each force and 1.0 N m for yaw. These proposed limits precede computation and do not represent experimental uncertainty. Numerical paper references are unavailable without plot digitisation; no comparison has been made.</p><table><tr><th>Candidate</th><th>Citation</th><th>Status</th></tr>' + candidate_rows + '</table>'),
        ("Way forward", "<p>Obtain tabulated numerical force and moment data covering distinct speed, separation, depth and size conditions. Establish response inputs and qualify an execution adapter. Launch remains refused until both a qualified adapter and an approved manifest digest exist. #2016 and #2017 remain open.</p>"),
        ("Sources and limitations", '<p>Historical project material is replaced by published sources. Plot digitisation, licensed solver execution and standards acceptance are excluded.</p><ul>' + source_rows + '</ul>'),
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
    manifest = {"revision": revisions["revision"], "base_commit": receipt["base_commit"],
                "execution": receipt, "launch_allowed": False, "approved_manifest_sha256": None,
                "adapter": None, "qualified_benchmarks": 0, "matrix_entries": len(matrix),
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
