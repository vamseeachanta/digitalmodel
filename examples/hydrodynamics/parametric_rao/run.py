# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Generate, solve and report a compact public parametric RAO pilot."""

import argparse
import json
from pathlib import Path

import numpy as np

from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
from digitalmodel.hydrodynamics.hull_library.parametric_rao_report import build_lookup
from digitalmodel.hydrodynamics.hull_library.rao_database import RAODatabase

ROOT = Path(__file__).resolve().parents[3]
INPUT = ROOT / "examples/hydrodynamics/parametric_rao"


def build(records, basis, criteria, receipt, reference, convergence, output):
    """Build compact retained coefficients, database, lookup and a byte manifest."""
    output.mkdir(parents=True, exist_ok=True)
    db = RAODatabase()
    for record in records:
        entry = pilot.database_entry(record)
        entry.metadata.update({
            "readiness_status": "partial",
            "intended_use": "Discrete pilot lookup; design use remains unverified",
            "heading_convention": "wave propagation angle measured counterclockwise from +x",
            "benchmark_qualified_dofs": ["Heave"] if entry.variation_id == "box_00" else [],
            "benchmark_frequency_rad_s": reference["omega_rad_s"] if entry.variation_id == "box_00" else [],
        })
        db.store(entry)
    database_path = output / "pilot.parquet"
    db.save_to_disk(database_path)
    baseline = records[0]
    heading = baseline["heading_deg"].index(0)
    sample_indices = [baseline["omega_rad_s"].index(w) for w in reference["omega_rad_s"]]
    response = np.abs(np.array(baseline["rao_real"]) + 1j*np.array(baseline["rao_imag"]))
    benchmark = pilot.compare_heave(response[sample_indices, heading, 2], reference, criteria)
    results = {"revision": 0, "basis": {key: value for key, value in basis.items() if key != "sources"}, "criteria": criteria, "solver_version": records[0]["solver_version"], "summary": f'{len(records)} discrete hull variants are solved with Capytaine. Six motion amplitudes and phases are stored for each frequency and heading; the lookup is read from the database. Data readiness is partial; design use remains unverified.',
               "benchmark": benchmark, "convergence": convergence, "records": records}
    pilot.write_json(output / "results.json", results)
    revisions = json.loads((INPUT / "revisions.json").read_text())
    (output / "lookup.html").write_text(build_lookup(database_path, revisions, results), encoding="utf-8")
    manifest = {"schema_version": 1, "criteria_receipt": receipt,
                "first_run_started_at": min(r["started_at"] for r in records),
                "basis_sha256": pilot.digest(INPUT / "basis.json"), "criteria_sha256": pilot.digest(INPUT / "criteria.json"),
                "revisions_sha256": pilot.digest(INPUT / "revisions.json"), "record_count": len(records),
                "inputs": {name: {"source_url": name, "retrieved_on": revisions[-1]["date"], "record_count": count, "sha256": pilot.digest(INPUT / name)} for name, count in (("basis.json", len(records)), ("criteria.json", 1), ("revisions.json", len(revisions)))},
                "input_url_base": "examples/hydrodynamics/parametric_rao/ in the same repository revision",
                "solver_version": records[0]["solver_version"], "sources": [source | {"source_url": f"../../../../../../blob/{basis['source_revision']}/{source['repository_path']}"} if "repository_path" in source else source for source in basis["sources"]],
                "outputs": {p.name: {"sha256": pilot.digest(p), "bytes": p.stat().st_size} for p in sorted(output.iterdir()) if p.name in ("pilot.parquet", "results.json", "lookup.html")},
                "native_evidence": "results.json retains hydrodynamic coefficients, complex excitation, inertia, stiffness and complex RAOs read from solver arrays; meshes remain execution scratch",
                "qualification": "Open-source reproduction and head-sea heave panel sensitivity only"}
    pilot.write_json(output / "manifest.json", manifest)
    pilot.validate_receipt(INPUT / "criteria.json", receipt, manifest["first_run_started_at"])
    return benchmark["passed"] and convergence["passed"]


def parse_arguments():
    """Read explicit scratch, output and execution-mode arguments."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--build-only", action="store_true")
    return parser.parse_args()


def main():
    """Require a matching dry-run table before any native solve."""
    args = parse_arguments()
    basis = json.loads((INPUT / "basis.json").read_text())
    criteria = json.loads((INPUT / "criteria.json").read_text())
    cases = pilot.case_table(basis)
    args.scratch.mkdir(parents=True, exist_ok=True)
    table_path = args.scratch / "case-table.json"
    table = {"basis_sha256": pilot.digest(INPUT / "basis.json"), "cases": cases}
    if args.dry_run:
        for case in cases:
            vertices, panels = pilot.geometry(case)
            if not len(panels) or not np.isfinite(vertices).all():
                raise ValueError("Invalid dry-run geometry")
        pilot.write_json(table_path, table)
        print(f"Dry run: {len(cases)} cases, all geometries validated")
        return
    if not table_path.exists() or json.loads(table_path.read_text()) != table:
        raise ValueError("A matching dry run is required")
    log = args.scratch / "run.jsonl"
    receipt = json.loads(log.read_text().splitlines()[0]) if log.exists() else pilot.freeze_criteria(INPUT / "criteria.json", log)
    if receipt["criteria_sha256"] != pilot.digest(INPUT / "criteria.json"):
        raise ValueError("Criteria changed after launch")
    def obtain(case, local_basis):
        if not args.build_only:
            return pilot.solve_case(case, local_basis, args.scratch, receipt)
        return pilot.read_record(args.scratch / (case["case_id"] + ".json"), case, local_basis, receipt)
    records = []
    for case in cases:
        record = obtain(case, basis)
        records.append(record)
        print(f'Completed {case["case_id"]}: {record["panel_count"]} panels', flush=True)
    raw_reference = json.loads((ROOT / basis["reference_file"]).read_text())
    indices = raw_reference["sample_indices"]
    reference = {"omega_rad_s": [raw_reference["omega_rad_s"][i] for i in indices],
                 "heave_rao": [raw_reference["runs"][0]["heave_rao_0deg"][i] for i in indices]}
    convergence = []
    for resolution in basis["convergence_resolutions"]:
        case = cases[0] | {"case_id": "convergence_" + "_".join(map(str, resolution)), "box_resolution": resolution}
        local_basis = basis | {"omega_rad_s": reference["omega_rad_s"], "heading_deg": basis["heading_deg"]}
        record = obtain(case, local_basis)
        response = np.abs(np.array(record["rao_real"]) + 1j*np.array(record["rao_imag"]))
        convergence.append(record | {"heave_rao": response[:, record["heading_deg"].index(0), 2].tolist()})
        print(f'Convergence: {record["panel_count"]} panels', flush=True)
    comparison = pilot.convergence_record(convergence, criteria)
    # Retain each convergence coefficient record with the main report data.
    passed = build(records, basis, criteria, receipt, reference, comparison, args.output)
    pilot.write_json(args.output / "convergence-native.json", convergence)
    manifest = json.loads((args.output / "manifest.json").read_text())
    manifest["outputs"]["convergence-native.json"] = {"sha256": pilot.digest(args.output / "convergence-native.json"), "bytes": (args.output / "convergence-native.json").stat().st_size}
    pilot.write_json(args.output / "manifest.json", manifest)
    print("Pilot criterion result:", "PASS" if passed else "FAIL")
    if not passed:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
