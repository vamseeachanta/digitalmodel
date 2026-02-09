#!/usr/bin/env python3
"""Regenerate barge benchmark reports from existing solver output files.

Re-extracts from the .owr and .LIS files using the corrected extraction
pipeline (phase convention normalization, M24 coupling sign fix) without
re-running the solvers.

Usage:
    uv run python scripts/benchmark/regenerate_barge_benchmark.py
"""
from __future__ import annotations

import json
import sys
from datetime import datetime
from pathlib import Path

import yaml

REPO_ROOT = Path(__file__).parent.parent.parent
sys.path.insert(0, str(REPO_ROOT / "src"))
sys.path.insert(0, str(REPO_ROOT))

from scripts.benchmark.run_3way_benchmark import (
    _extract_from_aqwa_lis,
    _extract_from_owr,
    _harmonize_headings,
)
from digitalmodel.hydrodynamics.diffraction.benchmark_runner import (
    BenchmarkConfig,
    BenchmarkRunner,
)

# --- Configuration ---
SOURCE_DIR = Path("benchmark_output/barge_final")
OWR_FILE = SOURCE_DIR / "orcawave" / "Barge_Benchmark.owr"
LIS_FILE = SOURCE_DIR / "aqwa" / "WRK-031_3WAY_BENCHMARK.LIS"
SPEC_FILE = Path("specs/modules/benchmark/barge_benchmark_spec.yml")
VESSEL_NAME = "Barge_Benchmark"
WATER_DEPTH = 200.0

# Revision-tracked output directory
REVISION = "r4_per_dof_report"
OUTPUT_DIR = Path("benchmark_output/barge_benchmark") / REVISION


def _build_solver_metadata(spec: dict) -> dict:
    """Build per-solver metadata dicts from the benchmark spec.

    Both solvers share the same geometry/mass/environment inputs.
    Solver-specific fields (mesh format, unit system) differ.
    """
    vessel = spec.get("vessel", {})
    geom = vessel.get("geometry", {})
    dims = geom.get("dimensions", {})
    inertia = vessel.get("inertia", {})
    env = spec.get("environment", {})
    solver_opts = spec.get("solver_options", {})

    length = dims.get("length", "")
    beam = dims.get("beam", "")
    draft = dims.get("draft", "")

    mass_kg = inertia.get("mass", 0)
    cog = inertia.get("centre_of_gravity", [])
    rog = inertia.get("radii_of_gyration", [])

    # Common metadata shared by both solvers (same physical inputs)
    common = {
        "length": f"{length}",
        "beam": f"{beam}",
        "draft": f"{draft}",
        "body_dimensions": f"{length} x {beam} x {draft}",
        "mass": f"{mass_kg:,.0f} kg",
        "centre_of_gravity": f"({', '.join(str(v) for v in cog)})",
        "radii_of_gyration": f"({', '.join(str(v) for v in rog)})",
        "water_density": f"{env.get('water_density', '')}",
        "gravity": f"{env.get('gravity', '')}",
        "mesh_file": geom.get("mesh_file", ""),
        "remove_irregular_frequencies": str(
            solver_opts.get("remove_irregular_frequencies", "")
        ),
        "qtf_calculation": str(solver_opts.get("qtf_calculation", "")),
        "precision": str(solver_opts.get("precision", "")),
    }

    # AQWA-specific
    aqwa_meta = {
        **common,
        "mass": f"{mass_kg:,.0f} kg (SI)",
        "mesh_format": "GDF (WAMIT)",
        "calculation_method": "AQWA Diffraction/Radiation",
        "raw_phase_convention": "ISO 6954 (phase lead)",
        "panel_count": "912",
        "mesh_symmetry": geom.get("symmetry", "none"),
        "radiation_damping": "Computed (BEM)",
        "viscous_damping": "None applied",
        "damping_lid": "None",
    }

    # OrcaWave-specific
    mass_te = mass_kg / 1000.0
    orcawave_meta = {
        **common,
        "mass": f"{mass_te:,.1f} te (OrcaFlex units)",
        "mesh_format": "WAMIT GDF",
        "calculation_method": "Potential + source formulations",
        "raw_phase_convention": "Orcina (phase lag)",
        "panel_count": "912",
        "mesh_symmetry": "None",
        "radiation_damping": "Computed (BEM)",
        "viscous_damping": "None applied",
        "damping_lid": "None",
    }

    return {
        "AQWA": aqwa_meta,
        "OrcaWave": orcawave_meta,
    }


def main() -> int:
    print(f"{'#'*60}")
    print("# Barge Benchmark Regeneration")
    print(f"# Revision: {REVISION}")
    print(f"# Source:   {SOURCE_DIR}")
    print(f"# Output:   {OUTPUT_DIR}")
    print(f"{'#'*60}")

    # Validate source files exist
    for f in (OWR_FILE, LIS_FILE, SPEC_FILE):
        if not f.exists():
            print(f"[ERROR] Source file not found: {f}")
            return 1

    # --- Load spec for metadata ---
    with open(SPEC_FILE) as f:
        spec = yaml.safe_load(f)
    solver_metadata = _build_solver_metadata(spec)
    print(f"  Spec loaded: {SPEC_FILE}")

    # --- Extract with corrected pipeline ---
    print("\n[1/4] Extracting OrcaWave results...")
    orcawave_results = _extract_from_owr(OWR_FILE, VESSEL_NAME, WATER_DEPTH)
    if orcawave_results is None:
        print("[ERROR] Failed to extract OrcaWave results")
        return 1
    n_freq = orcawave_results.raos.surge.frequencies.count
    n_head = orcawave_results.raos.surge.headings.count
    print(f"  OK: {n_freq} frequencies x {n_head} headings")
    print(f"  phase_convention={orcawave_results.phase_convention}")
    print(f"  unit_system={orcawave_results.unit_system}")

    print("\n[2/4] Extracting AQWA results (with phase/coupling normalization)...")
    aqwa_results = _extract_from_aqwa_lis(LIS_FILE, VESSEL_NAME, WATER_DEPTH)
    if aqwa_results is None:
        print("[ERROR] Failed to extract AQWA results")
        return 1
    n_freq = aqwa_results.raos.surge.frequencies.count
    n_head = aqwa_results.raos.surge.headings.count
    print(f"  OK: {n_freq} frequencies x {n_head} headings")
    print(f"  phase_convention={aqwa_results.phase_convention}")
    print(f"  unit_system={aqwa_results.unit_system}")

    # --- Harmonize headings ---
    print("\n[3/4] Harmonizing headings...")
    solver_results = {
        "OrcaWave": orcawave_results,
        "AQWA": aqwa_results,
    }
    solver_results = _harmonize_headings(solver_results)

    # --- Run benchmark comparison ---
    print("\n[4/4] Running benchmark comparison...")
    config = BenchmarkConfig(
        output_dir=OUTPUT_DIR,
        tolerance=0.05,
        x_axis="period",
    )
    runner = BenchmarkRunner(config)
    result = runner.run_from_results(solver_results, solver_metadata=solver_metadata)

    if not result.success:
        print(f"[ERROR] Benchmark failed: {result.error_message}")
        return 1

    # --- Write revision metadata ---
    revision_meta = {
        "revision": REVISION,
        "timestamp": datetime.now().isoformat(),
        "description": (
            "r4: Single-page report with per-DOF two-column layout "
            "(text/conclusions left, plot right). Individual legends per "
            "plot. Solver-column comparison tables. Alternating row colors. "
            "All fixes from r2/r3 carried forward."
        ),
        "changes": [
            "Per-DOF sections: text left, inline Plotly plot right",
            "Individual legend per DOF plot",
            "Solver-column comparison tables (headings as rows)",
            "Alternating row colors, hover highlight, dark header",
            "Consensus badges with color coding",
            "Auto-generated observations per DOF",
            "Carried forward: r2 phase/M24 fix, r3 input comparison",
        ],
        "source_files": {
            "orcawave": str(OWR_FILE),
            "aqwa": str(LIS_FILE),
            "spec": str(SPEC_FILE),
        },
        "previous_revision": "r3_input_comparison",
    }
    meta_path = OUTPUT_DIR / "revision.json"
    with open(meta_path, "w") as f:
        json.dump(revision_meta, f, indent=2)

    # --- Summary ---
    print(f"\n{'#'*60}")
    print("# Results")
    print(f"{'#'*60}")
    print(f"  Overall consensus: {result.report.overall_consensus}")

    report_data = runner._report_to_dict(result.report)
    pair = report_data["pairwise_results"].get("AQWA-vs-OrcaWave", {})
    rao_comps = pair.get("rao_comparisons", {})
    print("\n  Phase correlations (should be POSITIVE now):")
    for dof, data in rao_comps.items():
        phase_corr = data.get("phase_correlation", 0)
        mag_corr = data.get("magnitude_correlation", 0)
        status = "OK" if phase_corr > 0 else "NEGATIVE"
        print(f"    {dof:6s}: phase_corr={phase_corr:+.4f}  "
              f"mag_corr={mag_corr:.4f}  [{status}]")

    am_corrs = pair.get("added_mass_correlations", {})
    m24_corr = am_corrs.get("2,4", None)
    if m24_corr is not None:
        status = "OK" if m24_corr > 0 else "NEGATIVE"
        print(f"\n  Added mass M24 correlation: {m24_corr:+.4f} [{status}]")

    print(f"\n  Report HTML: {result.report_html_path}")
    print(f"  Report JSON: {result.report_json_path}")
    print(f"  Revision metadata: {meta_path}")
    print(f"  Plots: {len(result.plot_paths)} generated")

    return 0


if __name__ == "__main__":
    sys.exit(main())
