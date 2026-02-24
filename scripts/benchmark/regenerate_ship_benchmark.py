#!/usr/bin/env python3
"""Regenerate ship benchmark reports from existing solver output files.

Re-extracts from the .owr and .LIS files using the corrected extraction
pipeline (phase convention normalization, M24 coupling sign fix) without
re-running the solvers.

Usage:
    uv run python scripts/benchmark/regenerate_ship_benchmark.py
"""
from __future__ import annotations

import json
import sys
from datetime import datetime
from pathlib import Path

REPO_ROOT = Path(__file__).parent.parent.parent
sys.path.insert(0, str(REPO_ROOT / "src"))
sys.path.insert(0, str(REPO_ROOT))

from scripts.benchmark.run_3way_benchmark import (
    _extract_from_aqwa_lis,
    _extract_from_owr,
    _harmonize_frequencies,
    _harmonize_headings,
)
from digitalmodel.hydrodynamics.diffraction.benchmark_runner import (
    BenchmarkConfig,
    BenchmarkRunner,
)

# --- Configuration ---
BENCHMARK_DIR = Path("docs/modules/orcawave/L03_ship_benchmark")
SOURCE_DIR = Path("docs/modules/orcawave/L01_aqwa_benchmark")
OWR_FILE = SOURCE_DIR / "orcawave_001_ship_raos_rev2_matched_500m.owr"
LIS_FILE = BENCHMARK_DIR / "source_data" / "aqwa" / "001_SHIP_RAOS_REV2.LIS"
VESSEL_NAME = "Ship_001_RAOs"
WATER_DEPTH = 500.0

# Output directory — canonical location in docs/modules/
OUTPUT_DIR = BENCHMARK_DIR / "benchmark_results"


def _build_solver_metadata() -> dict:
    """Build per-solver metadata dicts for the ship benchmark.

    No spec YAML file is used -- metadata is hardcoded from the matched
    model properties since we extract directly from .owr and .LIS files.
    """
    # Common metadata shared by both solvers (same physical inputs)
    common = {
        "body_dimensions": "~220m LOA x 32m beam x 8m draft",
        "centre_of_gravity": "(108.88, 0.002, 8.0)",
        "radii_of_gyration": "from inertia tensor",
        "water_density": "1025.0",
        "gravity": "9.80665",
        "mesh_file": "aqwa_001_ship_raos_rev2.dat",
        "remove_irregular_frequencies": "Yes (lid method)",
        "qtf_calculation": "Yes",
        "precision": "double",
    }

    # AQWA-specific
    aqwa_meta = {
        **common,
        "mass": "44,082,200 kg (SI)",
        "mesh_format": "AQWA native",
        "calculation_method": "AQWA Diffraction/Radiation",
        "raw_phase_convention": "ISO 6954 (phase lead)",
        "panel_count": "8,615 (6,754 diffracting + 2,631 lid)",
        "mesh_symmetry": "None",
        "radiation_damping": "Computed (BEM)",
        "viscous_damping": "None applied",
        "damping_lid": "None",
    }

    # OrcaWave-specific
    orcawave_meta = {
        **common,
        "mass": "44,082.2 te (OrcaFlex units)",
        "mesh_format": "AQWA dat import",
        "calculation_method": "Full QTF calculation",
        "raw_phase_convention": "Orcina (phase lag)",
        "panel_count": "8,615",
        "mesh_symmetry": "None",
        "radiation_damping": "Computed (BEM)",
        "viscous_damping": "Roll damping: 36,010 kN\u00b7m\u00b7s/rad",
        "damping_lid": "None",
    }

    return {
        "AQWA": aqwa_meta,
        "OrcaWave": orcawave_meta,
    }


def main() -> int:
    print(f"{'#'*60}")
    print("# Ship Benchmark Regeneration")
    print(f"# Revision: {REVISION}")
    print(f"# Source:   {SOURCE_DIR}")
    print(f"# Output:   {OUTPUT_DIR}")
    print(f"{'#'*60}")

    # Validate source files exist
    for f in (OWR_FILE, LIS_FILE):
        if not f.exists():
            print(f"[ERROR] Source file not found: {f}")
            return 1

    # --- Build metadata (no spec file needed) ---
    solver_metadata = _build_solver_metadata()
    solver_metadata["AQWA"]["input_file"] = str(LIS_FILE)
    # OrcaWave .owr is binary; no text input file available
    # Mesh path for 3D schematic visualization (both solvers share same mesh)
    mesh_file = SOURCE_DIR / "aqwa_001_ship_raos_rev2.dat"
    if mesh_file.exists():
        for solver in solver_metadata:
            solver_metadata[solver]["mesh_path"] = str(mesh_file)
    print("  Metadata built from hardcoded model properties")

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

    # --- Harmonize headings and frequencies ---
    print("\n[3/5] Harmonizing headings...")
    solver_results = {
        "OrcaWave": orcawave_results,
        "AQWA": aqwa_results,
    }
    solver_results = _harmonize_headings(solver_results)

    print("\n[4/5] Harmonizing frequencies...")
    solver_results = _harmonize_frequencies(solver_results)

    # --- Run benchmark comparison ---
    print("\n[5/5] Running benchmark comparison...")
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
            "r1: Ship hull (001_SHIP_RAOS) benchmark. "
            "AQWA (500m depth, 10 freq, 9 headings) vs OrcaWave matched "
            "model (500m depth, 20 freq, 9 headings). "
            "Mass properties matched. r4 report format."
        ),
        "changes": [
            "Initial ship benchmark generation",
            "AQWA native mesh (8,615 panels) vs OrcaWave AQWA dat import",
            "OrcaWave includes roll viscous damping (36,010 kN-m-s/rad)",
            "Phase convention normalization (ISO 6954 -> Orcina)",
            "Per-DOF two-column report layout (r4 format)",
        ],
        "source_files": {
            "orcawave": str(OWR_FILE),
            "aqwa": str(LIS_FILE),
        },
        "previous_revision": None,
    }
    meta_path = BENCHMARK_DIR / "revision.json"
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
