#!/usr/bin/env python3
"""Run spar diffraction benchmark: AQWA vs OrcaWave.

Step 0: Run AQWA solver on pre-existing simple.dat (RESTART 1 5)
Step 1: Run OrcaWave diffraction via OrcFxAPI
Step 2: Extract AQWA results from SIMPLE.LIS
Step 3: Harmonize headings and frequencies (interpolation)
Step 4: Run benchmark comparison with r4 report format

Usage:
    uv run python scripts/benchmark/run_spar_benchmark.py
    uv run python scripts/benchmark/run_spar_benchmark.py --skip-aqwa
    uv run python scripts/benchmark/run_spar_benchmark.py --skip-orcawave
"""
from __future__ import annotations

import json
import shutil
import sys
import time
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
from digitalmodel.hydrodynamics.diffraction.aqwa_runner import (
    AQWARunConfig,
    AQWARunResult,
    AQWARunner,
    AQWARunStatus,
)

# --- Configuration ---
BENCHMARK_DIR = Path("docs/modules/orcawave/L04_spar_benchmark")
ORCAWAVE_YML = BENCHMARK_DIR / "source_data" / "orcawave" / "spar_benchmark.yml"
AQWA_DAT = Path("docs/modules/aqwa/examples/spar-example/simple.dat")
AQWA_DIR = AQWA_DAT.parent  # AQWA runs from the .dat directory
MESH_SRC = AQWA_DAT  # Same file (AQWA .dat contains geometry)
LIS_FILE = AQWA_DIR / "SIMPLE.LIS"
VESSEL_NAME = "Spar_Benchmark"
WATER_DEPTH = 200.0

# Output directory — canonical location in docs/modules/
OUTPUT_DIR = BENCHMARK_DIR / "benchmark_results"


def _build_solver_metadata() -> dict:
    """Build per-solver metadata for the spar benchmark."""
    common = {
        "body_dimensions": "Cylinder D=25m, draft=110m, freeboard=15m",
        "centre_of_gravity": "(0.0, 0.0, -61.63)",
        "radii_of_gyration": "(75.3, 75.3, 62.7)",
        "water_density": "1025.0",
        "gravity": "9.80665",
        "mesh_file": "simple.dat (AQWA dat format)",
        "remove_irregular_frequencies": "Yes (lid method)",
        "qtf_calculation": "No",
        "precision": "double",
    }
    aqwa_meta = {
        **common,
        "mass": "55,000,000 kg (SI)",
        "mesh_format": "AQWA native",
        "calculation_method": "AQWA Diffraction/Radiation",
        "raw_phase_convention": "ISO 6954 (phase lead)",
        "panel_count": "~8,283",
        "mesh_symmetry": "None",
        "frequency_count": "3",
        "heading_count": "9 (0° to 180° in 22.5° steps, AQWA default)",
    }
    orcawave_meta = {
        **common,
        "mass": "55,000 te (OrcaFlex units)",
        "mesh_format": "AQWA dat import",
        "calculation_method": "Potential formulation only",
        "raw_phase_convention": "Orcina (phase lag)",
        "panel_count": "~8,283 (same mesh)",
        "mesh_symmetry": "None",
        "frequency_count": "20",
        "heading_count": "5 (0°, 45°, 90°, 135°, 180°)",
    }
    return {"AQWA": aqwa_meta, "OrcaWave": orcawave_meta}


def _run_aqwa(dat_file: Path) -> bool:
    """Execute AQWA solver on an existing .dat file.

    Runs from the directory containing the .dat file so AQWA can find
    mesh data and writes output (.LIS, .AH1) in the same directory.

    Returns True on success, False on failure.
    """
    dat_dir = dat_file.parent.resolve()
    print(f"  Input:  {dat_file}")
    print(f"  CWD:    {dat_dir}")

    # v181 needs ANSYS license server (gives "No servers provided" error).
    # v252 works fine with v18.1 inputs when RESTART stages are correct (1 5).
    # Let auto-detection find the best available version.
    aqwa_exe = None

    config = AQWARunConfig(
        executable_path=aqwa_exe,
        output_dir=dat_dir,
        dry_run=False,
        timeout_seconds=3600,
        copy_mesh_files=False,
        nowind=True,
    )
    runner = AQWARunner(config)

    # Manually set up the result and input file, bypassing prepare()
    # which would try to generate a new .dat from a DiffractionSpec.
    runner._result = AQWARunResult(
        status=AQWARunStatus.PREPARING,
        output_dir=dat_dir,
        input_file=dat_file.resolve(),
        spec_name=dat_file.stem,
    )

    start = time.time()
    result = runner.execute()
    elapsed = time.time() - start

    if result.status == AQWARunStatus.COMPLETED:
        print(f"  AQWA completed in {elapsed:.1f}s")
        if result.lis_file:
            print(f"  LIS: {result.lis_file}")
        if result.ah1_file:
            print(f"  AH1: {result.ah1_file}")
        return True
    elif result.status == AQWARunStatus.DRY_RUN:
        print("  [WARN] AQWA executable not found — dry run only")
        print("  Set AQWA_PATH environment variable or install ANSYS AQWA")
        return False
    else:
        print(f"  [ERROR] AQWA failed: {result.error_message}")
        if result.stderr:
            print(f"  stderr: {result.stderr[:500]}")
        return False


def main() -> int:
    skip_aqwa = "--skip-aqwa" in sys.argv
    skip_orcawave = "--skip-orcawave" in sys.argv

    print(f"{'#'*60}")
    print("# Spar Benchmark: AQWA vs OrcaWave")
    print(f"# Revision: {REVISION}")
    print(f"# Output:   {OUTPUT_DIR}")
    if skip_aqwa:
        print("# Mode:    --skip-aqwa (using existing .LIS)")
    if skip_orcawave:
        print("# Mode:    --skip-orcawave (using existing .owr)")
    print(f"{'#'*60}")

    # Validate source files
    if not AQWA_DAT.exists():
        print(f"[ERROR] AQWA input not found: {AQWA_DAT}")
        return 1
    if not ORCAWAVE_YML.exists():
        print(f"[ERROR] OrcaWave YAML not found: {ORCAWAVE_YML}")
        return 1

    # Ensure mesh file is next to OrcaWave YAML
    mesh_dst = ORCAWAVE_YML.parent / MESH_SRC.name
    if not mesh_dst.exists():
        print(f"  Copying mesh: {MESH_SRC} -> {mesh_dst}")
        shutil.copy2(MESH_SRC, mesh_dst)

    solver_metadata = _build_solver_metadata()
    solver_metadata["AQWA"]["input_file"] = str(AQWA_DAT)
    solver_metadata["OrcaWave"]["input_file"] = str(ORCAWAVE_YML)
    # Mesh path for 3D schematic visualization (both solvers share same mesh)
    for solver in solver_metadata:
        solver_metadata[solver]["mesh_path"] = str(AQWA_DAT)

    # --- Step 0: Run AQWA ---
    if not skip_aqwa:
        print("\n[0/5] Running AQWA solver...")
        ok = _run_aqwa(AQWA_DAT)
        if not ok:
            if not LIS_FILE.exists():
                print("[ERROR] No existing .LIS file and AQWA failed")
                return 1
            print("  Falling back to existing .LIS file")
    else:
        print("\n[0/5] Skipping AQWA (--skip-aqwa)")

    if not LIS_FILE.exists():
        print(f"[ERROR] .LIS file not found: {LIS_FILE}")
        return 1

    # --- Step 1: Run OrcaWave ---
    owr_path = OUTPUT_DIR / f"{ORCAWAVE_YML.stem}.owr"
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    if not skip_orcawave:
        print("\n[1/5] Running OrcaWave diffraction via OrcFxAPI...")
        try:
            import OrcFxAPI

            start = time.time()
            yml_abs = str(ORCAWAVE_YML.resolve())
            print(f"  Loading: {yml_abs}")
            diffraction = OrcFxAPI.Diffraction(yml_abs)
            print("  Calculating (20 freq x 5 headings)...")
            diffraction.Calculate()
            elapsed = time.time() - start
            n_freq = len(diffraction.frequencies)
            n_head = len(diffraction.headings)
            print(f"  Calculation completed in {elapsed:.1f}s ({n_freq} freq x {n_head} headings)")

            # Save results
            diffraction.SaveResults(str(owr_path.resolve()))
            data_path = OUTPUT_DIR / f"{ORCAWAVE_YML.stem}_data.dat"
            diffraction.SaveData(str(data_path.resolve()))
            print(f"  Saved: {owr_path.name}")

        except ImportError:
            print("[ERROR] OrcFxAPI not available")
            return 1
        except Exception as e:
            print(f"[ERROR] OrcaWave failed: {e}")
            return 1
    else:
        print("\n[1/5] Skipping OrcaWave (--skip-orcawave)")
        if not owr_path.exists():
            print(f"[ERROR] .owr file not found: {owr_path}")
            return 1

    # --- Step 2: Extract results ---
    print("\n[2/5] Extracting OrcaWave results...")
    orcawave_results = _extract_from_owr(owr_path, VESSEL_NAME, WATER_DEPTH)
    if orcawave_results is None:
        print("[ERROR] Failed to extract OrcaWave results")
        return 1
    n_freq = orcawave_results.raos.surge.frequencies.count
    n_head = orcawave_results.raos.surge.headings.count
    print(f"  OK: {n_freq} frequencies x {n_head} headings")

    print("\n[3/5] Extracting AQWA results...")
    aqwa_results = _extract_from_aqwa_lis(LIS_FILE, VESSEL_NAME, WATER_DEPTH)
    if aqwa_results is None:
        print("[ERROR] Failed to extract AQWA results")
        return 1
    n_freq = aqwa_results.raos.surge.frequencies.count
    n_head = aqwa_results.raos.surge.headings.count
    print(f"  OK: {n_freq} frequencies x {n_head} headings")

    # --- Step 3: Harmonize ---
    print("\n[4/5] Harmonizing headings and frequencies...")
    solver_results = {
        "OrcaWave": orcawave_results,
        "AQWA": aqwa_results,
    }
    solver_results = _harmonize_headings(solver_results)
    solver_results = _harmonize_frequencies(solver_results)

    # --- Step 4: Benchmark ---
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
            "r1: Spar cylinder (D=25m, draft=110m) benchmark. "
            "AQWA (200m depth, 3 freq, 9 headings) vs OrcaWave "
            "(200m depth, 20 freq, 5 headings). "
            "Frequency interpolation used (3 AQWA freq as target grid). "
            "r4 report format."
        ),
        "changes": [
            "Initial spar benchmark generation",
            "AQWA mesh (8,283 panels) imported in both solvers",
            "Frequency harmonization via interpolation (AQWA 3 -> OrcaWave 20)",
            "Phase convention normalization (ISO 6954 -> Orcina)",
        ],
        "source_files": {
            "orcawave_yml": str(ORCAWAVE_YML),
            "orcawave_owr": str(owr_path),
            "aqwa_lis": str(LIS_FILE),
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
    print("\n  Phase correlations:")
    for dof, data in rao_comps.items():
        phase_corr = data.get("phase_correlation", 0)
        mag_corr = data.get("magnitude_correlation", 0)
        status = "OK" if phase_corr > 0 else "NEGATIVE"
        print(f"    {dof:6s}: phase_corr={phase_corr:+.4f}  "
              f"mag_corr={mag_corr:.4f}  [{status}]")

    print(f"\n  Report HTML: {result.report_html_path}")
    print(f"  Report JSON: {result.report_json_path}")
    print(f"  Revision metadata: {meta_path}")
    print(f"  Plots: {len(result.plot_paths)} generated")

    return 0


if __name__ == "__main__":
    sys.exit(main())
