#!/usr/bin/env python3
"""Re-run all 3 diffraction benchmarks with FIDP external damping fix.

Ship benchmark (primary validation):
    - Patches original AQWA .dat to insert FIDP cards in Deck 7
    - Re-runs AQWA v252 on the patched .dat
    - Extracts new AQWA results from .LIS and compares with OrcaWave

Barge benchmark:
    - Re-extracts from existing .owr/.LIS (no damping change)

Spar benchmark:
    - Re-extracts from existing .owr/.LIS (no damping change)

Prints a summary table comparing OLD vs NEW correlations per DOF per hull.

Usage:
    uv run python scripts/benchmark/rerun_benchmarks_with_fidp.py
    uv run python scripts/benchmark/rerun_benchmarks_with_fidp.py --ship-only
    uv run python scripts/benchmark/rerun_benchmarks_with_fidp.py --skip-aqwa
"""
from __future__ import annotations

import json
import shutil
import sys
import time
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Optional

REPO_ROOT = Path(__file__).parent.parent.parent
sys.path.insert(0, str(REPO_ROOT / "src"))
sys.path.insert(0, str(REPO_ROOT))

import yaml

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
from scripts.benchmark.solver_metadata import build_solver_metadata
from digitalmodel.hydrodynamics.diffraction.aqwa_runner import (
    AQWARunConfig,
    AQWARunResult,
    AQWARunner,
    AQWARunStatus,
)

# ---------------------------------------------------------------------------
# Output directories
# ---------------------------------------------------------------------------
OUTPUT_BASE = Path("benchmark_output/wrk132_rerun")
SHIP_OUTPUT = OUTPUT_BASE / "ship_r2"
BARGE_OUTPUT = OUTPUT_BASE / "barge_r5"
SPAR_OUTPUT = OUTPUT_BASE / "spar_r2"

# ---------------------------------------------------------------------------
# Source file paths
# ---------------------------------------------------------------------------

# Ship
SHIP_BENCHMARK_DIR = Path("docs/modules/orcawave/L03_ship_benchmark")
SHIP_SPEC = SHIP_BENCHMARK_DIR / "spec.yml"
SHIP_AQWA_DAT = (
    SHIP_BENCHMARK_DIR / "source_data" / "aqwa" / "aqwa_001_ship_raos_rev2.dat"
)
SHIP_LIS_ORIG = SHIP_BENCHMARK_DIR / "source_data" / "aqwa" / "001_SHIP_RAOS_REV2.LIS"
SHIP_OWR = Path(
    "docs/modules/orcawave/L01_aqwa_benchmark"
    "/orcawave_001_ship_raos_rev2_matched_500m.owr"
)
SHIP_VESSEL_NAME = "Ship_001_RAOs"
SHIP_WATER_DEPTH = 500.0  # confirmed from LIS + spec.yml

# Barge
BARGE_BENCHMARK_DIR = Path("docs/modules/orcawave/L02_barge_benchmark")
BARGE_OWR = Path("benchmark_output/barge_final/orcawave/Barge_Benchmark.owr")
BARGE_LIS = BARGE_BENCHMARK_DIR / "source_data" / "aqwa" / "WRK-031_3WAY_BENCHMARK.LIS"
BARGE_SPEC_FILE = Path("specs/modules/benchmark/barge_benchmark_spec.yml")
BARGE_VESSEL_NAME = "Barge_Benchmark"
BARGE_WATER_DEPTH = 200.0

# Spar
SPAR_BENCHMARK_DIR = Path("docs/modules/orcawave/L04_spar_benchmark")
SPAR_ORCAWAVE_YML = (
    SPAR_BENCHMARK_DIR / "source_data" / "orcawave" / "spar_benchmark.yml"
)
SPAR_OWR = Path("benchmark_output/spar_benchmark/r1_spar_benchmark/spar_benchmark.owr")
SPAR_LIS = Path("docs/modules/aqwa/examples/spar-example/SIMPLE.LIS")
SPAR_VESSEL_NAME = "Spar_Benchmark"
SPAR_WATER_DEPTH = 200.0

# Existing report JSONs (for OLD correlation baseline)
OLD_REPORTS = {
    "ship": SHIP_BENCHMARK_DIR / "benchmark_results" / "benchmark_report.json",
    "barge": BARGE_BENCHMARK_DIR / "benchmark_results" / "benchmark_report.json",
    "spar": SPAR_BENCHMARK_DIR / "benchmark_results" / "benchmark_report.json",
}

DOFS = ["surge", "sway", "heave", "roll", "pitch", "yaw"]


# ---------------------------------------------------------------------------
# Data classes for correlation tracking
# ---------------------------------------------------------------------------


@dataclass
class DofCorrelation:
    """Per-DOF magnitude and phase correlation pair."""

    magnitude: float = 0.0
    phase: float = 0.0


@dataclass
class HullResult:
    """Per-hull benchmark correlation results."""

    name: str
    old: dict[str, DofCorrelation] = field(default_factory=dict)
    new: dict[str, DofCorrelation] = field(default_factory=dict)
    consensus_old: str = ""
    consensus_new: str = ""
    report_html: str = ""
    report_json: str = ""


# ---------------------------------------------------------------------------
# FIDP patching
# ---------------------------------------------------------------------------


def _load_damping_matrix(spec_path: Path) -> list[list[float]]:
    """Load the external_damping 6x6 matrix from spec.yml."""
    with open(spec_path, encoding="utf-8") as f:
        spec = yaml.safe_load(f)
    matrix = spec["vessel"]["external_damping"]
    if len(matrix) != 6 or any(len(row) != 6 for row in matrix):
        raise ValueError(
            f"Expected 6x6 damping matrix, got {len(matrix)} rows"
        )
    return matrix


def _matrix_has_nonzero(matrix: list[list[float]]) -> bool:
    """Return True if the 6x6 matrix contains any non-zero values."""
    return any(val != 0.0 for row in matrix for val in row)


def _build_fidp_cards(matrix: list[list[float]]) -> list[str]:
    """Build AQWA FIDP card lines for a 6x6 damping matrix.

    Format per row::

        <6 spaces>FIDP<5 spaces><row_idx (5-wide)><6 x 10-char values>

    Values use scientific notation with 3 decimal places.
    """
    cards: list[str] = []
    for row_idx, row in enumerate(matrix, start=1):
        val_fields = "".join(f"{f'{v:.3e}':>10s}" for v in row)
        cards.append(
            f"{' ':>1s}{' ':>3s}{' ':>2s}FIDP"
            f"{' ':>5s}{row_idx:>5d}{val_fields}"
        )
    return cards


def _patch_aqwa_dat_with_fidp(
    src_dat: Path,
    dst_dat: Path,
    damping_matrix: list[list[float]],
) -> None:
    """Copy AQWA .dat file and insert FIDP cards after WFS1 in Deck 7.

    The original .dat has::

        WFS1
        END

    After patching::

        WFS1
        FIDP    1  0.000e+00  ...
        FIDP    2  ...
        ...
        FIDP    6  ...
        END

    Preserves latin-1 encoding and CRLF line endings.
    """
    with open(src_dat, "r", encoding="latin-1") as f:
        lines = f.readlines()

    # Find WFS1 line in Deck 7
    wfs1_idx = None
    for i, line in enumerate(lines):
        stripped = line.strip().rstrip("$")
        if stripped.strip() == "WFS1":
            wfs1_idx = i
            break

    if wfs1_idx is None:
        raise ValueError(f"Could not find WFS1 line in {src_dat}")

    # Build FIDP card lines (with CRLF endings to match the file)
    fidp_cards = _build_fidp_cards(damping_matrix)
    fidp_lines = [card + "\r\n" for card in fidp_cards]

    # Insert FIDP cards after WFS1, before END
    patched = lines[: wfs1_idx + 1] + fidp_lines + lines[wfs1_idx + 1 :]

    dst_dat.parent.mkdir(parents=True, exist_ok=True)
    with open(dst_dat, "w", encoding="latin-1", newline="") as f:
        f.writelines(patched)

    print(f"  Patched .dat: {dst_dat}")
    print(f"  Inserted {len(fidp_cards)} FIDP card lines after WFS1")


# ---------------------------------------------------------------------------
# AQWA execution (bypasses prepare(), runs on pre-existing .dat)
# ---------------------------------------------------------------------------


def _run_aqwa_on_patched(
    dat_file: Path,
    mesh_file: Path | None = None,
) -> Optional[Path]:
    """Execute AQWA solver on a patched .dat file.

    Follows the same pattern as run_spar_benchmark.py: manually sets
    ``runner._result`` and calls ``execute()`` directly, bypassing
    ``prepare()`` which would regenerate the .dat from a DiffractionSpec.

    Returns the .LIS file path on success, None on failure.
    """
    dat_dir = dat_file.parent.resolve()
    print(f"  Input:  {dat_file}")
    print(f"  CWD:    {dat_dir}")

    # Copy mesh file alongside the .dat if it's a separate file
    if mesh_file is not None and mesh_file != dat_file:
        mesh_dst = dat_dir / mesh_file.name
        if not mesh_dst.exists():
            print(f"  Copying mesh: {mesh_file} -> {mesh_dst}")
            shutil.copy2(mesh_file, mesh_dst)

    config = AQWARunConfig(
        executable_path=None,  # auto-detect
        output_dir=dat_dir,
        dry_run=False,
        timeout_seconds=3600,
        copy_mesh_files=False,
        nowind=True,
    )
    runner = AQWARunner(config)

    # Manually set up result, bypassing prepare()
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
        return result.lis_file
    elif result.status == AQWARunStatus.DRY_RUN:
        print("  [WARN] AQWA executable not found -- dry run only")
        print("  Set AQWA_PATH environment variable or install ANSYS AQWA")
        return None
    else:
        print(f"  [ERROR] AQWA failed: {result.error_message}")
        if result.stderr:
            print(f"  stderr: {result.stderr[:500]}")
        return None


# ---------------------------------------------------------------------------
# Correlation extraction from report JSON
# ---------------------------------------------------------------------------


def _extract_correlations_from_json(
    report_path: Path,
) -> dict[str, DofCorrelation]:
    """Extract per-DOF correlations from existing benchmark_report.json."""
    with open(report_path, encoding="utf-8") as f:
        data = json.load(f)

    pair = data.get("pairwise_results", {}).get("AQWA-vs-OrcaWave", {})
    rao_comps = pair.get("rao_comparisons", {})

    correlations: dict[str, DofCorrelation] = {}
    for dof in DOFS:
        vals = rao_comps.get(dof, {})
        correlations[dof] = DofCorrelation(
            magnitude=vals.get("magnitude_correlation", float("nan")),
            phase=vals.get("phase_correlation", float("nan")),
        )
    return correlations


def _extract_consensus_from_json(report_path: Path) -> str:
    """Extract overall_consensus from an existing benchmark_report.json."""
    with open(report_path, encoding="utf-8") as f:
        data = json.load(f)
    return data.get("overall_consensus", "UNKNOWN")


def _extract_correlations_from_runner(
    runner: BenchmarkRunner,
    result,
) -> dict[str, DofCorrelation]:
    """Extract per-DOF correlations from a freshly-run BenchmarkRunner."""
    report_data = runner._report_to_dict(result.report)
    pair = report_data.get("pairwise_results", {}).get("AQWA-vs-OrcaWave", {})
    rao_comps = pair.get("rao_comparisons", {})

    correlations: dict[str, DofCorrelation] = {}
    for dof in DOFS:
        vals = rao_comps.get(dof, {})
        correlations[dof] = DofCorrelation(
            magnitude=vals.get("magnitude_correlation", float("nan")),
            phase=vals.get("phase_correlation", float("nan")),
        )
    return correlations


# ---------------------------------------------------------------------------
# Hull-specific benchmark runners
# ---------------------------------------------------------------------------


def _run_ship_benchmark(
    skip_aqwa: bool = False,
) -> Optional[HullResult]:
    """Run the ship benchmark with FIDP-patched AQWA .dat.

    Steps:
        1. Load damping matrix from spec.yml
        2. Copy + patch the AQWA .dat with FIDP cards
        3. Run AQWA v252 on the patched .dat (or use existing .LIS)
        4. Extract AQWA results from new .LIS
        5. Extract OrcaWave results from existing .owr
        6. Harmonize and run benchmark
    """
    print(f"\n{'='*60}")
    print("  SHIP BENCHMARK (with FIDP external damping)")
    print(f"{'='*60}")

    hull = HullResult(name="ship")

    # Load OLD correlations
    if OLD_REPORTS["ship"].exists():
        hull.old = _extract_correlations_from_json(OLD_REPORTS["ship"])
        hull.consensus_old = _extract_consensus_from_json(OLD_REPORTS["ship"])

    # Step 1: Load damping matrix
    if not SHIP_SPEC.exists():
        print(f"[ERROR] Ship spec not found: {SHIP_SPEC}")
        return None
    damping_matrix = _load_damping_matrix(SHIP_SPEC)
    has_damping = _matrix_has_nonzero(damping_matrix)
    print(f"  Damping matrix loaded from spec.yml (non-zero: {has_damping})")
    if has_damping:
        # Print non-zero entries
        for i, row in enumerate(damping_matrix):
            for j, val in enumerate(row):
                if val != 0.0:
                    print(f"    M{i+1}{j+1} = {val:.1f}")

    # Step 2: Working directory and patching
    work_dir = SHIP_OUTPUT / "aqwa_run"
    work_dir.mkdir(parents=True, exist_ok=True)
    patched_dat = work_dir / SHIP_AQWA_DAT.name

    if not SHIP_AQWA_DAT.exists():
        print(f"[ERROR] Ship AQWA .dat not found: {SHIP_AQWA_DAT}")
        return None

    print("\n[1/6] Patching AQWA .dat with FIDP cards...")
    _patch_aqwa_dat_with_fidp(SHIP_AQWA_DAT, patched_dat, damping_matrix)

    # Verify the patch
    _verify_fidp_in_dat(patched_dat)

    # Step 3: Run AQWA or use existing LIS
    lis_file: Optional[Path] = None
    if not skip_aqwa:
        print("\n[2/6] Running AQWA v252 on patched .dat...")
        # The ship .dat is self-contained (mesh is in the same file)
        lis_file = _run_aqwa_on_patched(patched_dat, mesh_file=None)
        if lis_file is None:
            # Fall back to working dir LIS if AQWA not installed
            fallback = work_dir / (patched_dat.stem.upper() + ".LIS")
            if fallback.exists():
                print(f"  Using fallback LIS: {fallback}")
                lis_file = fallback
            else:
                print("[ERROR] AQWA failed and no fallback .LIS found")
                print("  Run with --skip-aqwa to use existing .LIS")
                return None
    else:
        print("\n[2/6] Skipping AQWA run (--skip-aqwa)")
        # Look for LIS in work dir first, then original location
        candidate = work_dir / (patched_dat.stem.upper() + ".LIS")
        if candidate.exists():
            lis_file = candidate
        elif SHIP_LIS_ORIG.exists():
            print(f"  [WARN] Using original (no FIDP) LIS: {SHIP_LIS_ORIG}")
            lis_file = SHIP_LIS_ORIG
        else:
            print("[ERROR] No .LIS file found")
            return None

    print(f"  Using LIS: {lis_file}")

    # Step 4: Extract AQWA results
    print("\n[3/6] Extracting AQWA results from .LIS...")
    aqwa_results = _extract_from_aqwa_lis(lis_file, SHIP_VESSEL_NAME, SHIP_WATER_DEPTH)
    if aqwa_results is None:
        print("[ERROR] Failed to extract AQWA results")
        return None
    n_freq = aqwa_results.raos.surge.frequencies.count
    n_head = aqwa_results.raos.surge.headings.count
    print(f"  OK: {n_freq} frequencies x {n_head} headings")

    # Step 5: Extract OrcaWave results
    print("\n[4/6] Extracting OrcaWave results from .owr...")
    if not SHIP_OWR.exists():
        print(f"[ERROR] Ship .owr not found: {SHIP_OWR}")
        return None
    orcawave_results = _extract_from_owr(
        SHIP_OWR, SHIP_VESSEL_NAME, SHIP_WATER_DEPTH
    )
    if orcawave_results is None:
        print("[ERROR] Failed to extract OrcaWave results")
        return None
    n_freq = orcawave_results.raos.surge.frequencies.count
    n_head = orcawave_results.raos.surge.headings.count
    print(f"  OK: {n_freq} frequencies x {n_head} headings")

    # Step 6: Harmonize and benchmark
    print("\n[5/6] Harmonizing headings and frequencies...")
    solver_results = {
        "OrcaWave": orcawave_results,
        "AQWA": aqwa_results,
    }
    solver_results = _harmonize_headings(solver_results)
    solver_results = _harmonize_frequencies(solver_results)

    print("\n[6/6] Running benchmark comparison...")
    with open(SHIP_SPEC, encoding="utf-8") as f:
        ship_spec = yaml.safe_load(f)
    solver_metadata = build_solver_metadata(
        ship_spec,
        aqwa_input_file=str(lis_file),
        panel_count="8,615 (6,754 diffracting + 2,631 lid)",
        spec_dir=SHIP_BENCHMARK_DIR,
    )
    config = BenchmarkConfig(
        output_dir=SHIP_OUTPUT,
        tolerance=0.05,
        x_axis="period",
    )
    runner = BenchmarkRunner(config)
    result = runner.run_from_results(solver_results, solver_metadata=solver_metadata)

    if not result.success:
        print(f"[ERROR] Benchmark failed: {result.error_message}")
        return None

    hull.consensus_new = result.report.overall_consensus
    hull.new = _extract_correlations_from_runner(runner, result)
    hull.report_html = str(result.report_html_path)
    hull.report_json = str(result.report_json_path)

    _print_hull_summary("ship", hull)
    return hull


def _verify_fidp_in_dat(dat_path: Path) -> None:
    """Print verification of FIDP cards in the patched .dat file."""
    with open(dat_path, "r", encoding="latin-1") as f:
        lines = f.readlines()

    fidp_count = 0
    in_deck7 = False
    for line in lines:
        stripped = line.strip()
        if "DECK  7" in stripped:
            in_deck7 = True
        elif "DECK  8" in stripped:
            in_deck7 = False
        if in_deck7 and "FIDP" in stripped:
            fidp_count += 1
            # Print first and last FIDP line for verification
            if fidp_count == 1:
                print(f"  First FIDP card: {stripped[:80]}")
            elif fidp_count == 6:
                print(f"  Last  FIDP card: {stripped[:80]}")

    print(f"  Total FIDP cards in Deck 7: {fidp_count}")
    if fidp_count != 6:
        print(f"  [WARN] Expected 6 FIDP cards, found {fidp_count}")


def _run_barge_benchmark() -> Optional[HullResult]:
    """Re-run barge benchmark from existing solver output (no damping change)."""
    print(f"\n{'='*60}")
    print("  BARGE BENCHMARK (no damping -- should be unchanged)")
    print(f"{'='*60}")

    hull = HullResult(name="barge")

    # Load OLD correlations
    if OLD_REPORTS["barge"].exists():
        hull.old = _extract_correlations_from_json(OLD_REPORTS["barge"])
        hull.consensus_old = _extract_consensus_from_json(OLD_REPORTS["barge"])

    # Validate source files
    for label, path in [("OWR", BARGE_OWR), ("LIS", BARGE_LIS)]:
        if not path.exists():
            print(f"[ERROR] Barge {label} not found: {path}")
            return None

    # Load spec for metadata
    if not BARGE_SPEC_FILE.exists():
        print(f"[ERROR] Barge spec not found: {BARGE_SPEC_FILE}")
        return None
    with open(BARGE_SPEC_FILE, encoding="utf-8") as f:
        spec = yaml.safe_load(f)
    solver_metadata = build_solver_metadata(
        spec,
        aqwa_input_file=str(BARGE_LIS),
        panel_count="912",
        spec_dir=BARGE_SPEC_FILE.parent,
    )

    # Extract OrcaWave
    print("\n[1/4] Extracting OrcaWave results...")
    orcawave_results = _extract_from_owr(BARGE_OWR, BARGE_VESSEL_NAME, BARGE_WATER_DEPTH)
    if orcawave_results is None:
        print("[ERROR] Failed to extract OrcaWave results")
        return None
    n_freq = orcawave_results.raos.surge.frequencies.count
    n_head = orcawave_results.raos.surge.headings.count
    print(f"  OK: {n_freq} frequencies x {n_head} headings")

    # Extract AQWA
    print("\n[2/4] Extracting AQWA results...")
    aqwa_results = _extract_from_aqwa_lis(BARGE_LIS, BARGE_VESSEL_NAME, BARGE_WATER_DEPTH)
    if aqwa_results is None:
        print("[ERROR] Failed to extract AQWA results")
        return None
    n_freq = aqwa_results.raos.surge.frequencies.count
    n_head = aqwa_results.raos.surge.headings.count
    print(f"  OK: {n_freq} frequencies x {n_head} headings")

    # Harmonize
    print("\n[3/4] Harmonizing headings...")
    solver_results = {
        "OrcaWave": orcawave_results,
        "AQWA": aqwa_results,
    }
    solver_results = _harmonize_headings(solver_results)

    # Benchmark
    print("\n[4/4] Running benchmark comparison...")
    BARGE_OUTPUT.mkdir(parents=True, exist_ok=True)
    config = BenchmarkConfig(
        output_dir=BARGE_OUTPUT,
        tolerance=0.05,
        x_axis="period",
    )
    runner = BenchmarkRunner(config)
    result = runner.run_from_results(solver_results, solver_metadata=solver_metadata)

    if not result.success:
        print(f"[ERROR] Benchmark failed: {result.error_message}")
        return None

    hull.consensus_new = result.report.overall_consensus
    hull.new = _extract_correlations_from_runner(runner, result)
    hull.report_html = str(result.report_html_path)
    hull.report_json = str(result.report_json_path)

    _print_hull_summary("barge", hull)
    return hull


def _run_spar_benchmark() -> Optional[HullResult]:
    """Re-run spar benchmark from existing solver output (no damping change)."""
    print(f"\n{'='*60}")
    print("  SPAR BENCHMARK (no damping -- should be unchanged)")
    print(f"{'='*60}")

    hull = HullResult(name="spar")

    # Load OLD correlations
    if OLD_REPORTS["spar"].exists():
        hull.old = _extract_correlations_from_json(OLD_REPORTS["spar"])
        hull.consensus_old = _extract_consensus_from_json(OLD_REPORTS["spar"])

    # Validate source files
    if not SPAR_OWR.exists():
        print(f"[ERROR] Spar .owr not found: {SPAR_OWR}")
        return None
    if not SPAR_LIS.exists():
        print(f"[ERROR] Spar .LIS not found: {SPAR_LIS}")
        return None

    # Extract OrcaWave
    print("\n[1/4] Extracting OrcaWave results...")
    orcawave_results = _extract_from_owr(SPAR_OWR, SPAR_VESSEL_NAME, SPAR_WATER_DEPTH)
    if orcawave_results is None:
        print("[ERROR] Failed to extract OrcaWave results")
        return None
    n_freq = orcawave_results.raos.surge.frequencies.count
    n_head = orcawave_results.raos.surge.headings.count
    print(f"  OK: {n_freq} frequencies x {n_head} headings")

    # Extract AQWA
    print("\n[2/4] Extracting AQWA results...")
    aqwa_results = _extract_from_aqwa_lis(SPAR_LIS, SPAR_VESSEL_NAME, SPAR_WATER_DEPTH)
    if aqwa_results is None:
        print("[ERROR] Failed to extract AQWA results")
        return None
    n_freq = aqwa_results.raos.surge.frequencies.count
    n_head = aqwa_results.raos.surge.headings.count
    print(f"  OK: {n_freq} frequencies x {n_head} headings")

    # Harmonize
    print("\n[3/4] Harmonizing headings and frequencies...")
    solver_results = {
        "OrcaWave": orcawave_results,
        "AQWA": aqwa_results,
    }
    solver_results = _harmonize_headings(solver_results)
    solver_results = _harmonize_frequencies(solver_results)

    # Benchmark
    print("\n[4/4] Running benchmark comparison...")
    spar_spec_path = SPAR_BENCHMARK_DIR / "spec.yml"
    with open(spar_spec_path, encoding="utf-8") as f:
        spar_spec = yaml.safe_load(f)
    solver_metadata = build_solver_metadata(
        spar_spec,
        aqwa_input_file=str(SPAR_LIS),
        panel_count="~8,283",
        spec_dir=SPAR_BENCHMARK_DIR,
    )
    SPAR_OUTPUT.mkdir(parents=True, exist_ok=True)
    config = BenchmarkConfig(
        output_dir=SPAR_OUTPUT,
        tolerance=0.05,
        x_axis="period",
    )
    runner = BenchmarkRunner(config)
    result = runner.run_from_results(solver_results, solver_metadata=solver_metadata)

    if not result.success:
        print(f"[ERROR] Benchmark failed: {result.error_message}")
        return None

    hull.consensus_new = result.report.overall_consensus
    hull.new = _extract_correlations_from_runner(runner, result)
    hull.report_html = str(result.report_html_path)
    hull.report_json = str(result.report_json_path)

    _print_hull_summary("spar", hull)
    return hull


# ---------------------------------------------------------------------------
# Summary printing
# ---------------------------------------------------------------------------


def _print_hull_summary(name: str, hull: HullResult) -> None:
    """Print per-hull correlation summary."""
    print(f"\n  {'#'*50}")
    print(f"  # {name.upper()} Results")
    print(f"  {'#'*50}")
    print(f"  Consensus: {hull.consensus_old} -> {hull.consensus_new}")
    print(f"\n  {'DOF':>6s}  {'mag_old':>8s}  {'mag_new':>8s}  "
          f"{'phase_old':>10s}  {'phase_new':>10s}  {'delta_ph':>8s}")
    print(f"  {'-'*6}  {'-'*8}  {'-'*8}  {'-'*10}  {'-'*10}  {'-'*8}")

    for dof in DOFS:
        old_c = hull.old.get(dof, DofCorrelation())
        new_c = hull.new.get(dof, DofCorrelation())
        delta_ph = new_c.phase - old_c.phase
        delta_str = f"{delta_ph:+.4f}" if not _isnan(delta_ph) else "  n/a"
        print(
            f"  {dof:>6s}  "
            f"{_fmt_corr(old_c.magnitude):>8s}  "
            f"{_fmt_corr(new_c.magnitude):>8s}  "
            f"{_fmt_corr(old_c.phase):>10s}  "
            f"{_fmt_corr(new_c.phase):>10s}  "
            f"{delta_str:>8s}"
        )

    if hull.report_html:
        print(f"\n  Report HTML: {hull.report_html}")
    if hull.report_json:
        print(f"  Report JSON: {hull.report_json}")


def _print_combined_summary(results: dict[str, HullResult]) -> None:
    """Print the final combined OLD vs NEW comparison table."""
    print(f"\n{'#'*72}")
    print("# COMBINED SUMMARY: OLD vs NEW Correlations (FIDP fix)")
    print(f"{'#'*72}")

    # Header
    header = f"{'Hull':>6s}  {'DOF':>6s}  "
    header += f"{'mag_old':>8s}  {'mag_new':>8s}  "
    header += f"{'ph_old':>8s}  {'ph_new':>8s}  {'delta_ph':>8s}  {'status':>8s}"
    print(f"\n{header}")
    print(f"{'-'*80}")

    for hull_name in ["ship", "barge", "spar"]:
        hull = results.get(hull_name)
        if hull is None:
            print(f"  {hull_name:>6s}  {'SKIPPED / FAILED':>50s}")
            continue

        for i, dof in enumerate(DOFS):
            old_c = hull.old.get(dof, DofCorrelation())
            new_c = hull.new.get(dof, DofCorrelation())
            delta_ph = new_c.phase - old_c.phase

            # Status: improved, regressed, unchanged, or n/a
            if _isnan(delta_ph):
                status = "n/a"
            elif abs(delta_ph) < 0.001:
                status = "same"
            elif delta_ph > 0:
                status = "BETTER"
            else:
                status = "worse"

            name_col = hull_name if i == 0 else ""
            print(
                f"  {name_col:>6s}  {dof:>6s}  "
                f"{_fmt_corr(old_c.magnitude):>8s}  "
                f"{_fmt_corr(new_c.magnitude):>8s}  "
                f"{_fmt_corr(old_c.phase):>8s}  "
                f"{_fmt_corr(new_c.phase):>8s}  "
                f"{_fmt_delta(delta_ph):>8s}  "
                f"{status:>8s}"
            )

        # Print consensus change
        print(
            f"  {'':>6s}  {'':>6s}  "
            f"{'Consensus:':>17s}  "
            f"{hull.consensus_old:>10s} -> {hull.consensus_new}"
        )
        print(f"  {'-'*78}")

    # Summary counts
    improved = 0
    regressed = 0
    unchanged = 0
    for hull in results.values():
        if hull is None:
            continue
        for dof in DOFS:
            old_c = hull.old.get(dof, DofCorrelation())
            new_c = hull.new.get(dof, DofCorrelation())
            delta = new_c.phase - old_c.phase
            if _isnan(delta):
                continue
            if abs(delta) < 0.001:
                unchanged += 1
            elif delta > 0:
                improved += 1
            else:
                regressed += 1

    total = improved + regressed + unchanged
    print(f"\n  Phase correlation changes: "
          f"{improved} improved, {regressed} regressed, "
          f"{unchanged} unchanged (of {total} DOF-hull pairs)")


def _fmt_corr(val: float) -> str:
    """Format a correlation value, handling NaN."""
    if _isnan(val):
        return "nan"
    return f"{val:+.4f}"


def _fmt_delta(val: float) -> str:
    """Format a delta value, handling NaN."""
    if _isnan(val):
        return "n/a"
    return f"{val:+.4f}"


def _isnan(val: float) -> bool:
    """Check for NaN (works without numpy)."""
    return val != val


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main() -> int:
    ship_only = "--ship-only" in sys.argv
    skip_aqwa = "--skip-aqwa" in sys.argv

    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    print(f"{'#'*72}")
    print("# WRK-132: Re-run Diffraction Benchmarks with FIDP Fix")
    print(f"# Timestamp: {timestamp}")
    print(f"# Output:    {OUTPUT_BASE}")
    if ship_only:
        print("# Mode:      --ship-only")
    if skip_aqwa:
        print("# Mode:      --skip-aqwa (using existing .LIS for ship)")
    print(f"{'#'*72}")

    OUTPUT_BASE.mkdir(parents=True, exist_ok=True)

    results: dict[str, HullResult] = {}

    # --- Ship benchmark (primary: FIDP validation) ---
    ship_result = _run_ship_benchmark(skip_aqwa=skip_aqwa)
    if ship_result is not None:
        results["ship"] = ship_result

    if not ship_only:
        # --- Barge benchmark (no change expected) ---
        barge_result = _run_barge_benchmark()
        if barge_result is not None:
            results["barge"] = barge_result

        # --- Spar benchmark (no change expected) ---
        spar_result = _run_spar_benchmark()
        if spar_result is not None:
            results["spar"] = spar_result

    # --- Combined summary table ---
    if results:
        _print_combined_summary(results)

    # --- Write summary JSON ---
    summary = {
        "timestamp": timestamp,
        "description": (
            "WRK-132: Re-run diffraction benchmarks with FIDP external "
            "damping fix. Ship benchmark patched with FIDP M44=36,010 "
            "roll damping. Barge and spar unchanged (no external damping)."
        ),
        "hulls": {},
    }
    for hull_name, hull in results.items():
        hull_data = {
            "consensus_old": hull.consensus_old,
            "consensus_new": hull.consensus_new,
            "report_html": hull.report_html,
            "report_json": hull.report_json,
            "correlations": {},
        }
        for dof in DOFS:
            old_c = hull.old.get(dof, DofCorrelation())
            new_c = hull.new.get(dof, DofCorrelation())
            hull_data["correlations"][dof] = {
                "magnitude_old": old_c.magnitude,
                "magnitude_new": new_c.magnitude,
                "phase_old": old_c.phase,
                "phase_new": new_c.phase,
                "phase_delta": new_c.phase - old_c.phase,
            }
        summary["hulls"][hull_name] = hull_data

    summary_path = OUTPUT_BASE / "fidp_benchmark_summary.json"
    with open(summary_path, "w", encoding="utf-8") as f:
        json.dump(summary, f, indent=2)
    print(f"\n  Summary JSON: {summary_path}")

    # Return 0 if ship benchmark succeeded (primary validation)
    if "ship" in results:
        print("\n  Ship benchmark completed successfully.")
        return 0
    else:
        print("\n  [ERROR] Ship benchmark failed.")
        return 1


if __name__ == "__main__":
    sys.exit(main())
