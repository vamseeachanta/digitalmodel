#!/usr/bin/env python3
"""
3-Way Solver Benchmark Runner

Orchestrates diffraction analysis across AQWA, OrcaWave, and BEMRosetta,
then runs comparison using the benchmark framework.

Usage:
    uv run python scripts/benchmark/run_3way_benchmark.py specs/modules/benchmark/unit_box_spec.yml
    uv run python scripts/benchmark/run_3way_benchmark.py spec.yml --solvers orcawave,aqwa
    uv run python scripts/benchmark/run_3way_benchmark.py spec.yml --dry-run

WRK-099: Run 3-way benchmark on Unit Box hull
"""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
import time
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional

import yaml

import numpy as np

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from digitalmodel.hydrodynamics.diffraction.benchmark_runner import (
    BenchmarkConfig,
    BenchmarkRunner,
    BenchmarkRunResult,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    AddedMassSet,
    DampingSet,
    DiffractionResults,
    DOF,
    FrequencyData,
    HeadingData,
    HydrodynamicMatrix,
    RAOComponent,
    RAOSet,
)


# ---------------------------------------------------------------------------
# Solver Configuration
# ---------------------------------------------------------------------------

SOLVER_PATHS = {
    "orcawave": None,  # Uses OrcFxAPI Python binding
    "aqwa": Path("C:/Program Files/ANSYS Inc/v252/aqwa/bin/winx64/Aqwa.exe"),
    "bemrosetta": Path("D:/software/BEMRosetta/BEMRosetta_cl.exe"),
}


@dataclass
class SolverRunResult:
    """Result of running a single solver."""

    solver_name: str
    status: str  # "completed", "failed", "skipped", "dry_run"
    duration_seconds: float = 0.0
    output_dir: Optional[Path] = None
    results: Optional[DiffractionResults] = None
    error_message: Optional[str] = None
    log_file: Optional[Path] = None


@dataclass
class BenchmarkOrchestrationResult:
    """Result of the full benchmark orchestration."""

    spec_path: Path
    solvers_requested: List[str]
    solver_results: Dict[str, SolverRunResult] = field(default_factory=dict)
    benchmark_result: Optional[BenchmarkRunResult] = None
    total_duration_seconds: float = 0.0
    success: bool = False
    error_message: Optional[str] = None


# ---------------------------------------------------------------------------
# Solver Runners
# ---------------------------------------------------------------------------


def run_orcawave(
    spec_path: Path,
    output_dir: Path,
    dry_run: bool = False,
) -> SolverRunResult:
    """Run OrcaWave diffraction analysis."""
    print(f"\n{'='*60}")
    print("Running OrcaWave...")
    print(f"{'='*60}")

    start_time = time.time()
    result = SolverRunResult(solver_name="OrcaWave", status="pending")

    try:
        from digitalmodel.hydrodynamics.diffraction.input_schemas import (
            DiffractionSpec,
        )
        from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
            OrcaWaveRunner,
            RunConfig,
            RunStatus,
        )

        # Load spec
        print(f"  Loading spec from: {spec_path}")
        spec = DiffractionSpec.from_yaml(spec_path)
        print(f"  Spec loaded successfully")

        # Configure runner
        orcawave_output = output_dir / "orcawave"
        config = RunConfig(
            output_dir=orcawave_output,
            dry_run=dry_run,
            timeout_seconds=3600,
        )

        # Run (pass spec_path so mesh files can be resolved and copied)
        print(f"  Creating OrcaWave runner...")
        runner = OrcaWaveRunner(config)
        print(f"  Running OrcaWave analysis...")
        run_result = runner.run(spec, spec_path=spec_path)

        result.output_dir = orcawave_output
        result.log_file = orcawave_output / "orcawave.log"

        # Debug: print actual status received
        print(f"  OrcaWave run_result.status = {run_result.status} (type: {type(run_result.status).__name__})")

        if dry_run:
            result.status = "dry_run"
            print("  [DRY RUN] OrcaWave files generated, solver not executed")
        elif run_result.status == RunStatus.COMPLETED:
            result.status = "completed"
            # Extract results from .owr file
            owr_files = sorted(orcawave_output.glob("*.owr"))
            if owr_files:
                print(f"  Extracting results from: {owr_files[0].name}")
                # Get vessel name and water depth from spec
                vessel_name = spec.get_bodies()[0].vessel.name
                water_depth = spec.environment.water_depth if spec.environment else 100.0
                result.results = _extract_from_owr(
                    owr_files[0], vessel_name, water_depth,
                )
                if result.results:
                    n_freq = result.results.raos.surge.frequencies.count
                    n_head = result.results.raos.surge.headings.count
                    print(f"  [OK] Extracted {n_freq} frequencies x {n_head} headings")
                else:
                    print(f"  [WARNING] Could not extract results from .owr")
            else:
                print(f"  [WARNING] No .owr file found in {orcawave_output}")
            print(f"  [OK] OrcaWave completed")
        elif run_result.status == RunStatus.DRY_RUN:
            # OrcaWave runner fell back to dry-run (executable not found)
            result.status = "dry_run"
            if run_result.error_message:
                print(f"  [DRY RUN] {run_result.error_message}")
            else:
                print(f"  [DRY RUN] OrcaWave executable not found, files generated only")
            # Check mesh file status
            if run_result.mesh_files:
                print(f"  Mesh files copied: {[f.name for f in run_result.mesh_files]}")
            else:
                print(f"  WARNING: No mesh files were copied")
        else:
            result.status = "failed"
            # Build a meaningful error message
            error_parts = []
            if run_result.error_message:
                error_parts.append(run_result.error_message)
            if run_result.stderr:
                error_parts.append(f"stderr: {run_result.stderr[:200]}")
            if run_result.return_code is not None and run_result.return_code != 0:
                error_parts.append(f"exit code: {run_result.return_code}")
            if not error_parts:
                error_parts.append(f"Unknown error (status: {run_result.status.value})")
            result.error_message = "; ".join(error_parts)
            print(f"  [FAILED] {result.error_message}")

    except ImportError as e:
        result.status = "skipped"
        result.error_message = f"OrcFxAPI not available: {e}"
        print(f"  [SKIPPED] {result.error_message}")
    except Exception as e:
        import traceback
        result.status = "failed"
        result.error_message = f"{type(e).__name__}: {e}"
        print(f"  [FAILED] {result.error_message}")
        print(f"  Traceback:\n{traceback.format_exc()}")

    result.duration_seconds = time.time() - start_time
    print(f"  Duration: {result.duration_seconds:.1f}s")
    return result


def run_aqwa(
    spec_path: Path,
    output_dir: Path,
    dry_run: bool = False,
) -> SolverRunResult:
    """Run AQWA diffraction analysis."""
    print(f"\n{'='*60}")
    print("Running AQWA...")
    print(f"{'='*60}")

    start_time = time.time()
    result = SolverRunResult(solver_name="AQWA", status="pending")

    try:
        from digitalmodel.hydrodynamics.diffraction.aqwa_runner import (
            AQWARunner,
            AQWARunConfig,
        )
        from digitalmodel.hydrodynamics.diffraction.input_schemas import (
            DiffractionSpec,
        )

        # Load spec
        spec = DiffractionSpec.from_yaml(spec_path)

        # Configure runner
        aqwa_output = output_dir / "aqwa"
        config = AQWARunConfig(
            executable_path=SOLVER_PATHS["aqwa"],
            output_dir=aqwa_output,
            dry_run=dry_run,
            timeout_seconds=3600,
        )

        # Run (pass spec_path so mesh files can be resolved and copied)
        runner = AQWARunner(config)
        run_result = runner.run(spec, spec_path=spec_path)

        result.output_dir = aqwa_output
        result.log_file = aqwa_output / "aqwa.log"

        if dry_run:
            result.status = "dry_run"
            print("  [DRY RUN] AQWA files generated, solver not executed")
        elif run_result.status.value == "completed":
            result.status = "completed"
            # Extract results from .LIS file
            lis_files = sorted(aqwa_output.glob("*.LIS")) + sorted(aqwa_output.glob("*.lis"))
            if lis_files:
                print(f"  Extracting results from: {lis_files[0].name}")
                vessel_name = spec.get_bodies()[0].vessel.name
                wd = spec.environment.water_depth if spec.environment else 100.0
                result.results = _extract_from_aqwa_lis(
                    lis_files[0], vessel_name, wd,
                )
                if result.results:
                    n_freq = result.results.raos.surge.frequencies.count
                    n_head = result.results.raos.surge.headings.count
                    print(f"  [OK] Extracted {n_freq} frequencies x {n_head} headings")
                else:
                    print("  [WARNING] Could not extract results from .LIS")
            else:
                # Fall back to JSON results if available
                results_file = aqwa_output / "results" / "diffraction_results.json"
                if results_file.exists():
                    result.results = _load_results(results_file, "AQWA")
                else:
                    print(f"  [WARNING] No .LIS file found in {aqwa_output}")
            print(f"  [OK] AQWA completed")
        else:
            result.status = "failed"
            result.error_message = run_result.error_message
            print(f"  [FAILED] {run_result.error_message}")

    except Exception as e:
        result.status = "failed"
        result.error_message = str(e)
        print(f"  [FAILED] {e}")

    result.duration_seconds = time.time() - start_time
    print(f"  Duration: {result.duration_seconds:.1f}s")
    return result


def run_bemrosetta(
    spec_path: Path,
    output_dir: Path,
    dry_run: bool = False,
) -> SolverRunResult:
    """Run BEMRosetta (NEMOH backend) diffraction analysis."""
    print(f"\n{'='*60}")
    print("Running BEMRosetta...")
    print(f"{'='*60}")

    start_time = time.time()
    result = SolverRunResult(solver_name="BEMRosetta", status="pending")

    bemrosetta_exe = SOLVER_PATHS["bemrosetta"]
    if not bemrosetta_exe or not bemrosetta_exe.exists():
        result.status = "skipped"
        result.error_message = "BEMRosetta executable not found"
        print(f"  [SKIPPED] {result.error_message}")
        return result

    try:
        # Load spec
        with open(spec_path) as f:
            spec = yaml.safe_load(f)

        # Create output directory
        bemrosetta_output = output_dir / "bemrosetta"
        bemrosetta_output.mkdir(parents=True, exist_ok=True)
        result.output_dir = bemrosetta_output

        # Get mesh file path (resolve relative to spec)
        mesh_rel = spec["vessel"]["geometry"]["mesh_file"]
        mesh_path = (spec_path.parent / mesh_rel).resolve()

        if not mesh_path.exists():
            raise FileNotFoundError(f"Mesh file not found: {mesh_path}")

        # Build BEMRosetta command using -mesh mode to convert GDF to NEMOH
        # Use absolute path for output file
        nemoh_mesh_out = (bemrosetta_output / "mesh_nemoh.dat").resolve()
        cg = spec["vessel"]["inertia"]["centre_of_gravity"]
        cmd = [
            str(bemrosetta_exe),
            "-mesh",
            "-i", str(mesh_path),
            "-cg", str(cg[0]), str(cg[1]), str(cg[2]),
            "-c", str(nemoh_mesh_out),
        ]

        result.log_file = bemrosetta_output / "bemrosetta.log"

        if dry_run:
            result.status = "dry_run"
            print(f"  [DRY RUN] Would execute: {' '.join(cmd[:6])}...")
            with open(result.log_file, "w") as f:
                f.write(f"DRY RUN - Command: {' '.join(cmd)}\n")
        else:
            print(f"  Executing BEMRosetta...")
            with open(result.log_file, "w") as log_f:
                proc = subprocess.run(
                    cmd,
                    capture_output=True,
                    text=True,
                    timeout=3600,
                    cwd=str(bemrosetta_output),
                )
                log_f.write(f"Command: {' '.join(cmd)}\n\n")
                log_f.write(f"STDOUT:\n{proc.stdout}\n\n")
                log_f.write(f"STDERR:\n{proc.stderr}\n\n")
                log_f.write(f"Return code: {proc.returncode}\n")

            if proc.returncode == 0:
                result.status = "completed"
                print(f"  [OK] BEMRosetta case generated")
                # Note: Actual NEMOH execution would be a separate step
            else:
                result.status = "failed"
                result.error_message = proc.stderr[:500] if proc.stderr else "Unknown error"
                print(f"  [FAILED] Return code {proc.returncode}")

    except subprocess.TimeoutExpired:
        result.status = "failed"
        result.error_message = "Timeout after 3600s"
        print(f"  [FAILED] {result.error_message}")
    except Exception as e:
        result.status = "failed"
        result.error_message = str(e)
        print(f"  [FAILED] {e}")

    result.duration_seconds = time.time() - start_time
    print(f"  Duration: {result.duration_seconds:.1f}s")
    return result


def _load_results(results_file: Path, solver_name: str) -> Optional[DiffractionResults]:
    """Load DiffractionResults from JSON file."""
    try:
        with open(results_file) as f:
            data = json.load(f)
        # TODO: Implement proper deserialization
        return None
    except Exception:
        return None


def _extract_from_owr(
    owr_path: Path,
    vessel_name: str,
    water_depth: float,
) -> Optional[DiffractionResults]:
    """Extract DiffractionResults from an OrcaWave .owr results file."""
    try:
        import OrcFxAPI
        from digitalmodel.hydrodynamics.diffraction.output_schemas import (
            AddedMassSet,
            DampingSet,
            DOF,
            FrequencyData,
            HeadingData,
            HydrodynamicMatrix,
            RAOComponent,
            RAOSet,
        )
    except ImportError:
        print("  [WARNING] OrcFxAPI or output schemas not available for extraction")
        return None

    try:
        diffraction = OrcFxAPI.Diffraction()
        diffraction.LoadResults(str(owr_path.resolve()))

        frequencies = np.array(diffraction.frequencies)
        headings = np.array(diffraction.headings)

        freq_data = FrequencyData(
            values=frequencies,
            periods=2.0 * np.pi / frequencies,
            count=len(frequencies),
            min_freq=0.0,
            max_freq=0.0,
        )
        head_data = HeadingData(
            values=headings,
            count=len(headings),
            min_heading=0.0,
            max_heading=0.0,
        )

        # Extract displacement RAOs - OrcFxAPI returns (nheading, nfreq, 6)
        # Transpose to (nfreq, nheading, 6) for consistency with other solvers
        raw_raos = np.array(diffraction.displacementRAOs)
        raw_raos = np.transpose(raw_raos, (1, 0, 2))  # (nfreq, nheading, 6)

        dof_list = [DOF.SURGE, DOF.SWAY, DOF.HEAVE, DOF.ROLL, DOF.PITCH, DOF.YAW]
        components = {}
        for i, dof in enumerate(dof_list):
            rao_complex = raw_raos[:, :, i]
            components[dof.name.lower()] = RAOComponent(
                dof=dof,
                magnitude=np.abs(rao_complex),
                phase=np.degrees(np.angle(rao_complex)),
                frequencies=freq_data,
                headings=head_data,
                unit="",
            )

        now_str = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        rao_set = RAOSet(
            vessel_name=vessel_name,
            analysis_tool="OrcaWave",
            water_depth=water_depth,
            surge=components["surge"],
            sway=components["sway"],
            heave=components["heave"],
            roll=components["roll"],
            pitch=components["pitch"],
            yaw=components["yaw"],
            created_date=now_str,
            source_file=str(owr_path),
        )

        # Extract added mass - shape: (nfreq, 6, 6)
        am_raw = np.array(diffraction.addedMass)
        am_matrices = []
        for j, freq in enumerate(frequencies):
            am_matrices.append(
                HydrodynamicMatrix(
                    matrix=am_raw[j],
                    frequency=float(freq),
                    matrix_type="added_mass",
                    units={"coupling": "kg"},
                )
            )

        am_set = AddedMassSet(
            vessel_name=vessel_name,
            analysis_tool="OrcaWave",
            water_depth=water_depth,
            matrices=am_matrices,
            frequencies=freq_data,
            created_date=now_str,
            source_file=str(owr_path),
        )

        # Extract damping - shape: (nfreq, 6, 6)
        damp_raw = np.array(diffraction.damping)
        damp_matrices = []
        for j, freq in enumerate(frequencies):
            damp_matrices.append(
                HydrodynamicMatrix(
                    matrix=damp_raw[j],
                    frequency=float(freq),
                    matrix_type="damping",
                    units={"coupling": "N.s/m"},
                )
            )

        damp_set = DampingSet(
            vessel_name=vessel_name,
            analysis_tool="OrcaWave",
            water_depth=water_depth,
            matrices=damp_matrices,
            frequencies=freq_data,
            created_date=now_str,
            source_file=str(owr_path),
        )

        return DiffractionResults(
            vessel_name=vessel_name,
            analysis_tool="OrcaWave",
            water_depth=water_depth,
            raos=rao_set,
            added_mass=am_set,
            damping=damp_set,
            created_date=now_str,
            source_files=[str(owr_path)],
        )

    except Exception as e:
        print(f"  [WARNING] Failed to extract from .owr: {e}")
        import traceback
        traceback.print_exc()
        return None


def _extract_from_aqwa_lis(
    lis_path: Path,
    vessel_name: str,
    water_depth: float,
) -> Optional[DiffractionResults]:
    """Extract DiffractionResults from an AQWA .LIS output file.

    Parses the text-based LIS file for:
    - Displacement RAOs (first R.A.O.S-VARIATION block only)
    - Added mass matrices (6x6 per frequency)
    - Damping matrices (6x6 per frequency)

    Args:
        lis_path: Path to the AQWA .LIS file.
        vessel_name: Name of the vessel for metadata.
        water_depth: Water depth in metres.

    Returns:
        DiffractionResults or None on failure.
    """
    if not lis_path.exists():
        print(f"  [WARNING] LIS file not found: {lis_path}")
        return None

    try:
        text = lis_path.read_text(errors="replace")
        lines = text.splitlines()
    except Exception as e:
        print(f"  [WARNING] Could not read LIS file: {e}")
        return None

    try:
        # ---------------------------------------------------------------
        # 1. Parse displacement RAOs (first occurrence only)
        # ---------------------------------------------------------------
        rao_data = _parse_aqwa_raos(lines)
        if not rao_data:
            print("  [WARNING] No RAO data found in LIS file")
            return None

        headings_list, freq_list, amp_array, phase_array = rao_data

        frequencies = np.array(sorted(set(freq_list)))
        headings_unique = np.array(sorted(set(headings_list)))
        n_freq = len(frequencies)
        n_head = len(headings_unique)

        # Build lookup: (freq, heading) -> row index in raw data
        # Raw data is stored per (heading_block, freq_row)
        # Reshape into [nfreq, nheading, 6]
        mag = np.zeros((n_freq, n_head, 6))
        pha = np.zeros((n_freq, n_head, 6))

        freq_idx_map = {float(f): i for i, f in enumerate(frequencies)}
        head_idx_map = {float(h): i for i, h in enumerate(headings_unique)}

        for row_i in range(len(freq_list)):
            fi = freq_idx_map.get(float(freq_list[row_i]))
            hi = head_idx_map.get(float(headings_list[row_i]))
            if fi is not None and hi is not None:
                mag[fi, hi, :] = amp_array[row_i]
                pha[fi, hi, :] = phase_array[row_i]

        freq_data = FrequencyData(
            values=frequencies,
            periods=2.0 * np.pi / frequencies,
            count=n_freq,
            min_freq=0.0,
            max_freq=0.0,
        )
        head_data = HeadingData(
            values=headings_unique,
            count=n_head,
            min_heading=0.0,
            max_heading=0.0,
        )

        dof_list = [DOF.SURGE, DOF.SWAY, DOF.HEAVE, DOF.ROLL, DOF.PITCH, DOF.YAW]
        components = {}
        for i, dof in enumerate(dof_list):
            components[dof.name.lower()] = RAOComponent(
                dof=dof,
                magnitude=mag[:, :, i],
                phase=pha[:, :, i],
                frequencies=freq_data,
                headings=head_data,
                unit="",
            )

        now_str = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        rao_set = RAOSet(
            vessel_name=vessel_name,
            analysis_tool="AQWA",
            water_depth=water_depth,
            surge=components["surge"],
            sway=components["sway"],
            heave=components["heave"],
            roll=components["roll"],
            pitch=components["pitch"],
            yaw=components["yaw"],
            created_date=now_str,
            source_file=str(lis_path),
        )

        # ---------------------------------------------------------------
        # 2. Parse added mass matrices
        # ---------------------------------------------------------------
        am_matrices, am_freqs = _parse_aqwa_matrix_section(
            lines, "ADDED MASS", "added_mass",
        )
        if am_matrices and am_freqs:
            am_freq_arr = np.array(am_freqs)
            am_freq_data = FrequencyData(
                values=am_freq_arr,
                periods=2.0 * np.pi / am_freq_arr,
                count=len(am_freq_arr),
                min_freq=0.0,
                max_freq=0.0,
            )
        else:
            # Fall back to RAO frequencies with zero matrices
            am_freq_data = freq_data
            am_matrices = [
                HydrodynamicMatrix(
                    matrix=np.zeros((6, 6)),
                    frequency=float(f),
                    matrix_type="added_mass",
                    units={"coupling": "kg"},
                )
                for f in frequencies
            ]
            print("  [INFO] Added mass not found in LIS; using zeros")

        am_set = AddedMassSet(
            vessel_name=vessel_name,
            analysis_tool="AQWA",
            water_depth=water_depth,
            matrices=am_matrices,
            frequencies=am_freq_data,
            created_date=now_str,
            source_file=str(lis_path),
        )

        # ---------------------------------------------------------------
        # 3. Parse damping matrices
        # ---------------------------------------------------------------
        damp_matrices, damp_freqs = _parse_aqwa_matrix_section(
            lines, "DAMPING", "damping",
        )
        if damp_matrices and damp_freqs:
            damp_freq_arr = np.array(damp_freqs)
            damp_freq_data = FrequencyData(
                values=damp_freq_arr,
                periods=2.0 * np.pi / damp_freq_arr,
                count=len(damp_freq_arr),
                min_freq=0.0,
                max_freq=0.0,
            )
        else:
            damp_freq_data = freq_data
            damp_matrices = [
                HydrodynamicMatrix(
                    matrix=np.zeros((6, 6)),
                    frequency=float(f),
                    matrix_type="damping",
                    units={"coupling": "N.s/m"},
                )
                for f in frequencies
            ]
            print("  [INFO] Damping not found in LIS; using zeros")

        damp_set = DampingSet(
            vessel_name=vessel_name,
            analysis_tool="AQWA",
            water_depth=water_depth,
            matrices=damp_matrices,
            frequencies=damp_freq_data,
            created_date=now_str,
            source_file=str(lis_path),
        )

        return DiffractionResults(
            vessel_name=vessel_name,
            analysis_tool="AQWA",
            water_depth=water_depth,
            raos=rao_set,
            added_mass=am_set,
            damping=damp_set,
            created_date=now_str,
            source_files=[str(lis_path)],
        )

    except Exception as e:
        print(f"  [WARNING] Failed to extract from AQWA LIS: {e}")
        import traceback
        traceback.print_exc()
        return None


def _parse_aqwa_raos(
    lines: List[str],
) -> Optional[tuple]:
    """Parse displacement RAOs from AQWA LIS lines.

    Only the *first* 'R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY' block
    is parsed (displacement RAOs). Subsequent blocks (velocity /
    acceleration) are skipped.

    Returns:
        Tuple of (headings, frequencies, amplitudes, phases) where
        headings and frequencies are flat lists aligned with
        amplitudes/phases rows, or None if nothing found.
    """
    disp_marker = "R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY"
    vel_marker = "VEL R.A.O"
    acc_marker = "ACC R.A.O"

    # Collect ALL displacement RAO blocks (one per heading pair).
    # Stop when we hit velocity or acceleration RAO sections.
    block_start = None
    block_end = None
    for i, line in enumerate(lines):
        if disp_marker in line and block_start is None:
            block_start = i
        elif block_start is not None and (vel_marker in line or acc_marker in line):
            block_end = i
            break

    if block_start is None:
        return None
    if block_end is None:
        block_end = len(lines)

    data_lines = lines[block_start:block_end]

    headings_out: List[float] = []
    freqs_out: List[float] = []
    amps_out: List[np.ndarray] = []
    phases_out: List[np.ndarray] = []

    current_heading: Optional[float] = None
    in_data = False

    for line in data_lines:
        stripped = line.strip()
        if not stripped:
            # Blank lines are common in the LIS format (e.g. between
            # heading blocks and between headers and data). Don't reset
            # in_data — just skip and continue.
            continue

        # Skip header / separator lines
        if stripped.startswith("R.A.O") or stripped.startswith("---"):
            continue
        if "PERIOD" in stripped and "FREQ" in stripped and "DIRECTION" in stripped:
            continue
        if stripped.startswith("(SECS)"):
            in_data = True
            continue
        if stripped.startswith("------"):
            continue
        # Skip page break lines (start with "1" in column 1 as form-feed)
        if line and line[0] == "1" and len(stripped) < 3:
            continue
        # Skip repeated section headers that appear after page breaks
        if "HYDRODYNAMIC" in stripped or "WITH RESPECT" in stripped:
            continue

        if not in_data:
            continue

        # Try to parse a data line
        parts = stripped.split()
        if len(parts) < 12:
            continue

        try:
            period = float(parts[0])
            freq = float(parts[1])
        except (ValueError, IndexError):
            continue

        # Determine if this line carries a direction value
        # Lines with direction have 15 numeric fields:
        #   period, freq, direction, then 6*(amp, phase) = 12 -> total 15
        # Lines without direction have 14 numeric fields:
        #   period, freq, then 6*(amp, phase) = 12 -> total 14
        try:
            if len(parts) >= 15:
                # Has direction
                direction = float(parts[2])
                current_heading = direction
                vals = [float(v) for v in parts[3:15]]
            elif len(parts) >= 14:
                # No direction column; reuse previous heading
                vals = [float(v) for v in parts[2:14]]
            else:
                continue
        except (ValueError, IndexError):
            continue

        if current_heading is None:
            continue

        amp_row = np.array([vals[j] for j in range(0, 12, 2)])
        pha_row = np.array([vals[j] for j in range(1, 12, 2)])

        headings_out.append(current_heading)
        freqs_out.append(freq)
        amps_out.append(amp_row)
        phases_out.append(pha_row)

    if not amps_out:
        return None

    return (
        headings_out,
        freqs_out,
        np.array(amps_out),
        np.array(phases_out),
    )


def _parse_aqwa_matrix_section(
    lines: List[str],
    keyword: str,
    matrix_type: str,
) -> tuple:
    """Parse 6x6 matrix blocks (added mass or damping) from AQWA LIS.

    Searches for sections containing *keyword* (e.g. 'ADDED MASS' or
    'DAMPING') followed by 6x6 numeric blocks, one per frequency.

    Returns:
        Tuple of (list[HydrodynamicMatrix], list[float]) or ([], [])
        if not found.
    """
    matrices: List[HydrodynamicMatrix] = []
    freq_values: List[float] = []

    units = {"coupling": "kg"} if matrix_type == "added_mass" else {"coupling": "N.s/m"}

    # Find all lines that mention the keyword in an appropriate context
    section_indices = []
    for i, line in enumerate(lines):
        upper = line.upper()
        if keyword.upper() in upper and "MATRIX" not in upper:
            # Likely a section header; look for frequency info nearby
            section_indices.append(i)

    if not section_indices:
        return [], []

    for sec_start in section_indices:
        # Search nearby lines for a frequency value
        freq_val = None
        for offset in range(0, 5):
            if sec_start + offset >= len(lines):
                break
            search_line = lines[sec_start + offset].upper()
            # Look for frequency patterns like "FREQ = 0.200" or
            # "FREQUENCY  0.200 RAD/S" or "PERIOD = 31.42"
            for token_i, token in enumerate(search_line.split()):
                if token in ("FREQ", "FREQUENCY", "FREQ."):
                    # Next numeric token is the frequency
                    remaining = search_line.split()[token_i + 1:]
                    for t in remaining:
                        t_clean = t.strip("=,()")
                        try:
                            freq_val = float(t_clean)
                            break
                        except ValueError:
                            continue
                    if freq_val is not None:
                        break
                elif token == "PERIOD":
                    remaining = search_line.split()[token_i + 1:]
                    for t in remaining:
                        t_clean = t.strip("=,()")
                        try:
                            period_val = float(t_clean)
                            if period_val > 0:
                                freq_val = 2.0 * np.pi / period_val
                            break
                        except ValueError:
                            continue
                    if freq_val is not None:
                        break
            if freq_val is not None:
                break

        if freq_val is None:
            continue

        # Now find the 6x6 matrix: scan forward for 6 consecutive lines
        # each having at least 6 numeric values.
        matrix_rows: List[np.ndarray] = []
        scan_start = sec_start + 1
        for j in range(scan_start, min(scan_start + 30, len(lines))):
            row_line = lines[j].strip()
            if not row_line:
                if matrix_rows:
                    break
                continue

            parts = row_line.split()
            # Try to parse at least 6 floats from this line
            nums = []
            for p in parts:
                try:
                    nums.append(float(p))
                except ValueError:
                    continue

            if len(nums) >= 6:
                matrix_rows.append(np.array(nums[:6]))
                if len(matrix_rows) == 6:
                    break
            elif matrix_rows:
                # Non-numeric line after partial matrix; reset
                break

        if len(matrix_rows) == 6:
            mat = np.array(matrix_rows)
            matrices.append(
                HydrodynamicMatrix(
                    matrix=mat,
                    frequency=freq_val,
                    matrix_type=matrix_type,
                    units=units,
                )
            )
            freq_values.append(freq_val)

    return matrices, freq_values


def _harmonize_headings(
    solver_results: Dict[str, DiffractionResults],
) -> Dict[str, DiffractionResults]:
    """Filter all solver results to a common heading set.

    AQWA may have expanded headings (-180..+180) while OrcaWave uses
    the spec headings (0..180). Find the intersection and slice.
    """
    # Collect heading sets
    heading_sets = []
    for name, dr in solver_results.items():
        h = set(float(v) for v in dr.raos.surge.headings.values)
        heading_sets.append(h)

    common = heading_sets[0]
    for hs in heading_sets[1:]:
        common = common & hs

    if not common:
        print("  [WARNING] No common headings across solvers")
        return solver_results

    common_sorted = sorted(common)
    print(f"  [INFO] Common headings ({len(common_sorted)}): {common_sorted}")

    filtered: Dict[str, DiffractionResults] = {}
    for name, dr in solver_results.items():
        all_headings = [float(v) for v in dr.raos.surge.headings.values]
        if set(all_headings) == set(common_sorted):
            filtered[name] = dr
            continue

        # Build index mask for common headings
        indices = [i for i, h in enumerate(all_headings) if h in common]
        if not indices:
            continue

        new_head_arr = np.array(common_sorted)
        new_head_data = HeadingData(
            values=new_head_arr,
            count=len(common_sorted),
            min_heading=0.0,
            max_heading=0.0,
        )

        # Re-slice each DOF component
        dof_names = ["surge", "sway", "heave", "roll", "pitch", "yaw"]
        new_components = {}
        for dof_name in dof_names:
            comp = getattr(dr.raos, dof_name)
            new_components[dof_name] = RAOComponent(
                dof=comp.dof,
                magnitude=comp.magnitude[:, indices],
                phase=comp.phase[:, indices],
                frequencies=comp.frequencies,
                headings=new_head_data,
                unit=comp.unit,
            )

        new_rao_set = RAOSet(
            vessel_name=dr.raos.vessel_name,
            analysis_tool=dr.raos.analysis_tool,
            water_depth=dr.raos.water_depth,
            surge=new_components["surge"],
            sway=new_components["sway"],
            heave=new_components["heave"],
            roll=new_components["roll"],
            pitch=new_components["pitch"],
            yaw=new_components["yaw"],
            created_date=dr.raos.created_date,
            source_file=dr.raos.source_file,
        )

        filtered[name] = DiffractionResults(
            vessel_name=dr.vessel_name,
            analysis_tool=dr.analysis_tool,
            water_depth=dr.water_depth,
            raos=new_rao_set,
            added_mass=dr.added_mass,
            damping=dr.damping,
            created_date=dr.created_date,
            source_files=dr.source_files,
        )
        print(f"  [INFO] Filtered {name}: {len(all_headings)} -> {len(common_sorted)} headings")

    return filtered


# ---------------------------------------------------------------------------
# Orchestrator
# ---------------------------------------------------------------------------


def run_benchmark(
    spec_path: Path,
    output_dir: Path,
    solvers: List[str],
    dry_run: bool = False,
) -> BenchmarkOrchestrationResult:
    """Run full 3-way benchmark orchestration."""
    print(f"\n{'#'*60}")
    print("# 3-Way Solver Benchmark")
    print(f"# Spec: {spec_path}")
    print(f"# Solvers: {', '.join(solvers)}")
    print(f"# Output: {output_dir}")
    print(f"# Dry Run: {dry_run}")
    print(f"{'#'*60}")

    start_time = time.time()
    result = BenchmarkOrchestrationResult(
        spec_path=spec_path,
        solvers_requested=solvers,
    )

    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)

    # Run each solver
    solver_runners = {
        "orcawave": run_orcawave,
        "aqwa": run_aqwa,
        "bemrosetta": run_bemrosetta,
    }

    for solver in solvers:
        solver_lower = solver.lower()
        if solver_lower in solver_runners:
            solver_result = solver_runners[solver_lower](
                spec_path=spec_path,
                output_dir=output_dir,
                dry_run=dry_run,
            )
            result.solver_results[solver_result.solver_name] = solver_result
        else:
            print(f"\n[WARNING] Unknown solver: {solver}")

    # Collect successful results for comparison
    successful_results: Dict[str, DiffractionResults] = {}
    for name, sr in result.solver_results.items():
        if sr.status == "completed" and sr.results is not None:
            successful_results[name] = sr.results

    # Harmonize heading sets: filter to common headings across solvers
    if len(successful_results) >= 2:
        successful_results = _harmonize_headings(successful_results)

    # Run benchmark comparison if we have 2+ results
    if len(successful_results) >= 2:
        print(f"\n{'='*60}")
        print("Running Benchmark Comparison...")
        print(f"{'='*60}")

        try:
            benchmark_output = output_dir / "benchmark_comparison"
            config = BenchmarkConfig(
                output_dir=benchmark_output,
                tolerance=0.05,
                x_axis="period",
            )
            runner = BenchmarkRunner(config)
            result.benchmark_result = runner.run_from_results(successful_results)

            if result.benchmark_result.success:
                print(f"  [OK] Benchmark comparison complete")
                print(f"  Consensus: {result.benchmark_result.report.overall_consensus}")
                print(f"  Report: {result.benchmark_result.report_html_path}")
            else:
                print(f"  [FAILED] {result.benchmark_result.error_message}")

        except Exception as e:
            print(f"  [FAILED] Benchmark comparison error: {e}")

    elif dry_run:
        print(f"\n[DRY RUN] Skipping benchmark comparison (no actual results)")
    else:
        print(f"\n[WARNING] Need 2+ successful solver runs for comparison")
        print(f"  Completed: {[n for n, r in result.solver_results.items() if r.status == 'completed']}")

    # Summary
    result.total_duration_seconds = time.time() - start_time
    result.success = any(
        r.status in ("completed", "dry_run")
        for r in result.solver_results.values()
    )

    print(f"\n{'#'*60}")
    print("# Summary")
    print(f"{'#'*60}")
    for name, sr in result.solver_results.items():
        status_icon = {
            "completed": "[OK]",
            "dry_run": "[DRY]",
            "failed": "[FAIL]",
            "skipped": "[SKIP]",
        }.get(sr.status, "[?]")
        print(f"  {status_icon} {name}: {sr.status} ({sr.duration_seconds:.1f}s)")

    print(f"\n  Total duration: {result.total_duration_seconds:.1f}s")
    print(f"  Output directory: {output_dir}")

    # Write summary JSON
    summary_file = output_dir / "benchmark_summary.json"
    _write_summary(result, summary_file)
    print(f"  Summary: {summary_file}")

    return result


def _write_summary(result: BenchmarkOrchestrationResult, output_file: Path) -> None:
    """Write orchestration summary to JSON."""
    summary = {
        "spec_path": str(result.spec_path),
        "timestamp": datetime.now().isoformat(),
        "solvers_requested": result.solvers_requested,
        "total_duration_seconds": result.total_duration_seconds,
        "success": result.success,
        "solver_results": {
            name: {
                "status": sr.status,
                "duration_seconds": sr.duration_seconds,
                "output_dir": str(sr.output_dir) if sr.output_dir else None,
                "error_message": sr.error_message,
            }
            for name, sr in result.solver_results.items()
        },
    }

    if result.benchmark_result and result.benchmark_result.report:
        summary["benchmark"] = {
            "overall_consensus": result.benchmark_result.report.overall_consensus,
            "report_json": str(result.benchmark_result.report_json_path),
            "report_html": str(result.benchmark_result.report_html_path),
        }

    with open(output_file, "w") as f:
        json.dump(summary, f, indent=2)


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def main():
    parser = argparse.ArgumentParser(
        description="Run 3-way solver benchmark (AQWA, OrcaWave, BEMRosetta)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Run all three solvers on Unit Box
  uv run python scripts/benchmark/run_3way_benchmark.py specs/modules/benchmark/unit_box_spec.yml

  # Run only OrcaWave and AQWA
  uv run python scripts/benchmark/run_3way_benchmark.py spec.yml --solvers orcawave,aqwa

  # Dry run (generate files, skip solver execution)
  uv run python scripts/benchmark/run_3way_benchmark.py spec.yml --dry-run

  # Custom output directory
  uv run python scripts/benchmark/run_3way_benchmark.py spec.yml -o results/my_benchmark
        """,
    )

    parser.add_argument(
        "spec_path",
        type=Path,
        help="Path to DiffractionSpec YAML file",
    )
    parser.add_argument(
        "--solvers", "-s",
        type=str,
        default="orcawave,aqwa,bemrosetta",
        help="Comma-separated list of solvers (default: orcawave,aqwa,bemrosetta)",
    )
    parser.add_argument(
        "--output", "-o",
        type=Path,
        default=None,
        help="Output directory (default: benchmark_output/<spec_name>_<timestamp>)",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Generate input files but don't execute solvers",
    )

    args = parser.parse_args()

    # Validate spec path
    if not args.spec_path.exists():
        print(f"Error: Spec file not found: {args.spec_path}")
        sys.exit(1)

    # Parse solvers
    solvers = [s.strip() for s in args.solvers.split(",")]

    # Determine output directory
    if args.output:
        output_dir = args.output
    else:
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        spec_name = args.spec_path.stem
        output_dir = Path("benchmark_output") / f"{spec_name}_{timestamp}"

    # Run benchmark
    result = run_benchmark(
        spec_path=args.spec_path,
        output_dir=output_dir,
        solvers=solvers,
        dry_run=args.dry_run,
    )

    # Exit code
    sys.exit(0 if result.success else 1)


if __name__ == "__main__":
    main()
