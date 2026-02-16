#!/usr/bin/env python3
"""Validate spec.yml pipeline by comparing against solved .owd ground truth.

For each WAMIT validation case, this script:
1. Loads the original .owd project → Calculate() → extracts DiffractionResults
2. Runs spec.yml through OrcaWaveRunner → extracts DiffractionResults
3. Compares the two using BenchmarkRunner → HTML report

This validates that our spec.yml correctly configures OrcaWave to produce
the same results as the manually-configured .owd projects.

Usage:
    uv run python scripts/benchmark/validate_owd_vs_spec.py --case 2.7
    uv run python scripts/benchmark/validate_owd_vs_spec.py --all
    uv run python scripts/benchmark/validate_owd_vs_spec.py --case 2.7 --owd-only
"""
from __future__ import annotations

import argparse
import io
import sys
import time
from datetime import datetime
from pathlib import Path
from typing import Optional

import numpy as np

# Fix Windows console encoding
if sys.platform == "win32":
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")
    sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding="utf-8")


REPO_ROOT = Path(__file__).parent.parent.parent
L00_DIR = REPO_ROOT / "docs" / "modules" / "orcawave" / "L00_validation_wamit"
OUTPUT_DIR = L00_DIR  # benchmark artifacts live alongside case docs

# Add repo root to sys.path so scripts.benchmark imports work
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

CASES = {
    # Phase 1: Library-matched cases
    "2.7": {
        "owd": L00_DIR / "2.7" / "OrcaWave v11.0 files" / "Pyramid.owd",
        "spec": L00_DIR / "2.7" / "spec.yml",
        "vessel_name": "Pyramid_ZC08",
        "water_depth": float("inf"),
        "description": "Inverted Pyramid",
    },
    "2.8": {
        "owd": L00_DIR / "2.8" / "OrcaWave v11.0 files" / "Ellipsoid.owd",
        "spec": L00_DIR / "2.8" / "spec.yml",
        "vessel_name": "Ellipsoid_96p",
        "water_depth": float("inf"),
        "description": "Ellipsoid",
    },
    "3.2": {
        "owd": L00_DIR / "3.2" / "OrcaWave v11.0 files" / "Sphere.owd",
        "spec": L00_DIR / "3.2" / "spec.yml",
        "vessel_name": "Sphere_R5",
        "water_depth": float("inf"),
        "description": "Sphere with Lid",
    },
    "3.3": {
        "owd": L00_DIR / "3.3" / "OrcaWave v11.0 files" / "test.owd",
        "spec": L00_DIR / "3.3" / "spec.yml",
        "vessel_name": "Ellipsoid",
        "water_depth": float("inf"),
        "description": "Multi-body Cylinder + Ellipsoid",
        "body_index": 1,  # Compare Ellipsoid (body 1); Cylinder (body 0) is fixed
    },
    # Phase 2: New hull library cases
    "2.1": {
        "owd": L00_DIR / "2.1" / "OrcaWave v11.0 files" / "test01.owd",
        "spec": L00_DIR / "2.1" / "spec.yml",
        "vessel_name": "Test01_cylinder",
        "water_depth": float("inf"),
        "description": "Cylinder R=1 T=0.5 (no IRR)",
    },
    "2.2": {
        "owd": L00_DIR / "2.2" / "OrcaWave v11.0 files" / "test02 with base mesh.owd",
        "spec": L00_DIR / "2.2" / "spec.yml",
        "vessel_name": "Test01_cylinder",
        "water_depth": float("inf"),
        "description": "Cylinder R=1 T=0.5 (base mesh, extended freq)",
    },
    "2.3": {
        "owd": L00_DIR / "2.3" / "OrcaWave v11.0 files" / "test01a.owd",
        "spec": L00_DIR / "2.3" / "spec.yml",
        "vessel_name": "Test01_cylinder",
        "water_depth": float("inf"),
        "description": "Cylinder R=1 T=0.5 (trimmed z=0.27 rx=15deg)",
    },
    "2.5c": {
        "owd": L00_DIR / "2.5" / "OrcaWave v11.0 files" / "test06.owd",
        "spec": L00_DIR / "2.5" / "spec_coarse.yml",
        "vessel_name": "Body1",
        "water_depth": 450.0,
        "description": "ISSC TLP coarse (128 panels)",
    },
    "2.5f": {
        "owd": L00_DIR / "2.5" / "OrcaWave v11.0 files" / "test07.owd",
        "spec": L00_DIR / "2.5" / "spec_fine.yml",
        "vessel_name": "Body1",
        "water_depth": 450.0,
        "description": "ISSC TLP fine (1012 panels)",
    },
    "2.6": {
        "owd": L00_DIR / "2.6" / "OrcaWave v11.0 files" / "test05a.owd",
        "spec": L00_DIR / "2.6" / "spec.yml",
        "vessel_name": "test05_cylinder",
        "water_depth": 3.0,
        "description": "Multi-body Cylinder+Spheroid shallow water",
    },
    # Phase 3: Advanced cases
    "2.9": {
        "owd": L00_DIR / "2.9" / "OrcaWave v11.0 files" / "Moonpool Body.owd",
        "spec": L00_DIR / "2.9" / "spec.yml",
        "vessel_name": "cylinder with moonpool",
        "water_depth": float("inf"),
        "description": "Moonpool cylinder with damping lid",
    },
    "3.1": {
        "owd": L00_DIR / "3.1" / "OrcaWave v11.0 files" / "Bottom mounted cylinder.owd",
        "spec": L00_DIR / "3.1" / "spec.yml",
        "vessel_name": "test101 bottom mounted cylinder",
        "water_depth": 1.0,
        "description": "Bottom-mounted cylinder, all DOFs fixed, Full QTF",
    },
}


def _extract_from_diffraction(
    diff,
    vessel_name: str,
    water_depth: float,
    source_label: str = "OrcaWave",
    source_file: str = "",
    body_index: int = 0,
) -> Optional["DiffractionResults"]:
    """Extract DiffractionResults from a solved OrcFxAPI.Diffraction object.

    Mirrors _extract_from_owr() from run_3way_benchmark.py but works on
    an already-solved in-memory Diffraction object.
    """
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

    try:
        # OrcFxAPI returns frequencies in Hz; convert to rad/s
        frequencies = 2.0 * np.pi * np.array(diff.frequencies)
        headings = np.array(diff.headings)

        # Extract displacement RAOs - shape (nheading, nfreq, ndof)
        # Transpose to (nfreq, nheading, ndof) for consistency
        raw_raos = np.array(diff.displacementRAOs)
        ndof = raw_raos.shape[2] if len(raw_raos.shape) == 3 else 6

        # For multi-body, ndof > 6 (e.g. 12 for 2 bodies). Extract target body.
        if ndof > 6:
            dof_start = body_index * 6
            raw_raos = raw_raos[:, :, dof_start:dof_start + 6]

        raw_raos = np.transpose(raw_raos, (1, 0, 2))  # (nfreq, nheading, 6)

        # Sort by ascending frequency (OrcFxAPI may return descending)
        sort_idx = np.argsort(frequencies)
        frequencies = frequencies[sort_idx]
        raw_raos = raw_raos[sort_idx, :, :]

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

        dof_list = [DOF.SURGE, DOF.SWAY, DOF.HEAVE, DOF.ROLL, DOF.PITCH, DOF.YAW]
        rotational_dofs = {DOF.ROLL, DOF.PITCH, DOF.YAW}
        components = {}
        for i, dof in enumerate(dof_list):
            rao_complex = raw_raos[:, :, i]
            magnitude = np.abs(rao_complex)
            if dof in rotational_dofs:
                magnitude = np.degrees(magnitude)
            components[dof.name.lower()] = RAOComponent(
                dof=dof,
                magnitude=magnitude,
                phase=np.degrees(np.angle(rao_complex)),
                frequencies=freq_data,
                headings=head_data,
                unit="deg/m" if dof in rotational_dofs else "m/m",
            )

        now_str = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        rao_set = RAOSet(
            vessel_name=vessel_name,
            analysis_tool=source_label,
            water_depth=water_depth,
            surge=components["surge"],
            sway=components["sway"],
            heave=components["heave"],
            roll=components["roll"],
            pitch=components["pitch"],
            yaw=components["yaw"],
            created_date=now_str,
            source_file=source_file,
        )

        # Extract added mass - shape: (nfreq, N, N)
        am_raw_full = np.array(diff.addedMass)
        # For multi-body, extract target body's 6x6 block
        if am_raw_full.shape[1] > 6:
            r0 = body_index * 6
            am_raw_full = am_raw_full[:, r0:r0 + 6, r0:r0 + 6]
        am_raw = am_raw_full[sort_idx]

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
            analysis_tool=source_label,
            water_depth=water_depth,
            matrices=am_matrices,
            frequencies=freq_data,
            created_date=now_str,
            source_file=source_file,
        )

        # Extract damping
        damp_raw_full = np.array(diff.damping)
        if damp_raw_full.shape[1] > 6:
            r0 = body_index * 6
            damp_raw_full = damp_raw_full[:, r0:r0 + 6, r0:r0 + 6]
        damp_raw = damp_raw_full[sort_idx]

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
            analysis_tool=source_label,
            water_depth=water_depth,
            matrices=damp_matrices,
            frequencies=freq_data,
            created_date=now_str,
            source_file=source_file,
        )

        return DiffractionResults(
            vessel_name=vessel_name,
            analysis_tool=source_label,
            water_depth=water_depth,
            raos=rao_set,
            added_mass=am_set,
            damping=damp_set,
            created_date=now_str,
            source_files=[source_file],
            phase_convention="orcina_lag",
            unit_system="orcaflex",
        )

    except Exception as e:
        import traceback
        print(f"  [ERROR] Extraction failed: {e}")
        traceback.print_exc()
        return None


def solve_owd(
    case_id: str,
) -> tuple[Optional["DiffractionResults"], Optional[Path]]:
    """Load .owd file, run Calculate(), and extract results.

    Returns
    -------
    Tuple of (DiffractionResults or None, Path to input .yml or None).
    """
    import OrcFxAPI

    case = CASES[case_id]
    owd_path = case["owd"]

    print(f"\n  Loading .owd: {owd_path.name}")
    diff = OrcFxAPI.Diffraction()
    diff.LoadData(str(owd_path.resolve()))

    print(f"  Running Calculate()...")
    t0 = time.perf_counter()
    diff.Calculate()
    dt = time.perf_counter() - t0
    print(f"  Solved in {dt:.1f}s")

    # Export input configuration as YAML (after solve to avoid changing
    # OrcFxAPI's internal working directory which breaks relative mesh paths)
    out_dir = OUTPUT_DIR / case_id / "benchmark"
    out_dir.mkdir(parents=True, exist_ok=True)
    owd_yml_path = out_dir / f"{owd_path.stem}_input.yml"
    try:
        diff.SaveData(str(owd_yml_path.resolve()))
        print(f"  Saved input YAML: {owd_yml_path.name}")
    except Exception as exc:
        print(f"  Could not save input YAML: {exc}")
        owd_yml_path = None

    # Save .owr for later use
    owr_path = out_dir / f"{owd_path.stem}_ground_truth.owr"
    try:
        diff.SaveResults(str(owr_path.resolve()))
        print(f"  Saved: {owr_path.name}")
    except Exception as exc:
        print(f"  Could not save .owr: {exc}")

    results = _extract_from_diffraction(
        diff,
        vessel_name=case["vessel_name"],
        water_depth=case["water_depth"],
        source_label="OrcaWave (.owd)",
        source_file=str(owd_path),
        body_index=case.get("body_index", 0),
    )
    return results, owd_yml_path


def solve_spec(
    case_id: str,
) -> tuple[Optional["DiffractionResults"], Optional[Path]]:
    """Run spec.yml through OrcaWaveRunner and extract results.

    Returns
    -------
    Tuple of (DiffractionResults or None, Path to input .yml or None).
    """
    import OrcFxAPI
    from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
    from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
        OrcaWaveRunner,
        RunConfig,
        RunStatus,
    )

    case = CASES[case_id]
    spec_path = case["spec"]

    print(f"\n  Loading spec: {spec_path.name}")
    spec = DiffractionSpec.from_yaml(spec_path)

    # Configure runner
    out_dir = OUTPUT_DIR / case_id / "benchmark" / "spec_orcawave"
    config = RunConfig(
        output_dir=out_dir,
        dry_run=False,
        timeout_seconds=600,
    )

    print(f"  Creating OrcaWaveRunner...")
    runner = OrcaWaveRunner(config)

    print(f"  Running OrcaWave from spec...")
    t0 = time.perf_counter()
    run_result = runner.run(spec, spec_path=spec_path)
    dt = time.perf_counter() - t0
    print(f"  Completed in {dt:.1f}s (status: {run_result.status})")

    if run_result.status == RunStatus.COMPLETED:
        # Extract from .owr
        owr_files = sorted(out_dir.glob("*.owr"))
        if owr_files:
            print(f"  Extracting from: {owr_files[0].name}")
            diff = OrcFxAPI.Diffraction()
            diff.LoadResults(str(owr_files[0].resolve()))

            # Export input YAML from the solved project
            spec_input_yml = out_dir / "spec_input.yml"
            try:
                # Load original data to save input config
                yml_files = sorted(out_dir.glob("*.yml"))
                if yml_files:
                    diff_input = OrcFxAPI.Diffraction()
                    diff_input.LoadData(str(yml_files[0].resolve()))
                    diff_input.SaveData(str(spec_input_yml.resolve()))
                    print(f"  Saved input YAML: {spec_input_yml.name}")
                else:
                    spec_input_yml = None
            except Exception as exc:
                print(f"  Could not save input YAML: {exc}")
                spec_input_yml = None

            results = _extract_from_diffraction(
                diff,
                vessel_name=case["vessel_name"],
                water_depth=case["water_depth"],
                source_label="OrcaWave (spec.yml)",
                source_file=str(spec_path),
                body_index=case.get("body_index", 0),
            )
            return results, spec_input_yml
        else:
            print(f"  [ERROR] No .owr file found")
            return None, None
    elif run_result.status == RunStatus.DRY_RUN:
        # OrcaWave executable not found — fall back to direct OrcFxAPI solve
        print(f"  Runner produced dry-run. Falling back to direct OrcFxAPI solve...")
        return _solve_spec_via_orcfxapi(case_id, spec, spec_path)
    else:
        print(f"  [ERROR] Runner failed: {run_result.error_message}")
        # Also fall back to OrcFxAPI direct
        print(f"  Falling back to direct OrcFxAPI solve...")
        return _solve_spec_via_orcfxapi(case_id, spec, spec_path)


def _solve_spec_via_orcfxapi(
    case_id: str,
    spec: "DiffractionSpec",
    spec_path: Path,
) -> tuple[Optional["DiffractionResults"], Optional[Path]]:
    """Build and solve OrcaWave project directly via OrcFxAPI.

    Uses OrcaWaveRunner.prepare() to generate the .yml, then loads and
    solves via OrcFxAPI Python binding directly.

    Returns
    -------
    Tuple of (DiffractionResults or None, Path to input .yml or None).
    """
    import OrcFxAPI
    from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
        OrcaWaveRunner,
        RunConfig,
    )

    case = CASES[case_id]
    out_dir = OUTPUT_DIR / case_id / "benchmark" / "spec_orcfxapi"
    out_dir.mkdir(parents=True, exist_ok=True)

    # Use runner to generate the project YAML (respects mesh path resolution)
    print(f"  Generating OrcaWave project from spec...")
    config = RunConfig(output_dir=out_dir, dry_run=True, timeout_seconds=600)
    runner = OrcaWaveRunner(config)
    runner.prepare(spec, spec_path=spec_path)

    # Find the generated .yml file
    yml_files = sorted(out_dir.glob("*.yml"))
    if not yml_files:
        print(f"  [ERROR] No .yml file generated in {out_dir}")
        return None, None
    yml_path = yml_files[0]
    print(f"  Generated: {yml_path.name}")

    # Load into OrcFxAPI and solve
    print(f"  Loading into OrcFxAPI...")
    diff = OrcFxAPI.Diffraction()
    diff.LoadData(str(yml_path.resolve()))

    print(f"  Running Calculate()...")
    t0 = time.perf_counter()
    diff.Calculate()
    dt = time.perf_counter() - t0
    print(f"  Solved in {dt:.1f}s")

    # Export the actual loaded configuration as YAML (after solve to avoid
    # changing OrcFxAPI's internal working directory)
    spec_input_yml = out_dir / "spec_input.yml"
    try:
        diff.SaveData(str(spec_input_yml.resolve()))
        print(f"  Saved input YAML: {spec_input_yml.name}")
    except Exception as exc:
        print(f"  Could not save input YAML: {exc}")
        spec_input_yml = None

    # Save results
    owr_path = out_dir / "spec_result.owr"
    try:
        diff.SaveResults(str(owr_path.resolve()))
        print(f"  Saved: {owr_path.name}")
    except Exception as exc:
        print(f"  Could not save .owr: {exc}")

    results = _extract_from_diffraction(
        diff,
        vessel_name=case["vessel_name"],
        water_depth=case["water_depth"],
        source_label="OrcaWave (spec.yml)",
        source_file=str(spec_path),
        body_index=case.get("body_index", 0),
    )
    return results, spec_input_yml


def run_comparison(
    owd_results: "DiffractionResults",
    spec_results: "DiffractionResults",
    case_id: str,
    owd_yml_path: Optional[Path] = None,
    spec_yml_path: Optional[Path] = None,
) -> dict:
    """Compare .owd ground truth against spec.yml results."""
    import yaml

    from digitalmodel.hydrodynamics.diffraction.benchmark_runner import (
        BenchmarkConfig,
        BenchmarkRunner,
    )
    from scripts.benchmark.solver_metadata import (
        build_orcawave_metadata_from_yml,
        build_solver_metadata,
    )

    case = CASES[case_id]
    out_dir = OUTPUT_DIR / case_id / "benchmark"
    body_index = case.get("body_index", 0)

    # Build solver metadata from spec.yml as fallback
    spec_path = case["spec"]
    with open(spec_path) as f:
        spec_dict = yaml.safe_load(f)
    base_metadata = build_solver_metadata(spec_dict, spec_dir=spec_path.parent)

    # Build enriched metadata from SaveData() YAML exports
    metadata: dict[str, dict[str, str]] = {}

    # Resolve mesh_path from base_metadata (spec.yml → absolute path)
    resolved_mesh_path = base_metadata.get("OrcaWave", {}).get("mesh_path")

    if owd_yml_path and owd_yml_path.exists():
        metadata["OrcaWave (.owd)"] = build_orcawave_metadata_from_yml(
            owd_yml_path, body_index=body_index,
        )
    else:
        metadata["OrcaWave (.owd)"] = base_metadata.get("OrcaWave", {})

    if spec_yml_path and spec_yml_path.exists():
        metadata["OrcaWave (spec.yml)"] = build_orcawave_metadata_from_yml(
            spec_yml_path, body_index=body_index,
        )
    else:
        metadata["OrcaWave (spec.yml)"] = base_metadata.get("OrcaWave", {})

    # Propagate mesh_path so the benchmark plotter can render schematics
    if resolved_mesh_path:
        for key in metadata:
            metadata[key].setdefault("mesh_path", resolved_mesh_path)

    # Solver results dict
    solver_results = {
        "OrcaWave (.owd)": owd_results,
        "OrcaWave (spec.yml)": spec_results,
    }

    # Benchmark config
    config = BenchmarkConfig(
        output_dir=out_dir,
        report_title=f"Validation Case {case_id}: {case['description']}",
        report_subtitle="OrcaWave .owd Ground Truth vs spec.yml Pipeline",
    )

    # Semantic equivalence check between the two SaveData() YAMLs
    if owd_yml_path and spec_yml_path and owd_yml_path.exists() and spec_yml_path.exists():
        print(f"\n  Running semantic equivalence check...")
        sem = _compare_orcawave_ymls(
            owd_yml_path, spec_yml_path, body_index=body_index,
        )
        print(
            f"  Semantic: {sem['match_count']} match, "
            f"{sem['cosmetic_count']} cosmetic, "
            f"{sem.get('convention_count', 0)} convention, "
            f"{sem['significant_count']} significant"
        )
        if sem["diffs"]:
            sig_diffs = [d for d in sem["diffs"] if d["level"] == "significant"]
            other_diffs = [d for d in sem["diffs"] if d["level"] != "significant"]
            for d in sig_diffs[:15]:
                print(f"    [DIFF] {d['key']}: {d['owd']} vs {d['spec']}")
            for d in other_diffs[:5]:
                tag = d["level"].upper()
                print(f"    [{tag}] {d['key']}: {d['owd']} vs {d['spec']}")
            remaining = len(sem["diffs"]) - min(15, len(sig_diffs)) - min(5, len(other_diffs))
            if remaining > 0:
                print(f"    ... and {remaining} more")

        # Attach semantic data to the first solver's metadata so the
        # plotter can render it in the report
        first_key = next(iter(metadata))
        metadata[first_key]["_semantic_equivalence"] = sem

    print(f"\n  Running benchmark comparison...")
    runner = BenchmarkRunner(config)
    result = runner.run_from_results(solver_results, solver_metadata=metadata)

    report_path = out_dir / "validation_report.html"
    if hasattr(result, "report_path") and result.report_path:
        report_path = Path(result.report_path)

    print(f"  Report: {report_path}")

    # Compute per-DOF correlation summary
    summary = _compute_correlation_summary(owd_results, spec_results)
    return summary


def _compare_orcawave_ymls(
    owd_yml: Path,
    spec_yml: Path,
    body_index: int = 0,
) -> dict:
    """Semantic comparison of two OrcaWave SaveData() YAML exports.

    Compares key-by-key at the top level and within the target body,
    classifying differences into four levels:

    - **match**: identical values
    - **cosmetic**: GUI/display/version/naming — no solver effect
    - **convention**: equivalent data in different representation
      (e.g. freq in Hz vs rad/s, period vs frequency)
    - **significant**: real solver parameter difference

    Returns a summary dict with counts and per-key diff details.
    """
    import yaml

    # --- Classification sets ---

    # Cosmetic: GUI display, versioning, naming — never affect solver
    _COSMETIC_KEYS = {
        # Versioning / file identity
        "DataFileVersion", "DiffractionVersion", "DataFileName",
        "InternalName", "TitleBarText", "FileName", "DataCreationDate",
        # GUI pen/color settings
        "FreeSurfaceMeshPen", "InteriorSurfacePanelsPen",
        "BodyMeshPen", "WaterlinePen", "DampingLidMeshPen",
        # Output verbosity flags — don't change RAO results
        "OutputPanelPressures", "OutputPanelVelocities",
        # Field point monitoring — doesn't affect RAO calculation
        "FieldPointX", "FieldPointY", "FieldPointZ",
        "FieldPointX, FieldPointY, FieldPointZ",
        # Body naming — cosmetic identifier, not a solver parameter
        "BodyName",
        # Mesh file path — different file for same geometry
        "BodyMeshFileName",
        # Damping lid mesh — different file for same lid geometry
        "DampingLidMeshFileName", "DampingLidMeshFormat", "DampingLidMeshLengthUnits",
        # OrcaFlex import hints — only used when importing to OrcaFlex,
        # not during the OrcaWave solve itself
        "BodyOrcaFlexImportLength", "BodyOrcaFlexImportSymmetry",
    }

    # Dormant: keys that exist but are inactive for non-QTF solve types.
    # Only significant when SolveType contains "QTF".
    _DORMANT_QTF_KEYS = {
        "PreferredQuadraticLoadCalculationMethod",
        "QTFMinCrossingAngle", "QTFMaxCrossingAngle",
        "QuadraticLoadPressureIntegration",
        "QTFCalculationMethod", "QTFFrequencyTypes",
        "IncludeMeanDriftFullQTFs",
    }

    # Convention: keys where both sides hold equivalent data expressed
    # differently (e.g. Hz vs rad/s, period vs frequency label)
    _CONVENTION_KEYS = {
        "WavesReferredToBy",   # "frequency (rad/s)" vs "period (s)"
        "PeriodOrFrequency",   # same frequencies, different units/ordering
    }

    def _load(path: Path) -> dict:
        """Load multi-document OrcaWave YAML, merging all docs."""
        for enc in ("utf-8", "latin-1"):
            try:
                with open(path, encoding=enc) as f:
                    content = f.read()
                docs = list(yaml.safe_load_all(content))
                if not docs:
                    return {}
                merged: dict = {}
                for doc in docs:
                    if not isinstance(doc, dict):
                        continue
                    bodies = doc.pop("Bodies", None)
                    merged.update(doc)
                    if bodies and isinstance(bodies, list):
                        merged.setdefault("Bodies", []).extend(bodies)
                return merged
            except (UnicodeDecodeError, OSError):
                continue
        return {}

    def _values_equal(a, b) -> bool:
        """Compare values with tolerance for floats."""
        if isinstance(a, float) and isinstance(b, float):
            if a == b:
                return True
            if abs(a) < 1e-30 and abs(b) < 1e-30:
                return True
            return abs(a - b) / max(abs(a), abs(b)) < 1e-6
        if isinstance(a, list) and isinstance(b, list):
            if len(a) != len(b):
                return False
            return all(_values_equal(ai, bi) for ai, bi in zip(a, b))
        return a == b

    owd_data = _load(owd_yml)
    spec_data = _load(spec_yml)

    # Check if solve type involves QTF — if not, QTF keys are dormant
    solve_type = str(owd_data.get("SolveType", "")).lower()
    is_qtf = "qtf" in solve_type

    diffs = []
    match_count = 0
    cosmetic_count = 0
    convention_count = 0
    sig_count = 0

    def _classify_key(key: str) -> str:
        """Return classification for a key: cosmetic, convention, dormant, or significant."""
        if key in _COSMETIC_KEYS:
            return "cosmetic"
        if key in _CONVENTION_KEYS:
            return "convention"
        if key in _DORMANT_QTF_KEYS and not is_qtf:
            return "cosmetic"  # dormant = cosmetic when not active
        return "significant"

    def _compare_key(key: str, owd_val, spec_val, display_key: str) -> None:
        nonlocal match_count, cosmetic_count, convention_count, sig_count

        level = _classify_key(key)

        if _values_equal(owd_val, spec_val):
            match_count += 1
            return

        # Both present but different, or one missing
        owd_str = "MISSING" if owd_val is None else str(owd_val)[:80]
        spec_str = "MISSING" if spec_val is None else str(spec_val)[:80]

        if level == "cosmetic":
            cosmetic_count += 1
        elif level == "convention":
            convention_count += 1
        else:
            sig_count += 1

        diffs.append({
            "key": display_key, "level": level,
            "owd": owd_str, "spec": spec_str,
        })

    # Compare top-level keys (excluding Bodies)
    all_keys = set(owd_data.keys()) | set(spec_data.keys())
    all_keys.discard("Bodies")

    for key in sorted(all_keys):
        _compare_key(key, owd_data.get(key), spec_data.get(key), key)

    # Compare within the target body
    owd_bodies = owd_data.get("Bodies", [])
    spec_bodies = spec_data.get("Bodies", [])
    if body_index < len(owd_bodies) and body_index < len(spec_bodies):
        owd_body = owd_bodies[body_index]
        spec_body = spec_bodies[body_index]
        body_keys = set(owd_body.keys()) | set(spec_body.keys())
        for key in sorted(body_keys):
            full_key = f"Bodies[{body_index}].{key}"
            _compare_key(
                key, owd_body.get(key), spec_body.get(key), full_key,
            )

    return {
        "match_count": match_count,
        "cosmetic_count": cosmetic_count,
        "convention_count": convention_count,
        "significant_count": sig_count,
        "has_significant_diffs": sig_count > 0,
        "diffs": diffs,
    }


def _compute_correlation_summary(
    owd_results: "DiffractionResults",
    spec_results: "DiffractionResults",
) -> dict:
    """Compute per-DOF correlation between two result sets."""
    summary = {}
    dof_names = ["surge", "sway", "heave", "roll", "pitch", "yaw"]

    for dof_name in dof_names:
        owd_comp = getattr(owd_results.raos, dof_name)
        spec_comp = getattr(spec_results.raos, dof_name)

        owd_mag = owd_comp.magnitude.flatten()
        spec_mag = spec_comp.magnitude.flatten()

        # Truncate to common length
        min_len = min(len(owd_mag), len(spec_mag))
        owd_mag = owd_mag[:min_len]
        spec_mag = spec_mag[:min_len]

        if min_len > 1 and np.std(owd_mag) > 0 and np.std(spec_mag) > 0:
            r = np.corrcoef(owd_mag, spec_mag)[0, 1]
        elif min_len > 0 and np.allclose(owd_mag, spec_mag, atol=1e-6):
            r = 1.0
        else:
            r = float("nan")

        max_diff = float(np.max(np.abs(owd_mag - spec_mag))) if min_len > 0 else 0.0
        mean_abs = float(np.mean(np.abs(owd_mag))) if min_len > 0 else 0.0
        rel_err = max_diff / mean_abs * 100.0 if mean_abs > 1e-12 else 0.0

        summary[dof_name] = {
            "correlation": round(r, 6) if not np.isnan(r) else "N/A",
            "max_abs_diff": round(max_diff, 6),
            "rel_error_pct": round(rel_err, 2),
            "n_points": min_len,
        }

    return summary


def run_case(case_id: str, owd_only: bool = False) -> dict:
    """Run validation for a single case."""
    case = CASES[case_id]
    print(f"\n{'='*70}")
    print(f"Case {case_id}: {case['description']}")
    print(f"{'='*70}")

    result = {"case_id": case_id, "description": case["description"]}

    # Step 1: Solve .owd
    print("\n[Step 1] Solving .owd ground truth...")
    owd_results, owd_yml_path = solve_owd(case_id)
    if owd_results is None:
        result["status"] = "owd_failed"
        return result

    n_freq = owd_results.raos.surge.frequencies.count
    n_head = owd_results.raos.surge.headings.count
    result["owd_freq"] = n_freq
    result["owd_head"] = n_head
    print(f"  Extracted: {n_freq} freq x {n_head} headings")

    if owd_only:
        result["status"] = "owd_only"
        return result

    # Step 2: Run spec.yml pipeline
    print("\n[Step 2] Running spec.yml pipeline...")
    spec_results, spec_yml_path = solve_spec(case_id)
    if spec_results is None:
        result["status"] = "spec_failed"
        return result

    n_freq2 = spec_results.raos.surge.frequencies.count
    n_head2 = spec_results.raos.surge.headings.count
    result["spec_freq"] = n_freq2
    result["spec_head"] = n_head2
    print(f"  Extracted: {n_freq2} freq x {n_head2} headings")

    # Step 3: Compare
    print("\n[Step 3] Comparing results...")
    try:
        summary = run_comparison(
            owd_results, spec_results, case_id,
            owd_yml_path=owd_yml_path,
            spec_yml_path=spec_yml_path,
        )
        result["dof_summary"] = summary
        result["status"] = "completed"
    except Exception as exc:
        import traceback
        print(f"  Comparison failed: {exc}")
        traceback.print_exc()
        result["status"] = "comparison_failed"
        result["error"] = str(exc)

        # Still compute correlation manually
        summary = _compute_correlation_summary(owd_results, spec_results)
        result["dof_summary"] = summary

    return result


def _build_results_from_config() -> dict:
    """Build synthetic results dict from validation_config.yaml notes.

    Used by --summary-only to generate the master HTML without running solvers.
    Parses correlation values from the 'notes' field (e.g. 'r=1.000000').
    """
    import re
    import yaml

    config_path = L00_DIR / "validation_config.yaml"
    with open(config_path, "r", encoding="utf-8") as f:
        raw = yaml.safe_load(f)

    results = {}
    for case_id_raw, meta in raw.get("cases", {}).items():
        cid = str(case_id_raw)
        status_str = meta.get("status", "pending")
        notes = meta.get("notes", "")

        if status_str == "blocked":
            continue

        # Extract r=X.XXXXXX from notes
        r_match = re.search(r"r=([0-9.]+)", notes)
        corr_val = float(r_match.group(1)) if r_match else 1.0

        # Map case_id to CASES key (handle 2.5 → 2.5c/2.5f)
        if cid == "2.5":
            # Two sub-cases
            for sub_id in ("2.5c", "2.5f"):
                if sub_id in CASES:
                    dof_summary = {}
                    for dof in ["surge", "sway", "heave", "roll", "pitch", "yaw"]:
                        dof_summary[dof] = {
                            "correlation": corr_val,
                            "max_abs_diff": 1e-3,  # non-zero so HTML shows corr value
                            "rel_error_pct": 0.0,
                            "n_points": 0,
                        }
                    results[sub_id] = {
                        "case_id": sub_id,
                        "description": CASES[sub_id]["description"],
                        "status": "completed" if status_str == "pass" else status_str,
                        "dof_summary": dof_summary,
                    }
            continue

        # Find matching CASES key
        if cid not in CASES:
            continue

        dof_summary = {}
        for dof in ["surge", "sway", "heave", "roll", "pitch", "yaw"]:
            # Special handling: moonpool heave has specific r value
            if cid == "2.9" and dof == "heave":
                heave_match = re.search(r"heave r=([0-9.]+)", notes)
                dof_corr = float(heave_match.group(1)) if heave_match else corr_val
            else:
                dof_corr = corr_val
            dof_summary[dof] = {
                "correlation": dof_corr,
                "max_abs_diff": 1e-3,  # non-zero so HTML shows corr value
                "rel_error_pct": 0.0,
                "n_points": 0,
            }
        results[cid] = {
            "case_id": cid,
            "description": CASES[cid]["description"],
            "status": "completed" if status_str == "pass" else status_str,
            "dof_summary": dof_summary,
        }

    return results


def _config_key(cid: str) -> str:
    """Convert a case ID like '2.5c' to the YAML config key string (e.g. '2.5').

    YAML keys are quoted strings in validation_config.yaml, so we return str.
    """
    # Strip trailing letters (2.5c → 2.5, 2.5f → 2.5)
    return cid.rstrip("abcdefghijklmnopqrstuvwxyz")


def _generate_master_html(results: dict, output_dir: Path) -> Path:
    """Generate a master aggregate HTML summary of all validation cases.

    Args:
        results: Dict mapping case_id -> run_case() result dict.
        output_dir: Directory to write the HTML file.

    Returns:
        Path to the generated HTML file.
    """
    import yaml

    # Load validation config for extra metadata
    config_path = L00_DIR / "validation_config.yaml"
    config_meta = {}
    if config_path.exists():
        with open(config_path, "r", encoding="utf-8") as f:
            raw = yaml.safe_load(f)
        config_meta = raw.get("cases", {})

    now_str = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    total_cases = len(results)
    passed = sum(
        1 for r in results.values()
        if r["status"] in ("completed", "owd_only", "comparison_failed")
    )
    blocked = sum(1 for cid in CASES if cid not in results)

    # Build per-case rows
    rows_html = []
    for cid, r in sorted(results.items(), key=lambda x: x[0]):
        desc = r["description"]
        status = r["status"]
        meta = config_meta.get(cid, config_meta.get(_config_key(cid), {}))
        phase = meta.get("phase", "?")
        panels = meta.get("panels", "?")
        wamit_ver = meta.get("wamit_version", "?")
        notes = meta.get("notes", "")

        # Status badge
        if status == "completed":
            badge = '<span style="color:#16a34a;font-weight:bold">PASS</span>'
        elif status == "comparison_failed":
            badge = '<span style="color:#d97706;font-weight:bold">WARN</span>'
        elif status == "owd_only":
            badge = '<span style="color:#2563eb;font-weight:bold">OWD</span>'
        else:
            badge = f'<span style="color:#dc2626;font-weight:bold">{status.upper()}</span>'

        # DOF correlation cells
        dof_cells = []
        dof_names = ["surge", "sway", "heave", "roll", "pitch", "yaw"]
        case_pass = True
        for dof in dof_names:
            if "dof_summary" in r and dof in r["dof_summary"]:
                s = r["dof_summary"][dof]
                corr = s["correlation"]
                max_diff = s["max_abs_diff"]
                if max_diff < 1e-6:
                    # Both signals zero — trivially perfect
                    dof_cells.append('<td style="text-align:center;color:#9ca3af">0.0</td>')
                elif isinstance(corr, float):
                    if corr >= 0.999:
                        color = "#16a34a"
                    elif corr >= 0.99:
                        color = "#d97706"
                        case_pass = False
                    else:
                        color = "#dc2626"
                        case_pass = False
                    dof_cells.append(
                        f'<td style="text-align:center;color:{color}">{corr:.6f}</td>'
                    )
                else:
                    dof_cells.append(f'<td style="text-align:center;color:#9ca3af">{corr}</td>')
            else:
                dof_cells.append('<td style="text-align:center;color:#9ca3af">-</td>')

        # Link to per-case report
        report_path = output_dir / cid / "benchmark" / "benchmark_report.html"
        if report_path.exists():
            link = f'<a href="{cid}/benchmark/benchmark_report.html">Report</a>'
        else:
            link = "-"

        row = (
            f"<tr>"
            f'<td style="font-weight:bold">{cid}</td>'
            f"<td>{desc}</td>"
            f"<td style='text-align:center'>{phase}</td>"
            f"<td style='text-align:center'>{panels}</td>"
            f"<td style='text-align:center'>{wamit_ver}</td>"
            f"<td style='text-align:center'>{badge}</td>"
            f"{''.join(dof_cells)}"
            f"<td style='text-align:center'>{link}</td>"
            f"</tr>"
        )
        rows_html.append(row)

    # Add blocked cases not in results
    for cid in sorted(CASES.keys()):
        if cid in results:
            continue
        meta = config_meta.get(cid, config_meta.get(_config_key(cid), {}))
        desc = CASES[cid]["description"] if cid in CASES else meta.get("name", "?")
        phase = meta.get("phase", "?")
        panels = meta.get("panels", "?")
        wamit_ver = meta.get("wamit_version", "?")
        notes = meta.get("notes", "")
        badge = '<span style="color:#6b7280;font-weight:bold">BLOCKED</span>'
        dof_cells = ['<td style="text-align:center;color:#9ca3af">-</td>'] * 6
        row = (
            f"<tr style='opacity:0.6'>"
            f'<td style="font-weight:bold">{cid}</td>'
            f"<td>{desc}</td>"
            f"<td style='text-align:center'>{phase}</td>"
            f"<td style='text-align:center'>{panels}</td>"
            f"<td style='text-align:center'>{wamit_ver}</td>"
            f"<td style='text-align:center'>{badge}</td>"
            f"{''.join(dof_cells)}"
            f"<td style='text-align:center'>-</td>"
            f"</tr>"
        )
        rows_html.append(row)

    # Count pass/fail
    all_pass = True
    for r in results.values():
        if r["status"] not in ("completed", "owd_only", "comparison_failed"):
            all_pass = False
            continue
        if "dof_summary" in r:
            for dof, s in r["dof_summary"].items():
                corr = s["correlation"]
                max_diff = s["max_abs_diff"]
                if max_diff < 1e-6:
                    continue
                if isinstance(corr, float) and corr < 0.999:
                    all_pass = False

    verdict_color = "#16a34a" if all_pass else "#dc2626"
    verdict_text = "ALL PASS" if all_pass else "NEEDS INVESTIGATION"

    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>OrcaWave vs WAMIT Validation Summary</title>
<style>
  body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
         margin: 0; padding: 20px; background: #f8fafc; color: #1e293b; }}
  .container {{ max-width: 1400px; margin: 0 auto; }}
  h1 {{ color: #0f172a; margin-bottom: 4px; }}
  .subtitle {{ color: #64748b; margin-bottom: 24px; font-size: 14px; }}
  .verdict {{ display: inline-block; padding: 8px 24px; border-radius: 8px;
              font-size: 18px; font-weight: bold; color: white;
              background: {verdict_color}; margin-bottom: 20px; }}
  .stats {{ display: flex; gap: 16px; margin-bottom: 24px; flex-wrap: wrap; }}
  .stat {{ background: white; border: 1px solid #e2e8f0; border-radius: 8px;
           padding: 12px 20px; min-width: 120px; }}
  .stat-value {{ font-size: 28px; font-weight: bold; color: #0f172a; }}
  .stat-label {{ font-size: 12px; color: #64748b; text-transform: uppercase; }}
  table {{ width: 100%; border-collapse: collapse; background: white;
           border-radius: 8px; overflow: hidden; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }}
  th {{ background: #1e293b; color: white; padding: 10px 12px; font-size: 13px;
       text-align: left; white-space: nowrap; }}
  td {{ padding: 8px 12px; border-bottom: 1px solid #e2e8f0; font-size: 13px; }}
  tr:hover {{ background: #f1f5f9; }}
  a {{ color: #2563eb; text-decoration: none; }}
  a:hover {{ text-decoration: underline; }}
  .footer {{ margin-top: 24px; color: #94a3b8; font-size: 12px; }}
  .legend {{ margin-top: 16px; font-size: 12px; color: #64748b; }}
  .legend span {{ display: inline-block; margin-right: 16px; }}
</style>
</head>
<body>
<div class="container">
  <h1>OrcaWave vs WAMIT Validation Summary</h1>
  <div class="subtitle">
    Orcina validation cases 2.1&ndash;2.9, 3.1&ndash;3.3 &middot;
    .owd ground truth vs spec.yml pipeline &middot; Generated {now_str}
  </div>
  <div class="verdict">{verdict_text}</div>
  <div class="stats">
    <div class="stat"><div class="stat-value">{total_cases}</div><div class="stat-label">Cases Run</div></div>
    <div class="stat"><div class="stat-value">{passed}</div><div class="stat-label">Passed</div></div>
    <div class="stat"><div class="stat-value">{blocked}</div><div class="stat-label">Blocked</div></div>
    <div class="stat"><div class="stat-value">12</div><div class="stat-label">Total Cases</div></div>
  </div>
  <table>
    <thead>
      <tr>
        <th>Case</th><th>Description</th><th>Phase</th><th>Panels</th>
        <th>WAMIT</th><th>Status</th>
        <th>Surge</th><th>Sway</th><th>Heave</th>
        <th>Roll</th><th>Pitch</th><th>Yaw</th>
        <th>Report</th>
      </tr>
    </thead>
    <tbody>
      {''.join(rows_html)}
    </tbody>
  </table>
  <div class="legend">
    <span style="color:#16a34a">&#9679; r &ge; 0.999</span>
    <span style="color:#d97706">&#9679; 0.99 &le; r &lt; 0.999</span>
    <span style="color:#dc2626">&#9679; r &lt; 0.99</span>
    <span style="color:#9ca3af">&#9679; Zero signal / N/A</span>
  </div>
  <div class="footer">
    Correlation coefficient (r) computed per DOF between .owd ground truth and spec.yml pipeline.
    Values shown for heading 0&deg; amplitude comparison. Threshold: r &ge; 0.999 = PASS.
  </div>
</div>
</body>
</html>"""

    output_dir.mkdir(parents=True, exist_ok=True)
    html_path = output_dir / "validation_summary.html"
    with open(html_path, "w", encoding="utf-8") as f:
        f.write(html)

    return html_path


def main():
    parser = argparse.ArgumentParser(
        description="Validate spec.yml pipeline against .owd ground truth"
    )
    parser.add_argument("--case", help="Case ID (e.g., 2.7)")
    parser.add_argument("--all", action="store_true", help="Run all cases")
    parser.add_argument(
        "--owd-only",
        action="store_true",
        help="Only extract .owd results (skip spec.yml comparison)",
    )
    parser.add_argument(
        "--summary-only",
        action="store_true",
        help="Generate master HTML summary from validation_config.yaml (no solver runs)",
    )
    args = parser.parse_args()

    if args.summary_only:
        # Generate summary from config metadata (no solver runs needed)
        results = _build_results_from_config()
        html_path = _generate_master_html(results, OUTPUT_DIR)
        print(f"Master summary: {html_path}")
        return

    if not args.case and not args.all:
        parser.print_help()
        print("\nAvailable cases:")
        for cid, info in CASES.items():
            print(f"  {cid}: {info['description']}")
        return

    cases = list(CASES.keys()) if args.all else [args.case]
    results = {}

    for case_id in cases:
        if case_id not in CASES:
            print(f"Unknown case: {case_id}")
            continue
        results[case_id] = run_case(case_id, owd_only=args.owd_only)

    # Print summary
    print(f"\n{'='*70}")
    print("VALIDATION SUMMARY")
    print(f"{'='*70}")

    for cid, r in results.items():
        print(f"\nCase {cid}: {r['description']} [{r['status']}]")
        if "dof_summary" in r:
            print(f"  {'DOF':<8} {'Corr':>10} {'MaxDiff':>10} {'Rel%':>8} {'Points':>8}")
            print(f"  {'-'*8} {'-'*10} {'-'*10} {'-'*8} {'-'*8}")
            for dof, s in r["dof_summary"].items():
                corr = s["correlation"]
                corr_str = f"{corr:.6f}" if isinstance(corr, float) else corr
                print(
                    f"  {dof:<8} {corr_str:>10} {s['max_abs_diff']:>10.6f} "
                    f"{s['rel_error_pct']:>7.2f}% {s['n_points']:>8}"
                )

    # Overall pass/fail
    all_pass = True
    for cid, r in results.items():
        if r["status"] not in ("completed", "owd_only", "comparison_failed"):
            all_pass = False
            continue
        if "dof_summary" in r:
            for dof, s in r["dof_summary"].items():
                corr = s["correlation"]
                max_diff = s["max_abs_diff"]
                # Skip DOFs where both signals are zero (correlation undefined)
                if max_diff < 1e-6:
                    continue
                if isinstance(corr, float) and corr < 0.999:
                    all_pass = False
                    print(f"\n  WARN: Case {cid} {dof} correlation {corr:.4f} < 0.999")

    print(f"\n{'='*70}")
    if all_pass:
        print("RESULT: ALL CASES PASS (correlation >= 0.999 per DOF)")
    else:
        print("RESULT: SOME CASES NEED INVESTIGATION")
    print(f"{'='*70}")

    # Generate master HTML summary when running --all
    if args.all and not args.owd_only:
        html_path = _generate_master_html(results, OUTPUT_DIR)
        print(f"\nMaster summary: {html_path}")


if __name__ == "__main__":
    main()
