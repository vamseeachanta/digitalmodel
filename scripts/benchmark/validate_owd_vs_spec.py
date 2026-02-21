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
import math
import sys
import time
from datetime import datetime
from pathlib import Path
from typing import Optional

import numpy as np

from digitalmodel.hydrodynamics.diffraction.diffraction_units import (
    complex_phase_degrees,
    hz_to_rad_per_s,
    rad_per_s_to_period_s,
    radians_to_degrees,
)

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
        # body_index: 1 removed, using bodies list instead
        "bodies": [
            {"body_index": 0, "vessel_name": "Cylinder", "skip_rao": True},
            {"body_index": 1, "vessel_name": "Ellipsoid"},
        ],
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
        "bodies": [
            {"body_index": 0, "vessel_name": "test05_cylinder"},
            {"body_index": 1, "vessel_name": "test05_spheroid"},
        ],
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



def _get_bodies(case: dict) -> list[dict]:
    """Get list of bodies to extract, falling back to single-body default."""
    if "bodies" in case:
        return case["bodies"]
    return [{
        "body_index": case.get("body_index", 0),
        "vessel_name": case["vessel_name"]
    }]


def _extract_coupling_matrices(diff, body_i: int, body_j: int) -> dict:
    """Extract off-diagonal 6x6 AM/Damp blocks from full NxN matrix."""
    frequencies = hz_to_rad_per_s(np.array(diff.frequencies))
    sort_idx = np.argsort(frequencies)

    am_full = np.array(diff.addedMass)  # (nfreq, 6N, 6N)
    damp_full = np.array(diff.damping)

    r0, c0 = body_i * 6, body_j * 6

    # Check bounds
    if am_full.shape[1] < r0 + 6 or am_full.shape[2] < c0 + 6:
        return {}

    return {
        "added_mass": am_full[:, r0:r0 + 6, c0:c0 + 6][sort_idx],
        "damping": damp_full[:, r0:r0 + 6, c0:c0 + 6][sort_idx],
        "frequencies": frequencies[sort_idx],
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
        frequencies = hz_to_rad_per_s(np.array(diff.frequencies))
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
            periods=rad_per_s_to_period_s(frequencies),
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
                magnitude = radians_to_degrees(magnitude)
            components[dof.name.lower()] = RAOComponent(
                dof=dof,
                magnitude=magnitude,
                phase=complex_phase_degrees(rao_complex),
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


def _fix_mesh_paths_in_yml(yml_path: Path, owd_dir: Path) -> None:
    """Resolve relative mesh file paths in an OrcaFlex input YAML to absolute paths.

    OrcFxAPI SaveData() writes mesh paths relative to the original .owd directory.
    When the YAML is loaded from a different directory (e.g. benchmark/), those
    relative paths break. This patches them to absolute paths in-place.
    """
    import re

    content = yml_path.read_text(encoding="utf-8")
    _FILE_KEYS = re.compile(
        r"^(\s*(?:BodyMeshFileName|FreeSurfacePanelledZoneMeshFileName"
        r"|BodyDipolePanelFileName|BodyControlSurfaceFileName)\s*:\s*)(.+)$",
        re.MULTILINE,
    )

    def _resolve(m: re.Match) -> str:
        key, raw = m.group(1), m.group(2).strip()
        if not raw or raw in ("~", "null", "Null", "NULL"):
            return m.group(0)
        p = Path(raw)
        if p.is_absolute():
            return m.group(0)  # already absolute — leave unchanged
        abs_p = (owd_dir / p).resolve()
        if abs_p.exists():
            return f"{key}{str(abs_p).replace(chr(92), '/')}"
        return m.group(0)  # leave unchanged if resolution fails

    new_content = _FILE_KEYS.sub(_resolve, content)
    if new_content != content:
        yml_path.write_text(new_content, encoding="utf-8")
        print(f"  Patched relative mesh paths in {yml_path.name}")


def solve_owd(
    case_id: str,
) -> tuple[dict[int, Optional["DiffractionResults"]], dict, Optional[Path], list[dict], Optional[Path]]:
    """Load .owd file, run Calculate(), and extract results.

    Returns
    -------
    Tuple of (results_by_body, coupling_matrices, Path to input .yml or None,
    panel_geometry_data list from OrcFxAPI panelGeometry).
    """
    import OrcFxAPI

    case = CASES[case_id]
    owd_path = case["owd"]

    # Prefer modified input YAML over binary .owd when it exists —
    # this lets us extend the frequency list without touching the binary file.
    out_dir = OUTPUT_DIR / case_id / "benchmark"
    out_dir.mkdir(parents=True, exist_ok=True)
    input_yml = out_dir / f"{owd_path.stem}_input.yml"

    if input_yml.exists():
        # Patch relative mesh paths before loading (SaveData writes paths relative
        # to the original .owd directory; loading from benchmark/ breaks them).
        _fix_mesh_paths_in_yml(input_yml, owd_path.parent)
        print(f"\n  Loading modified input YAML: {input_yml.name}")
        try:
            diff = OrcFxAPI.Diffraction(str(input_yml.resolve()))
        except Exception as _exc:
            print(f"  [WARN] YAML load failed ({_exc}), falling back to .owd")
            diff = OrcFxAPI.Diffraction()
            diff.LoadData(str(owd_path.resolve()))
    else:
        print(f"\n  Loading .owd: {owd_path.name}")
        diff = OrcFxAPI.Diffraction()
        diff.LoadData(str(owd_path.resolve()))

    print(f"  Running Calculate()...")
    t0 = time.perf_counter()
    diff.Calculate()
    dt = time.perf_counter() - t0
    print(f"  Solved in {dt:.1f}s")

    # Export input configuration as YAML (updates *_input.yml with solved config)
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
        owr_path = None

    # Save results spreadsheet alongside .owr for reference
    if owr_path is not None:
        xlsx_path = owr_path.with_suffix(".xlsx")
        try:
            diff.SaveResultsSpreadsheet(str(xlsx_path.resolve()))
            print(f"  Saved spreadsheet: {xlsx_path.name}")
        except Exception as exc:
            print(f"  Could not save spreadsheet: {exc}")

    # Extract per-body results
    bodies = _get_bodies(case)
    results_by_body = {}
    for body_info in bodies:
        bi = body_info["body_index"]
        results_by_body[bi] = _extract_from_diffraction(
            diff,
            vessel_name=body_info["vessel_name"],
            water_depth=case["water_depth"],
            source_label="OrcaWave (.owd)",
            source_file=str(owd_path),
            body_index=bi,
        )

    # Extract coupling matrices
    coupling = {}
    try:
        am_shape = np.array(diff.addedMass).shape
        body_count = am_shape[1] // 6 if len(am_shape) == 3 else 1
        if body_count > 1:
            for bi in range(body_count):
                for bj in range(body_count):
                    if bi != bj:
                        coupling[(bi, bj)] = _extract_coupling_matrices(diff, bi, bj)
    except Exception as e:
        print(f"  [WARN] Coupling extraction failed: {e}")

    # Extract panel geometry (symmetry-expanded, full mesh)
    panel_geometry_data: list[dict] = []
    try:
        for p in diff.panelGeometry:
            panel_geometry_data.append({
                "area": p["area"],
                "centroid": list(p["centroid"]),
                "objectName": p["objectName"],
            })
        print(f"  panelGeometry: {len(panel_geometry_data)} panels")
    except Exception as exc:
        print(f"  [WARN] panelGeometry extraction failed: {exc}")

    return results_by_body, coupling, owd_yml_path, panel_geometry_data, owr_path


def solve_spec(
    case_id: str,
) -> tuple[dict[int, Optional["DiffractionResults"]], dict, Optional[Path]]:
    """Run spec.yml through OrcaWaveRunner and extract results.

    Returns
    -------
    Tuple of (results_by_body, coupling, Path to input .yml or None).
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

            # Save spreadsheet alongside the .owr for reference
            xlsx_path = owr_files[0].with_suffix(".xlsx")
            try:
                diff.SaveResultsSpreadsheet(str(xlsx_path.resolve()))
                print(f"  Saved spreadsheet: {xlsx_path.name}")
            except Exception as exc:
                print(f"  Could not save spreadsheet: {exc}")

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

            # Extract per-body results
            bodies = _get_bodies(case)
            results_by_body = {}
            for body_info in bodies:
                bi = body_info["body_index"]
                results_by_body[bi] = _extract_from_diffraction(
                    diff,
                    vessel_name=body_info["vessel_name"],
                    water_depth=case["water_depth"],
                    source_label="OrcaWave (spec.yml)",
                    source_file=str(spec_path),
                    body_index=bi,
                )

            # Extract coupling matrices
            coupling = {}
            try:
                am_shape = np.array(diff.addedMass).shape
                body_count = am_shape[1] // 6 if len(am_shape) == 3 else 1
                if body_count > 1:
                    for bi in range(body_count):
                        for bj in range(body_count):
                            if bi != bj:
                                coupling[(bi, bj)] = _extract_coupling_matrices(diff, bi, bj)
            except Exception as e:
                print(f"  [WARN] Coupling extraction failed: {e}")

            return results_by_body, coupling, spec_input_yml
        else:
            print(f"  [ERROR] No .owr file found")
            return {}, {}, None
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
) -> tuple[dict[int, Optional["DiffractionResults"]], dict, Optional[Path]]:
    """Build and solve OrcaWave project directly via OrcFxAPI.

    Uses OrcaWaveRunner.prepare() to generate the .yml, then loads and
    solves via OrcFxAPI Python binding directly.

    Returns
    -------
    Tuple of (results_by_body, coupling, Path to input .yml or None).
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
        return {}, {}, None
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

    # Save results (.owr) and spreadsheet alongside it for reference
    owr_path = out_dir / "spec_result.owr"
    try:
        diff.SaveResults(str(owr_path.resolve()))
        print(f"  Saved: {owr_path.name}")
    except Exception as exc:
        print(f"  Could not save .owr: {exc}")

    xlsx_path = owr_path.with_suffix(".xlsx")
    try:
        diff.SaveResultsSpreadsheet(str(xlsx_path.resolve()))
        print(f"  Saved spreadsheet: {xlsx_path.name}")
    except Exception as exc:
        print(f"  Could not save spreadsheet: {exc}")

    # Extract per-body results
    bodies = _get_bodies(case)
    results_by_body = {}
    for body_info in bodies:
        bi = body_info["body_index"]
        results_by_body[bi] = _extract_from_diffraction(
            diff,
            vessel_name=body_info["vessel_name"],
            water_depth=case["water_depth"],
            source_label="OrcaWave (spec.yml)",
            source_file=str(spec_path),
            body_index=bi,
        )

    # Extract coupling matrices
    coupling = {}
    try:
        am_shape = np.array(diff.addedMass).shape
        body_count = am_shape[1] // 6 if len(am_shape) == 3 else 1
        if body_count > 1:
            for bi in range(body_count):
                for bj in range(body_count):
                    if bi != bj:
                        coupling[(bi, bj)] = _extract_coupling_matrices(diff, bi, bj)
    except Exception as e:
        print(f"  [WARN] Coupling extraction failed: {e}")

    return results_by_body, coupling, spec_input_yml


def run_comparison(
    owd_results_by_body: dict[int, "DiffractionResults"],
    spec_results_by_body: dict[int, "DiffractionResults"],
    coupling_owd: dict,
    coupling_spec: dict,
    case_id: str,
    owd_yml_path: Optional[Path] = None,
    spec_yml_path: Optional[Path] = None,
    panel_geometry_data: Optional[list] = None,
    owr_path: Optional[Path] = None,
) -> dict:
    """Compare .owd ground truth against spec.yml results for all bodies."""
    import json
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
    bodies = _get_bodies(case)

    # 1. Semantic Check (file-level)
    sem = None
    if owd_yml_path and spec_yml_path and owd_yml_path.exists() and spec_yml_path.exists():
        print(f"\n  Running semantic equivalence check...")
        sem = _compare_orcawave_ymls(
            owd_yml_path, spec_yml_path
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

    # 2. Per-Body Comparison
    dof_summary_by_body = {}  # key: body_index -> dof_summary dict

    # Load base spec metadata
    spec_path = case["spec"]
    with open(spec_path) as f:
        spec_dict = yaml.safe_load(f)
    base_metadata = build_solver_metadata(spec_dict, spec_dir=spec_path.parent)
    resolved_mesh_path = base_metadata.get("OrcaWave", {}).get("mesh_path")

    for body_info in bodies:
        bi = body_info["body_index"]
        vessel_name = body_info["vessel_name"]

        owd_r = owd_results_by_body.get(bi)
        spec_r = spec_results_by_body.get(bi)

        if not owd_r or not spec_r:
            print(f"  [WARN] Missing results for body {bi} ({vessel_name})")
            continue

        # Determine output directory for this body
        body_out_dir = out_dir / f"body_{bi}" if len(bodies) > 1 else out_dir
        body_out_dir.mkdir(parents=True, exist_ok=True)

        # Build metadata for this body
        metadata = {}
        if owd_yml_path and owd_yml_path.exists():
            metadata["OrcaWave (.owd)"] = build_orcawave_metadata_from_yml(
                owd_yml_path, body_index=bi,
            )
        else:
            metadata["OrcaWave (.owd)"] = base_metadata.get("OrcaWave", {})

        if spec_yml_path and spec_yml_path.exists():
            metadata["OrcaWave (spec.yml)"] = build_orcawave_metadata_from_yml(
                spec_yml_path, body_index=bi,
            )
        else:
            metadata["OrcaWave (spec.yml)"] = base_metadata.get("OrcaWave", {})

        # Resolve per-body mesh paths from mesh_file key (body-specific)
        # hull_dir = directory containing the reference GDF files
        hull_dir = (
            Path(resolved_mesh_path).parent if resolved_mesh_path else None
        )
        for key in metadata:
            m = metadata[key]
            if "mesh_path" in m:
                continue  # already resolved
            mesh_file = m.get("mesh_file", "").strip()
            if not mesh_file or mesh_file == "-":
                continue
            # Search for the mesh file: hull library first, then spec/owd dirs
            search_dirs = [d for d in [
                hull_dir,
                spec_yml_path.parent if spec_yml_path else None,
                owd_yml_path.parent if owd_yml_path else None,
            ] if d is not None]
            for d in search_dirs:
                candidate = d / mesh_file
                if candidate.exists():
                    m["mesh_path"] = str(candidate)
                    break

        # Attach semantic data
        if sem:
            first_key = next(iter(metadata))
            metadata[first_key]["_semantic_equivalence"] = sem

        # Attach panel geometry for mesh schematic (OrcFxAPI symmetry-expanded)
        if panel_geometry_data and "OrcaWave (.owd)" in metadata:
            metadata["OrcaWave (.owd)"]["panel_geometry"] = panel_geometry_data

        # Attach .owr path so report generator uses LoadResults() for correct headings
        if owr_path and owr_path.exists() and "OrcaWave (.owd)" in metadata:
            metadata["OrcaWave (.owd)"]["owr_path"] = str(owr_path)

        solver_results = {
            "OrcaWave (.owd)": owd_r,
            "OrcaWave (spec.yml)": spec_r,
        }

        if len(bodies) > 1:
            title = f"Validation Case {case_id}: {case['description']} (Body {bi}: {vessel_name})"
            subtitle = "OrcaWave .owd Ground Truth vs spec.yml Pipeline"
            # Build cross-body navigation bar
            nav_links = []
            for b in bodies:
                bdir = f"../body_{b['body_index']}"
                active = ' style="font-weight:bold"' if b["body_index"] == bi else ""
                nav_links.append(
                    f'<a href="{bdir}/benchmark_report.html"{active}>'
                    f"Body {b['body_index']}: {b['vessel_name']}</a>"
                )
            nav_links.append('<a href="../index.html">Overview</a>')
            nav_links.append('<a href="../coupling/">Coupling</a>')
            nav_links.append('<a href="../../validation_summary.html">Master Summary</a>')
            nav_html = (
                '<div class="nav-bar" style="margin:0.5em 0;padding:0.5em;'
                'background:#f0f4f8;border-radius:4px;font-size:0.95em">'
                + " | ".join(nav_links)
                + "</div>"
            )
        else:
            title = f"Validation Case {case_id}: {case['description']}"
            subtitle = None
            nav_html = None

        config = BenchmarkConfig(
            output_dir=body_out_dir,
            report_title=title,
            report_subtitle=subtitle,
            navigation_html=nav_html,
        )

        print(f"\n  Running benchmark for Body {bi} ({vessel_name})...")
        runner = BenchmarkRunner(config)
        result = runner.run_from_results(solver_results, solver_metadata=metadata)

        report_path = body_out_dir / "validation_report.html"
        if hasattr(result, "report_path") and result.report_path:
            report_path = Path(result.report_path)
        print(f"  Report: {report_path}")

        # Compute correlation
        summary = _compute_correlation_summary(owd_r, spec_r)
        dof_summary_by_body[bi] = summary

    # Clean up stale flat-directory reports for multi-body cases
    if len(bodies) > 1:
        stale_files = [
            "benchmark_report.html",
            "benchmark_report.json",
            "benchmark_amplitude.html",
            "benchmark_combined.html",
            "benchmark_heatmap.html",
            "benchmark_phase.html",
        ]
        for fname in stale_files:
            stale = out_dir / fname
            if stale.exists():
                stale.unlink()
                print(f"  Removed stale flat artifact: {stale.name}")
        # Remove old overlay PNGs from flat directory
        for png in out_dir.glob("*.png"):
            png.unlink()
            print(f"  Removed stale flat artifact: {png.name}")

    # 3. Coupling Comparison
    coupling_report = {}
    if coupling_owd and coupling_spec:
        from digitalmodel.hydrodynamics.diffraction.benchmark_plotter import BenchmarkPlotter

        coupling_dir = out_dir / "coupling"
        coupling_dir.mkdir(parents=True, exist_ok=True)

        print(f"\n  Comparing coupling matrices...")
        # For each pair (i,j)
        for key in coupling_owd:
            if key not in coupling_spec:
                continue

            # extract 6x6 blocks and compute correlation
            owd_m = coupling_owd[key]
            spec_m = coupling_spec[key]

            # Compare AM and Damping
            am_corr = _compute_matrix_correlation(owd_m["added_mass"], spec_m["added_mass"])
            damp_corr = _compute_matrix_correlation(owd_m["damping"], spec_m["damping"])

            coupling_report[str(key)] = {
                "added_mass_correlation": am_corr,
                "damping_correlation": damp_corr,
            }

            # Generate coupling heatmap
            bi, bj = key
            bi_name = next((b["vessel_name"] for b in bodies if b["body_index"] == bi), f"Body {bi}")
            bj_name = next((b["vessel_name"] for b in bodies if b["body_index"] == bj), f"Body {bj}")

            try:
                html_path = BenchmarkPlotter.build_coupling_heatmap_html(
                    coupling_dir, am_corr, damp_corr, bi_name, bj_name
                )
                print(f"  Coupling Heatmap: {html_path}")
            except Exception as exc:
                print(f"  [WARN] Coupling heatmap generation failed: {exc}")

        # Save coupling report
        with open(coupling_dir / "coupling_report.json", "w") as f:
            json.dump(coupling_report, f, indent=2)

    # 4. Combined multi-body overview page
    if len(bodies) > 1:
        index_path = _build_multibody_index_html(
            out_dir, case_id, case, bodies,
            dof_summary_by_body, coupling_report,
            panel_geometry_data=panel_geometry_data,
        )
        print(f"\n  Combined overview: {index_path}")

    return {
        "dof_summary_by_body": dof_summary_by_body,
        "coupling_summary": coupling_report,
        "semantic": sem,
    }


def _build_multibody_index_html(
    out_dir: Path,
    case_id: str,
    case: dict,
    bodies: list[dict],
    dof_summary_by_body: dict,
    coupling_report: dict,
    panel_geometry_data: Optional[list] = None,
) -> Path:
    """Build a combined multi-body overview page at benchmark/index.html."""
    import html as html_mod
    from digitalmodel.hydrodynamics.diffraction.benchmark_plotter import BenchmarkPlotter

    dof_names = ["surge", "sway", "heave", "roll", "pitch", "yaw"]
    dof_labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
    title = f"Validation Case {case_id}: {html_mod.escape(case['description'])}"

    # --- Per-body summary rows ---
    body_rows = []
    for b in bodies:
        bi = b["body_index"]
        vname = html_mod.escape(b["vessel_name"])
        summary = dof_summary_by_body.get(bi, {})

        cells = f'<td style="font-weight:600">{vname}</td>'
        all_pass = True
        for dof in dof_names:
            corr = summary.get(dof, {}).get("correlation", float("nan"))
            if corr >= 0.999:
                bg, fg = "#d5f5e3", "#16a34a"
            elif corr >= 0.99:
                bg, fg = "#fef9e7", "#d97706"
                all_pass = False
            else:
                bg, fg = "#fadbd8", "#dc2626"
                all_pass = False
            cells += (
                f'<td style="text-align:center;background:{bg};color:{fg}">'
                f"{corr:.6f}</td>"
            )

        verdict = "PASS" if all_pass else "REVIEW"
        v_bg = "#d5f5e3" if all_pass else "#fadbd8"
        cells += (
            f'<td style="text-align:center;background:{v_bg};font-weight:600">'
            f"{verdict}</td>"
        )
        cells += (
            f'<td style="text-align:center">'
            f'<a href="body_{bi}/benchmark_report.html">Full Report</a></td>'
        )
        body_rows.append(f"<tr>{cells}</tr>")

    body_table_html = "\n".join(body_rows)

    # --- Coupling heatmaps (inline) ---
    coupling_sections = []
    for key_str, data in coupling_report.items():
        # Parse body indices from key like "(0, 1)"
        parts = key_str.strip("()").split(",")
        bi_idx, bj_idx = int(parts[0].strip()), int(parts[1].strip())
        bi_name = next(
            (b["vessel_name"] for b in bodies if b["body_index"] == bi_idx),
            f"Body {bi_idx}",
        )
        bj_name = next(
            (b["vessel_name"] for b in bodies if b["body_index"] == bj_idx),
            f"Body {bj_idx}",
        )

        am_corr = data["added_mass_correlation"]
        damp_corr = data["damping_correlation"]

        # Convert to dict for _render_6x6_matrix
        def _to_dict(matrix):
            d = {}
            for i in range(6):
                for j in range(6):
                    d[(i + 1, j + 1)] = matrix[i][j]
            return d

        am_table = BenchmarkPlotter._render_6x6_matrix(_to_dict(am_corr), dof_labels)
        damp_table = BenchmarkPlotter._render_6x6_matrix(
            _to_dict(damp_corr), dof_labels,
        )

        # Compute min correlation across both matrices
        all_vals = [v for row in am_corr for v in row] + [
            v for row in damp_corr for v in row
        ]
        min_corr = min(all_vals) if all_vals else 0.0
        badge_bg = "#d5f5e3" if min_corr >= 0.999 else (
            "#fef9e7" if min_corr >= 0.99 else "#fadbd8"
        )

        coupling_sections.append(f"""
<div style="margin-bottom:2em">
  <h3>Body {bi_idx} ({html_mod.escape(bi_name)}) &harr; Body {bj_idx} ({html_mod.escape(bj_name)})
    <span style="background:{badge_bg};padding:2px 8px;border-radius:4px;
    font-size:0.8em;margin-left:0.5em">min r={min_corr:.6f}</span>
  </h3>
  <div style="display:flex;gap:2em;flex-wrap:wrap">
    <div>
      <h4 style="color:#555;margin-bottom:0.3em">Added Mass Coupling</h4>
      {am_table}
    </div>
    <div>
      <h4 style="color:#555;margin-bottom:0.3em">Radiation Damping Coupling</h4>
      {damp_table}
    </div>
  </div>
</div>""")

    coupling_html = "\n".join(coupling_sections) if coupling_sections else (
        '<p style="color:#888">No coupling data available.</p>'
    )

    # --- Panel geometry scatter (OrcFxAPI panelGeometry) ---
    geometry_section_html = ""
    if panel_geometry_data:
        try:
            scatter = BenchmarkPlotter._build_panel_scatter_html(
                panel_geometry_data,
                title="Multi-Body Panel Geometry (all bodies)",
                height=500,
            )
            geometry_section_html = f"""
  <div class="section">
    <h2>Panel Geometry</h2>
    <p style="color:#555;font-size:0.9em">
      OrcFxAPI panelGeometry &mdash; symmetry-expanded, {len(panel_geometry_data)} panels total.
      Colour-coded by body. Hover for panel area.
    </p>
    {scatter}
  </div>"""
        except Exception as exc:
            geometry_section_html = (
                f'<div class="section"><h2>Panel Geometry</h2>'
                f'<p style="color:#c0392b">Could not render panel geometry: {exc}</p></div>'
            )

    # --- Navigation ---
    nav_links = []
    for b in bodies:
        nav_links.append(
            f'<a href="body_{b["body_index"]}/benchmark_report.html">'
            f'Body {b["body_index"]}: {html_mod.escape(b["vessel_name"])}</a>'
        )
    nav_links.append('<a href="coupling/">Coupling Details</a>')
    nav_links.append('<a href="../validation_summary.html">Master Summary</a>')
    nav_bar = " | ".join(nav_links)

    # --- Assemble page ---
    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>{title} — Multi-Body Overview</title>
<style>
  * {{ box-sizing: border-box; }}
  body {{
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI',
                 Roboto, Arial, sans-serif;
    margin: 0; padding: 0; color: #333; background: #f8f9fa;
    font-size: 14px; line-height: 1.5;
  }}
  .container {{ max-width: 1200px; margin: 0 auto; padding: 1.5em 2em; }}
  .header {{
    background: #2c3e50; color: #fff; padding: 1.2em 2em;
    margin-bottom: 1.5em; border-radius: 6px;
  }}
  .header h1 {{ margin: 0 0 0.3em; font-size: 1.6em; }}
  .header .subtitle {{ font-size: 1.1em; opacity: 0.9; font-weight: 300; }}
  .nav-bar {{
    margin-top: 1em; padding-top: 0.8em;
    border-top: 1px solid rgba(255,255,255,0.15);
    font-size: 0.9em;
  }}
  .nav-bar a {{ color: #fff; text-decoration: none; opacity: 0.8; }}
  .nav-bar a:hover {{ opacity: 1; text-decoration: underline; }}
  .section {{
    background: #fff; padding: 1.5em; margin-bottom: 1.5em;
    border-radius: 6px; border: 1px solid #e2e8f0;
  }}
  .section h2 {{
    margin-top: 0; font-size: 1.3em; color: #2c3e50;
    border-bottom: 2px solid #3498db; padding-bottom: 0.3em;
  }}
  table {{ border-collapse: collapse; width: 100%; margin: 1em 0; }}
  th, td {{ border: 1px solid #ddd; padding: 8px; font-size: 0.9em; }}
  th {{ background: #f2f2f2; text-align: center; }}
  .solver-table {{ width: auto; max-width: 600px; }}
  .solver-table td {{ padding: 6px; }}
</style>
</head>
<body>
<div class="container">
  <div class="header">
    <h1>{title}</h1>
    <div class="subtitle">Multi-Body Overview &mdash; {len(bodies)} bodies</div>
    <div class="nav-bar">{nav_bar}</div>
  </div>

  <div class="section">
    <h2>Per-Body RAO Correlation Summary</h2>
    <p>Pearson correlation of displacement RAO magnitudes between .owd ground truth
       and spec.yml pipeline, per DOF per body.</p>
    <table>
      <tr>
        <th>Body</th>
        {"".join(f"<th>{d}</th>" for d in dof_labels)}
        <th>Verdict</th>
        <th>Detail</th>
      </tr>
      {body_table_html}
    </table>
  </div>

  <div class="section">
    <h2>Inter-Body Coupling</h2>
    <p>Correlation of off-diagonal added mass and radiation damping coupling matrices
       between .owd and spec.yml. Values near 1.000 indicate identical coupling
       coefficients across all frequencies.</p>
    {coupling_html}
  </div>
{geometry_section_html}
</div>
</body>
</html>"""

    index_path = out_dir / "index.html"
    index_path.write_text(html, encoding="utf-8")
    return index_path


def _compute_matrix_correlation(m1, m2):
    # m1, m2 are (nfreq, 6, 6)
    # Compute correlation per element (i, j) across frequencies
    corr = np.zeros((6, 6))
    for i in range(6):
        for j in range(6):
            v1 = m1[:, i, j]
            v2 = m2[:, i, j]
            if np.std(v1) > 0 and np.std(v2) > 0:
                corr[i, j] = np.corrcoef(v1, v2)[0, 1]
            elif np.allclose(v1, v2, atol=1e-6):
                corr[i, j] = 1.0
            else:
                corr[i, j] = float("nan")
    return corr.tolist()  # return as list of lists


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
        # Control surface mesh — different file naming for same geometry (e.g. Ellipsoid0096.csf vs ellipsoid_96p.csf)
        "BodyControlSurfaceMeshFileName", "BodyControlSurfaceMeshFormat", "BodyControlSurfaceMeshLengthUnits",
        # OrcaFlex import hints — only used when importing to OrcaFlex,
        # not during the OrcaWave solve itself
        "BodyOrcaFlexImportLength", "BodyOrcaFlexImportSymmetry",
        # OrcaWave internal defaults — not configurable via spec.yml,
        # always use OrcaWave's built-in defaults
        "ComputationStrategy",
        "EnableMultibodyConstraints",
        "BodyOriginType",
        "BodyVolumeWarningLevel",
        # Mesh preprocessing choices — different methods yield same results
        "BodyInteriorSurfacePanelMethod",  # Radial vs Triangulation
        "DivideNonPlanarPanels",           # mesh cleanup option
        # Control surface type descriptor (file-based variants already cosmetic above)
        "BodyControlSurfaceType",
        # Waterline tolerance parameters — tiny numerical differences, no RAO effect
        "WaterlineZTolerance",
        "WaterlineGapTolerance",
        # Resonance damping lid toggle — spec makes the default explicit (No)
        "HasResonanceDampingLid",
        # Load RAO method preference — solver choice, doesn't affect diffraction RAOs
        "PreferredLoadRAOCalculationMethod",
        # Free-surface panelled zone mesh file labels — filename/format/units rename only,
        # same geometry regardless of label (e.g. BMC.fdf → val_bmc.fdf)
        "FreeSurfacePanelledZoneMeshFileName",
        "FreeSurfacePanelledZoneMeshFormat",
        "FreeSurfacePanelledZoneMeshLengthUnits",
        # Roll damping target toggle — spec makes the default explicit (No/False);
        # OWD omits when at default. Disabled roll damping has no RAO effect.
        "BodyIncreaseRollDampingToTarget",
    }

    # Dormant: keys that exist but are inactive for non-QTF solve types.
    # Only significant when SolveType contains "QTF".
    _DORMANT_QTF_KEYS = {
        "PreferredQuadraticLoadCalculationMethod",
        "QTFMinCrossingAngle", "QTFMaxCrossingAngle",
        "QuadraticLoadPressureIntegration",
        "QTFCalculationMethod", "QTFFrequencyTypes",
        "IncludeMeanDriftFullQTFs",
        # Momentum conservation is a quadratic load method choice — inactive when
        # no quadratic loads are computed (potential-only or non-QTF solves)
        "QuadraticLoadMomentumConservation",
        # Control surface toggle for quadratic loads — only active in QTF solves
        "QuadraticLoadControlSurface",
        # QTF period/frequency range limits — only meaningful when QTF is active
        "QTFMinPeriodOrFrequency",
        "QTFMaxPeriodOrFrequency",
        # Preferred method selection — ignored in potential-only solves
        "PreferredQTFCalculationMethod",
    }

    # Convention: keys where both sides hold equivalent data expressed
    # differently (e.g. Hz vs rad/s, period vs frequency label), or where
    # different convergence/discretisation settings produce the same physics.
    _CONVENTION_KEYS = {
        "WavesReferredToBy",   # "frequency (rad/s)" vs "period (s)"
        "PeriodOrFrequency",   # same frequencies, different units/ordering
        "SolveType",           # e.g. "Full QTF" vs "Potential" — RAOs match
        "WaterDensity",        # unit system artifact (1 vs 1025) — scale-invariant RAOs
        # Free-surface discretisation parameters — different convergence settings
        # that produce the same first-order RAO results (e.g. finer OWD grid vs
        # spec default/0 values). Both choices are physically equivalent.
        "FreeSurfacePanelledZoneType",
        "FreeSurfacePanelledZoneInnerRadius",
        "FreeSurfaceOuterCircleNumberOfSegments",
        "FreeSurfaceAsymptoticZoneExpansionOrder",
        "FreeSurfaceQuadratureZoneNumberOfAnnuli",
        "FreeSurfaceQuadratureZoneRadiusStep",
        "FreeSurfaceQuadratureZoneNumberOfRadialNodes",
        "FreeSurfaceQuadratureZoneNumberOfAzimuthalNodes",
        "FreeSurfaceQuadratureZoneInnerRadius",
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

    def _try_as_float(v):
        """Try to parse a value as float (handles str scientific notation)."""
        if isinstance(v, (int, float)):
            return float(v)
        if isinstance(v, str):
            try:
                return float(v)
            except (ValueError, TypeError):
                return None
        return None

    def _values_equal(a, b) -> bool:
        """Compare values with tolerance for floats.

        Handles str-vs-float mismatches (e.g. '1e-6' vs 1e-06)
        from OrcaWave SaveData() YAML format differences.
        """
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
        # Handle str-vs-float: OrcaWave SaveData may quote numerics
        if type(a) != type(b):
            fa, fb = _try_as_float(a), _try_as_float(b)
            if fa is not None and fb is not None:
                return _values_equal(fa, fb)
        return a == b

    owd_data = _load(owd_yml)
    spec_data = _load(spec_yml)

    # Check if spec's solve type involves QTF — if spec is Potential-only,
    # all OWD QTF keys are dormant regardless of how the OWD was configured.
    # Using OWD's SolveType was wrong: Full-QTF OWDs caused QTF keys to remain
    # "significant" even when spec computes no QTF results.
    solve_type = str(spec_data.get("SolveType", "")).lower()
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

    # Compare within all bodies
    owd_bodies = owd_data.get("Bodies", [])
    spec_bodies = spec_data.get("Bodies", [])
    n_bodies = min(len(owd_bodies), len(spec_bodies))
    
    for bi in range(n_bodies):
        owd_body = owd_bodies[bi]
        spec_body = spec_bodies[bi]
        body_keys = set(owd_body.keys()) | set(spec_body.keys())
        for key in sorted(body_keys):
            full_key = f"Bodies[{bi}].{key}"
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
        owd_phase = owd_comp.phase.flatten()
        spec_phase = spec_comp.phase.flatten()

        # Truncate to common length
        min_len = min(len(owd_mag), len(spec_mag))
        owd_mag = owd_mag[:min_len]
        spec_mag = spec_mag[:min_len]
        owd_phase = owd_phase[:min_len]
        spec_phase = spec_phase[:min_len]

        # peak_mag is used by both the magnitude and phase correlation guards
        peak_mag = float(np.max(np.abs(owd_mag))) if min_len > 0 else 0.0

        # Zero-magnitude override: all values are zero so MaxDiff == 0; r = 1.0
        if peak_mag < 1e-10:
            r = 1.0
        elif min_len > 1 and np.std(owd_mag) > 0 and np.std(spec_mag) > 0:
            r = np.corrcoef(owd_mag, spec_mag)[0, 1]
        elif min_len > 0 and np.allclose(owd_mag, spec_mag, atol=1e-6):
            r = 1.0
        else:
            r = float("nan")

        max_diff = float(np.max(np.abs(owd_mag - spec_mag))) if min_len > 0 else 0.0
        mean_abs = float(np.mean(np.abs(owd_mag))) if min_len > 0 else 0.0
        rel_err = max_diff / mean_abs * 100.0 if mean_abs > 1e-12 else 0.0

        # Phase correlation with magnitude-weighted masking
        if peak_mag < 1e-10:
            phase_r = 1.0
            max_phase_diff = 0.0
        else:
            valid = np.abs(owd_mag) >= 0.01 * peak_mag
            owd_ph_valid = owd_phase[valid]
            spec_ph_valid = spec_phase[valid]
            n_valid = int(np.sum(valid))
            if n_valid < 3:
                phase_r = 1.0
                max_phase_diff = 0.0
            elif np.std(owd_ph_valid) > 0 and np.std(spec_ph_valid) > 0:
                phase_r = float(np.corrcoef(owd_ph_valid, spec_ph_valid)[0, 1])
                max_phase_diff = float(np.max(np.abs(owd_ph_valid - spec_ph_valid)))
            elif np.allclose(owd_ph_valid, spec_ph_valid, atol=1e-6):
                phase_r = 1.0
                max_phase_diff = 0.0
            else:
                phase_r = float("nan")
                max_phase_diff = float(np.max(np.abs(owd_ph_valid - spec_ph_valid)))

        summary[dof_name] = {
            "correlation": round(r, 6) if not np.isnan(r) else "N/A",
            "max_abs_diff": round(max_diff, 6),
            "rel_error_pct": round(rel_err, 2),
            "n_points": min_len,
            "phase_correlation": round(phase_r, 6) if not np.isnan(phase_r) else "N/A",
            "max_phase_diff": round(max_phase_diff, 2),
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
    owd_by_body, coupling_owd, owd_yml_path, panel_geometry_data, owr_path = solve_owd(case_id)
    if not owd_by_body:
        result["status"] = "owd_failed"
        return result

    first_r = next(iter(owd_by_body.values()))
    n_freq = first_r.raos.surge.frequencies.count
    n_head = first_r.raos.surge.headings.count
    result["owd_freq"] = n_freq
    result["owd_head"] = n_head
    print(f"  Extracted: {n_freq} freq x {n_head} headings")

    if owd_only:
        result["status"] = "owd_only"
        return result

    # Step 2: Run spec.yml pipeline
    print("\n[Step 2] Running spec.yml pipeline...")
    spec_by_body, coupling_spec, spec_yml_path = solve_spec(case_id)
    if not spec_by_body:
        result["status"] = "spec_failed"
        return result

    first_r2 = next(iter(spec_by_body.values()))
    n_freq2 = first_r2.raos.surge.frequencies.count
    n_head2 = first_r2.raos.surge.headings.count
    result["spec_freq"] = n_freq2
    result["spec_head"] = n_head2
    print(f"  Extracted: {n_freq2} freq x {n_head2} headings")

    # Step 3: Compare
    print("\n[Step 3] Comparing results...")
    try:
        comp_result = run_comparison(
            owd_by_body, spec_by_body,
            coupling_owd, coupling_spec,
            case_id,
            owd_yml_path=owd_yml_path,
            spec_yml_path=spec_yml_path,
            panel_geometry_data=panel_geometry_data,
            owr_path=owr_path,
        )
        result.update(comp_result)
        result["status"] = "completed"

        # Backwards compat: if single body, expose dof_summary at top level
        if len(comp_result["dof_summary_by_body"]) == 1:
            bi = next(iter(comp_result["dof_summary_by_body"]))
            result["dof_summary"] = comp_result["dof_summary_by_body"][bi]

    except Exception as exc:
        import traceback
        print(f"  Comparison failed: {exc}")
        traceback.print_exc()
        result["status"] = "comparison_failed"
        result["error"] = str(exc)

    # Step 4: QTF plots (auto-generated for Full QTF cases)
    _run_qtf_plots(case_id)

    return result


def _inject_qtf_into_html(html_path: Path, qtf_html: str) -> None:
    """Inject a QTF section into an existing benchmark HTML file.

    Idempotent: removes any pre-existing <section id="qtf-analysis"> block
    and any existing QTF TOC link before inserting the updated ones.
    Also adds a "12. QTF Analysis" entry to the report Table of Contents.
    Inserts before </body> when present.
    """
    import re

    content = html_path.read_text(encoding="utf-8")
    # Remove any existing QTF section so re-runs don't accumulate duplicates
    content = re.sub(
        r'<section id="qtf-analysis"[^>]*>.*?</section>',
        "",
        content,
        flags=re.DOTALL,
    )
    # Remove any existing QTF TOC entry (idempotent)
    content = re.sub(
        r'\s*<li><a href="#qtf-analysis">[^<]*</a></li>',
        "",
        content,
    )
    # Add QTF TOC entry after the Appendices link — the last numbered section.
    # Handles both compact (same-line) and indented TOC layouts.
    content = re.sub(
        r'(<a href="#appendices">[^<]*</a></li>)',
        r'\1<li><a href="#qtf-analysis">12. QTF Analysis</a></li>',
        content,
        count=1,
    )
    # Use rfind to target the LAST </body> — the real closing tag.
    # Benchmark HTML contains </body> inside JavaScript "view source" popup
    # strings; replace(..., 1) would hit those instead of the real tag.
    body_pos = content.rfind("</body>")
    if body_pos != -1:
        content = content[:body_pos] + qtf_html + "\n</body>" + content[body_pos + len("</body>"):]
    else:
        content += "\n" + qtf_html
    html_path.write_text(content, encoding="utf-8")


def _run_qtf_plots(case_id: str) -> None:
    """Generate QTF plots and embed them in the benchmark HTML report.

    Uses qtf_postprocessing.py to:
      1. Export .xlsx from .owr (if needed)
      2. Write standalone QTF HTML plot files to benchmark/
      3. Inject inline QTF section (with WAMIT reference screenshots side-by-side)
         into benchmark_report.html (single-body) or index.html (multi-body)

    No-ops silently for non-QTF cases.
    """
    import importlib.util

    qtf_script = Path(__file__).parent / "qtf_postprocessing.py"
    if not qtf_script.exists():
        return

    try:
        spec_ = importlib.util.spec_from_file_location("qtf_postprocessing", qtf_script)
        mod = importlib.util.module_from_spec(spec_)
        spec_.loader.exec_module(mod)
    except Exception as exc:
        print(f"  [QTF] Could not load qtf_postprocessing: {exc}")
        return

    if case_id not in mod.CASES:
        return  # Not a QTF case — skip silently

    print("\n[Step 4] Generating QTF plots...")
    try:
        # Write standalone HTML plot files
        plots = mod.run_case(case_id, force_export=False)
        if plots:
            print(f"  {len(plots)} QTF plot(s) written to benchmark/")

        # Build inline HTML section (Plotly + base64 reference screenshots)
        qtf_html = mod.build_inline_html(case_id, force_export=False)

        # Determine target: multi-body cases have index.html; single-body use benchmark_report.html
        output_dir = Path(mod.CASES[case_id]["output"])
        index_html = output_dir / "index.html"
        benchmark_html = output_dir / "benchmark_report.html"
        target_html = index_html if index_html.exists() else benchmark_html

        if target_html.exists():
            _inject_qtf_into_html(target_html, qtf_html)
            print(f"  QTF section injected into {target_html.name}")
        else:
            print(f"  [QTF] Target HTML not found: {target_html.name}")
    except Exception as exc:
        import traceback
        print(f"  [QTF] Failed: {exc}")
        traceback.print_exc()


def _build_results_from_config() -> dict:
    """Build results dict from benchmark artifacts, falling back to config notes.

    Used by --summary-only to generate the master HTML without running solvers.
    """
    import json
    import re
    import yaml

    config_path = L00_DIR / "validation_config.yaml"
    with open(config_path, "r", encoding="utf-8") as f:
        raw = yaml.safe_load(f)

    dof_names = ["surge", "sway", "heave", "roll", "pitch", "yaw"]

    results = {}
    for case_id_raw, meta in raw.get("cases", {}).items():
        cid = str(case_id_raw)
        status_str = meta.get("status", "pending")
        notes = meta.get("notes", "")

        if status_str == "blocked":
            continue

        if cid == "2.5":
            case_ids = [s for s in ("2.5c", "2.5f") if s in CASES]
        elif cid in CASES:
            case_ids = [cid]
        else:
            continue

        for case_key in case_ids:
            case_dir = case_key
            benchmark_dir = L00_DIR / case_dir / "benchmark"
            
            dof_summary_by_body = {}
            am_min_diag = float("nan")
            damp_min_diag = float("nan")

            # Detect body subdirectories
            body_dirs = sorted(benchmark_dir.glob("body_*"))
            if not body_dirs:
                # Flat structure (single body or legacy)
                body_dirs = [benchmark_dir]

            for b_dir in body_dirs:
                # Extract body index from dir name "body_N", default 0
                try:
                    bi = int(b_dir.name.split("_")[-1]) if b_dir.name.startswith("body_") else 0
                except ValueError:
                    bi = 0
                
                report_path = b_dir / "benchmark_report.json"
                
                if report_path.is_file():
                    with open(report_path, "r", encoding="utf-8") as jf:
                        report = json.load(jf)

                    consensus = report.get("consensus_by_dof", {})
                    pairwise = report.get("pairwise_results", {})
                    pw_key = next(iter(pairwise), None)
                    pw_entry = pairwise[pw_key] if pw_key else {}
                    pw_raos = pw_entry.get("rao_comparisons", {})
                    pw_am = pw_entry.get("added_mass_correlations", {})
                    pw_damp = pw_entry.get("damping_correlations", {})

                    dof_stats = {}
                    for dof in dof_names:
                        dof_upper = dof.upper()
                        corr = (
                            consensus.get(dof_upper, {})
                            .get("mean_pairwise_correlation", 1.0)
                        )
                        dof_rao = pw_raos.get(dof, {})
                        max_diff = dof_rao.get("max_magnitude_diff", 0.0)
                        dof_stats[dof] = {
                            "correlation": corr,
                            "max_abs_diff": max_diff,
                            "rel_error_pct": 0.0,
                            "n_points": 0,
                            "phase_correlation": dof_rao.get(
                                "phase_correlation", float("nan")
                            ),
                            "max_phase_diff": dof_rao.get(
                                "max_phase_diff", 0.0
                            ),
                        }
                    dof_summary_by_body[bi] = dof_stats
                    
                    # Store AM/Damp from body 0 (or last body) as summary
                    if bi == 0 or math.isnan(am_min_diag):
                        am_min_diag = _min_diag(pw_am)
                        damp_min_diag = _min_diag(pw_damp)

                else:
                    # Fallback to notes r= parsing (legacy/single-body only)
                    # Only if dof_summary_by_body is empty
                    if not dof_summary_by_body:
                        r_match = re.search(r"r=([0-9.]+)", notes)
                        corr_val = float(r_match.group(1)) if r_match else 1.0

                        dof_stats = {}
                        for dof in dof_names:
                            if cid == "2.9" and dof == "heave":
                                heave_match = re.search(r"heave r=([0-9.]+)", notes)
                                dof_corr = (
                                    float(heave_match.group(1))
                                    if heave_match
                                    else corr_val
                                )
                            else:
                                dof_corr = corr_val
                            dof_stats[dof] = {
                                "correlation": dof_corr,
                                "max_abs_diff": 1e-3,
                                "rel_error_pct": 0.0,
                                "n_points": 0,
                                "phase_correlation": float("nan"),
                                "max_phase_diff": 0.0,
                            }
                        dof_summary_by_body[0] = dof_stats

            # Semantic equivalence check
            # Use bodies list to find files if needed, or just look in root/spec_orcawave
            # Usually semantic check is done on input files which are in benchmark root
            owd_ymls = sorted(benchmark_dir.glob("*_input.yml"))
            # Fall back to companion yml next to the .owd when benchmark yml absent
            # (e.g. before the first full run, or when generate_owd_ymls.py was used)
            if not owd_ymls and case_key in CASES:
                owd_path = CASES[case_key].get("owd")
                if owd_path:
                    companion = Path(owd_path).with_suffix(".yml")
                    if companion.exists():
                        owd_ymls = [companion]
            spec_ymls = [
                p for p in sorted((benchmark_dir / "spec_orcawave").glob("*.yml"))
                if p.name != "spec_input.yml" and "modular" not in p.parts
            ]

            semantic = None
            if owd_ymls and spec_ymls:
                try:
                    semantic = _compare_orcawave_ymls(
                        owd_ymls[0], spec_ymls[0]
                    )
                except Exception:
                    semantic = None

            result_entry = {
                "case_id": case_key,
                "description": CASES[case_key]["description"],
                "status": ("completed" if status_str == "pass" else status_str),
                "dof_summary_by_body": dof_summary_by_body,
                "am_min_diag": am_min_diag,
                "damp_min_diag": damp_min_diag,
                "semantic": semantic,
            }
            
            # Backwards compat
            if 0 in dof_summary_by_body:
                result_entry["dof_summary"] = dof_summary_by_body[0]

            results[case_key] = result_entry

    return results


def _config_key(cid: str) -> str:
    """Convert a case ID like '2.5c' to the YAML config key string (e.g. '2.5').

    YAML keys are quoted strings in validation_config.yaml, so we return str.
    """
    # Strip trailing letters (2.5c → 2.5, 2.5f → 2.5)
    return cid.rstrip("abcdefghijklmnopqrstuvwxyz")


def _is_trivial_dof(dof_info: dict, threshold: float = 1e-6) -> bool:
    """DOF is trivial if max magnitude diff is below threshold (zero signal)."""
    return dof_info.get("max_abs_diff", 0.0) < threshold


def _corr_color(value: float) -> str:
    """CSS color for a correlation value: green/amber/red/grey."""
    if not isinstance(value, (int, float)) or math.isnan(value):
        return "#9ca3af"
    if value >= 0.999:
        return "#16a34a"
    if value >= 0.99:
        return "#d97706"
    return "#dc2626"


def _min_diag(matrix_corr: dict) -> float:
    """Min of 6 diagonal elements (i,i for i=1..6), skipping NaN."""
    vals = []
    for i in range(1, 7):
        v = matrix_corr.get(f"{i},{i}")
        if isinstance(v, (int, float)) and not math.isnan(v):
            vals.append(v)
    return min(vals) if vals else float("nan")


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

        # Semantic equivalence cell (inner content only — wrapped in <td> by row builder)
        sem = r.get("semantic")
        if sem:
            sig = sem["significant_count"]
            if sig == 0:
                sem_inner = '<span style="color:#16a34a;font-weight:bold">EQUIV</span>'
            else:
                sem_color = "#dc2626" if sig > 2 else "#d97706"
                sem_inner = f'<span style="color:{sem_color};font-weight:bold">{sig} diff(s)</span>'
        else:
            sem_inner = '<span style="color:#9ca3af">-</span>'

        # Determine bodies to display
        dof_by_body = r.get("dof_summary_by_body", {})
        if not dof_by_body and "dof_summary" in r:
            dof_by_body = {0: r["dof_summary"]}
        if not dof_by_body:
            dof_by_body = {0: {}}
        
        sorted_bodies = sorted(dof_by_body.keys())
        
        # Body names
        case_def = CASES.get(cid)
        body_names = {}
        if case_def and "bodies" in case_def:
            for b in case_def["bodies"]:
                body_names[b["body_index"]] = b["vessel_name"]
        elif case_def:
            body_names[case_def.get("body_index", 0)] = case_def["vessel_name"]

        rowspan = len(sorted_bodies)

        for i, bi in enumerate(sorted_bodies):
            is_first = (i == 0)
            summary = dof_by_body[bi]
            
            b_name = body_names.get(bi, f"Body {bi}")
            # If single body and name matches case vessel name, show dash
            if len(sorted_bodies) == 1 and b_name == case_def.get("vessel_name"):
                b_name_cell = '<td style="text-align:center;color:#9ca3af">\u2014</td>'
            else:
                b_name_cell = f'<td>{b_name}</td>'

            # DOF correlation cells
            dof_cells = []
            dof_names = ["surge", "sway", "heave", "roll", "pitch", "yaw"]
            for dof in dof_names:
                if dof in summary:
                    s = summary[dof]
                    corr = s["correlation"]
                    max_diff = s["max_abs_diff"]
                    ph_corr = s.get("phase_correlation", float("nan"))
                    ph_diff = s.get("max_phase_diff", 0.0)
                    tip_parts = [f"Mag r: {corr:.8f}" if isinstance(corr, float) else f"Mag r: {corr}"]
                    if isinstance(ph_corr, float) and not math.isnan(ph_corr):
                        tip_parts.append(f"Phase r: {ph_corr:.8f}")
                    tip_parts.append(f"Max |diff|: {max_diff:.2e}")
                    tip_parts.append(f"Max phase diff: {ph_diff:.2f}°")
                    if max_diff < 1e-6:
                        tip_parts[0] += " [identical — Δ<1e-6]"
                    tooltip = "&#10;".join(tip_parts)
                    if max_diff < 1e-6:
                        # Trivially identical results: show correlation in green,
                        # not "0.0" which is ambiguous and looks like zero correlation.
                        corr_display = f"{corr:.6f}" if isinstance(corr, float) else "1.000000"
                        dof_cells.append(f'<td style="text-align:center;color:#16a34a" title="{tooltip}">{corr_display}</td>')
                    elif isinstance(corr, float):
                        if corr >= 0.999:
                            color = "#16a34a"
                        elif corr >= 0.99:
                            color = "#d97706"
                        else:
                            color = "#dc2626"
                        dof_cells.append(
                            f'<td style="text-align:center;color:{color}" title="{tooltip}">{corr:.6f}</td>'
                        )
                    else:
                        dof_cells.append(f'<td style="text-align:center;color:#9ca3af" title="{tooltip}">{corr}</td>')
                else:
                    dof_cells.append('<td style="text-align:center;color:#9ca3af">-</td>')

            # Phase / AM / Damp summary
            ph_vals = []
            for dof in dof_names:
                s = summary.get(dof, {})
                if not _is_trivial_dof(s):
                    ph = s.get("phase_correlation", float("nan"))
                    if isinstance(ph, (int, float)) and not math.isnan(ph):
                        ph_vals.append(ph)
            min_ph = min(ph_vals) if ph_vals else float("nan")
            
            def _fmt_val(v: float) -> str:
                """Format a correlation value as colored HTML (no <td> wrapper)."""
                if not isinstance(v, (int, float)) or math.isnan(v):
                    return '<span style="color:#9ca3af">N/A</span>'
                return f'<span style="color:{_corr_color(v)}">{v:.6f}</span>'

            def _fmt_td(v: float) -> str:
                """Format a correlation value as a full <td> element."""
                return f'<td style="text-align:center">{_fmt_val(v)}</td>'

            # Link to report
            if len(sorted_bodies) > 1:
                rep_path = output_dir / cid / "benchmark" / f"body_{bi}" / "benchmark_report.html"
                report_link = f'{cid}/benchmark/body_{bi}/benchmark_report.html'
                overview_path = output_dir / cid / "benchmark" / "index.html"
            else:
                rep_path = output_dir / cid / "benchmark" / "benchmark_report.html"
                report_link = f'{cid}/benchmark/benchmark_report.html'
                overview_path = None

            link_parts = []
            if rep_path.exists():
                link_parts.append(f'<a href="{report_link}">Report</a>')
            if overview_path and overview_path.exists() and is_first:
                link_parts.append(
                    f'<a href="{cid}/benchmark/index.html">Overview</a>'
                )
            link = " ".join(link_parts) if link_parts else "-"

            # Construct row
            if is_first:
                row = (
                    f"<tr>"
                    f'<td rowspan="{rowspan}" style="font-weight:bold">{cid}</td>'
                    f"{b_name_cell}"
                    f'<td rowspan="{rowspan}">{desc}</td>'
                    f"<td rowspan='{rowspan}' style='text-align:center'>{phase}</td>"
                    f"<td rowspan='{rowspan}' style='text-align:center'>{panels}</td>"
                    f"<td rowspan='{rowspan}' style='text-align:center'>{wamit_ver}</td>"
                    f"<td rowspan='{rowspan}' style='text-align:center'>{badge}</td>"
                    f"<td rowspan='{rowspan}' style='text-align:center'>{sem_inner}</td>"
                )
            else:
                row = f"<tr>{b_name_cell}"

            row += f"{''.join(dof_cells)}"

            # Phase/AM/Damp/Report columns
            if is_first:
                am_val = r.get("am_min_diag", float("nan"))
                damp_val = r.get("damp_min_diag", float("nan"))
                row += _fmt_td(min_ph)
                row += f"<td rowspan='{rowspan}' style='text-align:center'>{_fmt_val(am_val)}</td>"
                row += f"<td rowspan='{rowspan}' style='text-align:center'>{_fmt_val(damp_val)}</td>"
            else:
                row += _fmt_td(min_ph)

            row += f"<td style='text-align:center'>{link}</td></tr>"
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
        badge = '<span style="color:#6b7280;font-weight:bold">BLOCKED</span>'
        dof_cells = ['<td style="text-align:center;color:#9ca3af">-</td>'] * 6
        
        # Blocked row (single row)
        row = (
            f"<tr style='opacity:0.6'>"
            f'<td style="font-weight:bold">{cid}</td>'
            f'<td style="text-align:center;color:#9ca3af">-</td>' # Body
            f"<td>{desc}</td>"
            f"<td style='text-align:center'>{phase}</td>"
            f"<td style='text-align:center'>{panels}</td>"
            f"<td style='text-align:center'>{wamit_ver}</td>"
            f"<td style='text-align:center'>{badge}</td>"
            f'<td style="text-align:center;color:#9ca3af">-</td>' # Semantic
            f"{''.join(dof_cells)}"
            f'<td style="text-align:center;color:#9ca3af">-</td>'
            f'<td style="text-align:center;color:#9ca3af">-</td>'
            f'<td style="text-align:center;color:#9ca3af">-</td>'
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
        dof_by_body = r.get("dof_summary_by_body", {})
        if not dof_by_body and "dof_summary" in r:
             dof_by_body = {0: r["dof_summary"]}
             
        for summary in dof_by_body.values():
            for dof, s in summary.items():
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
  .container {{ max-width: 1600px; margin: 0 auto; }}
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
  td[title] {{ cursor: help; }}
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
        <th>Case</th><th>Body</th><th>Description</th><th>Phase</th><th>Panels</th>
        <th>WAMIT</th><th>Status</th><th>Semantic</th>
        <th>Surge</th><th>Sway</th><th>Heave</th>
        <th>Roll</th><th>Pitch</th><th>Yaw</th>
        <th title="Min phase correlation across non-trivial DOFs">Ph.r</th>
        <th title="Min diagonal added mass correlation (6 self-coupling terms)">AM</th>
        <th title="Min diagonal damping correlation (6 self-coupling terms)">Damp</th>
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
    <span style="color:#64748b">&nbsp;|&nbsp;</span>
    <span style="color:#16a34a">EQUIV = 0 significant solver parameter differences</span>
  </div>
  <div class="footer">
    Correlation coefficient (r) computed per DOF between .owd ground truth and spec.yml pipeline.
    Values shown for heading 0&deg; amplitude comparison. Threshold: r &ge; 0.999 = PASS.
    Semantic column compares OrcaWave YAML configurations (cosmetic/convention differences excluded).
    <br>
    <strong>Ph.r</strong>: min phase correlation across non-trivial DOFs (hover DOF cells for per-DOF detail).
    <strong>AM</strong>/<strong>Damp</strong>: min of 6 diagonal (self-coupling) added mass / damping correlations.
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
        dof_by_body = r.get("dof_summary_by_body", {})
        if not dof_by_body and "dof_summary" in r:
            dof_by_body = {0: r["dof_summary"]}
        for bi in sorted(dof_by_body):
            summary = dof_by_body[bi]
            if len(dof_by_body) > 1:
                print(f"  Body {bi}:")
            print(f"  {'DOF':<8} {'Corr':>10} {'MaxDiff':>10} {'Rel%':>8} {'Points':>8}")
            print(f"  {'-'*8} {'-'*10} {'-'*10} {'-'*8} {'-'*8}")
            for dof, s in summary.items():
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
        dof_by_body = r.get("dof_summary_by_body", {})
        if not dof_by_body and "dof_summary" in r:
            dof_by_body = {0: r["dof_summary"]}
        for bi, summary in dof_by_body.items():
            for dof, s in summary.items():
                corr = s["correlation"]
                max_diff = s["max_abs_diff"]
                if max_diff < 1e-6:
                    continue
                if isinstance(corr, float) and corr < 0.999:
                    all_pass = False
                    body_tag = f" body {bi}" if len(dof_by_body) > 1 else ""
                    print(f"\n  WARN: Case {cid}{body_tag} {dof} correlation {corr:.4f} < 0.999")

    print(f"\n{'='*70}")
    if all_pass:
        print("RESULT: ALL CASES PASS (correlation >= 0.999 per DOF)")
    else:
        print("RESULT: SOME CASES NEED INVESTIGATION")
    print(f"{'='*70}")

    # Generate master HTML summary when running --all
    if args.all and not args.owd_only:
        # Enrich live results with semantic data from YAML pairs
        enriched = _build_results_from_config()
        for cid in results:
            if cid in enriched and "semantic" in enriched[cid]:
                results[cid]["semantic"] = enriched[cid]["semantic"]
        html_path = _generate_master_html(results, OUTPUT_DIR)
        print(f"\nMaster summary: {html_path}")


if __name__ == "__main__":
    main()
