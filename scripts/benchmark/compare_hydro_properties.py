#!/usr/bin/env python3
"""Compare hydrodynamic properties across AQWA and OrcaWave for benchmark hulls.

Extracts radiation damping, gyration radii, and metacentric height from both
AQWA and OrcaWave outputs for barge, ship, and spar benchmark hulls. Produces
comparison tables (console) and an interactive HTML report with Plotly plots.

Usage:
    uv run python scripts/benchmark/compare_hydro_properties.py
    uv run python scripts/benchmark/compare_hydro_properties.py --skip-orcawave
    uv run python scripts/benchmark/compare_hydro_properties.py --hulls barge spar

Data sources:
    - AQWA .LIS files  -> radiation damping + added mass (all 3 hulls)
    - AQWA .AH1 file   -> hydrostatic stiffness, mass matrix (spar only)
    - OrcaWave .yml     -> radiation damping + added mass via OrcFxAPI
    - spec.yml          -> input mass, inertia, radii of gyration
"""

from __future__ import annotations

import argparse
import math
import sys
import textwrap
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Optional

import numpy as np
import yaml

# ---------------------------------------------------------------------------
# Project root and path setup
# ---------------------------------------------------------------------------

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "src"))

from digitalmodel.hydrodynamics.diffraction.aqwa_lis_parser import AQWALISParser
from digitalmodel.hydrodynamics.diffraction.aqwa_ah1_parser import (
    AH1ParseResult,
    parse_ah1,
)

# Optional OrcFxAPI
try:
    import OrcFxAPI

    HAS_ORCFX = True
except ImportError:
    HAS_ORCFX = False

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

DOF_LABELS = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
DOF_INDICES = list(range(6))

HULL_CONFIGS: List[Dict[str, Any]] = [
    {
        "label": "Barge",
        "code": "L02",
        "spec": "docs/domains/orcawave/L02_barge_benchmark/spec.yml",
        "lis": "docs/domains/orcawave/L02_barge_benchmark/source_data/aqwa/WRK-031_3WAY_BENCHMARK.LIS",
        "ah1": None,
        "orcawave_yml": "docs/domains/orcawave/L02_barge_benchmark/source_data/orcawave/Barge_Benchmark.yml",
    },
    {
        "label": "Ship",
        "code": "L03",
        "spec": "docs/domains/orcawave/L03_ship_benchmark/spec.yml",
        "lis": "docs/domains/orcawave/L03_ship_benchmark/source_data/aqwa/001_SHIP_RAOS_REV2.LIS",
        "ah1": None,
        "orcawave_yml": "docs/domains/orcawave/L03_ship_benchmark/source_data/orcawave/orcawave_001_ship_raos_rev2_matched.yml",
    },
    {
        "label": "Spar",
        "code": "L04",
        "spec": "docs/domains/orcawave/L04_spar_benchmark/spec.yml",
        "lis": "docs/domains/orcawave/L04_spar_benchmark/source_data/aqwa/SIMPLE.LIS",
        "ah1": "docs/domains/orcawave/L04_spar_benchmark/source_data/aqwa/SIMPLE.AH1",
        "orcawave_yml": "docs/domains/orcawave/L04_spar_benchmark/source_data/orcawave/spar_benchmark.yml",
    },
]

OUTPUT_DIR = ROOT / "benchmark_output" / "hydro_property_comparison"

# Gravitational acceleration (m/s^2)
G = 9.80665


# ---------------------------------------------------------------------------
# Data containers
# ---------------------------------------------------------------------------


@dataclass
class DampingData:
    """Frequency-dependent radiation damping diagonals for one solver."""

    solver: str
    frequencies: np.ndarray  # rad/s
    diag: Dict[int, np.ndarray]  # DOF index -> array of B_ii values


@dataclass
class GyrationData:
    """Radii of gyration from various sources."""

    spec_kxx: float = 0.0
    spec_kyy: float = 0.0
    spec_kzz: float = 0.0
    aqwa_kxx: Optional[float] = None
    aqwa_kyy: Optional[float] = None
    aqwa_kzz: Optional[float] = None
    orcawave_kxx: Optional[float] = None
    orcawave_kyy: Optional[float] = None
    orcawave_kzz: Optional[float] = None


@dataclass
class HydrostaticData:
    """Hydrostatic properties from AH1 file (spar only)."""

    mass_matrix: Optional[np.ndarray] = None  # 6x6
    stiffness_matrix: Optional[np.ndarray] = None  # 6x6
    draft: Optional[float] = None
    cog: Optional[np.ndarray] = None  # [x, y, z]
    water_depth: Optional[float] = None
    water_density: Optional[float] = None
    gm_transverse: Optional[float] = None
    gm_longitudinal: Optional[float] = None


@dataclass
class HullResult:
    """Collected results for one hull."""

    label: str
    code: str
    spec_params: Dict[str, Any]
    aqwa_damping: Optional[DampingData] = None
    aqwa_added_mass: Optional[DampingData] = None  # reuse container for diagonals
    orcawave_damping: Optional[DampingData] = None
    orcawave_added_mass: Optional[DampingData] = None
    gyration: Optional[GyrationData] = None
    hydrostatic: Optional[HydrostaticData] = None
    errors: List[str] = field(default_factory=list)


# ---------------------------------------------------------------------------
# Spec extraction
# ---------------------------------------------------------------------------


def _load_spec(rel_path: str) -> Dict[str, Any]:
    """Load spec.yml and extract key parameters."""
    full = ROOT / rel_path
    if not full.exists():
        raise FileNotFoundError(f"Spec file not found: {full}")
    with full.open("r", encoding="utf-8") as fh:
        spec = yaml.safe_load(fh)

    vessel = spec.get("vessel", {})
    inertia = vessel.get("inertia", {})
    tensor = inertia.get("inertia_tensor", {})
    mass = inertia.get("mass", 0.0)
    cog = inertia.get("centre_of_gravity", [0.0, 0.0, 0.0])

    ixx = tensor.get("Ixx", 0.0)
    iyy = tensor.get("Iyy", 0.0)
    izz = tensor.get("Izz", 0.0)

    kxx = math.sqrt(ixx / mass) if mass > 0 and ixx > 0 else 0.0
    kyy = math.sqrt(iyy / mass) if mass > 0 and iyy > 0 else 0.0
    kzz = math.sqrt(izz / mass) if mass > 0 and izz > 0 else 0.0

    env = spec.get("environment", {})

    return {
        "mass": mass,
        "cog": cog,
        "ixx": ixx,
        "iyy": iyy,
        "izz": izz,
        "kxx": kxx,
        "kyy": kyy,
        "kzz": kzz,
        "water_depth": env.get("water_depth", 0.0),
        "water_density": env.get("water_density", 1025.0),
        "vessel_name": vessel.get("name", "Unknown"),
        "vessel_type": vessel.get("type", "unknown"),
        "external_damping": vessel.get("external_damping"),
    }


# ---------------------------------------------------------------------------
# AQWA extraction
# ---------------------------------------------------------------------------


def _extract_diagonals(
    matrix_dict: Dict[float, np.ndarray],
) -> tuple[np.ndarray, Dict[int, np.ndarray]]:
    """Extract diagonal elements from frequency-keyed 6x6 matrix dict.

    Returns (frequencies, {dof_index: array_of_values}).
    """
    freqs = np.array(sorted(matrix_dict.keys()))
    diag: Dict[int, np.ndarray] = {}
    for i in range(6):
        vals = np.array([matrix_dict[f][i, i] for f in freqs])
        diag[i] = vals
    return freqs, diag


def extract_aqwa_damping(lis_path: Path) -> Optional[DampingData]:
    """Extract AQWA radiation damping from LIS file."""
    try:
        parser = AQWALISParser(lis_path)
        damping_dict = parser.parse_damping_table()
        freqs, diag = _extract_diagonals(damping_dict)
        return DampingData(solver="AQWA", frequencies=freqs, diag=diag)
    except Exception as e:
        print(f"  [WARNING] AQWA damping extraction failed: {e}")
        return None


def extract_aqwa_added_mass(lis_path: Path) -> Optional[DampingData]:
    """Extract AQWA added mass from LIS file (diagonal elements)."""
    try:
        parser = AQWALISParser(lis_path)
        am_dict = parser.parse_added_mass_table()
        freqs, diag = _extract_diagonals(am_dict)
        return DampingData(solver="AQWA", frequencies=freqs, diag=diag)
    except Exception as e:
        print(f"  [WARNING] AQWA added mass extraction failed: {e}")
        return None


def extract_aqwa_hydrostatic(ah1_path: Path) -> Optional[HydrostaticData]:
    """Extract hydrostatic data from AQWA AH1 file."""
    try:
        result: AH1ParseResult = parse_ah1(ah1_path)
        body_idx = 0

        mass_matrix = result.mass[body_idx] if result.mass.size > 0 else None
        stiffness = (
            result.hydrostatic_stiffness[body_idx]
            if result.hydrostatic_stiffness.size > 0
            else None
        )
        draft = (
            float(result.drafts[body_idx]) if result.drafts.size > 0 else None
        )
        cog = result.cog[body_idx] if result.cog.size > 0 else None

        hdata = HydrostaticData(
            mass_matrix=mass_matrix,
            stiffness_matrix=stiffness,
            draft=draft,
            cog=cog,
            water_depth=result.water_depth,
            water_density=result.water_density,
        )

        # Compute GM from hydrostatic stiffness
        if stiffness is not None and mass_matrix is not None:
            mass_val = mass_matrix[0, 0]  # M11 = mass for single body
            if mass_val > 0:
                # K44 = rho * g * V * GM_T  (restoring moment coefficient)
                # For a single body: GM_T = K44 / (mass * g)
                k44 = stiffness[3, 3]
                k55 = stiffness[4, 4]
                hdata.gm_transverse = k44 / (mass_val * G) if k44 != 0 else None
                hdata.gm_longitudinal = k55 / (mass_val * G) if k55 != 0 else None

        return hdata

    except Exception as e:
        print(f"  [WARNING] AH1 hydrostatic extraction failed: {e}")
        return None


def extract_aqwa_gyration_from_ah1(
    ah1_path: Path,
) -> tuple[Optional[float], Optional[float], Optional[float]]:
    """Extract radii of gyration from AQWA AH1 mass matrix.

    Returns (Kxx, Kyy, Kzz) or (None, None, None) on failure.
    """
    try:
        result: AH1ParseResult = parse_ah1(ah1_path)
        if result.mass.size == 0:
            return None, None, None
        mass_matrix = result.mass[0]  # First body
        mass = mass_matrix[0, 0]
        if mass <= 0:
            return None, None, None
        i44 = mass_matrix[3, 3]  # Roll inertia
        i55 = mass_matrix[4, 4]  # Pitch inertia
        i66 = mass_matrix[5, 5]  # Yaw inertia
        kxx = math.sqrt(i44 / mass) if i44 > 0 else 0.0
        kyy = math.sqrt(i55 / mass) if i55 > 0 else 0.0
        kzz = math.sqrt(i66 / mass) if i66 > 0 else 0.0
        return kxx, kyy, kzz
    except Exception:
        return None, None, None


# ---------------------------------------------------------------------------
# OrcaWave extraction (requires OrcFxAPI)
# ---------------------------------------------------------------------------


def extract_orcawave_data(
    yml_path: Path,
) -> tuple[Optional[DampingData], Optional[DampingData]]:
    """Extract OrcaWave radiation damping and added mass via OrcFxAPI.

    Loads the OrcaWave YAML model, runs the diffraction calculation, and
    extracts the diagonal elements of the damping and added mass matrices.

    Returns (damping_data, added_mass_data) or (None, None) if OrcFxAPI
    is not available or extraction fails.
    """
    if not HAS_ORCFX:
        return None, None

    try:
        model = OrcFxAPI.Diffraction(str(yml_path.resolve()))
        model.Calculate()

        # Frequencies in Hz from OrcFxAPI; convert to rad/s
        frequencies_hz = np.array(model.frequencies)
        frequencies_rad = 2.0 * np.pi * frequencies_hz

        # Sort ascending by frequency
        sort_idx = np.argsort(frequencies_rad)
        frequencies_rad = frequencies_rad[sort_idx]

        # Damping: shape (nfreq, 6, 6) after sort
        damp_raw = np.array(model.damping)[sort_idx]
        damp_diag: Dict[int, np.ndarray] = {}
        for i in range(6):
            damp_diag[i] = damp_raw[:, i, i]

        damping_data = DampingData(
            solver="OrcaWave", frequencies=frequencies_rad, diag=damp_diag
        )

        # Added mass: shape (nfreq, 6, 6) after sort
        am_raw = np.array(model.addedMass)[sort_idx]
        am_diag: Dict[int, np.ndarray] = {}
        for i in range(6):
            am_diag[i] = am_raw[:, i, i]

        am_data = DampingData(
            solver="OrcaWave", frequencies=frequencies_rad, diag=am_diag
        )

        return damping_data, am_data

    except Exception as e:
        print(f"  [WARNING] OrcaWave extraction failed: {e}")
        return None, None


def _extract_orcawave_gyration_from_yaml(
    yml_path: Path,
) -> tuple[Optional[float], Optional[float], Optional[float]]:
    """Extract radii of gyration from OrcaWave YAML file directly.

    Reads the YAML and computes K = sqrt(I / mass) from body properties.
    This avoids needing OrcFxAPI or mesh files. OrcaWave stores mass in
    tonnes (te) and inertia in te.m^2, so the ratio is in m^2.

    OrcaWave YAML uses combined keys for the inertia tensor, e.g.:
        ``BodyInertiaTensorRx, BodyInertiaTensorRy, BodyInertiaTensorRz``
    whose value is a 3x3 matrix.  The diagonal elements are Ixx, Iyy, Izz.

    Returns (Kxx, Kyy, Kzz) or (None, None, None) if extraction fails.
    """
    try:
        with yml_path.open("r", encoding="utf-8-sig") as fh:
            docs = list(yaml.safe_load_all(fh))

        body_mass = None
        inertia_diag = None  # [Ixx, Iyy, Izz]

        for doc in docs:
            if not isinstance(doc, dict):
                continue

            # Collect all key-value pairs to scan, including nested dicts
            # and the Bodies list which contains body dicts
            items_to_scan: list[tuple[str, Any]] = []

            for key, val in doc.items():
                items_to_scan.append((str(key), val))
                if isinstance(val, dict):
                    for k2, v2 in val.items():
                        items_to_scan.append((str(k2), v2))
                elif isinstance(val, list):
                    # Bodies is a list of dicts
                    for item in val:
                        if isinstance(item, dict):
                            for k2, v2 in item.items():
                                items_to_scan.append((str(k2), v2))

            for key, val in items_to_scan:
                # Check for BodyMass
                if key == "BodyMass" and val is not None:
                    body_mass = float(val)
                # Check for inertia tensor (combined key format, e.g.
                # "BodyInertiaTensorRx, BodyInertiaTensorRy, ...")
                if "BodyInertiaTensor" in key:
                    if isinstance(val, list) and len(val) >= 3:
                        try:
                            row0 = val[0]
                            row1 = val[1]
                            row2 = val[2]
                            ixx = float(row0[0]) if isinstance(row0, list) else float(row0)
                            iyy = float(row1[1]) if isinstance(row1, list) else float(row1)
                            izz = float(row2[2]) if isinstance(row2, list) else float(row2)
                            inertia_diag = [ixx, iyy, izz]
                        except (IndexError, TypeError, ValueError):
                            pass
                # Also check individual moment of inertia keys (some YAMLs)
                if key == "BodyMomentOfInertiaRx" and val is not None:
                    if inertia_diag is None:
                        inertia_diag = [0.0, 0.0, 0.0]
                    inertia_diag[0] = float(val)
                if key == "BodyMomentOfInertiaRy" and val is not None:
                    if inertia_diag is None:
                        inertia_diag = [0.0, 0.0, 0.0]
                    inertia_diag[1] = float(val)
                if key == "BodyMomentOfInertiaRz" and val is not None:
                    if inertia_diag is None:
                        inertia_diag = [0.0, 0.0, 0.0]
                    inertia_diag[2] = float(val)

        if body_mass is None or body_mass <= 0:
            return None, None, None

        if inertia_diag is None:
            return None, None, None

        ixx, iyy, izz = inertia_diag
        kxx = math.sqrt(ixx / body_mass) if ixx > 0 else 0.0
        kyy = math.sqrt(iyy / body_mass) if iyy > 0 else 0.0
        kzz = math.sqrt(izz / body_mass) if izz > 0 else 0.0
        return kxx, kyy, kzz
    except Exception as e:
        print(f"  [WARNING] OrcaWave YAML gyration extraction failed: {e}")
        return None, None, None


def extract_orcawave_gyration(
    yml_path: Path,
) -> tuple[Optional[float], Optional[float], Optional[float]]:
    """Extract radii of gyration from OrcaWave model.

    Tries OrcFxAPI first (requires license + mesh). Falls back to direct
    YAML parsing which does not require OrcFxAPI at all.

    Returns (Kxx, Kyy, Kzz) or (None, None, None) if extraction fails.
    """
    if HAS_ORCFX:
        try:
            model = OrcFxAPI.Diffraction(str(yml_path.resolve()))
            # Access first body's properties
            body_count = model.bodyCount
            if body_count > 0:
                body = model.bodies[0]
                mass = body.BodyMass
                i44 = body.BodyMomentOfInertiaRx
                i55 = body.BodyMomentOfInertiaRy
                i66 = body.BodyMomentOfInertiaRz
                if mass > 0:
                    kxx = math.sqrt(i44 / mass) if i44 > 0 else 0.0
                    kyy = math.sqrt(i55 / mass) if i55 > 0 else 0.0
                    kzz = math.sqrt(i66 / mass) if i66 > 0 else 0.0
                    return kxx, kyy, kzz
        except Exception:
            pass  # Fall through to YAML parsing

    # Fallback: parse YAML directly
    return _extract_orcawave_gyration_from_yaml(yml_path)


# ---------------------------------------------------------------------------
# Processing pipeline
# ---------------------------------------------------------------------------


def process_hull(cfg: Dict[str, Any], skip_orcawave: bool = False) -> HullResult:
    """Process a single hull: extract all data from all sources."""
    label = cfg["label"]
    code = cfg["code"]
    print(f"\n{'='*60}")
    print(f"Processing {label} ({code})")
    print(f"{'='*60}")

    # Load spec
    spec_params = _load_spec(cfg["spec"])
    result = HullResult(label=label, code=code, spec_params=spec_params)

    # Gyration from spec
    result.gyration = GyrationData(
        spec_kxx=spec_params["kxx"],
        spec_kyy=spec_params["kyy"],
        spec_kzz=spec_params["kzz"],
    )

    # AQWA LIS extraction
    lis_path = ROOT / cfg["lis"]
    if lis_path.exists():
        print(f"  Extracting AQWA data from: {lis_path.name}")
        result.aqwa_damping = extract_aqwa_damping(lis_path)
        result.aqwa_added_mass = extract_aqwa_added_mass(lis_path)
        if result.aqwa_damping:
            nf = len(result.aqwa_damping.frequencies)
            print(f"    Damping: {nf} frequencies extracted")
        if result.aqwa_added_mass:
            nf = len(result.aqwa_added_mass.frequencies)
            print(f"    Added mass: {nf} frequencies extracted")
    else:
        msg = f"AQWA LIS file not found: {lis_path}"
        print(f"  [ERROR] {msg}")
        result.errors.append(msg)

    # AQWA AH1 extraction (spar only)
    if cfg["ah1"] is not None:
        ah1_path = ROOT / cfg["ah1"]
        if ah1_path.exists():
            print(f"  Extracting AQWA hydrostatic from: {ah1_path.name}")
            result.hydrostatic = extract_aqwa_hydrostatic(ah1_path)
            if result.hydrostatic:
                print(f"    Draft: {result.hydrostatic.draft}")
                if result.hydrostatic.gm_transverse is not None:
                    print(
                        f"    GM_T: {result.hydrostatic.gm_transverse:.4f} m"
                    )
                if result.hydrostatic.gm_longitudinal is not None:
                    print(
                        f"    GM_L: {result.hydrostatic.gm_longitudinal:.4f} m"
                    )

            # Gyration from AH1
            kxx, kyy, kzz = extract_aqwa_gyration_from_ah1(ah1_path)
            if kxx is not None and result.gyration is not None:
                result.gyration.aqwa_kxx = kxx
                result.gyration.aqwa_kyy = kyy
                result.gyration.aqwa_kzz = kzz
                print(
                    f"    Gyration (AH1): Kxx={kxx:.2f}, "
                    f"Kyy={kyy:.2f}, Kzz={kzz:.2f} m"
                )
        else:
            msg = f"AQWA AH1 file not found: {ah1_path}"
            print(f"  [WARNING] {msg}")
            result.errors.append(msg)

    # OrcaWave extraction
    if not skip_orcawave:
        ow_yml = ROOT / cfg["orcawave_yml"]
        if ow_yml.exists():
            if HAS_ORCFX:
                print(f"  Extracting OrcaWave data from: {ow_yml.name}")
                ow_damp, ow_am = extract_orcawave_data(ow_yml)
                result.orcawave_damping = ow_damp
                result.orcawave_added_mass = ow_am
                if ow_damp:
                    nf = len(ow_damp.frequencies)
                    print(f"    Damping: {nf} frequencies extracted")
                if ow_am:
                    nf = len(ow_am.frequencies)
                    print(f"    Added mass: {nf} frequencies extracted")

                # Gyration from OrcaWave
                kxx, kyy, kzz = extract_orcawave_gyration(ow_yml)
                if kxx is not None and result.gyration is not None:
                    result.gyration.orcawave_kxx = kxx
                    result.gyration.orcawave_kyy = kyy
                    result.gyration.orcawave_kzz = kzz
                    print(
                        f"    Gyration (OrcaWave): Kxx={kxx:.2f}, "
                        f"Kyy={kyy:.2f}, Kzz={kzz:.2f} m"
                    )
            else:
                print(
                    "  OrcFxAPI not available -- OrcaWave damping extraction skipped"
                )
        else:
            msg = f"OrcaWave YAML not found: {ow_yml}"
            print(f"  [WARNING] {msg}")
            result.errors.append(msg)
    else:
        print("  OrcaWave extraction skipped (--skip-orcawave)")

    return result


# ---------------------------------------------------------------------------
# Console output
# ---------------------------------------------------------------------------


def _fmt_sci(val: float) -> str:
    """Format a float in scientific notation."""
    if val == 0.0:
        return "0.00E+00"
    return f"{val:.3E}"


def _fmt_k(val: Optional[float]) -> str:
    """Format gyration radius value."""
    if val is None:
        return "N/A"
    return f"{val:.4f}"


def print_gyration_table(results: List[HullResult]) -> None:
    """Print gyration radii comparison table to console."""
    print("\n" + "=" * 80)
    print("GYRATION RADII COMPARISON (m)")
    print("=" * 80)

    hdr = f"  {'Hull':<8} {'Axis':<6} {'Spec':<12} {'AQWA (AH1)':<14} {'OrcaWave':<14} {'Match':<10}"
    print(hdr)
    print("  " + "-" * 70)

    for r in results:
        g = r.gyration
        if g is None:
            print(f"  {r.label:<8} No gyration data")
            continue

        for axis, spec_val, aqwa_val, ow_val in [
            ("Kxx", g.spec_kxx, g.aqwa_kxx, g.orcawave_kxx),
            ("Kyy", g.spec_kyy, g.aqwa_kyy, g.orcawave_kyy),
            ("Kzz", g.spec_kzz, g.aqwa_kzz, g.orcawave_kzz),
        ]:
            # Determine match status
            match_parts = []
            if aqwa_val is not None and spec_val > 0:
                diff_pct = abs(aqwa_val - spec_val) / spec_val * 100
                match_parts.append(f"AQ:{diff_pct:.1f}%")
            if ow_val is not None and spec_val > 0:
                diff_pct = abs(ow_val - spec_val) / spec_val * 100
                match_parts.append(f"OW:{diff_pct:.1f}%")
            match_str = ", ".join(match_parts) if match_parts else "--"

            print(
                f"  {r.label:<8} {axis:<6} {_fmt_k(spec_val):<12} "
                f"{_fmt_k(aqwa_val):<14} {_fmt_k(ow_val):<14} {match_str}"
            )
    print()


def print_damping_summary(results: List[HullResult]) -> None:
    """Print radiation damping summary table to console."""
    print("=" * 80)
    print("RADIATION DAMPING SUMMARY (diagonal B_ii)")
    print("=" * 80)

    for r in results:
        print(f"\n--- {r.label} ({r.code}) ---")
        aq = r.aqwa_damping
        ow = r.orcawave_damping

        if aq is None and ow is None:
            print("  No damping data available")
            continue

        hdr = f"  {'DOF':<8} {'AQWA Freq':<14} {'AQWA B_max':<14} {'OW Freq':<14} {'OW B_max':<14}"
        print(hdr)
        print("  " + "-" * 60)

        for i, dof_name in enumerate(DOF_LABELS):
            aq_info = "--"
            aq_bmax = "--"
            ow_info = "--"
            ow_bmax = "--"

            if aq is not None and i in aq.diag:
                nf = len(aq.frequencies)
                peak_idx = int(np.argmax(np.abs(aq.diag[i])))
                aq_info = f"{nf} pts"
                aq_bmax = _fmt_sci(float(aq.diag[i][peak_idx]))

            if ow is not None and i in ow.diag:
                nf = len(ow.frequencies)
                peak_idx = int(np.argmax(np.abs(ow.diag[i])))
                ow_info = f"{nf} pts"
                ow_bmax = _fmt_sci(float(ow.diag[i][peak_idx]))

            print(
                f"  {dof_name:<8} {aq_info:<14} {aq_bmax:<14} "
                f"{ow_info:<14} {ow_bmax:<14}"
            )


def print_hydrostatic_table(results: List[HullResult]) -> None:
    """Print hydrostatic / metacentric height comparison."""
    has_hydrostatic = any(r.hydrostatic is not None for r in results)
    if not has_hydrostatic:
        print("\n" + "=" * 80)
        print("HYDROSTATIC / METACENTRIC HEIGHT")
        print("=" * 80)
        print("  No AH1 data available for any hull.")
        print("  AH1 files provide hydrostatic stiffness for GM calculation.")
        print("  Currently only the Spar hull has an AH1 file.")
        return

    print("\n" + "=" * 80)
    print("HYDROSTATIC / METACENTRIC HEIGHT (from AQWA AH1)")
    print("=" * 80)

    hdr = (
        f"  {'Hull':<8} {'Draft(m)':<10} {'CoG_z(m)':<10} "
        f"{'K33(N/m)':<14} {'K44(Nm/rad)':<14} {'K55(Nm/rad)':<14} "
        f"{'GM_T(m)':<10} {'GM_L(m)':<10}"
    )
    print(hdr)
    print("  " + "-" * 90)

    for r in results:
        h = r.hydrostatic
        if h is None:
            print(f"  {r.label:<8} N/A -- requires AH1 file (hydrostatic rerun)")
            continue

        draft_str = f"{h.draft:.2f}" if h.draft is not None else "N/A"
        cog_z_str = f"{h.cog[2]:.2f}" if h.cog is not None else "N/A"
        k33_str = _fmt_sci(h.stiffness_matrix[2, 2]) if h.stiffness_matrix is not None else "N/A"
        k44_str = _fmt_sci(h.stiffness_matrix[3, 3]) if h.stiffness_matrix is not None else "N/A"
        k55_str = _fmt_sci(h.stiffness_matrix[4, 4]) if h.stiffness_matrix is not None else "N/A"
        gmt_str = f"{h.gm_transverse:.4f}" if h.gm_transverse is not None else "N/A"
        gml_str = f"{h.gm_longitudinal:.4f}" if h.gm_longitudinal is not None else "N/A"

        print(
            f"  {r.label:<8} {draft_str:<10} {cog_z_str:<10} "
            f"{k33_str:<14} {k44_str:<14} {k55_str:<14} "
            f"{gmt_str:<10} {gml_str:<10}"
        )

    print()


# ---------------------------------------------------------------------------
# HTML report generation
# ---------------------------------------------------------------------------


def _plotly_damping_figure(
    hull_result: HullResult,
    dof_idx: int,
    data_type: str = "damping",
) -> Optional[str]:
    """Generate a Plotly figure div for one DOF's frequency-dependent data.

    Args:
        hull_result: The hull result containing solver data.
        dof_idx: DOF index (0-5).
        data_type: "damping" or "added_mass".

    Returns:
        HTML string with the Plotly div, or None if no data.
    """
    dof_name = DOF_LABELS[dof_idx]

    if data_type == "damping":
        aq_data = hull_result.aqwa_damping
        ow_data = hull_result.orcawave_damping
        y_label = f"B_{dof_idx+1}{dof_idx+1}"
        title_type = "Radiation Damping"
    else:
        aq_data = hull_result.aqwa_added_mass
        ow_data = hull_result.orcawave_added_mass
        y_label = f"A_{dof_idx+1}{dof_idx+1}"
        title_type = "Added Mass"

    if aq_data is None and ow_data is None:
        return None

    div_id = f"plot_{data_type}_{hull_result.code}_{dof_name.lower()}"

    traces = []
    if aq_data is not None and dof_idx in aq_data.diag:
        x_vals = aq_data.frequencies.tolist()
        y_vals = aq_data.diag[dof_idx].tolist()
        traces.append(
            f"""{{
                x: {x_vals},
                y: {y_vals},
                mode: 'lines+markers',
                name: 'AQWA',
                line: {{color: '#e74c3c', width: 2}},
                marker: {{size: 5}}
            }}"""
        )

    if ow_data is not None and dof_idx in ow_data.diag:
        x_vals = ow_data.frequencies.tolist()
        y_vals = ow_data.diag[dof_idx].tolist()
        traces.append(
            f"""{{
                x: {x_vals},
                y: {y_vals},
                mode: 'lines+markers',
                name: 'OrcaWave',
                line: {{color: '#3498db', width: 2}},
                marker: {{size: 5}}
            }}"""
        )

    if not traces:
        return None

    # Determine y-axis units string
    if data_type == "damping":
        if dof_idx < 3:
            y_unit = "N.s/m"
        else:
            y_unit = "N.m.s/rad"
    else:
        if dof_idx < 3:
            y_unit = "kg"
        else:
            y_unit = "kg.m^2"

    title = f"{hull_result.label} - {dof_name} {title_type} ({y_label})"

    layout = f"""{{
        title: {{text: '{title}', font: {{size: 14}}}},
        xaxis: {{title: 'Frequency (rad/s)', gridcolor: '#eee'}},
        yaxis: {{title: '{y_label} ({y_unit})', gridcolor: '#eee', exponentformat: 'e'}},
        legend: {{x: 1.02, y: 1.0, orientation: 'v', tracegroupgap: 2}},
        margin: {{l: 80, r: 150, t: 40, b: 50}},
        plot_bgcolor: '#fff',
        paper_bgcolor: '#fff',
        height: 320,
        font: {{family: "'Segoe UI', Roboto, Arial, sans-serif", size: 12}}
    }}"""

    return f"""
    <div id="{div_id}" style="width:100%;"></div>
    <script>
        Plotly.newPlot('{div_id}', [{', '.join(traces)}], {layout}, {{responsive: true}});
    </script>
    """


def _gyration_table_html(results: List[HullResult]) -> str:
    """Generate HTML table for gyration radii comparison."""
    rows = []
    for r in results:
        g = r.gyration
        if g is None:
            rows.append(
                f"<tr><td>{r.label}</td>"
                + "<td colspan='9'>No gyration data</td></tr>"
            )
            continue

        for axis, spec_val, aqwa_val, ow_val in [
            ("Kxx", g.spec_kxx, g.aqwa_kxx, g.orcawave_kxx),
            ("Kyy", g.spec_kyy, g.aqwa_kyy, g.orcawave_kyy),
            ("Kzz", g.spec_kzz, g.aqwa_kzz, g.orcawave_kzz),
        ]:
            aqwa_str = f"{aqwa_val:.4f}" if aqwa_val is not None else "N/A"
            ow_str = f"{ow_val:.4f}" if ow_val is not None else "N/A"

            # Compute diff percentages
            aq_diff = ""
            if aqwa_val is not None and spec_val > 0:
                pct = abs(aqwa_val - spec_val) / spec_val * 100
                color = "#27ae60" if pct < 1.0 else "#e67e22" if pct < 5.0 else "#e74c3c"
                aq_diff = f'<span style="color:{color}">{pct:.2f}%</span>'

            ow_diff = ""
            if ow_val is not None and spec_val > 0:
                pct = abs(ow_val - spec_val) / spec_val * 100
                color = "#27ae60" if pct < 1.0 else "#e67e22" if pct < 5.0 else "#e74c3c"
                ow_diff = f'<span style="color:{color}">{pct:.2f}%</span>'

            rows.append(
                f"<tr>"
                f"<td>{r.label}</td>"
                f"<td>{axis}</td>"
                f'<td class="num">{spec_val:.4f}</td>'
                f'<td class="num">{aqwa_str}</td>'
                f"<td>{aq_diff}</td>"
                f'<td class="num">{ow_str}</td>'
                f"<td>{ow_diff}</td>"
                f"</tr>"
            )

    return f"""
    <table>
      <thead>
        <tr>
          <th>Hull</th><th>Axis</th>
          <th>Spec (m)</th>
          <th>AQWA AH1 (m)</th><th>Diff</th>
          <th>OrcaWave (m)</th><th>Diff</th>
        </tr>
      </thead>
      <tbody>
        {''.join(rows)}
      </tbody>
    </table>
    """


def _hydrostatic_table_html(results: List[HullResult]) -> str:
    """Generate HTML table for hydrostatic / metacentric height comparison."""
    rows = []
    for r in results:
        h = r.hydrostatic
        if h is None:
            rows.append(
                f"<tr><td>{r.label} ({r.code})</td>"
                f"<td colspan='6'>N/A -- requires AH1 file (hydrostatic rerun)</td></tr>"
            )
            continue

        draft_str = f"{h.draft:.2f}" if h.draft is not None else "N/A"
        cog_z = f"{h.cog[2]:.2f}" if h.cog is not None else "N/A"
        k33 = _fmt_sci(h.stiffness_matrix[2, 2]) if h.stiffness_matrix is not None else "N/A"
        k44 = _fmt_sci(h.stiffness_matrix[3, 3]) if h.stiffness_matrix is not None else "N/A"
        k55 = _fmt_sci(h.stiffness_matrix[4, 4]) if h.stiffness_matrix is not None else "N/A"
        gmt = f"{h.gm_transverse:.4f}" if h.gm_transverse is not None else "N/A"
        gml = f"{h.gm_longitudinal:.4f}" if h.gm_longitudinal is not None else "N/A"

        rows.append(
            f"<tr>"
            f"<td>{r.label} ({r.code})</td>"
            f'<td class="num">{draft_str}</td>'
            f'<td class="num">{cog_z}</td>'
            f'<td class="num">{k33}</td>'
            f'<td class="num">{k44}</td>'
            f'<td class="num">{k55}</td>'
            f'<td class="num">{gmt}</td>'
            f'<td class="num">{gml}</td>'
            f"</tr>"
        )

    return f"""
    <table>
      <thead>
        <tr>
          <th>Hull</th>
          <th>Draft (m)</th>
          <th>CoG_z (m)</th>
          <th>K33 (N/m)</th>
          <th>K44 (Nm/rad)</th>
          <th>K55 (Nm/rad)</th>
          <th>GM_T (m)</th>
          <th>GM_L (m)</th>
        </tr>
      </thead>
      <tbody>
        {''.join(rows)}
      </tbody>
    </table>
    """


def _damping_peak_table_html(results: List[HullResult]) -> str:
    """Generate HTML table summarising peak damping values per DOF per hull."""
    rows = []
    for r in results:
        for i, dof_name in enumerate(DOF_LABELS):
            aq_peak = "--"
            aq_freq = "--"
            ow_peak = "--"
            ow_freq = "--"

            if r.aqwa_damping is not None and i in r.aqwa_damping.diag:
                vals = r.aqwa_damping.diag[i]
                idx = int(np.argmax(np.abs(vals)))
                aq_peak = _fmt_sci(float(vals[idx]))
                aq_freq = f"{r.aqwa_damping.frequencies[idx]:.3f}"

            if r.orcawave_damping is not None and i in r.orcawave_damping.diag:
                vals = r.orcawave_damping.diag[i]
                idx = int(np.argmax(np.abs(vals)))
                ow_peak = _fmt_sci(float(vals[idx]))
                ow_freq = f"{r.orcawave_damping.frequencies[idx]:.3f}"

            rows.append(
                f"<tr>"
                f"<td>{r.label}</td>"
                f"<td>{dof_name}</td>"
                f'<td class="num">{aq_peak}</td>'
                f'<td class="num">{aq_freq}</td>'
                f'<td class="num">{ow_peak}</td>'
                f'<td class="num">{ow_freq}</td>'
                f"</tr>"
            )

    return f"""
    <table>
      <thead>
        <tr>
          <th>Hull</th><th>DOF</th>
          <th>AQWA B_max</th><th>@ freq (rad/s)</th>
          <th>OrcaWave B_max</th><th>@ freq (rad/s)</th>
        </tr>
      </thead>
      <tbody>
        {''.join(rows)}
      </tbody>
    </table>
    """


def generate_html_report(results: List[HullResult], output_path: Path) -> None:
    """Generate the full HTML comparison report."""
    now = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")

    # Collect all plot divs per hull
    hull_sections = []
    for r in results:
        # Damping plots
        damping_plots = []
        for i in range(6):
            fig_html = _plotly_damping_figure(r, i, "damping")
            if fig_html:
                damping_plots.append(fig_html)

        # Added mass plots
        am_plots = []
        for i in range(6):
            fig_html = _plotly_damping_figure(r, i, "added_mass")
            if fig_html:
                am_plots.append(fig_html)

        damping_section = "\n".join(damping_plots) if damping_plots else (
            "<p>No damping data available for this hull.</p>"
        )
        am_section = "\n".join(am_plots) if am_plots else (
            "<p>No added mass data available for this hull.</p>"
        )

        hull_sections.append(f"""
        <div class="section">
          <h2>{r.label} ({r.code}) -- Radiation Damping B_ii vs Frequency</h2>
          <div class="plot-grid">
            {damping_section}
          </div>
        </div>

        <div class="section">
          <h2>{r.label} ({r.code}) -- Added Mass A_ii vs Frequency</h2>
          <div class="plot-grid">
            {am_section}
          </div>
        </div>
        """)

    # Build full HTML
    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>Hydrodynamic Property Comparison</title>
<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
<style>
  * {{ box-sizing: border-box; }}
  body {{
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI',
                 Roboto, Arial, sans-serif;
    margin: 0; padding: 0; color: #333; background: #f8f9fa;
    font-size: 14px; line-height: 1.5;
  }}
  .container {{ max-width: 1400px; margin: 0 auto; padding: 1.5em 2em; }}

  /* Header */
  .report-header {{
    background: #2c3e50; color: #fff; padding: 1.2em 2em;
    margin-bottom: 1.5em; border-radius: 6px;
  }}
  .report-header h1 {{ margin: 0 0 0.3em; font-size: 1.6em; }}
  .report-header .meta {{ font-size: 0.9em; opacity: 0.85; }}

  /* Section cards */
  .section {{
    background: #fff; border-radius: 6px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.08);
    margin-bottom: 1.5em; padding: 1.2em 1.5em;
  }}
  .section h2 {{
    margin: 0 0 0.8em; font-size: 1.2em; color: #2c3e50;
    border-bottom: 2px solid #3498db; padding-bottom: 0.3em;
  }}

  /* Tables */
  table {{
    border-collapse: collapse; margin: 0.5em 0; font-size: 0.85em;
    width: 100%;
  }}
  th, td {{
    border: 1px solid #ddd; padding: 0.45em 0.7em;
    text-align: left;
  }}
  th {{
    background: #34495e; color: #fff; font-weight: 600;
    font-size: 0.85em; text-transform: uppercase; letter-spacing: 0.3px;
  }}
  tbody tr:nth-child(even) {{ background: #f8f9fa; }}
  tbody tr:nth-child(odd) {{ background: #fff; }}
  tbody tr:hover {{ background: #ebf5fb; }}
  td.num {{
    font-family: 'Cascadia Code', 'Consolas', 'SF Mono', monospace;
    text-align: right;
  }}

  /* Plot grid */
  .plot-grid {{
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 1em;
  }}
  @media (max-width: 900px) {{
    .plot-grid {{ grid-template-columns: 1fr; }}
  }}

  /* Notes */
  .note {{
    background: #fef9e7; border-left: 4px solid #f39c12;
    padding: 0.8em 1em; margin: 0.8em 0; font-size: 0.9em;
  }}
  .error-note {{
    background: #fdedec; border-left: 4px solid #e74c3c;
    padding: 0.8em 1em; margin: 0.8em 0; font-size: 0.9em;
  }}
</style>
</head>
<body>
<div class="container">

<div class="report-header">
  <h1>Hydrodynamic Property Comparison</h1>
  <div class="meta">
    Generated: {now} |
    Hulls: {', '.join(r.label for r in results)} |
    OrcFxAPI: {'Available' if HAS_ORCFX else 'Not available'}
  </div>
</div>

<!-- Gyration Radii -->
<div class="section">
  <h2>Gyration Radii Comparison</h2>
  <p>Radii of gyration computed as K = sqrt(I / mass) from each source.
     Spec values are from spec.yml inertia tensor. AQWA values from AH1 mass
     matrix (spar only). OrcaWave values from model body properties.</p>
  {_gyration_table_html(results)}
</div>

<!-- Hydrostatic / Metacentric Height -->
<div class="section">
  <h2>Metacentric Height (from Hydrostatic Stiffness)</h2>
  <p>GM_T = K44 / (mass &times; g), GM_L = K55 / (mass &times; g).
     Hydrostatic stiffness extracted from AQWA AH1 file where available.</p>
  {_hydrostatic_table_html(results)}
  <div class="note">
    AH1 files are only available for the Spar hull. Barge and Ship require
    a hydrostatic rerun with the <code>OPTIONS AHD1</code> card to generate
    AH1 output.
  </div>
</div>

<!-- Peak Damping Summary -->
<div class="section">
  <h2>Radiation Damping -- Peak Values Summary</h2>
  <p>Maximum diagonal radiation damping coefficient and the frequency at
     which it occurs, per DOF per hull.</p>
  {_damping_peak_table_html(results)}
</div>

<!-- Per-hull damping + added mass plots -->
{''.join(hull_sections)}

<!-- Errors -->
{_errors_section_html(results)}

</div>
</body>
</html>
"""
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(html, encoding="utf-8")
    print(f"\nHTML report written to: {output_path}")


def _errors_section_html(results: List[HullResult]) -> str:
    """Generate HTML section listing any errors encountered."""
    all_errors = []
    for r in results:
        for err in r.errors:
            all_errors.append(f"<li><strong>{r.label}:</strong> {err}</li>")

    if not all_errors:
        return ""

    return f"""
    <div class="section">
      <h2>Errors / Warnings</h2>
      <ul>
        {''.join(all_errors)}
      </ul>
    </div>
    """


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Compare hydrodynamic properties across benchmark hulls."
    )
    parser.add_argument(
        "--skip-orcawave",
        action="store_true",
        help="Skip OrcaWave extraction even if OrcFxAPI is available.",
    )
    parser.add_argument(
        "--hulls",
        nargs="+",
        choices=["barge", "ship", "spar"],
        default=None,
        help="Only process the specified hulls (default: all).",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=OUTPUT_DIR / "hydro_comparison.html",
        help="Path to output HTML report.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()

    # Filter hull configs if --hulls specified
    if args.hulls:
        selected = {h.lower() for h in args.hulls}
        configs = [c for c in HULL_CONFIGS if c["label"].lower() in selected]
    else:
        configs = HULL_CONFIGS

    if not configs:
        print("ERROR: No hulls selected.", file=sys.stderr)
        sys.exit(1)

    now = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")
    print("=" * 80)
    print("Hydrodynamic Property Comparison")
    print(f"Date: {now}")
    print(f"OrcFxAPI: {'Available' if HAS_ORCFX else 'Not available'}")
    print(f"Hulls: {', '.join(c['label'] for c in configs)}")
    print("=" * 80)

    # Process each hull
    results: List[HullResult] = []
    for cfg in configs:
        try:
            result = process_hull(cfg, skip_orcawave=args.skip_orcawave)
            results.append(result)
        except Exception as e:
            print(f"\n[ERROR] Failed to process {cfg['label']}: {e}")
            results.append(
                HullResult(
                    label=cfg["label"],
                    code=cfg["code"],
                    spec_params={},
                    errors=[str(e)],
                )
            )

    # Console output
    print_gyration_table(results)
    print_damping_summary(results)
    print_hydrostatic_table(results)

    # HTML report
    generate_html_report(results, args.output)

    # Summary
    print("\n" + "=" * 80)
    print("Comparison complete.")
    total_errors = sum(len(r.errors) for r in results)
    if total_errors > 0:
        print(f"  {total_errors} warning(s)/error(s) encountered.")
    print(f"  Report: {args.output}")
    print("=" * 80)


if __name__ == "__main__":
    main()
