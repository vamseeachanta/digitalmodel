#!/usr/bin/env python3
"""
Diffraction Report Data Extractors

ABOUTME: Functions to extract report data from OrcFxAPI Diffraction objects
(.owr files) and from multi-solver benchmark results.
Extracted from report_generator.py as part of WRK-591 God Object split.

Imports from report_data_models and report_computations.
"""

from __future__ import annotations

from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

import numpy as np

from digitalmodel.hydrodynamics.diffraction.diffraction_units import (
    complex_phase_degrees,
    hz_to_rad_per_s,
    rad_per_s_to_period_s,
    radians_to_degrees,
)
from digitalmodel.hydrodynamics.diffraction.report_data_models import (
    DOF_NAMES,
    DiffractionReportData,
    HydrostaticData,
    LoadRAOData,
    MeshQualityData,
    RollDampingData,
)
from digitalmodel.hydrodynamics.diffraction.report_computations import (
    compute_coupling_significance,
    compute_natural_periods,
    compute_peak_responses,
    compute_radii_of_gyration,
    compute_stability,
    generate_executive_warnings,
)


def _round_2d(arr: np.ndarray, decimals: int = 6) -> List[List[float]]:
    """Round 2D array to nested list."""
    return [[round(float(v), decimals) for v in row] for row in arr]


def extract_report_data_from_owr(owr_path: Path) -> DiffractionReportData:
    """Extract all report data from an OrcaWave results file (.owr).

    Args:
        owr_path: Path to .owr file.

    Returns:
        DiffractionReportData populated from the results.
    """
    import OrcFxAPI

    d = OrcFxAPI.Diffraction()
    d.LoadResults(str(Path(owr_path).resolve()))

    # --- Frequency/heading grids ---
    # CRITICAL: frequencies from OrcFxAPI are in Hz descending
    freq_hz = np.array(d.frequencies)
    freq_rad_s = hz_to_rad_per_s(freq_hz)
    sort_idx = np.argsort(freq_rad_s)
    freq_rad_s = freq_rad_s[sort_idx]
    periods_s = rad_per_s_to_period_s(freq_rad_s)
    headings = np.array(d.headings)

    # --- Hydrostatics ---
    hs = d.hydrostaticResults[0]
    restoring = hs["restoringMatrix"]
    inertia = hs["inertiaMatrix"]

    hydrostatics = HydrostaticData(
        volume=float(hs["volume"]),
        mass=float(hs["mass"]),
        centre_of_buoyancy=hs["centreOfBuoyancy"].tolist(),
        centre_of_mass=hs["centreOfMass"].tolist(),
        waterplane_area=float(hs["Awp"]),
        Lxx=float(hs["Lxx"]),
        Lyy=float(hs["Lyy"]),
        Lxy=float(hs["Lxy"]),
        centre_of_floatation=hs["centreOfFloatation"].tolist(),
        restoring_matrix=restoring.tolist(),
        inertia_matrix=inertia.tolist(),
    )

    # --- Roll damping ---
    rdpc = d.rollDampingPercentCritical[sort_idx, 0]  # sort by frequency
    added_mass = np.array(d.addedMass)[sort_idx]  # (nfreq, 6, 6)
    damping = np.array(d.damping)[sort_idx]  # (nfreq, 6, 6)

    A_44 = added_mass[:, 3, 3]
    B_44 = damping[:, 3, 3]
    C_44 = float(restoring[3, 3])
    I_44 = float(inertia[3, 3])

    # Find peak roll RAO period
    raw_raos = np.array(d.displacementRAOs)  # (nheading, nfreq, 6)
    raw_raos = np.transpose(raw_raos, (1, 0, 2))  # (nfreq, nheading, 6)
    raw_raos = raw_raos[sort_idx, :, :]
    roll_amp = np.abs(raw_raos[:, :, 3])  # roll DOF
    # Convert rotational RAOs from rad/m to deg/m
    roll_amp_deg = radians_to_degrees(roll_amp)

    peak_idx = np.unravel_index(np.argmax(roll_amp_deg), roll_amp_deg.shape)
    peak_period = float(periods_s[peak_idx[0]]) if np.max(roll_amp_deg) > 1e-10 else None
    zeta_at_peak = float(rdpc[peak_idx[0]]) if peak_period else None

    roll_damping = RollDampingData(
        frequencies_rad_s=freq_rad_s.tolist(),
        periods_s=periods_s.tolist(),
        roll_damping_percent_critical=rdpc.tolist(),
        B_44=B_44.tolist(),
        A_44=A_44.tolist(),
        C_44=C_44,
        I_44=I_44,
        peak_roll_rao_period=peak_period,
        zeta_at_peak=zeta_at_peak,
    )

    # --- Load RAOs (diffraction method) ---
    raw_load = np.array(d.loadRAOsDiffraction)  # (nheading, nfreq, 6)
    raw_load = np.transpose(raw_load, (1, 0, 2))  # (nfreq, nheading, 6)
    raw_load = raw_load[sort_idx, :, :]

    load_amp: Dict[str, List[List[float]]] = {}
    load_phase: Dict[str, List[List[float]]] = {}
    for i, dof in enumerate(DOF_NAMES):
        amp = np.abs(raw_load[:, :, i])
        phase = complex_phase_degrees(raw_load[:, :, i])
        load_amp[dof] = _round_2d(amp)
        load_phase[dof] = _round_2d(phase)

    load_raos = LoadRAOData(
        method="diffraction",
        frequencies_rad_s=freq_rad_s.tolist(),
        periods_s=periods_s.tolist(),
        headings_deg=headings.tolist(),
        amplitude=load_amp,
        phase_deg=load_phase,
    )

    # --- Mesh quality ---
    panels = d.panelGeometry
    areas = np.array([p["area"] for p in panels])
    mesh_quality = MeshQualityData(
        panel_count=len(panels),
        mean_area=float(np.mean(areas)),
        min_area=float(np.min(areas)),
        max_area=float(np.max(areas)),
        area_ratio=float(np.max(areas) / np.min(areas)) if np.min(areas) > 0 else float("inf"),
    )

    # --- Infinite frequency added mass ---
    inf_am = np.array(d.infiniteFrequencyAddedMass)

    # --- Full coefficient arrays for diagonal plots and coupling ---
    added_mass_diag: Dict[str, List[float]] = {}
    damping_diag: Dict[str, List[float]] = {}
    for i, dof in enumerate(DOF_NAMES):
        added_mass_diag[dof] = added_mass[:, i, i].tolist()
        damping_diag[dof] = damping[:, i, i].tolist()

    # --- Stability ---
    stability = compute_stability(hydrostatics)
    rog = compute_radii_of_gyration(hydrostatics)

    # --- Natural periods ---
    nat_periods = compute_natural_periods(
        hydrostatics, added_mass_diag, freq_rad_s.tolist(),
    )

    # --- Coupling significance ---
    coupling = compute_coupling_significance(
        added_mass_diag, damping_diag,
        added_mass_full=added_mass,
        damping_full=damping,
    )

    # --- Peak displacement responses ---
    peak_resp = compute_peak_responses(raw_raos, periods_s.tolist(), headings.tolist())

    # Use filename stem, cleaned up (remove _ground_truth suffix)
    vessel_name = owr_path.stem.replace("_ground_truth", "").replace("_", " ")

    data = DiffractionReportData(
        vessel_name=vessel_name,
        report_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        hydrostatics=hydrostatics,
        roll_damping=roll_damping,
        load_raos=load_raos,
        mesh_quality=mesh_quality,
        solver_names=["OrcaWave"],
        source_file=str(owr_path),
        frequencies_rad_s=freq_rad_s.tolist(),
        periods_s=periods_s.tolist(),
        headings_deg=headings.tolist(),
        infinite_freq_added_mass=inf_am.tolist(),
        added_mass_diagonal=added_mass_diag,
        damping_diagonal=damping_diag,
        gm_transverse=stability["gm_transverse"],
        gm_longitudinal=stability["gm_longitudinal"],
        bm_transverse=stability["bm_transverse"],
        bm_longitudinal=stability["bm_longitudinal"],
        kb=stability["kb"],
        radii_of_gyration=rog,
        natural_periods=nat_periods,
        peak_responses=peak_resp,
        coupling_significance=coupling,
    )
    data.executive_warnings = generate_executive_warnings(data)
    return data


def build_report_data_from_solver_results(
    solver_results: Dict[str, Any],
    owr_path: Optional[Path] = None,
    vessel_name: Optional[str] = None,
) -> DiffractionReportData:
    """Build a DiffractionReportData from multi-solver results.

    If *owr_path* is provided, delegates to ``extract_report_data_from_owr``
    for full physics data (hydrostatics, load RAOs, roll damping, etc.).
    Otherwise builds a minimal data model from the first solver's
    ``DiffractionResults`` (frequencies, added-mass/damping diagonals only).

    Args:
        solver_results: Mapping of solver name to DiffractionResults.
        owr_path: Optional path to OrcaWave .owr file for full extraction.
        vessel_name: Override vessel name (falls back to first solver's).

    Returns:
        Populated DiffractionReportData.
    """
    if owr_path is not None:
        data = extract_report_data_from_owr(Path(owr_path))
        data.solver_names = list(solver_results.keys())
        if vessel_name:
            data.vessel_name = vessel_name
        return data

    # Minimal path: extract from first solver's DiffractionResults
    first_name = next(iter(solver_results))
    dr = solver_results[first_name]

    freq_rad_s = dr.added_mass.frequencies.values.tolist()
    periods_s = rad_per_s_to_period_s(np.array(freq_rad_s)).tolist()
    # Extract headings from first solver (available in RAO structure)
    headings_list = dr.raos.surge.headings.values.tolist()

    added_mass_diag: Dict[str, List[float]] = {}
    damping_diag: Dict[str, List[float]] = {}
    for i, dof in enumerate(DOF_NAMES):
        added_mass_diag[dof] = [
            float(hm.matrix[i, i]) for hm in dr.added_mass.matrices
        ]
        damping_diag[dof] = [
            float(hm.matrix[i, i]) for hm in dr.damping.matrices
        ]

    return DiffractionReportData(
        vessel_name=vessel_name or dr.vessel_name,
        solver_names=list(solver_results.keys()),
        frequencies_rad_s=freq_rad_s,
        periods_s=periods_s,
        headings_deg=headings_list,
        added_mass_diagonal=added_mass_diag,
        damping_diagonal=damping_diag,
    )
