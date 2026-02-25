#!/usr/bin/env python3
"""
Diffraction Report Computations

ABOUTME: Pure computation functions for derived report data (stability, natural
periods, coupling, peak responses, executive warnings).
Extracted from report_generator.py as part of WRK-591 God Object split.

Imports from report_data_models only (no circular deps).
"""

from __future__ import annotations

import math
from typing import Any, Dict, List, Optional

import numpy as np

from digitalmodel.hydrodynamics.diffraction.diffraction_units import (
    period_s_to_rad_per_s,
    rad_per_s_to_period_s,
    radians_to_degrees,
)
from digitalmodel.hydrodynamics.diffraction.report_data_models import (
    DOF_NAMES,
    DOF_UNITS,
    DiffractionReportData,
    HydrostaticData,
)


def _find_closest_idx(values: List[float], target: float) -> int:
    """Find index of closest value in a list."""
    return min(range(len(values)), key=lambda i: abs(values[i] - target))


def compute_stability(
    hydrostatics: HydrostaticData,
    rho: float = 1025.0,
    g: float = 9.81,
) -> Dict[str, Optional[float]]:
    """Compute stability parameters from hydrostatic data.

    GM_T = C(4,4) / (rho * g * V)  for roll
    GM_L = C(5,5) / (rho * g * V)  for pitch
    BM_T = I_xx / V
    BM_L = I_yy / V
    KB   = z_B (from CoB z-coordinate)
    """
    C = hydrostatics.restoring_matrix
    V = hydrostatics.volume

    result: Dict[str, Optional[float]] = {
        "gm_transverse": None,
        "gm_longitudinal": None,
        "bm_transverse": None,
        "bm_longitudinal": None,
        "kb": None,
    }

    if V <= 0:
        return result

    rho_g_v = rho * g * V
    result["gm_transverse"] = C[3][3] / rho_g_v if rho_g_v > 0 else None
    result["gm_longitudinal"] = C[4][4] / rho_g_v if rho_g_v > 0 else None
    result["bm_transverse"] = hydrostatics.Lxx / V
    result["bm_longitudinal"] = hydrostatics.Lyy / V
    result["kb"] = hydrostatics.centre_of_buoyancy[2]  # z-coordinate

    return result


def compute_radii_of_gyration(
    hydrostatics: HydrostaticData,
) -> Dict[str, float]:
    """Compute radii of gyration from inertia matrix and mass.

    r_xx = sqrt(I_44 / M)
    r_yy = sqrt(I_55 / M)
    r_zz = sqrt(I_66 / M)
    """
    M = hydrostatics.mass
    I = hydrostatics.inertia_matrix
    result: Dict[str, float] = {}

    if M > 0:
        for label, idx in [("r_xx", 3), ("r_yy", 4), ("r_zz", 5)]:
            val = I[idx][idx]
            result[label] = math.sqrt(val / M) if val > 0 else 0.0

    return result


def compute_natural_periods(
    hydrostatics: HydrostaticData,
    added_mass_diagonal: Dict[str, List[float]],
    frequencies_rad_s: List[float],
) -> Dict[str, Optional[float]]:
    """Compute natural periods for restoring DOFs (heave, roll, pitch).

    T_n = 2*pi * sqrt((M_ii + A_ii(omega_n)) / C_ii)

    Uses iterative nearest-frequency approach: sweep A_ii(omega) and find
    intersection where omega^2 = C_ii / (M_ii + A_ii(omega)).
    """
    C = hydrostatics.restoring_matrix
    I = hydrostatics.inertia_matrix
    freq = np.array(frequencies_rad_s)

    result: Dict[str, Optional[float]] = {
        "surge": None, "sway": None,
        "heave": None, "roll": None, "pitch": None,
        "yaw": None,
    }

    # Only compute for DOFs with restoring stiffness
    for dof_name, dof_idx, key in [
        ("heave", 2, "heave"),
        ("roll", 3, "roll"),
        ("pitch", 4, "pitch"),
    ]:
        c_ii = C[dof_idx][dof_idx]
        m_ii = I[dof_idx][dof_idx]

        if c_ii <= 0 or m_ii <= 0:
            continue

        a_ii = np.array(added_mass_diagonal.get(dof_name, []))
        if len(a_ii) != len(freq):
            continue

        # Sweep: find frequency where omega^2 ~ C_ii / (M_ii + A_ii(omega))
        omega_n_sq = c_ii / (m_ii + a_ii)
        omega_n_est = np.sqrt(np.maximum(omega_n_sq, 0))

        # Find intersection: omega_n_est[i] ~ freq[i]
        diff = np.abs(omega_n_est - freq)
        best_idx = int(np.argmin(diff))

        if freq[best_idx] > 0:
            t_n = rad_per_s_to_period_s(freq[best_idx])
            result[key] = float(t_n)

    return result


def compute_peak_responses(
    raw_raos: np.ndarray,
    periods_s: List[float],
    headings_deg: List[float],
) -> Dict[str, Dict[str, float]]:
    """Scan displacement RAO arrays to find peak amplitude per DOF.

    Args:
        raw_raos: Complex displacement RAOs, shape (nfreq, nheading, 6).
                  Rotational DOFs assumed in radians/m (converted to deg/m).
        periods_s: Period array in seconds, length nfreq.
        headings_deg: Heading array in degrees, length nheading.

    Returns:
        Dict of DOF -> {amplitude, period_s, heading_deg, unit}.
    """
    result: Dict[str, Dict[str, float]] = {}

    for i, dof in enumerate(DOF_NAMES):
        amp = np.abs(raw_raos[:, :, i])
        # Convert rotational RAOs from rad/m to deg/m
        if i >= 3:
            amp = radians_to_degrees(amp)
        if amp.size == 0 or np.max(amp) < 1e-12:
            continue

        peak_idx = np.unravel_index(np.argmax(amp), amp.shape)
        peak_amp = float(amp[peak_idx])
        peak_period = float(periods_s[peak_idx[0]])
        peak_heading = float(headings_deg[peak_idx[1]])

        result[dof] = {
            "amplitude": round(peak_amp, 4),
            "period_s": round(peak_period, 2),
            "heading_deg": round(peak_heading, 1),
            "unit": DOF_UNITS[i],
        }

    return result


def compute_coupling_significance(
    added_mass_diagonal: Dict[str, List[float]],
    damping_diagonal: Dict[str, List[float]],
    added_mass_full: Optional[np.ndarray] = None,
    damping_full: Optional[np.ndarray] = None,
) -> Dict[str, float]:
    """Scan off-diagonal A_ij, B_ij for significance.

    Threshold: |A_ij(omega)| / max(|A_ii(omega)|, |A_jj(omega)|) > 5%.

    Returns dict like {"A_15": 0.12, "A_24": 0.08} with max ratios.
    """
    result: Dict[str, float] = {}

    if added_mass_full is None:
        return result

    # added_mass_full shape: (nfreq, 6, 6)
    for i in range(6):
        for j in range(6):
            if i == j:
                continue
            off_diag = np.abs(added_mass_full[:, i, j])
            diag_i = np.abs(added_mass_full[:, i, i])
            diag_j = np.abs(added_mass_full[:, j, j])
            denominator = np.maximum(diag_i, diag_j)
            denominator = np.where(denominator < 1e-30, 1e-30, denominator)
            ratio = off_diag / denominator
            max_ratio = float(np.max(ratio))
            if max_ratio > 0.05:
                key = f"A_{i+1}{j+1}"
                result[key] = round(max_ratio, 4)

    if damping_full is not None:
        for i in range(6):
            for j in range(6):
                if i == j:
                    continue
                off_diag = np.abs(damping_full[:, i, j])
                diag_i = np.abs(damping_full[:, i, i])
                diag_j = np.abs(damping_full[:, j, j])
                denominator = np.maximum(diag_i, diag_j)
                denominator = np.where(
                    denominator < 1e-30, 1e-30, denominator
                )
                ratio = off_diag / denominator
                max_ratio = float(np.max(ratio))
                if max_ratio > 0.05:
                    key = f"B_{i+1}{j+1}"
                    result[key] = round(max_ratio, 4)

    return result


def generate_executive_warnings(
    report_data: DiffractionReportData,
) -> List[str]:
    """Generate auto-alerts for the executive summary.

    Checks: low GM, mesh quality issues, low roll damping, solver disagreement.
    """
    warnings: List[str] = []

    # GM check
    if report_data.gm_transverse is not None:
        if report_data.gm_transverse <= 0:
            warnings.append(
                f"CRITICAL: Negative GM_T ({report_data.gm_transverse:.3f}m) "
                "-- vessel is statically unstable in roll."
            )
        elif report_data.gm_transverse < 1.0:
            warnings.append(
                f"WARNING: Low GM_T ({report_data.gm_transverse:.3f}m) "
                "-- below DNV-OS-C301 minimum of 1.0m for mobile offshore units."
            )

    # Mesh quality
    if report_data.mesh_quality:
        if report_data.mesh_quality.area_ratio > 10:
            warnings.append(
                f"NOTE: Global mesh area ratio is "
                f"{report_data.mesh_quality.area_ratio:.1f}. "
                "Gradual size variation is acceptable; "
                "only abrupt transitions between adjacent panels "
                "affect BEM solver accuracy."
            )
        if report_data.mesh_quality.panel_count < 100:
            warnings.append(
                f"WARNING: Low panel count "
                f"({report_data.mesh_quality.panel_count}) -- "
                "mesh may be too coarse for accurate results."
            )

    # Roll damping
    if (
        report_data.roll_damping
        and report_data.roll_damping.zeta_at_peak is not None
    ):
        zeta = report_data.roll_damping.zeta_at_peak
        if zeta < 2.0:
            warnings.append(
                f"WARNING: Low radiation roll damping "
                f"({zeta:.2f}% critical at resonance) -- "
                "viscous damping contributions are essential. "
                "Add bilge keel, skin friction, and eddy-making "
                "damping per DNV-RP-C205 S7."
            )

    return warnings
