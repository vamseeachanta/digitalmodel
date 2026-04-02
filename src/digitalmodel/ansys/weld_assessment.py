# ABOUTME: Weld joint assessment — hotspot stress, stress linearization, SCL evaluation
# ABOUTME: Implements ASME VIII Div 2 Part 5 and DNV-RP-C203 methodologies in APDL

"""
Weld Assessment
===============

Generates ANSYS APDL commands and post-processing procedures for weld joint
assessment in accordance with:

    - ASME BPVC Section VIII Division 2, Part 5 (stress classification)
    - DNV-RP-C203 (fatigue assessment of offshore structures)
    - IIW (International Institute of Welding) hotspot stress method

Methodologies:

    1. **Hotspot Stress Extraction** — Surface stress extrapolation
       (Type A: 0.4t/1.0t or 0.5t/1.5t reference points)
    2. **Through-Thickness Linearization** — Membrane + bending decomposition
       along stress classification lines (SCLs)
    3. **Weld Toe Stress Concentration** — Parametric SCF equations
    4. **ASME Stress Categories** — Pm, PL, PL+Pb, PL+Pb+Q classification

All functions produce APDL command text or post-processing result data.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional

import numpy as np


# ---------------------------------------------------------------------------
# Configuration dataclasses
# ---------------------------------------------------------------------------

@dataclass
class WeldGeometry:
    """Weld joint geometry parameters."""
    joint_type: str = "butt"  # butt, fillet, partial_pen, full_pen
    plate_thickness_mm: float = 25.0
    weld_toe_angle_deg: float = 45.0
    weld_toe_radius_mm: float = 1.0
    weld_leg_size_mm: float = 8.0  # for fillet welds
    attachment_length_mm: float = 100.0
    misalignment_mm: float = 0.0


@dataclass
class HotspotConfig:
    """Configuration for hotspot stress extraction."""
    method: str = "type_a"  # type_a, type_b, type_c
    extrapolation_points: str = "0.4t_1.0t"  # 0.4t_1.0t or 0.5t_1.5t
    plate_thickness_mm: float = 25.0
    surface: str = "outer"  # outer, inner, both
    num_path_points: int = 48


@dataclass
class StressClassificationLine:
    """Stress classification line (SCL) definition."""
    scl_id: str = "SCL_1"
    start_x_mm: float = 0.0
    start_y_mm: float = 0.0
    start_z_mm: float = 0.0
    end_x_mm: float = 0.0
    end_y_mm: float = 25.0  # through thickness
    end_z_mm: float = 0.0
    num_divisions: int = 48
    description: str = "Shell mid-length"


@dataclass
class LinearizedStress:
    """Linearized stress components from SCL evaluation."""
    scl_id: str = ""
    membrane_stress_mpa: float = 0.0
    bending_stress_mpa: float = 0.0
    peak_stress_mpa: float = 0.0
    membrane_plus_bending_mpa: float = 0.0
    total_stress_mpa: float = 0.0


@dataclass
class ASMEStressLimits:
    """ASME VIII Div 2 stress category limits."""
    Sm: float = 138.0  # Design stress intensity at temperature
    Sy: float = 260.0  # Yield strength at temperature
    Pm_limit: float = 138.0  # Sm
    PL_limit: float = 207.0  # 1.5 * Sm
    PL_Pb_limit: float = 207.0  # 1.5 * Sm
    PL_Pb_Q_limit: float = 414.0  # 3.0 * Sm (for fatigue screening)


# ---------------------------------------------------------------------------
# Weld assessment generator
# ---------------------------------------------------------------------------

class WeldAssessmentGenerator:
    """Generate APDL commands for weld joint stress assessment."""

    def generate_hotspot_stress_extraction(
        self, config: HotspotConfig, weld: WeldGeometry
    ) -> str:
        """Generate APDL commands for hotspot stress extraction.

        Uses surface stress extrapolation per DNV-RP-C203.
        Type A: perpendicular to weld toe at plate surface.

        Returns
        -------
        str
            APDL commands for path-based hotspot stress calculation.
        """
        t = config.plate_thickness_mm

        if config.extrapolation_points == "0.4t_1.0t":
            d1 = 0.4 * t
            d2 = 1.0 * t
            # Linear extrapolation: sigma_hs = 1.67*sigma_0.4t - 0.67*sigma_1.0t
            c1, c2 = 1.67, -0.67
        else:
            d1 = 0.5 * t
            d2 = 1.5 * t
            # sigma_hs = 1.5*sigma_0.5t - 0.5*sigma_1.5t
            c1, c2 = 1.50, -0.50

        lines = [
            "/POST1",
            "SET,LAST",
            "",
            f"! --- Hotspot Stress Extraction ({config.method.upper()}) ---",
            f"! Plate thickness: {t} mm",
            f"! Extrapolation: {config.extrapolation_points}",
            f"! Reference points at {d1:.1f} mm and {d2:.1f} mm from weld toe",
            "",
            "! Define path perpendicular to weld toe along plate surface",
            f"PATH,HS_PATH,3,,{config.num_path_points}",
            "! Point 1: At weld toe",
            "PPATH,1,,WELD_TOE_X,WELD_TOE_Y,WELD_TOE_Z",
            f"! Point 2: At {d1:.1f} mm from toe",
            f"PPATH,2,,HS_REF1_X,HS_REF1_Y,HS_REF1_Z",
            f"! Point 3: At {d2:.1f} mm from toe",
            f"PPATH,3,,HS_REF2_X,HS_REF2_Y,HS_REF2_Z",
            "",
            "! Map stress onto path",
            "PDEF,SEQV_HS,S,EQV",
            "PDEF,SX_HS,S,X",
            "PDEF,SY_HS,S,Y",
            "PDEF,SZ_HS,S,Z",
            "",
            "! Extrapolation formula:",
            f"! sigma_hs = {c1:.2f} * sigma_ref1 + {c2:.2f} * sigma_ref2",
            "",
            "! Extract reference point stresses using *GET",
            "*GET,S_REF1,PATH,,SEQV_HS,AT_POINT,2",
            "*GET,S_REF2,PATH,,SEQV_HS,AT_POINT,3",
            f"*SET,SIGMA_HS,{c1}*S_REF1+({c2})*S_REF2",
            "",
            "! Report hotspot stress",
            "*MSG,INFO,SIGMA_HS",
            "Hotspot stress = %G MPa",
            "",
        ]

        return "\n".join(lines)

    def generate_stress_linearization(
        self, scl: StressClassificationLine
    ) -> str:
        """Generate APDL commands for through-thickness stress linearization.

        Implements ASME VIII Div 2 Part 5 stress classification
        (membrane + bending decomposition).

        Returns
        -------
        str
            APDL commands for SCL path and linearization.
        """
        lines = [
            "/POST1",
            "SET,LAST",
            "",
            f"! --- Stress Classification Line: {scl.scl_id} ---",
            f"! {scl.description}",
            f"! From ({scl.start_x_mm}, {scl.start_y_mm}, {scl.start_z_mm})",
            f"!   To ({scl.end_x_mm}, {scl.end_y_mm}, {scl.end_z_mm})",
            "",
            f"PATH,{scl.scl_id},2,,{scl.num_divisions}",
            f"PPATH,1,,{scl.start_x_mm},{scl.start_y_mm},{scl.start_z_mm}",
            f"PPATH,2,,{scl.end_x_mm},{scl.end_y_mm},{scl.end_z_mm}",
            "",
            "! Map all stress components onto path",
            f"PDEF,SX_{scl.scl_id},S,X",
            f"PDEF,SY_{scl.scl_id},S,Y",
            f"PDEF,SZ_{scl.scl_id},S,Z",
            f"PDEF,SXY_{scl.scl_id},S,XY",
            f"PDEF,SYZ_{scl.scl_id},S,YZ",
            f"PDEF,SXZ_{scl.scl_id},S,XZ",
            "",
            "! Linearize: membrane + bending + peak decomposition",
            "PRSECT  ! Print linearized stresses",
            "",
            "! Store linearized values",
            f"*GET,PM_{scl.scl_id},PRSECT,,MEMBRANE",
            f"*GET,PB_{scl.scl_id},PRSECT,,BENDING",
            f"*GET,PMB_{scl.scl_id},PRSECT,,MEMB_PLUS_BEND",
            f"*GET,PEAK_{scl.scl_id},PRSECT,,PEAK",
            f"*GET,TOTAL_{scl.scl_id},PRSECT,,TOTAL",
            "",
        ]

        return "\n".join(lines)

    def generate_weld_toe_scf(self, weld: WeldGeometry) -> str:
        """Generate APDL APDL parameter calculations for weld toe SCF.

        Uses Mashiri-Zhao parametric SCF equations for various joint types.

        Returns
        -------
        str
            APDL *SET commands computing SCF parameters.
        """
        t = weld.plate_thickness_mm
        theta = weld.weld_toe_angle_deg
        rho = weld.weld_toe_radius_mm

        # Simplified parametric SCF (IIW approach)
        # Kt = 1 + 0.388 * (theta/45)^0.37 * (t/rho)^0.454
        # (Simplified Monahan equation)

        lines = [
            f"! --- Weld Toe Stress Concentration Factor ---",
            f"! Joint type: {weld.joint_type}",
            f"! Plate thickness: {t} mm",
            f"! Weld toe angle: {theta}°",
            f"! Weld toe radius: {rho} mm",
            "",
            f"*SET,T_PLATE,{t}",
            f"*SET,THETA_TOE,{theta}",
            f"*SET,RHO_TOE,{rho}",
            "",
            "! Monahan SCF equation (simplified)",
            "*SET,SCF_K,1+0.388*(THETA_TOE/45)**0.37*(T_PLATE/RHO_TOE)**0.454",
            "",
            "*MSG,INFO,SCF_K",
            "Weld toe SCF = %G",
            "",
            "! Apply SCF to hotspot stress",
            "*SET,S_NOTCH,SIGMA_HS*SCF_K",
            "*MSG,INFO,S_NOTCH",
            "Notch stress = %G MPa",
            "",
        ]

        return "\n".join(lines)

    def generate_asme_stress_check(
        self, scl: StressClassificationLine, limits: ASMEStressLimits
    ) -> str:
        """Generate APDL commands to check ASME VIII Div 2 stress limits.

        Evaluates:
            - Pm <= Sm (general primary membrane)
            - PL <= 1.5*Sm (local primary membrane)
            - PL + Pb <= 1.5*Sm (primary membrane + bending)
            - PL + Pb + Q <= 3*Sm (fatigue screening — secondary range)

        Returns
        -------
        str
            APDL commands with stress limit comparisons.
        """
        lines = [
            f"! --- ASME VIII Div 2 Part 5 Stress Check: {scl.scl_id} ---",
            f"! Sm = {limits.Sm} MPa, Sy = {limits.Sy} MPa",
            "",
            f"! Pm limit (general primary membrane): {limits.Pm_limit} MPa",
            f"*SET,PM_RATIO,PM_{scl.scl_id}/{limits.Pm_limit}",
            "*MSG,INFO,PM_RATIO",
            "Pm utilization = %G",
            "",
            f"! PL limit (local primary membrane): {limits.PL_limit} MPa",
            f"*SET,PL_RATIO,PM_{scl.scl_id}/{limits.PL_limit}",
            "*MSG,INFO,PL_RATIO",
            "PL utilization = %G",
            "",
            f"! PL+Pb limit: {limits.PL_Pb_limit} MPa",
            f"*SET,PLPb_RATIO,PMB_{scl.scl_id}/{limits.PL_Pb_limit}",
            "*MSG,INFO,PLPb_RATIO",
            "PL+Pb utilization = %G",
            "",
            f"! PL+Pb+Q limit (secondary range): {limits.PL_Pb_Q_limit} MPa",
            f"*SET,PLPbQ_RATIO,TOTAL_{scl.scl_id}/{limits.PL_Pb_Q_limit}",
            "*MSG,INFO,PLPbQ_RATIO",
            "PL+Pb+Q utilization = %G",
            "",
            "! Check pass/fail",
            f"*IF,PM_{scl.scl_id},GT,{limits.Pm_limit},THEN",
            f"  *MSG,WARN",
            f"  Pm EXCEEDS allowable at {scl.scl_id}",
            "*ENDIF",
            "",
        ]

        return "\n".join(lines)

    def calculate_linearized_stresses(
        self, stress_through_thickness: list[float], thickness_mm: float
    ) -> LinearizedStress:
        """Calculate linearized stress components numerically.

        Performs trapezoidal integration through the thickness to
        decompose total stress into membrane + bending + peak.

        Parameters
        ----------
        stress_through_thickness : list[float]
            Stress values at equally-spaced through-thickness points
            (from inner surface to outer surface).
        thickness_mm : float
            Total wall thickness.

        Returns
        -------
        LinearizedStress
            Decomposed stress components.
        """
        if not stress_through_thickness:
            return LinearizedStress()

        n = len(stress_through_thickness)
        s = np.array(stress_through_thickness)
        t = thickness_mm

        # Coordinate through thickness: -t/2 to +t/2
        x = np.linspace(-t / 2, t / 2, n)

        # Membrane stress: (1/t) * integral(sigma dx) over thickness
        sigma_m = float(np.trapz(s, x) / t)

        # Bending stress: (6/t^2) * integral(sigma * x dx) over thickness
        # This gives the max bending stress at the surface
        sigma_b = float(6.0 * np.trapz(s * x, x) / (t ** 2))

        # Peak stress at surfaces
        sigma_mb = sigma_m + sigma_b
        sigma_total_outer = float(s[-1])
        sigma_peak = sigma_total_outer - sigma_mb

        return LinearizedStress(
            membrane_stress_mpa=sigma_m,
            bending_stress_mpa=sigma_b,
            peak_stress_mpa=sigma_peak,
            membrane_plus_bending_mpa=sigma_mb,
            total_stress_mpa=sigma_total_outer,
        )
