# ABOUTME: ASME Section VIII Div 1/2 pressure vessel FEA setup — APDL command generation
# ABOUTME: Shell thickness calcs per UG-27/UG-32, nozzle/thermal/wind load modeling

"""
Pressure Vessel Analysis
========================

Generates ANSYS APDL commands for pressure vessel finite element analysis
in accordance with ASME BPVC Section VIII Division 1 and Division 2.

Capabilities:

    - Minimum wall thickness per UG-27 (cylindrical shells) and UG-32 (heads)
    - Internal/external pressure loading
    - Nozzle reinforcement loads (WRC 537 / FEA approach)
    - Thermal gradient loading (operating vs ambient)
    - Wind and seismic load application
    - ASME stress classification line paths

Design basis defaults are for typical offshore pressure vessels:

    - Material: SA-516 Grade 70 (carbon steel)
    - Design pressure: 10 MPa (100 bar)
    - Design temperature: 250°C
    - Corrosion allowance: 3.0 mm
    - Joint efficiency: 1.0 (full RT)

All functions return APDL command text strings.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Optional


# ---------------------------------------------------------------------------
# Configuration dataclasses
# ---------------------------------------------------------------------------

@dataclass
class VesselGeometry:
    """Pressure vessel geometric parameters."""
    inner_diameter_mm: float = 1500.0
    shell_length_mm: float = 5000.0
    wall_thickness_mm: float = 30.0
    head_type: str = "2:1_ellipsoidal"  # hemispherical, 2:1_ellipsoidal, torispherical
    head_thickness_mm: Optional[float] = None  # None = same as shell
    corrosion_allowance_mm: float = 3.0
    nozzle_locations: list[NozzleConfig] = field(default_factory=list)


@dataclass
class NozzleConfig:
    """Nozzle geometry and location on vessel."""
    nozzle_id: str = "N1"
    outer_diameter_mm: float = 168.3  # 6" NPS Sch 80
    wall_thickness_mm: float = 10.97
    projection_mm: float = 200.0
    elevation_mm: float = 2500.0  # from vessel tangent line
    azimuth_deg: float = 0.0
    reinforcement_pad_mm: float = 0.0  # additional pad thickness


@dataclass
class DesignConditions:
    """ASME design conditions for pressure vessel."""
    design_pressure_mpa: float = 10.0
    design_temperature_c: float = 250.0
    operating_pressure_mpa: float = 8.0
    operating_temperature_c: float = 200.0
    ambient_temperature_c: float = 25.0
    hydrotest_pressure_mpa: Optional[float] = None  # None = 1.3 * design
    allowable_stress_mpa: float = 138.0  # SA-516 Gr 70 at 250C
    yield_strength_mpa: float = 260.0
    tensile_strength_mpa: float = 485.0
    joint_efficiency: float = 1.0
    material_name: str = "SA-516 Gr 70"


@dataclass
class NozzleLoad:
    """External loads applied at a nozzle."""
    nozzle_id: str = "N1"
    axial_force_n: float = 0.0  # Along nozzle axis
    shear_fy_n: float = 0.0
    shear_fz_n: float = 0.0
    bending_my_nm: float = 0.0
    bending_mz_nm: float = 0.0
    torsion_mx_nm: float = 0.0


@dataclass
class WindSeismicLoad:
    """Wind or seismic load parameters."""
    wind_speed_ms: float = 44.0  # 3-sec gust, m/s
    wind_drag_coefficient: float = 0.7
    seismic_g_horizontal: float = 0.15
    seismic_g_vertical: float = 0.10
    vessel_weight_n: float = 50000.0
    load_type: str = "wind"  # wind, seismic


# ---------------------------------------------------------------------------
# ASME thickness calculations
# ---------------------------------------------------------------------------

class ASMEThicknessCalc:
    """ASME Section VIII Div 1 minimum thickness calculations."""

    def ug27_cylindrical_shell(
        self, pressure_mpa: float, inner_radius_mm: float,
        allowable_stress_mpa: float, joint_efficiency: float = 1.0,
        corrosion_allowance_mm: float = 3.0,
    ) -> dict[str, float]:
        """UG-27 minimum thickness for cylindrical shell under internal pressure.

        t = (P * R) / (S * E - 0.6 * P) + CA

        Parameters
        ----------
        pressure_mpa : float
            Design pressure in MPa.
        inner_radius_mm : float
            Inner radius in mm.
        allowable_stress_mpa : float
            Allowable stress at design temperature.
        joint_efficiency : float
            Weld joint efficiency (0.65 to 1.0).
        corrosion_allowance_mm : float
            Corrosion allowance in mm.

        Returns
        -------
        dict with t_required, t_with_ca, pressure_rating keys.
        """
        S = allowable_stress_mpa
        E = joint_efficiency
        P = pressure_mpa
        R = inner_radius_mm

        t_required = (P * R) / (S * E - 0.6 * P)
        t_with_ca = t_required + corrosion_allowance_mm

        # Back-calculate MAWP for a given thickness
        return {
            "t_required_mm": t_required,
            "t_with_ca_mm": t_with_ca,
            "formula": "UG-27(c)(1): t = PR / (SE - 0.6P)",
        }

    def ug32_ellipsoidal_head(
        self, pressure_mpa: float, inner_diameter_mm: float,
        allowable_stress_mpa: float, joint_efficiency: float = 1.0,
        corrosion_allowance_mm: float = 3.0,
        aspect_ratio: float = 2.0,
    ) -> dict[str, float]:
        """UG-32(d) minimum thickness for ellipsoidal head.

        t = (P * D * K) / (2 * S * E - 0.2 * P) + CA

        K = (1/6) * [2 + (D/2h)^2] for 2:1 ellipsoidal K = 1.0

        Returns
        -------
        dict with t_required, t_with_ca keys.
        """
        S = allowable_stress_mpa
        E = joint_efficiency
        P = pressure_mpa
        D = inner_diameter_mm

        K = (1.0 / 6.0) * (2.0 + aspect_ratio ** 2)
        t_required = (P * D * K) / (2.0 * S * E - 0.2 * P)
        t_with_ca = t_required + corrosion_allowance_mm

        return {
            "t_required_mm": t_required,
            "t_with_ca_mm": t_with_ca,
            "K_factor": K,
            "formula": "UG-32(d): t = PDK / (2SE - 0.2P)",
        }

    def ug32_hemispherical_head(
        self, pressure_mpa: float, inner_radius_mm: float,
        allowable_stress_mpa: float, joint_efficiency: float = 1.0,
        corrosion_allowance_mm: float = 3.0,
    ) -> dict[str, float]:
        """UG-32(f) minimum thickness for hemispherical head.

        t = (P * R) / (2 * S * E - 0.2 * P) + CA
        """
        S = allowable_stress_mpa
        E = joint_efficiency
        P = pressure_mpa
        R = inner_radius_mm

        t_required = (P * R) / (2.0 * S * E - 0.2 * P)
        t_with_ca = t_required + corrosion_allowance_mm

        return {
            "t_required_mm": t_required,
            "t_with_ca_mm": t_with_ca,
            "formula": "UG-32(f): t = PR / (2SE - 0.2P)",
        }

    def calculate_mawp(
        self, wall_thickness_mm: float, inner_radius_mm: float,
        allowable_stress_mpa: float, joint_efficiency: float = 1.0,
        corrosion_allowance_mm: float = 3.0,
    ) -> float:
        """Calculate Maximum Allowable Working Pressure (MAWP) per UG-27.

        Inverse of UG-27 thickness formula:
            MAWP = S * E * t_corr / (R + 0.6 * t_corr)

        Parameters
        ----------
        wall_thickness_mm : float
            Actual wall thickness in mm.
        inner_radius_mm : float
            Inner radius in mm.
        allowable_stress_mpa : float
            Allowable stress at design temperature.
        joint_efficiency : float
            Weld joint efficiency.
        corrosion_allowance_mm : float
            Corrosion allowance in mm.

        Returns
        -------
        float
            MAWP in MPa.

        Raises
        ------
        ValueError
            If corroded thickness is zero or negative.
        """
        t_corr = wall_thickness_mm - corrosion_allowance_mm
        if t_corr <= 0:
            raise ValueError(
                f"corroded thickness is {t_corr:.2f} mm "
                f"(wall={wall_thickness_mm}, CA={corrosion_allowance_mm})"
            )
        S = allowable_stress_mpa
        E = joint_efficiency
        R = inner_radius_mm
        return (S * E * t_corr) / (R + 0.6 * t_corr)

    def check_nozzle_reinforcement(
        self,
        shell_inner_radius_mm: float,
        shell_thickness_mm: float,
        nozzle_outer_diameter_mm: float,
        nozzle_thickness_mm: float,
        design_pressure_mpa: float,
        allowable_stress_mpa: float,
        joint_efficiency: float = 1.0,
        corrosion_allowance_mm: float = 3.0,
        reinforcement_pad_thickness_mm: float = 0.0,
    ) -> dict[str, float | bool]:
        """Check nozzle reinforcement per ASME VIII Div 1 UG-37.

        Uses the equal-area replacement method:
        - Area removed (A_required) = d * t_r
        - Area available = excess shell area + excess nozzle area + pad area

        Parameters
        ----------
        shell_inner_radius_mm : float
            Shell inner radius.
        shell_thickness_mm : float
            Actual shell thickness.
        nozzle_outer_diameter_mm : float
            Nozzle OD.
        nozzle_thickness_mm : float
            Nozzle wall thickness.
        design_pressure_mpa : float
            Internal design pressure.
        allowable_stress_mpa : float
            Allowable stress at design temperature.
        joint_efficiency : float
            Weld joint efficiency.
        corrosion_allowance_mm : float
            Corrosion allowance.
        reinforcement_pad_thickness_mm : float
            Additional reinforcement pad thickness (0 = none).

        Returns
        -------
        dict with area_required_mm2, area_available_mm2, is_adequate, and details.
        """
        CA = corrosion_allowance_mm
        # Corroded dimensions
        t_shell = shell_thickness_mm - CA
        t_nozzle = nozzle_thickness_mm - CA
        d = nozzle_outer_diameter_mm - 2.0 * nozzle_thickness_mm  # nozzle ID

        # Required shell thickness from UG-27
        t_r = self.ug27_cylindrical_shell(
            pressure_mpa=design_pressure_mpa,
            inner_radius_mm=shell_inner_radius_mm,
            allowable_stress_mpa=allowable_stress_mpa,
            joint_efficiency=joint_efficiency,
            corrosion_allowance_mm=0.0,  # already subtracted
        )["t_required_mm"]

        # Required nozzle thickness (treat nozzle as cylinder)
        nozzle_inner_radius = d / 2.0
        t_rn = self.ug27_cylindrical_shell(
            pressure_mpa=design_pressure_mpa,
            inner_radius_mm=nozzle_inner_radius,
            allowable_stress_mpa=allowable_stress_mpa,
            joint_efficiency=joint_efficiency,
            corrosion_allowance_mm=0.0,
        )["t_required_mm"]

        # Area required (area removed by opening)
        area_required = d * t_r

        # Area available: excess in shell + excess in nozzle + pad
        # Limit of reinforcement along shell: larger of d or (R*t_shell)^0.5
        limit_shell = max(d, (shell_inner_radius_mm * t_shell) ** 0.5)
        # But use d for the simplified calculation
        area_shell_excess = (t_shell - t_r) * d  # excess shell metal
        area_nozzle_excess = 2.0 * (t_nozzle - t_rn) * min(
            2.5 * t_shell, 2.5 * t_nozzle + shell_thickness_mm
        )
        area_pad = reinforcement_pad_thickness_mm * d if reinforcement_pad_thickness_mm > 0 else 0.0

        area_available = area_shell_excess + max(area_nozzle_excess, 0.0) + area_pad

        return {
            "area_required_mm2": float(area_required),
            "area_available_mm2": float(area_available),
            "is_adequate": area_available >= area_required,
            "t_required_shell_mm": float(t_r),
            "t_required_nozzle_mm": float(t_rn),
            "excess_shell_mm2": float(area_shell_excess),
            "excess_nozzle_mm2": float(max(area_nozzle_excess, 0.0)),
            "pad_area_mm2": float(area_pad),
        }

    def calculate_hydrotest_pressure(
        self,
        design_pressure_mpa: float,
        test_factor: float = 1.3,
        allowable_design_mpa: float | None = None,
        allowable_test_mpa: float | None = None,
    ) -> float:
        """Calculate hydrostatic test pressure per UG-99.

        Default: P_test = test_factor * P_design
        With stress ratio: P_test = test_factor * P_design * (S_test / S_design)

        Parameters
        ----------
        design_pressure_mpa : float
            Design pressure.
        test_factor : float
            Test pressure factor (default 1.3 per UG-99).
        allowable_design_mpa : float, optional
            Allowable stress at design temperature.
        allowable_test_mpa : float, optional
            Allowable stress at test temperature (usually higher).

        Returns
        -------
        float
            Test pressure in MPa.
        """
        p_test = test_factor * design_pressure_mpa
        if allowable_design_mpa is not None and allowable_test_mpa is not None:
            p_test *= (allowable_test_mpa / allowable_design_mpa)
        return p_test


    def calculate_mawp(
        self, wall_thickness_mm: float, inner_radius_mm: float,
        allowable_stress_mpa: float, joint_efficiency: float = 1.0,
        corrosion_allowance_mm: float = 3.0,
    ) -> float:
        """Calculate Maximum Allowable Working Pressure (MAWP) per UG-27.

        Inverse of UG-27 thickness formula:
            MAWP = S * E * t_corr / (R + 0.6 * t_corr)

        Parameters
        ----------
        wall_thickness_mm : float
            Actual wall thickness in mm.
        inner_radius_mm : float
            Inner radius in mm.
        allowable_stress_mpa : float
            Allowable stress at design temperature.
        joint_efficiency : float
            Weld joint efficiency.
        corrosion_allowance_mm : float
            Corrosion allowance in mm.

        Returns
        -------
        float
            MAWP in MPa.

        Raises
        ------
        ValueError
            If corroded thickness is zero or negative.
        """
        t_corr = wall_thickness_mm - corrosion_allowance_mm
        if t_corr <= 0:
            raise ValueError(
                f"corroded thickness is {t_corr:.2f} mm "
                f"(wall={wall_thickness_mm}, CA={corrosion_allowance_mm})"
            )
        S = allowable_stress_mpa
        E = joint_efficiency
        R = inner_radius_mm
        return (S * E * t_corr) / (R + 0.6 * t_corr)

    def check_nozzle_reinforcement(
        self,
        shell_inner_radius_mm: float,
        shell_thickness_mm: float,
        nozzle_outer_diameter_mm: float,
        nozzle_thickness_mm: float,
        design_pressure_mpa: float,
        allowable_stress_mpa: float,
        joint_efficiency: float = 1.0,
        corrosion_allowance_mm: float = 3.0,
        reinforcement_pad_thickness_mm: float = 0.0,
    ) -> dict[str, float | bool]:
        """Check nozzle reinforcement per ASME VIII Div 1 UG-37.

        Uses the equal-area replacement method:
        - Area removed (A_required) = d * t_r
        - Area available = excess shell area + excess nozzle area + pad area

        Parameters
        ----------
        shell_inner_radius_mm : float
            Shell inner radius.
        shell_thickness_mm : float
            Actual shell thickness.
        nozzle_outer_diameter_mm : float
            Nozzle OD.
        nozzle_thickness_mm : float
            Nozzle wall thickness.
        design_pressure_mpa : float
            Internal design pressure.
        allowable_stress_mpa : float
            Allowable stress at design temperature.
        joint_efficiency : float
            Weld joint efficiency.
        corrosion_allowance_mm : float
            Corrosion allowance.
        reinforcement_pad_thickness_mm : float
            Additional reinforcement pad thickness (0 = none).

        Returns
        -------
        dict with area_required_mm2, area_available_mm2, is_adequate, and details.
        """
        CA = corrosion_allowance_mm
        # Corroded dimensions
        t_shell = shell_thickness_mm - CA
        t_nozzle = nozzle_thickness_mm - CA
        d = nozzle_outer_diameter_mm - 2.0 * nozzle_thickness_mm  # nozzle ID

        # Required shell thickness from UG-27
        t_r = self.ug27_cylindrical_shell(
            pressure_mpa=design_pressure_mpa,
            inner_radius_mm=shell_inner_radius_mm,
            allowable_stress_mpa=allowable_stress_mpa,
            joint_efficiency=joint_efficiency,
            corrosion_allowance_mm=0.0,  # already subtracted
        )["t_required_mm"]

        # Required nozzle thickness (treat nozzle as cylinder)
        nozzle_inner_radius = d / 2.0
        t_rn = self.ug27_cylindrical_shell(
            pressure_mpa=design_pressure_mpa,
            inner_radius_mm=nozzle_inner_radius,
            allowable_stress_mpa=allowable_stress_mpa,
            joint_efficiency=joint_efficiency,
            corrosion_allowance_mm=0.0,
        )["t_required_mm"]

        # Area required (area removed by opening)
        area_required = d * t_r

        # Area available: excess in shell + excess in nozzle + pad
        # Limit of reinforcement along shell: larger of d or (R*t_shell)^0.5
        limit_shell = max(d, (shell_inner_radius_mm * t_shell) ** 0.5)
        # But use d for the simplified calculation
        area_shell_excess = (t_shell - t_r) * d  # excess shell metal
        area_nozzle_excess = 2.0 * (t_nozzle - t_rn) * min(
            2.5 * t_shell, 2.5 * t_nozzle + shell_thickness_mm
        )
        area_pad = reinforcement_pad_thickness_mm * d if reinforcement_pad_thickness_mm > 0 else 0.0

        area_available = area_shell_excess + max(area_nozzle_excess, 0.0) + area_pad

        return {
            "area_required_mm2": float(area_required),
            "area_available_mm2": float(area_available),
            "is_adequate": area_available >= area_required,
            "t_required_shell_mm": float(t_r),
            "t_required_nozzle_mm": float(t_rn),
            "excess_shell_mm2": float(area_shell_excess),
            "excess_nozzle_mm2": float(max(area_nozzle_excess, 0.0)),
            "pad_area_mm2": float(area_pad),
        }

    def calculate_hydrotest_pressure(
        self,
        design_pressure_mpa: float,
        test_factor: float = 1.3,
        allowable_design_mpa: float | None = None,
        allowable_test_mpa: float | None = None,
    ) -> float:
        """Calculate hydrostatic test pressure per UG-99.

        Default: P_test = test_factor * P_design
        With stress ratio: P_test = test_factor * P_design * (S_test / S_design)

        Parameters
        ----------
        design_pressure_mpa : float
            Design pressure.
        test_factor : float
            Test pressure factor (default 1.3 per UG-99).
        allowable_design_mpa : float, optional
            Allowable stress at design temperature.
        allowable_test_mpa : float, optional
            Allowable stress at test temperature (usually higher).

        Returns
        -------
        float
            Test pressure in MPa.
        """
        p_test = test_factor * design_pressure_mpa
        if allowable_design_mpa is not None and allowable_test_mpa is not None:
            p_test *= (allowable_test_mpa / allowable_design_mpa)
        return p_test


# ---------------------------------------------------------------------------
# APDL Generator for pressure vessel
# ---------------------------------------------------------------------------

class PressureVesselGenerator:
    """Generate APDL commands for pressure vessel FEA."""

    def __init__(self) -> None:
        self._calc = ASMEThicknessCalc()

    def generate_vessel_geometry(self, geom: VesselGeometry) -> str:
        """Generate APDL geometry commands for the vessel shell.

        Creates cylindrical shell and head geometry using
        area and volume primitives.
        """
        R_inner = geom.inner_diameter_mm / 2.0
        R_outer = R_inner + geom.wall_thickness_mm
        head_t = geom.head_thickness_mm or geom.wall_thickness_mm

        lines = [
            "/PREP7",
            f"! Pressure Vessel Geometry",
            f"! ID={geom.inner_diameter_mm} mm, t={geom.wall_thickness_mm} mm",
            f"! Length={geom.shell_length_mm} mm, Head={geom.head_type}",
            "",
            "! --- Cylindrical Shell ---",
            f"CYL4,0,0,{R_inner},{R_outer},,,{geom.shell_length_mm}",
            "",
        ]

        # Head geometry
        if geom.head_type == "hemispherical":
            lines.extend([
                "! --- Hemispherical Head ---",
                f"SPHERE,{R_inner},{R_inner + head_t},,180",
                f"WPOFFS,0,0,{geom.shell_length_mm}",
                f"SPHERE,{R_inner},{R_inner + head_t},,180",
                "WPOFFS,0,0,0  ! Reset WP",
            ])
        elif geom.head_type == "2:1_ellipsoidal":
            # 2:1 ellipsoidal: semi-minor = D/4
            semi_minor = geom.inner_diameter_mm / 4.0
            lines.extend([
                "! --- 2:1 Ellipsoidal Heads ---",
                f"! Semi-minor axis = {semi_minor} mm",
                f"*SET,R_IN,{R_inner}",
                f"*SET,R_OUT,{R_outer}",
                f"*SET,SEMI_MIN,{semi_minor}",
                f"*SET,HEAD_T,{head_t}",
                "! Create ellipsoidal head profile via keypoints",
                "! (Simplified as shell elements on ellipsoidal surface)",
            ])

        # Nozzle cutouts
        for nzl in geom.nozzle_locations:
            lines.extend([
                "",
                f"! --- Nozzle {nzl.nozzle_id} ---",
                f"! OD={nzl.outer_diameter_mm} mm, t={nzl.wall_thickness_mm} mm",
                f"WPOFFS,0,{nzl.elevation_mm},0",
                f"WPROTA,0,0,{nzl.azimuth_deg}",
                f"CYL4,0,0,{nzl.outer_diameter_mm/2 - nzl.wall_thickness_mm},"
                f"{nzl.outer_diameter_mm/2},,,"
                f"{nzl.projection_mm + geom.wall_thickness_mm}",
                "WPOFFS,0,0,0",
            ])
            if nzl.reinforcement_pad_mm > 0:
                pad_od = nzl.outer_diameter_mm + 100  # typical pad extent
                lines.append(
                    f"! Reinforcement pad: {nzl.reinforcement_pad_mm} mm "
                    f"x {pad_od} mm OD"
                )

        lines.append("")
        return "\n".join(lines)

    def generate_internal_pressure(
        self, conditions: DesignConditions, geom: VesselGeometry
    ) -> str:
        """Generate APDL commands for internal pressure loading."""
        lines = [
            "! --- Internal Pressure Loading ---",
            f"! Design pressure: {conditions.design_pressure_mpa} MPa",
            f"! Material: {conditions.material_name}",
            "",
            "/SOLU",
            "ANTYPE,0  ! Static analysis",
            "NLGEOM,ON",
            "",
            "! Apply internal pressure to all inner surfaces",
            f"SFA,ALL,1,PRES,{conditions.design_pressure_mpa}",
            "",
            "! End cap pressure equivalent (for open-ended models)",
            f"! P_endcap = P * Ri^2 / (Ro^2 - Ri^2)",
        ]

        R_inner = geom.inner_diameter_mm / 2.0
        R_outer = R_inner + geom.wall_thickness_mm
        endcap_stress = (
            conditions.design_pressure_mpa
            * R_inner ** 2
            / (R_outer ** 2 - R_inner ** 2)
        )
        lines.append(f"! End-cap equivalent stress = {endcap_stress:.2f} MPa")
        lines.append("")
        return "\n".join(lines)

    def generate_thermal_loading(
        self, conditions: DesignConditions
    ) -> str:
        """Generate APDL commands for thermal gradient loading."""
        delta_t = conditions.operating_temperature_c - conditions.ambient_temperature_c

        lines = [
            "! --- Thermal Loading ---",
            f"! Operating temp: {conditions.operating_temperature_c}°C",
            f"! Ambient temp: {conditions.ambient_temperature_c}°C",
            f"! Delta T: {delta_t}°C",
            "",
            f"TREF,{conditions.ambient_temperature_c}",
            f"TUNIF,{conditions.operating_temperature_c}",
            "",
            "! For through-thickness gradient (inside hotter):",
            f"BF,ALL,TEMP,{conditions.operating_temperature_c}",
            "",
        ]
        return "\n".join(lines)

    def generate_nozzle_loads(self, loads: list[NozzleLoad]) -> str:
        """Generate APDL commands for external nozzle loads."""
        lines = ["! --- Nozzle External Loads ---"]

        for load in loads:
            lines.extend([
                f"! Nozzle {load.nozzle_id}",
                f"! Fx={load.axial_force_n}N, Fy={load.shear_fy_n}N, "
                f"Fz={load.shear_fz_n}N",
                f"! Mx={load.torsion_mx_nm}Nm, My={load.bending_my_nm}Nm, "
                f"Mz={load.bending_mz_nm}Nm",
                f"CMSEL,S,NOZZLE_{load.nozzle_id}_PILOT",
                f"F,ALL,FX,{load.axial_force_n}",
                f"F,ALL,FY,{load.shear_fy_n}",
                f"F,ALL,FZ,{load.shear_fz_n}",
                f"F,ALL,MX,{load.torsion_mx_nm}",
                f"F,ALL,MY,{load.bending_my_nm}",
                f"F,ALL,MZ,{load.bending_mz_nm}",
                "CMSEL,ALL",
                "",
            ])
        return "\n".join(lines)

    def generate_wind_load(
        self, load: WindSeismicLoad, geom: VesselGeometry
    ) -> str:
        """Generate APDL commands for wind loading on vertical vessel."""
        # Simplified wind pressure: q = 0.5 * rho * Cd * V^2
        rho_air = 1.225  # kg/m3
        V = load.wind_speed_ms
        Cd = load.wind_drag_coefficient
        q_pa = 0.5 * rho_air * Cd * V ** 2
        q_mpa = q_pa * 1e-6

        D_outer = geom.inner_diameter_mm + 2 * geom.wall_thickness_mm

        lines = [
            "! --- Wind Load ---",
            f"! Wind speed: {V} m/s (3-sec gust)",
            f"! Drag coefficient: {Cd}",
            f"! Dynamic pressure: {q_pa:.1f} Pa ({q_mpa:.6f} MPa)",
            f"! Vessel OD: {D_outer} mm",
            "",
            "! Apply as distributed pressure on windward side",
            f"! Wind pressure = {q_mpa:.6E} MPa",
            f"SFA,WIND_AREA,1,PRES,{q_mpa:.6E}",
            "",
        ]
        return "\n".join(lines)

    def generate_seismic_load(
        self, load: WindSeismicLoad, geom: VesselGeometry
    ) -> str:
        """Generate APDL commands for seismic loading."""
        lines = [
            "! --- Seismic Load ---",
            f"! Horizontal acceleration: {load.seismic_g_horizontal}g",
            f"! Vertical acceleration: {load.seismic_g_vertical}g",
            f"! Vessel weight: {load.vessel_weight_n} N",
            "",
            "! Apply as inertia load (acceleration in opposite direction)",
            f"ACEL,{load.seismic_g_horizontal * 9810},0,"
            f"{load.seismic_g_vertical * 9810}",
            "",
            f"! Equivalent horizontal force: "
            f"{load.vessel_weight_n * load.seismic_g_horizontal:.0f} N",
            f"! Equivalent vertical force: "
            f"{load.vessel_weight_n * load.seismic_g_vertical:.0f} N",
            "",
        ]
        return "\n".join(lines)

    def generate_stress_classification_paths(
        self, geom: VesselGeometry
    ) -> str:
        """Generate APDL PATH commands for stress classification lines (SCLs).

        Creates through-thickness paths at critical locations for
        ASME VIII Div 2 Part 5 stress linearization.
        """
        R_inner = geom.inner_diameter_mm / 2.0
        R_outer = R_inner + geom.wall_thickness_mm
        mid_length = geom.shell_length_mm / 2.0

        lines = [
            "/POST1",
            "SET,LAST",
            "",
            "! --- Stress Classification Lines ---",
            "! Through-thickness paths for ASME VIII Div 2 Part 5",
            "",
            "! SCL 1: Mid-shell (away from discontinuities)",
            f"PATH,SCL_SHELL,2,,{48}",
            f"PPATH,1,,{R_inner},{mid_length},0",
            f"PPATH,2,,{R_outer},{mid_length},0",
            "PDEF,SEQV_SCL1,S,EQV",
            "PDEF,S1_SCL1,S,1",
            "PDEF,S3_SCL1,S,3",
            "PDEF,SX_SCL1,S,X",
            "PDEF,SY_SCL1,S,Y",
            "PDEF,SZ_SCL1,S,Z",
            "PDEF,SXY_SCL1,S,XY",
            "",
            "! Linearize stresses on SCL",
            "PRSECT",
            "",
        ]

        # Add SCLs at nozzle junctions if present
        for idx, nzl in enumerate(geom.nozzle_locations, start=2):
            lines.extend([
                f"! SCL {idx}: Nozzle {nzl.nozzle_id} junction",
                f"PATH,SCL_{nzl.nozzle_id},2,,{48}",
                f"! Inner surface at nozzle-shell junction",
                f"PPATH,1,,{R_inner},{nzl.elevation_mm},0",
                f"PPATH,2,,{R_outer},{nzl.elevation_mm},0",
                "PRSECT",
                "",
            ])

        return "\n".join(lines)

    def generate_pv_apdl(
        self, geom: VesselGeometry, conditions: DesignConditions,
        include_thermal: bool = True, include_hydrotest: bool = False,
    ) -> str:
        """Generate a complete APDL macro for pressure vessel FEA.

        Combines geometry, pressure loading, thermal loading, and
        post-processing into a single APDL script.

        Parameters
        ----------
        geom : VesselGeometry
            Vessel geometric parameters.
        conditions : DesignConditions
            Design conditions including pressure, temperature, material.
        include_thermal : bool
            Whether to include thermal loading (default True).
        include_hydrotest : bool
            Whether to use hydrotest pressure instead of design pressure.

        Returns
        -------
        str
            Complete APDL macro text.
        """
        sections = [
            f"! ============================================",
            f"! Pressure Vessel FEA — {conditions.material_name}",
            f"! Generated by digitalmodel.ansys",
            f"! ============================================",
            "",
            self.generate_vessel_geometry(geom),
        ]

        if include_hydrotest:
            sections.append(self.generate_hydrotest_load(conditions))
        else:
            sections.append(self.generate_internal_pressure(conditions, geom))

        if include_thermal:
            sections.append(self.generate_thermal_loading(conditions))

        sections.append(self.generate_stress_classification_paths(geom))

        return "\n".join(sections)

    def generate_pv_apdl(
        self, geom: "VesselGeometry", conditions: "DesignConditions",
        include_thermal: bool = True, include_hydrotest: bool = False,
    ) -> str:
        """Generate a complete APDL macro for pressure vessel FEA.

        Combines geometry, pressure loading, thermal loading, and
        post-processing into a single APDL script.

        Parameters
        ----------
        geom : VesselGeometry
            Vessel geometric parameters.
        conditions : DesignConditions
            Design conditions including pressure, temperature, material.
        include_thermal : bool
            Whether to include thermal loading (default True).
        include_hydrotest : bool
            Whether to use hydrotest pressure instead of design pressure.

        Returns
        -------
        str
            Complete APDL macro text.
        """
        sections = [
            f"! ============================================",
            f"! Pressure Vessel FEA \u2014 {conditions.material_name}",
            f"! Generated by digitalmodel.ansys",
            f"! ============================================",
            "",
            self.generate_vessel_geometry(geom),
        ]

        if include_hydrotest:
            sections.append(self.generate_hydrotest_load(conditions))
        else:
            sections.append(self.generate_internal_pressure(conditions, geom))

        if include_thermal:
            sections.append(self.generate_thermal_loading(conditions))

        sections.append(self.generate_stress_classification_paths(geom))

        return "\n".join(sections)

    def generate_hydrotest_load(
        self, conditions: DesignConditions
    ) -> str:
        """Generate APDL commands for hydrostatic test pressure."""
        if conditions.hydrotest_pressure_mpa is not None:
            p_test = conditions.hydrotest_pressure_mpa
        else:
            p_test = 1.3 * conditions.design_pressure_mpa

        lines = [
            "! --- Hydrostatic Test ---",
            f"! Test pressure: {p_test:.2f} MPa",
            f"! (Design pressure x 1.3 = {conditions.design_pressure_mpa * 1.3:.2f} MPa)",
            "",
            f"SFA,ALL,1,PRES,{p_test}",
            "! Note: Material allowable at test = yield / 1.5 per UG-99",
            f"! Allowable test stress = {conditions.yield_strength_mpa / 1.5:.1f} MPa",
            "",
        ]
        return "\n".join(lines)
