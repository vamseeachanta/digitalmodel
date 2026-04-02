"""Riser configuration utilities for OrcaFlex pre-processing.

Provides SCR hang-off angle calculation, lazy-wave geometry (buoyancy section
sizing, sag-bend clearance), TTR stroke estimation, and riser weight in water
calculation.

Does NOT require OrcFxAPI — all calculations are analytical/numerical.

References:
    - API RP 2RD: Design of Risers for Floating Production Systems
    - DNV-OS-F201: Dynamic Risers
    - Bai & Bai (2005): Subsea Pipelines and Risers, Chapters 16-18
"""

import math
from enum import Enum
from typing import Dict, List, Optional, Tuple

import numpy as np
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------

class RiserType(str, Enum):
    """Riser configuration types."""
    SCR = "SCR"
    LAZY_WAVE = "LazyWave"
    STEEP_WAVE = "SteepWave"
    FREE_HANGING = "FreeHanging"
    TTR = "TTR"
    HYBRID = "Hybrid"


class PipeGrade(str, Enum):
    """Common pipe steel grades."""
    X52 = "X52"
    X60 = "X60"
    X65 = "X65"
    X70 = "X70"
    X80 = "X80"


# ---------------------------------------------------------------------------
# Pipe material library
# ---------------------------------------------------------------------------

PIPE_GRADES: Dict[str, Dict[str, float]] = {
    "X52": {"smys_mpa": 358, "smts_mpa": 455, "E_gpa": 207, "density_kg_m3": 7850},
    "X60": {"smys_mpa": 414, "smts_mpa": 517, "E_gpa": 207, "density_kg_m3": 7850},
    "X65": {"smys_mpa": 448, "smts_mpa": 531, "E_gpa": 207, "density_kg_m3": 7850},
    "X70": {"smys_mpa": 483, "smts_mpa": 565, "E_gpa": 207, "density_kg_m3": 7850},
    "X80": {"smys_mpa": 552, "smts_mpa": 621, "E_gpa": 207, "density_kg_m3": 7850},
}


# ---------------------------------------------------------------------------
# Core riser properties model
# ---------------------------------------------------------------------------

class RiserPipeProperties(BaseModel):
    """Riser pipe cross-section properties.

    Reference: API RP 2RD Section 4.
    """
    outer_diameter: float = Field(0.2731, gt=0.0, description="Outer diameter (m)")
    wall_thickness: float = Field(0.0254, gt=0.0, description="Wall thickness (m)")
    coating_thickness: float = Field(0.04, ge=0.0, description="External coating thickness (m)")
    coating_density: float = Field(900.0, ge=0.0, description="Coating density (kg/m^3)")
    grade: PipeGrade = PipeGrade.X65
    contents_density: float = Field(800.0, ge=0.0, description="Internal fluid density (kg/m^3)")
    corrosion_allowance: float = Field(0.003, ge=0.0, description="Corrosion allowance (m)")
    seawater_density: float = Field(1025.0, gt=0.0, description="Seawater density (kg/m^3)")

    @property
    def inner_diameter(self) -> float:
        """Inner diameter (m)."""
        return self.outer_diameter - 2 * self.wall_thickness

    @property
    def nominal_wt(self) -> float:
        """Nominal wall thickness minus corrosion allowance (m)."""
        return self.wall_thickness - self.corrosion_allowance

    @property
    def steel_area(self) -> float:
        """Steel cross-sectional area (m^2)."""
        return math.pi / 4.0 * (self.outer_diameter**2 - self.inner_diameter**2)

    @property
    def internal_area(self) -> float:
        """Internal bore area (m^2)."""
        return math.pi / 4.0 * self.inner_diameter**2

    @property
    def total_od(self) -> float:
        """Total outer diameter including coating (m)."""
        return self.outer_diameter + 2 * self.coating_thickness

    @property
    def displaced_volume_per_m(self) -> float:
        """Displaced volume per unit length (m^3/m), includes coating."""
        return math.pi / 4.0 * self.total_od**2

    @property
    def steel_mass_per_m(self) -> float:
        """Steel mass per unit length (kg/m)."""
        return PIPE_GRADES[self.grade.value]["density_kg_m3"] * self.steel_area

    @property
    def coating_mass_per_m(self) -> float:
        """Coating mass per unit length (kg/m)."""
        coating_area = math.pi / 4.0 * (self.total_od**2 - self.outer_diameter**2)
        return self.coating_density * coating_area

    @property
    def contents_mass_per_m(self) -> float:
        """Contents mass per unit length (kg/m)."""
        return self.contents_density * self.internal_area

    @property
    def total_mass_per_m(self) -> float:
        """Total mass per unit length (kg/m)."""
        return self.steel_mass_per_m + self.coating_mass_per_m + self.contents_mass_per_m

    @property
    def submerged_weight_per_m(self) -> float:
        """Submerged weight per unit length (N/m). Positive = sinks."""
        g = 9.80665
        buoyancy = self.seawater_density * g * self.displaced_volume_per_m
        weight = self.total_mass_per_m * g
        return weight - buoyancy

    @property
    def bending_stiffness(self) -> float:
        """Bending stiffness EI (N.m^2)."""
        E = PIPE_GRADES[self.grade.value]["E_gpa"] * 1e9
        I = math.pi / 64.0 * (self.outer_diameter**4 - self.inner_diameter**4)
        return E * I

    @property
    def axial_stiffness(self) -> float:
        """Axial stiffness EA (N)."""
        E = PIPE_GRADES[self.grade.value]["E_gpa"] * 1e9
        return E * self.steel_area


# ---------------------------------------------------------------------------
# SCR design
# ---------------------------------------------------------------------------

class SCRDesignInput(BaseModel):
    """Steel Catenary Riser (SCR) design input parameters.

    Reference: API RP 2RD Section 5.2, Bai & Bai Ch. 16.
    """
    water_depth: float = Field(1500.0, gt=0.0, description="Water depth (m)")
    pipe: RiserPipeProperties = Field(default_factory=RiserPipeProperties)
    hang_off_angle_from_vertical: float = Field(12.0, ge=0.0, le=30.0,
                                                 description="Hang-off angle from vertical (deg)")
    vessel_draft: float = Field(12.0, ge=0.0, description="Vessel draft (m)")
    hang_off_elevation: float = Field(-10.0, description="Hang-off point elevation relative to MSL (m)")
    min_tdp_offset: float = Field(200.0, ge=0.0, description="Minimum TDP offset from vessel (m)")

    def calculate_geometry(self) -> Dict[str, float]:
        """Calculate SCR catenary geometry.

        Uses simple catenary model:
        - Catenary parameter a = H/w
        - Vertical span h = a * [cosh(x_tdp/a) - 1]
        - Suspended length s = a * sinh(x_tdp/a)

        Returns:
            Dict with geometry parameters.
        """
        h = self.water_depth + self.hang_off_elevation
        w = self.pipe.submerged_weight_per_m  # N/m

        if w <= 0:
            raise ValueError("Pipe must have positive submerged weight for SCR")

        theta = math.radians(self.hang_off_angle_from_vertical)
        # Top tension: T_top * sin(theta) = H
        # T_top * cos(theta) = H + w*s ... actually V_top = w*s
        # tan(theta_from_horiz) = V/H at top => theta_from_vert means:
        # At top: ds/dx direction angle from vertical = theta
        # => dx/ds = sin(theta), dz/ds = cos(theta)
        # For catenary: tan(phi_top) = sinh(x_tdp/a) where phi_top is from horizontal
        phi_top = math.pi / 2.0 - theta  # angle from horizontal at top

        # tan(phi_top) = V_top / H = w*s / H = sinh(x_tdp/a)
        # Also h = a*(cosh(x_tdp/a) - 1)
        # From catenary: a = h / (cosh(x_tdp/a) - 1)
        # and sinh(x_tdp/a) = tan(phi_top)
        # so cosh(x_tdp/a) = sqrt(1 + tan^2(phi_top)) = 1/cos(phi_top)
        # therefore a = h / (1/cos(phi_top) - 1)

        cos_phi = math.cos(phi_top)
        if cos_phi >= 1.0:
            raise ValueError("Hang-off angle too small for catenary solution")

        a = h / (1.0 / cos_phi - 1.0)
        H = a * w  # horizontal tension (N)

        x_tdp_over_a = math.acosh(1.0 + h / a)
        x_tdp = a * x_tdp_over_a
        s_suspended = a * math.sinh(x_tdp_over_a)

        V_top = w * s_suspended
        T_top = math.sqrt(H**2 + V_top**2)
        T_tdp = H  # tension at TDP = horizontal component (catenary)

        return {
            "catenary_parameter_m": round(a, 1),
            "horizontal_tension_kN": round(H / 1000.0, 1),
            "top_tension_kN": round(T_top / 1000.0, 1),
            "tdp_tension_kN": round(T_tdp / 1000.0, 1),
            "suspended_length_m": round(s_suspended, 1),
            "horizontal_offset_m": round(x_tdp, 1),
            "hang_off_angle_deg": self.hang_off_angle_from_vertical,
            "estimated_total_length_m": round(s_suspended * 1.15, 1),
        }


# ---------------------------------------------------------------------------
# Lazy-wave design
# ---------------------------------------------------------------------------

class LazyWaveDesignInput(BaseModel):
    """Lazy-Wave Riser design parameters.

    Reference: API RP 2RD Section 5.3, Bai & Bai Ch. 17.
    """
    water_depth: float = Field(1500.0, gt=0.0, description="Water depth (m)")
    pipe: RiserPipeProperties = Field(default_factory=RiserPipeProperties)
    buoyancy_od: float = Field(0.65, gt=0.0, description="Buoyancy module OD (m)")
    buoyancy_density: float = Field(450.0, gt=0.0, description="Buoyancy module effective density (kg/m^3)")
    hog_bend_depth: float = Field(800.0, gt=0.0, description="Hog-bend target depth below MSL (m)")
    sag_bend_clearance: float = Field(50.0, ge=0.0, description="Min clearance above seabed at sag-bend (m)")
    hang_off_angle_deg: float = Field(8.0, ge=0.0, description="Hang-off angle from vertical (deg)")

    @property
    def buoyancy_volume_per_m(self) -> float:
        """Displaced volume of buoyancy module per unit length (m^3/m)."""
        return math.pi / 4.0 * self.buoyancy_od**2

    @property
    def buoyancy_mass_per_m(self) -> float:
        """Mass of buoyancy module per unit length (kg/m)."""
        buoy_area = math.pi / 4.0 * (self.buoyancy_od**2 - self.pipe.outer_diameter**2)
        return self.buoyancy_density * buoy_area

    @property
    def net_uplift_per_m(self) -> float:
        """Net uplift force per unit length of buoyancy section (N/m).

        Positive = upward.
        """
        g = 9.80665
        rho_sw = self.pipe.seawater_density

        total_mass = self.pipe.steel_mass_per_m + self.buoyancy_mass_per_m + self.pipe.contents_mass_per_m
        buoyancy = rho_sw * g * self.buoyancy_volume_per_m

        return buoyancy - total_mass * g

    def size_buoyancy_section(self) -> Dict[str, float]:
        """Estimate buoyancy section length for lazy-wave configuration.

        The buoyancy section must provide enough uplift to support the
        weight of the riser between the hog-bend and the sag-bend,
        ensuring sag-bend clearance from seabed.

        Returns:
            Dict with buoyancy section sizing results.
        """
        g = 9.80665
        w_bare = self.pipe.submerged_weight_per_m  # N/m (bare pipe, positive = heavy)
        w_buoy = -self.net_uplift_per_m  # N/m (buoyant section, negative = light)

        sag_bend_depth = self.water_depth - self.sag_bend_clearance
        lower_catenary_span = sag_bend_depth - self.hog_bend_depth

        if lower_catenary_span <= 0:
            raise ValueError("Hog-bend depth must be less than seabed minus clearance")

        # Weight of lower catenary (approximate as vertical span * w_bare)
        lower_weight = w_bare * lower_catenary_span * 1.2  # 20% extra for catenary shape

        # Buoyancy section length to support lower catenary weight
        if self.net_uplift_per_m <= 0:
            raise ValueError("Buoyancy section must have positive net uplift")

        buoyancy_length = lower_weight / self.net_uplift_per_m

        # Upper catenary from hang-off to hog-bend
        upper_span = self.hog_bend_depth + (-self.pipe.outer_diameter)  # simplified
        upper_length = upper_span / math.cos(math.radians(self.hang_off_angle_deg)) * 1.1

        # Lower catenary from sag-bend to seabed
        lower_length = lower_catenary_span * 1.3

        total_length = upper_length + buoyancy_length + lower_length

        return {
            "buoyancy_section_length_m": round(buoyancy_length, 1),
            "upper_catenary_length_m": round(upper_length, 1),
            "lower_catenary_length_m": round(lower_length, 1),
            "total_riser_length_m": round(total_length, 1),
            "net_uplift_per_m_N": round(self.net_uplift_per_m, 1),
            "hog_bend_depth_m": self.hog_bend_depth,
            "sag_bend_depth_m": sag_bend_depth,
        }


# ---------------------------------------------------------------------------
# TTR design
# ---------------------------------------------------------------------------

class TTRDesignInput(BaseModel):
    """Top-Tensioned Riser (TTR) design parameters.

    Reference: API RP 2RD Section 5.4.
    """
    water_depth: float = Field(1500.0, gt=0.0, description="Water depth (m)")
    pipe: RiserPipeProperties = Field(default_factory=RiserPipeProperties)
    vessel_heave_amplitude: float = Field(3.0, ge=0.0, description="Vessel heave amplitude (m)")
    vessel_pitch_amplitude_deg: float = Field(5.0, ge=0.0, description="Vessel pitch amplitude (deg)")
    tensioner_offset: float = Field(20.0, ge=0.0, description="Tensioner radial offset from CL (m)")
    overpull_pct: float = Field(50.0, ge=0.0, description="Overpull as percentage of riser weight (%)")

    def calculate_stroke(self) -> Dict[str, float]:
        """Estimate tensioner stroke requirements.

        Stroke components:
        - Heave: direct vessel heave
        - Pitch/Roll: rotational contribution at offset
        - Setdown: heave due to vessel offset (surge)
        - Thermal: thermal expansion of riser

        Returns:
            Dict with stroke components and total.
        """
        # Heave component
        stroke_heave = self.vessel_heave_amplitude

        # Pitch/Roll component at tensioner offset
        pitch_rad = math.radians(self.vessel_pitch_amplitude_deg)
        stroke_pitch = self.tensioner_offset * math.sin(pitch_rad)

        # Setdown (simplified: for semi-sub, ~0.1 * heave; for TLP, ~0)
        stroke_setdown = 0.1 * stroke_heave

        # Thermal expansion (rough: 12e-6 /°C * 50°C * water_depth)
        stroke_thermal = 12e-6 * 50.0 * self.water_depth

        total_stroke = stroke_heave + stroke_pitch + stroke_setdown + stroke_thermal
        # Add 20% margin
        design_stroke = total_stroke * 1.2

        return {
            "stroke_heave_m": round(stroke_heave, 3),
            "stroke_pitch_m": round(stroke_pitch, 3),
            "stroke_setdown_m": round(stroke_setdown, 3),
            "stroke_thermal_m": round(stroke_thermal, 3),
            "total_stroke_m": round(total_stroke, 3),
            "design_stroke_m": round(design_stroke, 3),
        }

    def calculate_top_tension(self) -> Dict[str, float]:
        """Estimate TTR top tension requirement.

        T_top = W_riser * (1 + overpull/100)

        Returns:
            Dict with tension components.
        """
        g = 9.80665
        w_riser = self.pipe.submerged_weight_per_m * self.water_depth  # N
        weight_kN = w_riser / 1000.0
        overpull_factor = 1.0 + self.overpull_pct / 100.0
        top_tension = weight_kN * overpull_factor

        return {
            "riser_submerged_weight_kN": round(weight_kN, 1),
            "overpull_factor": round(overpull_factor, 3),
            "required_top_tension_kN": round(top_tension, 1),
        }


# ---------------------------------------------------------------------------
# General utilities
# ---------------------------------------------------------------------------

def calculate_weight_in_water(
    outer_diameter: float,
    wall_thickness: float,
    steel_density: float = 7850.0,
    seawater_density: float = 1025.0,
    coating_thickness: float = 0.0,
    coating_density: float = 900.0,
    contents_density: float = 0.0,
) -> float:
    """Calculate submerged weight per unit length of a pipe.

    Args:
        outer_diameter: Pipe OD (m).
        wall_thickness: Pipe WT (m).
        steel_density: Steel density (kg/m^3).
        seawater_density: Seawater density (kg/m^3).
        coating_thickness: External coating thickness (m).
        coating_density: Coating density (kg/m^3).
        contents_density: Internal fluid density (kg/m^3).

    Returns:
        Submerged weight per unit length (N/m). Positive = sinks.
    """
    g = 9.80665
    id_ = outer_diameter - 2 * wall_thickness
    total_od = outer_diameter + 2 * coating_thickness

    steel_area = math.pi / 4.0 * (outer_diameter**2 - id_**2)
    coating_area = math.pi / 4.0 * (total_od**2 - outer_diameter**2)
    internal_area = math.pi / 4.0 * id_**2
    displaced_area = math.pi / 4.0 * total_od**2

    mass = (steel_density * steel_area + coating_density * coating_area +
            contents_density * internal_area)
    buoyancy = seawater_density * displaced_area

    return (mass - buoyancy) * g


def estimate_scr_hang_off_angle(
    water_depth: float,
    horizontal_offset: float,
    riser_length: Optional[float] = None,
) -> float:
    """Estimate SCR hang-off angle from vertical based on geometry.

    Simple geometric estimate: theta ≈ atan(x_offset / depth).
    For more accurate results, use SCRDesignInput.calculate_geometry().

    Args:
        water_depth: Water depth (m).
        horizontal_offset: Horizontal distance from vessel to TDP (m).
        riser_length: If provided, refines estimate.

    Returns:
        Hang-off angle from vertical (degrees).
    """
    if horizontal_offset <= 0:
        return 0.0
    angle_rad = math.atan(horizontal_offset / water_depth)
    return math.degrees(angle_rad)
