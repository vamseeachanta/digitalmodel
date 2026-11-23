"""Pipelay pre-processing utilities for OrcaFlex analysis.

Provides S-lay and J-lay geometry, stinger radius calculation,
overbend/sagbend stress estimation, departure angle tables,
and lay rate vs sea state operability analysis.

Does NOT require OrcFxAPI — all calculations are analytical.

References:
    - Bai & Bai (2014): Subsea Pipeline Design, Analysis and Installation, Ch. 4-6
    - DNV-OS-F101: Submarine Pipeline Systems
    - Mousselli (1981): Offshore Pipeline Design, Analysis and Methods
"""

import math
from enum import Enum
from typing import Dict, List, Optional, Tuple

import numpy as np
from pydantic import BaseModel, ConfigDict, Field, model_validator


# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------

class LayMethod(str, Enum):
    """Pipelay method."""
    S_LAY = "S-Lay"
    J_LAY = "J-Lay"
    REEL_LAY = "Reel-Lay"


class PipeEndCondition(str, Enum):
    """Pipe end condition for stress calculation."""
    CAPPED = "capped"
    UNCAPPED = "uncapped"


# ---------------------------------------------------------------------------
# Pipe properties
# ---------------------------------------------------------------------------

class PipelayPipeProperties(BaseModel):
    """Pipe properties for pipelay analysis.

    Reference: DNV-OS-F101 Section 5.
    """
    model_config = ConfigDict(extra='forbid', allow_inf_nan=False)
    outer_diameter: float = Field(0.3238, gt=0.0, description="Outer diameter (m)")
    wall_thickness: float = Field(0.0206, gt=0.0, description="Wall thickness (m)")
    steel_density: float = Field(7850.0, gt=0.0, description="Steel density (kg/m^3)")
    youngs_modulus: float = Field(207e9, gt=0.0, description="Young's modulus (Pa)")
    smys: float = Field(448e6, gt=0.0, description="SMYS (Pa)")
    poissons_ratio: float = Field(0.3, gt=0.0, description="Poisson's ratio")
    coating_thickness: float = Field(0.06, ge=0.0, description="Concrete + anti-corrosion coating (m)")
    coating_density: float = Field(2400.0, ge=0.0, description="Coating density (kg/m^3)")
    seawater_density: float = Field(1025.0, gt=0.0, description="Seawater density (kg/m^3)")

    @model_validator(mode='after')
    def validate_annulus(self):
        # screening-basis.md E1: inner diameter D - 2t must be positive.
        if self.wall_thickness >= self.outer_diameter / 2:
            raise ValueError('wall_thickness must be less than outer_diameter / 2')
        return self

    @property
    def inner_diameter(self) -> float:
        return self.outer_diameter - 2 * self.wall_thickness

    @property
    def total_od(self) -> float:
        return self.outer_diameter + 2 * self.coating_thickness

    @property
    def moment_of_inertia(self) -> float:
        """Second moment of area (m^4)."""
        return math.pi / 64.0 * (self.outer_diameter**4 - self.inner_diameter**4)

    @property
    def bending_stiffness(self) -> float:
        """EI (N.m^2)."""
        return self.youngs_modulus * self.moment_of_inertia

    @property
    def steel_area(self) -> float:
        """Steel cross-section area (m^2)."""
        return math.pi / 4.0 * (self.outer_diameter**2 - self.inner_diameter**2)

    @property
    def submerged_weight_per_m(self) -> float:
        """Submerged weight per unit length (N/m)."""
        g = 9.80665
        steel_mass = self.steel_density * self.steel_area
        coating_area = math.pi / 4.0 * (self.total_od**2 - self.outer_diameter**2)
        coating_mass = self.coating_density * coating_area
        displaced_vol = math.pi / 4.0 * self.total_od**2
        buoyancy = self.seawater_density * displaced_vol
        return ((steel_mass + coating_mass) - buoyancy) * g


# ---------------------------------------------------------------------------
# S-Lay analysis
# ---------------------------------------------------------------------------

class SLayConfig(BaseModel):
    """S-Lay configuration parameters.

    Reference: Bai & Bai (2014) Chapter 4.
    """
    water_depth: float = Field(200.0, gt=0.0, description="Water depth (m)")
    pipe: PipelayPipeProperties = Field(default_factory=PipelayPipeProperties)
    stinger_radius: float = Field(120.0, gt=0.0, description="Stinger radius of curvature (m)")
    stinger_length: float = Field(80.0, gt=0.0, description="Stinger length (m)")
    departure_angle: float = Field(60.0, gt=0.0, lt=90.0, description="Departure angle from horizontal (deg)")
    lay_tension: float = Field(500.0, gt=0.0, description="Lay tension at tensioner (kN)")
    strain_limit_overbend: float = Field(0.002, gt=0.0, description="Allowable overbend strain")
    strain_limit_sagbend: float = Field(0.002, gt=0.0, description="Allowable sagbend strain")

    def calculate_overbend_stress(self) -> Dict[str, float]:
        """Calculate overbend (stinger) stress.

        Bending strain = D/(2R), bending stress = E * strain.

        Returns:
            Dict with overbend stress results.
        """
        D = self.pipe.outer_diameter
        R = self.stinger_radius
        E = self.pipe.youngs_modulus

        bending_strain = D / (2.0 * R)
        bending_stress = E * bending_strain
        utilisation = bending_strain / self.strain_limit_overbend

        return {
            "overbend_bending_strain": round(bending_strain, 6),
            "overbend_bending_stress_MPa": round(bending_stress / 1e6, 1),
            "overbend_strain_utilisation": round(utilisation, 3),
            "stinger_radius_m": self.stinger_radius,
            "min_stinger_radius_m": round(D / (2.0 * self.strain_limit_overbend), 1),
        }

    def calculate_sagbend_stress(self) -> Dict[str, float]:
        """Estimate sagbend bending stress.

        Sagbend radius approximation: R_sag ≈ T / (w * cos(theta))
        where T = horizontal tension, w = submerged weight, theta = departure angle.

        Reference: Mousselli (1981) Ch. 5.

        Returns:
            Dict with sagbend stress results.
        """
        w = self.pipe.submerged_weight_per_m  # N/m
        T = self.lay_tension * 1000.0  # N
        theta = math.radians(self.departure_angle)
        D = self.pipe.outer_diameter
        E = self.pipe.youngs_modulus

        # Horizontal component of tension
        H = T * math.cos(theta)

        # Sagbend radius
        R_sag = H / w if w > 0 else float("inf")

        bending_strain = D / (2.0 * R_sag)
        bending_stress = E * bending_strain
        utilisation = bending_strain / self.strain_limit_sagbend

        return {
            "sagbend_radius_m": round(R_sag, 1),
            "sagbend_bending_strain": round(bending_strain, 6),
            "sagbend_bending_stress_MPa": round(bending_stress / 1e6, 1),
            "sagbend_strain_utilisation": round(utilisation, 3),
            "horizontal_tension_kN": round(H / 1000.0, 1),
        }

    def calculate_departure_angle_table(self) -> List[Dict[str, float]]:
        """Generate departure angle vs water depth table.

        For varying water depths, compute the required departure angle
        to maintain sagbend stress below limit.

        Returns:
            List of dicts with depth and corresponding departure angle.
        """
        depths = np.arange(50, min(self.water_depth * 1.5, 500) + 1, 25)
        table = []

        for d in depths:
            # Target sagbend radius: R_sag >= D / (2 * strain_limit)
            R_min = self.pipe.outer_diameter / (2.0 * self.strain_limit_sagbend)
            w = self.pipe.submerged_weight_per_m
            T = self.lay_tension * 1000.0

            # H_required = w * R_min
            H_req = w * R_min
            if H_req > T:
                angle_deg = 0.0  # insufficient tension
            else:
                cos_theta = H_req / T
                cos_theta = min(1.0, max(-1.0, cos_theta))
                angle_deg = math.degrees(math.acos(cos_theta))

            table.append({
                "water_depth_m": float(d),
                "departure_angle_deg": round(angle_deg, 1),
                "min_sagbend_radius_m": round(R_min, 1),
            })

        return table


# ---------------------------------------------------------------------------
# J-Lay analysis
# ---------------------------------------------------------------------------

class JLayConfig(BaseModel):
    """J-Lay configuration parameters.

    Reference: Bai & Bai (2014) Chapter 5.
    """
    model_config = ConfigDict(extra='forbid', allow_inf_nan=False)
    water_depth: float = Field(gt=0.0, description="Water depth (m)")
    pipe: PipelayPipeProperties = Field(default_factory=PipelayPipeProperties)
    tower_angle: float = Field(gt=0.0, le=15.0,
                               description="J-lay tower angle from vertical (deg)")
    top_tension: Optional[float] = Field(None, gt=0.0, description="Top tension check (kN)")

    def calculate_sagbend(self) -> Dict[str, float]:
        """Calculate sagbend geometry for J-Lay.

        Governing derivation: pipelay/docs/screening-basis.md E2-E5, V1.
        Depth and angle solve the curve; optional tension checks closure.
        """
        w = self.pipe.submerged_weight_per_m
        if not math.isfinite(w) or w <= 0:
            raise ValueError('submerged weight must be finite and positive for a J-lay catenary')
        D = self.pipe.outer_diameter
        E = self.pipe.youngs_modulus
        theta = math.radians(self.tower_angle)
        # screening-basis.md E2-E4: H=T*sin(theta), T=H+w*h, a=H/w.
        a = self.water_depth * math.sin(theta) / (1.0 - math.sin(theta))
        derived_tension = w * (a + self.water_depth)
        closure_error = 0.0
        if self.top_tension is not None:
            supplied_tension = self.top_tension * 1000.0
            if supplied_tension <= w * self.water_depth:
                raise ValueError('top_tension must exceed suspended weight w * water_depth')
            closure_error = (supplied_tension - derived_tension) / derived_tension
            # V1: fixed screening consistency tolerance; not a standard allowable.
            if abs(closure_error) > 0.001:
                raise ValueError('top_tension closure error exceeds 0.1% of derived tension')
        bending_strain = D / (2.0 * a)  # E5: touchdown curvature = 1/a.
        bending_stress = E * bending_strain
        # E4: h=a*(cosh(x/a)-1), s^2=h^2+2*a*h.
        x_tdp = a * math.acosh(1.0 + self.water_depth / a)
        s_suspended = math.sqrt(self.water_depth**2 + 2.0 * a * self.water_depth)
        return {
            "catenary_parameter_m": a,
            "sagbend_radius_m": a,
            "sagbend_bending_strain": bending_strain,
            "sagbend_bending_stress_MPa": round(bending_stress / 1e6, 1),
            "horizontal_offset_m": round(x_tdp, 1),
            "suspended_length_m": round(s_suspended, 1),
            "derived_top_tension_kN": derived_tension / 1000.0,
            "horizontal_tension_kN": w * a / 1000.0,
            "tension_closure_relative_error": closure_error,
            "tension_closure_relative_tolerance": 0.001,
        }


# ---------------------------------------------------------------------------
# Operability
# ---------------------------------------------------------------------------

class LayOperability(BaseModel):
    """Lay rate vs sea state operability assessment.

    Reference: DNV-OS-F101 Section 11.
    """
    model_config = ConfigDict(extra='forbid', allow_inf_nan=False)
    max_hs_lay: float = Field(2.5, gt=0.0, description="Max Hs for lay operations (m)")
    max_hs_welding: float = Field(2.0, gt=0.0, description="Max Hs for welding/connection (m)")
    max_hs_aband_recovery: float = Field(3.5, gt=0.0,
                                          description="Max Hs for abandonment & recovery (m)")
    lay_rate_calm: float = Field(4.0, gt=0.0, description="Lay rate in calm seas (km/day)")
    lay_rate_degradation: float = Field(0.5, ge=0.0,
                                         description="Lay rate reduction per m Hs (km/day/m)")

    def lay_rate_at_hs(self, hs: float, *, welding_required: bool = False) -> float:
        """Calculate lay rate at given Hs.

        Args:
            hs: Significant wave height (m).

        Returns:
            Lay rate (km/day). Zero if above operational limit.
        """
        # screening-basis.md E7: J-lay requires both joining and laying.
        limit = min(self.max_hs_lay, self.max_hs_welding) if welding_required else self.max_hs_lay
        if hs > limit:
            return 0.0
        rate = self.lay_rate_calm - self.lay_rate_degradation * hs
        return max(0.0, round(rate, 2))

    def generate_operability_table(self, *, welding_required: bool = False) -> List[Dict[str, float]]:
        """Generate Hs vs lay rate table.

        Returns:
            List of dicts with hs and lay_rate.
        """
        hs_values = np.arange(0.0, self.max_hs_lay + 0.5 + 0.01, 0.5)
        return [
            {"hs_m": round(float(hs), 1), "lay_rate_km_per_day": self.lay_rate_at_hs(
                float(hs), welding_required=welding_required)}
            for hs in hs_values
        ]


def calculate_stinger_radius(
    pipe_od: float,
    max_overbend_strain: float = 0.002,
) -> float:
    """Calculate minimum stinger radius from allowable overbend strain.

    R_min = D / (2 * epsilon_max)

    Args:
        pipe_od: Pipe outer diameter (m).
        max_overbend_strain: Maximum allowable overbend strain.

    Returns:
        Minimum stinger radius (m).

    Reference: DNV-OS-F101 Section 11.
    """
    # screening-basis.md E5, V3: finite positive diameter and strain in (0, 1).
    if not math.isfinite(max_overbend_strain) or not 0 < max_overbend_strain < 1:
        raise ValueError('allowable_overbend_strain must be finite and in (0, 1)')
    if not math.isfinite(pipe_od) or pipe_od <= 0:
        raise ValueError('pipe_od must be finite and positive')
    return pipe_od / (2.0 * max_overbend_strain)
