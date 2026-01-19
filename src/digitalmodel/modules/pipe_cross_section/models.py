# ABOUTME: Data models for pipe cross-section analysis.
# ABOUTME: Defines PipeLayer and PipeCrossSection dataclasses for coated subsea pipes.

"""
Pipe Cross-Section Data Models
==============================

Provides structured data models for multi-layer pipe configurations:
- PipeLayer: Individual coating/material layer
- PipeCrossSectionConfig: Configuration for pipe geometry
- BuoyancyResult: Buoyancy calculation results

Compatible with DNV-ST-F101, API 5L, and ISO 21809 standards.
"""

import math
from dataclasses import dataclass, field
from typing import List, Dict, Any, Optional
from enum import Enum


class CoatingType(Enum):
    """Standard coating types for subsea pipelines."""
    STEEL = "steel"
    THREE_LPP = "3lpp"  # 3-Layer Polypropylene
    THREE_LPE = "3lpe"  # 3-Layer Polyethylene
    FBE = "fbe"         # Fusion Bonded Epoxy
    CONCRETE = "concrete"
    CWC = "cwc"         # Concrete Weight Coating
    INSULATION = "insulation"
    CUSTOM = "custom"


class InternalContents(Enum):
    """Internal pipe contents options."""
    AIR = "air"
    SEAWATER = "seawater"
    OIL = "oil"
    GAS = "gas"
    WATER = "water"
    CUSTOM = "custom"


def inch_to_mm(inches: float) -> float:
    """Convert inches to millimeters."""
    return inches * 25.4


def mm_to_inch(mm: float) -> float:
    """Convert millimeters to inches."""
    return mm / 25.4


@dataclass
class PipeLayer:
    """
    Represents a single layer of a pipe cross-section.

    Attributes:
        name: Layer identifier (e.g., "Steel Pipe", "3LPP Coating")
        inner_diameter_mm: Internal diameter in millimeters
        outer_diameter_mm: External diameter in millimeters
        density_kg_m3: Material density in kg/m³
        color: Hex color code for visualization
        coating_type: Standard coating type classification
    """
    name: str
    inner_diameter_mm: float
    outer_diameter_mm: float
    density_kg_m3: float
    color: str = "#808080"
    coating_type: CoatingType = CoatingType.CUSTOM

    @property
    def thickness_mm(self) -> float:
        """Wall/coating thickness in mm."""
        return (self.outer_diameter_mm - self.inner_diameter_mm) / 2

    @property
    def inner_radius_m(self) -> float:
        """Inner radius in meters."""
        return self.inner_diameter_mm / 2000

    @property
    def outer_radius_m(self) -> float:
        """Outer radius in meters."""
        return self.outer_diameter_mm / 2000

    @property
    def cross_sectional_area_m2(self) -> float:
        """Cross-sectional area of the layer material in m²."""
        return math.pi * (self.outer_radius_m**2 - self.inner_radius_m**2)

    @property
    def weight_per_meter_kg(self) -> float:
        """Weight per meter of pipe layer in kg/m."""
        return self.cross_sectional_area_m2 * self.density_kg_m3

    @property
    def weight_per_meter_kN(self) -> float:
        """Weight per meter in kN/m."""
        return self.weight_per_meter_kg * 9.81 / 1000

    def to_dict(self) -> Dict[str, Any]:
        """Convert layer to dictionary for export."""
        return {
            "Layer": self.name,
            "ID_mm": round(self.inner_diameter_mm, 1),
            "OD_mm": round(self.outer_diameter_mm, 1),
            "Thickness_mm": round(self.thickness_mm, 2),
            "Density_kg_m3": self.density_kg_m3,
            "Area_m2_m": round(self.cross_sectional_area_m2, 5),
            "Weight_kg_m": round(self.weight_per_meter_kg, 1),
            "Weight_kN_m": round(self.weight_per_meter_kN, 4),
        }


@dataclass
class PipeCrossSectionConfig:
    """
    Configuration for pipe cross-section analysis.

    This follows the digitalmodel configuration pattern for YAML-based inputs.

    Attributes:
        steel_od_mm: Steel pipe outer diameter in mm
        steel_wt_mm: Steel pipe wall thickness in mm
        steel_density: Steel density in kg/m³ (default: 7850)
        lpp_thickness_mm: 3LPP/anti-corrosion coating thickness in mm
        lpp_density: 3LPP coating density in kg/m³ (default: 1100)
        concrete_thickness_mm: Concrete weight coating thickness in mm
        concrete_density: Concrete density in kg/m³ (default: 3000)
        seawater_density: Seawater density in kg/m³ (default: 1025)
        internal_contents: Internal contents type
        internal_fluid_density: Internal fluid density in kg/m³
    """
    steel_od_mm: float
    steel_wt_mm: float
    steel_density: float = 7850
    lpp_thickness_mm: float = 0
    lpp_density: float = 1100
    concrete_thickness_mm: float = 0
    concrete_density: float = 3000
    seawater_density: float = 1025
    internal_contents: str = "air"
    internal_fluid_density: float = 0

    @classmethod
    def from_inches(
        cls,
        steel_od_inch: float,
        steel_wt_inch: float,
        lpp_thickness_mm: float = 0,
        concrete_thickness_inch: float = 0,
        **kwargs
    ) -> "PipeCrossSectionConfig":
        """Create configuration from imperial units."""
        return cls(
            steel_od_mm=inch_to_mm(steel_od_inch),
            steel_wt_mm=inch_to_mm(steel_wt_inch),
            lpp_thickness_mm=lpp_thickness_mm,
            concrete_thickness_mm=inch_to_mm(concrete_thickness_inch),
            **kwargs
        )

    @classmethod
    def from_dict(cls, cfg: Dict[str, Any]) -> "PipeCrossSectionConfig":
        """Create configuration from dictionary (YAML config pattern)."""
        pipe_cfg = cfg.get("pipe_cross_section", cfg)
        return cls(
            steel_od_mm=pipe_cfg.get("steel_od_mm", pipe_cfg.get("Nominal_OD", 0)),
            steel_wt_mm=pipe_cfg.get("steel_wt_mm", pipe_cfg.get("Design_WT", 0)),
            steel_density=pipe_cfg.get("steel_density", 7850),
            lpp_thickness_mm=pipe_cfg.get("lpp_thickness_mm", 0),
            lpp_density=pipe_cfg.get("lpp_density", 1100),
            concrete_thickness_mm=pipe_cfg.get("concrete_thickness_mm", 0),
            concrete_density=pipe_cfg.get("concrete_density", 3000),
            seawater_density=pipe_cfg.get("seawater_density", 1025),
            internal_contents=pipe_cfg.get("internal_contents", "air"),
            internal_fluid_density=pipe_cfg.get("internal_fluid_density", 0),
        )

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for YAML export."""
        return {
            "steel_od_mm": self.steel_od_mm,
            "steel_wt_mm": self.steel_wt_mm,
            "steel_density": self.steel_density,
            "lpp_thickness_mm": self.lpp_thickness_mm,
            "lpp_density": self.lpp_density,
            "concrete_thickness_mm": self.concrete_thickness_mm,
            "concrete_density": self.concrete_density,
            "seawater_density": self.seawater_density,
            "internal_contents": self.internal_contents,
            "internal_fluid_density": self.internal_fluid_density,
        }


@dataclass
class BuoyancyResult:
    """
    Results from buoyancy analysis.

    Attributes:
        displaced_volume_m3_m: Volume of water displaced per meter
        buoyancy_force_kg_m: Buoyancy force per meter (kg/m equivalent)
        buoyancy_force_kN_m: Buoyancy force per meter in kN/m
        submerged_weight_kg_m: Submerged weight per meter
        submerged_weight_kN_m: Submerged weight per meter in kN/m
        is_buoyant: True if pipe floats (negative submerged weight)
    """
    displaced_volume_m3_m: float
    buoyancy_force_kg_m: float
    buoyancy_force_kN_m: float
    submerged_weight_kg_m: float
    submerged_weight_kN_m: float
    is_buoyant: bool

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "Displaced_Volume_m3_m": round(self.displaced_volume_m3_m, 4),
            "Buoyancy_Force_kg_m": round(self.buoyancy_force_kg_m, 1),
            "Buoyancy_Force_kN_m": round(self.buoyancy_force_kN_m, 3),
            "Submerged_Weight_kg_m": round(self.submerged_weight_kg_m, 1),
            "Submerged_Weight_kN_m": round(self.submerged_weight_kN_m, 3),
            "Is_Buoyant": self.is_buoyant,
        }
