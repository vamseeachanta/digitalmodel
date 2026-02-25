# ABOUTME: Core calculation module for coated subsea pipe cross-sections.
# ABOUTME: Calculates geometry, weight, and buoyancy for multi-layer pipe configurations.

"""
Pipe Cross-Section Calculator
=============================

Provides PipeCrossSection class for calculating:
- Layer geometry (ID, OD, thickness)
- Weight in air per meter
- Buoyancy and submerged weight
- Section properties (area, moment of inertia)

Compatible with DNV-ST-F101, API 5L, and ISO 21809 standards.

Usage:
    from digitalmodel.structural.pipe_cross_section import PipeCrossSection

    pipe = PipeCrossSection(
        steel_od_mm=609.6,
        steel_wt_mm=14.29,
        lpp_thickness_mm=3.5,
        concrete_thickness_mm=80.0
    )
    pipe.print_summary()
"""

import math
from dataclasses import dataclass, field
from typing import List, Dict, Any, Optional

from .models import (
    PipeLayer,
    PipeCrossSectionConfig,
    BuoyancyResult,
    CoatingType,
    inch_to_mm,
    mm_to_inch,
)


@dataclass
class PipeCrossSection:
    """
    Complete pipe cross-section with multiple coating layers.

    Supports standard offshore pipeline configurations:
    - Steel carrier pipe (API 5L grades)
    - Anti-corrosion coating (3LPP, 3LPE, FBE)
    - Concrete weight coating (CWC)
    - Insulation layers

    Example:
        >>> pipe = PipeCrossSection(
        ...     steel_od_mm=609.6,
        ...     steel_wt_mm=14.29,
        ...     lpp_thickness_mm=3.5,
        ...     concrete_thickness_mm=80.0
        ... )
        >>> print(f"Submerged weight: {pipe.submerged_weight_kg_m:.1f} kg/m")
        Submerged weight: 256.9 kg/m
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
    layers: List[PipeLayer] = field(default_factory=list, init=False)

    def __post_init__(self):
        """Build layers after initialization."""
        self._build_layers()

    @classmethod
    def from_config(cls, config: PipeCrossSectionConfig) -> "PipeCrossSection":
        """Create from configuration object."""
        return cls(
            steel_od_mm=config.steel_od_mm,
            steel_wt_mm=config.steel_wt_mm,
            steel_density=config.steel_density,
            lpp_thickness_mm=config.lpp_thickness_mm,
            lpp_density=config.lpp_density,
            concrete_thickness_mm=config.concrete_thickness_mm,
            concrete_density=config.concrete_density,
            seawater_density=config.seawater_density,
            internal_contents=config.internal_contents,
            internal_fluid_density=config.internal_fluid_density,
        )

    @classmethod
    def from_dict(cls, cfg: Dict[str, Any]) -> "PipeCrossSection":
        """Create from dictionary configuration (YAML pattern)."""
        config = PipeCrossSectionConfig.from_dict(cfg)
        return cls.from_config(config)

    @classmethod
    def from_inches(
        cls,
        steel_od_inch: float,
        steel_wt_inch: float,
        lpp_thickness_mm: float = 0,
        concrete_thickness_inch: float = 0,
        **kwargs
    ) -> "PipeCrossSection":
        """Create from imperial units (common in API standards)."""
        return cls(
            steel_od_mm=inch_to_mm(steel_od_inch),
            steel_wt_mm=inch_to_mm(steel_wt_inch),
            lpp_thickness_mm=lpp_thickness_mm,
            concrete_thickness_mm=inch_to_mm(concrete_thickness_inch),
            **kwargs
        )

    def _build_layers(self):
        """Construct all pipe layers from configuration."""
        self.layers = []

        # Steel pipe layer
        steel_id = self.steel_od_mm - 2 * self.steel_wt_mm
        steel_layer = PipeLayer(
            name="Steel Pipe",
            inner_diameter_mm=steel_id,
            outer_diameter_mm=self.steel_od_mm,
            density_kg_m3=self.steel_density,
            color="#708090",  # Slate gray
            coating_type=CoatingType.STEEL,
        )
        self.layers.append(steel_layer)

        current_od = self.steel_od_mm

        # 3LPP/anti-corrosion coating layer (if present)
        if self.lpp_thickness_mm > 0:
            lpp_od = current_od + 2 * self.lpp_thickness_mm
            lpp_layer = PipeLayer(
                name="3LPP Coating",
                inner_diameter_mm=current_od,
                outer_diameter_mm=lpp_od,
                density_kg_m3=self.lpp_density,
                color="#4169E1",  # Royal blue
                coating_type=CoatingType.THREE_LPP,
            )
            self.layers.append(lpp_layer)
            current_od = lpp_od

        # Concrete weight coating layer (if present)
        if self.concrete_thickness_mm > 0:
            concrete_od = current_od + 2 * self.concrete_thickness_mm
            concrete_layer = PipeLayer(
                name="Concrete Coating",
                inner_diameter_mm=current_od,
                outer_diameter_mm=concrete_od,
                density_kg_m3=self.concrete_density,
                color="#D2B48C",  # Tan
                coating_type=CoatingType.CONCRETE,
            )
            self.layers.append(concrete_layer)

    @property
    def steel_layer(self) -> PipeLayer:
        """Get the steel pipe layer."""
        return self.layers[0]

    @property
    def inner_diameter_mm(self) -> float:
        """Internal bore diameter in mm."""
        return self.layers[0].inner_diameter_mm

    @property
    def outer_diameter_mm(self) -> float:
        """Overall outer diameter in mm."""
        return self.layers[-1].outer_diameter_mm

    @property
    def outer_diameter_inch(self) -> float:
        """Overall outer diameter in inches."""
        return mm_to_inch(self.outer_diameter_mm)

    @property
    def total_weight_in_air_kg_m(self) -> float:
        """Total weight in air per meter (kg/m)."""
        return sum(layer.weight_per_meter_kg for layer in self.layers)

    @property
    def total_weight_in_air_kN_m(self) -> float:
        """Total weight in air per meter (kN/m)."""
        return self.total_weight_in_air_kg_m * 9.81 / 1000

    @property
    def displaced_volume_m3_m(self) -> float:
        """Volume of water displaced per meter (m³/m)."""
        outer_radius_m = self.outer_diameter_mm / 2000
        return math.pi * outer_radius_m**2

    @property
    def buoyancy_force_kg_m(self) -> float:
        """Buoyancy force per meter (kg/m equivalent)."""
        return self.displaced_volume_m3_m * self.seawater_density

    @property
    def buoyancy_force_kN_m(self) -> float:
        """Buoyancy force per meter (kN/m)."""
        return self.buoyancy_force_kg_m * 9.81 / 1000

    @property
    def submerged_weight_kg_m(self) -> float:
        """Submerged weight per meter (kg/m)."""
        return self.total_weight_in_air_kg_m - self.buoyancy_force_kg_m

    @property
    def submerged_weight_kN_m(self) -> float:
        """Submerged weight per meter (kN/m)."""
        return self.submerged_weight_kg_m * 9.81 / 1000

    @property
    def is_buoyant(self) -> bool:
        """True if pipe floats (negative submerged weight)."""
        return self.submerged_weight_kg_m < 0

    def get_buoyancy_result(self) -> BuoyancyResult:
        """Get structured buoyancy analysis result."""
        return BuoyancyResult(
            displaced_volume_m3_m=self.displaced_volume_m3_m,
            buoyancy_force_kg_m=self.buoyancy_force_kg_m,
            buoyancy_force_kN_m=self.buoyancy_force_kN_m,
            submerged_weight_kg_m=self.submerged_weight_kg_m,
            submerged_weight_kN_m=self.submerged_weight_kN_m,
            is_buoyant=self.is_buoyant,
        )

    def get_summary(self) -> Dict[str, Any]:
        """Get summary of pipe properties."""
        return {
            "Inner_Diameter_mm": round(self.inner_diameter_mm, 1),
            "Outer_Diameter_mm": round(self.outer_diameter_mm, 1),
            "Outer_Diameter_inch": round(self.outer_diameter_inch, 2),
            "Total_Weight_Air_kg_m": round(self.total_weight_in_air_kg_m, 1),
            "Total_Weight_Air_kN_m": round(self.total_weight_in_air_kN_m, 3),
            "Displaced_Volume_m3_m": round(self.displaced_volume_m3_m, 4),
            "Buoyancy_Force_kg_m": round(self.buoyancy_force_kg_m, 1),
            "Submerged_Weight_kg_m": round(self.submerged_weight_kg_m, 1),
            "Submerged_Weight_kN_m": round(self.submerged_weight_kN_m, 3),
            "Is_Buoyant": self.is_buoyant,
            "Seawater_Density_kg_m3": self.seawater_density,
            "Internal_Contents": self.internal_contents,
        }

    def to_dict(self) -> Dict[str, Any]:
        """Convert full pipe configuration to dictionary."""
        return {
            "layers": [layer.to_dict() for layer in self.layers],
            "summary": self.get_summary(),
            "buoyancy": self.get_buoyancy_result().to_dict(),
            "configuration": {
                "steel_od_mm": self.steel_od_mm,
                "steel_wt_mm": self.steel_wt_mm,
                "steel_density": self.steel_density,
                "lpp_thickness_mm": self.lpp_thickness_mm,
                "lpp_density": self.lpp_density,
                "concrete_thickness_mm": self.concrete_thickness_mm,
                "concrete_density": self.concrete_density,
                "seawater_density": self.seawater_density,
                "internal_contents": self.internal_contents,
            },
        }

    def to_csv_data(self) -> List[Dict[str, Any]]:
        """Generate CSV-ready data rows."""
        rows = []

        # Layer data
        for layer in self.layers:
            rows.append(layer.to_dict())

        # Summary row
        rows.append({
            "Layer": "TOTAL",
            "ID_mm": round(self.inner_diameter_mm, 1),
            "OD_mm": round(self.outer_diameter_mm, 1),
            "Thickness_mm": "-",
            "Density_kg_m3": "-",
            "Area_m2_m": round(sum(l.cross_sectional_area_m2 for l in self.layers), 5),
            "Weight_kg_m": round(self.total_weight_in_air_kg_m, 1),
            "Weight_kN_m": round(self.total_weight_in_air_kN_m, 4),
        })

        return rows

    def print_summary(self):
        """Print formatted summary to console."""
        print("\n" + "=" * 65)
        print("PIPE CROSS-SECTION SUMMARY")
        print("=" * 65)

        print(f"\n{'Layer':<20} {'ID (mm)':>10} {'OD (mm)':>10} {'Thickness':>12} {'Weight':>12}")
        print("-" * 65)

        for layer in self.layers:
            print(
                f"{layer.name:<20} {layer.inner_diameter_mm:>10.1f} "
                f"{layer.outer_diameter_mm:>10.1f} {layer.thickness_mm:>10.2f} mm "
                f"{layer.weight_per_meter_kg:>10.1f} kg/m"
            )

        print("-" * 65)
        print(f"{'Final OD:':<20} {self.outer_diameter_mm:>10.1f} mm ({self.outer_diameter_inch:.2f} in)")
        print(f"{'Total Weight:':<20} {self.total_weight_in_air_kg_m:>10.1f} kg/m ({self.total_weight_in_air_kN_m:.3f} kN/m)")

        print("\nBUOYANCY ANALYSIS:")
        print("-" * 45)
        print(f"{'Displaced Volume:':<25} {self.displaced_volume_m3_m:>10.4f} m³/m")
        print(f"{'Buoyancy Force:':<25} {self.buoyancy_force_kg_m:>10.1f} kg/m")
        print(f"{'Submerged Weight:':<25} {self.submerged_weight_kg_m:>10.1f} kg/m ({self.submerged_weight_kN_m:.3f} kN/m)")

        status = "FLOATS (positive buoyancy)" if self.is_buoyant else "SINKS (negative buoyancy)"
        print(f"\nStatus: {status}")
        print("=" * 65 + "\n")
