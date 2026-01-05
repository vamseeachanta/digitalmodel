#!/usr/bin/env python3
"""
ABOUTME: Data models for catenary riser analysis including simple catenary,
lazy wave configurations, buoyancy modules, and results per DNV-OS-F201 and API RP 1111.
"""

from dataclasses import dataclass, field
from typing import Optional, List, Dict
from enum import Enum
import numpy as np


class RiserType(Enum):
    """Riser configuration types"""
    SIMPLE_CATENARY = "simple_catenary"
    LAZY_WAVE = "lazy_wave"
    STEEP_WAVE = "steep_wave"
    STEEP_S = "steep_s"
    PLIANT_WAVE = "pliant_wave"


class FluidType(Enum):
    """Internal fluid types"""
    EMPTY = "empty"
    AIR = "air"
    GAS = "gas"
    OIL = "oil"
    WATER = "water"
    MUD = "mud"


# ============================================================================
# Material and Fluid Properties
# ============================================================================

@dataclass
class MaterialProperties:
    """Material properties for riser pipe"""
    name: str
    youngs_modulus: float  # MPa
    poissons_ratio: float
    density: float  # kg/m³
    yield_strength: Optional[float] = None  # MPa
    ultimate_strength: Optional[float] = None  # MPa


@dataclass
class FluidProperties:
    """Fluid properties (internal or external)"""
    name: str
    density: float  # kg/m³
    kinematic_viscosity: Optional[float] = None  # m²/s
    bulk_modulus: Optional[float] = None  # MPa


# Standard materials
STEEL_API_5L_X65 = MaterialProperties(
    name="API 5L X65",
    youngs_modulus=207_000,
    poissons_ratio=0.3,
    density=7850,
    yield_strength=448,
    ultimate_strength=531
)

STEEL_API_5L_X70 = MaterialProperties(
    name="API 5L X70",
    youngs_modulus=207_000,
    poissons_ratio=0.3,
    density=7850,
    yield_strength=483,
    ultimate_strength=565
)

# Standard fluids
SEAWATER = FluidProperties(
    name="Seawater",
    density=1025,
    kinematic_viscosity=1.35e-6
)

PRODUCTION_OIL = FluidProperties(
    name="Production Oil",
    density=850,
    kinematic_viscosity=5e-6
)

DRILLING_MUD = FluidProperties(
    name="Drilling Mud",
    density=1200,
    kinematic_viscosity=3e-5
)

AIR = FluidProperties(
    name="Air",
    density=1.225,
    kinematic_viscosity=1.5e-5
)


# ============================================================================
# Riser Configuration
# ============================================================================

@dataclass
class RiserConfiguration:
    """Complete riser configuration including geometry and materials"""
    name: str
    outer_diameter: float  # m
    wall_thickness: float  # m
    length: float  # m
    material: MaterialProperties
    internal_fluid: FluidProperties
    external_fluid: FluidProperties = field(default_factory=lambda: SEAWATER)

    # Optional coating
    coating_thickness: Optional[float] = None  # m
    coating_density: Optional[float] = None  # kg/m³

    # Top connection
    top_tension_applied: Optional[float] = None  # N
    top_angle: Optional[float] = None  # degrees from vertical

    # Touchdown point
    water_depth: Optional[float] = None  # m
    horizontal_offset: Optional[float] = None  # m

    @property
    def inner_diameter(self) -> float:
        """Inner diameter of pipe (m)"""
        return self.outer_diameter - 2 * self.wall_thickness

    @property
    def pipe_cross_section(self) -> float:
        """Pipe wall cross-sectional area (m²)"""
        return np.pi / 4 * (self.outer_diameter**2 - self.inner_diameter**2)

    @property
    def internal_area(self) -> float:
        """Internal flow area (m²)"""
        return np.pi / 4 * self.inner_diameter**2

    @property
    def external_area(self) -> float:
        """External area including coating (m²)"""
        if self.coating_thickness:
            D_coat = self.outer_diameter + 2 * self.coating_thickness
            return np.pi / 4 * D_coat**2
        return np.pi / 4 * self.outer_diameter**2

    @property
    def displaced_volume_per_length(self) -> float:
        """Volume displaced per unit length (m³/m)"""
        return self.external_area

    @property
    def steel_weight_per_length(self) -> float:
        """Steel weight per unit length in air (N/m)"""
        return self.pipe_cross_section * self.material.density * 9.81

    @property
    def coating_weight_per_length(self) -> float:
        """Coating weight per unit length in air (N/m)"""
        if self.coating_thickness and self.coating_density:
            D_outer = self.outer_diameter
            D_coat = self.outer_diameter + 2 * self.coating_thickness
            A_coat = np.pi / 4 * (D_coat**2 - D_outer**2)
            return A_coat * self.coating_density * 9.81
        return 0.0

    @property
    def contents_weight_per_length(self) -> float:
        """Contents weight per unit length (N/m)"""
        return self.internal_area * self.internal_fluid.density * 9.81

    @property
    def buoyancy_per_length(self) -> float:
        """Buoyancy force per unit length (N/m)"""
        return self.displaced_volume_per_length * self.external_fluid.density * 9.81

    @property
    def effective_weight_per_length(self) -> float:
        """
        Effective weight per unit length in water (N/m)

        w_eff = w_steel + w_coating + w_contents - w_buoyancy

        Positive = sink, Negative = float
        """
        return (
            self.steel_weight_per_length +
            self.coating_weight_per_length +
            self.contents_weight_per_length -
            self.buoyancy_per_length
        )


# ============================================================================
# Buoyancy Modules
# ============================================================================

@dataclass
class BuoyancyModule:
    """Buoyancy module configuration for lazy wave risers"""
    name: str
    length: float  # m
    outer_diameter: float  # m
    buoyancy_material_density: float  # kg/m³ (syntactic foam, air-filled, etc.)

    # Location on riser
    start_length: float  # m from bottom

    # Optional coating over buoyancy
    outer_coating_thickness: Optional[float] = None  # m
    outer_coating_density: Optional[float] = None  # kg/m³

    def buoyancy_force_per_length(self, external_fluid: FluidProperties) -> float:
        """Net buoyancy force per unit length (N/m)"""
        # Volume displaced
        if self.outer_coating_thickness:
            D_outer = self.outer_diameter + 2 * self.outer_coating_thickness
        else:
            D_outer = self.outer_diameter

        V_displaced = np.pi / 4 * D_outer**2

        # Weight of buoyancy material
        V_buoy = np.pi / 4 * self.outer_diameter**2
        w_buoy = V_buoy * self.buoyancy_material_density * 9.81

        # Weight of outer coating
        if self.outer_coating_thickness and self.outer_coating_density:
            V_coat = np.pi / 4 * (D_outer**2 - self.outer_diameter**2)
            w_coat = V_coat * self.outer_coating_density * 9.81
        else:
            w_coat = 0.0

        # Buoyancy
        w_displaced = V_displaced * external_fluid.density * 9.81

        # Net buoyancy (upward force)
        return w_displaced - w_buoy - w_coat


# ============================================================================
# Lazy Wave Configuration
# ============================================================================

@dataclass
class LazyWaveConfiguration:
    """Complete lazy wave riser configuration"""
    riser: RiserConfiguration
    buoyancy_modules: List[BuoyancyModule]

    # Target geometry
    target_sag_bend_depth: Optional[float] = None  # m below surface
    target_hog_bend_depth: Optional[float] = None  # m below surface
    target_arch_height: Optional[float] = None  # m

    # Constraints
    max_top_tension: Optional[float] = None  # N
    min_touchdown_tension: Optional[float] = None  # N
    max_angle_at_hang_off: Optional[float] = None  # degrees from vertical

    def total_buoyancy_length(self) -> float:
        """Total length of buoyancy modules (m)"""
        return sum(m.length for m in self.buoyancy_modules)

    def get_effective_weight_profile(self) -> Dict[str, float]:
        """Get effective weight at different riser sections (N/m)"""
        profile = {}

        # Bare riser sections
        bare_weight = self.riser.effective_weight_per_length

        # For each buoyancy module, calculate net effect
        for module in self.buoyancy_modules:
            # Buoyancy adds upward force
            buoy_force = module.buoyancy_force_per_length(self.riser.external_fluid)
            # Net weight in buoyancy section
            section_weight = bare_weight + buoy_force  # buoy_force is negative (upward)

            profile[module.name] = section_weight

        profile['bare_riser'] = bare_weight

        return profile


# ============================================================================
# Results
# ============================================================================

@dataclass
class CatenaryRiserResult:
    """Results from simple catenary riser analysis"""
    # Configuration
    riser_name: str
    water_depth: float  # m
    horizontal_offset: float  # m

    # Catenary parameters
    horizontal_tension: float  # N
    catenary_parameter: float  # m
    effective_weight: float  # N/m

    # Geometry
    arc_length: float  # m (suspended length)
    grounded_length: float  # m (on seabed)

    # Forces
    top_tension: float  # N
    touchdown_tension: float  # N

    # Angles
    top_angle: float  # degrees from vertical
    touchdown_angle: float  # degrees from horizontal

    # Maximum values
    max_tension: float  # N
    max_curvature: Optional[float] = None  # 1/m

    # Utilization
    tension_utilization: Optional[float] = None  # fraction of capacity


@dataclass
class LazyWaveResult:
    """Results from lazy wave riser analysis"""
    # Configuration
    configuration_name: str

    # Geometry
    total_length: float  # m
    suspended_length: float  # m
    grounded_length: float  # m

    # Lazy wave geometry
    sag_bend_depth: float  # m below surface
    hog_bend_depth: float  # m below surface
    arch_height: float  # m (vertical distance sag to hog)

    # Forces
    top_tension: float  # N
    tension_at_sag_bend: float  # N (minimum tension point)
    tension_at_hog_bend: float  # N (local maximum)
    touchdown_tension: float  # N

    # Angles
    top_angle: float  # degrees from vertical
    angle_at_sag_bend: float  # degrees
    angle_at_hog_bend: float  # degrees

    # Buoyancy effectiveness
    buoyancy_utilization: float  # fraction
    effective_arch_stiffness: Optional[float] = None

    # Safety margins
    top_tension_margin: Optional[float] = None  # vs. capacity
    compression_margin: Optional[float] = None  # vs. buckling

    # Optimization metrics
    is_optimized: bool = False
    optimization_iterations: Optional[int] = None


@dataclass
class EffectiveWeightResult:
    """Detailed effective weight calculation results"""
    # Component weights (N/m)
    steel_weight: float
    coating_weight: float
    contents_weight: float
    buoyancy: float

    # Net effective weight
    effective_weight: float  # Positive = sink, Negative = float

    # Breakdown (for reporting)
    total_dry_weight: float
    total_submerged_weight: float

    # With buoyancy modules (if applicable)
    buoyancy_module_force: Optional[float] = None  # N/m in buoyancy section
    effective_weight_with_buoyancy: Optional[float] = None

    def to_dict(self) -> Dict[str, float]:
        """Export to dictionary"""
        return {
            'steel_weight_N_per_m': self.steel_weight,
            'coating_weight_N_per_m': self.coating_weight,
            'contents_weight_N_per_m': self.contents_weight,
            'buoyancy_N_per_m': self.buoyancy,
            'effective_weight_N_per_m': self.effective_weight,
            'total_dry_weight_N_per_m': self.total_dry_weight,
            'total_submerged_weight_N_per_m': self.total_submerged_weight,
        }


# ============================================================================
# Helper Functions
# ============================================================================

def get_material(name: str) -> MaterialProperties:
    """Get material by name"""
    materials = {
        'x65': STEEL_API_5L_X65,
        'api5l_x65': STEEL_API_5L_X65,
        'x70': STEEL_API_5L_X70,
        'api5l_x70': STEEL_API_5L_X70,
    }

    key = name.lower().replace(' ', '_').replace('-', '_')
    if key not in materials:
        raise ValueError(f"Unknown material: {name}. Available: {list(materials.keys())}")

    return materials[key]


def get_fluid(name: str) -> FluidProperties:
    """Get fluid by name"""
    fluids = {
        'seawater': SEAWATER,
        'water': SEAWATER,
        'oil': PRODUCTION_OIL,
        'mud': DRILLING_MUD,
        'air': AIR,
        'empty': AIR,
    }

    key = name.lower().replace(' ', '_').replace('-', '_')
    if key not in fluids:
        raise ValueError(f"Unknown fluid: {name}. Available: {list(fluids.keys())}")

    return fluids[key]


# Material library
RISER_MATERIALS = {
    'API 5L X65': STEEL_API_5L_X65,
    'API 5L X70': STEEL_API_5L_X70,
}

# Fluid library
FLUIDS = {
    'Seawater': SEAWATER,
    'Production Oil': PRODUCTION_OIL,
    'Drilling Mud': DRILLING_MUD,
    'Air': AIR,
}
