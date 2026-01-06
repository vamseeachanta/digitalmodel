#!/usr/bin/env python3
"""
ABOUTME: Data models for VIV analysis including tubular member properties,
current profiles, natural frequencies, and VIV susceptibility results.
"""

from dataclasses import dataclass, field
from typing import List, Dict, Optional, Tuple
from enum import Enum
import numpy as np


class BoundaryCondition(Enum):
    """Boundary conditions for beam vibration."""
    PINNED_PINNED = "pinned-pinned"
    FIXED_FIXED = "fixed-fixed"
    FIXED_PINNED = "fixed-pinned"
    FREE_FREE = "free-free"
    CANTILEVER = "cantilever"
    TENSION_CONTROLLED = "tension-controlled"  # For risers with tension


class CurrentProfileType(Enum):
    """Types of current velocity profiles."""
    UNIFORM = "uniform"
    POWER_LAW = "power_law"
    LINEAR = "linear"
    CUSTOM = "custom"


class DesignCode(Enum):
    """Design codes for VIV assessment."""
    DNV_RP_C205 = "DNV-RP-C205"
    DNV_RP_F105 = "DNV-RP-F105"
    API_RP_2A = "API-RP-2A"
    ISO_13819_2 = "ISO-13819-2"


@dataclass
class MaterialProperties:
    """Material properties for VIV analysis."""
    youngs_modulus: float    # Pa (N/m²)
    density: float           # kg/m³
    poissons_ratio: float = 0.3
    name: str = "Steel"


@dataclass
class TubularMember:
    """Tubular member geometry and properties."""
    name: str
    length: float                    # meters
    outer_diameter: float            # meters
    wall_thickness: float            # meters
    material: MaterialProperties
    boundary_condition: BoundaryCondition = BoundaryCondition.PINNED_PINNED
    effective_length_factor: float = 1.0
    top_tension: Optional[float] = None  # N (for tension-controlled members like risers)

    @property
    def inner_diameter(self) -> float:
        """Inner diameter (m)."""
        return self.outer_diameter - 2 * self.wall_thickness

    @property
    def cross_sectional_area(self) -> float:
        """Cross-sectional area (m²)."""
        return np.pi / 4 * (self.outer_diameter**2 - self.inner_diameter**2)

    @property
    def second_moment_of_area(self) -> float:
        """Second moment of area I (m⁴)."""
        return np.pi / 64 * (self.outer_diameter**4 - self.inner_diameter**4)

    @property
    def mass_per_length(self) -> float:
        """Mass per unit length (kg/m)."""
        return self.cross_sectional_area * self.material.density


@dataclass
class FluidProperties:
    """Fluid properties for added mass calculation."""
    density: float = 1025.0           # kg/m³ (seawater)
    kinematic_viscosity: float = 1.19e-6  # m²/s
    added_mass_coefficient: float = 1.0   # Ca (typically 1.0 for circular cylinder)


@dataclass
class CurrentProfile:
    """Current velocity profile with depth."""
    profile_type: CurrentProfileType
    surface_velocity: float          # m/s
    reference_depth: float = 0.0     # m (surface)

    # For power law: V(z) = V_surface * (z / z_ref)^exponent
    power_exponent: float = 0.143    # Typical 1/7th power law

    # For linear: V(z) = V_surface + gradient * z
    velocity_gradient: float = 0.0   # (m/s) / m

    # For custom profile
    depths: Optional[List[float]] = None       # m (positive down)
    velocities: Optional[List[float]] = None   # m/s

    def velocity_at_depth(self, depth: float) -> float:
        """
        Calculate current velocity at given depth.

        Args:
            depth: Depth below surface (m, positive down)

        Returns:
            Current velocity (m/s)
        """
        if self.profile_type == CurrentProfileType.UNIFORM:
            return self.surface_velocity

        elif self.profile_type == CurrentProfileType.POWER_LAW:
            if depth <= 0:
                return self.surface_velocity
            # V(z) = V_surface * (z/z_ref)^n, typically z_ref = 1m
            z_ref = max(self.reference_depth, 1.0)
            return self.surface_velocity * (depth / z_ref) ** self.power_exponent

        elif self.profile_type == CurrentProfileType.LINEAR:
            return self.surface_velocity + self.velocity_gradient * depth

        elif self.profile_type == CurrentProfileType.CUSTOM:
            if self.depths is None or self.velocities is None:
                raise ValueError("Custom profile requires depths and velocities")
            return np.interp(depth, self.depths, self.velocities)

        else:
            raise ValueError(f"Unknown profile type: {self.profile_type}")


@dataclass
class NaturalFrequencyResult:
    """Results from natural frequency calculation."""
    member_name: str
    mode_number: int
    frequency: float             # Hz
    period: float                # seconds
    angular_frequency: float     # rad/s
    wavelength: float            # meters
    boundary_condition: str
    includes_added_mass: bool
    effective_mass_ratio: float  # (m + m_added) / m


@dataclass
class VortexSheddingResult:
    """Results from vortex shedding analysis."""
    diameter: float                   # m
    current_velocity: float           # m/s
    strouhal_number: float
    shedding_frequency: float         # Hz
    reynolds_number: float
    depth: Optional[float] = None     # m (if depth-specific)


@dataclass
class ReducedVelocityResult:
    """Reduced velocity calculation results."""
    current_velocity: float           # m/s
    natural_frequency: float          # Hz
    diameter: float                   # m
    reduced_velocity: float           # V_r = V / (f_n * D)
    is_lock_in: bool                  # True if in lock-in range
    lock_in_margin: float             # Distance from lock-in range


@dataclass
class VIVScreeningResult:
    """VIV susceptibility screening results."""
    member_name: str
    natural_frequency: float          # Hz
    shedding_frequency: float         # Hz
    reduced_velocity: float
    is_susceptible: bool
    lock_in_status: str               # "safe", "marginal", "lock-in"
    safety_factor: float
    recommendation: str
    details: Dict = field(default_factory=dict)


@dataclass
class VIVFatigueResult:
    """VIV-induced fatigue damage results."""
    stress_range: float               # MPa
    frequency: float                  # Hz (VIV frequency)
    duration: float                   # seconds
    num_cycles: float
    fatigue_damage: float             # Miner's sum
    fatigue_life_years: float
    sn_curve: str


# Pre-defined materials
STEEL_CARBON = MaterialProperties(
    youngs_modulus=207e9,
    density=7850.0,
    poissons_ratio=0.3,
    name="Carbon Steel"
)

STEEL_STAINLESS = MaterialProperties(
    youngs_modulus=193e9,
    density=8000.0,
    poissons_ratio=0.3,
    name="Stainless Steel"
)

TITANIUM = MaterialProperties(
    youngs_modulus=110e9,
    density=4500.0,
    poissons_ratio=0.34,
    name="Titanium"
)

# Material library
VIV_MATERIALS = {
    'steel': STEEL_CARBON,
    'steel_carbon': STEEL_CARBON,
    'steel_stainless': STEEL_STAINLESS,
    'stainless': STEEL_STAINLESS,
    'titanium': TITANIUM,
    'ti': TITANIUM,
}


def get_material(material_name: str) -> MaterialProperties:
    """
    Get material by name from library.

    Args:
        material_name: Material identifier

    Returns:
        MaterialProperties instance

    Raises:
        KeyError: If material not found
    """
    return VIV_MATERIALS[material_name.lower()]


# VIV parameters from design codes

# Strouhal numbers for different cylinder types
STROUHAL_NUMBERS = {
    'smooth': 0.20,
    'rough': 0.21,
    'straked': 0.17,  # Average for straked cylinders
    'helical_strake': 0.16,
}

# Reduced velocity ranges for lock-in (DNV-RP-C205)
LOCK_IN_RANGES = {
    'cross_flow': (4.0, 8.0),      # Cross-flow VIV
    'in_line': (1.0, 3.5),          # In-line VIV
    'critical': (5.0, 7.0),         # Most critical range
}

# Added mass coefficients
ADDED_MASS_COEFFICIENTS = {
    'infinite_fluid': 1.0,
    'near_seabed_0.5D': 1.2,  # Gap/D = 0.5
    'near_seabed_0.1D': 2.0,  # Gap/D = 0.1
}
