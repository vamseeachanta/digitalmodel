#!/usr/bin/env python3
"""
ABOUTME: Data models for mooring system analysis including CALM/SALM buoys,
spread mooring configurations, line properties, and design results.
"""

from dataclasses import dataclass, field
from typing import List, Dict, Optional, Tuple
from enum import Enum


class MooringType(Enum):
    """Types of mooring systems."""
    CALM = "calm"
    SALM = "salm"
    SPREAD = "spread"
    TURRET = "turret"


class LineType(Enum):
    """Types of mooring line materials."""
    CHAIN = "chain"
    WIRE = "wire"
    POLYESTER = "polyester"
    COMBINATION = "combination"


class ConditionType(Enum):
    """Load condition types for safety factor assessment."""
    INTACT = "intact"
    DAMAGED = "damaged"
    TRANSIENT = "transient"


@dataclass
class MooringLineProperties:
    """Properties of a mooring line segment."""
    line_type: LineType
    length: float            # meters
    diameter: float          # mm
    mbl: float              # Minimum Breaking Load (kN)
    weight_water: float     # Weight in water (kg/m)
    ea: float               # Axial stiffness EA (kN)
    drag_coeff: float = 2.4 # Normal drag coefficient
    name: str = ""

    def __post_init__(self):
        if not self.name:
            self.name = f"{self.line_type.value}_{self.diameter}mm"


@dataclass
class AnchorProperties:
    """Anchor properties."""
    anchor_type: str         # drag, suction, pile, driven_pile
    holding_capacity: float  # kN
    location: Tuple[float, float, float]  # (x, y, z) in meters
    name: str = ""

    def __post_init__(self):
        if not self.name:
            self.name = f"{self.anchor_type}_anchor"


@dataclass
class MooringLine:
    """Complete mooring line configuration."""
    line_id: str
    segments: List[MooringLineProperties]
    anchor: AnchorProperties
    fairlead_location: Tuple[float, float, float]  # (x, y, z) meters
    pretension: float = 0.0  # kN

    @property
    def total_length(self) -> float:
        """Total line length in meters."""
        return sum(seg.length for seg in self.segments)

    @property
    def min_mbl(self) -> float:
        """Minimum breaking load of weakest segment (kN)."""
        return min(seg.mbl for seg in self.segments)


@dataclass
class EnvironmentalConditions:
    """Environmental loading conditions."""
    wave_hs: float           # Significant wave height (m)
    wave_tp: float           # Peak period (s)
    wave_direction: float    # degrees from North
    current_speed: float     # m/s at surface
    current_direction: float # degrees from North
    wind_speed: float        # m/s at 10m height
    wind_direction: float    # degrees from North
    return_period: float = 100  # years
    name: str = ""

    def __post_init__(self):
        if not self.name:
            self.name = f"{self.return_period}yr_return"


@dataclass
class VesselParticulars:
    """Vessel characteristics for environmental load calculations."""
    vessel_type: str         # tanker, fpso, semi-submersible, etc.
    length: float            # Length overall (m)
    beam: float              # Beam (m)
    draft: float             # Draft (m)
    displacement: float      # Displacement (tonnes)
    windage_area: float      # Projected wind area (m²)
    name: str = ""

    def __post_init__(self):
        if not self.name:
            self.name = f"{self.vessel_type}_{int(self.length)}m"


@dataclass
class MooringSystem:
    """Complete mooring system configuration."""
    system_type: MooringType
    water_depth: float       # meters
    lines: List[MooringLine]
    vessel: VesselParticulars
    design_life_years: float = 20.0
    name: str = ""

    def __post_init__(self):
        if not self.name:
            self.name = f"{self.system_type.value}_{len(self.lines)}lines"

    @property
    def n_lines(self) -> int:
        """Number of mooring lines."""
        return len(self.lines)


@dataclass
class CatenaryResult:
    """Results from catenary analysis."""
    horizontal_tension: float      # kN
    fairlead_tension: float       # kN at fairlead
    touchdown_tension: float      # kN at touchdown (= H)
    arc_length: float             # Suspended line length (m)
    horizontal_distance: float    # Horizontal projection (m)
    fairlead_angle: float         # Angle at fairlead (degrees)
    catenary_parameter: float     # a = H/w (m)
    grounded_length: float        # Line on seabed (m)
    vertical_height: float        # Vertical span (m)


@dataclass
class StiffnessResult:
    """Mooring line stiffness results."""
    horizontal_stiffness: float   # kN/m
    vertical_stiffness: float     # kN/m
    geometric_stiffness: float    # kN/m
    elastic_stiffness: float      # kN/m


@dataclass
class EnvironmentalLoads:
    """Environmental loads on vessel."""
    wave_drift_force: float       # kN
    current_force: float          # kN
    wind_force: float             # kN
    total_force: float            # kN
    direction: float              # degrees from North


@dataclass
class DesignLoadCase:
    """Design load case for mooring analysis."""
    name: str
    condition: ConditionType
    environment: EnvironmentalConditions
    damaged_line_id: Optional[str] = None  # For damaged cases

    @property
    def safety_factor_required(self) -> float:
        """Required safety factor per DNV-OS-E301."""
        # DNV-OS-E301 safety factors for dynamic analysis
        if self.condition == ConditionType.INTACT:
            return 1.67
        elif self.condition == ConditionType.DAMAGED:
            return 1.25
        else:  # TRANSIENT
            return 1.05


@dataclass
class MooringDesignResult:
    """Results from mooring design analysis."""
    line_id: str
    load_case: str
    max_tension: float        # Maximum line tension (kN)
    min_mbl_required: float   # Required MBL based on SF (kN)
    actual_mbl: float         # Actual MBL of line (kN)
    safety_factor: float      # Actual safety factor
    utilization: float        # Tension / MBL
    passes: bool              # True if SF >= required
    details: Dict = field(default_factory=dict)


# Pre-defined mooring line materials (common offshore grades)

# Studlink chain R3 grade
CHAIN_R3_84MM = MooringLineProperties(
    line_type=LineType.CHAIN,
    length=0.0,  # To be specified per application
    diameter=84.0,
    mbl=8500.0,
    weight_water=145.0,
    ea=850000.0,
    drag_coeff=2.4,
    name="Chain_R3_84mm"
)

CHAIN_R3_102MM = MooringLineProperties(
    line_type=LineType.CHAIN,
    length=0.0,
    diameter=102.0,
    mbl=12200.0,
    weight_water=210.0,
    ea=1050000.0,
    drag_coeff=2.4,
    name="Chain_R3_102mm"
)

CHAIN_R4_84MM = MooringLineProperties(
    line_type=LineType.CHAIN,
    length=0.0,
    diameter=84.0,
    mbl=9800.0,
    weight_water=145.0,
    ea=850000.0,
    drag_coeff=2.4,
    name="Chain_R4_84mm"
)

# Wire rope
WIRE_76MM = MooringLineProperties(
    line_type=LineType.WIRE,
    length=0.0,
    diameter=76.0,
    mbl=4950.0,
    weight_water=27.5,
    ea=550000.0,
    drag_coeff=1.2,
    name="Wire_76mm"
)

# Polyester rope
POLYESTER_140MM = MooringLineProperties(
    line_type=LineType.POLYESTER,
    length=0.0,
    diameter=140.0,
    mbl=7200.0,
    weight_water=-8.0,  # Negative = buoyant
    ea=180000.0,
    drag_coeff=1.6,
    name="Polyester_140mm"
)

POLYESTER_180MM = MooringLineProperties(
    line_type=LineType.POLYESTER,
    length=0.0,
    diameter=180.0,
    mbl=12000.0,
    weight_water=-13.0,
    ea=280000.0,
    drag_coeff=1.6,
    name="Polyester_180mm"
)


# Material library
MOORING_MATERIALS = {
    'chain_r3_84': CHAIN_R3_84MM,
    'chain_r3_102': CHAIN_R3_102MM,
    'chain_r4_84': CHAIN_R4_84MM,
    'wire_76': WIRE_76MM,
    'polyester_140': POLYESTER_140MM,
    'polyester_180': POLYESTER_180MM,
}


def get_material(material_name: str) -> MooringLineProperties:
    """
    Get mooring line material by name.

    Args:
        material_name: Material identifier (e.g., 'chain_r3_84')

    Returns:
        MooringLineProperties instance

    Raises:
        KeyError: If material not found
    """
    return MOORING_MATERIALS[material_name.lower()]
