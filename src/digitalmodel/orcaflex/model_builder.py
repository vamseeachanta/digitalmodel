"""Build OrcaFlex model configurations as Python dicts.

Provides dataclass-like builders for common OrcaFlex objects (vessel, line,
6DOF buoy) with realistic defaults for typical offshore configurations:
SCR, lazy-wave riser, mooring lines. Output as dict that can be serialized
to YAML/JSON for OrcaFlex data file import or batch processing.

Does NOT require OrcFxAPI — all operations produce plain dicts.

References:
    - OrcaFlex documentation: Object Data reference
    - API RP 2RD: Design of Risers for Floating Production Systems
    - API RP 2SK: Design and Analysis of Stationkeeping Systems
"""

import copy
import math
from enum import Enum
from typing import Any, Dict, List, Optional, Tuple

from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------

class ObjectType(str, Enum):
    """OrcaFlex object types."""
    VESSEL = "Vessel"
    LINE = "Line"
    BUOY_6DOF = "6D Buoy"
    SHAPE = "Shape"
    WINCH = "Winch"
    LINK = "Link"
    CONSTRAINT = "Constraint"


class LineEndCondition(str, Enum):
    """Line end connection types."""
    FREE = "Free"
    ANCHORED = "Anchored"
    CONNECTED = "Connected"


class LineType(str, Enum):
    """Predefined line type categories."""
    FLEXIBLE_RISER = "FlexibleRiser"
    RIGID_RISER = "RigidRiser"
    CHAIN = "Chain"
    WIRE_ROPE = "WireRope"
    POLYESTER = "Polyester"
    UMBILICAL = "Umbilical"


# ---------------------------------------------------------------------------
# Line section & type definitions
# ---------------------------------------------------------------------------

class LineSectionProperties(BaseModel):
    """Properties for a single line section.

    Stores cross-section geometry and material properties needed for
    OrcaFlex line type definition.
    """
    outer_diameter: float = Field(0.3048, gt=0.0, description="Outer diameter (m)")
    inner_diameter: float = Field(0.2032, ge=0.0, description="Inner diameter (m)")
    mass_per_unit_length: float = Field(100.0, gt=0.0, description="Mass per unit length (kg/m)")
    bending_stiffness: float = Field(1.0e6, ge=0.0, description="Bending stiffness EI (N.m^2)")
    axial_stiffness: float = Field(1.0e9, gt=0.0, description="Axial stiffness EA (N)")
    cd_normal: float = Field(1.2, gt=0.0, description="Normal drag coefficient Cdn")
    cd_axial: float = Field(0.008, ge=0.0, description="Axial drag coefficient Cda")
    cm_normal: float = Field(2.0, gt=0.0, description="Normal added mass coefficient Cmn")
    ca_normal: float = Field(1.0, ge=0.0, description="Normal added mass coefficient Ca")
    contents_density: float = Field(0.0, ge=0.0, description="Contents density (kg/m^3)")

    @property
    def weight_in_water_per_m(self) -> float:
        """Submerged weight per unit length (N/m) without contents."""
        rho_sw = 1025.0
        g = 9.80665
        displaced_area = math.pi / 4.0 * self.outer_diameter**2
        buoyancy = rho_sw * g * displaced_area
        return self.mass_per_unit_length * g - buoyancy


class LineSection(BaseModel):
    """A segment within a line (length + properties reference)."""
    length: float = Field(100.0, gt=0.0, description="Section length (m)")
    line_type_name: str = Field("DefaultLineType", description="Name of line type")
    target_segment_length: float = Field(1.0, gt=0.0, description="Target segment length (m)")


# ---------------------------------------------------------------------------
# Object builders
# ---------------------------------------------------------------------------

class VesselConfig(BaseModel):
    """Vessel (FPSO / drillship / semi-sub) configuration.

    Reference: OrcaFlex Vessel data reference.
    """
    name: str = Field("Vessel1", description="Object name")
    length: float = Field(280.0, gt=0.0, description="Length overall (m)")
    breadth: float = Field(46.0, gt=0.0, description="Breadth (m)")
    depth: float = Field(25.0, gt=0.0, description="Depth (m)")
    draft: float = Field(12.0, gt=0.0, description="Operating draft (m)")
    displacement: float = Field(150000.0, gt=0.0, description="Displacement (te)")
    initial_position: Tuple[float, float, float] = Field((0.0, 0.0, 0.0),
                                                         description="Initial (x, y, z) position (m)")
    initial_heading: float = Field(0.0, description="Initial heading (deg)")

    def to_orcaflex_dict(self) -> Dict[str, Any]:
        """Generate OrcaFlex vessel data dict."""
        return {
            "ObjectType": ObjectType.VESSEL.value,
            "Name": self.name,
            "Length": self.length,
            "InitialX": self.initial_position[0],
            "InitialY": self.initial_position[1],
            "InitialZ": self.initial_position[2],
            "InitialHeading": self.initial_heading,
            "Draught": self.draft,
            "Connection": "Free",
        }


class Buoy6DConfig(BaseModel):
    """6DOF Buoy configuration.

    Common uses: clump weight, buoyancy module, mid-water arch.
    """
    name: str = Field("Buoy1", description="Object name")
    mass: float = Field(5000.0, ge=0.0, description="Mass (kg)")
    volume: float = Field(10.0, ge=0.0, description="Displaced volume (m^3)")
    height: float = Field(2.0, gt=0.0, description="Height (m)")
    initial_position: Tuple[float, float, float] = Field((100.0, 0.0, -500.0),
                                                         description="Initial (x, y, z) position")

    @property
    def net_buoyancy(self) -> float:
        """Net buoyancy force (N). Positive = upward."""
        rho_sw = 1025.0
        g = 9.80665
        return (rho_sw * self.volume - self.mass) * g

    def to_orcaflex_dict(self) -> Dict[str, Any]:
        """Generate OrcaFlex 6D buoy data dict."""
        return {
            "ObjectType": ObjectType.BUOY_6DOF.value,
            "Name": self.name,
            "Mass": self.mass,
            "Volume": self.volume,
            "Height": self.height,
            "InitialX": self.initial_position[0],
            "InitialY": self.initial_position[1],
            "InitialZ": self.initial_position[2],
        }


class LineConfig(BaseModel):
    """Line object configuration (riser, mooring, umbilical).

    Reference: OrcaFlex Line data reference.
    """
    name: str = Field("Line1", description="Object name")
    sections: List[LineSection] = Field(default_factory=lambda: [LineSection()],
                                       description="Line sections")
    end_a_connection: LineEndCondition = LineEndCondition.CONNECTED
    end_a_object: str = Field("Vessel1", description="End A connection object")
    end_a_position: Tuple[float, float, float] = Field((0.0, -20.0, -10.0),
                                                        description="End A connection coordinates")
    end_b_connection: LineEndCondition = LineEndCondition.ANCHORED
    end_b_object: str = Field("", description="End B connection object (if connected)")
    end_b_position: Tuple[float, float, float] = Field((500.0, 0.0, 0.0),
                                                        description="End B position/anchor point")

    @property
    def total_length(self) -> float:
        """Total line length (m)."""
        return sum(s.length for s in self.sections)

    def to_orcaflex_dict(self) -> Dict[str, Any]:
        """Generate OrcaFlex line data dict."""
        section_data = []
        for s in self.sections:
            section_data.append({
                "Length": s.length,
                "LineType": s.line_type_name,
                "TargetSegmentLength": s.target_segment_length,
            })

        return {
            "ObjectType": ObjectType.LINE.value,
            "Name": self.name,
            "EndAConnection": self.end_a_connection.value,
            "EndAConnectedObject": self.end_a_object,
            "EndAX": self.end_a_position[0],
            "EndAY": self.end_a_position[1],
            "EndAZ": self.end_a_position[2],
            "EndBConnection": self.end_b_connection.value,
            "EndBConnectedObject": self.end_b_object,
            "EndBX": self.end_b_position[0],
            "EndBY": self.end_b_position[1],
            "EndBZ": self.end_b_position[2],
            "Sections": section_data,
        }


# ---------------------------------------------------------------------------
# Predefined configurations (factory functions)
# ---------------------------------------------------------------------------

# Material property library
MATERIAL_LIBRARY: Dict[str, LineSectionProperties] = {
    "10in_flexible_riser": LineSectionProperties(
        outer_diameter=0.350,
        inner_diameter=0.254,
        mass_per_unit_length=180.0,
        bending_stiffness=50000.0,
        axial_stiffness=400e6,
        cd_normal=1.2,
    ),
    "8in_rigid_riser": LineSectionProperties(
        outer_diameter=0.2732,
        inner_diameter=0.2032,
        mass_per_unit_length=120.0,
        bending_stiffness=40.0e6,
        axial_stiffness=3.0e9,
        cd_normal=1.05,
    ),
    "R4_84mm_chain": LineSectionProperties(
        outer_diameter=0.168,
        inner_diameter=0.0,
        mass_per_unit_length=139.0,
        bending_stiffness=0.0,
        axial_stiffness=757e6,
        cd_normal=2.4,
    ),
    "96mm_wire_rope": LineSectionProperties(
        outer_diameter=0.096,
        inner_diameter=0.0,
        mass_per_unit_length=36.5,
        bending_stiffness=1000.0,
        axial_stiffness=750e6,
        cd_normal=1.2,
    ),
    "160mm_polyester": LineSectionProperties(
        outer_diameter=0.160,
        inner_diameter=0.0,
        mass_per_unit_length=18.5,
        bending_stiffness=0.0,
        axial_stiffness=200e6,
        cd_normal=1.2,
    ),
}


def build_scr_model(
    water_depth: float = 1500.0,
    riser_od: float = 0.273,
    riser_id: float = 0.203,
    hang_off_angle: float = 10.0,
    vessel_name: str = "FPSO",
) -> Dict[str, Any]:
    """Build a Steel Catenary Riser (SCR) model configuration.

    Args:
        water_depth: Water depth (m).
        riser_od: Riser outer diameter (m).
        riser_id: Riser inner diameter (m).
        hang_off_angle: Top angle from vertical (deg).
        vessel_name: Name of the host vessel.

    Returns:
        Dict with vessel, line type, and line configuration.

    Reference: API RP 2RD, Section 5.
    """
    # Estimate riser length
    riser_length = water_depth / math.cos(math.radians(hang_off_angle)) * 1.15

    vessel = VesselConfig(name=vessel_name, draft=12.0)
    line = LineConfig(
        name="SCR1",
        sections=[
            LineSection(length=riser_length, line_type_name="SCR_Steel", target_segment_length=5.0),
        ],
        end_a_connection=LineEndCondition.CONNECTED,
        end_a_object=vessel_name,
        end_a_position=(0.0, -20.0, -10.0),
        end_b_connection=LineEndCondition.ANCHORED,
        end_b_position=(water_depth / math.tan(math.radians(90.0 - hang_off_angle)) * 0.8, 0.0, 0.0),
    )

    return {
        "model_type": "SCR",
        "water_depth": water_depth,
        "objects": {
            "vessel": vessel.to_orcaflex_dict(),
            "line": line.to_orcaflex_dict(),
        },
        "line_types": {
            "SCR_Steel": LineSectionProperties(
                outer_diameter=riser_od,
                inner_diameter=riser_id,
                mass_per_unit_length=120.0,
                bending_stiffness=40.0e6,
                axial_stiffness=3.0e9,
            ).model_dump(),
        },
    }


def build_lazy_wave_model(
    water_depth: float = 1500.0,
    buoyancy_section_length: float = 200.0,
    vessel_name: str = "FPSO",
) -> Dict[str, Any]:
    """Build a Lazy-Wave Riser model configuration.

    Args:
        water_depth: Water depth (m).
        buoyancy_section_length: Length of buoyancy section (m).
        vessel_name: Name of the host vessel.

    Returns:
        Dict with vessel, line type, and line configuration.

    Reference: API RP 2RD, Section 5.3.
    """
    upper_section = water_depth * 0.4
    lower_section = water_depth * 0.5

    vessel = VesselConfig(name=vessel_name)
    line = LineConfig(
        name="LazyWave1",
        sections=[
            LineSection(length=upper_section, line_type_name="FlexRiser_Bare", target_segment_length=5.0),
            LineSection(length=buoyancy_section_length, line_type_name="FlexRiser_Buoyancy",
                        target_segment_length=3.0),
            LineSection(length=lower_section, line_type_name="FlexRiser_Bare", target_segment_length=5.0),
        ],
        end_a_connection=LineEndCondition.CONNECTED,
        end_a_object=vessel_name,
        end_b_connection=LineEndCondition.ANCHORED,
        end_b_position=(400.0, 0.0, 0.0),
    )

    return {
        "model_type": "LazyWave",
        "water_depth": water_depth,
        "objects": {
            "vessel": vessel.to_orcaflex_dict(),
            "line": line.to_orcaflex_dict(),
        },
        "line_types": {
            "FlexRiser_Bare": MATERIAL_LIBRARY["10in_flexible_riser"].model_dump(),
            "FlexRiser_Buoyancy": LineSectionProperties(
                outer_diameter=0.650,
                inner_diameter=0.254,
                mass_per_unit_length=80.0,
                bending_stiffness=50000.0,
                axial_stiffness=400e6,
            ).model_dump(),
        },
    }


def build_mooring_line(
    water_depth: float = 1000.0,
    line_length: float = 2500.0,
    vessel_name: str = "FPSO",
    azimuth: float = 0.0,
) -> Dict[str, Any]:
    """Build a single mooring line configuration.

    Three-segment: chain (top) – wire/polyester (mid) – chain (bottom).

    Args:
        water_depth: Water depth (m).
        line_length: Total line length (m).
        vessel_name: Host vessel name.
        azimuth: Line azimuth from vessel (deg).

    Returns:
        Dict with line and line type configuration.

    Reference: API RP 2SK, Section 5.
    """
    chain_top = line_length * 0.15
    mid_section = line_length * 0.60
    chain_bottom = line_length * 0.25

    anchor_radius = math.sqrt(line_length**2 - water_depth**2) if line_length > water_depth else line_length * 0.8
    ax = anchor_radius * math.cos(math.radians(azimuth))
    ay = anchor_radius * math.sin(math.radians(azimuth))

    line = LineConfig(
        name=f"Mooring_{azimuth:.0f}",
        sections=[
            LineSection(length=chain_top, line_type_name="R4_84mm_Chain", target_segment_length=10.0),
            LineSection(length=mid_section, line_type_name="160mm_Polyester", target_segment_length=15.0),
            LineSection(length=chain_bottom, line_type_name="R4_84mm_Chain", target_segment_length=10.0),
        ],
        end_a_connection=LineEndCondition.CONNECTED,
        end_a_object=vessel_name,
        end_a_position=(0.0, 0.0, -10.0),
        end_b_connection=LineEndCondition.ANCHORED,
        end_b_position=(ax, ay, 0.0),
    )

    return {
        "model_type": "MooringLine",
        "water_depth": water_depth,
        "azimuth_deg": azimuth,
        "objects": {"line": line.to_orcaflex_dict()},
        "line_types": {
            "R4_84mm_Chain": MATERIAL_LIBRARY["R4_84mm_chain"].model_dump(),
            "160mm_Polyester": MATERIAL_LIBRARY["160mm_polyester"].model_dump(),
        },
    }
