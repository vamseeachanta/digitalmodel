"""
Pydantic schema models for OrcaFlex Modular Model Generator.

This package defines comprehensive Pydantic models for validating project-specific
YAML input files that describe OrcaFlex models.

All models are re-exported here for backward compatibility:
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
"""

from ._enums import RampType, RollerType, StructureType, WaveType
from .environment import (
    Current,
    CurrentProfile,
    Environment,
    Seabed,
    SeabedStiffness,
    Water,
    Waves,
    Wind,
)
from .equipment import (
    BuoyancyModuleProperties,
    BuoyancyModules,
    Equipment,
    Ramp,
    RollerArrangement,
    Rollers,
    RollerStation,
    Stinger,
    StingerRoller,
    StingerSection,
    Tensioner,
    TugProperties,
    Tugs,
    Vessel,
    VesselMooring,
    VesselProperties,
)
from .metadata import Metadata
from .pipeline import Coating, Coatings, Dimensions, Pipeline, Segment
from .riser import (
    BuoyancyZone,
    ClumpAttachments,
    ClumpType,
    ConnectionType,
    EndConnection,
    LinkConnection,
    LinkType,
    Riser,
    RiserConfiguration,
    RiserContents,
    RiserLine,
    RiserLineType,
    RiserLink,
    RiserSection,
    RiserVessel,
)
from .root import ProjectInputSpec
from .simulation import Simulation

__all__ = [
    # Root
    "ProjectInputSpec",
    # Enums
    "StructureType",
    "WaveType",
    "RampType",
    "RollerType",
    # Metadata
    "Metadata",
    # Environment
    "Water",
    "SeabedStiffness",
    "Seabed",
    "Waves",
    "CurrentProfile",
    "Current",
    "Wind",
    "Environment",
    # Pipeline
    "Dimensions",
    "Coating",
    "Coatings",
    "Segment",
    "Pipeline",
    # Riser
    "RiserConfiguration",
    "ConnectionType",
    "LinkType",
    "LinkConnection",
    "RiserLink",
    "RiserLineType",
    "RiserSection",
    "EndConnection",
    "RiserContents",
    "BuoyancyZone",
    "ClumpType",
    "ClumpAttachments",
    "RiserVessel",
    "RiserLine",
    "Riser",
    # Equipment
    "TugProperties",
    "Tugs",
    "Rollers",
    "RollerStation",
    "RollerArrangement",
    "BuoyancyModuleProperties",
    "BuoyancyModules",
    "Ramp",
    "Equipment",
    # Equipment (S-lay)
    "VesselProperties",
    "VesselMooring",
    "Vessel",
    "StingerSection",
    "StingerRoller",
    "Stinger",
    "Tensioner",
    # Simulation
    "Simulation",
]
