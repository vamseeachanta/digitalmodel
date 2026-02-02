"""
Pydantic schema models for OrcaFlex Modular Model Generator.

This package defines comprehensive Pydantic models for validating project-specific
YAML input files that describe OrcaFlex models.

All models are re-exported here for backward compatibility:
    from digitalmodel.orcaflex.modular_generator.schema import ProjectInputSpec
"""

from ._enums import RampType, StructureType, WaveType
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
    Rollers,
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
from .root import ProjectInputSpec
from .simulation import Simulation

__all__ = [
    # Root
    "ProjectInputSpec",
    # Enums
    "StructureType",
    "WaveType",
    "RampType",
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
    # Equipment
    "TugProperties",
    "Tugs",
    "Rollers",
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
