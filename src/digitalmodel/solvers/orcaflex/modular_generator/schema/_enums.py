"""Enumerations for OrcaFlex Modular Model Generator schema."""

from enum import Enum


class StructureType(str, Enum):
    """Supported structure types for OrcaFlex models."""

    PIPELINE = "pipeline"
    RISER = "riser"
    MOORING = "mooring"
    VESSEL = "vessel"
    UMBILICAL = "umbilical"
    SUBSEA = "subsea"


class WaveType(str, Enum):
    """Supported wave types in OrcaFlex."""

    DEAN_STREAM = "dean_stream"
    AIRY = "airy"
    STOKES_5TH = "stokes_5th"
    CNOIDAL = "cnoidal"
    JONSWAP = "jonswap"
    PIERSON_MOSKOWITZ = "pierson_moskowitz"
    USER_DEFINED = "user_defined"


class RampType(str, Enum):
    """Types of ramp/shape elements."""

    BLOCK = "block"
    CURVED_PLATE = "curved_plate"
    CYLINDER = "cylinder"
    POLYGON = "polygon"


class RollerType(str, Enum):
    """Roller types for pipeline support during installation."""

    V_ROLLER = "v_roller"
    FLAT = "flat"
    CRADLE = "cradle"
