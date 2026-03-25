"""
BEM Solver Metadata Model

Data model for storing information about the BEM solver and analysis parameters.
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any


class BEMSolverType(Enum):
    """Supported BEM solver types."""

    AQWA = "AQWA"
    WAMIT = "WAMIT"
    NEMOH = "NEMOH"
    ORCAWAVE = "OrcaWave"
    HAMS = "HAMS"
    CAPYTAINE = "Capytaine"
    UNKNOWN = "Unknown"


@dataclass
class BEMSolverMetadata:
    """Metadata about the BEM solver and analysis configuration.

    Attributes:
        solver_type: Type of BEM solver used.
        solver_version: Version string of the solver.
        analysis_date: Date/time of the analysis.
        project_name: Name of the project or analysis.
        description: Optional description of the analysis.
        water_depth: Water depth in meters (positive, inf for deep water).
        water_density: Water density in kg/m^3.
        gravity: Gravitational acceleration in m/s^2.
        frequency_count: Number of wave frequencies analyzed.
        frequency_range: Tuple of (min, max) frequencies in rad/s.
        heading_count: Number of wave headings analyzed.
        heading_range: Tuple of (min, max) headings in degrees.
        body_count: Number of bodies in the analysis.
        body_names: Names of the bodies.
        reference_point: Reference point coordinates [x, y, z].
        units: Dictionary of unit specifications.
        custom_data: Additional solver-specific data.
    """

    solver_type: BEMSolverType = BEMSolverType.UNKNOWN
    solver_version: str = ""
    analysis_date: datetime | None = None
    project_name: str = ""
    description: str = ""

    # Physical parameters
    water_depth: float = float("inf")
    water_density: float = 1025.0
    gravity: float = 9.80665

    # Analysis parameters
    frequency_count: int = 0
    frequency_range: tuple[float, float] = (0.0, 0.0)
    heading_count: int = 0
    heading_range: tuple[float, float] = (0.0, 360.0)

    # Body information
    body_count: int = 1
    body_names: list[str] = field(default_factory=list)
    reference_point: list[float] = field(default_factory=lambda: [0.0, 0.0, 0.0])

    # Units specification
    units: dict[str, str] = field(default_factory=dict)

    # Additional data
    custom_data: dict[str, Any] = field(default_factory=dict)

    def __post_init__(self):
        """Validate and normalize data after initialization."""
        # Set default units if not provided
        if not self.units:
            self.units = {
                "length": "m",
                "mass": "kg",
                "time": "s",
                "force": "N",
                "moment": "N.m",
                "angle": "deg",
                "frequency": "rad/s",
            }

    @property
    def is_deep_water(self) -> bool:
        """Check if analysis uses deep water approximation."""
        return self.water_depth == float("inf") or self.water_depth > 500

    @property
    def has_qtf(self) -> bool:
        """Check if QTF data is likely available based on solver type."""
        return self.solver_type in {
            BEMSolverType.AQWA,
            BEMSolverType.WAMIT,
            BEMSolverType.ORCAWAVE,
        }

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary representation.

        Returns:
            Dictionary with all metadata fields.
        """
        return {
            "solver_type": self.solver_type.value,
            "solver_version": self.solver_version,
            "analysis_date": self.analysis_date.isoformat() if self.analysis_date else None,
            "project_name": self.project_name,
            "description": self.description,
            "water_depth": self.water_depth if not self.is_deep_water else "infinite",
            "water_density": self.water_density,
            "gravity": self.gravity,
            "frequency_count": self.frequency_count,
            "frequency_range": list(self.frequency_range),
            "heading_count": self.heading_count,
            "heading_range": list(self.heading_range),
            "body_count": self.body_count,
            "body_names": self.body_names,
            "reference_point": self.reference_point,
            "units": self.units,
            "custom_data": self.custom_data,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "BEMSolverMetadata":
        """Create instance from dictionary.

        Args:
            data: Dictionary with metadata fields.

        Returns:
            BEMSolverMetadata instance.
        """
        # Handle solver type conversion
        solver_type = data.get("solver_type", "Unknown")
        if isinstance(solver_type, str):
            try:
                solver_type = BEMSolverType(solver_type)
            except ValueError:
                solver_type = BEMSolverType.UNKNOWN

        # Handle date conversion
        analysis_date = data.get("analysis_date")
        if isinstance(analysis_date, str):
            analysis_date = datetime.fromisoformat(analysis_date)

        # Handle water depth
        water_depth = data.get("water_depth", float("inf"))
        if water_depth == "infinite":
            water_depth = float("inf")

        return cls(
            solver_type=solver_type,
            solver_version=data.get("solver_version", ""),
            analysis_date=analysis_date,
            project_name=data.get("project_name", ""),
            description=data.get("description", ""),
            water_depth=water_depth,
            water_density=data.get("water_density", 1025.0),
            gravity=data.get("gravity", 9.80665),
            frequency_count=data.get("frequency_count", 0),
            frequency_range=tuple(data.get("frequency_range", (0.0, 0.0))),
            heading_count=data.get("heading_count", 0),
            heading_range=tuple(data.get("heading_range", (0.0, 360.0))),
            body_count=data.get("body_count", 1),
            body_names=data.get("body_names", []),
            reference_point=data.get("reference_point", [0.0, 0.0, 0.0]),
            units=data.get("units", {}),
            custom_data=data.get("custom_data", {}),
        )
