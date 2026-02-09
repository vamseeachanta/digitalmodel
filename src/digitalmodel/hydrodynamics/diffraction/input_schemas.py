"""Canonical input schema for diffraction analysis (WRK-057).

Solver-agnostic specification that can generate inputs for AQWA, OrcaWave,
and other BEM solvers. This is the 'spec.yml' format.

Uses Pydantic v2 models to capture the superset of fields needed by both
AQWA and OrcaWave backends. Each sub-model is independently validatable
and composable into the top-level DiffractionSpec.

Example usage:
    from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec

    spec = DiffractionSpec.from_yaml("analysis.yml")
    freqs = spec.frequencies.to_frequencies_rad_s()
    headings = spec.wave_headings.to_heading_list()
    bodies = spec.get_bodies()
"""

from __future__ import annotations

from enum import Enum
from pathlib import Path
from typing import Any, Optional

import numpy as np
from pydantic import BaseModel, Field, field_validator, model_validator


# ---------------------------------------------------------------------------
# Enumerations
# ---------------------------------------------------------------------------


class AnalysisType(str, Enum):
    """Type of hydrodynamic analysis."""

    DIFFRACTION = "diffraction"
    RADIATION = "radiation"
    FULL_QTF = "full_qtf"
    FREQUENCY_DOMAIN = "frequency_domain"
    TIME_DOMAIN = "time_domain"


class MeshFormatType(str, Enum):
    """Supported mesh file formats."""

    GDF = "gdf"
    DAT = "dat"
    STL = "stl"
    MSH = "msh"
    OBJ = "obj"
    AUTO = "auto"  # detect from extension


class SymmetryType(str, Enum):
    """Mesh symmetry options."""

    NONE = "none"
    XZ = "xz"
    YZ = "yz"
    XZ_YZ = "xz+yz"


class FrequencyInputType(str, Enum):
    """How frequencies are specified."""

    PERIOD = "period"
    FREQUENCY = "frequency"


class FrequencyDistribution(str, Enum):
    """Frequency spacing distribution."""

    LINEAR = "linear"
    LOGARITHMIC = "logarithmic"


class LoadRAOMethod(str, Enum):
    """Load RAO calculation method."""

    HASKIND = "haskind"
    DIRECT = "direct"
    BOTH = "both"


class SolverPrecision(str, Enum):
    """Solver numerical precision."""

    SINGLE = "single"
    DOUBLE = "double"


class OutputFormat(str, Enum):
    """Output file formats."""

    CSV = "csv"
    XLSX = "xlsx"
    HDF5 = "hdf5"
    ORCAFLEX_YML = "orcaflex_yml"
    ORCAFLEX_JSON = "orcaflex_json"


class OutputComponent(str, Enum):
    """Result components to output."""

    RAOS = "raos"
    ADDED_MASS = "added_mass"
    DAMPING = "damping"
    EXCITATION = "excitation"
    MEAN_DRIFT = "mean_drift"
    QTF = "qtf"
    PRESSURE = "pressure"


# ---------------------------------------------------------------------------
# Sub-models
# ---------------------------------------------------------------------------


class VesselGeometry(BaseModel):
    """Vessel mesh geometry specification."""

    mesh_file: str = Field(
        ...,
        description="Path to mesh file (relative to spec.yml location)",
    )
    mesh_format: MeshFormatType = Field(
        MeshFormatType.AUTO,
        description=(
            "Mesh file format, auto-detected from extension if not specified"
        ),
    )
    symmetry: SymmetryType = Field(
        SymmetryType.NONE,
        description="Mesh symmetry plane(s)",
    )
    reference_point: list[float] = Field(
        default=[0.0, 0.0, 0.0],
        description="Reference point [x, y, z] in meters",
    )
    waterline_z: float = Field(
        default=0.0,
        description="Z-coordinate of waterline in meters",
    )
    length_units: str = Field(
        default="m",
        description="Length units of the mesh file",
    )

    @field_validator("reference_point")
    @classmethod
    def validate_reference_point(cls, v: list[float]) -> list[float]:
        if len(v) != 3:
            raise ValueError(
                "reference_point must have exactly 3 values [x, y, z]"
            )
        return v


class VesselInertia(BaseModel):
    """Vessel mass and inertia properties."""

    mass: float = Field(..., gt=0, description="Vessel mass in kg")
    centre_of_gravity: list[float] = Field(
        ...,
        description="Centre of gravity [x, y, z] in meters",
    )
    radii_of_gyration: Optional[list[float]] = Field(
        None,
        description="Radii of gyration [kxx, kyy, kzz] in meters",
    )
    inertia_tensor: Optional[dict[str, float]] = Field(
        None,
        description=(
            "Full inertia tensor components: "
            "Ixx, Iyy, Izz, Ixy, Ixz, Iyz (kg.m^2)"
        ),
    )

    @field_validator("centre_of_gravity")
    @classmethod
    def validate_cog(cls, v: list[float]) -> list[float]:
        if len(v) != 3:
            raise ValueError(
                "centre_of_gravity must have exactly 3 values [x, y, z]"
            )
        return v

    @field_validator("radii_of_gyration")
    @classmethod
    def validate_rog(
        cls, v: Optional[list[float]]
    ) -> Optional[list[float]]:
        if v is not None and len(v) != 3:
            raise ValueError(
                "radii_of_gyration must have exactly 3 values [kxx, kyy, kzz]"
            )
        return v

    @model_validator(mode="after")
    def check_inertia_specified(self) -> VesselInertia:
        if self.radii_of_gyration is None and self.inertia_tensor is None:
            raise ValueError(
                "Either radii_of_gyration or inertia_tensor must be specified"
            )
        return self


class VesselSpec(BaseModel):
    """Complete vessel specification."""

    name: str = Field(..., description="Vessel name identifier")
    type: Optional[str] = Field(
        None,
        description=(
            "Vessel type (e.g., FPSO, semi-sub, ship, spar, barge)"
        ),
    )
    geometry: VesselGeometry
    inertia: VesselInertia
    external_stiffness: Optional[list[list[float]]] = Field(
        None,
        description="6x6 external stiffness matrix",
    )
    external_damping: Optional[list[list[float]]] = Field(
        None,
        description="6x6 external damping matrix",
    )
    fixed_dofs: Optional[list[str]] = Field(
        None,
        description=(
            "Fixed degrees of freedom: surge, sway, heave, roll, pitch, yaw"
        ),
    )


class EnvironmentSpec(BaseModel):
    """Environmental conditions for the analysis."""

    water_depth: float | str = Field(
        ...,
        description="Water depth in meters, or 'infinite' for deep water",
    )
    water_density: float = Field(
        default=1025.0,
        gt=0,
        description="Water density in kg/m^3",
    )
    gravity: float = Field(
        default=9.80665,
        gt=0,
        description="Gravitational acceleration in m/s^2",
    )

    @field_validator("water_depth", mode="before")
    @classmethod
    def validate_water_depth(cls, v: Any) -> float | str:
        if isinstance(v, str):
            if v.lower() in ("infinite", "inf", "deep"):
                return "infinite"
            raise ValueError(
                f"Invalid water_depth string: {v}. "
                "Use 'infinite' for deep water."
            )
        if isinstance(v, (int, float)):
            if v <= 0:
                raise ValueError(
                    "water_depth must be positive or 'infinite'"
                )
            return float(v)
        raise ValueError(
            f"water_depth must be a number or 'infinite', got {type(v)}"
        )


class FrequencyRangeSpec(BaseModel):
    """Frequency range specification (alternative to explicit values)."""

    start: float = Field(..., gt=0)
    end: float = Field(..., gt=0)
    count: int = Field(..., gt=0)
    distribution: FrequencyDistribution = Field(
        FrequencyDistribution.LINEAR,
    )


class FrequencySpec(BaseModel):
    """Frequency/period specification for the analysis."""

    input_type: FrequencyInputType = Field(
        FrequencyInputType.FREQUENCY,
        description="Whether values are periods (s) or frequencies (rad/s)",
    )
    values: Optional[list[float]] = Field(
        None,
        description="Explicit list of frequency/period values",
    )
    range: Optional[FrequencyRangeSpec] = Field(
        None,
        description="Range specification (alternative to explicit values)",
    )

    @model_validator(mode="after")
    def check_values_or_range(self) -> FrequencySpec:
        if self.values is None and self.range is None:
            raise ValueError(
                "Either 'values' or 'range' must be specified for frequencies"
            )
        if self.values is not None and self.range is not None:
            raise ValueError(
                "Specify either 'values' or 'range', not both"
            )
        return self

    def to_frequencies_rad_s(self) -> list[float]:
        """Convert to frequencies in rad/s regardless of input type."""
        if self.values is not None:
            raw = self.values
        else:
            r = self.range
            if r.distribution == FrequencyDistribution.LINEAR:
                raw = list(np.linspace(r.start, r.end, r.count))
            else:
                raw = list(
                    np.logspace(np.log10(r.start), np.log10(r.end), r.count)
                )

        if self.input_type == FrequencyInputType.PERIOD:
            return [2.0 * np.pi / t for t in raw if t > 0]
        return raw

    def to_periods_s(self) -> list[float]:
        """Convert to periods in seconds regardless of input type."""
        freqs = self.to_frequencies_rad_s()
        return [2.0 * np.pi / w for w in freqs if w > 0]


class HeadingRangeSpec(BaseModel):
    """Heading range specification."""

    start: float = Field(default=0.0)
    end: float = Field(default=180.0)
    increment: float = Field(default=15.0, gt=0)


class WaveHeadingSpec(BaseModel):
    """Wave heading specification."""

    values: Optional[list[float]] = Field(
        None,
        description="Explicit list of headings in degrees",
    )
    range: Optional[HeadingRangeSpec] = Field(
        None,
        description="Range specification",
    )
    symmetry: bool = Field(
        default=False,
        description=(
            "Exploit heading symmetry (0-180 mirrored to 180-360)"
        ),
    )

    @model_validator(mode="after")
    def check_values_or_range(self) -> WaveHeadingSpec:
        if self.values is None and self.range is None:
            raise ValueError(
                "Either 'values' or 'range' must be specified "
                "for wave headings"
            )
        if self.values is not None and self.range is not None:
            raise ValueError(
                "Specify either 'values' or 'range', not both"
            )
        return self

    def to_heading_list(self) -> list[float]:
        """Get headings as a flat list in degrees."""
        if self.values is not None:
            return self.values
        r = self.range
        headings: list[float] = []
        h = r.start
        while h <= r.end + 1e-9:
            headings.append(round(h, 6))
            h += r.increment
        return headings


class SolverOptions(BaseModel):
    """Solver-specific options."""

    remove_irregular_frequencies: bool = Field(
        default=True,
        description="Remove irregular frequency effects",
    )
    qtf_calculation: bool = Field(
        default=False,
        description="Calculate Quadratic Transfer Functions",
    )
    load_rao_method: LoadRAOMethod = Field(
        default=LoadRAOMethod.BOTH,
        description="Load RAO calculation method",
    )
    precision: SolverPrecision = Field(
        default=SolverPrecision.DOUBLE,
        description="Numerical precision",
    )
    output_ah1: bool = False
    """Generate .AH1 ASCII hydrodynamic database file (OPTIONS AHD1).
    Alternative to binary .HYD — produces readable text output with
    added mass, damping, and force RAO data."""
    qtf_min_frequency: Optional[float] = Field(
        None,
        description="Minimum QTF frequency (rad/s)",
    )
    qtf_max_frequency: Optional[float] = Field(
        None,
        description="Maximum QTF frequency (rad/s)",
    )


class OutputSpec(BaseModel):
    """Output configuration."""

    formats: list[OutputFormat] = Field(
        default=[OutputFormat.CSV],
        description="Output file formats",
    )
    components: list[OutputComponent] = Field(
        default=[
            OutputComponent.RAOS,
            OutputComponent.ADDED_MASS,
            OutputComponent.DAMPING,
        ],
        description="Result components to include",
    )
    directory: Optional[str] = Field(
        None,
        description="Output directory (defaults to ./results/)",
    )


class MetadataSpec(BaseModel):
    """Project metadata."""

    project: Optional[str] = Field(
        None,
        description="Project name or code",
    )
    author: Optional[str] = Field(
        None,
        description="Author or organization",
    )
    date: Optional[str] = Field(
        None,
        description="Analysis date (ISO 8601)",
    )
    description: Optional[str] = Field(
        None,
        description="Analysis description",
    )
    tags: list[str] = Field(
        default=[],
        description="Tags for categorization",
    )


class ControlSurfaceSpec(BaseModel):
    """Control surface for irregular frequency removal."""

    type: str = Field(
        default="auto",
        description=(
            "'auto' for automatic generation, or 'mesh' for explicit "
            "mesh file"
        ),
    )
    mesh_file: Optional[str] = Field(
        None,
        description=(
            "Path to control surface mesh file (when type='mesh')"
        ),
    )
    panel_size: Optional[float] = Field(
        None,
        description="Panel size for auto-generated control surface",
    )
    separation: Optional[float] = Field(
        None,
        description="Separation distance from body surface",
    )


class MorisonElementSpec(BaseModel):
    """Morison element specification (for viscous drag on slender members)."""

    name: str
    position: list[float] = Field(description="Position [x, y, z]")
    attitude: list[float] = Field(
        default=[0, 0, 0],
        description="Attitude [rx, ry, rz] degrees",
    )
    length: float = Field(gt=0)
    diameter: float = Field(gt=0)
    cd: float = Field(default=1.0, description="Drag coefficient")
    cm: float = Field(default=2.0, description="Inertia coefficient")


class BodySpec(BaseModel):
    """Single body in the analysis (extends VesselSpec for multi-body)."""

    vessel: VesselSpec
    position: list[float] = Field(
        default=[0.0, 0.0, 0.0],
        description="Body position [x, y, z]",
    )
    attitude: list[float] = Field(
        default=[0.0, 0.0, 0.0],
        description="Body attitude [rx, ry, rz] degrees",
    )
    control_surface: Optional[ControlSurfaceSpec] = Field(
        None,
        description="Control surface spec",
    )
    morison_elements: list[MorisonElementSpec] = Field(
        default=[],
        description="Morison drag elements",
    )
    connection_parent: Optional[str] = Field(
        None,
        description=(
            "Parent body name for constraints, or None for free"
        ),
    )


# ---------------------------------------------------------------------------
# Top-level spec
# ---------------------------------------------------------------------------


class DiffractionSpec(BaseModel):
    """Top-level canonical specification for diffraction analysis.

    This is the schema for spec.yml files. It captures all information
    needed to generate solver-specific inputs for AQWA, OrcaWave, or
    other BEM solvers.

    Example::

        spec = DiffractionSpec.from_yaml("analysis.yml")
        bodies = spec.get_bodies()
        freqs = spec.frequencies.to_frequencies_rad_s()
    """

    version: str = Field(
        default="1.0",
        description="Schema version",
    )
    analysis_type: AnalysisType = Field(
        default=AnalysisType.DIFFRACTION,
        description="Type of analysis",
    )

    # Single body (simple case)
    vessel: Optional[VesselSpec] = Field(
        None,
        description="Single vessel (for single-body analysis)",
    )
    # Multi-body
    bodies: Optional[list[BodySpec]] = Field(
        None,
        description="Multiple bodies (for multi-body analysis)",
    )

    environment: EnvironmentSpec
    frequencies: FrequencySpec
    wave_headings: WaveHeadingSpec
    solver_options: SolverOptions = Field(default_factory=SolverOptions)
    outputs: OutputSpec = Field(default_factory=OutputSpec)
    metadata: MetadataSpec = Field(default_factory=MetadataSpec)

    @model_validator(mode="after")
    def check_vessel_or_bodies(self) -> DiffractionSpec:
        if self.vessel is None and self.bodies is None:
            raise ValueError(
                "Either 'vessel' (single body) or 'bodies' (multi-body) "
                "must be specified"
            )
        if self.vessel is not None and self.bodies is not None:
            raise ValueError(
                "Specify either 'vessel' or 'bodies', not both"
            )
        return self

    def get_bodies(self) -> list[BodySpec]:
        """Get all bodies as a list, wrapping single vessel if needed."""
        if self.bodies is not None:
            return self.bodies
        return [BodySpec(vessel=self.vessel)]

    @classmethod
    def from_yaml(cls, path: str | Path) -> DiffractionSpec:
        """Load spec from a YAML file."""
        import yaml

        with open(path) as f:
            data = yaml.safe_load(f)
        return cls.model_validate(data)

    def to_yaml(self, path: str | Path) -> Path:
        """Save spec to a YAML file."""
        import yaml

        path = Path(path)
        data = self.model_dump(mode="json", exclude_none=True)
        with open(path, "w") as f:
            yaml.dump(data, f, default_flow_style=False, sort_keys=False)
        return path

    def to_json_schema(self) -> dict:
        """Export JSON Schema for external validation."""
        return self.model_json_schema()


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

__all__ = [
    # Enumerations
    "AnalysisType",
    "MeshFormatType",
    "SymmetryType",
    "FrequencyInputType",
    "FrequencyDistribution",
    "LoadRAOMethod",
    "SolverPrecision",
    "OutputFormat",
    "OutputComponent",
    # Sub-models
    "VesselGeometry",
    "VesselInertia",
    "VesselSpec",
    "EnvironmentSpec",
    "FrequencyRangeSpec",
    "FrequencySpec",
    "HeadingRangeSpec",
    "WaveHeadingSpec",
    "SolverOptions",
    "OutputSpec",
    "MetadataSpec",
    "ControlSurfaceSpec",
    "MorisonElementSpec",
    "BodySpec",
    # Top-level spec
    "DiffractionSpec",
]
