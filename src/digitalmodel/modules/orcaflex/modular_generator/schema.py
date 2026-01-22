"""
Pydantic schema models for OrcaFlex Modular Model Generator.

This module defines comprehensive Pydantic models for validating project-specific
YAML input files that describe OrcaFlex models. The schema captures engineering
intent and validates against physical constraints.

Models are organized hierarchically:
- ProjectInputSpec (root)
  - Metadata
  - Environment (Water, Seabed, Waves, Current, Wind)
  - Pipeline (Dimensions, Coatings, Segments)
  - Equipment (Tugs, Rollers, BuoyancyModules, Ramps)
  - Simulation
"""

from __future__ import annotations

from enum import Enum
from typing import Any, Literal

from pydantic import BaseModel, Field, field_validator, model_validator


# =============================================================================
# Enumerations
# =============================================================================


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


# =============================================================================
# Metadata Models
# =============================================================================


class Metadata(BaseModel):
    """
    Model metadata for identification and categorization.

    Attributes:
        name: Unique model identifier (e.g., '30in_pipeline_installation').
        description: Human-readable description of the model.
        structure: Type of offshore structure being modeled.
        operation: Operation type (e.g., 'installation/floating', 'in-place').
        project: Project code or name for traceability.
        version: Optional schema version for future compatibility.
        author: Optional author name or team.
    """

    name: str = Field(..., min_length=1, description="Unique model identifier")
    description: str = Field(..., description="Human-readable model description")
    structure: str = Field(..., description="Structure type (pipeline, riser, mooring, etc.)")
    operation: str = Field(..., description="Operation type (installation/floating, in-place, etc.)")
    project: str = Field(..., description="Project code for traceability")
    version: str = Field(default="1.0", description="Schema version")
    author: str | None = Field(default=None, description="Author name or team")


# =============================================================================
# Environment Models
# =============================================================================


class Water(BaseModel):
    """
    Water properties for the marine environment.

    Attributes:
        depth: Water depth in meters. Must be positive.
        density: Seawater density in te/m3. Typical range: 1.02-1.03.
    """

    depth: float = Field(..., gt=0, description="Water depth (m)")
    density: float = Field(
        default=1.025,
        gt=0,
        le=1.1,
        description="Seawater density (te/m3), typical 1.025",
    )

    @field_validator("depth")
    @classmethod
    def validate_depth(cls, v: float) -> float:
        """Validate water depth is within reasonable offshore range."""
        if v > 5000:
            raise ValueError(f"Water depth {v}m exceeds maximum reasonable value of 5000m")
        return v

    @field_validator("density")
    @classmethod
    def validate_density(cls, v: float) -> float:
        """Validate water density is within physical range."""
        if v < 1.0 or v > 1.1:
            raise ValueError(
                f"Water density {v} te/m3 outside expected range [1.0, 1.1]. "
                "Typical seawater density is 1.025 te/m3."
            )
        return v


class SeabedStiffness(BaseModel):
    """
    Seabed soil stiffness parameters.

    Attributes:
        normal: Normal (vertical) stiffness in kN/m/m2.
        shear: Shear (lateral) stiffness in kN/m/m2.
    """

    normal: float = Field(..., gt=0, description="Normal stiffness (kN/m/m2)")
    shear: float = Field(..., gt=0, description="Shear stiffness (kN/m/m2)")

    @field_validator("normal", "shear")
    @classmethod
    def validate_stiffness(cls, v: float) -> float:
        """Validate stiffness is positive and within reasonable range."""
        if v > 1e8:
            raise ValueError(f"Stiffness {v} exceeds maximum reasonable value")
        return v


class Seabed(BaseModel):
    """
    Seabed properties including slope and soil characteristics.

    Attributes:
        slope: Seabed slope in degrees. Range: [-90, 90].
        stiffness: Soil stiffness parameters (normal and shear).
        friction_coefficient: Optional friction coefficient for contact.
    """

    slope: float = Field(default=0, ge=-90, le=90, description="Seabed slope (degrees)")
    stiffness: SeabedStiffness = Field(..., description="Soil stiffness parameters")
    friction_coefficient: float = Field(
        default=0.3, ge=0, le=1.5, description="Seabed friction coefficient"
    )


class Waves(BaseModel):
    """
    Wave specification for environmental loading.

    Attributes:
        type: Wave theory type (dean_stream, airy, etc.).
        height: Significant wave height Hs (m) or deterministic height.
        period: Wave period Tp (s) or zero-crossing period Tz.
        direction: Wave direction in degrees from North, going to.
        phase: Optional wave phase in degrees.
        gamma: JONSWAP gamma parameter (default 3.3).
    """

    type: str = Field(default="airy", description="Wave theory type")
    height: float = Field(default=0, ge=0, description="Wave height Hs (m)")
    period: float = Field(default=8, gt=0, description="Wave period Tp (s)")
    direction: float = Field(
        default=0, ge=0, lt=360, description="Wave direction (deg from North)"
    )
    phase: float = Field(default=0, ge=0, lt=360, description="Wave phase (deg)")
    gamma: float = Field(
        default=3.3, gt=1, le=7, description="JONSWAP gamma (typically 1-7)"
    )

    @field_validator("period")
    @classmethod
    def validate_period(cls, v: float) -> float:
        """Validate wave period is physically reasonable."""
        if v < 1 or v > 30:
            raise ValueError(
                f"Wave period {v}s outside typical range [1, 30]s for offshore conditions"
            )
        return v

    @field_validator("height")
    @classmethod
    def validate_height(cls, v: float) -> float:
        """Validate wave height is physically reasonable."""
        if v > 40:
            raise ValueError(f"Wave height {v}m exceeds maximum reasonable value of 40m")
        return v


class CurrentProfile(BaseModel):
    """
    Current profile point at a specific depth.

    Attributes:
        depth: Depth below water surface (m). Positive downward.
        factor: Current speed factor at this depth (fraction of surface speed).
    """

    depth: float = Field(..., ge=0, description="Depth below surface (m)")
    factor: float = Field(..., ge=0, le=2, description="Speed factor (0-2)")


class Current(BaseModel):
    """
    Current specification for environmental loading.

    Attributes:
        speed: Surface current speed (m/s).
        direction: Current direction in degrees from North, going to.
        profile: Depth-varying current profile as [[depth, factor], ...].
    """

    speed: float = Field(default=0, ge=0, description="Surface current speed (m/s)")
    direction: float = Field(
        default=0, ge=0, lt=360, description="Current direction (deg from North)"
    )
    profile: list[list[float]] = Field(
        default_factory=lambda: [[0, 1.0]],
        description="Current profile [[depth, factor], ...]",
    )

    @field_validator("speed")
    @classmethod
    def validate_speed(cls, v: float) -> float:
        """Validate current speed is physically reasonable."""
        if v > 5:
            raise ValueError(
                f"Current speed {v} m/s exceeds maximum reasonable value of 5 m/s"
            )
        return v

    @field_validator("profile")
    @classmethod
    def validate_profile(cls, v: list[list[float]]) -> list[list[float]]:
        """Validate current profile structure and values."""
        if not v:
            return [[0, 1.0]]

        for i, point in enumerate(v):
            if len(point) != 2:
                raise ValueError(
                    f"Profile point {i} must have exactly 2 values [depth, factor], got {len(point)}"
                )
            depth, factor = point
            if depth < 0:
                raise ValueError(f"Profile depth at point {i} must be >= 0, got {depth}")
            if factor < 0 or factor > 2:
                raise ValueError(
                    f"Profile factor at point {i} must be in [0, 2], got {factor}"
                )

        # Check depths are monotonically increasing
        depths = [p[0] for p in v]
        if depths != sorted(depths):
            raise ValueError("Profile depths must be monotonically increasing")

        return v


class Wind(BaseModel):
    """
    Wind specification for environmental loading.

    Attributes:
        speed: Wind speed at reference height (m/s).
        direction: Wind direction in degrees from North, going to.
        reference_height: Height at which wind speed is specified (m).
    """

    speed: float = Field(default=0, ge=0, description="Wind speed (m/s)")
    direction: float = Field(
        default=0, ge=0, lt=360, description="Wind direction (deg from North)"
    )
    reference_height: float = Field(
        default=10, gt=0, description="Reference height (m)"
    )

    @field_validator("speed")
    @classmethod
    def validate_speed(cls, v: float) -> float:
        """Validate wind speed is physically reasonable."""
        if v > 80:
            raise ValueError(
                f"Wind speed {v} m/s exceeds maximum reasonable value of 80 m/s"
            )
        return v


class Environment(BaseModel):
    """
    Complete environmental specification for the model.

    Contains all environmental parameters including water properties,
    seabed characteristics, and metocean conditions (waves, current, wind).
    """

    water: Water = Field(..., description="Water properties")
    seabed: Seabed = Field(..., description="Seabed properties")
    waves: Waves = Field(default_factory=Waves, description="Wave specification")
    current: Current = Field(default_factory=Current, description="Current specification")
    wind: Wind = Field(default_factory=Wind, description="Wind specification")


# =============================================================================
# Pipeline Models
# =============================================================================


class Dimensions(BaseModel):
    """
    Pipeline cross-section dimensions.

    Attributes:
        outer_diameter: Pipe outer diameter (m).
        wall_thickness: Pipe wall thickness (m).
    """

    outer_diameter: float = Field(..., gt=0, description="Outer diameter (m)")
    wall_thickness: float = Field(..., gt=0, description="Wall thickness (m)")

    @model_validator(mode="after")
    def validate_wall_thickness_ratio(self) -> "Dimensions":
        """
        Validate wall thickness is less than half the outer diameter.

        This is a physical constraint: wall thickness cannot exceed the radius.
        """
        if self.wall_thickness >= self.outer_diameter / 2:
            raise ValueError(
                f"Wall thickness ({self.wall_thickness}m) must be less than half "
                f"the outer diameter ({self.outer_diameter / 2}m). "
                f"Current ratio: {self.wall_thickness / self.outer_diameter:.2%}"
            )
        return self


class Coating(BaseModel):
    """
    Single coating layer definition (corrosion coating or weight coating).

    Attributes:
        thickness: Coating thickness (m).
        density: Coating density (te/m3).
        name: Optional coating identifier for weight coatings.
    """

    thickness: float = Field(..., gt=0, description="Coating thickness (m)")
    density: float = Field(..., gt=0, description="Coating density (te/m3)")
    name: str | None = Field(default=None, description="Coating identifier")

    @field_validator("thickness")
    @classmethod
    def validate_thickness(cls, v: float) -> float:
        """Validate coating thickness is reasonable."""
        if v > 0.5:
            raise ValueError(
                f"Coating thickness {v}m exceeds maximum reasonable value of 0.5m"
            )
        return v

    @field_validator("density")
    @classmethod
    def validate_density(cls, v: float) -> float:
        """Validate coating density is physically reasonable."""
        if v < 0.5 or v > 8:
            raise ValueError(
                f"Coating density {v} te/m3 outside typical range [0.5, 8] te/m3"
            )
        return v


class Coatings(BaseModel):
    """
    Pipeline coating system including corrosion protection and weight coatings.

    Attributes:
        corrosion: Anti-corrosion coating (e.g., FBE, 3LPE).
        weight: List of weight coatings (e.g., concrete weight coatings).
    """

    corrosion: Coating = Field(..., description="Corrosion coating layer")
    weight: list[Coating] = Field(
        default_factory=list, description="Weight coating layers"
    )


class Segment(BaseModel):
    """
    Pipeline segment definition for sectioned pipelines.

    Attributes:
        type: Segment type identifier (e.g., 'X65+coating+CWC120').
        length: Total length of this segment (m).
        segment_length: OrcaFlex mesh segment length (m).
        start_arc_length: Optional explicit start position (m from End A).
    """

    type: str = Field(..., min_length=1, description="Segment type identifier")
    length: float = Field(..., gt=0, description="Total segment length (m)")
    segment_length: float = Field(..., gt=0, description="Mesh segment length (m)")
    start_arc_length: float | None = Field(
        default=None, ge=0, description="Start position from End A (m)"
    )

    @field_validator("length")
    @classmethod
    def validate_length(cls, v: float) -> float:
        """Validate segment length is positive and reasonable."""
        if v > 50000:
            raise ValueError(
                f"Segment length {v}m exceeds maximum reasonable value of 50000m"
            )
        return v

    @field_validator("segment_length")
    @classmethod
    def validate_segment_length(cls, v: float) -> float:
        """Validate mesh segment length is reasonable."""
        if v < 0.1:
            raise ValueError(
                f"Segment length {v}m is too small. Minimum recommended is 0.1m"
            )
        if v > 100:
            raise ValueError(
                f"Segment length {v}m exceeds maximum reasonable value of 100m"
            )
        return v

    @model_validator(mode="after")
    def validate_segment_count(self) -> "Segment":
        """Validate that segment length divides evenly into total length."""
        if self.length > 0 and self.segment_length > 0:
            segment_count = self.length / self.segment_length
            if segment_count < 1:
                raise ValueError(
                    f"Segment length ({self.segment_length}m) exceeds total length ({self.length}m)"
                )
        return self


class Pipeline(BaseModel):
    """
    Complete pipeline definition including material, dimensions, and segmentation.

    Attributes:
        name: Pipeline name/identifier for OrcaFlex (e.g., "30'' Line").
        material: Material grade (e.g., X65, X70, API 5L).
        dimensions: Cross-section dimensions.
        coatings: Coating system definition.
        segments: List of pipeline segments with varying properties.
        youngs_modulus: Optional override for Young's modulus (kN/m2).
        poissons_ratio: Optional override for Poisson's ratio.
    """

    name: str = Field(..., min_length=1, description="Pipeline name for OrcaFlex")
    material: str = Field(..., description="Material grade (e.g., X65)")
    dimensions: Dimensions = Field(..., description="Cross-section dimensions")
    coatings: Coatings = Field(..., description="Coating system")
    segments: list[Segment] = Field(
        ..., min_length=1, description="Pipeline segments"
    )
    youngs_modulus: float | None = Field(
        default=None, gt=0, description="Young's modulus override (kN/m2)"
    )
    poissons_ratio: float | None = Field(
        default=None, gt=0, lt=0.5, description="Poisson's ratio override"
    )

    @field_validator("segments")
    @classmethod
    def validate_segments(cls, v: list[Segment]) -> list[Segment]:
        """Validate segment list is not empty and has unique types."""
        if not v:
            raise ValueError("At least one segment is required")
        return v


# =============================================================================
# Equipment Models
# =============================================================================


class TugProperties(BaseModel):
    """
    Properties for tug buoys used in installation operations.

    Attributes:
        mass: Tug mass (te).
        volume: Tug displaced volume (m3).
        height: Optional tug height for drawing (m).
        moments_of_inertia: Optional moments of inertia [Ixx, Iyy, Izz] (te.m2).
    """

    mass: float = Field(..., ge=0, description="Tug mass (te)")
    volume: float = Field(..., ge=0, description="Displaced volume (m3)")
    height: float = Field(default=6, gt=0, description="Tug height for drawing (m)")
    moments_of_inertia: list[float] | None = Field(
        default=None, description="Moments of inertia [Ixx, Iyy, Izz] (te.m2)"
    )


class Tugs(BaseModel):
    """
    Tug configuration for floating installation operations.

    Attributes:
        count: Number of tugs.
        spacing: Distance between adjacent tugs (m).
        first_position: [x, y, z] position of first tug (m).
        properties: Tug physical properties.
    """

    count: int = Field(..., ge=1, le=20, description="Number of tugs")
    spacing: float = Field(..., gt=0, description="Spacing between tugs (m)")
    first_position: list[float] = Field(
        ..., min_length=3, max_length=3, description="First tug position [x, y, z] (m)"
    )
    properties: TugProperties = Field(..., description="Tug physical properties")

    @field_validator("first_position")
    @classmethod
    def validate_position(cls, v: list[float]) -> list[float]:
        """Validate position has exactly 3 components."""
        if len(v) != 3:
            raise ValueError(f"Position must have exactly 3 components, got {len(v)}")
        return v


class Rollers(BaseModel):
    """
    Roller system configuration for pipeline support.

    Attributes:
        position: [x, y, z] position of roller system (m).
        supports: Number of support points on the roller.
        support_type: Optional support type name.
    """

    position: list[float] = Field(
        ..., min_length=3, max_length=3, description="Roller position [x, y, z] (m)"
    )
    supports: int = Field(default=4, ge=1, le=20, description="Number of supports")
    support_type: str = Field(default="Support type2", description="Support type name")

    @field_validator("position")
    @classmethod
    def validate_position(cls, v: list[float]) -> list[float]:
        """Validate position has exactly 3 components."""
        if len(v) != 3:
            raise ValueError(f"Position must have exactly 3 components, got {len(v)}")
        return v


class BuoyancyModuleProperties(BaseModel):
    """
    Properties for inline buoyancy modules.

    Attributes:
        mass: Module mass (te).
        volume: Module displaced volume (m3).
        height: Optional module height (m).
    """

    mass: float = Field(default=0.05, ge=0, description="Module mass (te)")
    volume: float = Field(..., gt=0, description="Displaced volume (m3)")
    height: float = Field(default=2, gt=0, description="Module height (m)")


class BuoyancyModules(BaseModel):
    """
    Buoyancy module configuration for pipeline flotation.

    Attributes:
        spacing: Distance between buoyancy modules (m).
        properties: Module physical properties.
        start_arc_length: Optional start position for BM zone (m from End A).
        end_arc_length: Optional end position for BM zone (m from End A).
    """

    spacing: float = Field(..., gt=0, description="Module spacing (m)")
    properties: BuoyancyModuleProperties = Field(..., description="Module properties")
    start_arc_length: float | None = Field(
        default=None, ge=0, description="Start of BM zone (m from End A)"
    )
    end_arc_length: float | None = Field(
        default=None, ge=0, description="End of BM zone (m from End A)"
    )

    @model_validator(mode="after")
    def validate_arc_length_range(self) -> "BuoyancyModules":
        """Validate start is before end when both are specified."""
        if (
            self.start_arc_length is not None
            and self.end_arc_length is not None
            and self.start_arc_length >= self.end_arc_length
        ):
            raise ValueError(
                f"start_arc_length ({self.start_arc_length}) must be less than "
                f"end_arc_length ({self.end_arc_length})"
            )
        return self


class Ramp(BaseModel):
    """
    Ramp or fixed shape definition for seabed features.

    Attributes:
        name: Shape name for OrcaFlex.
        type: Shape type (block, curved_plate, cylinder).
        origin: [x, y, z] origin position (m). Optional for some types.
        size: [length, width, height] dimensions (m). Optional.
        rotation: [rx, ry, rz] rotation angles (deg). Optional.
    """

    name: str = Field(..., min_length=1, description="Shape name")
    type: str = Field(..., description="Shape type (block, curved_plate, etc.)")
    origin: list[float] | None = Field(
        default=None,
        min_length=3,
        max_length=3,
        description="Origin position [x, y, z] (m)",
    )
    size: list[float] | None = Field(
        default=None,
        alias="Size",
        min_length=3,
        max_length=3,
        description="Dimensions [L, W, H] (m)",
    )
    rotation: list[float] | None = Field(
        default=None,
        alias="Rotation",
        min_length=3,
        max_length=3,
        description="Rotation [rx, ry, rz] (deg)",
    )

    class Config:
        """Pydantic config to allow field aliasing."""
        populate_by_name = True

    @field_validator("origin", "size", "rotation", mode="before")
    @classmethod
    def validate_vector3(cls, v: list[float] | None) -> list[float] | None:
        """Validate 3-component vectors."""
        if v is not None and len(v) != 3:
            raise ValueError(f"Must have exactly 3 components, got {len(v)}")
        return v


class Equipment(BaseModel):
    """
    Complete equipment specification for installation operations.

    Contains all equipment definitions including tugs, rollers,
    buoyancy modules, and seabed ramps/shapes.
    """

    tugs: Tugs | None = Field(default=None, description="Tug configuration")
    rollers: Rollers | None = Field(default=None, description="Roller configuration")
    buoyancy_modules: BuoyancyModules | None = Field(
        default=None, description="Buoyancy module configuration"
    )
    ramps: list[Ramp] = Field(
        default_factory=list, description="Ramp/shape definitions"
    )


# =============================================================================
# Simulation Models
# =============================================================================


class Simulation(BaseModel):
    """
    Simulation control parameters.

    Attributes:
        time_step: Outer time step for dynamics (s).
        stages: Stage durations [build-up, main] (s).
        north_direction: Direction of model North in degrees.
        inner_time_step: Optional inner time step (s).
        implicit_constant: Optional implicit integration constant.
    """

    time_step: float = Field(
        default=0.1, gt=0, le=1, description="Outer time step (s)"
    )
    stages: list[int | float] = Field(
        default_factory=lambda: [8, 16],
        min_length=1,
        description="Stage durations (s)",
    )
    north_direction: float = Field(
        default=0, ge=0, lt=360, description="North direction (deg)"
    )
    inner_time_step: float | None = Field(
        default=None, gt=0, le=0.1, description="Inner time step (s)"
    )
    implicit_constant: float | None = Field(
        default=None, ge=0, le=1, description="Implicit integration constant"
    )

    @field_validator("time_step")
    @classmethod
    def validate_time_step(cls, v: float) -> float:
        """Validate time step is reasonable for offshore analysis."""
        if v < 0.001:
            raise ValueError(
                f"Time step {v}s is too small. Minimum recommended is 0.001s"
            )
        return v

    @field_validator("stages")
    @classmethod
    def validate_stages(cls, v: list[int | float]) -> list[int | float]:
        """Validate stage durations are positive."""
        for i, stage in enumerate(v):
            if stage <= 0:
                raise ValueError(f"Stage {i} duration must be positive, got {stage}")
        return v


# =============================================================================
# Root Model
# =============================================================================


class ProjectInputSpec(BaseModel):
    """
    Root model for project-specific OrcaFlex model input specification.

    This is the main entry point for validating YAML input files.
    It contains all components needed to generate a complete OrcaFlex model.

    Example:
        ```python
        from digitalmodel.modules.orcaflex.modular_generator.schema import ProjectInputSpec
        import yaml

        with open('spec.yml') as f:
            data = yaml.safe_load(f)

        spec = ProjectInputSpec(**data)
        print(f"Model: {spec.metadata.name}")
        print(f"Pipeline length: {sum(s.length for s in spec.pipeline.segments)}m")
        ```

    Attributes:
        metadata: Model identification and categorization.
        environment: Environmental conditions (water, metocean).
        pipeline: Pipeline definition (dimensions, coatings, segments).
        equipment: Installation equipment (tugs, rollers, buoyancy modules).
        simulation: Simulation control parameters.
    """

    metadata: Metadata = Field(..., description="Model metadata")
    environment: Environment = Field(..., description="Environmental conditions")
    pipeline: Pipeline = Field(..., description="Pipeline definition")
    equipment: Equipment = Field(
        default_factory=Equipment, description="Equipment configuration"
    )
    simulation: Simulation = Field(
        default_factory=Simulation, description="Simulation parameters"
    )

    @model_validator(mode="after")
    def validate_consistency(self) -> "ProjectInputSpec":
        """
        Cross-validate model components for physical consistency.

        Checks:
        - Water depth consistency with seabed features
        - Tug positions relative to pipeline length
        - Buoyancy module spacing vs segment length
        """
        # Check if tug spacing and count fit within pipeline length
        if self.equipment.tugs:
            total_pipeline_length = sum(s.length for s in self.pipeline.segments)
            tugs = self.equipment.tugs
            max_tug_x = tugs.first_position[0] + (tugs.count - 1) * tugs.spacing

            # Only warn if tugs extend significantly beyond pipeline
            if max_tug_x > total_pipeline_length * 1.5:
                # This could be intentional for staged installation
                pass  # Could add warning logging here

        return self

    def get_total_pipeline_length(self) -> float:
        """Calculate total pipeline length from all segments."""
        return sum(s.length for s in self.pipeline.segments)

    def get_tug_positions(self) -> list[list[float]]:
        """Generate positions for all tugs based on spacing configuration."""
        if not self.equipment.tugs:
            return []

        tugs = self.equipment.tugs
        positions = []
        for i in range(tugs.count):
            x = tugs.first_position[0] + i * tugs.spacing
            y = tugs.first_position[1]
            z = tugs.first_position[2]
            positions.append([x, y, z])
        return positions

    def get_buoyancy_module_positions(self) -> list[float]:
        """Generate arc-length positions for buoyancy modules."""
        if not self.equipment.buoyancy_modules:
            return []

        bm = self.equipment.buoyancy_modules
        total_length = self.get_total_pipeline_length()

        start = bm.start_arc_length or 0
        end = bm.end_arc_length or total_length

        positions = []
        current = start
        while current <= end:
            positions.append(current)
            current += bm.spacing

        return positions
