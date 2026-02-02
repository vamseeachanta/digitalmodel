"""Pipeline models for pipe definition, coatings, and segmentation."""

from __future__ import annotations

from pydantic import BaseModel, Field, field_validator, model_validator


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
