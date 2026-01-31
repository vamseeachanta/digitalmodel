"""Equipment models for installation operations."""

from __future__ import annotations

from pydantic import BaseModel, Field, field_validator, model_validator


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
