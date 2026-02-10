"""Environment models for marine conditions."""

from __future__ import annotations

from pydantic import BaseModel, Field, field_validator


class Water(BaseModel):
    """
    Water properties for the marine environment.

    Attributes:
        depth: Water depth in meters. Must be positive.
        density: Seawater density in te/m3. Typical range: 1.02-1.03.
            Some OrcaFlex models use non-standard values (e.g. model-scale
            density, air density for in-air models).
    """

    depth: float = Field(..., gt=0, description="Water depth (m)")
    density: float = Field(
        default=1.025,
        gt=0,
        description="Fluid density (te/m3), typical seawater 1.025",
    )


class SeabedStiffness(BaseModel):
    """
    Seabed soil stiffness parameters.

    Attributes:
        normal: Normal (vertical) stiffness in kN/m/m2.
        shear: Shear (lateral) stiffness in kN/m/m2.
    """

    normal: float = Field(..., ge=0, description="Normal stiffness (kN/m/m2)")
    shear: float = Field(..., ge=0, description="Shear stiffness (kN/m/m2)")


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
