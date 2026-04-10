"""Environment models for marine conditions."""

from __future__ import annotations

from typing import Any

from pydantic import BaseModel, Field, field_validator, model_validator


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

    normal: float = Field(default=0, ge=0, description="Normal stiffness (kN/m/m2)")
    shear: float = Field(default=0, ge=0, description="Shear stiffness (kN/m/m2)")

    @field_validator("normal", "shear", mode="before")
    @classmethod
    def _coerce_none_to_zero(cls, v: Any) -> float:
        """Treat YAML null (~) as 0 for stiffness fields."""
        return 0 if v is None else v


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

    @field_validator("slope", mode="before")
    @classmethod
    def _coerce_none_slope(cls, v: Any) -> float:
        """Treat YAML null (~) as 0 for slope."""
        return 0 if v is None else v


class WaveTrain(BaseModel):
    """Single wave train specification.

    Attributes:
        name: Display name for this wave train.
        type: Wave theory type (dean_stream, airy, etc.).
        height: Significant wave height Hs (m) or deterministic height.
        period: Wave period Tp (s) or zero-crossing period Tz.
        direction: Wave direction in degrees from North, going to.
        phase: Wave phase in degrees.
        gamma: JONSWAP gamma parameter (default 3.3).
    """

    name: str = "Wave 1"
    type: str = Field(default="airy", description="Wave theory type")
    height: float = Field(default=0, ge=0, description="Wave height Hs (m)")
    period: float = Field(default=8, gt=0, description="Wave period Tp (s)")
    direction: float = Field(
        default=0, description="Wave direction (deg from North)"
    )
    phase: float = Field(default=0, ge=0, lt=360, description="Wave phase (deg)")
    gamma: float = Field(
        default=3.3, gt=1, le=7, description="JONSWAP gamma (typically 1-7)"
    )

    @field_validator("direction", mode="before")
    @classmethod
    def _normalize_direction(cls, v: Any) -> float:
        """Normalize direction to [0, 360)."""
        if v is None:
            return 0
        return float(v) % 360


class Waves(BaseModel):
    """Wave specification supporting single or multiple wave trains.

    Backward compatible: single-train specs using flat fields (type, height,
    period, etc.) are auto-wrapped into ``trains[0]``.  Multi-train specs
    use the ``trains`` list directly.

    Properties ``type``, ``height``, ``period``, ``direction``, ``phase``,
    and ``gamma`` delegate to ``trains[0]`` so existing code that reads
    ``waves.height`` continues to work unchanged.
    """

    trains: list[WaveTrain] = Field(
        default_factory=lambda: [WaveTrain()],
        description="Wave trains",
    )

    @model_validator(mode="before")
    @classmethod
    def _compat_single_train(cls, data: Any) -> Any:
        """If flat fields provided without trains, wrap into trains[0]."""
        if isinstance(data, dict) and "trains" not in data:
            flat_keys = {"type", "height", "period", "direction", "phase", "gamma", "name"}
            train_fields = {k: data.pop(k) for k in list(data) if k in flat_keys}
            if train_fields:
                data["trains"] = [train_fields]
        return data

    # ------------------------------------------------------------------
    # Backward-compat properties — delegate to trains[0]
    # ------------------------------------------------------------------
    @property
    def type(self) -> str:
        """Wave theory type of the primary train."""
        return self.trains[0].type if self.trains else "airy"

    @property
    def height(self) -> float:
        """Wave height of the primary train (m)."""
        return self.trains[0].height if self.trains else 0

    @property
    def period(self) -> float:
        """Wave period of the primary train (s)."""
        return self.trains[0].period if self.trains else 8

    @property
    def direction(self) -> float:
        """Wave direction of the primary train (deg)."""
        return self.trains[0].direction if self.trains else 0

    @property
    def phase(self) -> float:
        """Wave phase of the primary train (deg)."""
        return self.trains[0].phase if self.trains else 0

    @property
    def gamma(self) -> float:
        """JONSWAP gamma of the primary train."""
        return self.trains[0].gamma if self.trains else 3.3



class CurrentProfile(BaseModel):
    """
    Current profile point at a specific depth.

    Attributes:
        depth: Depth below water surface (m). Positive downward.
        factor: Current speed factor at this depth (fraction of surface speed).
    """

    depth: float = Field(..., ge=0, description="Depth below surface (m)")
    factor: float = Field(..., ge=0, description="Speed factor (fraction of surface speed)")


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
        default=0, description="Current direction (deg from North)"
    )
    profile: list[list[float]] = Field(
        default_factory=lambda: [[0, 1.0]],
        description="Current profile [[depth, factor], ...]",
    )

    @field_validator("direction", mode="before")
    @classmethod
    def _normalize_direction(cls, v: Any) -> float:
        """Normalize direction to [0, 360)."""
        if v is None:
            return 0
        return float(v) % 360

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
            if factor < 0:
                raise ValueError(
                    f"Profile factor at point {i} must be >= 0, got {factor}"
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
        default=0, description="Wind direction (deg from North)"
    )
    reference_height: float = Field(
        default=10, gt=0, description="Reference height (m)"
    )

    @field_validator("direction", mode="before")
    @classmethod
    def _normalize_direction(cls, v: Any) -> float:
        """Normalize direction to [0, 360)."""
        if v is None:
            return 0
        return float(v) % 360

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

    The ``raw_properties`` dict captures ALL raw OrcaFlex Environment
    properties from a monolithic YAML during extraction.  The
    EnvironmentBuilder uses this as a base layer, overlaying spec-derived
    values on top.  This ensures round-trip fidelity for properties not
    captured by the typed schema fields.
    """

    water: Water = Field(..., description="Water properties")
    seabed: Seabed = Field(..., description="Seabed properties")
    waves: Waves = Field(default_factory=Waves, description="Wave specification")
    current: Current = Field(default_factory=Current, description="Current specification")
    wind: Wind = Field(default_factory=Wind, description="Wind specification")
    raw_properties: dict[str, Any] = Field(
        default_factory=dict,
        description="Pass-through raw OrcaFlex Environment properties for round-trip fidelity",
    )
