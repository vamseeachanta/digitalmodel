"""Canonical input schema for passing ship analysis (WRK-131 Phase 2).

Solver-agnostic specification that fully defines a passing ship analysis:
moored vessel, passing ship dimensions and track, environment, analysis cases,
and output requests.

Follows the DiffractionSpec pattern from
``digitalmodel.hydrodynamics.diffraction.input_schemas``.

Example usage::

    from digitalmodel.hydrodynamics.passing_ship.input_schemas import PassingShipSpec

    spec = PassingShipSpec.from_yaml("spec.yml")
    moored = spec.moored_vessel
    passing = spec.passing_ship
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, List, Optional, Union

from pydantic import BaseModel, Field, field_validator, model_validator


# ---------------------------------------------------------------------------
# Hull / vessel geometry
# ---------------------------------------------------------------------------


class HullSpec(BaseModel):
    """Hull geometry for a vessel."""

    loa: float = Field(..., gt=0, description="Length overall (m)")
    beam: float = Field(..., gt=0, description="Breadth moulded (m)")
    draft: float = Field(..., gt=0, description="Draft (m)")
    block_coefficient: float = Field(
        0.75,
        gt=0,
        le=1.0,
        description="Block coefficient Cb (-)",
    )
    displacement_tonnes: Optional[float] = Field(
        None,
        gt=0,
        description="Displacement (t); derived from hull geometry if not given",
    )

    @model_validator(mode="after")
    def derive_displacement(self) -> HullSpec:
        """Auto-compute displacement from hull dimensions if not provided."""
        if self.displacement_tonnes is None:
            volume = self.loa * self.beam * self.draft * self.block_coefficient
            self.displacement_tonnes = volume * 1.025  # seawater density
        return self


# ---------------------------------------------------------------------------
# Mooring lines
# ---------------------------------------------------------------------------


class MooringLineSpec(BaseModel):
    """Single mooring line specification."""

    line_id: str = Field(..., description="Unique line identifier")
    line_type: str = Field(
        ...,
        description="Line type: 'chain' | 'wire' | 'synthetic'",
    )
    length: float = Field(..., gt=0, description="Unstretched line length (m)")
    pretension: float = Field(..., ge=0, description="Pretension (kN)")
    fairlead_x: float = Field(
        ...,
        description="Fairlead x-position from vessel midship (m)",
    )
    fairlead_y: float = Field(
        ...,
        description=(
            "Fairlead y-position from centreline (m); positive = starboard"
        ),
    )
    anchor_x: float = Field(..., description="Anchor x-position (m)")
    anchor_y: float = Field(..., description="Anchor y-position (m)")

    @field_validator("line_type")
    @classmethod
    def validate_line_type(cls, v: str) -> str:
        allowed = {"chain", "wire", "synthetic"}
        if v.lower() not in allowed:
            raise ValueError(
                f"line_type must be one of {allowed}, got '{v}'"
            )
        return v.lower()


# ---------------------------------------------------------------------------
# Moored vessel
# ---------------------------------------------------------------------------


class MooredVesselSpec(BaseModel):
    """Complete specification of the moored vessel."""

    hull: HullSpec
    loading_condition: str = Field(
        "full_load",
        description="Loading condition: 'full_load' | 'ballast'",
    )
    draft: Optional[float] = Field(
        None,
        gt=0,
        description=(
            "Draft override (m); supersedes hull.draft when set"
        ),
    )
    mooring_lines: List[MooringLineSpec] = Field(
        default_factory=list,
        description="Mooring line definitions",
    )
    rao_source: Optional[str] = Field(
        None,
        description="Path to RAO file (relative to spec.yml)",
    )

    @field_validator("loading_condition")
    @classmethod
    def validate_loading_condition(cls, v: str) -> str:
        allowed = {"full_load", "ballast"}
        if v.lower() not in allowed:
            raise ValueError(
                f"loading_condition must be one of {allowed}, got '{v}'"
            )
        return v.lower()

    def effective_draft(self) -> float:
        """Return the operative draft (override or hull value)."""
        return self.draft if self.draft is not None else self.hull.draft


# ---------------------------------------------------------------------------
# Passing ship
# ---------------------------------------------------------------------------


class PassingShipDimensions(BaseModel):
    """Dimensions of the passing vessel."""

    loa: float = Field(..., gt=0, description="Length overall (m)")
    beam: float = Field(..., gt=0, description="Breadth moulded (m)")
    draft: float = Field(..., gt=0, description="Draft (m)")
    displacement_tonnes: Optional[float] = Field(
        None,
        gt=0,
        description="Displacement (t); derived if not given",
    )
    block_coefficient: float = Field(
        0.75,
        gt=0,
        le=1.0,
        description="Block coefficient Cb (-)",
    )

    @model_validator(mode="after")
    def derive_displacement(self) -> PassingShipDimensions:
        if self.displacement_tonnes is None:
            volume = (
                self.loa * self.beam * self.draft * self.block_coefficient
            )
            self.displacement_tonnes = volume * 1.025
        return self


class PassingTrack(BaseModel):
    """Track of the passing vessel relative to the moored vessel."""

    speed_knots: float = Field(
        ...,
        description="Passing ship speed (knots)",
    )
    separation_m: float = Field(
        ...,
        description="Lateral distance centreline-to-centreline (m)",
    )
    angle_deg: float = Field(
        0.0,
        description=(
            "Heading angle relative to moored vessel (deg); "
            "0 = parallel, 90 = perpendicular"
        ),
    )
    closest_approach_offset_m: float = Field(
        0.0,
        description=(
            "Longitudinal offset at closest point of approach (m)"
        ),
    )

    @field_validator("speed_knots")
    @classmethod
    def validate_speed(cls, v: float) -> float:
        if v <= 0:
            raise ValueError(
                f"speed_knots must be positive, got {v}"
            )
        return v

    @field_validator("separation_m")
    @classmethod
    def validate_separation(cls, v: float) -> float:
        if v <= 0:
            raise ValueError(
                f"separation_m must be positive, got {v}"
            )
        return v

    @property
    def speed_ms(self) -> float:
        """Passing speed in m/s."""
        return self.speed_knots * 0.514444


class PassingShipScenario(BaseModel):
    """Full specification of the passing vessel and its track."""

    vessel_type: str = Field(
        "tanker",
        description="Vessel type: tanker | container | bulk | lng",
    )
    dimensions: PassingShipDimensions
    track: PassingTrack

    @field_validator("vessel_type")
    @classmethod
    def validate_vessel_type(cls, v: str) -> str:
        allowed = {"tanker", "container", "bulk", "lng"}
        if v.lower() not in allowed:
            raise ValueError(
                f"vessel_type must be one of {allowed}, got '{v}'"
            )
        return v.lower()


# ---------------------------------------------------------------------------
# Environment
# ---------------------------------------------------------------------------


class EnvironmentSpec(BaseModel):
    """Environmental conditions for the passing ship analysis."""

    water_depth: Union[float, str] = Field(
        "infinite",
        description="Water depth (m) or 'infinite'",
    )
    water_density: float = Field(
        1025.0,
        gt=0,
        description="Water density (kg/m³)",
    )
    wind_speed: Optional[float] = Field(
        None,
        ge=0,
        description="Wind speed (m/s)",
    )
    wind_direction_deg: Optional[float] = Field(
        None,
        description="Wind direction (deg, meteorological convention)",
    )
    current_speed: Optional[float] = Field(
        None,
        ge=0,
        description="Surface current speed (m/s)",
    )
    current_direction_deg: Optional[float] = Field(
        None,
        description="Current direction (deg, meteorological convention)",
    )

    @field_validator("water_depth", mode="before")
    @classmethod
    def validate_water_depth(cls, v: Any) -> Union[float, str]:
        if isinstance(v, str):
            if v.lower() in ("infinite", "inf", "deep"):
                return "infinite"
            raise ValueError(
                f"Invalid water_depth string: '{v}'. "
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

    @property
    def is_infinite_depth(self) -> bool:
        """True when water depth is specified as infinite."""
        return self.water_depth == "infinite"


# ---------------------------------------------------------------------------
# Analysis cases
# ---------------------------------------------------------------------------


class AnalysisCase(BaseModel):
    """A single analysis case within the parametric matrix."""

    name: str = Field(..., description="Unique case identifier")
    include_wind: bool = Field(
        False,
        description="Include wind loads in this case",
    )
    include_passing_ship: bool = Field(
        True,
        description="Include passing ship forces in this case",
    )
    passing_speed_knots: Optional[float] = Field(
        None,
        gt=0,
        description=(
            "Override passing speed (knots); uses scenario speed if not set"
        ),
    )
    separation_m: Optional[float] = Field(
        None,
        gt=0,
        description=(
            "Override lateral separation (m); uses scenario separation if not set"
        ),
    )


# ---------------------------------------------------------------------------
# Output requests
# ---------------------------------------------------------------------------


class OutputSpec(BaseModel):
    """Requested output quantities."""

    line_tensions: bool = Field(True, description="Compute mooring line tensions")
    vessel_motions: bool = Field(True, description="Compute vessel motions")
    fender_loads: bool = Field(False, description="Compute fender loads")
    force_time_histories: bool = Field(
        True,
        description="Generate force time history series",
    )


# ---------------------------------------------------------------------------
# Top-level spec
# ---------------------------------------------------------------------------


class PassingShipSpec(BaseModel):
    """Full passing ship analysis specification.

    This is the schema for ``spec.yml`` files that define a complete passing
    ship analysis, including moored vessel properties, passing ship scenario,
    environmental conditions, analysis cases, and output requests.

    Example::

        spec = PassingShipSpec.from_yaml("spec.yml")
        depth = spec.environment.water_depth
        speed = spec.passing_ship.track.speed_knots
    """

    name: str = Field(..., description="Analysis name / identifier")
    description: str = Field("", description="Free-text analysis description")
    moored_vessel: MooredVesselSpec
    passing_ship: PassingShipScenario
    environment: EnvironmentSpec
    analysis_cases: List[AnalysisCase] = Field(
        default_factory=list,
        description="Parametric analysis cases",
    )
    outputs: OutputSpec = Field(
        default_factory=OutputSpec,
        description="Output request specification",
    )

    @classmethod
    def from_yaml(cls, path: Union[str, Path]) -> PassingShipSpec:
        """Load spec from a YAML file.

        Args:
            path: Path to the YAML specification file.

        Returns:
            Validated PassingShipSpec instance.
        """
        import yaml

        with open(path) as fh:
            data = yaml.safe_load(fh)
        return cls.model_validate(data)

    def to_yaml(self, path: Union[str, Path]) -> Path:
        """Save spec to a YAML file.

        Args:
            path: Destination path for the YAML file.

        Returns:
            Path object pointing to the written file.
        """
        import yaml

        path = Path(path)
        data = self.model_dump(mode="json", exclude_none=True)
        with open(path, "w") as fh:
            yaml.dump(data, fh, default_flow_style=False, sort_keys=False)
        return path

    @classmethod
    def from_dict(cls, data: dict) -> PassingShipSpec:
        """Construct from a plain dictionary.

        Args:
            data: Dictionary matching the PassingShipSpec schema.

        Returns:
            Validated PassingShipSpec instance.
        """
        return cls.model_validate(data)

    def to_dict(self) -> dict:
        """Serialise to a plain dictionary.

        Returns:
            Dictionary representation (Pydantic v2 model_dump).
        """
        return self.model_dump()


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


__all__ = [
    "HullSpec",
    "MooringLineSpec",
    "MooredVesselSpec",
    "PassingShipDimensions",
    "PassingTrack",
    "PassingShipScenario",
    "EnvironmentSpec",
    "AnalysisCase",
    "OutputSpec",
    "PassingShipSpec",
]
