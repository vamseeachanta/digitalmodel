"""Pydantic v2 input schema for jumper installation analysis (issue #506).

Solver-agnostic specification for the OrcaFlex jumper installation spec.yml
files under ``docs/domains/orcaflex/subsea/jumper/installation/`` (GoM tieback
MF-PLET and PLET-PLEM). These specs use a different top-level structure than
``ProjectInputSpec`` (``pipe``/``jumper``/``environment.metocean`` keys), and
previously had no schema validation -- the spec audit only flagged them as a
"different_schema" and skipped them.

This module mirrors the convention of
``digitalmodel.hydrodynamics.passing_ship.input_schemas`` and
``digitalmodel.hydrodynamics.diffraction.input_schemas`` (Pydantic v2,
``from_yaml``/``from_dict`` constructors, composable sub-models). Extra keys
are ignored (the repo norm; these models are not ``extra="forbid"``) so the
two real specs -- which differ in their optional sections -- both validate.

Example usage::

    from digitalmodel.marine_ops.installation.jumper_installation_schema import (
        JumperInstallationSpec,
    )

    spec = JumperInstallationSpec.from_yaml("spec.yml")
    od = spec.pipe.outer_diameter
    cranes = spec.metadata.cranes
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Dict, List, Literal, Optional, Union

from pydantic import (
    BaseModel,
    ConfigDict,
    Field,
    FiniteFloat,
    field_validator,
    model_validator,
)


# ---------------------------------------------------------------------------
# Metadata
# ---------------------------------------------------------------------------


class JumperMetadata(BaseModel):
    """Top-level ``metadata`` block of a jumper installation spec."""

    name: str = Field(..., description="Jumper configuration identifier")
    description: str = Field("", description="Free-text description")
    structure: str = Field("jumper", description="Structure type")
    operation: Optional[str] = Field(
        None, description="Operation type, e.g. 'installation/lift'"
    )
    cranes: List[str] = Field(
        default_factory=list,
        description="Crane configuration keys, e.g. ['SZ', 'DZ']",
    )


# ---------------------------------------------------------------------------
# Environment
# ---------------------------------------------------------------------------


class WaterSpec(BaseModel):
    """Water properties within ``environment.water``."""

    depth: float = Field(..., gt=0, description="Water depth (m)")
    density: float = Field(
        1.025, gt=0, description="Seawater density (te/m3)"
    )


class SeabedSpec(BaseModel):
    """Seabed properties within ``environment.seabed`` (optional)."""

    soil_type: Optional[str] = Field(
        None, description="Soil type, e.g. 'soft_clay'"
    )
    stiffness: Optional[Dict[str, float]] = Field(
        None, description="Seabed stiffness components (kN/m/m2)"
    )


class WindSpec(BaseModel):
    """Wind metocean data within ``environment.metocean.wind``."""

    design_speed: float = Field(..., ge=0, description="Design wind speed (m/s)")
    gust_factors: List[float] = Field(
        default_factory=list, description="Gust factor profile (-)"
    )


class WaveSpec(BaseModel):
    """Wave metocean data within ``environment.metocean.wave``."""

    significant: float = Field(..., ge=0, description="Significant wave height Hs (m)")
    period: float = Field(..., gt=0, description="Peak/zero-up period (s)")
    direction: Optional[float] = Field(
        None, description="Wave direction (deg, from-convention)"
    )


class MetoceanSpec(BaseModel):
    """Metocean conditions within ``environment.metocean`` (optional)."""

    wind: Optional[WindSpec] = None
    wave: Optional[WaveSpec] = None


class JumperEnvironmentSpec(BaseModel):
    """``environment`` block of a jumper installation spec."""

    water: WaterSpec
    seabed: Optional[SeabedSpec] = None
    metocean: Optional[MetoceanSpec] = None


# ---------------------------------------------------------------------------
# Pipe
# ---------------------------------------------------------------------------


class InsulationSpec(BaseModel):
    """Pipe insulation within ``pipe.insulation`` (optional)."""

    thickness: float = Field(..., ge=0, description="Insulation thickness (m)")
    density: float = Field(..., gt=0, description="Insulation density (kg/m3)")


class PipeSpec(BaseModel):
    """``pipe`` block: jumper steel-pipe geometry and material."""

    name: str = Field(..., description="Pipe identifier")
    outer_diameter: float = Field(..., gt=0, description="Steel OD (m)")
    wall_thickness: float = Field(..., gt=0, description="Wall thickness (m)")
    bend_radius: Optional[float] = Field(
        None, gt=0, description="Bend radius (m)"
    )
    insulated_od: Optional[float] = Field(
        None, gt=0, description="OD including insulation (m)"
    )
    insulation: Optional[InsulationSpec] = None
    steel_density: float = Field(
        7850.0, gt=0, description="Steel density (kg/m3)"
    )


# ---------------------------------------------------------------------------
# Jumper segment / module geometry
# ---------------------------------------------------------------------------


class JumperGeometrySpec(BaseModel):
    """``jumper`` block: segment lengths and module counts (JumperConfig)."""

    config_name: str = Field(..., description="Jumper config name")
    seg_a_inch: float = Field(..., gt=0, description="Segment A length (in)")
    seg_b_inch: float = Field(..., gt=0, description="Segment B length (in)")
    seg_c_inch: float = Field(..., gt=0, description="Segment C length (in)")
    seg_d_inch: float = Field(..., gt=0, description="Segment D length (in)")
    seg_e_inch: float = Field(..., gt=0, description="Segment E length (in)")
    seg_f_inch: float = Field(..., gt=0, description="Segment F length (in)")
    seg_g_inch: float = Field(..., gt=0, description="Segment G length (in)")
    connector_weight_kg: Optional[float] = Field(
        None, ge=0, description="Per-connector weight (kg)"
    )
    connector_length_m: Optional[float] = Field(
        None, ge=0, description="Connector length (m)"
    )
    clamp_weight_kg: Optional[float] = Field(
        None, ge=0, description="Clamp weight (kg)"
    )
    clamp_wll_te: Optional[float] = Field(
        None, ge=0, description="Clamp working load limit (te)"
    )
    num_buoy_modules_in_c: Optional[int] = Field(
        None, ge=0, description="Buoyancy modules in segment C"
    )
    num_buoy_modules_in_d: Optional[int] = Field(
        None, ge=0, description="Buoyancy modules in segment D"
    )
    num_buoy_modules_in_e: Optional[int] = Field(
        None, ge=0, description="Buoyancy modules in segment E"
    )
    num_strakes: Optional[int] = Field(
        None, ge=0, description="Total strake modules"
    )
    clamp_ocs_offset_m: Optional[float] = Field(
        None, description="Clamp OCS offset (m)"
    )


# ---------------------------------------------------------------------------
# Crane configuration
# ---------------------------------------------------------------------------


class CraneSpec(BaseModel):
    """Single crane configuration entry within ``crane_configuration``."""

    radius_m: float = Field(..., gt=0, description="Crane radius (m)")
    swl_te: float = Field(..., gt=0, description="Safe working load (te)")
    ddf: float = Field(
        1.0, gt=0, description="Dynamic design factor (-)"
    )
    ahc_offset: Optional[bool] = Field(
        None, description="Active heave compensation enabled"
    )


# ---------------------------------------------------------------------------
# Vessel attachments for modular YAML conversion
# ---------------------------------------------------------------------------


class JumperVesselTypeSpec(BaseModel):
    """Domain-supplied GenericVesselType fields; RAOs pass through unchanged."""

    model_config = ConfigDict(extra="forbid")

    name: str = Field(min_length=1)
    length: FiniteFloat = Field(gt=0, description="Type length (m)")
    properties: Dict[str, Any] = Field(default_factory=dict)


class JumperVesselSpec(BaseModel):
    """Explicit generic vessel definition; no vessel data are inferred."""

    model_config = ConfigDict(extra="forbid")

    name: str = Field(min_length=1)
    vessel_type: str = Field(min_length=1)
    connection: Literal["Free", "Fixed"]
    initial_position: List[FiniteFloat] = Field(min_length=3, max_length=3)
    length: Optional[FiniteFloat] = Field(None, gt=0, description="Vessel length (m)")
    properties: Dict[str, Any] = Field(default_factory=dict)

    @model_validator(mode="after")
    def validate_explicit_motion(self) -> JumperVesselSpec:
        """Require explicit vessel orientation and motion selections."""
        # Orcina online help (2026), Vessel data: Initial position and
        # orientation; calculation settings must be supplied, not inferred.
        for key in ("Orientation", "PrimaryMotion", "SuperimposedMotion"):
            if key not in self.properties:
                raise ValueError(f"vessel properties require {key}")
        return self


class JumperEndConnectionSpec(BaseModel):
    """End coordinates and angles in connection axes (m, degrees).

    Source: Orcina, OrcaFlex online help (2026), Line data: End connections,
    Position and Orientation. Fixed coordinates are global; vessel attachments
    use vessel axes. All orientations are explicit inputs.
    """

    model_config = ConfigDict(extra="forbid")

    connection: str = Field(min_length=1)
    position: List[FiniteFloat] = Field(min_length=3, max_length=3)
    azimuth: FiniteFloat
    declination: FiniteFloat
    gamma: FiniteFloat

    def to_connection_row(self) -> List[Any]:
        """Emit the native table row; no release or line-relative coordinate."""
        return [
            self.connection, *self.position,
            self.azimuth, self.declination, self.gamma, None, None,
        ]


class JumperInstallationStageSpec(BaseModel):
    """Optional vessel definitions and explicit suspended-stage geometry.

    The legacy conversion remains in place when this block is absent.
    Suspended stages require a global starting spline supplied independently
    of the vessel-relative end coordinates; no rigid-body transform or rigging
    geometry is guessed. See Orcina online help (2026), Line data: Statics.
    """

    model_config = ConfigDict(extra="forbid")

    stage: Literal["in_place", "suspended"]
    vessel_types: List[JumperVesselTypeSpec]
    vessels: List[JumperVesselSpec]
    end_a: Optional[JumperEndConnectionSpec] = None
    end_b: Optional[JumperEndConnectionSpec] = None
    # Cubic starting spline uses at least four supplied control points.
    # Source: existing converter's SplineOrder=3; this is an input-shape rule.
    spline_control_points: Optional[List[List[FiniteFloat]]] = Field(
        None, min_length=4
    )

    @field_validator("spline_control_points")
    @classmethod
    def validate_spline_points(
        cls, points: Optional[List[List[float]]]
    ) -> Optional[List[List[float]]]:
        """Require three coordinates for each global spline point."""
        if points is not None and any(len(point) != 3 for point in points):
            raise ValueError("spline_control_points must contain XYZ triples")
        return points

    @model_validator(mode="after")
    def validate_references_and_stage(self) -> JumperInstallationStageSpec:
        """Check references and require complete suspended-stage inputs."""
        for field in ("vessels", "vessel_types"):
            names = [obj.name for obj in getattr(self, field)]
            if len(names) != len(set(names)):
                raise ValueError(f"duplicate names in {field}")
        type_names = {obj.name for obj in self.vessel_types}
        vessel_names = {obj.name for obj in self.vessels}
        if vessel_names & {"Fixed", "Free", "Anchored"}:
            raise ValueError("vessel names must not use reserved connection names")
        for vessel in self.vessels:
            if vessel.vessel_type not in type_names:
                raise ValueError("vessel_type must reference a supplied vessel type")
        ends = (self.end_a, self.end_b)
        if self.stage == "in_place":
            if (
                any(end is not None for end in ends)
                or self.spline_control_points is not None
            ):
                raise ValueError("in_place stage cannot specify suspended geometry")
            return self
        if not self.vessels or not self.vessel_types:
            raise ValueError("suspended stage requires vessels and vessel_types")
        for field in ("end_a", "end_b", "spline_control_points"):
            if getattr(self, field) is None:
                raise ValueError(f"suspended stage requires {field}")
        for end in ends:
            if end.connection != "Fixed" and end.connection not in vessel_names:
                raise ValueError("end connection must reference Fixed or a supplied vessel")
        if all(end.connection == "Fixed" for end in ends):
            raise ValueError("suspended stage requires at least one vessel attachment")
        return self


# ---------------------------------------------------------------------------
# Top-level spec
# ---------------------------------------------------------------------------


class JumperInstallationSpec(BaseModel):
    """Full jumper installation analysis specification.

    Schema for ``spec.yml`` files under
    ``docs/domains/orcaflex/subsea/jumper/installation/``. Captures pipe
    geometry, jumper segment lengths/module counts, environment, and crane
    configurations. Optional sub-blocks (``seabed``, ``metocean``,
    ``rigging``-style extras) vary between the MF-PLET and PLET-PLEM specs;
    extra/unmodeled keys (e.g. ``rigging``, ``buoyancy_modules``,
    ``weight_check``) are ignored.

    Example::

        spec = JumperInstallationSpec.from_yaml("spec.yml")
        od = spec.pipe.outer_diameter
        sz = spec.crane_configuration["SZ"]
    """

    metadata: JumperMetadata
    environment: JumperEnvironmentSpec
    pipe: PipeSpec
    jumper: JumperGeometrySpec
    installation: Optional[JumperInstallationStageSpec] = None
    crane_configuration: Dict[str, CraneSpec] = Field(
        default_factory=dict,
        description="Crane configurations keyed by crane id (SZ, DZ, ...)",
    )

    @field_validator("crane_configuration")
    @classmethod
    def validate_cranes_nonempty(cls, v: Dict[str, CraneSpec]) -> Dict[str, CraneSpec]:
        if not v:
            raise ValueError("crane_configuration must define at least one crane")
        return v

    @classmethod
    def from_yaml(cls, path: Union[str, Path]) -> "JumperInstallationSpec":
        """Load and validate a spec from a YAML file.

        Args:
            path: Path to the YAML specification file.

        Returns:
            Validated JumperInstallationSpec instance.
        """
        import yaml

        with open(path) as fh:
            data = yaml.safe_load(fh)
        return cls.model_validate(data)

    @classmethod
    def from_dict(cls, data: dict) -> "JumperInstallationSpec":
        """Construct from a plain dictionary.

        Args:
            data: Dictionary matching the JumperInstallationSpec schema.

        Returns:
            Validated JumperInstallationSpec instance.
        """
        return cls.model_validate(data)

    def to_dict(self) -> dict:
        """Serialise to a plain dictionary (Pydantic v2 model_dump)."""
        return self.model_dump()

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


__all__ = [
    "JumperMetadata",
    "WaterSpec",
    "SeabedSpec",
    "WindSpec",
    "WaveSpec",
    "MetoceanSpec",
    "JumperEnvironmentSpec",
    "InsulationSpec",
    "PipeSpec",
    "JumperGeometrySpec",
    "CraneSpec",
    "JumperVesselTypeSpec",
    "JumperVesselSpec",
    "JumperEndConnectionSpec",
    "JumperInstallationStageSpec",
    "JumperInstallationSpec",
]
