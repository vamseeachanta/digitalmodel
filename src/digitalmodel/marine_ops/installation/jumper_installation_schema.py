"""Pydantic v2 input schema for jumper installation analysis (issue #506).

Solver-agnostic specification for the OrcaFlex jumper installation spec.yml
files under ``docs/domains/orcaflex/subsea/jumper/installation/`` (Ballymore
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
from typing import Dict, List, Optional, Union

from pydantic import BaseModel, Field, field_validator


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
    "JumperInstallationSpec",
]
