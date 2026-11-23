"""Unified declarative riser input schema (issue #809).

A single canonical ``input.yml`` schema spanning the riser variants the repo
currently accepts as separate models:

- ``SCR``      -> Steel Catenary Riser           (legacy: ``SCRDesignInput``)
- ``SLWR``     -> Steel Lazy-Wave Riser          (legacy: ``LazyWaveDesignInput``)
- ``TTR``      -> Top-Tensioned Riser            (legacy: ``TTRDesignInput``)
- ``DRILLING`` -> Drilling riser (top-tensioned variant)
- ``FLEXIBLE`` -> Flexible riser (lazy-wave style)

The schema is a *discriminated union* on the ``riser_type`` field, sharing
common ``geometry`` / ``material`` / ``environment`` / ``analysis`` blocks and
adding per-type extension blocks. It validates loudly (Pydantic v2), supports a
``from_yaml`` loader, and provides ``to_legacy_*`` adapters that map a unified
input back onto the existing ``riser_config`` design-input models so the
existing analytical calculators keep working unchanged.

This module is **additive and backward-compatible**: it imports (and does not
modify) the existing ``riser_config`` types, and the legacy loaders are left
untouched.

NOTE (deferred / follow-on for #809): wiring a ``riser`` study type into the
``python -m digitalmodel`` package router (CLI parity with the existing
``scripts/run_riser_analysis.py`` entry, which needs OrcFxAPI) is intentionally
NOT done here. This module provides only the schema + validation + loader +
adapters. See the issue's "Register a ``riser`` workflow/study type" item.

References:
    - API RP 2RD: Design of Risers for Floating Production Systems
    - DNV-OS-F201: Dynamic Risers
"""

from __future__ import annotations

from enum import Enum
from pathlib import Path
from typing import Annotated, Literal, Optional, Union

import yaml
from pydantic import BaseModel, Field

from .riser_config import (
    LazyWaveDesignInput,
    PipeGrade,
    RiserPipeProperties,
    SCRDesignInput,
    TTRDesignInput,
)


# ---------------------------------------------------------------------------
# Riser-type discriminator
# ---------------------------------------------------------------------------

class UnifiedRiserType(str, Enum):
    """Discriminator values for the unified riser input schema.

    Distinct from ``riser_config.RiserType`` (which is the analytical-model
    enum); this is the *input-schema* discriminator covering the five
    end-to-end study variants named in issue #809.
    """

    SCR = "SCR"
    SLWR = "SLWR"
    TTR = "TTR"
    DRILLING = "drilling"
    FLEXIBLE = "flexible"


# ---------------------------------------------------------------------------
# Shared blocks
# ---------------------------------------------------------------------------

class GeometryBlock(BaseModel):
    """Shared riser geometry common to all riser types."""

    model_config = {"extra": "forbid"}

    water_depth: float = Field(1500.0, gt=0.0, description="Water depth (m)")
    hang_off_elevation: float = Field(
        -10.0, description="Hang-off point elevation relative to MSL (m)"
    )
    vessel_draft: float = Field(12.0, ge=0.0, description="Vessel draft (m)")


class MaterialBlock(BaseModel):
    """Shared pipe / material properties.

    Mirrors the fields of ``RiserPipeProperties`` so a unified input maps
    cleanly onto the legacy pipe model.
    """

    model_config = {"extra": "forbid"}

    outer_diameter: float = Field(0.2731, gt=0.0, description="Outer diameter (m)")
    wall_thickness: float = Field(0.0254, gt=0.0, description="Wall thickness (m)")
    coating_thickness: float = Field(0.04, ge=0.0, description="External coating thickness (m)")
    coating_density: float = Field(900.0, ge=0.0, description="Coating density (kg/m^3)")
    grade: PipeGrade = Field(PipeGrade.X65, description="Pipe steel grade")
    contents_density: float = Field(800.0, ge=0.0, description="Internal fluid density (kg/m^3)")
    corrosion_allowance: float = Field(0.003, ge=0.0, description="Corrosion allowance (m)")
    seawater_density: float = Field(1025.0, gt=0.0, description="Seawater density (kg/m^3)")

    def to_pipe_properties(self) -> RiserPipeProperties:
        """Adapt to the legacy ``RiserPipeProperties`` model."""
        return RiserPipeProperties(**self.model_dump())


class EnvironmentBlock(BaseModel):
    """Shared metocean / environment block.

    Optional; informational for the schema today (the analytical legacy models
    take explicit motion amplitudes per-type). Kept here so the unified
    ``input.yml`` is one declarative document.
    """

    model_config = {"extra": "forbid"}

    surface_current: float = Field(0.0, ge=0.0, description="Surface current speed (m/s)")
    wave_height: float = Field(0.0, ge=0.0, description="Significant wave height Hs (m)")
    wave_period: float = Field(0.0, ge=0.0, description="Peak wave period Tp (s)")


class AnalysisBlock(BaseModel):
    """Shared analysis options."""

    model_config = {"extra": "forbid"}

    run_statics: bool = Field(True, description="Run statics analysis")
    run_dynamics: bool = Field(False, description="Run dynamics simulation")
    safety_factor: float = Field(1.5, gt=0.0, description="Design safety factor")


# ---------------------------------------------------------------------------
# Common base for every riser variant
# ---------------------------------------------------------------------------

class _RiserInputBase(BaseModel):
    """Common envelope shared by all unified riser inputs."""

    model_config = {"extra": "forbid"}

    name: str = Field("riser", description="Model / case name")
    geometry: GeometryBlock = Field(default_factory=GeometryBlock)
    material: MaterialBlock = Field(default_factory=MaterialBlock)
    environment: EnvironmentBlock = Field(default_factory=EnvironmentBlock)
    analysis: AnalysisBlock = Field(default_factory=AnalysisBlock)


# ---------------------------------------------------------------------------
# Per-type variants (discriminated on ``riser_type``)
# ---------------------------------------------------------------------------

class SCRInput(_RiserInputBase):
    """Steel Catenary Riser unified input."""

    riser_type: Literal[UnifiedRiserType.SCR] = UnifiedRiserType.SCR
    hang_off_angle_from_vertical: float = Field(
        12.0, ge=0.0, le=30.0, description="Hang-off angle from vertical (deg)"
    )
    min_tdp_offset: float = Field(
        200.0, ge=0.0, description="Minimum TDP offset from vessel (m)"
    )

    def to_legacy(self) -> SCRDesignInput:
        """Adapt to legacy ``SCRDesignInput``."""
        return SCRDesignInput(
            water_depth=self.geometry.water_depth,
            pipe=self.material.to_pipe_properties(),
            hang_off_angle_from_vertical=self.hang_off_angle_from_vertical,
            vessel_draft=self.geometry.vessel_draft,
            hang_off_elevation=self.geometry.hang_off_elevation,
            min_tdp_offset=self.min_tdp_offset,
        )


class _LazyWaveLikeInput(_RiserInputBase):
    """Shared lazy-wave geometry for SLWR and flexible risers."""

    buoyancy_od: float = Field(0.65, gt=0.0, description="Buoyancy module OD (m)")
    buoyancy_density: float = Field(
        450.0, gt=0.0, description="Buoyancy module effective density (kg/m^3)"
    )
    hog_bend_depth: float = Field(
        800.0, gt=0.0, description="Hog-bend target depth below MSL (m)"
    )
    sag_bend_clearance: float = Field(
        50.0, ge=0.0, description="Min clearance above seabed at sag-bend (m)"
    )
    hang_off_angle_deg: float = Field(
        8.0, ge=0.0, description="Hang-off angle from vertical (deg)"
    )

    def to_legacy(self) -> LazyWaveDesignInput:
        """Adapt to legacy ``LazyWaveDesignInput``."""
        return LazyWaveDesignInput(
            water_depth=self.geometry.water_depth,
            pipe=self.material.to_pipe_properties(),
            buoyancy_od=self.buoyancy_od,
            buoyancy_density=self.buoyancy_density,
            hog_bend_depth=self.hog_bend_depth,
            sag_bend_clearance=self.sag_bend_clearance,
            hang_off_angle_deg=self.hang_off_angle_deg,
        )


class SLWRInput(_LazyWaveLikeInput):
    """Steel Lazy-Wave Riser unified input."""

    riser_type: Literal[UnifiedRiserType.SLWR] = UnifiedRiserType.SLWR


class FlexibleInput(_LazyWaveLikeInput):
    """Flexible riser unified input (lazy-wave style geometry).

    Adds a bend-stiffener flag; maps onto the lazy-wave legacy model for
    geometry sizing.
    """

    riser_type: Literal[UnifiedRiserType.FLEXIBLE] = UnifiedRiserType.FLEXIBLE
    min_bend_radius: float = Field(
        4.0, gt=0.0, description="Manufacturer minimum bend radius (m)"
    )
    bend_stiffener: bool = Field(True, description="Bend stiffener fitted at hang-off")


class _TopTensionedInput(_RiserInputBase):
    """Shared top-tensioned geometry for TTR and drilling risers."""

    vessel_heave_amplitude: float = Field(
        3.0, ge=0.0, description="Vessel heave amplitude (m)"
    )
    vessel_pitch_amplitude_deg: float = Field(
        5.0, ge=0.0, description="Vessel pitch amplitude (deg)"
    )
    tensioner_offset: float = Field(
        20.0, ge=0.0, description="Tensioner radial offset from CL (m)"
    )
    overpull_pct: float = Field(
        50.0, ge=0.0, description="Overpull as percentage of riser weight (%)"
    )

    def to_legacy(self) -> TTRDesignInput:
        """Adapt to legacy ``TTRDesignInput``."""
        return TTRDesignInput(
            water_depth=self.geometry.water_depth,
            pipe=self.material.to_pipe_properties(),
            vessel_heave_amplitude=self.vessel_heave_amplitude,
            vessel_pitch_amplitude_deg=self.vessel_pitch_amplitude_deg,
            tensioner_offset=self.tensioner_offset,
            overpull_pct=self.overpull_pct,
        )


class TTRInput(_TopTensionedInput):
    """Top-Tensioned Riser unified input."""

    riser_type: Literal[UnifiedRiserType.TTR] = UnifiedRiserType.TTR


class DrillingInput(_TopTensionedInput):
    """Drilling riser unified input (top-tensioned variant).

    Adds drilling-specific mud parameters; maps onto the TTR legacy model for
    stroke / tension estimation.
    """

    riser_type: Literal[UnifiedRiserType.DRILLING] = UnifiedRiserType.DRILLING
    mud_density: float = Field(
        1500.0, gt=0.0, description="Drilling mud density (kg/m^3)"
    )
    dynamic_factor: float = Field(
        1.25, gt=0.0, description="Dynamic safety factor on top tension"
    )


# ---------------------------------------------------------------------------
# Discriminated union + loader
# ---------------------------------------------------------------------------

RiserInput = Annotated[
    Union[SCRInput, SLWRInput, TTRInput, DrillingInput, FlexibleInput],
    Field(discriminator="riser_type"),
]
"""The single canonical unified riser input type (a discriminated union)."""


# Validator wrapper so callers can write ``RiserInputModel.validate_python(...)``
# style usage and ``from_yaml`` without importing TypeAdapter themselves.
from pydantic import TypeAdapter  # noqa: E402

_RISER_INPUT_ADAPTER: TypeAdapter = TypeAdapter(RiserInput)


def validate_riser_input(data: dict) -> "RiserInputUnion":
    """Validate a plain dict into the correct riser-input variant.

    Raises ``pydantic.ValidationError`` on invalid / missing-discriminator
    input.
    """
    return _RISER_INPUT_ADAPTER.validate_python(data)


def load_riser_input(path: str | Path) -> "RiserInputUnion":
    """Load and validate a unified riser ``input.yml`` from disk.

    Args:
        path: Path to a YAML file with at least a ``riser_type`` key.

    Returns:
        The validated riser-input variant (SCRInput / SLWRInput / ...).

    Raises:
        pydantic.ValidationError: invalid or incomplete input.
        FileNotFoundError: missing file.
    """
    path = Path(path)
    with open(path) as f:
        data = yaml.safe_load(f)
    if not isinstance(data, dict):
        raise ValueError(f"Riser input {path} must be a YAML mapping, got {type(data).__name__}")
    return validate_riser_input(data)


# Concrete union alias for type hints / isinstance-style checks.
RiserInputUnion = Union[SCRInput, SLWRInput, TTRInput, DrillingInput, FlexibleInput]


__all__ = [
    "UnifiedRiserType",
    "GeometryBlock",
    "MaterialBlock",
    "EnvironmentBlock",
    "AnalysisBlock",
    "SCRInput",
    "SLWRInput",
    "TTRInput",
    "DrillingInput",
    "FlexibleInput",
    "RiserInput",
    "RiserInputUnion",
    "validate_riser_input",
    "load_riser_input",
]
