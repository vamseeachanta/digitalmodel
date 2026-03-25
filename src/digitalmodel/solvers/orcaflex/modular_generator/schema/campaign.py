"""Campaign models for parametric installation analysis."""
from __future__ import annotations

import logging
from dataclasses import dataclass, field as dc_field
from itertools import product
from pathlib import Path
from typing import Any, Iterator

import yaml
from pydantic import BaseModel, Field, field_validator, model_validator

from .environment import Current, SeabedStiffness, Waves, Wind
from .root import ProjectInputSpec

logger = logging.getLogger(__name__)


class EnvironmentVariation(BaseModel):
    """Named environment configuration for campaign matrix.

    Overrides: spec.environment.waves, spec.environment.current, spec.environment.wind
    Does NOT override: spec.environment.water (use water_depths), spec.environment.seabed (use soils)
    """

    name: str = Field(..., min_length=1)
    waves: Waves
    current: Current
    wind: Wind


class SoilVariation(BaseModel):
    """Named soil/seabed configuration for campaign matrix.

    Overrides: spec.environment.seabed.stiffness, spec.environment.seabed.friction_coefficient,
               and optionally spec.environment.seabed.slope
    """

    name: str = Field(..., min_length=1)
    stiffness: SeabedStiffness
    friction_coefficient: float = Field(..., ge=0, le=1.5)
    slope: float | None = Field(
        default=None, ge=-45, le=45,
        description="Seabed slope override (deg). None = keep base value"
    )


class InstallationSection(BaseModel):
    """Override for a specific include-file section.

    template_path resolved relative to the campaign YAML file's directory.
    """

    builder_file: str = Field(
        ...,
        description="Builder output filename to override (e.g., '08_buoys.yml'). "
                    "Must match a BuilderRegistry entry.",
    )
    template: str | None = Field(
        default=None,
        description="Path to template file (resolved relative to campaign YAML directory)",
    )
    variables: dict[str, Any] = Field(default_factory=dict)
    enabled: bool = Field(default=True)


class CampaignMatrix(BaseModel):
    """Defines the parametric variation space for batch generation."""

    water_depths: list[float] = Field(..., min_length=1, description="Water depths (m)")
    route_lengths: list[float] | None = Field(
        default=None,
        description="Route lengths (m). Adjusts last pipeline segment only.",
    )
    tensions: list[float] | None = Field(
        default=None,
        description="Tension values (kN). S-lay only.",
    )
    environments: list[EnvironmentVariation] | None = None
    soils: list[SoilVariation] | None = None

    @field_validator("water_depths")
    @classmethod
    def validate_water_depths(cls, v: list[float]) -> list[float]:
        for depth in v:
            if depth <= 0:
                raise ValueError(f"Water depth must be positive, got {depth}")
        return v

    def combinations(self) -> list[dict[str, float | str]]:
        """Generate cartesian product of all non-None parameters.

        Returns list of dicts with keys:
          - "water_depth": float (always present)
          - "route_length": float (if route_lengths specified)
          - "tension": float (if tensions specified)
          - "environment": str (name, if environments specified)
          - "soil": str (name, if soils specified)
        """
        axes: list[tuple[str, list]] = [("water_depth", self.water_depths)]

        if self.route_lengths:
            axes.append(("route_length", self.route_lengths))
        if self.tensions:
            axes.append(("tension", self.tensions))
        if self.environments:
            axes.append(("environment", [e.name for e in self.environments]))
        if self.soils:
            axes.append(("soil", [s.name for s in self.soils]))

        keys = [k for k, _ in axes]
        value_lists = [v for _, v in axes]

        return [dict(zip(keys, combo)) for combo in product(*value_lists)]


class CampaignSpec(BaseModel):
    """Top-level model for parametric installation campaign."""

    schema_version: str = Field(default="1.0", description="Schema version for migration")
    base: ProjectInputSpec
    campaign: CampaignMatrix
    sections: list[InstallationSection] = Field(
        default_factory=list,
        description="Optional section overrides",
    )
    output_naming: str = Field(
        default="{base_name}_wd{water_depth}m_{environment}",
        description="Output directory naming template",
    )
    max_runs: int | None = Field(
        default=None, ge=1, description="Maximum runs to generate. None = unlimited."
    )

    @classmethod
    def from_legacy_config(cls, cfg: dict) -> "CampaignSpec":
        """Convert legacy OrcInstallation config to CampaignSpec.

        Maps the legacy delta_elevations-based config to the new
        campaign water_depths format.

        Args:
            cfg: Legacy configuration dict with 'structure' and 'Analysis' keys.

        Returns:
            CampaignSpec with water depths derived from delta_elevations.

        Raises:
            ValueError: If cfg is missing required keys.
        """
        if "structure" not in cfg:
            raise ValueError("Legacy config must contain 'structure' key")

        structure = cfg["structure"]
        if "delta_elevations" not in structure:
            raise ValueError(
                "Legacy config must contain 'structure.delta_elevations'"
            )

        delta_elevations = structure["delta_elevations"]

        # Convert delta_elevations to water_depths.
        # In legacy system, delta_elevation is applied to InitialZ (negative for
        # underwater).  More negative delta = deeper water.
        # Use reference_depth if provided, otherwise use absolute values.
        reference_depth = structure.get("reference_depth", 0)
        water_depths = [reference_depth - de for de in delta_elevations]
        # Ensure all positive
        water_depths = [abs(d) if d != 0 else 0.1 for d in water_depths]
        # Filter out zero/negative
        water_depths = [d for d in water_depths if d > 0]

        if not water_depths:
            raise ValueError(
                "No valid water depths derived from delta_elevations"
            )

        # Build minimal base spec with reasonable defaults
        base_name = structure.get("BaseFile", "legacy_model")
        base_data = {
            "metadata": {
                "name": base_name,
                "description": "Converted from legacy config",
                "structure": "pipeline",
                "operation": "installation/floating",
                "project": "legacy",
            },
            "environment": {
                "water": {"depth": water_depths[0], "density": 1.025},
                "seabed": {
                    "slope": 0,
                    "stiffness": {"normal": 100, "shear": 10},
                },
                "waves": {
                    "type": "jonswap",
                    "height": 1.5,
                    "period": 6,
                    "direction": 180,
                },
                "current": {"speed": 0.5, "direction": 270},
                "wind": {"speed": 5, "direction": 270},
            },
            "pipeline": {
                "name": base_name,
                "material": "X65",
                "dimensions": {
                    "outer_diameter": 0.5,
                    "wall_thickness": 0.02,
                },
                "coatings": {
                    "corrosion": {"thickness": 0.004, "density": 0.95},
                    "weight": [
                        {"name": "CWC", "thickness": 0.1, "density": 3.0},
                    ],
                },
                "segments": [
                    {
                        "type": "X65+CWC",
                        "length": 5000,
                        "segment_length": 10,
                    },
                ],
            },
            "equipment": {
                "tugs": {
                    "count": 3,
                    "spacing": 500,
                    "first_position": [500, -20, 0],
                    "properties": {"mass": 30, "volume": 100},
                },
                "buoyancy_modules": {
                    "spacing": 4,
                    "properties": {"volume": 4.91},
                },
            },
            "simulation": {"time_step": 0.1, "stages": [8, 16]},
        }

        base_spec = ProjectInputSpec(**base_data)

        return cls(
            base=base_spec,
            campaign=CampaignMatrix(water_depths=water_depths),
            output_naming="{base_name}_wd{water_depth}m",
        )

    @model_validator(mode="after")
    def validate_installation_type_constraints(self) -> "CampaignSpec":
        """Cross-validate campaign parameters against installation type."""
        if self.campaign.tensions and not self.base.is_s_lay():
            raise ValueError(
                "tensions can only be specified for S-lay models "
                "(base spec must define equipment.vessel)"
            )
        return self

    @model_validator(mode="after")
    def validate_output_naming_coverage(self) -> "CampaignSpec":
        """Warn if varying parameters are not in output_naming template."""
        varying = []
        if self.campaign.water_depths:
            varying.append("water_depth")
        if self.campaign.route_lengths:
            varying.append("route_length")
        if self.campaign.tensions:
            varying.append("tension")
        if self.campaign.environments:
            varying.append("environment")
        if self.campaign.soils:
            varying.append("soil")
        for param in varying:
            if f"{{{param}}}" not in self.output_naming:
                raise ValueError(
                    f"Parameter '{param}' varies in campaign but is missing from "
                    f"output_naming template: '{self.output_naming}'"
                )
        return self

    def generate_run_specs(self) -> Iterator[tuple[str, ProjectInputSpec]]:
        """Yield (name, modified_spec) pairs. Streaming - one at a time.

        Uses model_copy(deep=True) for Pydantic v2 optimized copy.
        """
        combos = self.campaign.combinations()
        limit = self.max_runs if self.max_runs else len(combos)

        for i, combo in enumerate(combos):
            if i >= limit:
                break
            spec = self.base.model_copy(deep=True)
            _apply_overrides(spec, combo, self.campaign)
            name = self.output_naming.format(
                base_name=spec.metadata.name, **combo
            )
            yield (name, spec)


def _apply_overrides(
    spec: ProjectInputSpec,
    combo: dict[str, float | str],
    matrix: CampaignMatrix,
) -> None:
    """Apply parameter overrides to a deep-copied spec."""
    if "water_depth" in combo:
        old_depth = spec.environment.water.depth
        new_depth = combo["water_depth"]
        spec.environment.water.depth = new_depth

        # Scale current profile depths proportionally
        if old_depth > 0 and spec.environment.current.profile:
            scale = new_depth / old_depth
            spec.environment.current.profile = [
                [p[0] * scale, p[1]] for p in spec.environment.current.profile
            ]

    if "route_length" in combo:
        new_total = combo["route_length"]
        segments = spec.pipeline.segments
        current_total = sum(s.length for s in segments)
        delta = new_total - current_total
        last_seg = segments[-1]
        new_last_length = last_seg.length + delta
        if new_last_length <= 0:
            raise ValueError(
                f"Route length {new_total}m makes last segment length "
                f"{new_last_length}m (<=0). Cannot adjust."
            )
        last_seg.length = new_last_length

    if "tension" in combo:
        if spec.equipment.tensioner is None:
            raise ValueError("Cannot apply tension override: no tensioner in spec")
        spec.equipment.tensioner.tension_value = combo["tension"]

    if "environment" in combo and matrix.environments:
        env_name = combo["environment"]
        env_var = next(e for e in matrix.environments if e.name == env_name)
        spec.environment.waves = env_var.waves
        spec.environment.current = env_var.current
        spec.environment.wind = env_var.wind

    if "soil" in combo and matrix.soils:
        soil_name = combo["soil"]
        soil_var = next(s for s in matrix.soils if s.name == soil_name)
        spec.environment.seabed.stiffness = soil_var.stiffness
        spec.environment.seabed.friction_coefficient = soil_var.friction_coefficient
        if soil_var.slope is not None:
            spec.environment.seabed.slope = soil_var.slope


@dataclass
class CampaignResult:
    """Aggregated result from campaign generation."""

    run_count: int
    skipped_count: int
    run_dirs: list[Path]
    errors: list[str]
    matrix_summary: dict[str, list]


class CampaignGenerator:
    """Generates parametric campaign of OrcaFlex models.

    Loads a campaign YAML file, validates it against CampaignSpec,
    and generates one OrcaFlex run directory per parameter combination.
    """

    def __init__(self, campaign_file: Path):
        self.campaign_file = Path(campaign_file)
        data = yaml.safe_load(self.campaign_file.read_text())
        self.spec = CampaignSpec(**data)

    def preview(self) -> list[dict[str, float | str]]:
        """Dry-run: returns parameter combinations without generating files."""
        return self.spec.campaign.combinations()

    def validate(self) -> list[str]:
        """Validate campaign consistency. Returns list of warnings."""
        warnings: list[str] = []
        combos = self.preview()

        # Check for duplicate run names after template formatting
        names: set[str] = set()
        for combo in combos:
            name = self.spec.output_naming.format(
                base_name=self.spec.base.metadata.name, **combo
            )
            if name in names:
                warnings.append(f"Duplicate run name: '{name}'")
            names.add(name)

        # Environment name uniqueness
        if self.spec.campaign.environments:
            env_names = [e.name for e in self.spec.campaign.environments]
            if len(env_names) != len(set(env_names)):
                warnings.append("Duplicate environment names found")

        # Soil name uniqueness
        if self.spec.campaign.soils:
            soil_names = [s.name for s in self.spec.campaign.soils]
            if len(soil_names) != len(set(soil_names)):
                warnings.append("Duplicate soil names found")

        return warnings

    def generate(
        self,
        output_dir: Path,
        force: bool = False,
        resume: bool = False,
    ) -> CampaignResult:
        """Generate all campaign runs.

        Streams through the matrix one run at a time.

        Args:
            output_dir: Root directory for all generated run directories.
            force: If True, overwrite existing run directories.
            resume: If True, skip runs where master.yml already exists.

        Returns:
            CampaignResult with aggregated statistics.
        """
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        run_dirs: list[Path] = []
        errors: list[str] = []
        skipped = 0

        for name, spec in self.spec.generate_run_specs():
            run_dir = output_dir / name

            if run_dir.exists():
                if resume and (run_dir / "master.yml").exists():
                    logger.info("Skipping (resume): %s", name)
                    skipped += 1
                    continue
                elif not force:
                    logger.warning("Skipping existing directory: %s", name)
                    skipped += 1
                    continue

            try:
                # Import here to avoid circular imports
                from digitalmodel.solvers.orcaflex.modular_generator import (
                    ModularModelGenerator,
                )

                gen = ModularModelGenerator.from_spec(spec)

                # Use generate_with_overrides if campaign has sections
                if self.spec.sections:
                    gen.generate_with_overrides(
                        output_dir=run_dir,
                        sections=self.spec.sections,
                        variables={"water_depth": spec.environment.water.depth},
                        template_base_dir=self.campaign_file.parent,
                    )
                else:
                    gen.generate(run_dir)

                run_dirs.append(run_dir)
                logger.info("Generated: %s", name)

            except Exception as exc:
                msg = f"Failed to generate run '{name}': {exc}"
                logger.error(msg)
                errors.append(msg)

        # Build matrix summary
        combos = self.preview()
        matrix_summary: dict[str, list] = {}
        if combos:
            for key in combos[0]:
                matrix_summary[key] = sorted(
                    set(c[key] for c in combos), key=str
                )

        return CampaignResult(
            run_count=len(run_dirs),
            skipped_count=skipped,
            run_dirs=run_dirs,
            errors=errors,
            matrix_summary=matrix_summary,
        )
