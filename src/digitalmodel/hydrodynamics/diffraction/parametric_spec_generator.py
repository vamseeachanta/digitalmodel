"""Bridge module connecting hull_library to DiffractionSpec pipeline.

ABOUTME: Converts HullProfile objects from the hull_library into validated
DiffractionSpec spec.yml files for BEM solver input. Handles the full chain:
hull profile -> panel mesh -> GDF export -> mass/inertia estimation ->
DiffractionSpec assembly -> YAML output. Supports single-hull and batch
parametric generation via HullParametricSpace.

Key gaps bridged:
- MESH FILE: in-memory PanelMesh -> on-disk .gdf via export_scaled_gdf
- MASS/INERTIA: empirical naval architecture formulae from hull dimensions
- ENVIRONMENT/FREQUENCIES/HEADINGS: configurable with sensible defaults
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import Iterator, Optional

import numpy as np
from pydantic import BaseModel, Field

from digitalmodel.hydrodynamics.diffraction.input_schemas import (
    DiffractionSpec,
    EnvironmentSpec,
    FrequencyInputType,
    FrequencyRangeSpec,
    FrequencySpec,
    HeadingRangeSpec,
    MeshFormatType,
    SymmetryType,
    VesselGeometry,
    VesselInertia,
    VesselSpec,
    WaveHeadingSpec,
)
from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
    HullMeshGenerator,
    MeshGeneratorConfig,
)
from digitalmodel.hydrodynamics.hull_library.mesh_scaler import (
    export_scaled_gdf,
)
from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
)

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

RHO_SEAWATER = 1025.0  # kg/m^3
GRAVITY = 9.80665  # m/s^2

# Empirical radii of gyration coefficients for ship-type hulls
KXX_COEFF = 0.34  # Kxx ~ 0.34 * B
KYY_COEFF = 0.25  # Kyy ~ 0.25 * L
KZZ_COEFF = 0.26  # Kzz ~ 0.26 * L


# ---------------------------------------------------------------------------
# Configuration model
# ---------------------------------------------------------------------------


class SpecGeneratorConfig(BaseModel):
    """Configuration for parametric spec generation.

    Controls mesh generation, environment, frequency range, and heading
    range with sensible defaults for typical ship-type diffraction analysis.
    """

    # Mesh generation
    mesh_config: MeshGeneratorConfig = Field(
        default_factory=MeshGeneratorConfig,
        description="Panel mesh generation settings",
    )

    # Environment defaults
    water_depth: float | str = Field(
        default="infinite",
        description="Water depth in metres, or 'infinite' for deep water",
    )
    water_density: float = Field(
        default=RHO_SEAWATER,
        description="Water density in kg/m^3",
    )
    gravity: float = Field(
        default=GRAVITY,
        description="Gravitational acceleration in m/s^2",
    )

    # Frequency range defaults (rad/s)
    freq_start: float = Field(
        default=0.2, gt=0, description="Start frequency in rad/s"
    )
    freq_end: float = Field(
        default=2.0, gt=0, description="End frequency in rad/s"
    )
    freq_count: int = Field(
        default=30, gt=0, description="Number of frequency steps"
    )

    # Heading range defaults (degrees)
    heading_start: float = Field(
        default=0.0, description="Start heading in degrees"
    )
    heading_end: float = Field(
        default=180.0, description="End heading in degrees"
    )
    heading_increment: float = Field(
        default=15.0, gt=0, description="Heading step in degrees"
    )

    # Inertia overrides (None = use empirical estimation)
    cog_override: Optional[list[float]] = Field(
        None, description="Override centre of gravity [x, y, z] in metres"
    )
    radii_override: Optional[list[float]] = Field(
        None, description="Override radii of gyration [kxx, kyy, kzz] in metres"
    )
    mass_override: Optional[float] = Field(
        None, gt=0, description="Override mass in kg"
    )


# ---------------------------------------------------------------------------
# Mass / inertia estimation
# ---------------------------------------------------------------------------


def estimate_mass(profile: HullProfile, rho: float = RHO_SEAWATER) -> float:
    """Estimate vessel mass from hull profile.

    Uses displacement in tonnes if available on the profile; otherwise
    falls back to Cb * L * B * T * rho.  When block_coefficient is also
    missing, a default Cb of 0.65 (typical full-form vessel) is used.

    Args:
        profile: Hull profile with principal dimensions.
        rho: Water density in kg/m^3.

    Returns:
        Estimated mass in kg.
    """
    if profile.displacement is not None and profile.displacement > 0:
        return profile.displacement * 1000.0  # tonnes -> kg

    cb = profile.block_coefficient if profile.block_coefficient else 0.65
    volume = cb * profile.length_bp * profile.beam * profile.draft
    return volume * rho


def estimate_cog(profile: HullProfile) -> list[float]:
    """Estimate centre of gravity from hull dimensions.

    Simple approximation: x at midships (L/2), y on centreline,
    z at half-draft below waterline.

    Returns:
        [x, y, z] in metres (marine convention: z=0 at waterline).
    """
    return [
        profile.length_bp / 2.0,
        0.0,
        -profile.draft / 2.0,
    ]


def estimate_radii_of_gyration(profile: HullProfile) -> list[float]:
    """Estimate radii of gyration from hull dimensions.

    Uses standard naval architecture empirical coefficients:
    - Kxx ~ 0.34 * B  (roll)
    - Kyy ~ 0.25 * L  (pitch)
    - Kzz ~ 0.26 * L  (yaw)

    Returns:
        [kxx, kyy, kzz] in metres.
    """
    return [
        KXX_COEFF * profile.beam,
        KYY_COEFF * profile.length_bp,
        KZZ_COEFF * profile.length_bp,
    ]


# ---------------------------------------------------------------------------
# Core bridge: HullProfile -> DiffractionSpec
# ---------------------------------------------------------------------------


def generate_diffraction_spec(
    profile: HullProfile,
    output_dir: Path,
    config: Optional[SpecGeneratorConfig] = None,
) -> DiffractionSpec:
    """Convert a HullProfile to a validated DiffractionSpec.

    Pipeline:
        1. Generate PanelMesh via HullMeshGenerator.
        2. Export mesh to GDF file on disk.
        3. Estimate mass, CoG, and radii of gyration.
        4. Build VesselSpec + EnvironmentSpec + FrequencySpec + WaveHeadingSpec.
        5. Assemble and return DiffractionSpec.

    Args:
        profile: Hull profile to convert.
        output_dir: Directory for mesh GDF files and spec output.
        config: Generation configuration. Uses defaults if None.

    Returns:
        Validated DiffractionSpec ready for to_yaml() serialisation.
    """
    if config is None:
        config = SpecGeneratorConfig()

    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    # 1. Generate panel mesh
    mesh_gen = HullMeshGenerator()
    mesh = mesh_gen.generate(profile, config.mesh_config)
    logger.info(
        "Generated mesh for '%s': %d panels", profile.name, mesh.n_panels
    )

    # 2. Export to GDF
    gdf_path = output_dir / f"{profile.name}.gdf"
    export_scaled_gdf(mesh, gdf_path)
    logger.info("Exported GDF: %s", gdf_path)

    # 3. Estimate mass / inertia
    rho = config.water_density
    mass = config.mass_override or estimate_mass(profile, rho)
    cog = config.cog_override or estimate_cog(profile)
    radii = config.radii_override or estimate_radii_of_gyration(profile)

    # 4. Build sub-specs
    # Determine symmetry from mesh
    symmetry = SymmetryType.NONE
    if mesh.symmetry_plane and "y" in (mesh.symmetry_plane or "").lower():
        symmetry = SymmetryType.XZ

    vessel_geometry = VesselGeometry(
        mesh_file=gdf_path.name,
        mesh_format=MeshFormatType.GDF,
        symmetry=symmetry,
        reference_point=[0.0, 0.0, 0.0],
        waterline_z=0.0,
    )

    vessel_inertia = VesselInertia(
        mode="free_floating",
        mass=mass,
        centre_of_gravity=cog,
        radii_of_gyration=radii,
        cog_z=cog[2],
    )

    vessel_spec = VesselSpec(
        name=profile.name,
        type=profile.hull_type.value,
        geometry=vessel_geometry,
        inertia=vessel_inertia,
    )

    environment = EnvironmentSpec(
        water_depth=config.water_depth,
        water_density=config.water_density,
        gravity=config.gravity,
    )

    frequencies = FrequencySpec(
        input_type=FrequencyInputType.FREQUENCY,
        range=FrequencyRangeSpec(
            start=config.freq_start,
            end=config.freq_end,
            count=config.freq_count,
        ),
    )

    wave_headings = WaveHeadingSpec(
        range=HeadingRangeSpec(
            start=config.heading_start,
            end=config.heading_end,
            increment=config.heading_increment,
        ),
        symmetry=True,
    )

    # 5. Assemble DiffractionSpec
    spec = DiffractionSpec(
        vessel=vessel_spec,
        environment=environment,
        frequencies=frequencies,
        wave_headings=wave_headings,
    )

    logger.info(
        "Built DiffractionSpec for '%s': mass=%.0f kg, "
        "CoG=%s, radii=%s",
        profile.name,
        mass,
        cog,
        radii,
    )

    return spec


def generate_spec_yaml(
    profile: HullProfile,
    output_dir: Path,
    config: Optional[SpecGeneratorConfig] = None,
) -> Path:
    """Generate a DiffractionSpec and write it as spec.yml.

    Convenience wrapper that calls generate_diffraction_spec() then
    serialises the result to YAML.

    Args:
        profile: Hull profile to convert.
        output_dir: Directory for output files.
        config: Generation configuration.

    Returns:
        Path to the written spec.yml file.
    """
    spec = generate_diffraction_spec(profile, output_dir, config)
    yaml_path = Path(output_dir) / f"{profile.name}_spec.yml"
    spec.to_yaml(yaml_path)
    logger.info("Wrote spec YAML: %s", yaml_path)
    return yaml_path


# ---------------------------------------------------------------------------
# Batch mode via HullParametricSpace
# ---------------------------------------------------------------------------


def generate_batch_specs(
    parametric_space: "HullParametricSpace",
    catalog: "HullCatalog",
    output_dir: Path,
    config: Optional[SpecGeneratorConfig] = None,
) -> Iterator[tuple[str, Path]]:
    """Generate DiffractionSpec YAML files for all parametric hull variants.

    Iterates over all combinations in the parametric space, scales the
    base hull from the catalog, and generates a spec.yml for each variant.

    Args:
        parametric_space: HullParametricSpace defining dimension ranges.
        catalog: HullCatalog containing the base hull.
        output_dir: Root output directory; each variant gets a subdirectory.
        config: Generation configuration (shared across all variants).

    Yields:
        (variation_id, spec_yaml_path) tuples.
    """
    from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
        HullParametricSpace,
    )

    output_dir = Path(output_dir)

    for variation_id, profile in parametric_space.generate_profiles(catalog):
        variant_dir = output_dir / variation_id
        variant_dir.mkdir(parents=True, exist_ok=True)

        yaml_path = generate_spec_yaml(profile, variant_dir, config)
        logger.info(
            "Batch spec: %s -> %s", variation_id, yaml_path
        )
        yield variation_id, yaml_path


def generate_batch_diffraction_specs(
    parametric_space: "HullParametricSpace",
    catalog: "HullCatalog",
    output_dir: Path,
    config: Optional[SpecGeneratorConfig] = None,
) -> Iterator[tuple[str, DiffractionSpec]]:
    """Generate DiffractionSpec objects (in-memory) for all parametric variants.

    Like generate_batch_specs but yields the DiffractionSpec objects
    instead of writing YAML files, useful for programmatic downstream
    processing.

    Args:
        parametric_space: HullParametricSpace defining dimension ranges.
        catalog: HullCatalog containing the base hull.
        output_dir: Root output directory for mesh GDF files.
        config: Generation configuration (shared across all variants).

    Yields:
        (variation_id, DiffractionSpec) tuples.
    """
    output_dir = Path(output_dir)

    for variation_id, profile in parametric_space.generate_profiles(catalog):
        variant_dir = output_dir / variation_id
        variant_dir.mkdir(parents=True, exist_ok=True)

        spec = generate_diffraction_spec(profile, variant_dir, config)
        yield variation_id, spec


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

__all__ = [
    "SpecGeneratorConfig",
    "estimate_mass",
    "estimate_cog",
    "estimate_radii_of_gyration",
    "generate_diffraction_spec",
    "generate_spec_yaml",
    "generate_batch_specs",
    "generate_batch_diffraction_specs",
]
