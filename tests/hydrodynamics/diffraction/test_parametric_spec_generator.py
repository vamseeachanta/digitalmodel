"""Tests for the parametric spec bridge (hull_library -> DiffractionSpec).

ABOUTME: Validates the bridge module that converts HullProfile objects to
DiffractionSpec spec.yml files, including mass/inertia estimation,
mesh generation + GDF export, and batch parametric generation.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.input_schemas import (
    DiffractionSpec,
)
from digitalmodel.hydrodynamics.diffraction.parametric_spec_generator import (
    SpecGeneratorConfig,
    estimate_cog,
    estimate_mass,
    estimate_radii_of_gyration,
    generate_batch_specs,
    generate_diffraction_spec,
    generate_spec_yaml,
)
from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
    HullStation,
    HullType,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_barge_profile(
    name: str = "test_barge",
    length: float = 100.0,
    beam: float = 20.0,
    draft: float = 5.0,
    depth: float = 8.0,
    cb: float = 0.85,
    displacement: float | None = None,
) -> HullProfile:
    """Create a simple rectangular barge HullProfile for testing."""
    half_beam = beam / 2.0
    stations = []
    n_stations = 5
    for i in range(n_stations):
        x = i * length / (n_stations - 1)
        stations.append(
            HullStation(
                x_position=x,
                waterline_offsets=[
                    (0.0, half_beam),
                    (draft, half_beam),
                    (depth, half_beam),
                ],
            )
        )
    return HullProfile(
        name=name,
        hull_type=HullType.BARGE,
        stations=stations,
        length_bp=length,
        beam=beam,
        draft=draft,
        depth=depth,
        source="test_fixture",
        block_coefficient=cb,
        displacement=displacement,
    )


# ---------------------------------------------------------------------------
# Mass / inertia estimation tests
# ---------------------------------------------------------------------------


class TestMassEstimation:
    """Tests for estimate_mass function."""

    def test_mass_from_displacement(self):
        """When displacement is given, mass = displacement * 1000."""
        profile = _make_barge_profile(displacement=10000.0)
        mass = estimate_mass(profile)
        assert mass == pytest.approx(10_000_000.0)

    def test_mass_from_cb(self):
        """When no displacement, mass = Cb * L * B * T * rho."""
        profile = _make_barge_profile(
            length=100.0, beam=20.0, draft=5.0, cb=0.85
        )
        expected = 0.85 * 100.0 * 20.0 * 5.0 * 1025.0
        mass = estimate_mass(profile)
        assert mass == pytest.approx(expected)

    def test_mass_fallback_cb(self):
        """When neither displacement nor Cb is given, default Cb=0.65."""
        profile = _make_barge_profile(
            length=100.0, beam=20.0, draft=5.0, cb=0.85
        )
        # Remove block_coefficient by constructing without it
        profile_no_cb = profile.model_copy(
            update={"block_coefficient": None, "displacement": None}
        )
        expected = 0.65 * 100.0 * 20.0 * 5.0 * 1025.0
        mass = estimate_mass(profile_no_cb)
        assert mass == pytest.approx(expected)

    def test_mass_custom_density(self):
        """Custom water density is respected."""
        profile = _make_barge_profile(cb=0.85)
        mass = estimate_mass(profile, rho=1000.0)
        expected = 0.85 * 100.0 * 20.0 * 5.0 * 1000.0
        assert mass == pytest.approx(expected)


class TestCoGEstimation:
    """Tests for estimate_cog function."""

    def test_cog_values(self):
        profile = _make_barge_profile(length=100.0, draft=5.0)
        cog = estimate_cog(profile)
        assert len(cog) == 3
        assert cog[0] == pytest.approx(50.0)  # L/2
        assert cog[1] == pytest.approx(0.0)   # centreline
        assert cog[2] == pytest.approx(-2.5)  # -T/2


class TestRadiiEstimation:
    """Tests for estimate_radii_of_gyration function."""

    def test_radii_values(self):
        profile = _make_barge_profile(length=100.0, beam=20.0)
        radii = estimate_radii_of_gyration(profile)
        assert len(radii) == 3
        assert radii[0] == pytest.approx(0.34 * 20.0)   # Kxx = 0.34*B
        assert radii[1] == pytest.approx(0.25 * 100.0)  # Kyy = 0.25*L
        assert radii[2] == pytest.approx(0.26 * 100.0)  # Kzz = 0.26*L


# ---------------------------------------------------------------------------
# Spec generation tests
# ---------------------------------------------------------------------------


class TestGenerateDiffractionSpec:
    """Tests for generate_diffraction_spec function."""

    def test_produces_valid_spec(self, tmp_path: Path):
        """Generated spec passes Pydantic validation."""
        profile = _make_barge_profile()
        spec = generate_diffraction_spec(profile, tmp_path)
        assert isinstance(spec, DiffractionSpec)

    def test_gdf_file_created(self, tmp_path: Path):
        """GDF mesh file is created on disk."""
        profile = _make_barge_profile()
        generate_diffraction_spec(profile, tmp_path)
        gdf_path = tmp_path / "test_barge.gdf"
        assert gdf_path.exists()
        assert gdf_path.stat().st_size > 0

    def test_vessel_name_matches(self, tmp_path: Path):
        """Vessel name in spec matches the hull profile name."""
        profile = _make_barge_profile(name="my_hull")
        spec = generate_diffraction_spec(profile, tmp_path)
        assert spec.vessel.name == "my_hull"

    def test_vessel_type_matches(self, tmp_path: Path):
        """Vessel type in spec matches the hull type."""
        profile = _make_barge_profile()
        spec = generate_diffraction_spec(profile, tmp_path)
        assert spec.vessel.type == "barge"

    def test_inertia_mode_free_floating(self, tmp_path: Path):
        """Inertia mode is set to free_floating."""
        profile = _make_barge_profile()
        spec = generate_diffraction_spec(profile, tmp_path)
        assert spec.vessel.inertia.mode == "free_floating"

    def test_mass_populates_correctly(self, tmp_path: Path):
        """Mass is estimated from Cb * L * B * T * rho."""
        profile = _make_barge_profile(cb=0.85)
        spec = generate_diffraction_spec(profile, tmp_path)
        expected = 0.85 * 100.0 * 20.0 * 5.0 * 1025.0
        assert spec.vessel.inertia.mass == pytest.approx(expected)

    def test_radii_populates(self, tmp_path: Path):
        """Radii of gyration are populated in the spec."""
        profile = _make_barge_profile()
        spec = generate_diffraction_spec(profile, tmp_path)
        radii = spec.vessel.inertia.radii_of_gyration
        assert radii is not None
        assert len(radii) == 3
        assert all(r > 0 for r in radii)

    def test_environment_defaults(self, tmp_path: Path):
        """Environment has default deep water settings."""
        profile = _make_barge_profile()
        spec = generate_diffraction_spec(profile, tmp_path)
        assert spec.environment.water_depth == "infinite"
        assert spec.environment.water_density == pytest.approx(1025.0)

    def test_frequency_range(self, tmp_path: Path):
        """Frequency range has default values."""
        profile = _make_barge_profile()
        spec = generate_diffraction_spec(profile, tmp_path)
        freqs = spec.frequencies.to_frequencies_rad_s()
        assert len(freqs) == 30
        assert freqs[0] == pytest.approx(0.2)
        assert freqs[-1] == pytest.approx(2.0)

    def test_heading_range(self, tmp_path: Path):
        """Heading range has default values."""
        profile = _make_barge_profile()
        spec = generate_diffraction_spec(profile, tmp_path)
        headings = spec.wave_headings.to_heading_list()
        assert headings[0] == pytest.approx(0.0)
        assert headings[-1] == pytest.approx(180.0)

    def test_custom_config(self, tmp_path: Path):
        """Custom config overrides are respected."""
        config = SpecGeneratorConfig(
            water_depth=50.0,
            freq_start=0.1,
            freq_end=1.5,
            freq_count=20,
            heading_increment=30.0,
            mass_override=5_000_000.0,
            cog_override=[40.0, 0.0, -3.0],
            radii_override=[7.0, 25.0, 27.0],
        )
        profile = _make_barge_profile()
        spec = generate_diffraction_spec(profile, tmp_path, config)

        assert spec.environment.water_depth == pytest.approx(50.0)
        assert spec.vessel.inertia.mass == pytest.approx(5_000_000.0)
        assert spec.vessel.inertia.centre_of_gravity == [40.0, 0.0, -3.0]
        assert spec.vessel.inertia.radii_of_gyration == [7.0, 25.0, 27.0]
        freqs = spec.frequencies.to_frequencies_rad_s()
        assert len(freqs) == 20


class TestGenerateSpecYaml:
    """Tests for generate_spec_yaml (YAML output)."""

    def test_writes_yaml_file(self, tmp_path: Path):
        """spec.yml file is written to disk."""
        profile = _make_barge_profile()
        yaml_path = generate_spec_yaml(profile, tmp_path)
        assert yaml_path.exists()
        assert yaml_path.suffix == ".yml"

    def test_yaml_roundtrip(self, tmp_path: Path):
        """Written YAML can be loaded back as a valid DiffractionSpec."""
        profile = _make_barge_profile()
        yaml_path = generate_spec_yaml(profile, tmp_path)
        reloaded = DiffractionSpec.from_yaml(yaml_path)
        assert reloaded.vessel.name == profile.name


# ---------------------------------------------------------------------------
# Batch mode tests
# ---------------------------------------------------------------------------


class TestBatchGeneration:
    """Tests for batch spec generation via HullParametricSpace."""

    def test_batch_generates_multiple_specs(self, tmp_path: Path):
        """Batch generation yields one spec per parametric combination."""
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace,
            ParametricRange,
        )

        profile = _make_barge_profile(name="base_barge")
        catalog = HullCatalog()
        catalog.register_hull(profile)

        space = HullParametricSpace(
            base_hull_id="base_barge",
            ranges={
                "length_scale": ParametricRange(min=0.8, max=1.2, steps=3),
                "beam_scale": ParametricRange(min=0.9, max=1.1, steps=2),
            },
        )

        results = list(generate_batch_specs(space, catalog, tmp_path))
        assert len(results) == 6  # 3 * 2 combinations
        for variation_id, yaml_path in results:
            assert yaml_path.exists()
            assert yaml_path.suffix == ".yml"
            # Each variant has its own subdirectory
            assert yaml_path.parent.name == variation_id
