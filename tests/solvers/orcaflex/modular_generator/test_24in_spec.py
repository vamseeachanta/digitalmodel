"""Tests for 24in pipeline spec validation and generation.

Validates the 24-inch pipeline floating installation spec at:
    docs/domains/orcaflex/pipeline/installation/floating/24in_pipeline/spec.yml

NOTE: The directory is named '24in_pipeline' but the actual steel pipe OD is
0.762m (30-inch NPS per API 5L). This is a legacy naming discrepancy in the
original project data. All test values reflect the physical spec.
"""
from __future__ import annotations

import copy
from pathlib import Path

import pytest
import yaml

SPEC_DIR = Path(__file__).parent.parent.parent.parent.parent / (
    "docs/domains/orcaflex/pipeline/installation/floating/24in_pipeline"
)
SPEC_FILE = SPEC_DIR / "spec.yml"


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def spec_data_24in() -> dict:
    """Load raw YAML data from the 24in pipeline spec."""
    with open(SPEC_FILE) as f:
        return yaml.safe_load(f)


@pytest.fixture
def validated_spec_24in(spec_data_24in):
    """Load and validate the 24in pipeline spec via Pydantic."""
    from digitalmodel.solvers.orcaflex.modular_generator.schema import (
        ProjectInputSpec,
    )

    return ProjectInputSpec(**spec_data_24in)


# ===================================================================
# 1. Spec existence and loading (~3 tests)
# ===================================================================


class TestSpecLoading:
    """Verify spec file exists, loads as valid YAML, and validates."""

    def test_spec_file_exists(self):
        """Spec file exists on disk."""
        assert SPEC_FILE.exists(), f"Spec file not found: {SPEC_FILE}"

    def test_spec_loads_as_valid_yaml(self, spec_data_24in):
        """Spec file loads as a non-empty YAML dictionary."""
        assert isinstance(spec_data_24in, dict)
        assert len(spec_data_24in) > 0

    def test_spec_validates_against_schema(self, spec_data_24in):
        """Spec data validates against ProjectInputSpec without errors."""
        from digitalmodel.solvers.orcaflex.modular_generator.schema import (
            ProjectInputSpec,
        )

        spec = ProjectInputSpec(**spec_data_24in)
        assert spec is not None


# ===================================================================
# 2. Metadata validation (~2 tests)
# ===================================================================


class TestMetadata:
    """Validate metadata fields."""

    def test_metadata_name(self, validated_spec_24in):
        """Metadata name matches expected value."""
        assert validated_spec_24in.metadata.name == "24in_pipeline_installation"

    def test_metadata_operation(self, validated_spec_24in):
        """Metadata operation is installation/floating."""
        assert validated_spec_24in.metadata.operation == "installation/floating"


# ===================================================================
# 3. Environment validation (~5 tests)
# ===================================================================


class TestEnvironment:
    """Validate environmental parameters."""

    def test_water_depth(self, validated_spec_24in):
        """Water depth is 8 m."""
        assert validated_spec_24in.environment.water.depth == pytest.approx(8)

    def test_water_density(self, validated_spec_24in):
        """Water density is 1.03 te/m3."""
        assert validated_spec_24in.environment.water.density == pytest.approx(1.03)

    def test_seabed_slope(self, validated_spec_24in):
        """Seabed slope is 0.57 degrees."""
        assert validated_spec_24in.environment.seabed.slope == pytest.approx(0.57)

    def test_wave_height_calm_sea(self, validated_spec_24in):
        """Wave height is 0 (calm sea condition)."""
        assert validated_spec_24in.environment.waves.height == pytest.approx(0)

    def test_current_speed_and_direction(self, validated_spec_24in):
        """Current speed is 1 m/s, direction is 270 deg."""
        assert validated_spec_24in.environment.current.speed == pytest.approx(1)
        assert validated_spec_24in.environment.current.direction == pytest.approx(270)

    def test_wind_speed_and_direction(self, validated_spec_24in):
        """Wind speed is 8.87 m/s, direction is 270 deg."""
        assert validated_spec_24in.environment.wind.speed == pytest.approx(8.87)
        assert validated_spec_24in.environment.wind.direction == pytest.approx(270)


# ===================================================================
# 4. Pipeline validation (~5 tests)
# ===================================================================


class TestPipeline:
    """Validate pipeline definition."""

    def test_pipeline_name(self, validated_spec_24in):
        """Pipeline name is 'pipeline'."""
        assert validated_spec_24in.pipeline.name == "pipeline"

    def test_pipeline_od_and_wt(self, validated_spec_24in):
        """OD is 0.762 m (30in NPS) and WT is 0.0254 m."""
        dims = validated_spec_24in.pipeline.dimensions
        assert dims.outer_diameter == pytest.approx(0.762)
        assert dims.wall_thickness == pytest.approx(0.0254)

    def test_corrosion_coating(self, validated_spec_24in):
        """Corrosion coating: thickness 0.0035 m, density 1.1 te/m3."""
        corr = validated_spec_24in.pipeline.coatings.corrosion
        assert corr.thickness == pytest.approx(0.0035)
        assert corr.density == pytest.approx(1.1)

    def test_weight_coating_cwc80(self, validated_spec_24in):
        """Single weight coating CWC80: thickness 0.08 m, density 3.0 te/m3."""
        weight = validated_spec_24in.pipeline.coatings.weight
        assert len(weight) == 1
        assert weight[0].name == "CWC80"
        assert weight[0].thickness == pytest.approx(0.08)
        assert weight[0].density == pytest.approx(3.0)

    def test_single_segment(self, validated_spec_24in):
        """Single segment: type X65+3LPP+CWC80, length 4900 m."""
        segments = validated_spec_24in.pipeline.segments
        assert len(segments) == 1
        assert segments[0].type == "X65+3LPP+CWC80"
        assert segments[0].length == pytest.approx(4900)


# ===================================================================
# 5. Equipment validation (~5 tests)
# ===================================================================


class TestEquipment:
    """Validate equipment configuration."""

    def test_tugs_count_and_spacing(self, validated_spec_24in):
        """5 tugs with 816.6 m spacing."""
        tugs = validated_spec_24in.equipment.tugs
        assert tugs.count == 5
        assert tugs.spacing == pytest.approx(816.6)

    def test_tugs_first_position(self, validated_spec_24in):
        """First tug position is [716.7, -20, 0]."""
        pos = validated_spec_24in.equipment.tugs.first_position
        assert pos[0] == pytest.approx(716.7)
        assert pos[1] == pytest.approx(-20)
        assert pos[2] == pytest.approx(0)

    def test_rollers_position_and_supports(self, validated_spec_24in):
        """Rollers at position [5, 0, -2] with 4 supports."""
        rollers = validated_spec_24in.equipment.rollers
        assert rollers.position == [pytest.approx(5), pytest.approx(0), pytest.approx(-2)]
        assert rollers.supports == 4

    def test_buoyancy_modules(self, validated_spec_24in):
        """BM spacing 1.5 m, volume 0.72 m3, height 0.918 m."""
        bm = validated_spec_24in.equipment.buoyancy_modules
        assert bm.spacing == pytest.approx(1.5)
        assert bm.properties.volume == pytest.approx(0.72)
        assert bm.properties.height == pytest.approx(0.918)

    def test_ramps(self, validated_spec_24in):
        """Two ramps: block and curved_plate."""
        ramps = validated_spec_24in.equipment.ramps
        assert len(ramps) == 2
        assert ramps[0].type == "block"
        assert ramps[1].type == "curved_plate"


# ===================================================================
# 6. Simulation validation (~2 tests)
# ===================================================================


class TestSimulation:
    """Validate simulation parameters."""

    def test_time_step_and_stages(self, validated_spec_24in):
        """Time step 0.1 s, stages [8, 16]."""
        sim = validated_spec_24in.simulation
        assert sim.time_step == pytest.approx(0.1)
        assert sim.stages == [8, 16]

    def test_north_direction(self, validated_spec_24in):
        """North direction is 70 degrees."""
        assert validated_spec_24in.simulation.north_direction == pytest.approx(70)


# ===================================================================
# 7. Generation tests (~5 tests)
# ===================================================================


class TestGeneration:
    """Test modular model generation from the 24in spec."""

    def test_generates_master_yml(self, tmp_path):
        """ModularModelGenerator produces master.yml."""
        from digitalmodel.solvers.orcaflex.modular_generator import (
            ModularModelGenerator,
        )

        generator = ModularModelGenerator(SPEC_FILE)
        generator.generate(tmp_path)
        assert (tmp_path / "master.yml").exists()

    def test_generates_at_least_five_includes(self, tmp_path):
        """At least 5 include files are generated."""
        from digitalmodel.solvers.orcaflex.modular_generator import (
            ModularModelGenerator,
        )

        generator = ModularModelGenerator(SPEC_FILE)
        generator.generate(tmp_path)
        includes = list((tmp_path / "includes").glob("*.yml"))
        assert len(includes) >= 5

    def test_generates_parameters_yml(self, tmp_path):
        """parameters.yml is generated in the inputs directory."""
        from digitalmodel.solvers.orcaflex.modular_generator import (
            ModularModelGenerator,
        )

        generator = ModularModelGenerator(SPEC_FILE)
        generator.generate(tmp_path)
        assert (tmp_path / "inputs" / "parameters.yml").exists()

    def test_parameters_contain_correct_water_depth(self, tmp_path):
        """parameters.yml contains water_depth = 8."""
        from digitalmodel.solvers.orcaflex.modular_generator import (
            ModularModelGenerator,
        )

        generator = ModularModelGenerator(SPEC_FILE)
        generator.generate(tmp_path)

        with open(tmp_path / "inputs" / "parameters.yml") as f:
            params = yaml.safe_load(f)

        assert params["water_depth"] == pytest.approx(8)

    def test_from_spec_classmethod(self, validated_spec_24in, tmp_path):
        """from_spec() classmethod creates a working generator."""
        from digitalmodel.solvers.orcaflex.modular_generator import (
            ModularModelGenerator,
        )

        generator = ModularModelGenerator.from_spec(validated_spec_24in)
        assert generator.spec is validated_spec_24in
        assert generator.spec_file is None

        generator.generate(tmp_path)
        assert (tmp_path / "master.yml").exists()


# ===================================================================
# 8. Negative tests (~2 tests)
# ===================================================================


class TestNegativeValidation:
    """Verify schema rejects invalid data."""

    def test_missing_pipeline_raises_validation_error(self, spec_data_24in):
        """Removing the 'pipeline' key raises pydantic ValidationError."""
        from pydantic import ValidationError

        from digitalmodel.solvers.orcaflex.modular_generator.schema import (
            ProjectInputSpec,
        )

        data = copy.deepcopy(spec_data_24in)
        del data["pipeline"]

        with pytest.raises(ValidationError):
            ProjectInputSpec(**data)

    def test_negative_outer_diameter_raises_validation_error(self, spec_data_24in):
        """Setting outer_diameter to a negative value raises ValidationError."""
        from pydantic import ValidationError

        from digitalmodel.solvers.orcaflex.modular_generator.schema import (
            ProjectInputSpec,
        )

        data = copy.deepcopy(spec_data_24in)
        data["pipeline"]["dimensions"]["outer_diameter"] = -0.5

        with pytest.raises(ValidationError):
            ProjectInputSpec(**data)
