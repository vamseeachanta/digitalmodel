"""Tests for schema backward compatibility after package split."""

from __future__ import annotations

import pytest


class TestSchemaImports:
    """Verify all classes are importable from the schema package."""

    def test_import_project_input_spec(self):
        from digitalmodel.modules.orcaflex.modular_generator.schema import (
            ProjectInputSpec,
        )

        assert ProjectInputSpec is not None

    def test_import_enums(self):
        from digitalmodel.modules.orcaflex.modular_generator.schema import (
            RampType,
            StructureType,
            WaveType,
        )

        assert StructureType.PIPELINE.value == "pipeline"
        assert WaveType.AIRY.value == "airy"
        assert RampType.BLOCK.value == "block"

    def test_import_metadata(self):
        from digitalmodel.modules.orcaflex.modular_generator.schema import Metadata

        m = Metadata(
            name="test",
            description="test desc",
            structure="pipeline",
            operation="installation/floating",
            project="TEST",
        )
        assert m.name == "test"

    def test_import_environment_classes(self):
        from digitalmodel.modules.orcaflex.modular_generator.schema import (
            Current,
            CurrentProfile,
            Environment,
            Seabed,
            SeabedStiffness,
            Water,
            Waves,
            Wind,
        )

        w = Water(depth=10, density=1.025)
        assert w.depth == 10

    def test_import_pipeline_classes(self):
        from digitalmodel.modules.orcaflex.modular_generator.schema import (
            Coating,
            Coatings,
            Dimensions,
            Pipeline,
            Segment,
        )

        d = Dimensions(outer_diameter=0.5, wall_thickness=0.02)
        assert d.outer_diameter == 0.5

    def test_import_equipment_classes(self):
        from digitalmodel.modules.orcaflex.modular_generator.schema import (
            BuoyancyModuleProperties,
            BuoyancyModules,
            Equipment,
            Ramp,
            Rollers,
            TugProperties,
            Tugs,
        )

        e = Equipment()
        assert e.tugs is None

    def test_import_simulation(self):
        from digitalmodel.modules.orcaflex.modular_generator.schema import Simulation

        s = Simulation()
        assert s.time_step == 0.1


class TestSchemaValidation:
    """Test schema validation with valid and invalid inputs."""

    def test_valid_spec_loads(self, spec_data):
        from digitalmodel.modules.orcaflex.modular_generator.schema import (
            ProjectInputSpec,
        )

        spec = ProjectInputSpec(**spec_data)
        assert spec.metadata.name == "30in_pipeline_installation"
        assert spec.environment.water.depth == 8
        assert len(spec.pipeline.segments) == 2

    def test_total_pipeline_length(self, validated_spec):
        total = validated_spec.get_total_pipeline_length()
        assert total == pytest.approx(1894.4 + 3005.6)

    def test_tug_positions(self, validated_spec):
        positions = validated_spec.get_tug_positions()
        assert len(positions) == 5
        assert positions[0] == [716.7, -20, 0]
        assert positions[1][0] == pytest.approx(716.7 + 816.6)

    def test_buoyancy_module_positions(self, validated_spec):
        positions = validated_spec.get_buoyancy_module_positions()
        assert len(positions) > 0
        assert positions[0] == 0
        assert positions[1] == pytest.approx(3.9)

    def test_invalid_water_depth_negative(self):
        from pydantic import ValidationError

        from digitalmodel.modules.orcaflex.modular_generator.schema import Water

        with pytest.raises(ValidationError):
            Water(depth=-1, density=1.025)

    def test_invalid_water_depth_too_deep(self):
        from pydantic import ValidationError

        from digitalmodel.modules.orcaflex.modular_generator.schema import Water

        with pytest.raises(ValidationError):
            Water(depth=6000, density=1.025)

    def test_invalid_wall_thickness(self):
        from pydantic import ValidationError

        from digitalmodel.modules.orcaflex.modular_generator.schema import Dimensions

        with pytest.raises(ValidationError):
            Dimensions(outer_diameter=0.5, wall_thickness=0.3)

    def test_invalid_current_profile_not_monotonic(self):
        from pydantic import ValidationError

        from digitalmodel.modules.orcaflex.modular_generator.schema import Current

        with pytest.raises(ValidationError):
            Current(speed=1.0, direction=0, profile=[[10, 1.0], [5, 0.8]])
