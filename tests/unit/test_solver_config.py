"""
ABOUTME: Unit tests for SolverConfigManager
ABOUTME: Test schema validation and configuration management
"""

import pytest
from digitalmodel.base_solvers.config import SolverConfigManager


class TestSolverConfigManager:
    """Tests for SolverConfigManager class."""

    def test_initialization(self):
        """Test manager initialization."""
        manager = SolverConfigManager()
        assert manager.config == {}
        assert len(manager.get_validation_errors()) == 0

    def test_initialization_with_config(self):
        """Test initialization with initial configuration."""
        initial_config = {"key1": "value1", "key2": "value2"}
        manager = SolverConfigManager(initial_config)

        assert manager.config["key1"] == "value1"
        assert manager.config["key2"] == "value2"

    def test_get_schema_structural(self):
        """Test retrieving structural solver schema."""
        manager = SolverConfigManager()
        schema = manager.get_schema("structural")

        assert "tolerance" in schema
        assert "max_iterations" in schema
        assert "method" in schema
        assert "output_format" in schema

    def test_get_schema_marine(self):
        """Test retrieving marine solver schema."""
        manager = SolverConfigManager()
        schema = manager.get_schema("marine")

        assert "water_depth" in schema
        assert "wave_height" in schema
        assert "current_velocity" in schema
        assert "material_density" in schema
        assert "safety_factor" in schema
        assert "analysis_type" in schema

    def test_get_schema_signal(self):
        """Test retrieving signal solver schema."""
        manager = SolverConfigManager()
        schema = manager.get_schema("signal")

        assert "sampling_rate" in schema
        assert "window_size" in schema
        assert "overlap" in schema
        assert "filter_type" in schema
        assert "cutoff_frequency" in schema

    def test_get_schema_fatigue(self):
        """Test retrieving fatigue solver schema."""
        manager = SolverConfigManager()
        schema = manager.get_schema("fatigue")

        assert "material_type" in schema
        assert "design_life" in schema
        assert "sn_curve" in schema
        assert "mean_stress_correction" in schema
        assert "stress_concentration" in schema

    def test_get_schema_unknown_type(self):
        """Test getting schema for unknown solver type."""
        manager = SolverConfigManager()
        schema = manager.get_schema("unknown_type")

        assert schema == {}

    def test_validate_structural_valid_config(self):
        """Test validation of valid structural configuration."""
        manager = SolverConfigManager()
        config = {
            "tolerance": 1e-6,
            "max_iterations": 500,
            "method": "direct",
            "output_format": "numpy"
        }

        is_valid, errors = manager.validate_solver_config("structural", config)

        assert is_valid is True
        assert len(errors) == 0

    def test_validate_structural_invalid_type(self):
        """Test validation with wrong type."""
        manager = SolverConfigManager()
        config = {"tolerance": "not_a_number"}

        is_valid, errors = manager.validate_solver_config("structural", config)

        assert is_valid is False
        assert len(errors) > 0

    def test_validate_min_value_constraint(self):
        """Test validation of minimum value constraint."""
        manager = SolverConfigManager()
        config = {"tolerance": 1e-12}  # Below minimum of 1e-10

        is_valid, errors = manager.validate_solver_config("structural", config)

        assert is_valid is False
        assert any("minimum" in error.lower() for error in errors)

    def test_validate_max_value_constraint(self):
        """Test validation of maximum value constraint."""
        manager = SolverConfigManager()
        config = {"tolerance": 0.5}  # Above maximum of 1e-2

        is_valid, errors = manager.validate_solver_config("structural", config)

        assert is_valid is False
        assert any("maximum" in error.lower() for error in errors)

    def test_validate_allowed_values_constraint(self):
        """Test validation of allowed values constraint."""
        manager = SolverConfigManager()
        config = {"method": "invalid_method"}

        is_valid, errors = manager.validate_solver_config("structural", config)

        assert is_valid is False
        assert any("allowed" in error.lower() for error in errors)

    def test_validate_marine_config(self):
        """Test validation of marine configuration."""
        manager = SolverConfigManager()
        # Only include parameters that are in the schema
        config = {
            "water_depth": 1000.0,
            "wave_height": 5.0,
            "current_velocity": 1.5,
            "material_density": 1025.0,
            "safety_factor": 1.5,
            "analysis_type": "static"
        }

        is_valid, errors = manager.validate_solver_config("marine", config)

        # Debug: show any validation errors
        if errors:
            print(f"Validation errors: {errors}")

        assert is_valid is True, f"Validation failed with errors: {errors}"
        assert len(errors) == 0

    def test_validate_marine_invalid_safety_factor(self):
        """Test marine config with invalid safety factor."""
        manager = SolverConfigManager()
        config = {"safety_factor": 10}  # Above maximum of 5.0

        is_valid, errors = manager.validate_solver_config("marine", config)

        assert is_valid is False

    def test_get_default_config_structural(self):
        """Test getting default configuration for structural solver."""
        manager = SolverConfigManager()
        defaults = manager.get_default_config("structural")

        assert defaults["tolerance"] == 1e-6
        assert defaults["max_iterations"] == 1000
        assert defaults["method"] == "direct"
        assert defaults["output_format"] == "numpy"

    def test_get_default_config_marine(self):
        """Test getting default configuration for marine solver."""
        manager = SolverConfigManager()
        defaults = manager.get_default_config("marine")

        assert defaults["water_depth"] == 0
        assert defaults["wave_height"] == 0
        assert defaults["material_density"] == 1025
        assert defaults["safety_factor"] == 1.5

    def test_merge_configs(self):
        """Test merging configurations."""
        manager = SolverConfigManager()
        base = {"tolerance": 1e-6, "max_iterations": 100}
        override = {"max_iterations": 500, "method": "iterative"}

        merged = manager.merge_configs(base, override)

        assert merged["tolerance"] == 1e-6
        assert merged["max_iterations"] == 500
        assert merged["method"] == "iterative"

    def test_merge_configs_isolation(self):
        """Test that merge doesn't modify originals."""
        manager = SolverConfigManager()
        base = {"value": 100}
        override = {"extra": 200}

        merged = manager.merge_configs(base, override)
        merged["value"] = 999

        assert base["value"] == 100
        assert merged["value"] == 999

    def test_validation_errors_retrieval(self):
        """Test retrieving validation errors."""
        manager = SolverConfigManager()
        config = {
            "tolerance": "invalid",
            "max_iterations": -1,
            "method": "unknown"
        }

        is_valid, _ = manager.validate_solver_config("structural", config)

        errors = manager.get_validation_errors()
        assert len(errors) > 0
        assert is_valid is False

    def test_unknown_solver_type(self):
        """Test validation for unknown solver type."""
        manager = SolverConfigManager()
        config = {"some_key": "some_value"}

        is_valid, errors = manager.validate_solver_config("unknown_solver", config)

        assert is_valid is False
        assert "Unknown solver type" in errors[0]

    def test_multiple_validations(self):
        """Test multiple validations update error list."""
        manager = SolverConfigManager()

        # First validation
        manager.validate_solver_config("structural", {"tolerance": "invalid"})
        errors1 = manager.get_validation_errors()

        # Second validation should clear previous errors
        manager.validate_solver_config("marine", {"safety_factor": 10})
        errors2 = manager.get_validation_errors()

        assert len(errors2) > 0
        # Errors should be for the second validation, not accumulated
        assert "safety_factor" in errors2[0] or "maximum" in errors2[0]


class TestSolverConfigIntegration:
    """Integration tests for solver configuration."""

    def test_config_workflow(self):
        """Test complete configuration workflow."""
        manager = SolverConfigManager()

        # Get defaults
        defaults = manager.get_default_config("structural")
        assert "tolerance" in defaults

        # Validate defaults
        is_valid, errors = manager.validate_solver_config("structural", defaults)
        assert is_valid is True

        # Merge with overrides
        overrides = {"tolerance": 1e-7, "max_iterations": 2000}
        merged = manager.merge_configs(defaults, overrides)

        # Validate merged config
        is_valid, errors = manager.validate_solver_config("structural", merged)
        assert is_valid is True
        assert merged["tolerance"] == 1e-7
        assert merged["max_iterations"] == 2000

    def test_config_isolation_in_operations(self):
        """Test that config operations maintain isolation."""
        manager = SolverConfigManager()

        # Get config
        config1 = manager.get_default_config("structural")
        config1["tolerance"] = 0.999

        # Get again
        config2 = manager.get_default_config("structural")

        # Second should have original default
        assert config2["tolerance"] == 1e-6


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
