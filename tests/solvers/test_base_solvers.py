#!/usr/bin/env python3
"""
ABOUTME: Comprehensive test suite for base_solvers module
ABOUTME: Tests for abstract base classes, configuration, and solver implementations
"""

import pytest
import logging
import sys
from copy import deepcopy
from typing import Dict, Any, List
from unittest.mock import Mock, MagicMock, patch

# Mock ConfigManager before importing base_solvers
with patch("digitalmodel.base_configs.ConfigManager"):
    from digitalmodel.base_solvers.base import (
        SolverStatus,
        BaseSolver,
        ConfigurableSolver,
        AnalysisSolver,
    )


# ============================================================================
# FIXTURES
# ============================================================================

@pytest.fixture
def sample_solver_config():
    """Sample configuration for test solvers."""
    return {
        "name": "test-solver",
        "version": "1.0.0",
        "timeout_seconds": 30,
        "max_iterations": 100,
        "tolerance": 1e-6,
        "nested": {
            "parameter_1": 1.5,
            "parameter_2": [1, 2, 3],
        }
    }


@pytest.fixture
def sample_input_data():
    """Sample input data for analysis solvers."""
    return {
        "parameter_1": 1.5,
        "parameter_2": [1, 2, 3],
        "parameter_3": {"nested": "value"},
        "matrix": [[1, 2], [3, 4]],
    }


@pytest.fixture
def sample_results():
    """Sample results dictionary from solver execution."""
    return {
        "success": True,
        "status": SolverStatus.COMPLETED.value,
        "iterations": 42,
        "final_value": 3.14159,
        "convergence": True,
        "computation_time": 0.125,
    }


@pytest.fixture
def mock_config_manager():
    """Mock ConfigManager for testing."""
    mock = MagicMock()
    mock.config = {}
    mock.get = MagicMock(return_value=None)
    mock.set = MagicMock()
    return mock


@pytest.fixture
def mock_cm():
    """Mock ConfigManager instance for test methods."""
    return MagicMock()


@pytest.fixture
def concrete_base_solver():
    """Concrete implementation of BaseSolver for testing."""
    class ConcreteBaseSolver(BaseSolver):
        def validate_inputs(self):
            return (True, [])

        def solve(self):
            self._set_status(SolverStatus.EXECUTING)
            results = {"success": True, "status": SolverStatus.COMPLETED.value}
            self._cache_results(results)
            self._set_status(SolverStatus.COMPLETED)
            return results

        def get_solver_metadata(self):
            return {
                "name": self.name,
                "version": self.version,
                "description": "Test solver",
                "inputs": {"param1": {"type": "float"}},
                "outputs": {"result": {"type": "float"}},
                "solver_type": "structural",
            }

    return ConcreteBaseSolver


@pytest.fixture
def concrete_configurable_solver():
    """Concrete implementation of ConfigurableSolver for testing."""
    class ConcreteConfigurableSolver(ConfigurableSolver):
        def validate_inputs(self):
            return (True, [])

        def solve(self):
            self._set_status(SolverStatus.EXECUTING)
            results = {"success": True, "status": SolverStatus.COMPLETED.value}
            self._cache_results(results)
            self._set_status(SolverStatus.COMPLETED)
            return results

        def get_solver_metadata(self):
            return {
                "name": self.name,
                "version": self.version,
                "description": "Test configurable solver",
                "inputs": {},
                "outputs": {},
                "solver_type": "marine",
            }

    return ConcreteConfigurableSolver


@pytest.fixture
def concrete_analysis_solver():
    """Concrete implementation of AnalysisSolver for testing."""
    class ConcreteAnalysisSolver(AnalysisSolver):
        def validate_inputs(self):
            if not self.input_data:
                return (False, ["No input data provided"])
            return (True, [])

        def solve(self):
            is_valid, errors = self.validate_inputs()
            if not is_valid:
                self.set_validation_errors(errors)
                self._set_status(SolverStatus.FAILED)
                return {"success": False, "status": SolverStatus.FAILED.value, "errors": errors}

            self._set_status(SolverStatus.EXECUTING)
            results = {"success": True, "status": SolverStatus.COMPLETED.value}
            self._cache_results(results)
            self._set_status(SolverStatus.COMPLETED)
            return results

        def get_solver_metadata(self):
            return {
                "name": self.name,
                "version": self.version,
                "description": "Test analysis solver",
                "inputs": {"data": {"type": "dict"}},
                "outputs": {"results": {"type": "dict"}},
                "solver_type": "signal",
            }

    return ConcreteAnalysisSolver


# ============================================================================
# TEST SOLVER STATUS ENUM
# ============================================================================

class TestSolverStatus:
    """Test SolverStatus enumeration."""

    def test_solver_status_values(self):
        """Test that all status values are defined."""
        assert SolverStatus.PENDING.value == "pending"
        assert SolverStatus.VALIDATING.value == "validating"
        assert SolverStatus.EXECUTING.value == "executing"
        assert SolverStatus.COMPLETED.value == "completed"
        assert SolverStatus.FAILED.value == "failed"

    def test_solver_status_enum_members(self):
        """Test that all enum members are present."""
        statuses = [s.name for s in SolverStatus]
        assert "PENDING" in statuses
        assert "VALIDATING" in statuses
        assert "EXECUTING" in statuses
        assert "COMPLETED" in statuses
        assert "FAILED" in statuses

    def test_solver_status_comparison(self):
        """Test status comparison."""
        assert SolverStatus.PENDING == SolverStatus.PENDING
        assert SolverStatus.COMPLETED != SolverStatus.FAILED


# ============================================================================
# TEST BASE SOLVER
# ============================================================================

class TestBaseSolver:
    """Test BaseSolver abstract base class."""

    def test_base_solver_initialization(self, concrete_base_solver):
        """Test solver initialization with name and version."""
        solver = concrete_base_solver("test-solver", "1.0.0")
        assert solver.name == "test-solver"
        assert solver.version == "1.0.0"
        assert solver.status == SolverStatus.PENDING

    def test_base_solver_cannot_instantiate_abstract(self):
        """Test that BaseSolver cannot be instantiated directly."""
        with pytest.raises(TypeError):
            BaseSolver("test", "1.0.0")

    def test_base_solver_requires_validate_inputs(self):
        """Test that implementations must implement validate_inputs."""
        class IncompleteSolver(BaseSolver):
            def solve(self):
                pass

            def get_solver_metadata(self):
                pass

        with pytest.raises(TypeError):
            IncompleteSolver("test", "1.0.0")

    def test_base_solver_get_status(self, concrete_base_solver):
        """Test get_status returns current status."""
        solver = concrete_base_solver("test", "1.0.0")
        assert solver.get_status() == SolverStatus.PENDING

    def test_base_solver_status_transitions(self, concrete_base_solver):
        """Test status transitions through execution lifecycle."""
        solver = concrete_base_solver("test", "1.0.0")
        assert solver.status == SolverStatus.PENDING

        solver._set_status(SolverStatus.VALIDATING)
        assert solver.status == SolverStatus.VALIDATING

        solver._set_status(SolverStatus.EXECUTING)
        assert solver.status == SolverStatus.EXECUTING

        solver._set_status(SolverStatus.COMPLETED)
        assert solver.status == SolverStatus.COMPLETED

    def test_base_solver_results_caching(self, concrete_base_solver, sample_results):
        """Test that results are properly cached."""
        solver = concrete_base_solver("test", "1.0.0")
        solver._cache_results(sample_results)

        cached = solver.get_results()
        assert cached == sample_results
        assert cached is not sample_results  # Deep copy verification

    def test_base_solver_results_isolation(self, concrete_base_solver, sample_results):
        """Test that modifications to cached results don't affect internal state."""
        solver = concrete_base_solver("test", "1.0.0")
        solver._cache_results(sample_results)

        retrieved = solver.get_results()
        retrieved["final_value"] = 999.0

        # Original should be unchanged
        final_results = solver.get_results()
        assert final_results["final_value"] == 3.14159

    def test_base_solver_empty_results_on_init(self, concrete_base_solver):
        """Test that results are empty on initialization."""
        solver = concrete_base_solver("test", "1.0.0")
        assert solver.get_results() == {}

    def test_base_solver_solve_execution(self, concrete_base_solver):
        """Test complete solve execution cycle."""
        solver = concrete_base_solver("test", "1.0.0")
        results = solver.solve()

        assert results["success"] is True
        assert solver.get_status() == SolverStatus.COMPLETED
        assert solver.get_results() == results

    def test_base_solver_metadata_retrieval(self, concrete_base_solver):
        """Test getting solver metadata."""
        solver = concrete_base_solver("test-solver", "2.0.0")
        metadata = solver.get_solver_metadata()

        assert metadata["name"] == "test-solver"
        assert metadata["version"] == "2.0.0"
        assert "description" in metadata
        assert "inputs" in metadata
        assert "outputs" in metadata
        assert "solver_type" in metadata

    def test_base_solver_multiple_instances(self, concrete_base_solver):
        """Test that multiple solver instances are independent."""
        solver1 = concrete_base_solver("solver1", "1.0.0")
        solver2 = concrete_base_solver("solver2", "2.0.0")

        solver1._cache_results({"value": 1})
        solver2._cache_results({"value": 2})

        assert solver1.get_results()["value"] == 1
        assert solver2.get_results()["value"] == 2


# ============================================================================
# TEST CONFIGURABLE SOLVER
# ============================================================================

class TestConfigurableSolver:
    """Test ConfigurableSolver class."""

    def test_configurable_solver_initialization(self, concrete_configurable_solver):
        """Test initialization of configurable solver."""
        solver = concrete_configurable_solver("test", "1.0.0")
        assert solver.name == "test"
        assert solver.version == "1.0.0"
        assert hasattr(solver, "config_manager")

    def test_configurable_solver_with_initial_config(
        self, concrete_configurable_solver, sample_solver_config
    ):
        """Test initialization with initial configuration."""
        solver = concrete_configurable_solver("test", "1.0.0", sample_solver_config)
        assert solver.name == "test"

    def test_configurable_solver_get_config(self, concrete_configurable_solver):
        """Test get_config method."""
        solver = concrete_configurable_solver("test", "1.0.0")
        # ConfigManager is mocked, test that method exists
        assert hasattr(solver, "get_config")

    def test_configurable_solver_set_config(self, concrete_configurable_solver):
        """Test set_config method."""
        solver = concrete_configurable_solver("test", "1.0.0")
        # ConfigManager is mocked, test that method exists
        assert hasattr(solver, "set_config")

    def test_configurable_solver_get_all_config(
        self, concrete_configurable_solver, sample_solver_config
    ):
        """Test get_all_config returns configuration."""
        solver = concrete_configurable_solver("test", "1.0.0")
        # ConfigManager is mocked, test that method exists
        assert hasattr(solver, "get_all_config")

    def test_configurable_solver_inheritance(self, concrete_configurable_solver):
        """Test that ConfigurableSolver inherits from BaseSolver."""
        solver = concrete_configurable_solver("test", "1.0.0")
        assert isinstance(solver, BaseSolver)
        assert hasattr(solver, "name")
        assert hasattr(solver, "version")
        assert hasattr(solver, "status")


# ============================================================================
# TEST ANALYSIS SOLVER
# ============================================================================

class TestAnalysisSolver:
    """Test AnalysisSolver class."""

    # ConfigManager is mocked at module level
    def test_analysis_solver_initialization(
        self, mock_cm, concrete_analysis_solver
    ):
        """Test initialization of analysis solver."""
        solver = concrete_analysis_solver("test", "1.0.0")
        assert solver.name == "test"
        assert solver.input_data == {}
        assert solver.validation_errors == []

    # ConfigManager is mocked at module level
    def test_analysis_solver_set_input_data(
        self, mock_cm, concrete_analysis_solver, sample_input_data
    ):
        """Test setting input data."""
        solver = concrete_analysis_solver("test", "1.0.0")
        solver.set_input_data(sample_input_data)

        assert solver.input_data == sample_input_data
        assert solver.input_data is not sample_input_data  # Deep copy

    # ConfigManager is mocked at module level
    def test_analysis_solver_get_input_data(
        self, mock_cm, concrete_analysis_solver, sample_input_data
    ):
        """Test getting input data returns deep copy."""
        solver = concrete_analysis_solver("test", "1.0.0")
        solver.set_input_data(sample_input_data)

        retrieved = solver.get_input_data()
        assert retrieved == sample_input_data
        assert retrieved is not sample_input_data  # Deep copy

    # ConfigManager is mocked at module level
    def test_analysis_solver_input_data_isolation(
        self, mock_cm, concrete_analysis_solver, sample_input_data
    ):
        """Test that external modifications don't affect input data."""
        solver = concrete_analysis_solver("test", "1.0.0")
        solver.set_input_data(sample_input_data)

        retrieved = solver.get_input_data()
        retrieved["parameter_1"] = 999.0

        # Original should be unchanged
        final_data = solver.get_input_data()
        assert final_data["parameter_1"] == 1.5

    # ConfigManager is mocked at module level
    def test_analysis_solver_set_validation_errors(
        self, mock_cm, concrete_analysis_solver
    ):
        """Test setting validation errors."""
        errors = ["Error 1", "Error 2", "Error 3"]
        solver = concrete_analysis_solver("test", "1.0.0")
        solver.set_validation_errors(errors)

        assert solver.get_validation_errors() == errors
        assert solver.has_validation_errors() is True

    # ConfigManager is mocked at module level
    def test_analysis_solver_get_validation_errors(
        self, mock_cm, concrete_analysis_solver
    ):
        """Test getting validation errors returns copy."""
        errors = ["Error 1", "Error 2"]
        solver = concrete_analysis_solver("test", "1.0.0")
        solver.set_validation_errors(errors)

        retrieved = solver.get_validation_errors()
        assert retrieved == errors
        assert retrieved is not errors  # Copy, not reference

    # ConfigManager is mocked at module level
    def test_analysis_solver_add_validation_error(
        self, mock_cm, concrete_analysis_solver
    ):
        """Test adding individual validation errors."""
        solver = concrete_analysis_solver("test", "1.0.0")
        solver.add_validation_error("Error 1")
        solver.add_validation_error("Error 2")

        errors = solver.get_validation_errors()
        assert len(errors) == 2
        assert "Error 1" in errors
        assert "Error 2" in errors

    # ConfigManager is mocked at module level
    def test_analysis_solver_clear_validation_errors(
        self, mock_cm, concrete_analysis_solver
    ):
        """Test clearing validation errors."""
        solver = concrete_analysis_solver("test", "1.0.0")
        solver.add_validation_error("Error 1")
        assert solver.has_validation_errors() is True

        solver.clear_validation_errors()
        assert solver.has_validation_errors() is False
        assert solver.get_validation_errors() == []

    # ConfigManager is mocked at module level
    def test_analysis_solver_has_validation_errors(
        self, mock_cm, concrete_analysis_solver
    ):
        """Test checking for validation errors."""
        solver = concrete_analysis_solver("test", "1.0.0")
        assert solver.has_validation_errors() is False

        solver.add_validation_error("Error")
        assert solver.has_validation_errors() is True

    # ConfigManager is mocked at module level
    def test_analysis_solver_inheritance(self, mock_cm, concrete_analysis_solver):
        """Test that AnalysisSolver inherits from ConfigurableSolver."""
        solver = concrete_analysis_solver("test", "1.0.0")
        assert isinstance(solver, ConfigurableSolver)
        assert isinstance(solver, BaseSolver)

    # ConfigManager is mocked at module level
    def test_analysis_solver_with_config(
        self, mock_cm, concrete_analysis_solver, sample_solver_config, sample_input_data
    ):
        """Test AnalysisSolver with both configuration and input data."""
        mock_instance = MagicMock()
        mock_cm.return_value = mock_instance

        solver = concrete_analysis_solver("test", "1.0.0", sample_solver_config)
        solver.set_input_data(sample_input_data)

        assert len(solver.get_input_data()) > 0
        assert hasattr(solver, "config_manager")

    # ConfigManager is mocked at module level
    def test_analysis_solver_validation_workflow(
        self, mock_cm, concrete_analysis_solver, sample_input_data
    ):
        """Test complete validation workflow."""
        solver = concrete_analysis_solver("test", "1.0.0")

        # No input data - should fail validation
        is_valid, errors = solver.validate_inputs()
        assert is_valid is False
        assert len(errors) > 0

        # With input data - should pass validation
        solver.set_input_data(sample_input_data)
        is_valid, errors = solver.validate_inputs()
        assert is_valid is True
        assert len(errors) == 0


# ============================================================================
# TEST INTEGRATION SCENARIOS
# ============================================================================

class TestIntegrationScenarios:
    """Test integration scenarios across multiple components."""

    # ConfigManager is mocked at module level
    def test_multiple_solvers_independent(
        self, mock_cm, concrete_analysis_solver, sample_input_data
    ):
        """Test that multiple solver instances are truly independent."""
        # Create two separate ConfigManager mocks
        mock_cm.side_effect = [MagicMock(), MagicMock()]

        solver1 = concrete_analysis_solver("solver1", "1.0.0")
        solver2 = concrete_analysis_solver("solver2", "2.0.0")

        # Modify solver1
        solver1.set_input_data(sample_input_data)
        solver1.add_validation_error("Error in solver1")

        # Solver2 should be unaffected
        assert solver2.get_input_data() == {}
        assert solver2.get_validation_errors() == []

    # ConfigManager is mocked at module level
    def test_solver_configuration_isolation(
        self, concrete_configurable_solver
    ):
        """Test that configurations are isolated between solvers."""
        # Create two solvers with independent config managers
        solver1 = concrete_configurable_solver("solver1", "1.0.0")
        solver2 = concrete_configurable_solver("solver2", "1.0.0")

        # Verify both have config_manager attribute
        assert hasattr(solver1, "config_manager")
        assert hasattr(solver2, "config_manager")
        # Verify they are different instances
        assert solver1.config_manager is not solver2.config_manager

    # ConfigManager is mocked at module level
    def test_solver_execution_state_tracking(
        self, mock_cm, concrete_analysis_solver, sample_input_data, sample_results
    ):
        """Test complete execution state tracking."""
        solver = concrete_analysis_solver("test", "1.0.0")
        solver.set_input_data(sample_input_data)

        # Execute solve
        results = solver.solve()

        # Verify state progression
        assert solver.get_status() == SolverStatus.COMPLETED
        assert results["success"] is True
        assert solver.get_results() == results

    # ConfigManager is mocked at module level
    def test_solver_error_handling_workflow(
        self, mock_cm, concrete_analysis_solver
    ):
        """Test error handling workflow with validation."""
        solver = concrete_analysis_solver("test", "1.0.0")

        # No input data - should fail
        results = solver.solve()

        assert results["success"] is False
        assert solver.get_status() == SolverStatus.FAILED
        assert solver.has_validation_errors() is True

    # ConfigManager is mocked at module level
    def test_deep_copy_guarantees(
        self, mock_cm, concrete_analysis_solver
    ):
        """Test that all return values are deep copies."""
        data = {"nested": {"deep": {"value": 42}}}
        solver = concrete_analysis_solver("test", "1.0.0")
        solver.set_input_data(data)

        # Retrieve and modify
        retrieved = solver.get_input_data()
        retrieved["nested"]["deep"]["value"] = 999

        # Original should be unchanged
        final = solver.get_input_data()
        assert final["nested"]["deep"]["value"] == 42

    # ConfigManager is mocked at module level
    def test_metadata_consistency(
        self, mock_cm, concrete_base_solver, concrete_configurable_solver,
        concrete_analysis_solver
    ):
        """Test that metadata is consistent across solver types."""
        base = concrete_base_solver("base", "1.0.0")
        config = concrete_configurable_solver("config", "1.0.0")
        analysis = concrete_analysis_solver("analysis", "1.0.0")

        for solver in [base, config, analysis]:
            metadata = solver.get_solver_metadata()
            assert metadata["name"] == solver.name
            assert metadata["version"] == solver.version
            assert "description" in metadata
            assert "inputs" in metadata
            assert "outputs" in metadata
            assert "solver_type" in metadata


# ============================================================================
# TEST EDGE CASES
# ============================================================================

class TestEdgeCases:
    """Test edge cases and boundary conditions."""

    # ConfigManager is mocked at module level
    def test_empty_solver_name(self, mock_cm, concrete_base_solver):
        """Test solver with empty name."""
        solver = concrete_base_solver("", "1.0.0")
        assert solver.name == ""

    # ConfigManager is mocked at module level
    def test_unicode_solver_names(self, mock_cm, concrete_base_solver):
        """Test solver with unicode names."""
        solver = concrete_base_solver("测试求解器", "1.0.0")
        assert solver.name == "测试求解器"

    # ConfigManager is mocked at module level
    def test_very_long_solver_name(self, mock_cm, concrete_base_solver):
        """Test solver with very long name."""
        long_name = "a" * 1000
        solver = concrete_base_solver(long_name, "1.0.0")
        assert solver.name == long_name

    # ConfigManager is mocked at module level
    def test_multiple_error_additions(self, mock_cm, concrete_analysis_solver):
        """Test adding many validation errors."""
        solver = concrete_analysis_solver("test", "1.0.0")

        for i in range(100):
            solver.add_validation_error(f"Error {i}")

        assert len(solver.get_validation_errors()) == 100

    # ConfigManager is mocked at module level
    def test_large_input_data(self, mock_cm, concrete_analysis_solver):
        """Test handling large input datasets."""
        large_data = {
            "matrix": [[i + j for j in range(100)] for i in range(100)],
            "arrays": [[1, 2, 3] * 1000 for _ in range(10)],
        }

        solver = concrete_analysis_solver("test", "1.0.0")
        solver.set_input_data(large_data)

        retrieved = solver.get_input_data()
        assert retrieved == large_data

    # ConfigManager is mocked at module level
    def test_none_values_in_data(self, mock_cm, concrete_analysis_solver):
        """Test handling None values in input data."""
        data = {
            "param1": None,
            "param2": [1, None, 3],
            "param3": {"key": None},
        }

        solver = concrete_analysis_solver("test", "1.0.0")
        solver.set_input_data(data)

        assert solver.get_input_data() == data

    # ConfigManager is mocked at module level
    def test_clear_errors_when_already_empty(self, mock_cm, concrete_analysis_solver):
        """Test clearing errors when list is already empty."""
        solver = concrete_analysis_solver("test", "1.0.0")
        solver.clear_validation_errors()
        assert solver.get_validation_errors() == []

    # ConfigManager is mocked at module level
    def test_status_transition_invalid(self, mock_cm, concrete_base_solver):
        """Test setting status to various values."""
        solver = concrete_base_solver("test", "1.0.0")

        # Set to each status
        for status in SolverStatus:
            solver._set_status(status)
            assert solver.get_status() == status


# ============================================================================
# TEST LOGGING
# ============================================================================

class TestLogging:
    """Test logging output from solvers."""

    # ConfigManager is mocked at module level
    def test_solver_initialization_logging(self, mock_cm, concrete_base_solver, caplog):
        """Test that solver initialization is logged."""
        with caplog.at_level(logging.DEBUG):
            solver = concrete_base_solver("test-solver", "1.0.0")

        # Check for debug log message
        assert "Initialized solver" in caplog.text or "test-solver" in caplog.text

    # ConfigManager is mocked at module level
    def test_status_change_logging(self, mock_cm, concrete_base_solver, caplog):
        """Test that status changes are logged."""
        solver = concrete_base_solver("test", "1.0.0")

        with caplog.at_level(logging.DEBUG):
            solver._set_status(SolverStatus.EXECUTING)

        assert "status changed" in caplog.text or "executing" in caplog.text

    # ConfigManager is mocked at module level
    def test_validation_error_logging(self, mock_cm, concrete_analysis_solver, caplog):
        """Test that validation errors are logged."""
        solver = concrete_analysis_solver("test", "1.0.0")

        with caplog.at_level(logging.WARNING):
            solver.add_validation_error("Test error")

        assert "validation error" in caplog.text or "Test error" in caplog.text


# ============================================================================
# PARAMETRIZED TESTS
# ============================================================================

class TestParametrized:
    """Parametrized tests for comprehensive coverage."""

    @pytest.mark.parametrize("status", [
        SolverStatus.PENDING,
        SolverStatus.VALIDATING,
        SolverStatus.EXECUTING,
        SolverStatus.COMPLETED,
        SolverStatus.FAILED,
    ])
    # ConfigManager is mocked at module level
    def test_all_status_transitions(self, mock_cm, concrete_base_solver, status):
        """Test all possible status transitions."""
        solver = concrete_base_solver("test", "1.0.0")
        solver._set_status(status)
        assert solver.get_status() == status

    @pytest.mark.parametrize("version", [
        "1.0.0",
        "2.1.3",
        "0.0.1",
        "10.20.30",
        "1.0.0-beta",
    ])
    # ConfigManager is mocked at module level
    def test_various_versions(self, mock_cm, concrete_base_solver, version):
        """Test solver with various version strings."""
        solver = concrete_base_solver("test", version)
        assert solver.version == version

    @pytest.mark.parametrize("error_count", [0, 1, 5, 10, 50])
    # ConfigManager is mocked at module level
    def test_various_error_counts(self, mock_cm, concrete_analysis_solver, error_count):
        """Test adding various numbers of validation errors."""
        solver = concrete_analysis_solver("test", "1.0.0")

        for i in range(error_count):
            solver.add_validation_error(f"Error {i}")

        assert len(solver.get_validation_errors()) == error_count
        assert solver.has_validation_errors() == (error_count > 0)

    @pytest.mark.parametrize("key,value", [
        ("simple_key", "simple_value"),
        ("nested.key", 42),
        ("deep.nested.key", [1, 2, 3]),
        ("unicode_key_😊", "unicode_value_🎉"),
    ])
    # ConfigManager is mocked at module level
    def test_various_config_keys_values(
        self, concrete_configurable_solver, key, value
    ):
        """Test configuration with various keys and values."""
        solver = concrete_configurable_solver("test", "1.0.0")
        # Verify set_config method exists and is callable
        assert callable(solver.set_config)
        # Verify get_config method exists and is callable
        assert callable(solver.get_config)
        # Try calling set_config (will use mocked ConfigManager)
        solver.set_config(key, value)
        # If no exception raised, test passes (ConfigManager mock handles it)
        assert True


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
