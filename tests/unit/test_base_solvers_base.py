"""
ABOUTME: Unit tests for base_solvers.base module
ABOUTME: Test base classes, status enums, and abstract interfaces
"""

import pytest
from typing import Dict, Any, List, Tuple
from digitalmodel.base_solvers import (
    BaseSolver,
    ConfigurableSolver,
    AnalysisSolver,
    SolverStatus,
)


# Concrete implementations for testing abstract classes
class SimpleSolver(BaseSolver):
    """Concrete solver implementation for testing BaseSolver."""

    def __init__(self, name: str = "test-solver", version: str = "1.0.0"):
        super().__init__(name, version)
        self.test_inputs: Dict[str, Any] = {}

    def validate_inputs(self) -> Tuple[bool, List[str]]:
        """Validate test inputs."""
        errors = []
        if not self.test_inputs.get("value"):
            errors.append("'value' parameter is required")
        return len(errors) == 0, errors

    def solve(self) -> Dict[str, Any]:
        """Execute test solver."""
        self._set_status(SolverStatus.VALIDATING)
        is_valid, errors = self.validate_inputs()

        if not is_valid:
            self._set_status(SolverStatus.FAILED)
            return {"success": False, "errors": errors, "status": self.status.value}

        self._set_status(SolverStatus.EXECUTING)
        value = self.test_inputs.get("value", 0)
        results = {
            "success": True,
            "status": self.status.value,
            "input": value,
            "output": value * 2
        }

        self._set_status(SolverStatus.COMPLETED)
        self._cache_results(results)
        return results

    def get_solver_metadata(self) -> Dict[str, Any]:
        """Get solver metadata."""
        return {
            "name": self.name,
            "version": self.version,
            "description": "Simple test solver",
            "solver_type": "test",
            "inputs": {
                "value": {"type": "float", "required": True}
            },
            "outputs": {
                "output": {"type": "float"}
            }
        }


class TestSolverStatus:
    """Tests for SolverStatus enum."""

    def test_solver_status_values(self):
        """Test that all status values are defined."""
        assert SolverStatus.PENDING.value == "pending"
        assert SolverStatus.VALIDATING.value == "validating"
        assert SolverStatus.EXECUTING.value == "executing"
        assert SolverStatus.COMPLETED.value == "completed"
        assert SolverStatus.FAILED.value == "failed"

    def test_solver_status_members(self):
        """Test number of status members."""
        statuses = list(SolverStatus)
        assert len(statuses) == 5


class TestBaseSolver:
    """Tests for BaseSolver abstract base class."""

    def test_initialization(self):
        """Test solver initialization."""
        solver = SimpleSolver("test-solver", "1.0.0")
        assert solver.name == "test-solver"
        assert solver.version == "1.0.0"
        assert solver.status == SolverStatus.PENDING

    def test_status_change(self):
        """Test status tracking."""
        solver = SimpleSolver()
        assert solver.get_status() == SolverStatus.PENDING
        solver._set_status(SolverStatus.EXECUTING)
        assert solver.get_status() == SolverStatus.EXECUTING

    def test_validate_inputs_abstract(self):
        """Test that validate_inputs must be implemented."""
        solver = SimpleSolver()
        is_valid, errors = solver.validate_inputs()
        assert isinstance(is_valid, bool)
        assert isinstance(errors, list)

    def test_solve_execution_flow(self):
        """Test complete solve execution flow."""
        solver = SimpleSolver()
        solver.test_inputs = {"value": 10}

        results = solver.solve()

        assert results["success"] is True
        assert results["output"] == 20
        assert solver.get_status() == SolverStatus.COMPLETED

    def test_results_caching(self):
        """Test that results are properly cached."""
        solver = SimpleSolver()
        solver.test_inputs = {"value": 5}

        results1 = solver.solve()
        results2 = solver.get_results()

        assert results1["output"] == 10
        assert results2["output"] == 10
        assert results1 == results2

    def test_results_isolation(self):
        """Test that cached results cannot be modified externally."""
        solver = SimpleSolver()
        solver.test_inputs = {"value": 5}
        solver.solve()

        cached = solver.get_results()
        cached["output"] = 999

        # Original should be unchanged
        assert solver.get_results()["output"] == 10

    def test_metadata_retrieval(self):
        """Test metadata can be retrieved."""
        solver = SimpleSolver("my-solver", "2.0.0")
        metadata = solver.get_solver_metadata()

        assert metadata["name"] == "my-solver"
        assert metadata["version"] == "2.0.0"
        assert "inputs" in metadata
        assert "outputs" in metadata

    def test_validation_failure(self):
        """Test behavior when validation fails."""
        solver = SimpleSolver()
        solver.test_inputs = {}  # Missing required 'value'

        results = solver.solve()

        assert results["success"] is False
        assert len(results["errors"]) > 0
        assert solver.get_status() == SolverStatus.FAILED


class ConfigurableSolverTest(ConfigurableSolver):
    """Concrete implementation of ConfigurableSolver for testing."""

    def validate_inputs(self) -> Tuple[bool, List[str]]:
        """Validate inputs."""
        return True, []

    def solve(self) -> Dict[str, Any]:
        """Execute solver."""
        return {"success": True, "status": self.status.value}

    def get_solver_metadata(self) -> Dict[str, Any]:
        """Get metadata."""
        return {
            "name": self.name,
            "version": self.version,
            "solver_type": "test"
        }


class AnalysisSolverTest(AnalysisSolver):
    """Concrete implementation of AnalysisSolver for testing."""

    def validate_inputs(self) -> Tuple[bool, List[str]]:
        """Validate inputs."""
        return True, []

    def solve(self) -> Dict[str, Any]:
        """Execute solver."""
        return {"success": True, "status": self.status.value}

    def get_solver_metadata(self) -> Dict[str, Any]:
        """Get metadata."""
        return {
            "name": self.name,
            "version": self.version,
            "solver_type": "test"
        }


class TestConfigurableSolver:
    """Tests for ConfigurableSolver with configuration management."""

    def test_initialization_without_config(self):
        """Test initialization without initial config."""
        solver = ConfigurableSolverTest("config-solver", "1.0.0")
        assert solver.name == "config-solver"
        assert solver.version == "1.0.0"

    def test_set_and_get_config(self):
        """Test setting and getting configuration values."""
        solver = ConfigurableSolverTest("config-solver", "1.0.0")
        solver.set_config("tolerance", 1e-6)

        assert solver.get_config("tolerance") == 1e-6

    def test_get_config_with_default(self):
        """Test getting config with default value."""
        solver = ConfigurableSolverTest("config-solver", "1.0.0")
        value = solver.get_config("nonexistent", default="default_value")

        assert value == "default_value"

    def test_dot_notation_config(self):
        """Test dot notation for nested configuration."""
        solver = ConfigurableSolverTest("config-solver", "1.0.0")
        solver.set_config("solver.tolerance", 1e-6)
        solver.set_config("solver.max_iterations", 1000)

        assert solver.get_config("solver.tolerance") == 1e-6
        assert solver.get_config("solver.max_iterations") == 1000

    def test_initial_config_loading(self):
        """Test loading initial configuration."""
        initial_config = {
            "method": "direct",
            "tolerance": 1e-6
        }
        solver = ConfigurableSolverTest("config-solver", "1.0.0", initial_config)

        assert solver.get_config("method") == "direct"
        assert solver.get_config("tolerance") == 1e-6

    def test_get_all_config(self):
        """Test retrieving all configuration."""
        initial_config = {
            "method": "direct",
            "tolerance": 1e-6
        }
        solver = ConfigurableSolverTest("config-solver", "1.0.0", initial_config)
        all_config = solver.get_all_config()

        assert all_config["method"] == "direct"
        assert all_config["tolerance"] == 1e-6

    def test_config_isolation(self):
        """Test that config cannot be modified externally."""
        initial_config = {"value": 100}
        solver = ConfigurableSolverTest("config-solver", "1.0.0", initial_config)

        config = solver.get_all_config()
        config["value"] = 999

        # Original should be unchanged
        assert solver.get_config("value") == 100


class TestAnalysisSolver:
    """Tests for AnalysisSolver with input data management."""

    def test_initialization(self):
        """Test analysis solver initialization."""
        solver = AnalysisSolverTest("analysis", "1.0.0")
        assert solver.name == "analysis"
        assert solver.input_data == {}
        assert solver.validation_errors == []

    def test_set_input_data(self):
        """Test setting input data."""
        solver = AnalysisSolverTest("analysis", "1.0.0")
        data = {"x": 1, "y": 2}

        solver.set_input_data(data)

        assert solver.get_input_data() == data

    def test_input_data_isolation(self):
        """Test that input data is isolated."""
        solver = AnalysisSolverTest("analysis", "1.0.0")
        data = {"x": 1}

        solver.set_input_data(data)
        retrieved = solver.get_input_data()
        retrieved["x"] = 999

        # Original should be unchanged
        assert solver.get_input_data()["x"] == 1

    def test_validation_errors_management(self):
        """Test validation error tracking."""
        solver = AnalysisSolverTest("analysis", "1.0.0")

        solver.add_validation_error("Error 1")
        solver.add_validation_error("Error 2")

        assert len(solver.get_validation_errors()) == 2
        assert solver.has_validation_errors() is True

    def test_clear_validation_errors(self):
        """Test clearing validation errors."""
        solver = AnalysisSolverTest("analysis", "1.0.0")

        solver.add_validation_error("Error 1")
        assert solver.has_validation_errors() is True

        solver.clear_validation_errors()
        assert solver.has_validation_errors() is False
        assert solver.get_validation_errors() == []

    def test_set_validation_errors_list(self):
        """Test setting validation errors list."""
        solver = AnalysisSolverTest("analysis", "1.0.0")
        errors = ["Error 1", "Error 2", "Error 3"]

        solver.set_validation_errors(errors)

        assert len(solver.get_validation_errors()) == 3
        assert solver.has_validation_errors() is True

    def test_configuration_integration(self):
        """Test that AnalysisSolver inherits configuration management."""
        config = {"tolerance": 1e-6}
        solver = AnalysisSolverTest("analysis", "1.0.0", config)

        assert solver.get_config("tolerance") == 1e-6


class TestSolverIntegration:
    """Integration tests for solver classes."""

    def test_complete_workflow(self):
        """Test complete solver workflow."""
        # Setup
        solver = SimpleSolver("workflow-test", "1.0.0")
        solver.test_inputs = {"value": 42}

        # Execute
        results = solver.solve()

        # Verify
        assert results["success"] is True
        assert results["output"] == 84
        assert solver.get_status() == SolverStatus.COMPLETED
        assert solver.get_results() == results

    def test_configuration_and_analysis(self):
        """Test AnalysisSolver with configuration and input data."""
        # Setup
        config = {"tolerance": 1e-6, "max_iterations": 1000}
        solver = AnalysisSolverTest("test", "1.0.0", config)
        solver.set_input_data({"parameter1": 100, "parameter2": 200})

        # Verify configuration
        assert solver.get_config("tolerance") == 1e-6
        assert solver.get_config("max_iterations") == 1000

        # Verify input data
        data = solver.get_input_data()
        assert data["parameter1"] == 100
        assert data["parameter2"] == 200

    def test_multiple_solvers_independence(self):
        """Test that multiple solver instances are independent."""
        solver1 = SimpleSolver("solver1", "1.0.0")
        solver2 = SimpleSolver("solver2", "1.0.0")

        solver1.test_inputs = {"value": 10}
        solver2.test_inputs = {"value": 20}

        results1 = solver1.solve()
        results2 = solver2.solve()

        assert results1["output"] == 20
        assert results2["output"] == 40
        assert solver1.name == "solver1"
        assert solver2.name == "solver2"


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
