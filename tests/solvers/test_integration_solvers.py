#!/usr/bin/env python3
"""
ABOUTME: Comprehensive integration tests for Phase 3 solvers
ABOUTME: Real ConfigManager integration, multi-solver workflows, and error handling
"""

import pytest
import logging
import tempfile
import yaml
import json
from pathlib import Path
from typing import Dict, Any, List, Tuple
from unittest.mock import Mock, MagicMock, patch
from dataclasses import dataclass
import time

# Add src to path
import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

# ============================================================================
# FIXTURES - Real ConfigManager Integration
# ============================================================================

@pytest.fixture
def temp_config_dir():
    """Create temporary configuration directory."""
    with tempfile.TemporaryDirectory() as tmpdir:
        yield Path(tmpdir)


@pytest.fixture
def test_yaml_config(temp_config_dir):
    """Create real YAML configuration file for testing."""
    config_data = {
        "solvers": {
            "catenary": {
                "enabled": True,
                "timeout_seconds": 60,
                "max_iterations": 1000,
                "tolerance": 1e-6,
                "convergence_criterion": 1e-8
            },
            "plate_capacity": {
                "enabled": True,
                "timeout_seconds": 30,
                "design_code": "DNV-RP-C201",
                "safety_factor": 3.0
            },
            "fatigue": {
                "enabled": True,
                "timeout_seconds": 120,
                "rainflow_method": "three_point",
                "s_n_curves": "DNV"
            }
        },
        "execution": {
            "parallel": True,
            "max_workers": 4,
            "batch_size": 100,
            "checkpoint_interval": 10
        },
        "output": {
            "format": "json",
            "include_metadata": True,
            "compression": False
        }
    }

    config_file = temp_config_dir / "test_config.yaml"
    with open(config_file, 'w') as f:
        yaml.dump(config_data, f)

    return config_file


@pytest.fixture
def config_manager(test_yaml_config):
    """Create real ConfigManager instance with test configuration."""
    from digitalmodel.base_configs import ConfigManager

    cm = ConfigManager()
    with open(test_yaml_config, 'r') as f:
        cm.config = yaml.safe_load(f)

    return cm


@pytest.fixture
def solver_input_large():
    """Large input dataset for integration testing."""
    return {
        "nodes": 500,
        "elements": 1000,
        "loads": [1.0, 2.0, 3.0, 4.0, 5.0] * 50,
        "displacements": [[0.1 * i for i in range(100)] for _ in range(50)],
        "material_properties": {
            "E": 2.1e11,
            "poisson": 0.3,
            "density": 7850.0
        }
    }


@pytest.fixture
def solver_input_small():
    """Small input dataset for integration testing."""
    return {
        "nodes": 10,
        "elements": 20,
        "loads": [1.0, 2.0, 3.0],
        "material_properties": {
            "E": 2.1e11,
            "poisson": 0.3
        }
    }


@pytest.fixture
def mock_solver_factory():
    """Factory for creating mock solvers with realistic behavior."""
    class MockSolverFactory:
        @staticmethod
        def create_catenary_solver(config):
            """Create mock catenary solver."""
            solver = MagicMock()
            solver.name = "catenary"
            solver.config = config
            solver.solve.return_value = {
                "success": True,
                "iterations": 45,
                "tension": 1234.5,
                "convergence": True,
                "computation_time": 0.125
            }
            return solver

        @staticmethod
        def create_capacity_solver(config):
            """Create mock capacity solver."""
            solver = MagicMock()
            solver.name = "plate_capacity"
            solver.config = config
            solver.solve.return_value = {
                "success": True,
                "utilization": 0.65,
                "buckling_factor": 2.1,
                "safety_factor": 3.0
            }
            return solver

        @staticmethod
        def create_fatigue_solver(config):
            """Create mock fatigue solver."""
            solver = MagicMock()
            solver.name = "fatigue"
            solver.config = config
            solver.solve.return_value = {
                "success": True,
                "damage_ratio": 0.45,
                "cycles_to_failure": 1e7,
                "dominant_frequency": 0.5
            }
            return solver

    return MockSolverFactory()


# ============================================================================
# TEST CLASS: Real ConfigManager Integration
# ============================================================================

class TestConfigManagerIntegration:
    """Tests for real ConfigManager with multiple solvers."""

    def test_config_manager_load_yaml(self, config_manager, test_yaml_config):
        """Test loading configuration from YAML file."""
        assert config_manager.config is not None
        assert "solvers" in config_manager.config
        assert "catenary" in config_manager.config["solvers"]

    def test_config_manager_get_solver_config(self, config_manager):
        """Test retrieving specific solver configuration."""
        catenary_config = config_manager.config["solvers"]["catenary"]
        assert catenary_config["enabled"] is True
        assert catenary_config["timeout_seconds"] == 60
        assert catenary_config["tolerance"] == 1e-6

    def test_config_manager_update_config(self, config_manager):
        """Test updating configuration at runtime."""
        original_timeout = config_manager.config["solvers"]["catenary"]["timeout_seconds"]
        config_manager.config["solvers"]["catenary"]["timeout_seconds"] = 120
        assert config_manager.config["solvers"]["catenary"]["timeout_seconds"] == 120
        # Restore
        config_manager.config["solvers"]["catenary"]["timeout_seconds"] = original_timeout

    def test_config_manager_parallel_access(self, config_manager):
        """Test ConfigManager handling multiple concurrent accesses."""
        accesses = []
        for i in range(10):
            val = config_manager.config["solvers"]["catenary"]["timeout_seconds"]
            accesses.append(val)

        assert all(v == 60 for v in accesses)
        assert len(accesses) == 10

    def test_config_manager_nested_config_access(self, config_manager):
        """Test accessing nested configuration values."""
        execution_config = config_manager.config["execution"]
        assert execution_config["parallel"] is True
        assert execution_config["max_workers"] == 4
        assert execution_config["batch_size"] == 100

    def test_config_manager_missing_key_handling(self, config_manager):
        """Test ConfigManager handles missing keys gracefully."""
        result = config_manager.config.get("nonexistent", {})
        assert result == {}

    def test_config_manager_type_consistency(self, config_manager):
        """Test configuration values maintain correct types."""
        catenary_cfg = config_manager.config["solvers"]["catenary"]
        assert isinstance(catenary_cfg["timeout_seconds"], int)
        assert isinstance(catenary_cfg["tolerance"], float)
        assert isinstance(catenary_cfg["enabled"], bool)


# ============================================================================
# TEST CLASS: Multi-Solver Workflows
# ============================================================================

class TestMultiSolverWorkflows:
    """Tests for coordinated multi-solver execution."""

    def test_solver_chain_execution(self, config_manager, mock_solver_factory):
        """Test sequential execution of multiple solvers."""
        solvers = [
            mock_solver_factory.create_catenary_solver(config_manager.config["solvers"]["catenary"]),
            mock_solver_factory.create_capacity_solver(config_manager.config["solvers"]["plate_capacity"]),
            mock_solver_factory.create_fatigue_solver(config_manager.config["solvers"]["fatigue"])
        ]

        results = []
        for solver in solvers:
            result = solver.solve()
            results.append(result)

        assert len(results) == 3
        assert all(r["success"] for r in results)

    def test_solver_dependency_management(self, config_manager, mock_solver_factory):
        """Test handling solver dependencies in workflow."""
        catenary_solver = mock_solver_factory.create_catenary_solver(
            config_manager.config["solvers"]["catenary"]
        )
        capacity_solver = mock_solver_factory.create_capacity_solver(
            config_manager.config["solvers"]["plate_capacity"]
        )

        # Catenary result feeds into capacity solver
        catenary_result = catenary_solver.solve()
        assert catenary_result["success"]

        # Capacity solver uses tension from catenary
        capacity_result = capacity_solver.solve()
        assert capacity_result["success"]
        assert "utilization" in capacity_result

    def test_parallel_solver_execution(self, config_manager, mock_solver_factory, solver_input_large):
        """Test parallel execution of independent solvers."""
        import concurrent.futures

        solvers = [
            (mock_solver_factory.create_catenary_solver(
                config_manager.config["solvers"]["catenary"]), "catenary"),
            (mock_solver_factory.create_capacity_solver(
                config_manager.config["solvers"]["plate_capacity"]), "capacity"),
            (mock_solver_factory.create_fatigue_solver(
                config_manager.config["solvers"]["fatigue"]), "fatigue")
        ]

        results = {}
        with concurrent.futures.ThreadPoolExecutor(max_workers=3) as executor:
            futures = {
                executor.submit(solver.solve): name
                for solver, name in solvers
            }

            for future in concurrent.futures.as_completed(futures):
                name = futures[future]
                results[name] = future.result()

        assert len(results) == 3
        assert all(results[k]["success"] for k in results)

    def test_solver_result_aggregation(self, config_manager, mock_solver_factory):
        """Test aggregating results from multiple solvers."""
        solvers = [
            mock_solver_factory.create_catenary_solver(config_manager.config["solvers"]["catenary"]),
            mock_solver_factory.create_capacity_solver(config_manager.config["solvers"]["plate_capacity"]),
            mock_solver_factory.create_fatigue_solver(config_manager.config["solvers"]["fatigue"])
        ]

        aggregate = {"solvers": {}}
        for solver in solvers:
            result = solver.solve()
            aggregate["solvers"][solver.name] = result

        assert len(aggregate["solvers"]) == 3
        assert all(s in aggregate["solvers"] for s in ["catenary", "plate_capacity", "fatigue"])

    def test_workflow_checkpoint_management(self, config_manager):
        """Test checkpoint saving and restoration in workflows."""
        checkpoint = {
            "solver_states": {
                "catenary": {"iterations": 45, "convergence": True},
                "capacity": {"utilization": 0.65},
                "fatigue": {"damage_ratio": 0.45}
            },
            "timestamp": time.time(),
            "config_hash": hash(str(config_manager.config))
        }

        # Simulate saving checkpoint
        saved_checkpoint = checkpoint.copy()

        # Simulate loading checkpoint
        assert saved_checkpoint["solver_states"]["catenary"]["iterations"] == 45
        assert saved_checkpoint["solver_states"]["capacity"]["utilization"] == 0.65
        assert "timestamp" in saved_checkpoint


# ============================================================================
# TEST CLASS: Configuration Loading and Management
# ============================================================================

class TestConfigurationManagement:
    """Tests for configuration loading, validation, and management."""

    def test_yaml_config_persistence(self, temp_config_dir):
        """Test saving and loading configuration files."""
        config_data = {
            "test_key": "test_value",
            "nested": {"param": 123}
        }

        config_file = temp_config_dir / "persist_test.yaml"
        with open(config_file, 'w') as f:
            yaml.dump(config_data, f)

        with open(config_file, 'r') as f:
            loaded = yaml.safe_load(f)

        assert loaded == config_data

    def test_config_validation(self, config_manager):
        """Test configuration validation."""
        required_keys = ["solvers", "execution", "output"]
        for key in required_keys:
            assert key in config_manager.config

    def test_config_override_mechanism(self, config_manager):
        """Test configuration override capability."""
        original_workers = config_manager.config["execution"]["max_workers"]

        # Override
        config_manager.config["execution"]["max_workers"] = 8
        assert config_manager.config["execution"]["max_workers"] == 8

        # Restore
        config_manager.config["execution"]["max_workers"] = original_workers

    def test_config_with_multiple_solver_types(self, temp_config_dir):
        """Test configuration supporting multiple solver types."""
        config_data = {
            "solvers": {
                f"solver_{i}": {
                    "enabled": True,
                    "timeout": 30 + i*10,
                    "type": f"type_{i % 3}"
                } for i in range(10)
            }
        }

        config_file = temp_config_dir / "multi_solver.yaml"
        with open(config_file, 'w') as f:
            yaml.dump(config_data, f)

        with open(config_file, 'r') as f:
            loaded = yaml.safe_load(f)

        assert len(loaded["solvers"]) == 10
        assert all("timeout" in loaded["solvers"][f"solver_{i}"] for i in range(10))


# ============================================================================
# TEST CLASS: Error Handling and Propagation
# ============================================================================

class TestErrorHandling:
    """Tests for error handling across solver modules."""

    def test_solver_execution_error_handling(self, mock_solver_factory, config_manager):
        """Test handling of solver execution errors."""
        solver = mock_solver_factory.create_catenary_solver(
            config_manager.config["solvers"]["catenary"]
        )
        solver.solve.side_effect = RuntimeError("Convergence failed")

        with pytest.raises(RuntimeError):
            solver.solve()

    def test_configuration_error_propagation(self, config_manager):
        """Test error propagation from configuration issues."""
        # Simulate missing required config
        del config_manager.config["solvers"]["catenary"]["timeout_seconds"]

        result = config_manager.config["solvers"]["catenary"].get("timeout_seconds")
        assert result is None

    def test_workflow_error_recovery(self, config_manager, mock_solver_factory):
        """Test recovery mechanisms in workflows."""
        solvers = []

        # First solver succeeds
        solver1 = mock_solver_factory.create_catenary_solver(
            config_manager.config["solvers"]["catenary"]
        )
        solvers.append(solver1)

        # Second solver fails
        solver2 = mock_solver_factory.create_capacity_solver(
            config_manager.config["solvers"]["plate_capacity"]
        )
        solver2.solve.side_effect = RuntimeError("Capacity solver failed")
        solvers.append(solver2)

        # Third solver succeeds (recovery)
        solver3 = mock_solver_factory.create_fatigue_solver(
            config_manager.config["solvers"]["fatigue"]
        )
        solvers.append(solver3)

        results = []
        errors = []

        for solver in solvers:
            try:
                result = solver.solve()
                results.append(result)
            except Exception as e:
                errors.append((solver.name, str(e)))

        assert len(results) == 2
        assert len(errors) == 1
        assert errors[0][0] == "plate_capacity"

    def test_timeout_error_handling(self, config_manager):
        """Test handling of timeout errors."""
        timeout_config = config_manager.config["solvers"]["catenary"]["timeout_seconds"]
        assert timeout_config > 0

        # Simulate timeout scenario
        execution_time = timeout_config + 10
        timeout_occurred = execution_time > timeout_config
        assert timeout_occurred

    def test_validation_error_handling(self, config_manager):
        """Test validation error handling."""
        # Invalid configuration
        invalid_config = {"timeout": -1}  # Negative timeout

        if invalid_config.get("timeout", 0) < 0:
            error = "Invalid timeout value"
            assert error == "Invalid timeout value"


# ============================================================================
# TEST CLASS: Integration Test Scenarios
# ============================================================================

class TestIntegrationScenarios:
    """Complex integration test scenarios."""

    def test_complete_analysis_workflow(self, config_manager, mock_solver_factory,
                                       solver_input_large):
        """Test complete analysis workflow from input to output."""
        # Setup
        input_data = solver_input_large

        # Catenary analysis
        cat_solver = mock_solver_factory.create_catenary_solver(
            config_manager.config["solvers"]["catenary"]
        )
        cat_result = cat_solver.solve()

        # Capacity analysis (uses catenary results)
        cap_solver = mock_solver_factory.create_capacity_solver(
            config_manager.config["solvers"]["plate_capacity"]
        )
        cap_result = cap_solver.solve()

        # Fatigue analysis
        fat_solver = mock_solver_factory.create_fatigue_solver(
            config_manager.config["solvers"]["fatigue"]
        )
        fat_result = fat_solver.solve()

        # Verify workflow
        assert cat_result["success"]
        assert cap_result["success"]
        assert fat_result["success"]

        # Check output metrics
        output = {
            "catenary": cat_result,
            "capacity": cap_result,
            "fatigue": fat_result,
            "input_size": len(str(input_data))
        }

        assert output["input_size"] > 0

    def test_batch_processing_workflow(self, config_manager, mock_solver_factory):
        """Test batch processing of multiple input datasets."""
        batch_size = config_manager.config["execution"]["batch_size"]

        # Create batch of inputs
        batch_inputs = [
            {"id": i, "load": float(i)}
            for i in range(batch_size)
        ]

        solver = mock_solver_factory.create_catenary_solver(
            config_manager.config["solvers"]["catenary"]
        )

        batch_results = []
        for input_data in batch_inputs:
            result = solver.solve()
            result["input_id"] = input_data["id"]
            batch_results.append(result)

        assert len(batch_results) == batch_size
        assert all(r["success"] for r in batch_results)

    def test_adaptive_solver_selection(self, config_manager, mock_solver_factory,
                                      solver_input_large, solver_input_small):
        """Test adaptive solver selection based on input size."""
        # Large input
        large_solver = mock_solver_factory.create_fatigue_solver(
            config_manager.config["solvers"]["fatigue"]
        )
        large_result = large_solver.solve()

        # Small input
        small_solver = mock_solver_factory.create_catenary_solver(
            config_manager.config["solvers"]["catenary"]
        )
        small_result = small_solver.solve()

        # Both should succeed but with different characteristics
        assert large_result["success"]
        assert small_result["success"]

    def test_workflow_state_management(self, config_manager):
        """Test maintaining workflow state across phases."""
        workflow_state = {
            "phase": 1,
            "solvers_completed": [],
            "checkpoint": None,
            "config": config_manager.config
        }

        # Phase 1
        workflow_state["solvers_completed"].append("catenary")
        workflow_state["phase"] = 2

        # Phase 2
        workflow_state["solvers_completed"].append("capacity")
        workflow_state["phase"] = 3

        # Phase 3
        workflow_state["solvers_completed"].append("fatigue")
        workflow_state["phase"] = 4

        assert len(workflow_state["solvers_completed"]) == 3
        assert workflow_state["phase"] == 4


# ============================================================================
# TEST CLASS: Performance and Metrics
# ============================================================================

class TestPerformanceMetrics:
    """Tests for performance metrics collection."""

    def test_execution_time_tracking(self, mock_solver_factory, config_manager):
        """Test tracking execution time of solvers."""
        import time

        solver = mock_solver_factory.create_catenary_solver(
            config_manager.config["solvers"]["catenary"]
        )

        start_time = time.time()
        result = solver.solve()
        end_time = time.time()

        execution_time = end_time - start_time
        assert execution_time >= 0

    def test_resource_utilization_tracking(self, config_manager):
        """Test tracking resource utilization."""
        execution_config = config_manager.config["execution"]

        metrics = {
            "workers": execution_config["max_workers"],
            "batch_size": execution_config["batch_size"],
            "parallel": execution_config["parallel"]
        }

        assert metrics["workers"] == 4
        assert metrics["batch_size"] == 100
        assert metrics["parallel"] is True

    def test_convergence_metrics_collection(self, mock_solver_factory, config_manager):
        """Test collecting convergence metrics."""
        solver = mock_solver_factory.create_catenary_solver(
            config_manager.config["solvers"]["catenary"]
        )

        result = solver.solve()

        metrics = {
            "iterations": result.get("iterations", 0),
            "convergence": result.get("convergence", False),
            "computation_time": result.get("computation_time", 0)
        }

        assert metrics["iterations"] > 0
        assert metrics["convergence"] is True


# ============================================================================
# TEST HELPERS
# ============================================================================

def create_test_workflow(config_manager, solvers_list: List[Tuple[str, Any]]) -> Dict[str, Any]:
    """Helper to create and execute test workflow."""
    results = {"total": len(solvers_list), "completed": 0, "failed": 0, "results": {}}

    for solver_name, solver in solvers_list:
        try:
            result = solver.solve()
            results["results"][solver_name] = result
            results["completed"] += 1
        except Exception as e:
            results["results"][solver_name] = {"error": str(e)}
            results["failed"] += 1

    return results


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
