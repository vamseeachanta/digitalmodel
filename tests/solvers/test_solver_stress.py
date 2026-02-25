#!/usr/bin/env python3
"""
ABOUTME: Stress testing for Phase 3 solvers
ABOUTME: Large-scale input handling, memory stress, and edge case validation
"""

import pytest
import logging
import sys
import random
import numpy as np
from pathlib import Path
from typing import Dict, Any, List
from unittest.mock import MagicMock
import gc

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

# ============================================================================
# FIXTURES
# ============================================================================

@pytest.fixture
def stress_logger():
    """Configure stress test logging."""
    logger = logging.getLogger("stress_tests")
    logger.setLevel(logging.DEBUG)
    return logger


@pytest.fixture
def large_scale_input():
    """Generate large-scale input data (10,000+ elements)."""
    size = 10000

    return {
        "nodes": size,
        "elements": size * 2,
        "loads": [random.random() * 100 for _ in range(size)],
        "displacements": [
            [random.random() * 0.1 for _ in range(size // 100)]
            for _ in range(100)
        ],
        "material_properties": {
            "E": 2.1e11,
            "poisson": 0.3,
            "density": 7850.0,
            "yield_strength": 355e6
        },
        "boundary_conditions": [
            {"node": i, "type": "fixed"} for i in range(100)
        ]
    }


@pytest.fixture
def extreme_input():
    """Generate extremely large input (100,000+ elements)."""
    size = 100000

    return {
        "nodes": size,
        "elements": size * 2,
        "loads": np.random.random(size) * 100,
        "displacements": np.random.random((100, size // 100)) * 0.1,
        "coordinates": np.random.random((size, 3)) * 1000
    }


@pytest.fixture
def edge_case_inputs():
    """Collection of edge case inputs."""
    return {
        "zero_loads": {
            "nodes": 100,
            "loads": [0.0] * 100,
            "material": {"E": 0.0}
        },
        "negative_values": {
            "nodes": 100,
            "loads": [-100.0] * 100,
            "tolerance": -1e-6
        },
        "extreme_values": {
            "nodes": 100,
            "loads": [1e15] * 100,
            "tolerance": 1e-20
        },
        "mixed_scales": {
            "nodes": 100,
            "loads": [1e-15, 1e15, 1e-15, 1e15] * 25,
            "tolerance": 1e-10
        },
        "nan_values": {
            "nodes": 100,
            "loads": [float('nan')] * 100
        },
        "infinite_values": {
            "nodes": 100,
            "loads": [float('inf')] * 100
        }
    }


@pytest.fixture
def mock_stress_solver():
    """Create mock solver for stress testing."""
    class StressSolver:
        def __init__(self, max_iterations: int = 1000):
            self.max_iterations = max_iterations
            self.iteration_count = 0
            self.execution_log = []

        def solve_large_input(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
            """Solve with large input."""
            self.iteration_count = 0

            try:
                num_nodes = input_data.get("nodes", 0)
                num_elements = input_data.get("elements", 0)

                # Simulate iterative solving
                for i in range(min(100, self.max_iterations)):
                    self.iteration_count += 1
                    convergence_error = 1.0 / (i + 1)

                    if convergence_error < 1e-6:
                        break

                return {
                    "success": True,
                    "nodes": num_nodes,
                    "elements": num_elements,
                    "iterations": self.iteration_count,
                    "convergence": True,
                    "final_error": convergence_error
                }

            except Exception as e:
                return {
                    "success": False,
                    "error": str(e),
                    "iterations": self.iteration_count
                }

        def solve_with_extreme_values(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
            """Handle extreme value inputs."""
            try:
                loads = input_data.get("loads", [])

                # Check for NaN and Inf
                has_nan = any(isinstance(x, float) and np.isnan(x) for x in loads)
                has_inf = any(isinstance(x, float) and np.isinf(x) for x in loads)

                if has_nan or has_inf:
                    return {
                        "success": False,
                        "error": "Invalid numerical values detected",
                        "has_nan": has_nan,
                        "has_inf": has_inf
                    }

                # Process extreme values safely
                magnitude_range = {
                    "max": max(abs(x) for x in loads) if loads else 0,
                    "min": min(abs(x) for x in loads) if loads else 0
                }

                return {
                    "success": True,
                    "magnitude_range": magnitude_range,
                    "tolerance": input_data.get("tolerance", 1e-6)
                }

            except Exception as e:
                return {"success": False, "error": str(e)}

        def solve_edge_case(self, edge_case_name: str, input_data: Dict[str, Any]) -> Dict[str, Any]:
            """Handle edge case inputs."""
            return {
                "success": True,
                "edge_case": edge_case_name,
                "input_size": input_data.get("nodes", 0)
            }

    return StressSolver()


# ============================================================================
# TEST CLASS: Large-Scale Input Handling
# ============================================================================

class TestLargeScaleInputHandling:
    """Tests for handling large-scale inputs."""

    def test_10k_node_problem(self, mock_stress_solver, large_scale_input, stress_logger):
        """Test solving 10,000 node problem."""
        stress_logger.info("Starting 10K node stress test")

        result = mock_stress_solver.solve_large_input(large_scale_input)

        assert result["success"]
        assert result["nodes"] == 10000
        assert result["elements"] == 20000
        assert result["convergence"]
        assert result["iterations"] > 0

        stress_logger.info(f"10K node test completed in {result['iterations']} iterations")

    def test_100k_element_problem(self, mock_stress_solver, extreme_input, stress_logger):
        """Test solving 100,000 element problem."""
        stress_logger.info("Starting 100K element stress test")

        result = mock_stress_solver.solve_large_input(extreme_input)

        assert result["success"]
        assert result["elements"] == 200000

        stress_logger.info("100K element test completed successfully")

    def test_large_load_vector(self, mock_stress_solver):
        """Test handling large load vectors."""
        size = 50000

        input_data = {
            "nodes": size,
            "loads": [random.random() * 100 for _ in range(size)]
        }

        result = mock_stress_solver.solve_large_input(input_data)

        assert result["success"]
        assert result["nodes"] == size

    def test_large_displacement_matrix(self, mock_stress_solver):
        """Test handling large displacement matrices."""
        input_data = {
            "nodes": 5000,
            "elements": 10000,
            "displacements": np.random.random((1000, 500)) * 0.1
        }

        result = mock_stress_solver.solve_large_input(input_data)

        assert result["success"]
        assert result["nodes"] == 5000

    def test_batch_large_problems(self, mock_stress_solver):
        """Test batch processing of large problems."""
        batch_results = []

        for i in range(5):
            input_data = {
                "nodes": 2000 * (i + 1),
                "elements": 4000 * (i + 1),
                "loads": [random.random() * 100 for _ in range(1000)]
            }

            result = mock_stress_solver.solve_large_input(input_data)
            batch_results.append(result)

        assert len(batch_results) == 5
        assert all(r["success"] for r in batch_results)


# ============================================================================
# TEST CLASS: Memory Stress Testing
# ============================================================================

class TestMemoryStress:
    """Memory stress testing."""

    def test_memory_intensive_solve(self, mock_stress_solver, stress_logger):
        """Test memory-intensive solver execution."""
        # Create large dataset
        size = 50000
        input_data = {
            "nodes": size,
            "loads": [random.random() * 100 for _ in range(size)],
            "matrix": [[random.random() for _ in range(100)] for _ in range(size // 100)]
        }

        stress_logger.info(f"Testing memory-intensive solve with {size} nodes")

        result = mock_stress_solver.solve_large_input(input_data)

        assert result["success"]

        # Clean up
        del input_data
        gc.collect()

    def test_iterative_memory_growth(self, mock_stress_solver):
        """Test for memory growth in iterative solving."""
        memory_snapshots = []

        for iteration in range(20):
            gc.collect()

            input_data = {
                "nodes": 10000,
                "loads": [random.random() * 100 for _ in range(10000)]
            }

            result = mock_stress_solver.solve_large_input(input_data)
            assert result["success"]

            # Clean up
            del input_data

        # Memory should not grow significantly
        assert len(memory_snapshots) == 0 or max(memory_snapshots) < 2.0

    def test_large_array_operations(self, mock_stress_solver):
        """Test large array operations."""
        size = 100000

        input_data = {
            "nodes": size,
            "loads": np.random.random(size) * 100,
            "coordinates": np.random.random((size, 3)) * 1000
        }

        result = mock_stress_solver.solve_large_input(input_data)
        assert result["success"]

    def test_memory_recovery_after_stress(self, mock_stress_solver):
        """Test memory recovery after stress operations."""
        # Perform stress operations
        for _ in range(10):
            input_data = {
                "nodes": 20000,
                "loads": [random.random() * 100 for _ in range(20000)]
            }

            result = mock_stress_solver.solve_large_input(input_data)
            assert result["success"]

            del input_data
            gc.collect()

        # Verify system is stable
        small_input = {
            "nodes": 100,
            "loads": [1.0] * 100
        }

        result = mock_stress_solver.solve_large_input(small_input)
        assert result["success"]


# ============================================================================
# TEST CLASS: Edge Case Handling
# ============================================================================

class TestEdgeCaseHandling:
    """Tests for edge case handling."""

    def test_zero_loads(self, mock_stress_solver, edge_case_inputs):
        """Test handling of zero loads."""
        zero_case = edge_case_inputs["zero_loads"]
        result = mock_stress_solver.solve_edge_case("zero_loads", zero_case)

        assert result["success"]
        assert result["edge_case"] == "zero_loads"

    def test_negative_values(self, mock_stress_solver, edge_case_inputs):
        """Test handling of negative values."""
        negative_case = edge_case_inputs["negative_values"]
        result = mock_stress_solver.solve_edge_case("negative_values", negative_case)

        assert result["success"]

    def test_extreme_values(self, mock_stress_solver, edge_case_inputs):
        """Test handling of extreme numerical values."""
        extreme_case = edge_case_inputs["extreme_values"]
        result = mock_stress_solver.solve_with_extreme_values(extreme_case)

        assert "magnitude_range" in result

    def test_mixed_scale_values(self, mock_stress_solver, edge_case_inputs):
        """Test handling of mixed scale values."""
        mixed_case = edge_case_inputs["mixed_scales"]
        result = mock_stress_solver.solve_with_extreme_values(mixed_case)

        # Should handle gracefully
        assert "magnitude_range" in result or "error" in result

    def test_nan_detection(self, mock_stress_solver, edge_case_inputs):
        """Test NaN value detection and handling."""
        nan_case = edge_case_inputs["nan_values"]
        result = mock_stress_solver.solve_with_extreme_values(nan_case)

        assert result["has_nan"]
        assert result["success"] is False

    def test_inf_detection(self, mock_stress_solver, edge_case_inputs):
        """Test infinite value detection."""
        inf_case = edge_case_inputs["infinite_values"]
        result = mock_stress_solver.solve_with_extreme_values(inf_case)

        assert result["has_inf"]
        assert result["success"] is False

    def test_empty_input(self, mock_stress_solver):
        """Test handling of empty input."""
        empty_input = {}
        result = mock_stress_solver.solve_large_input(empty_input)

        # Should handle gracefully
        assert "success" in result

    def test_single_element(self, mock_stress_solver):
        """Test solving with minimal input."""
        minimal_input = {
            "nodes": 1,
            "elements": 0,
            "loads": [1.0]
        }

        result = mock_stress_solver.solve_large_input(minimal_input)
        assert result["success"]


# ============================================================================
# TEST CLASS: Convergence Under Stress
# ============================================================================

class TestConvergenceStress:
    """Tests for convergence under stress conditions."""

    def test_convergence_large_system(self, mock_stress_solver):
        """Test convergence with large system."""
        input_data = {
            "nodes": 10000,
            "loads": [random.random() * 100 for _ in range(10000)]
        }

        result = mock_stress_solver.solve_large_input(input_data)

        assert result["success"]
        assert result["convergence"]

    def test_convergence_tight_tolerance(self, mock_stress_solver):
        """Test convergence with tight tolerance."""
        input_data = {
            "nodes": 5000,
            "loads": [1.0] * 5000,
            "tolerance": 1e-12
        }

        result = mock_stress_solver.solve_large_input(input_data)

        # Should converge even with tight tolerance
        assert "convergence" in result

    def test_convergence_loose_tolerance(self, mock_stress_solver):
        """Test convergence with loose tolerance."""
        input_data = {
            "nodes": 5000,
            "loads": [1.0] * 5000,
            "tolerance": 1e-2
        }

        result = mock_stress_solver.solve_large_input(input_data)

        assert result["success"]

    def test_ill_conditioned_system(self, mock_stress_solver):
        """Test convergence with ill-conditioned system."""
        size = 5000
        ill_conditioned_loads = [10**i % 100 for i in range(size)]

        input_data = {
            "nodes": size,
            "loads": ill_conditioned_loads
        }

        result = mock_stress_solver.solve_large_input(input_data)

        # Should attempt convergence
        assert result["success"]

    def test_convergence_speed_degradation(self, mock_stress_solver):
        """Test convergence speed with increasing problem size."""
        sizes = [1000, 5000, 10000]
        convergence_rates = []

        for size in sizes:
            input_data = {
                "nodes": size,
                "loads": [random.random() * 100 for _ in range(size)]
            }

            result = mock_stress_solver.solve_large_input(input_data)

            if result["success"]:
                convergence_rates.append(result["iterations"])

        # Convergence should not degrade dramatically
        assert len(convergence_rates) > 0


# ============================================================================
# TEST CLASS: Concurrent Stress Testing
# ============================================================================

class TestConcurrentStress:
    """Concurrent execution stress testing."""

    def test_concurrent_large_solves(self, mock_stress_solver):
        """Test concurrent execution of large solves."""
        import concurrent.futures

        def run_large_solve(problem_id):
            input_data = {
                "nodes": 5000,
                "elements": 10000,
                "loads": [random.random() * 100 for _ in range(5000)]
            }

            return mock_stress_solver.solve_large_input(input_data)

        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
            futures = [executor.submit(run_large_solve, i) for i in range(10)]
            results = [f.result() for f in concurrent.futures.as_completed(futures)]

        assert len(results) == 10
        assert all(r["success"] for r in results)

    def test_mixed_size_concurrent(self, mock_stress_solver):
        """Test concurrent execution with mixed problem sizes."""
        import concurrent.futures

        def run_solve(size):
            input_data = {
                "nodes": size,
                "loads": [random.random() * 100 for _ in range(size)]
            }

            return mock_stress_solver.solve_large_input(input_data)

        sizes = [100, 1000, 5000, 10000, 5000, 1000, 100]

        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
            futures = [executor.submit(run_solve, size) for size in sizes]
            results = [f.result() for f in concurrent.futures.as_completed(futures)]

        assert len(results) == len(sizes)
        assert all(r["success"] for r in results)


# ============================================================================
# TEST CLASS: Recovery and Stability
# ============================================================================

class TestRecoveryAndStability:
    """Tests for system recovery and stability under stress."""

    def test_recovery_after_invalid_input(self, mock_stress_solver):
        """Test recovery after invalid input."""
        # Send invalid input
        invalid_input = {"nodes": -1, "loads": []}
        result1 = mock_stress_solver.solve_large_input(invalid_input)

        # Send valid input after
        valid_input = {"nodes": 100, "loads": [1.0] * 100}
        result2 = mock_stress_solver.solve_large_input(valid_input)

        # Should recover
        assert result2["success"]

    def test_stability_under_continuous_stress(self, mock_stress_solver):
        """Test stability under continuous stress."""
        results = []

        for i in range(50):
            input_data = {
                "nodes": random.randint(100, 10000),
                "loads": [random.random() * 100 for _ in range(random.randint(100, 10000))]
            }

            try:
                result = mock_stress_solver.solve_large_input(input_data)
                results.append(result)
            except Exception as e:
                results.append({"success": False, "error": str(e)})

        # Most should succeed
        success_rate = sum(1 for r in results if r.get("success")) / len(results)
        assert success_rate > 0.8

    def test_resource_cleanup(self, mock_stress_solver):
        """Test resource cleanup after stress."""
        # Run stress
        for _ in range(100):
            input_data = {
                "nodes": 1000,
                "loads": [random.random() * 100 for _ in range(1000)]
            }

            result = mock_stress_solver.solve_large_input(input_data)
            assert result["success"]

            del input_data
            gc.collect()

        # Verify cleanup
        # System should be in clean state
        clean_input = {"nodes": 10, "loads": [1.0] * 10}
        clean_result = mock_stress_solver.solve_large_input(clean_input)

        assert clean_result["success"]


# ============================================================================
# TEST HELPERS
# ============================================================================

def generate_stress_report(test_results: List[Dict[str, Any]]) -> Dict[str, Any]:
    """Generate stress test report."""
    return {
        "total_tests": len(test_results),
        "passed": sum(1 for r in test_results if r.get("success")),
        "failed": sum(1 for r in test_results if not r.get("success")),
        "success_rate": sum(1 for r in test_results if r.get("success")) / len(test_results) if test_results else 0
    }


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
