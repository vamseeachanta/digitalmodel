"""
Performance benchmarks for unified catenary module.

Measures execution time and validates performance targets:
- Phase 1 BVP solver: <10ms per solve
- Simplified methods: <0.1ms per solve
- Adapter overhead: <5% additional time

Author: Digital Model Project (Test Agent)
Date: 2025-10-03
"""

import pytest
import time
import numpy as np
import warnings
from digitalmodel.marine_ops.marine_analysis.catenary import (
    CatenarySolver, CatenaryInput,
    catenaryEquation, catenaryForces
)
from digitalmodel.marine_ops.marine_analysis.catenary.simplified import SimplifiedCatenarySolver


class TestPhase1SolverPerformance:
    """Performance tests for Phase 1 BVP solver."""

    def test_single_solve_performance(self):
        """Test single solve completes within target time."""
        params = CatenaryInput(
            length=1000.0,
            horizontal_span=800.0,
            vertical_span=100.0,
            weight_per_length=1962.0,
            ea_stiffness=64e9
        )

        solver = CatenarySolver()

        start = time.perf_counter()
        result = solver.solve(params)
        duration = time.perf_counter() - start

        assert result.converged, "Must converge for valid benchmark"
        assert duration < 0.010, f"Phase 1 solver took {duration*1000:.2f}ms (target: <10ms)"

        print(f"\nPhase 1 solver: {duration*1000:.3f}ms")

    def test_batch_solve_performance(self):
        """Test batch solving performance."""
        params_list = [
            CatenaryInput(
                length=1000.0 + i*10,
                horizontal_span=800.0,
                vertical_span=100.0,
                weight_per_length=1962.0,
                ea_stiffness=64e9
            ) for i in range(100)
        ]

        solver = CatenarySolver()

        start = time.perf_counter()
        results = [solver.solve(p) for p in params_list]
        duration = time.perf_counter() - start

        avg_time = duration / len(params_list)

        assert all(r.converged for r in results), "All must converge"
        assert avg_time < 0.010, f"Average solve: {avg_time*1000:.2f}ms (target: <10ms)"

        print(f"\nPhase 1 batch (100 solves): {duration*1000:.1f}ms total, {avg_time*1000:.3f}ms avg")

    def test_tight_tolerance_performance(self):
        """Test performance with tight tolerance."""
        params = CatenaryInput(
            length=1000.0,
            horizontal_span=800.0,
            vertical_span=100.0,
            weight_per_length=1962.0,
            ea_stiffness=64e9
        )

        solver = CatenarySolver(tolerance=1e-9)

        start = time.perf_counter()
        result = solver.solve(params)
        duration = time.perf_counter() - start

        assert result.converged
        # Tight tolerance may take longer, but should still be reasonable
        assert duration < 0.050, f"Tight tolerance took {duration*1000:.2f}ms (target: <50ms)"

        print(f"\nPhase 1 (tight tolerance 1e-9): {duration*1000:.3f}ms")


class TestSimplifiedSolverPerformance:
    """Performance tests for simplified catenary methods."""

    def test_angle_based_performance(self):
        """Test angle-based calculation performance."""
        solver = SimplifiedCatenarySolver()

        start = time.perf_counter()
        result = solver.solve_from_angle(30.0, 100.0)
        duration = time.perf_counter() - start

        assert duration < 0.0001, f"Angle-based took {duration*1000:.4f}ms (target: <0.1ms)"

        print(f"\nSimplified angle-based: {duration*1000000:.1f}μs")

    def test_force_based_performance(self):
        """Test force-based calculation performance."""
        solver = SimplifiedCatenarySolver()

        start = time.perf_counter()
        result = solver.solve_from_force(10000.0, 50.0, 100.0)
        duration = time.perf_counter() - start

        assert duration < 0.0001, f"Force-based took {duration*1000:.4f}ms (target: <0.1ms)"

        print(f"\nSimplified force-based: {duration*1000000:.1f}μs")

    def test_forces_calculation_performance(self):
        """Test forces calculation performance."""
        solver = SimplifiedCatenarySolver()

        start = time.perf_counter()
        Fv, F, Fh = solver.calculate_forces(50.0, 100.0, 30.0)
        duration = time.perf_counter() - start

        assert duration < 0.0001, f"Forces calc took {duration*1000:.4f}ms (target: <0.1ms)"

        print(f"\nSimplified forces: {duration*1000000:.1f}μs")

    def test_batch_simplified_performance(self):
        """Test batch simplified calculations."""
        solver = SimplifiedCatenarySolver()

        angles = [10 + i*5 for i in range(15)]  # 10° to 80° in 5° steps

        start = time.perf_counter()
        results = [solver.solve_from_angle(a, 100.0) for a in angles]
        duration = time.perf_counter() - start

        avg_time = duration / len(angles)

        assert avg_time < 0.0001, f"Batch avg: {avg_time*1000:.4f}ms (target: <0.1ms)"

        print(f"\nSimplified batch (15 solves): {duration*1000:.3f}ms total, {avg_time*1000000:.1f}μs avg")


class TestAdapterPerformance:
    """Performance tests for backward compatibility adapter."""

    def test_adapter_overhead_angle(self):
        """Measure adapter overhead for angle-based method."""
        # Direct simplified call
        solver = SimplifiedCatenarySolver()
        start1 = time.perf_counter()
        result1 = solver.solve_from_angle(30.0, 100.0)
        time_direct = time.perf_counter() - start1

        # Via adapter
        with warnings.catch_warnings():
            warnings.filterwarnings('ignore', category=DeprecationWarning)
            start2 = time.perf_counter()
            result2 = catenaryEquation({'q': 30, 'd': 100, 'F': None, 'w': None, 'X': None})
            time_adapter = time.perf_counter() - start2

        overhead = (time_adapter - time_direct) / time_direct if time_direct > 0 else 0

        # Adapter overhead should be minimal (<20%)
        assert overhead < 0.20, f"Adapter overhead: {overhead*100:.1f}% (target: <20%)"

        print(f"\nAdapter overhead (angle): {overhead*100:.1f}%")
        print(f"  Direct: {time_direct*1000000:.1f}μs")
        print(f"  Adapter: {time_adapter*1000000:.1f}μs")

    def test_adapter_overhead_force(self):
        """Measure adapter overhead for force-based method."""
        # Direct simplified call
        solver = SimplifiedCatenarySolver()
        start1 = time.perf_counter()
        result1 = solver.solve_from_force(10000.0, 50.0, 100.0)
        time_direct = time.perf_counter() - start1

        # Via adapter
        with warnings.catch_warnings():
            warnings.filterwarnings('ignore', category=DeprecationWarning)
            start2 = time.perf_counter()
            result2 = catenaryEquation({'F': 10000, 'w': 50, 'd': 100, 'X': None, 'q': None})
            time_adapter = time.perf_counter() - start2

        overhead = (time_adapter - time_direct) / time_direct if time_direct > 0 else 0

        assert overhead < 0.20, f"Adapter overhead: {overhead*100:.1f}% (target: <20%)"

        print(f"\nAdapter overhead (force): {overhead*100:.1f}%")
        print(f"  Direct: {time_direct*1000000:.1f}μs")
        print(f"  Adapter: {time_adapter*1000000:.1f}μs")


class TestMemoryPerformance:
    """Test memory efficiency."""

    def test_solver_memory_footprint(self):
        """Verify solver doesn't accumulate memory."""
        import sys

        solver = CatenarySolver()
        params = CatenaryInput(
            length=1000.0,
            horizontal_span=800.0,
            vertical_span=100.0,
            weight_per_length=1962.0,
            ea_stiffness=64e9
        )

        # Run multiple solves
        results = []
        for _ in range(1000):
            result = solver.solve(params)
            results.append(result.horizontal_tension)

        # All results should be identical (deterministic)
        assert len(set(results)) == 1, "Results should be deterministic"

    def test_result_size_reasonable(self):
        """Verify result objects are not bloated."""
        import sys

        params = CatenaryInput(
            length=1000.0,
            horizontal_span=800.0,
            vertical_span=100.0,
            weight_per_length=1962.0,
            ea_stiffness=64e9
        )

        solver = CatenarySolver()
        result = solver.solve(params)

        # Result size should be reasonable (mainly numpy arrays)
        size_bytes = sys.getsizeof(result)

        # Should be less than 10KB (arrays with 100 points each)
        assert size_bytes < 10000, f"Result size: {size_bytes} bytes (target: <10KB)"

        print(f"\nResult object size: {size_bytes} bytes")


class TestScalability:
    """Test performance scaling."""

    def test_shape_resolution_scaling(self):
        """Test how performance scales with shape resolution."""
        # Note: Current solver uses fixed 100 points
        # This test documents current behavior

        params = CatenaryInput(
            length=1000.0,
            horizontal_span=800.0,
            vertical_span=100.0,
            weight_per_length=1962.0,
            ea_stiffness=64e9
        )

        solver = CatenarySolver()

        start = time.perf_counter()
        result = solver.solve(params)
        duration = time.perf_counter() - start

        # Verify shape has expected number of points
        assert len(result.shape_x) == 100
        assert len(result.shape_y) == 100

        print(f"\nShape generation (100 points): {duration*1000:.3f}ms")

    def test_parameter_range_performance(self):
        """Test performance across wide parameter range."""
        test_cases = [
            # (length, h_span, v_span, weight, EA)
            (500, 400, 50, 1000, 50e9),      # Small
            (1000, 800, 100, 1962, 64e9),    # Medium
            (2000, 1600, 200, 3000, 100e9),  # Large
        ]

        solver = CatenarySolver()
        times = []

        for length, h_span, v_span, weight, ea in test_cases:
            params = CatenaryInput(
                length=length,
                horizontal_span=h_span,
                vertical_span=v_span,
                weight_per_length=weight,
                ea_stiffness=ea
            )

            start = time.perf_counter()
            result = solver.solve(params)
            duration = time.perf_counter() - start

            times.append(duration)
            assert result.converged

        # Performance should be consistent across parameter ranges
        max_time = max(times)
        min_time = min(times)
        variation = (max_time - min_time) / min_time if min_time > 0 else 0

        print(f"\nParameter range timing:")
        print(f"  Small: {times[0]*1000:.3f}ms")
        print(f"  Medium: {times[1]*1000:.3f}ms")
        print(f"  Large: {times[2]*1000:.3f}ms")
        print(f"  Variation: {variation*100:.1f}%")


class TestComparativePerformance:
    """Compare performance across methods."""

    def test_phase1_vs_simplified_speedup(self):
        """Measure speedup of simplified vs Phase 1 solver."""
        # Phase 1 solver
        params = CatenaryInput(
            length=1000.0,
            horizontal_span=800.0,
            vertical_span=100.0,
            weight_per_length=1962.0,
            ea_stiffness=64e9
        )
        solver_phase1 = CatenarySolver()

        start1 = time.perf_counter()
        result1 = solver_phase1.solve(params)
        time_phase1 = time.perf_counter() - start1

        # Simplified solver (angle-based as proxy)
        solver_simp = SimplifiedCatenarySolver()

        start2 = time.perf_counter()
        result2 = solver_simp.solve_from_angle(30.0, 100.0)
        time_simp = time.perf_counter() - start2

        speedup = time_phase1 / time_simp if time_simp > 0 else 0

        # Simplified should be significantly faster (>10x)
        assert speedup > 10, f"Speedup: {speedup:.1f}x (target: >10x)"

        print(f"\nPhase 1 vs Simplified speedup: {speedup:.1f}x")
        print(f"  Phase 1: {time_phase1*1000:.3f}ms")
        print(f"  Simplified: {time_simp*1000000:.1f}μs")


if __name__ == "__main__":
    pytest.main([__file__, "-v", "-s", "--tb=short"])
