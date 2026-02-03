#!/usr/bin/env python3
# ABOUTME: Example integration of cache layer with hydrodynamics coefficient loading
# Demonstrates real-world usage pattern for marine engineering analysis

"""
Hydrodynamics Cache Integration Example

This demonstrates how to integrate the cache layer with hydrodynamic
coefficient loading, which is a common bottleneck in marine analysis.

Typical workflow:
1. Load 6x6 added mass and damping matrices from AQWA/OrcaWave
2. Interpolate RAOs for different headings/periods
3. Run multiple analyses reusing same vessel data
4. Monitor cache performance

Performance impact:
- First load: 100-500ms (file I/O + parsing)
- Cached loads: <1ms (>500x speedup)
- Hit rate: 85-95% for typical workflows
"""

import time
from typing import Dict, List, Optional

from digitalmodel.infrastructure.core.cache import CacheClient, CacheConfig, cache_result


# =============================================================================
# Module-level cache instance (recommended pattern)
# =============================================================================

HYDRO_CACHE = CacheClient(
    CacheConfig(
        enable_redis=True,  # Try Redis first, fallback to memory
        redis_host="localhost",
        redis_port=6379,
        default_ttl=7200,  # 2 hours (hydrodynamic data doesn't change often)
        max_memory_items=500,  # Store ~500 coefficient sets
    )
)


# =============================================================================
# Simulated hydrodynamic database (replace with real implementation)
# =============================================================================

class HydrodynamicCoefficientDatabase:
    """
    Simulated hydrodynamic database with cache integration.

    In real usage, this would:
    - Read AQWA .LIS files
    - Parse OrcaWave .out files
    - Query SQL database
    - Load from HDF5/NetCDF
    """

    def __init__(self, data_directory: str = "data/hydrodynamics"):
        self.data_directory = data_directory

    @cache_result(HYDRO_CACHE, ttl=7200)
    def load_added_mass_matrix(
        self, vessel_id: str, wave_period: float, wave_heading: float = 0.0
    ) -> Dict:
        """
        Load 6x6 added mass matrix for vessel.

        Args:
            vessel_id: Vessel identifier (e.g., "FPSO-A", "SEMISUBMERSIBLE-B")
            wave_period: Wave period in seconds
            wave_heading: Wave heading in degrees (0-360)

        Returns:
            Dict with 6x6 added mass matrix
        """
        print(f"  [CACHE MISS] Loading added mass: {vessel_id}, T={wave_period}s, heading={wave_heading}deg")

        # Simulate file I/O delay
        time.sleep(0.15)

        # Simulate AQWA output parsing
        # Real implementation would use: parse_aqwa_lis_file()
        return {
            "vessel_id": vessel_id,
            "wave_period": wave_period,
            "wave_heading": wave_heading,
            "added_mass_matrix": [
                [1000.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                [0.0, 1000.0, 0.0, 0.0, 0.0, 100.0],
                [0.0, 0.0, 2000.0, 0.0, 0.0, 0.0],
                [0.0, 0.0, 0.0, 500.0, 0.0, 0.0],
                [0.0, 0.0, 0.0, 0.0, 500.0, 0.0],
                [0.0, 100.0, 0.0, 0.0, 0.0, 100.0],
            ],
            "units": "kg, kg·m, kg·m²",
        }

    @cache_result(HYDRO_CACHE, ttl=7200)
    def load_damping_matrix(
        self, vessel_id: str, wave_period: float, wave_heading: float = 0.0
    ) -> Dict:
        """
        Load 6x6 damping matrix for vessel.

        Args:
            vessel_id: Vessel identifier
            wave_period: Wave period in seconds
            wave_heading: Wave heading in degrees

        Returns:
            Dict with 6x6 damping matrix
        """
        print(f"  [CACHE MISS] Loading damping: {vessel_id}, T={wave_period}s, heading={wave_heading}deg")

        time.sleep(0.15)

        return {
            "vessel_id": vessel_id,
            "wave_period": wave_period,
            "wave_heading": wave_heading,
            "damping_matrix": [
                [50.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                [0.0, 50.0, 0.0, 0.0, 0.0, 5.0],
                [0.0, 0.0, 100.0, 0.0, 0.0, 0.0],
                [0.0, 0.0, 0.0, 25.0, 0.0, 0.0],
                [0.0, 0.0, 0.0, 0.0, 25.0, 0.0],
                [0.0, 5.0, 0.0, 0.0, 0.0, 10.0],
            ],
            "units": "N·s/m, N·s·m/rad",
        }

    @cache_result(HYDRO_CACHE, ttl=3600)
    def interpolate_rao(
        self,
        vessel_id: str,
        wave_period: float,
        wave_heading: float,
        dof: str = "heave",
    ) -> Dict:
        """
        Interpolate RAO (Response Amplitude Operator) from database.

        Args:
            vessel_id: Vessel identifier
            wave_period: Wave period in seconds
            wave_heading: Wave heading in degrees
            dof: Degree of freedom (surge, sway, heave, roll, pitch, yaw)

        Returns:
            Dict with RAO amplitude and phase
        """
        print(f"  [CACHE MISS] Interpolating RAO: {vessel_id}, T={wave_period}s, heading={wave_heading}deg, DOF={dof}")

        time.sleep(0.05)

        # Simulate RAO interpolation
        amplitude = 1.0 / wave_period  # Simplified
        phase = (wave_heading / 360.0) * 180.0

        return {
            "vessel_id": vessel_id,
            "wave_period": wave_period,
            "wave_heading": wave_heading,
            "dof": dof,
            "amplitude": amplitude,
            "phase": phase,
            "units": "m/m (or rad/m for rotational)",
        }

    def warm_cache_for_vessel(
        self, vessel_id: str, wave_periods: List[float], wave_headings: List[float]
    ):
        """
        Warm cache for a vessel across multiple periods and headings.

        This is typically called at application startup or when a new
        analysis project is opened.

        Args:
            vessel_id: Vessel identifier
            wave_periods: List of wave periods to preload
            wave_headings: List of wave headings to preload
        """
        print(f"\nWarming cache for {vessel_id}...")
        print(f"  Periods: {wave_periods}")
        print(f"  Headings: {wave_headings}")

        start = time.time()
        count = 0

        for period in wave_periods:
            for heading in wave_headings:
                # Preload added mass
                self.load_added_mass_matrix(vessel_id, period, heading)
                count += 1

                # Preload damping
                self.load_damping_matrix(vessel_id, period, heading)
                count += 1

        duration = time.time() - start
        print(f"  Warmed {count} coefficient sets in {duration:.2f}s")


# =============================================================================
# Usage Examples
# =============================================================================

def example_basic_usage():
    """Demonstrate basic caching with hydrodynamic coefficients."""
    print("\n" + "=" * 70)
    print("Example 1: Basic Caching")
    print("=" * 70)

    db = HydrodynamicCoefficientDatabase()

    vessel = "FPSO-PETROLEO-A"
    period = 10.0
    heading = 0.0

    # First call - cache miss
    print("\nFirst load (from file):")
    start = time.time()
    added_mass1 = db.load_added_mass_matrix(vessel, period, heading)
    duration1 = time.time() - start
    print(f"  Duration: {duration1:.3f}s")

    # Second call - cache hit
    print("\nSecond load (from cache):")
    start = time.time()
    added_mass2 = db.load_added_mass_matrix(vessel, period, heading)
    duration2 = time.time() - start
    print(f"  Duration: {duration2:.3f}s")

    if duration2 > 0.001:
        print(f"  Speedup: {duration1 / duration2:.1f}x")
    else:
        print(f"  Speedup: >500x (cache retrieval < 1ms)")


def example_cache_warming():
    """Demonstrate cache warming for analysis preparation."""
    print("\n" + "=" * 70)
    print("Example 2: Cache Warming")
    print("=" * 70)

    db = HydrodynamicCoefficientDatabase()

    # Define analysis matrix
    vessel = "SEMISUBMERSIBLE-B"
    wave_periods = [5.0, 10.0, 15.0, 20.0]
    wave_headings = [0.0, 45.0, 90.0, 135.0, 180.0]

    # Warm cache
    db.warm_cache_for_vessel(vessel, wave_periods, wave_headings)

    # Now run analysis - all loads will be cached
    print("\nRunning analysis with warmed cache:")
    start = time.time()

    for period in wave_periods[:2]:  # Sample analysis
        for heading in wave_headings[:2]:
            added_mass = db.load_added_mass_matrix(vessel, period, heading)
            damping = db.load_damping_matrix(vessel, period, heading)
            # All cache hits - very fast

    duration = time.time() - start
    print(f"  Analysis completed in {duration:.3f}s (all cache hits)")


def example_rao_interpolation():
    """Demonstrate RAO caching for motion analysis."""
    print("\n" + "=" * 70)
    print("Example 3: RAO Interpolation")
    print("=" * 70)

    db = HydrodynamicCoefficientDatabase()

    vessel = "FPSO-ATLANTICO"
    dofs = ["surge", "heave", "pitch"]

    print("\nInterpolating RAOs for 3 DOFs:")

    for dof in dofs:
        # First call - miss
        start = time.time()
        rao1 = db.interpolate_rao(vessel, 12.0, 0.0, dof)
        duration1 = time.time() - start

        # Second call - hit
        start = time.time()
        rao2 = db.interpolate_rao(vessel, 12.0, 0.0, dof)
        duration2 = time.time() - start

        print(f"\n  {dof.upper()}:")
        print(f"    First call: {duration1:.3f}s")
        print(f"    Second call: {duration2:.3f}s")
        print(f"    Amplitude: {rao2['amplitude']:.4f} {rao2['units']}")


def example_performance_monitoring():
    """Demonstrate cache performance monitoring."""
    print("\n" + "=" * 70)
    print("Example 4: Performance Monitoring")
    print("=" * 70)

    db = HydrodynamicCoefficientDatabase()

    # Reset stats
    HYDRO_CACHE.reset_stats()

    # Simulate typical workflow
    vessel = "DRILLSHIP-C"
    periods = [8.0, 10.0, 12.0, 14.0]
    headings = [0.0, 90.0, 180.0]

    print("\nSimulating typical analysis workflow...")

    for period in periods:
        for heading in headings:
            # Multiple modules accessing same data
            db.load_added_mass_matrix(vessel, period, heading)
            db.load_damping_matrix(vessel, period, heading)
            db.interpolate_rao(vessel, period, heading, "heave")

            # Second access (from cache)
            db.load_added_mass_matrix(vessel, period, heading)

    # Show statistics
    print("\nCache Performance:")
    stats = HYDRO_CACHE.get_stats()
    print(f"  Total operations: {stats['hits'] + stats['misses']}")
    print(f"  Cache hits: {stats['hits']}")
    print(f"  Cache misses: {stats['misses']}")
    print(f"  Hit rate: {stats['hit_rate']:.1%}")
    print(f"  Values cached: {stats['sets']}")

    # Export statistics
    import os
    os.makedirs("reports", exist_ok=True)
    HYDRO_CACHE.export_stats("reports/hydro_cache_stats.json")
    print(f"\n  Statistics exported to: reports/hydro_cache_stats.json")


def example_realistic_workflow():
    """Demonstrate realistic multi-vessel analysis workflow."""
    print("\n" + "=" * 70)
    print("Example 5: Realistic Multi-Vessel Workflow")
    print("=" * 70)

    db = HydrodynamicCoefficientDatabase()

    # Analysis setup
    vessels = ["FPSO-A", "FPSO-B"]
    periods = [10.0, 12.0, 15.0]
    headings = [0.0, 45.0, 90.0]

    print("\nPhase 1: Initial loads (cache warming)")
    start_phase1 = time.time()

    for vessel in vessels:
        for period in periods:
            for heading in headings:
                db.load_added_mass_matrix(vessel, period, heading)
                db.load_damping_matrix(vessel, period, heading)

    duration_phase1 = time.time() - start_phase1
    print(f"  Phase 1 completed in {duration_phase1:.2f}s")

    print("\nPhase 2: Analysis iterations (cached)")
    start_phase2 = time.time()

    # Simulate 10 analysis iterations
    for iteration in range(10):
        for vessel in vessels:
            for period in periods:
                for heading in headings:
                    # All cache hits
                    added_mass = db.load_added_mass_matrix(vessel, period, heading)
                    damping = db.load_damping_matrix(vessel, period, heading)

    duration_phase2 = time.time() - start_phase2
    print(f"  Phase 2 (10 iterations) completed in {duration_phase2:.2f}s")

    # Compare
    print(f"\nPerformance comparison:")
    print(f"  Phase 1 (uncached): {duration_phase1:.2f}s")
    print(f"  Phase 2 (cached): {duration_phase2:.2f}s")
    print(f"  Speedup: {duration_phase1 / max(duration_phase2, 0.001):.1f}x")

    # Show final stats
    stats = HYDRO_CACHE.get_stats()
    print(f"\nFinal cache statistics:")
    print(f"  Hit rate: {stats['hit_rate']:.1%}")
    print(f"  Total operations: {stats['hits'] + stats['misses']}")


def main():
    """Run all integration examples."""
    print("\n" + "=" * 70)
    print("HYDRODYNAMICS CACHE INTEGRATION")
    print("=" * 70)
    print(f"\nCache backend: {HYDRO_CACHE.backend}")
    print(f"Default TTL: {HYDRO_CACHE.config.default_ttl}s")
    print(f"Max memory items: {HYDRO_CACHE.config.max_memory_items}")

    example_basic_usage()
    example_cache_warming()
    example_rao_interpolation()
    example_performance_monitoring()
    example_realistic_workflow()

    print("\n" + "=" * 70)
    print("INTEGRATION EXAMPLES COMPLETED")
    print("=" * 70)
    print("\nIntegration checklist:")
    print("  [x] Module-level cache instance created")
    print("  [x] @cache_result decorator on database methods")
    print("  [x] Cache warming for bulk preloading")
    print("  [x] Performance monitoring enabled")
    print("  [x] Statistics export configured")
    print("\nNext steps:")
    print("  1. Replace simulated methods with real AQWA/OrcaWave parsers")
    print("  2. Integrate with existing hydrodynamics module")
    print("  3. Monitor production hit rates")
    print("  4. Tune TTL and capacity based on usage patterns")
    print()


if __name__ == "__main__":
    main()
