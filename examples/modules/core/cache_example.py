#!/usr/bin/env python3
# ABOUTME: Example demonstrating cache usage for hydrodynamic coefficient loading
# Shows both decorator and explicit client API patterns with metrics tracking

"""
Cache Layer Example - Hydrodynamic Coefficient Caching

This example demonstrates using the cache layer to speed up repeated
loading of hydrodynamic coefficients, which is a common operation in
marine engineering analysis.

Features demonstrated:
1. Decorator-based caching (@cache_result)
2. Explicit cache client API (cache.get/set)
3. Cache warming for bulk data
4. Statistics tracking and export
5. Redis fallback to in-memory LRU
"""

import time
from typing import Dict, List

from digitalmodel.core.cache import CacheClient, CacheConfig, cache_result


# Example 1: Decorator-based caching
def example_decorator_caching():
    """Demonstrate @cache_result decorator for automatic caching."""
    print("\n" + "=" * 60)
    print("Example 1: Decorator-Based Caching")
    print("=" * 60)

    # Create cache client (will fallback to memory if Redis unavailable)
    config = CacheConfig(enable_redis=False, max_memory_items=100)
    cache = CacheClient(config)

    @cache_result(cache, ttl=3600)
    def load_added_mass_matrix(vessel_id: str, wave_period: float) -> Dict:
        """
        Simulate expensive hydrodynamic coefficient loading.

        In real usage, this would load from AQWA/OrcaWave output files
        or query a hydrodynamic database.
        """
        print(f"  Computing added mass for vessel={vessel_id}, T={wave_period}s...")
        time.sleep(0.1)  # Simulate computation time

        # Return 6x6 added mass matrix (simplified)
        return {
            "vessel_id": vessel_id,
            "wave_period": wave_period,
            "added_mass": [
                [1000.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                [0.0, 1000.0, 0.0, 0.0, 0.0, 0.0],
                [0.0, 0.0, 1000.0, 0.0, 0.0, 0.0],
                [0.0, 0.0, 0.0, 100.0, 0.0, 0.0],
                [0.0, 0.0, 0.0, 0.0, 100.0, 0.0],
                [0.0, 0.0, 0.0, 0.0, 0.0, 100.0],
            ],
        }

    # First call - will compute and cache
    print("\nFirst call (cache miss):")
    start = time.time()
    result1 = load_added_mass_matrix("FPSO-A", 10.0)
    duration1 = time.time() - start
    print(f"  Duration: {duration1:.3f}s")

    # Second call - will use cache
    print("\nSecond call (cache hit):")
    start = time.time()
    result2 = load_added_mass_matrix("FPSO-A", 10.0)
    duration2 = time.time() - start
    print(f"  Duration: {duration2:.3f}s")
    if duration2 > 0.001:  # Avoid division by zero
        print(f"  Speedup: {duration1 / duration2:.1f}x faster")
    else:
        print(f"  Speedup: >1000x faster (cache retrieval < 1ms)")

    # Different parameters - will compute again
    print("\nThird call with different parameters (cache miss):")
    start = time.time()
    result3 = load_added_mass_matrix("FPSO-A", 15.0)
    duration3 = time.time() - start
    print(f"  Duration: {duration3:.3f}s")

    # Show statistics
    print("\nCache Statistics:")
    stats = cache.get_stats()
    for key, value in stats.items():
        print(f"  {key}: {value}")


# Example 2: Explicit client API
def example_explicit_client():
    """Demonstrate explicit cache.get/set API for manual control."""
    print("\n" + "=" * 60)
    print("Example 2: Explicit Client API")
    print("=" * 60)

    config = CacheConfig(enable_redis=False)
    cache = CacheClient(config)

    def compute_rao(vessel_id: str, heading: float, wave_period: float) -> Dict:
        """Simulate RAO computation."""
        print(f"  Computing RAO for {vessel_id}, heading={heading}°, T={wave_period}s")
        time.sleep(0.05)
        return {
            "amplitude": 1.0 / wave_period,
            "phase": heading / 2.0,
        }

    # Manual cache key generation
    cache_key = f"rao:FPSO-A:heading=45:period=10"

    # Check cache first
    print("\nChecking cache...")
    result = cache.get(cache_key)
    if result is None:
        print("  Cache miss - computing...")
        result = compute_rao("FPSO-A", 45.0, 10.0)
        cache.set(cache_key, result, ttl=7200)
    else:
        print("  Cache hit!")

    print(f"  Result: {result}")

    # Second retrieval - will hit cache
    print("\nSecond retrieval...")
    result = cache.get(cache_key)
    print("  Cache hit!")
    print(f"  Result: {result}")


# Example 3: Cache warming
def example_cache_warming():
    """Demonstrate cache warming for bulk data preloading."""
    print("\n" + "=" * 60)
    print("Example 3: Cache Warming")
    print("=" * 60)

    config = CacheConfig(enable_redis=False)
    cache = CacheClient(config)

    def compute_damping_coefficient(wave_period: float) -> Dict:
        """Simulate damping coefficient computation."""
        time.sleep(0.05)
        return {
            "wave_period": wave_period,
            "linear_damping": 50.0 / wave_period,
            "quadratic_damping": 10.0 / (wave_period**2),
        }

    # Warm cache for common wave periods
    print("\nWarming cache for wave periods: 5s, 10s, 15s, 20s...")
    wave_periods = [(5.0,), (10.0,), (15.0,), (20.0,)]

    start = time.time()
    cache.warm(compute_damping_coefficient, wave_periods)
    duration = time.time() - start

    print(f"  Warmed {len(wave_periods)} entries in {duration:.3f}s")

    # Now retrieve from cache (fast)
    print("\nRetrieving from warmed cache:")
    for period_tuple in wave_periods:
        period = period_tuple[0]
        key = f"compute_damping_coefficient:({period},)"
        result = cache.get(key)
        print(f"  T={period}s: linear={result['linear_damping']:.2f}")

    # Show statistics
    stats = cache.get_stats()
    print(f"\nCache hits: {stats['hits']}, Sets: {stats['sets']}")


# Example 4: Statistics export
def example_stats_export(output_dir: str = "reports"):
    """Demonstrate statistics tracking and export."""
    print("\n" + "=" * 60)
    print("Example 4: Statistics Export")
    print("=" * 60)

    import json
    import os

    config = CacheConfig(enable_redis=False)
    cache = CacheClient(config)

    @cache_result(cache, ttl=3600)
    def expensive_operation(x: float) -> float:
        time.sleep(0.01)
        return x**2

    # Generate some cache activity
    print("\nGenerating cache activity...")
    for i in range(10):
        expensive_operation(i % 5)  # Will create hits and misses

    # Export stats
    os.makedirs(output_dir, exist_ok=True)
    stats_file = os.path.join(output_dir, "cache_stats.json")

    cache.export_stats(stats_file)
    print(f"\nStatistics exported to: {stats_file}")

    # Show stats
    with open(stats_file) as f:
        stats = json.load(f)

    print("\nCache Performance:")
    print(f"  Total operations: {stats['hits'] + stats['misses']}")
    print(f"  Cache hits: {stats['hits']}")
    print(f"  Cache misses: {stats['misses']}")
    print(f"  Hit rate: {stats['hit_rate']:.1%}")
    print(f"  Values cached: {stats['sets']}")


# Example 5: Real-world integration
def example_realistic_usage():
    """Demonstrate realistic integration pattern."""
    print("\n" + "=" * 60)
    print("Example 5: Realistic Integration Pattern")
    print("=" * 60)

    # Global cache instance (typically module-level)
    HYDRO_CACHE = CacheClient(
        CacheConfig(
            enable_redis=True,  # Try Redis first
            redis_host="localhost",
            redis_port=6379,
            default_ttl=7200,  # 2 hours
            max_memory_items=500,
        )
    )

    @cache_result(HYDRO_CACHE, ttl=7200)
    def load_hydrodynamic_database(
        vessel_id: str, wave_heading: float, wave_periods: List[float]
    ) -> Dict:
        """
        Load complete hydrodynamic database for a vessel.

        In real usage, this would:
        - Read AQWA .LIS or OrcaWave .out files
        - Parse 6x6 added mass and damping matrices
        - Interpolate RAOs
        - Return structured coefficient data
        """
        print(f"\n  Loading hydro DB for {vessel_id}, heading={wave_heading}°...")
        time.sleep(0.2)  # Simulate file I/O

        # Simulate returning structured data
        return {
            "vessel_id": vessel_id,
            "heading": wave_heading,
            "periods": wave_periods,
            "added_mass_matrices": {T: [[0.0] * 6 for _ in range(6)] for T in wave_periods},
            "damping_matrices": {T: [[0.0] * 6 for _ in range(6)] for T in wave_periods},
            "raos": {T: {"surge": 1.0, "heave": 0.5, "pitch": 0.1} for T in wave_periods},
        }

    # Simulate typical workflow
    print("\nFirst analysis run (loading from files):")
    start = time.time()
    data1 = load_hydrodynamic_database("FPSO-001", 0.0, [5.0, 10.0, 15.0, 20.0])
    duration1 = time.time() - start
    print(f"  Loaded {len(data1['periods'])} periods in {duration1:.3f}s")

    print("\nSecond analysis run (from cache):")
    start = time.time()
    data2 = load_hydrodynamic_database("FPSO-001", 0.0, [5.0, 10.0, 15.0, 20.0])
    duration2 = time.time() - start
    print(f"  Loaded {len(data2['periods'])} periods in {duration2:.3f}s")
    if duration2 > 0.001:
        print(f"  Speedup: {duration1 / duration2:.1f}x faster")
    else:
        print(f"  Speedup: >1000x faster (cache retrieval < 1ms)")

    # Show cache backend
    backend = HYDRO_CACHE.backend
    print(f"\nCache backend: {backend}")
    print(f"  (Redis available: {backend == 'redis'})")


def main():
    """Run all examples."""
    print("\n" + "=" * 60)
    print("CACHE LAYER EXAMPLES - HYDRODYNAMIC COEFFICIENTS")
    print("=" * 60)

    example_decorator_caching()
    example_explicit_client()
    example_cache_warming()
    example_stats_export()
    example_realistic_usage()

    print("\n" + "=" * 60)
    print("ALL EXAMPLES COMPLETED")
    print("=" * 60)
    print("\nKey takeaways:")
    print("  1. Use @cache_result decorator for automatic caching")
    print("  2. Use cache.get/set for manual control")
    print("  3. Use cache.warm() to preload bulk data")
    print("  4. Monitor performance with cache.get_stats()")
    print("  5. Cache automatically falls back to memory if Redis unavailable")
    print()


if __name__ == "__main__":
    main()
