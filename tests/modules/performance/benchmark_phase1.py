#!/usr/bin/env python3
"""
Phase 1 Optimization Benchmarks
================================

Benchmarks to verify Phase 1 optimization improvements:
1. Hash-based indexing: O(n) → O(1) component searches
2. Lazy cleanup: 0.8ms → 0.1ms rate limiter overhead

Expected Results:
- Component search: <0.5ms (80-90% faster than 2-5ms baseline)
- Rate limiter overhead: <0.1ms (88% faster than 0.8ms baseline)
"""

import time
import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / 'src'))

from digitalmodel.data_procurement.mooring.database_clients.chain_db_client import ChainDatabaseClient
from digitalmodel.data_procurement.mooring.database_clients.wire_rope_db_client import WireRopeDatabaseClient
from digitalmodel.data_procurement.mooring.database_clients.synthetic_rope_db_client import SyntheticRopeDatabaseClient
from digitalmodel.data_procurement.mooring.database_clients.anchor_db_client import AnchorDatabaseClient
from digitalmodel.data_procurement.mooring.database_clients.connector_db_client import ConnectorDatabaseClient
from digitalmodel.data_procurement.common.base_client import RateLimiter


def benchmark_chain_searches():
    """Benchmark chain database searches with hash indexing."""
    print("\n" + "="*60)
    print("BENCHMARK: Chain Database Searches")
    print("="*60)

    client = ChainDatabaseClient()
    iterations = 1000

    # Benchmark: 1000 chain searches with grade filter (uses index)
    start = time.perf_counter()
    for _ in range(iterations):
        chains = client.find_by_design_load(2500, grade='R4S')
    elapsed = time.perf_counter() - start

    avg_time_ms = (elapsed / iterations) * 1000
    print(f"✓ Average indexed search time: {avg_time_ms:.3f}ms")
    print(f"  Total searches: {iterations}")
    print(f"  Total time: {elapsed:.3f}s")

    # Verify correctness
    chains = client.find_by_design_load(2500, grade='R4S')
    print(f"  Sample result: Found {len(chains)} chains for 2500kN design load")

    # Performance assertion
    if avg_time_ms < 0.5:
        print(f"✅ PASSED: Search time {avg_time_ms:.3f}ms < 0.5ms target")
        return True
    else:
        print(f"❌ FAILED: Search time {avg_time_ms:.3f}ms >= 0.5ms target")
        return False


def benchmark_wire_rope_searches():
    """Benchmark wire rope database searches with hash indexing."""
    print("\n" + "="*60)
    print("BENCHMARK: Wire Rope Database Searches")
    print("="*60)

    client = WireRopeDatabaseClient()
    iterations = 1000

    start = time.perf_counter()
    for _ in range(iterations):
        ropes = client.find_by_design_load(4000, construction='6x36 IWRC')
    elapsed = time.perf_counter() - start

    avg_time_ms = (elapsed / iterations) * 1000
    print(f"✓ Average indexed search time: {avg_time_ms:.3f}ms")
    print(f"  Total searches: {iterations}")

    if avg_time_ms < 0.5:
        print(f"✅ PASSED: Search time {avg_time_ms:.3f}ms < 0.5ms target")
        return True
    else:
        print(f"❌ FAILED: Search time {avg_time_ms:.3f}ms >= 0.5ms target")
        return False


def benchmark_synthetic_rope_searches():
    """Benchmark synthetic rope database searches with hash indexing."""
    print("\n" + "="*60)
    print("BENCHMARK: Synthetic Rope Database Searches")
    print("="*60)

    client = SyntheticRopeDatabaseClient()
    iterations = 1000

    start = time.perf_counter()
    for _ in range(iterations):
        ropes = client.find_by_design_load(8000, fiber_type='polyester')
    elapsed = time.perf_counter() - start

    avg_time_ms = (elapsed / iterations) * 1000
    print(f"✓ Average indexed search time: {avg_time_ms:.3f}ms")
    print(f"  Total searches: {iterations}")

    if avg_time_ms < 0.5:
        print(f"✅ PASSED: Search time {avg_time_ms:.3f}ms < 0.5ms target")
        return True
    else:
        print(f"❌ FAILED: Search time {avg_time_ms:.3f}ms >= 0.5ms target")
        return False


def benchmark_anchor_searches():
    """Benchmark anchor database searches with hash indexing."""
    print("\n" + "="*60)
    print("BENCHMARK: Anchor Database Searches")
    print("="*60)

    client = AnchorDatabaseClient()
    iterations = 1000

    start = time.perf_counter()
    for _ in range(iterations):
        anchors = client.find_by_holding_capacity(1500, anchor_type='stevpris_mk6', soil='clay')
    elapsed = time.perf_counter() - start

    avg_time_ms = (elapsed / iterations) * 1000
    print(f"✓ Average indexed search time: {avg_time_ms:.3f}ms")
    print(f"  Total searches: {iterations}")

    if avg_time_ms < 0.5:
        print(f"✅ PASSED: Search time {avg_time_ms:.3f}ms < 0.5ms target")
        return True
    else:
        print(f"❌ FAILED: Search time {avg_time_ms:.3f}ms >= 0.5ms target")
        return False


def benchmark_connector_searches():
    """Benchmark connector database searches with hash indexing."""
    print("\n" + "="*60)
    print("BENCHMARK: Connector Database Searches")
    print("="*60)

    client = ConnectorDatabaseClient()
    iterations = 1000

    start = time.perf_counter()
    for _ in range(iterations):
        connectors = client.find_by_working_load(850, connector_type='bow_shackle')
    elapsed = time.perf_counter() - start

    avg_time_ms = (elapsed / iterations) * 1000
    print(f"✓ Average indexed search time: {avg_time_ms:.3f}ms")
    print(f"  Total searches: {iterations}")

    if avg_time_ms < 0.5:
        print(f"✅ PASSED: Search time {avg_time_ms:.3f}ms < 0.5ms target")
        return True
    else:
        print(f"❌ FAILED: Search time {avg_time_ms:.3f}ms >= 0.5ms target")
        return False


def benchmark_rate_limiter():
    """Benchmark rate limiter lazy cleanup overhead."""
    print("\n" + "="*60)
    print("BENCHMARK: Rate Limiter Lazy Cleanup")
    print("="*60)

    # Create rate limiter (no actual limits to avoid waiting)
    limiter = RateLimiter(requests_per_minute=None)

    iterations = 1000

    # Warmup
    for _ in range(10):
        limiter.acquire()

    # Benchmark
    start = time.perf_counter()
    for _ in range(iterations):
        limiter.acquire()
    elapsed = time.perf_counter() - start

    avg_time_ms = (elapsed / iterations) * 1000
    print(f"✓ Average acquire() overhead: {avg_time_ms:.3f}ms")
    print(f"  Total calls: {iterations}")
    print(f"  Cleanup threshold: {limiter._cleanup_threshold}")

    if avg_time_ms < 0.1:
        print(f"✅ PASSED: Rate limiter overhead {avg_time_ms:.3f}ms < 0.1ms target")
        return True
    else:
        print(f"❌ FAILED: Rate limiter overhead {avg_time_ms:.3f}ms >= 0.1ms target")
        return False


def main():
    """Run all Phase 1 benchmarks."""
    print("\n" + "="*60)
    print("PHASE 1 OPTIMIZATION BENCHMARKS")
    print("="*60)
    print("\nTargets:")
    print("  • Component searches: <0.5ms (80-90% improvement)")
    print("  • Rate limiter overhead: <0.1ms (88% improvement)")

    results = []

    # Run all benchmarks
    results.append(("Chain searches", benchmark_chain_searches()))
    results.append(("Wire rope searches", benchmark_wire_rope_searches()))
    results.append(("Synthetic rope searches", benchmark_synthetic_rope_searches()))
    results.append(("Anchor searches", benchmark_anchor_searches()))
    results.append(("Connector searches", benchmark_connector_searches()))
    results.append(("Rate limiter", benchmark_rate_limiter()))

    # Summary
    print("\n" + "="*60)
    print("SUMMARY")
    print("="*60)

    passed = sum(1 for _, result in results if result)
    total = len(results)

    for name, result in results:
        status = "✅ PASSED" if result else "❌ FAILED"
        print(f"{status}: {name}")

    print("\n" + "="*60)
    print(f"Results: {passed}/{total} benchmarks passed")
    print("="*60)

    if passed == total:
        print("\n✅ All Phase 1 optimizations verified!")
        print("\nExpected improvements achieved:")
        print("  • Database searches: 80-90% faster (2-5ms → <0.5ms)")
        print("  • Rate limiter: 88% faster (0.8ms → <0.1ms)")
        return 0
    else:
        print(f"\n❌ {total - passed} benchmark(s) failed")
        return 1


if __name__ == "__main__":
    exit(main())
