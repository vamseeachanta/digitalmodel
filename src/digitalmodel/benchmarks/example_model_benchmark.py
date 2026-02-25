"""
Example Model Benchmark — public API facade.

Imports and re-exports all public symbols from the inventory and metrics
sub-modules so that consumers can import from a single location:

    from digitalmodel.benchmarks.example_model_benchmark import (
        ModelCategory,
        ModelInventoryEntry,
        build_model_inventory,
        TimeDomainStats,
        compute_time_domain_stats,
        SeedEquivalenceResult,
        check_seed_equivalence,
        FrequencyDomainMetrics,
        compare_frequency_domain,
        BenchmarkPassFail,
        classify_benchmark_result,
        BenchmarkSummary,
        build_benchmark_summary,
        SEED_COV_THRESHOLD,
        RAO_AMP_TOLERANCE_PCT,
        TENSION_TOLERANCE_PCT,
    )

Sub-module layout
-----------------
    inventory.py   — ModelCategory, ModelInventoryEntry, build_model_inventory
    metrics.py     — statistics, CoV, RAO comparison, pass/fail, summary
"""

from .inventory import (  # noqa: F401
    ModelCategory,
    ModelInventoryEntry,
    build_model_inventory,
)

from .metrics import (  # noqa: F401
    SEED_COV_THRESHOLD,
    RAO_AMP_TOLERANCE_PCT,
    RAO_PHASE_TOLERANCE_DEG,
    TENSION_TOLERANCE_PCT,
    BENDING_TOLERANCE_PCT,
    TimeDomainStats,
    compute_time_domain_stats,
    SeedEquivalenceResult,
    check_seed_equivalence,
    FrequencyDomainMetrics,
    compare_frequency_domain,
    BenchmarkPassFail,
    classify_benchmark_result,
    BenchmarkSummary,
    build_benchmark_summary,
)
