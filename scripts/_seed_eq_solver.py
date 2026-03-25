"""
Solver-tier stubs for seed equivalence benchmark.

These functions are called by seed_equivalence.py when --mock is NOT supplied.
They require OrcFxAPI on a licensed workstation.  On unlicensed machines the
OrcFxAPI import will fail with ImportError, which the main script intercepts.

Implementation notes for the Codex solver-tier agent
------------------------------------------------------
_run_solver_time_domain:
    1. Load the OrcaFlex model from entry.path.
    2. For each seed: modify the random wave seed parameter, run dynamics.
    3. Extract tension / motion time histories for representative lines/DoF.
    4. Build TimeDomainStats via compute_time_domain_stats.
    5. Call check_seed_equivalence and return the result.

_run_solver_frequency_domain:
    1. Load the model, configure frequency-domain analysis parameters.
    2. Run the OrcaFlex frequency-domain analysis (modal or direct).
    3. Extract RAO amplitudes for the target DoF across the frequency range.
    4. Compare against a stored baseline using compare_frequency_domain.
    5. Return FrequencyDomainMetrics.
"""

from __future__ import annotations

from typing import List, Optional

from digitalmodel.benchmarks.example_model_benchmark import (
    FrequencyDomainMetrics,
    ModelInventoryEntry,
    SeedEquivalenceResult,
    TimeDomainStats,
)


def run_solver_time_domain(
    entry: ModelInventoryEntry,
    n_seeds: int,
    seeds: List[int],
) -> Optional[SeedEquivalenceResult]:
    """Solver-tier time-domain run (requires OrcFxAPI).

    Parameters
    ----------
    entry:   Model inventory entry (provides path and name).
    n_seeds: Number of seeds (informational).
    seeds:   List of integer seed values to use.

    Raises
    ------
    NotImplementedError: Until implemented on a licensed machine.
    ImportError: If OrcFxAPI is not installed.
    """
    import OrcFxAPI  # noqa: F401, F811 — fails fast on unlicensed machines

    raise NotImplementedError(
        f"Solver-tier time-domain run not yet implemented for '{entry.name}'. "
        "Use --mock for unit-tier validation or delegate to Codex on a "
        "licensed OrcaFlex workstation."
    )


def run_solver_frequency_domain(
    entry: ModelInventoryEntry,
) -> Optional[FrequencyDomainMetrics]:
    """Solver-tier frequency-domain run (requires OrcFxAPI).

    Parameters
    ----------
    entry: Model inventory entry (provides path and name).

    Raises
    ------
    NotImplementedError: Until implemented on a licensed machine.
    ImportError: If OrcFxAPI is not installed.
    """
    import OrcFxAPI  # noqa: F401, F811 — fails fast on unlicensed machines

    raise NotImplementedError(
        f"Solver-tier frequency-domain run not yet implemented for "
        f"'{entry.name}'. Use --mock for unit-tier validation or delegate "
        "to Codex on a licensed OrcaFlex workstation."
    )
