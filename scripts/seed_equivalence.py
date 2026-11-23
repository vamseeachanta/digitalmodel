#!/usr/bin/env python3
"""
Seed Equivalence Benchmark — All Example Models.

Runs time-domain and frequency-domain benchmarks across all classified
example models, demonstrating statistical equivalence across multiple random
wave seeds.

Execution tiers
---------------
Unit tier (no solver, mock data):
    python3 scripts/seed_equivalence.py --mock
    -- Runs immediately on any machine.  Validates the statistical framework.

Solver tier (OrcFxAPI required, licensed machine):
    python3 scripts/seed_equivalence.py --max-models 5
    python3 scripts/seed_equivalence.py

Full benchmark (overnight / batch):
    python3 scripts/seed_equivalence.py --n-seeds 10

Output
------
benchmark_output/model_inventory.yaml      — classified model list
benchmark_output/seed_equivalence/         — per-model per-seed JSON
benchmark_output/seed_equivalence_report.html — consolidated HTML report
benchmark_output/seed_equivalence_baselines.json — regression baselines

Usage
-----
    uv run python scripts/seed_equivalence.py --mock
    uv run python scripts/seed_equivalence.py --max-models 3 --n-seeds 5
    uv run python scripts/seed_equivalence.py --html-only
"""

from __future__ import annotations

import argparse
import sys
import time
from pathlib import Path
from typing import List, Optional

# ---------------------------------------------------------------------------
# Setup path — allow running directly without `uv run` / package install
# ---------------------------------------------------------------------------
_REPO_ROOT = Path(__file__).resolve().parent.parent
_SRC = _REPO_ROOT / "src"
if str(_SRC) not in sys.path:
    sys.path.insert(0, str(_SRC))

from digitalmodel.benchmarks.example_model_benchmark import (
    BenchmarkPassFail,
    BenchmarkSummary,
    FrequencyDomainMetrics,
    ModelCategory,
    ModelInventoryEntry,
    SeedEquivalenceResult,
    build_benchmark_summary,
    build_model_inventory,
    SEED_COV_THRESHOLD,
    RAO_AMP_TOLERANCE_PCT,
)
from _seed_eq_mock import run_mock_time_domain, run_mock_frequency_domain
from _seed_eq_report import (
    export_inventory_yaml,
    generate_html_report,
    load_existing_summaries,
    save_baselines,
    save_model_result,
)
from _seed_eq_solver import run_solver_time_domain, run_solver_frequency_domain

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------
DEFAULT_N_SEEDS = 5


# ---------------------------------------------------------------------------
# Utilities
# ---------------------------------------------------------------------------
def _flush(*args) -> None:
    print(*args)
    sys.stdout.flush()


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
def main() -> int:
    parser = argparse.ArgumentParser(
        description="Seed equivalence benchmark for all example models"
    )
    parser.add_argument(
        "--mock",
        action="store_true",
        help="Run with synthetic mock signals (no OrcFxAPI required)",
    )
    parser.add_argument(
        "--n-seeds",
        type=int,
        default=DEFAULT_N_SEEDS,
        help=f"Number of random seeds (default: {DEFAULT_N_SEEDS})",
    )
    parser.add_argument(
        "--max-models",
        type=int,
        default=0,
        help="Limit number of models to process (0 = all)",
    )
    parser.add_argument(
        "--html-only",
        action="store_true",
        help="Regenerate HTML report from existing JSON results",
    )
    parser.add_argument(
        "--categories",
        nargs="*",
        choices=["statics_only", "time_domain", "frequency_domain", "both"],
        help="Only process models in these categories",
    )
    args = parser.parse_args()

    # Discover inventory
    inventory = build_model_inventory()
    export_inventory_yaml(inventory)

    # Apply filters
    if args.categories:
        selected_cats = {ModelCategory(c) for c in args.categories}
        inventory = [e for e in inventory if e.category in selected_cats]
    if args.max_models > 0:
        inventory = inventory[:args.max_models]

    _flush(f"Models to benchmark: {len(inventory)}")
    _flush(f"Seeds per model: {args.n_seeds}")
    _flush(f"Tier: {'mock (unit)' if args.mock else 'solver (OrcFxAPI)'}")
    _flush(f"Seed CoV threshold: {SEED_COV_THRESHOLD:.0%}")
    _flush(f"RAO amplitude tolerance: {RAO_AMP_TOLERANCE_PCT:.1f}%")

    if args.html_only:
        summaries = load_existing_summaries()
        if not summaries:
            _flush("No existing results found. Run without --html-only first.")
            return 1
        generate_html_report(summaries, mock=args.mock)
        return 0

    # Solver availability check (non-mock mode)
    if not args.mock:
        try:
            import OrcFxAPI  # noqa: F401
        except ImportError:
            _flush("ERROR: OrcFxAPI not available. Use --mock for unit-tier run.")
            _flush(
                "  On a licensed machine: "
                "uv run python scripts/seed_equivalence.py --n-seeds 5"
            )
            return 1

    seeds = list(range(args.n_seeds))
    t0 = time.time()
    summaries: List[BenchmarkSummary] = []

    for idx, entry in enumerate(inventory, 1):
        _flush(f"\n{'='*60}")
        _flush(f"[{idx}/{len(inventory)}] {entry.name}")
        _flush(f"  Category: {entry.category.value}  |  Path: {entry.path}")

        try:
            seed_result: Optional[SeedEquivalenceResult] = None
            if entry.has_time_domain:
                if args.mock:
                    seed_result = run_mock_time_domain(entry, seeds)
                else:
                    seed_result = run_solver_time_domain(entry, args.n_seeds, seeds)
                if seed_result:
                    status = "CONVERGED" if seed_result.converged else "DIVERGED"
                    _flush(
                        f"  Time domain: {status} "
                        f"(CoV max={seed_result.cov_max:.4f})"
                    )

            freq_result: Optional[FrequencyDomainMetrics] = None
            if entry.has_frequency_domain:
                if args.mock:
                    freq_result = run_mock_frequency_domain(entry)
                else:
                    freq_result = run_solver_frequency_domain(entry)
                if freq_result:
                    _flush(
                        f"  Freq domain: max RAO diff "
                        f"{freq_result.max_amp_diff_pct:.2f}%"
                    )

            summary = build_benchmark_summary(
                model_name=entry.name,
                category=entry.category,
                statics_pass=True,
                seed_equiv_result=seed_result,
                freq_metrics=freq_result,
            )
            _flush(f"  Overall: {summary.overall_status.value.upper()}")

        except Exception as exc:
            _flush(f"  ERROR: {exc}")
            summary = BenchmarkSummary(
                model_name=entry.name,
                category=entry.category,
                statics_pass=False,
                time_domain_status=BenchmarkPassFail.SKIP,
                frequency_domain_status=BenchmarkPassFail.SKIP,
                seed_equivalence_status=BenchmarkPassFail.SKIP,
                overall_status=BenchmarkPassFail.FAIL,
                notes=f"Exception: {exc}",
            )

        summaries.append(summary)
        save_model_result(summary)

    elapsed = time.time() - t0
    n_pass = sum(1 for s in summaries if s.overall_status == BenchmarkPassFail.PASS)
    n_fail = sum(1 for s in summaries if s.overall_status == BenchmarkPassFail.FAIL)
    n_warn = sum(1 for s in summaries if s.overall_status == BenchmarkPassFail.WARN)

    _flush(f"\n{'='*60}")
    _flush(f"DONE: {len(summaries)} models in {elapsed:.1f}s")
    _flush(f"  Pass: {n_pass}  Warn: {n_warn}  Fail: {n_fail}")

    save_baselines(summaries)
    generate_html_report(summaries, mock=args.mock)

    return 0 if n_fail == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
