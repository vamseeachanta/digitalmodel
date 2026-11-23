"""
Unit tests for benchmark pass/fail classification and consolidated summary.

Sections:
  5 — BenchmarkPassFail enum and classify_benchmark_result
  6 — BenchmarkSummary dataclass and build_benchmark_summary
  7 — Constants validation
  8 — Edge cases and robustness
"""

from __future__ import annotations

import json
import math
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.benchmarks.example_model_benchmark import (
    BenchmarkPassFail,
    BenchmarkSummary,
    FrequencyDomainMetrics,
    ModelCategory,
    SeedEquivalenceResult,
    TimeDomainStats,
    build_benchmark_summary,
    build_model_inventory,
    check_seed_equivalence,
    classify_benchmark_result,
    compare_frequency_domain,
    compute_time_domain_stats,
    RAO_AMP_TOLERANCE_PCT,
    SEED_COV_THRESHOLD,
    TENSION_TOLERANCE_PCT,
)


# ===========================================================================
# Section 5: Pass/Fail Classification
# ===========================================================================

class TestBenchmarkPassFail:
    """BenchmarkPassFail enum covers all states."""

    def test_all_states_present(self):
        states = {s.value for s in BenchmarkPassFail}
        assert "pass" in states
        assert "warn" in states
        assert "fail" in states

    def test_skip_state_present(self):
        states = {s.value for s in BenchmarkPassFail}
        assert "skip" in states


class TestClassifyBenchmarkResult:
    """classify_benchmark_result maps CoV / diff% to pass/warn/fail."""

    def test_low_cov_passes(self):
        status = classify_benchmark_result(
            cov=0.02, tolerance=SEED_COV_THRESHOLD)
        assert status == BenchmarkPassFail.PASS

    def test_cov_at_threshold_warns(self):
        # CoV exactly at threshold is borderline — warn or pass
        status = classify_benchmark_result(
            cov=SEED_COV_THRESHOLD, tolerance=SEED_COV_THRESHOLD)
        assert status in (BenchmarkPassFail.WARN, BenchmarkPassFail.PASS)

    def test_high_cov_fails(self):
        status = classify_benchmark_result(
            cov=SEED_COV_THRESHOLD * 3, tolerance=SEED_COV_THRESHOLD)
        assert status == BenchmarkPassFail.FAIL

    def test_tension_within_tolerance_passes(self):
        status = classify_benchmark_result(
            cov=0.005, tolerance=TENSION_TOLERANCE_PCT / 100.0)
        assert status == BenchmarkPassFail.PASS


# ===========================================================================
# Section 6: Consolidated Benchmark Summary
# ===========================================================================

class TestBenchmarkSummary:
    """BenchmarkSummary dataclass contract."""

    def test_fields_present(self):
        summary = BenchmarkSummary(
            model_name="test_model",
            category=ModelCategory.TIME_DOMAIN,
            statics_pass=True,
            time_domain_status=BenchmarkPassFail.PASS,
            frequency_domain_status=BenchmarkPassFail.SKIP,
            seed_equivalence_status=BenchmarkPassFail.PASS,
            overall_status=BenchmarkPassFail.PASS,
        )
        assert summary.model_name == "test_model"
        assert summary.overall_status == BenchmarkPassFail.PASS

    def test_optional_details(self):
        summary = BenchmarkSummary(
            model_name="m",
            category=ModelCategory.BOTH,
            statics_pass=True,
            time_domain_status=BenchmarkPassFail.PASS,
            frequency_domain_status=BenchmarkPassFail.WARN,
            seed_equivalence_status=BenchmarkPassFail.PASS,
            overall_status=BenchmarkPassFail.WARN,
            seed_cov=0.05,
            freq_max_diff_pct=4.2,
            notes="Minor frequency domain discrepancy",
        )
        assert summary.seed_cov == pytest.approx(0.05)
        assert summary.freq_max_diff_pct == pytest.approx(4.2)
        assert "Minor" in summary.notes


class TestBuildBenchmarkSummary:
    """build_benchmark_summary produces correct overall status."""

    def test_all_pass_produces_pass(self):
        summary = build_benchmark_summary(
            model_name="m",
            category=ModelCategory.BOTH,
            statics_pass=True,
            seed_equiv_result=SeedEquivalenceResult(
                model_name="m", n_seeds=5, seeds=list(range(5)),
                cov_mean=0.02, cov_std=0.01, cov_max=0.04, converged=True),
            freq_metrics=FrequencyDomainMetrics(
                model_name="m",
                frequency_hz=np.array([0.1, 0.2]),
                rao_reference=np.array([1.0, 1.0]),
                rao_computed=np.array([1.01, 1.01]),
                max_amp_diff_pct=1.0,
                mean_amp_diff_pct=1.0,
            ),
        )
        assert summary.overall_status == BenchmarkPassFail.PASS

    def test_failed_statics_yields_fail(self):
        summary = build_benchmark_summary(
            model_name="m",
            category=ModelCategory.STATICS_ONLY,
            statics_pass=False,
            seed_equiv_result=None,
            freq_metrics=None,
        )
        assert summary.overall_status == BenchmarkPassFail.FAIL

    def test_failed_seed_equiv_yields_fail(self):
        summary = build_benchmark_summary(
            model_name="m",
            category=ModelCategory.TIME_DOMAIN,
            statics_pass=True,
            seed_equiv_result=SeedEquivalenceResult(
                model_name="m", n_seeds=5, seeds=list(range(5)),
                cov_mean=0.30, cov_std=0.10, cov_max=0.50, converged=False),
            freq_metrics=None,
        )
        assert summary.overall_status == BenchmarkPassFail.FAIL

    def test_freq_domain_skip_for_td_only_model(self):
        summary = build_benchmark_summary(
            model_name="td_only",
            category=ModelCategory.TIME_DOMAIN,
            statics_pass=True,
            seed_equiv_result=SeedEquivalenceResult(
                model_name="td_only", n_seeds=5, seeds=list(range(5)),
                cov_mean=0.01, cov_std=0.005, cov_max=0.02, converged=True),
            freq_metrics=None,
        )
        assert summary.frequency_domain_status == BenchmarkPassFail.SKIP

    def test_statics_only_skips_td_and_fd(self):
        summary = build_benchmark_summary(
            model_name="static",
            category=ModelCategory.STATICS_ONLY,
            statics_pass=True,
            seed_equiv_result=None,
            freq_metrics=None,
        )
        assert summary.time_domain_status == BenchmarkPassFail.SKIP
        assert summary.frequency_domain_status == BenchmarkPassFail.SKIP
        assert summary.seed_equivalence_status == BenchmarkPassFail.SKIP
        assert summary.overall_status == BenchmarkPassFail.PASS

    def test_warn_when_freq_domain_borderline(self):
        """Frequency domain diff near but below tolerance → WARN."""
        metrics = FrequencyDomainMetrics(
            model_name="m",
            frequency_hz=np.array([0.1]),
            rao_reference=np.array([1.0]),
            rao_computed=np.array([1.04]),  # 4% — just under 5% tolerance
            max_amp_diff_pct=4.0,
            mean_amp_diff_pct=4.0,
        )
        summary = build_benchmark_summary(
            model_name="m",
            category=ModelCategory.FREQUENCY_DOMAIN,
            statics_pass=True,
            seed_equiv_result=None,
            freq_metrics=metrics,
        )
        assert summary.frequency_domain_status in (
            BenchmarkPassFail.WARN, BenchmarkPassFail.PASS)


# ===========================================================================
# Section 7: Constants
# ===========================================================================

class TestConstants:
    """Validate required benchmark constants are defined and sane."""

    def test_seed_cov_threshold_positive(self):
        assert SEED_COV_THRESHOLD > 0

    def test_seed_cov_threshold_below_one(self):
        assert SEED_COV_THRESHOLD < 1.0

    def test_rao_amp_tolerance_positive(self):
        assert RAO_AMP_TOLERANCE_PCT > 0

    def test_tension_tolerance_positive(self):
        assert TENSION_TOLERANCE_PCT > 0

    def test_rao_amp_tolerance_reasonable(self):
        assert 1.0 < RAO_AMP_TOLERANCE_PCT <= 20.0

    def test_tension_tolerance_reasonable(self):
        assert 0.1 < TENSION_TOLERANCE_PCT <= 10.0


# ===========================================================================
# Section 8: Edge cases and robustness
# ===========================================================================

class TestEdgeCases:
    """Edge cases and boundary conditions."""

    def test_single_point_frequency_domain(self):
        """Single frequency point should not raise."""
        freqs = np.array([0.1])
        rao = np.array([1.0])
        metrics = compare_frequency_domain("m", freqs, rao, rao * 1.02)
        assert metrics.max_amp_diff_pct == pytest.approx(2.0, rel=0.01)

    def test_two_seeds_minimum(self):
        """Exactly 2 seeds is the minimum for equivalence."""
        stats = [
            TimeDomainStats(seed=0, mean=1.0, std=0.1, maximum=1.5,
                            minimum=0.5, rms=1.005),
            TimeDomainStats(seed=1, mean=1.01, std=0.1, maximum=1.51,
                            minimum=0.49, rms=1.015),
        ]
        result = check_seed_equivalence("m", stats)
        assert result.n_seeds == 2
        assert result.converged is True

    def test_constant_signal_stats(self):
        """Constant signal: std=0, mean=constant."""
        sig = np.ones(1000) * 5.0
        stats = compute_time_domain_stats(sig, seed=0)
        assert stats.mean == pytest.approx(5.0)
        assert stats.std == pytest.approx(0.0, abs=1e-12)
        assert stats.maximum == pytest.approx(5.0)
        assert stats.minimum == pytest.approx(5.0)

    def test_inventory_serialisable_to_dict(self):
        """Each inventory entry must be JSON-serialisable as a plain dict."""
        inventory = build_model_inventory()
        for entry in inventory:
            d = {
                "name": entry.name,
                "category": entry.category.value,
                "path": str(entry.path),
                "description": entry.description,
                "has_time_domain": entry.has_time_domain,
                "has_frequency_domain": entry.has_frequency_domain,
            }
            assert d["name"]
            assert d["category"]
            json.dumps(d)
