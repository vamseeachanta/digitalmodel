"""
Unit tests for seed equivalence and frequency domain comparison metrics.

Sections:
  3 — SeedEquivalenceResult and check_seed_equivalence
  4 — FrequencyDomainMetrics and compare_frequency_domain
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.benchmarks.example_model_benchmark import (
    FrequencyDomainMetrics,
    SeedEquivalenceResult,
    TimeDomainStats,
    check_seed_equivalence,
    compare_frequency_domain,
    RAO_AMP_TOLERANCE_PCT,
    SEED_COV_THRESHOLD,
)


# ===========================================================================
# Section 3: Seed Equivalence
# ===========================================================================

class TestSeedEquivalenceResult:
    """SeedEquivalenceResult dataclass contract."""

    def test_fields_present(self):
        result = SeedEquivalenceResult(
            model_name="mooring_model",
            n_seeds=5,
            seeds=[1, 2, 3, 4, 5],
            cov_mean=0.02,
            cov_std=0.03,
            cov_max=0.05,
            converged=True,
        )
        assert result.model_name == "mooring_model"
        assert result.n_seeds == 5
        assert result.converged is True

    def test_failed_convergence(self):
        result = SeedEquivalenceResult(
            model_name="bad_model",
            n_seeds=3,
            seeds=[1, 2, 3],
            cov_mean=0.25,
            cov_std=0.30,
            cov_max=0.40,
            converged=False,
        )
        assert result.converged is False


class TestCheckSeedEquivalence:
    """check_seed_equivalence computes CoV and convergence from multiple stats."""

    def _make_stats(self, seed: int, base_mean: float,
                    noise_frac: float) -> TimeDomainStats:
        """Create stats with controlled variation."""
        rng = np.random.default_rng(seed)
        mean_val = base_mean * (1.0 + rng.uniform(-noise_frac, noise_frac))
        std_val = 0.2 * base_mean
        return TimeDomainStats(
            seed=seed,
            mean=mean_val,
            std=std_val,
            maximum=mean_val + 2 * std_val,
            minimum=mean_val - 2 * std_val,
            rms=math.sqrt(mean_val ** 2 + std_val ** 2),
        )

    def test_low_variation_converges(self):
        """5 seeds with <2% variation should converge."""
        stats_list = [self._make_stats(s, base_mean=100.0, noise_frac=0.01)
                      for s in range(5)]
        result = check_seed_equivalence("test_model", stats_list)
        assert result.converged is True
        assert result.cov_mean < SEED_COV_THRESHOLD

    def test_high_variation_fails(self):
        """5 seeds with >20% variation should NOT converge."""
        stats_list = [self._make_stats(s, base_mean=100.0, noise_frac=0.20)
                      for s in range(5)]
        result = check_seed_equivalence("test_model", stats_list)
        assert result.converged is False

    def test_n_seeds_recorded(self):
        stats_list = [self._make_stats(s, 50.0, 0.005) for s in range(3)]
        result = check_seed_equivalence("m", stats_list)
        assert result.n_seeds == 3

    def test_seeds_list_recorded(self):
        seeds = [10, 20, 30, 40]
        stats_list = [self._make_stats(s, 50.0, 0.005) for s in seeds]
        result = check_seed_equivalence("m", stats_list)
        assert set(result.seeds) == set(seeds)

    def test_single_seed_raises(self):
        """At least 2 seeds are required for equivalence testing."""
        stats_list = [self._make_stats(0, 10.0, 0.01)]
        with pytest.raises(ValueError, match="at least 2"):
            check_seed_equivalence("m", stats_list)

    def test_cov_values_non_negative(self):
        stats_list = [self._make_stats(s, 100.0, 0.02) for s in range(5)]
        result = check_seed_equivalence("m", stats_list)
        assert result.cov_mean >= 0
        assert result.cov_std >= 0
        assert result.cov_max >= 0

    def test_cov_max_geq_cov_mean(self):
        stats_list = [self._make_stats(s, 100.0, 0.03) for s in range(5)]
        result = check_seed_equivalence("m", stats_list)
        assert result.cov_max >= result.cov_mean

    def test_model_name_recorded(self):
        stats_list = [self._make_stats(s, 100.0, 0.01) for s in range(2)]
        result = check_seed_equivalence("my_model", stats_list)
        assert result.model_name == "my_model"

    def test_near_zero_mean_uses_std_fallback(self):
        """When mean is near zero, CoV should not produce NaN/Inf."""
        stats_list = []
        rng = np.random.default_rng(0)
        for i in range(5):
            val = rng.normal(0, 1)
            stats_list.append(TimeDomainStats(
                seed=i, mean=val, std=1.0, maximum=3.0, minimum=-3.0,
                rms=math.sqrt(val ** 2 + 1.0),
            ))
        result = check_seed_equivalence("zero_mean_model", stats_list)
        assert math.isfinite(result.cov_mean)
        assert math.isfinite(result.cov_max)


# ===========================================================================
# Section 4: Frequency Domain Comparison
# ===========================================================================

class TestFrequencyDomainMetrics:
    """FrequencyDomainMetrics dataclass contract."""

    def test_fields_present(self):
        metrics = FrequencyDomainMetrics(
            model_name="hydro_model",
            frequency_hz=np.array([0.05, 0.10, 0.15, 0.20]),
            rao_reference=np.array([1.0, 1.5, 1.2, 0.8]),
            rao_computed=np.array([1.01, 1.52, 1.21, 0.81]),
            max_amp_diff_pct=1.5,
            mean_amp_diff_pct=0.8,
        )
        assert metrics.model_name == "hydro_model"
        assert len(metrics.frequency_hz) == 4
        assert metrics.max_amp_diff_pct == pytest.approx(1.5)

    def test_optional_phase_diff(self):
        metrics = FrequencyDomainMetrics(
            model_name="m",
            frequency_hz=np.array([0.1]),
            rao_reference=np.array([1.0]),
            rao_computed=np.array([1.0]),
            max_amp_diff_pct=0.0,
            mean_amp_diff_pct=0.0,
            max_phase_diff_deg=2.0,
        )
        assert metrics.max_phase_diff_deg == pytest.approx(2.0)


class TestCompareFrequencyDomain:
    """compare_frequency_domain calculates RAO amplitude/phase differences."""

    def test_identical_raos_zero_diff(self):
        freqs = np.linspace(0.05, 0.5, 50)
        rao = np.ones_like(freqs) * 1.2
        metrics = compare_frequency_domain("test", freqs, rao, rao)
        assert metrics.max_amp_diff_pct == pytest.approx(0.0, abs=1e-9)
        assert metrics.mean_amp_diff_pct == pytest.approx(0.0, abs=1e-9)

    def test_small_difference_within_tolerance(self):
        freqs = np.linspace(0.05, 0.5, 50)
        rao_ref = np.ones_like(freqs) * 1.0
        rao_cmp = rao_ref * 1.03  # 3% difference
        metrics = compare_frequency_domain("test", freqs, rao_ref, rao_cmp)
        assert metrics.max_amp_diff_pct == pytest.approx(3.0, rel=0.05)
        assert metrics.max_amp_diff_pct <= RAO_AMP_TOLERANCE_PCT

    def test_large_difference_exceeds_tolerance(self):
        freqs = np.linspace(0.05, 0.5, 50)
        rao_ref = np.ones_like(freqs) * 1.0
        rao_cmp = rao_ref * 1.10  # 10% difference
        metrics = compare_frequency_domain("test", freqs, rao_ref, rao_cmp)
        assert metrics.max_amp_diff_pct > RAO_AMP_TOLERANCE_PCT

    def test_frequency_array_stored(self):
        freqs = np.array([0.1, 0.2, 0.3])
        rao = np.array([1.0, 1.2, 0.8])
        metrics = compare_frequency_domain("m", freqs, rao, rao)
        np.testing.assert_array_equal(metrics.frequency_hz, freqs)

    def test_model_name_stored(self):
        freqs = np.array([0.1])
        rao = np.array([1.0])
        metrics = compare_frequency_domain("my_model", freqs, rao, rao)
        assert metrics.model_name == "my_model"

    def test_mismatched_lengths_raises(self):
        freqs = np.array([0.1, 0.2])
        rao_ref = np.array([1.0, 1.1])
        rao_wrong = np.array([1.0])
        with pytest.raises(ValueError, match="shape"):
            compare_frequency_domain("m", freqs, rao_ref, rao_wrong)

    def test_empty_arrays_raise(self):
        with pytest.raises(ValueError, match="empty"):
            compare_frequency_domain(
                "m", np.array([]), np.array([]), np.array([]))
