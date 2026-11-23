"""
Unit Tests for Fatigue Reliability Analysis Module
===================================================

Tests cover:
- failure_probability: FORM-based failure probability for known S-N + stress inputs
- reliability_index: beta = Phi^-1(1 - Pf) conversion
- sensitivity_analysis: Sobol-like sensitivity indices via sampling
- monte_carlo_fatigue: Monte Carlo simulation of fatigue life
- Edge cases: zero CoV, deterministic limits, negative inputs
"""

import math
import numpy as np
import pytest

from digitalmodel.structural.fatigue.fatigue_reliability import (
    failure_probability,
    monte_carlo_fatigue,
    reliability_index,
    sensitivity_analysis,
)
from digitalmodel.structural.fatigue.sn_curves import PowerLawSNCurve


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def simple_sn():
    """Simple power-law SN curve: N = 1e12 * S^(-3)."""
    return PowerLawSNCurve(name="TestSN", A=1e12, m=3.0, fatigue_limit=0.0)


@pytest.fixture
def mild_sn():
    """Moderate fatigue curve: N = 1e15 * S^(-5)."""
    return PowerLawSNCurve(name="MildSN", A=1e15, m=5.0, fatigue_limit=0.0)


# ---------------------------------------------------------------------------
# reliability_index
# ---------------------------------------------------------------------------


class TestReliabilityIndex:
    """Test beta = Phi^-1(1 - Pf) conversion."""

    def test_known_pf_0_5(self):
        """Pf = 0.5 => beta = 0.0"""
        beta = reliability_index(0.5)
        assert abs(beta) < 1e-10

    def test_known_pf_small(self):
        """Pf = 2.33e-2 => beta ~ 2.0 (from standard normal table)"""
        from scipy.stats import norm

        pf = norm.cdf(-2.0)  # ~ 0.02275
        beta = reliability_index(pf)
        assert abs(beta - 2.0) < 1e-6

    def test_known_pf_very_small(self):
        """Pf = Phi(-3.0) => beta = 3.0"""
        from scipy.stats import norm

        pf = norm.cdf(-3.0)
        beta = reliability_index(pf)
        assert abs(beta - 3.0) < 1e-6

    def test_pf_one_gives_negative_inf(self):
        """Pf = 1.0 => beta = -inf"""
        beta = reliability_index(1.0)
        assert beta == float("-inf")

    def test_pf_zero_gives_positive_inf(self):
        """Pf = 0.0 => beta = +inf"""
        beta = reliability_index(0.0)
        assert beta == float("inf")

    def test_invalid_pf_raises(self):
        """Pf outside [0, 1] must raise ValueError."""
        with pytest.raises(ValueError, match="between 0 and 1"):
            reliability_index(-0.1)
        with pytest.raises(ValueError, match="between 0 and 1"):
            reliability_index(1.5)


# ---------------------------------------------------------------------------
# failure_probability
# ---------------------------------------------------------------------------


class TestFailureProbability:
    """Test FORM-based failure probability estimation."""

    def test_deterministic_safe(self, simple_sn):
        """Low stress with low CoV => near-zero Pf."""
        # N = 1e12 * 10^(-3) = 1e9 allowable at 10 MPa
        # design_life = 1e6 << 1e9 => very safe
        result = failure_probability(
            sn_curve=simple_sn,
            stress_range=10.0,
            design_life=1e6,
            cov_stress=0.05,
            cov_sn=0.05,
        )
        assert 0.0 <= result["pf"] <= 0.01
        assert result["beta"] > 2.0
        assert "pf" in result
        assert "beta" in result

    def test_deterministic_unsafe(self, simple_sn):
        """High stress with low CoV => high Pf."""
        # N = 1e12 * 200^(-3) = 1e12 / 8e6 = 125000 allowable at 200 MPa
        # design_life = 1e8 >> 125000 => certain failure
        result = failure_probability(
            sn_curve=simple_sn,
            stress_range=200.0,
            design_life=1e8,
            cov_stress=0.05,
            cov_sn=0.05,
        )
        assert result["pf"] > 0.99
        assert result["beta"] < -2.0

    def test_moderate_case(self, simple_sn):
        """Moderate stress => Pf between 0 and 1."""
        # N = 1e12 * 50^(-3) = 8000 allowable at 50 MPa
        # design_life = 5000 => margin is moderate
        result = failure_probability(
            sn_curve=simple_sn,
            stress_range=50.0,
            design_life=5000,
            cov_stress=0.15,
            cov_sn=0.15,
        )
        assert 0.0 < result["pf"] < 1.0

    def test_higher_cov_increases_pf(self, simple_sn):
        """Higher CoV should generally increase Pf for a safe design."""
        result_low = failure_probability(
            sn_curve=simple_sn,
            stress_range=10.0,
            design_life=1e6,
            cov_stress=0.05,
            cov_sn=0.05,
        )
        result_high = failure_probability(
            sn_curve=simple_sn,
            stress_range=10.0,
            design_life=1e6,
            cov_stress=0.30,
            cov_sn=0.30,
        )
        assert result_high["pf"] >= result_low["pf"]

    def test_result_keys(self, simple_sn):
        """Result dict must contain expected keys."""
        result = failure_probability(
            sn_curve=simple_sn,
            stress_range=50.0,
            design_life=5000,
            cov_stress=0.1,
            cov_sn=0.1,
        )
        assert "pf" in result
        assert "beta" in result
        assert "safety_margin_mean" in result
        assert "safety_margin_std" in result

    def test_zero_cov(self, simple_sn):
        """Zero CoV => deterministic case, Pf is 0 or 1."""
        # Very safe deterministic case
        result = failure_probability(
            sn_curve=simple_sn,
            stress_range=10.0,
            design_life=1e6,
            cov_stress=0.0,
            cov_sn=0.0,
        )
        assert result["pf"] == 0.0 or result["pf"] == pytest.approx(0.0, abs=1e-15)


# ---------------------------------------------------------------------------
# sensitivity_analysis
# ---------------------------------------------------------------------------


class TestSensitivityAnalysis:
    """Test Sobol-like sensitivity indices via sampling."""

    def test_indices_sum_near_one(self, simple_sn):
        """First-order Sobol indices should approximately sum to 1."""
        result = sensitivity_analysis(
            sn_curve=simple_sn,
            stress_range=50.0,
            design_life=5000,
            cov_stress=0.15,
            cov_sn=0.15,
            n_samples=5000,
            seed=42,
        )
        total = sum(result["first_order"].values())
        assert abs(total - 1.0) < 0.3  # Allow tolerance for sampling noise

    def test_indices_non_negative(self, simple_sn):
        """Sensitivity indices must be non-negative."""
        result = sensitivity_analysis(
            sn_curve=simple_sn,
            stress_range=50.0,
            design_life=5000,
            cov_stress=0.15,
            cov_sn=0.15,
            n_samples=5000,
            seed=42,
        )
        for key, val in result["first_order"].items():
            assert val >= -0.05, f"Sobol index for {key} is too negative: {val}"

    def test_result_keys(self, simple_sn):
        """Result must contain first_order dict with stress and sn keys."""
        result = sensitivity_analysis(
            sn_curve=simple_sn,
            stress_range=50.0,
            design_life=5000,
            cov_stress=0.15,
            cov_sn=0.15,
            n_samples=2000,
            seed=42,
        )
        assert "first_order" in result
        assert "stress" in result["first_order"]
        assert "sn" in result["first_order"]

    def test_dominant_variable(self, simple_sn):
        """When stress CoV >> SN CoV, stress sensitivity should dominate."""
        result = sensitivity_analysis(
            sn_curve=simple_sn,
            stress_range=50.0,
            design_life=5000,
            cov_stress=0.40,
            cov_sn=0.05,
            n_samples=5000,
            seed=42,
        )
        assert result["first_order"]["stress"] > result["first_order"]["sn"]


# ---------------------------------------------------------------------------
# monte_carlo_fatigue
# ---------------------------------------------------------------------------


class TestMonteCarloFatigue:
    """Test Monte Carlo simulation of fatigue life."""

    def test_returns_expected_keys(self, simple_sn):
        """Result dict must contain expected keys."""
        result = monte_carlo_fatigue(
            sn_curve=simple_sn,
            stress_range=50.0,
            design_life=5000,
            cov_stress=0.15,
            cov_sn=0.15,
            n_samples=1000,
            seed=42,
        )
        assert "pf" in result
        assert "mean_life" in result
        assert "std_life" in result
        assert "samples" in result
        assert "percentiles" in result

    def test_pf_range(self, simple_sn):
        """Pf must be in [0, 1]."""
        result = monte_carlo_fatigue(
            sn_curve=simple_sn,
            stress_range=50.0,
            design_life=5000,
            cov_stress=0.15,
            cov_sn=0.15,
            n_samples=5000,
            seed=42,
        )
        assert 0.0 <= result["pf"] <= 1.0

    def test_safe_design_low_pf(self, simple_sn):
        """Very safe design => low Monte Carlo Pf."""
        result = monte_carlo_fatigue(
            sn_curve=simple_sn,
            stress_range=10.0,
            design_life=1e6,
            cov_stress=0.05,
            cov_sn=0.05,
            n_samples=5000,
            seed=42,
        )
        assert result["pf"] < 0.05

    def test_unsafe_design_high_pf(self, simple_sn):
        """Certain-failure design => high Monte Carlo Pf."""
        # N = 1e12 * 200^(-3) = 125000 allowable at 200 MPa
        # design_life = 1e8 >> 125000 => certain failure
        result = monte_carlo_fatigue(
            sn_curve=simple_sn,
            stress_range=200.0,
            design_life=1e8,
            cov_stress=0.05,
            cov_sn=0.05,
            n_samples=5000,
            seed=42,
        )
        assert result["pf"] > 0.95

    def test_convergence_more_samples(self, simple_sn):
        """More samples should give tighter estimate (std of Pf shrinks)."""
        pf_list = []
        for s in [100, 500, 2000]:
            r = monte_carlo_fatigue(
                sn_curve=simple_sn,
                stress_range=50.0,
                design_life=5000,
                cov_stress=0.15,
                cov_sn=0.15,
                n_samples=s,
                seed=42,
            )
            pf_list.append(r["pf"])
        # All estimates should be in a reasonable band
        assert max(pf_list) - min(pf_list) < 0.2

    def test_sample_count_matches(self, simple_sn):
        """Number of returned samples must match n_samples."""
        n = 500
        result = monte_carlo_fatigue(
            sn_curve=simple_sn,
            stress_range=50.0,
            design_life=5000,
            cov_stress=0.15,
            cov_sn=0.15,
            n_samples=n,
            seed=42,
        )
        assert len(result["samples"]) == n

    def test_percentiles_ordered(self, simple_sn):
        """Percentiles must be in ascending order."""
        result = monte_carlo_fatigue(
            sn_curve=simple_sn,
            stress_range=50.0,
            design_life=5000,
            cov_stress=0.15,
            cov_sn=0.15,
            n_samples=2000,
            seed=42,
        )
        p = result["percentiles"]
        assert p["p5"] <= p["p25"] <= p["p50"] <= p["p75"] <= p["p95"]

    def test_mean_life_positive(self, simple_sn):
        """Mean fatigue life must be positive."""
        result = monte_carlo_fatigue(
            sn_curve=simple_sn,
            stress_range=50.0,
            design_life=5000,
            cov_stress=0.15,
            cov_sn=0.15,
            n_samples=1000,
            seed=42,
        )
        assert result["mean_life"] > 0


# ---------------------------------------------------------------------------
# Cross-method consistency
# ---------------------------------------------------------------------------


class TestCrossMethodConsistency:
    """FORM and Monte Carlo results should be directionally consistent."""

    def test_form_vs_mc_safe_design(self, simple_sn):
        """Both methods should agree the design is safe."""
        form = failure_probability(
            sn_curve=simple_sn,
            stress_range=10.0,
            design_life=1e6,
            cov_stress=0.1,
            cov_sn=0.1,
        )
        mc = monte_carlo_fatigue(
            sn_curve=simple_sn,
            stress_range=10.0,
            design_life=1e6,
            cov_stress=0.1,
            cov_sn=0.1,
            n_samples=10000,
            seed=42,
        )
        # Both should give Pf < 0.05
        assert form["pf"] < 0.05
        assert mc["pf"] < 0.05

    def test_form_vs_mc_unsafe_design(self, simple_sn):
        """Both methods should agree the design is unsafe."""
        # N = 1e12 * 200^(-3) = 125000 allowable at 200 MPa
        # design_life = 1e8 >> 125000 => certain failure
        form = failure_probability(
            sn_curve=simple_sn,
            stress_range=200.0,
            design_life=1e8,
            cov_stress=0.1,
            cov_sn=0.1,
        )
        mc = monte_carlo_fatigue(
            sn_curve=simple_sn,
            stress_range=200.0,
            design_life=1e8,
            cov_stress=0.1,
            cov_sn=0.1,
            n_samples=10000,
            seed=42,
        )
        assert form["pf"] > 0.95
        assert mc["pf"] > 0.95
