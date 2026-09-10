"""Tests for campaign comparability.

The invariance test is the one that matters: it asserts the negative result the
module exists to establish, so if it ever fails the module's central claim has
been broken.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.asset_integrity.campaign_comparability import (
    GaugeEstimate,
    gauge_from_short_interval,
    invariance_family,
    minimum_detectable_rate,
    reference_set_size,
    t_quantile_95,
    variance_budget,
)


class TestGaugeFromShortInterval:
    def test_scatter_and_mean(self):
        g = gauge_from_short_interval([-1.0, -4.0, 0.0, 0.0, -5.0, -1.0, 1.0])
        assert g.n == 7
        assert g.mean_difference == pytest.approx(-1.4286, abs=1e-3)
        assert g.difference_sd == pytest.approx(2.2254, abs=1e-3)
        assert g.single_measurement_sd == pytest.approx(2.2254 / math.sqrt(2), abs=1e-3)

    def test_small_sample_mean_may_not_establish_an_offset(self):
        """A visibly non-zero mean over seven articles need not be real."""
        g = gauge_from_short_interval([-1.0, -4.0, 0.0, 0.0, -5.0, -1.0, 1.0])
        assert g.mean_consistent_with_zero
        assert not g.offset_established
        assert "does not establish" in g.note

    def test_clearly_offset_sample_does_establish_one(self):
        g = gauge_from_short_interval([-5.0, -5.2, -4.8, -5.1, -4.9, -5.0])
        assert g.offset_established
        assert not g.mean_consistent_with_zero

    def test_variance_interval_is_wide_for_small_samples(self):
        g = gauge_from_short_interval([-1.0, -4.0, 0.0, 0.0, -5.0, -1.0, 1.0])
        lo, hi = g.difference_sd_ci95
        assert lo < g.difference_sd < hi
        assert hi / lo > 3.0          # seven articles pin very little

    def test_requires_two_articles(self):
        with pytest.raises(ValueError):
            gauge_from_short_interval([1.0])


class TestInvarianceFamily:
    def test_every_assumed_rate_fits_the_data_exactly(self):
        """The central negative result: the data does not choose a rate."""
        contrasts = {1: 3.630, 2: 2.993}
        elapsed = {1: 0.5914, 2: 0.6407}
        rows = invariance_family(contrasts, elapsed, [0.0, 2.0, 4.67, 10.0])
        assert len(rows) == 4
        for row in rows:
            # Reconstructing the contrast from rate and offset must return the
            # observation, for every rate. That is the confounding.
            for k, off in zip(sorted(contrasts), row.required_offsets):
                assert off + row.assumed_rate * elapsed[k] == pytest.approx(
                    contrasts[k], abs=1e-9)

    def test_offsets_move_with_the_assumed_rate(self):
        rows = invariance_family({1: 3.0}, {1: 0.5}, [0.0, 6.0])
        assert rows[0].required_offsets[0] == pytest.approx(3.0)
        assert rows[1].required_offsets[0] == pytest.approx(0.0)

    def test_mismatched_campaign_keys_rejected(self):
        with pytest.raises(ValueError):
            invariance_family({1: 1.0}, {2: 1.0}, [0.0])


class TestReferenceSetSize:
    def test_count_is_quadratic_in_scatter(self):
        """Halving the scatter quarters the control set -- the design lever."""
        big = reference_set_size(8.0, 1.0, sd_is_estimated=False)
        small = reference_set_size(4.0, 1.0, sd_is_estimated=False)
        assert big == pytest.approx(4 * small, rel=0.02)

    def test_estimated_scatter_needs_more_articles_than_known(self):
        known = reference_set_size(2.23, 1.0, sd_is_estimated=False)
        estimated = reference_set_size(2.23, 1.0)
        assert estimated > known

    def test_registered_scan_makes_the_control_set_practical(self):
        assert reference_set_size(7.36, 1.0) > 100      # unregistered, cross-crew
        assert reference_set_size(1.0, 1.0) <= 10       # registered scan

    def test_floor_is_two_articles(self):
        assert reference_set_size(0.01, 100.0) == 2

    @pytest.mark.parametrize("sd,tol", [(0.0, 1.0), (-1.0, 1.0), (1.0, 0.0)])
    def test_rejects_non_positive_inputs(self, sd, tol):
        with pytest.raises(ValueError):
            reference_set_size(sd, tol)


class TestMinimumDetectableRate:
    def test_power_makes_the_threshold_harder_than_significance(self):
        at_power = minimum_detectable_rate(7.36, 150, 1.0, power=0.80)
        at_significance_only = minimum_detectable_rate(7.36, 150, 1.0, power=0.50)
        assert at_power > at_significance_only

    def test_estimated_offset_raises_the_floor(self):
        known = minimum_detectable_rate(2.23, 150, 1.0)
        estimated = minimum_detectable_rate(2.23, 150, 1.0, n_reference=20)
        assert estimated > known

    def test_control_set_puts_a_floor_under_any_number_of_articles(self):
        """More paired articles cannot out-run a small control set."""
        many = minimum_detectable_rate(2.23, 100_000, 1.0, n_reference=20)
        floor = minimum_detectable_rate(2.23, 10**9, 1.0, n_reference=20)
        assert many == pytest.approx(floor, rel=0.01)

    def test_longer_interval_lowers_the_detectable_rate(self):
        one = minimum_detectable_rate(2.23, 150, 1.0, n_reference=20)
        two = minimum_detectable_rate(2.23, 150, 2.0, n_reference=20)
        assert two == pytest.approx(one / 2.0, rel=1e-9)

    def test_rejects_impossible_power(self):
        with pytest.raises(ValueError):
            minimum_detectable_rate(1.0, 10, 1.0, power=1.0)


class TestVarianceBudget:
    def test_residual_dominates_when_gauge_is_small(self):
        b = variance_budget(observed_sd=7.36, gauge_sd=2.23)
        assert b.residual_sd == pytest.approx(math.sqrt(7.36**2 - 2.23**2), abs=1e-6)
        assert b.gauge_fraction_of_variance < 0.10

    def test_gauge_exceeding_observed_clamps_to_zero_residual(self):
        b = variance_budget(observed_sd=1.0, gauge_sd=3.0)
        assert b.residual_sd == 0.0
        assert b.gauge_fraction_of_variance == 1.0


def test_t_quantile_falls_to_normal_for_large_samples():
    assert t_quantile_95(6) == pytest.approx(2.447)
    assert t_quantile_95(10_000) == pytest.approx(1.96)
