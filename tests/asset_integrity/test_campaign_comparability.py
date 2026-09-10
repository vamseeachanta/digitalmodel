"""Tests for campaign comparability.

The invariance test is the one that matters: it asserts the negative result the
module exists to establish, so if it ever fails the module's central claim has
been broken.
"""

from __future__ import annotations

import math
import statistics as st

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
    def test_raw_observations_are_reproduced_under_every_assumed_rate(self):
        """The central negative result, tested against synthetic RAW readings.

        Rather than re-running the module's own arithmetic, this builds
        observations from a known truth, derives the contrasts from them, and
        then checks that a DIFFERENT assumed rate reconstructs the very same
        readings once its offsets are applied. That is the confounding.
        """
        elapsed = {1: 0.6, 2: 1.4}
        baselines = [350.0, 355.0, 360.0, 344.0]
        true_rate, true_offset = 4.0, {0: 0.0, 1: 1.5, 2: -0.5}

        def reading(base, j):
            t = 0.0 if j == 0 else elapsed[j]
            return base - true_rate * t + true_offset[j]

        obs = {j: [reading(b, j) for b in baselines] for j in (0, 1, 2)}
        contrasts = {j: st.mean([obs[0][i] - obs[j][i]
                                 for i in range(len(baselines))])
                     for j in (1, 2)}

        for row in invariance_family(contrasts, elapsed, [0.0, true_rate, 9.0]):
            for k, ref_minus in zip(row.campaigns,
                                    row.reference_minus_campaign_offset):
                # The returned value is offset[ref] - offset[k], so campaign k's
                # own offset under this assumed rate is its negation.
                offset_k = -ref_minus
                for i in range(len(baselines)):
                    rebuilt = (obs[0][i] - row.assumed_rate * elapsed[k] + offset_k)
                    assert rebuilt == pytest.approx(obs[k][i], abs=1e-9)

    def test_offsets_move_with_the_assumed_rate(self):
        rows = invariance_family({1: 3.0}, {1: 0.5}, [0.0, 6.0])
        assert rows[0].reference_minus_campaign_offset[0] == pytest.approx(3.0)
        assert rows[1].reference_minus_campaign_offset[0] == pytest.approx(0.0)

    def test_campaign_keys_are_carried_with_their_offsets(self):
        rows = invariance_family({"b": 1.0, "a": 2.0}, {"b": 1.0, "a": 1.0}, [0.0])
        assert rows[0].campaigns == ("a", "b")
        assert rows[0].reference_minus_campaign_offset == (2.0, 1.0)

    def test_non_finite_input_rejected(self):
        with pytest.raises(ValueError):
            invariance_family({1: float("nan")}, {1: 1.0}, [0.0])

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

    def test_returned_count_actually_meets_the_tolerance(self):
        """The bug this replaced: fixed-point iteration returned counts that did not.

        At sd == tol the iteration cycles 6, 7, 6 and could return 6, whose
        half-width is 1.05. At sd == tol/2 it oscillates 41, 2 and could return
        the floor of 2, whose half-width is 4.49 -- more than four times what
        was asked for.
        """
        for sd, tol in ((7.36, 1.0), (2.23, 1.0), (1.0, 1.0), (0.5, 1.0),
                        (0.5, 2.0), (4.9, 0.5), (3.0, 0.25)):
            m = reference_set_size(sd, tol)
            half = t_quantile_95(m - 1) * sd / math.sqrt(m)
            assert half <= tol, f"sd={sd} tol={tol} returned {m}, half-width {half}"
            # and it must be the SMALLEST such count
            if m > 2:
                prev = t_quantile_95(m - 2) * sd / math.sqrt(m - 1)
                assert prev > tol, f"sd={sd} tol={tol}: {m-1} would have done"

    def test_known_answers(self):
        assert reference_set_size(7.36, 1.0) == 211
        assert reference_set_size(2.23, 1.0) == 22
        assert reference_set_size(1.0, 1.0) == 7
        assert reference_set_size(0.5, 1.0) == 4

    def test_floor_is_two_articles(self):
        assert reference_set_size(0.01, 100.0) == 2

    def test_infeasible_requirement_raises_rather_than_capping(self):
        with pytest.raises(ValueError, match="cannot be met"):
            reference_set_size(100.0, 0.001, max_articles=50)

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


class TestTQuantile:
    def test_tabulated_values(self):
        assert t_quantile_95(6) == pytest.approx(2.447)
        assert t_quantile_95(17) == pytest.approx(2.110)
        assert t_quantile_95(30) == pytest.approx(2.042)

    def test_expansion_above_the_table(self):
        assert t_quantile_95(40) == pytest.approx(2.021, abs=0.002)
        assert t_quantile_95(60) == pytest.approx(2.000, abs=0.002)
        assert t_quantile_95(120) == pytest.approx(1.980, abs=0.002)

    def test_falls_to_normal_for_large_samples(self):
        assert t_quantile_95(10_000) == pytest.approx(1.96, abs=1e-3)

    def test_monotone_decreasing(self):
        """Rounding df the wrong way understated every count derived from it."""
        vals = [t_quantile_95(d) for d in range(1, 200)]
        assert all(a > b for a, b in zip(vals, vals[1:]))

    def test_below_one_degree_of_freedom_is_infinite(self):
        assert t_quantile_95(0) == float("inf")


def test_normal_quantile_matches_reference_values():
    from digitalmodel.asset_integrity.campaign_comparability import _normal_quantile
    for p, want in ((0.5, 0.0), (0.975, 1.959964), (0.8, 0.841621),
                    (0.025, -1.959964), (0.001, -3.090232)):
        assert _normal_quantile(p) == pytest.approx(want, abs=1e-6)


class TestVarianceBudgetConsistency:
    def test_inconsistent_estimates_are_flagged(self):
        assert not variance_budget(1.0, 3.0).estimates_consistent
        assert variance_budget(7.36, 2.23).estimates_consistent
