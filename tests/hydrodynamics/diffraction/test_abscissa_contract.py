"""Tests for the benchmark abscissa contract."""

import re

import numpy as np
import pytest


AQWA_L01_FREQUENCIES = np.array(
    [0.286, 0.331, 0.370, 0.407, 0.715, 1.022, 1.330, 1.637, 1.945, 2.252]
)
ORCAWAVE_L01_FREQUENCIES = np.array(
    [
        0.28559933214452665,
        0.3141592653589793,
        0.3306939635357677,
        0.36959913571644626,
        0.39269908169872414,
        0.41887902047863906,
        0.483321946706122,
        0.5711986642890533,
        0.6283185307179586,
        0.6613879270715354,
        0.6981317007977318,
        0.7391982714328925,
        0.7853981633974483,
        0.8377580409572781,
        0.8975979010256552,
        1.0471975511965976,
        1.2566370614359172,
        1.5707963267948966,
        2.0943951023931953,
        3.141592653589793,
    ]
)


def _api():
    from digitalmodel.hydrodynamics.diffraction import benchmark_abscissa

    return benchmark_abscissa


def test_loader_reorders_raos_with_frequencies():
    api = _api()
    frequencies = np.array([3.0, 1.0, 2.0])
    raos = np.array([[30.0, 300.0], [10.0, 100.0], [20.0, 200.0]])

    ordered = api.order_solver_data(frequencies, raos)

    np.testing.assert_array_equal(
        np.column_stack((ordered.frequencies, ordered.raos)),
        np.array(
            [
                [1.0, 10.0, 100.0],
                [2.0, 20.0, 200.0],
                [3.0, 30.0, 300.0],
            ]
        ),
    )


def test_default_relative_gap_follows_from_damping_and_interval_count():
    """The bound must be *computed* from its inputs, not a literal.

    A previous revision hard-coded ``max_gap=1.1`` rad/s with a physics-shaped
    justification quoting ``omega_n=11 rad/s`` -- a 0.57 s natural period, which
    is not a vessel. 1.1 happened to sit just above the largest gap in the L01
    OrcaWave grid (1.0472 rad/s), i.e. the threshold had been fitted to the data
    it was meant to judge. Asserting the arithmetic makes that unrepeatable:
    changing the constant now requires changing zeta or N, which are named,
    physical, and reviewable.
    """
    api = _api()

    assert api.MAX_RELATIVE_GAP == pytest.approx(
        2.0 * api.DAMPING_RATIO / api.INTERVALS_ACROSS_HALF_POWER_BAND
    )
    assert api.AbscissaConfig().max_relative_gap == pytest.approx(0.10)
    assert api.DAMPING_RATIO == pytest.approx(0.10)
    assert api.INTERVALS_ACROSS_HALF_POWER_BAND == 2


def test_default_minimum_sample_count_is_computed_from_peak_shape():
    api = _api()

    assert api.AbscissaConfig().min_samples == (
        api.PEAK_SAMPLE_COUNT
        + 2 * api.SAMPLES_PER_RESONANCE_FLANK
    )


def test_default_overlap_has_no_underived_fractional_threshold():
    api = _api()

    assert api.AbscissaConfig().min_coverage is None


def test_default_overlap_requires_one_complete_source_domain():
    api = _api()
    first = np.linspace(1.0, 2.0, 11)
    second = np.linspace(1.2, 2.2, 11)

    with pytest.raises(
        api.AbscissaOverlapError,
        match="shared interval must contain one complete source domain",
    ):
        api.build_evaluation_grid(first, second)


def test_custom_thresholds_require_custom_justification():
    api = _api()

    with pytest.raises(ValueError, match="custom thresholds require custom justification"):
        api.AbscissaConfig(min_samples=11)


def test_relative_gap_bound_is_dimensionless_and_scale_free():
    """The same config must accept a decade-shifted copy of an adequate grid.

    This is the property an absolute rad/s bound cannot have, and the reason
    option 3 was chosen over a scalar threshold.
    """
    api = _api()
    fine = np.linspace(1.0, 1.5, 12)

    grid_low = api.build_evaluation_grid(fine, fine.copy())
    grid_high = api.build_evaluation_grid(fine * 10.0, fine.copy() * 10.0)

    np.testing.assert_allclose(grid_high, grid_low * 10.0)


def test_descending_frequencies_raise():
    api = _api()

    with pytest.raises(
        api.AbscissaOrderError,
        match=f"^{re.escape('first abscissa must be strictly increasing')}$",
    ):
        api.build_evaluation_grid(np.array([3.0, 2.0, 1.0]), np.array([1.0, 2.0]))


def test_duplicate_frequencies_raise():
    api = _api()

    with pytest.raises(
        api.AbscissaOrderError,
        match=f"^{re.escape('second abscissa must be strictly increasing')}$",
    ):
        api.build_evaluation_grid(np.array([1.0, 2.0]), np.array([1.0, 1.0, 2.0]))


def test_l01_grids_have_adequate_interval_overlap():
    """Interval coverage is fine -- AQWA's range sits wholly inside OrcaWave's.

    Kept separate from sampling density so the two failure modes stay
    distinguishable. Coincident-node count is NOT overlap; an earlier draft of
    the plan wrongly proposed rejecting these grids because only three abscissa
    values coincide.
    """
    api = _api()
    lower = max(AQWA_L01_FREQUENCIES[0], ORCAWAVE_L01_FREQUENCIES[0])
    upper = min(AQWA_L01_FREQUENCIES[-1], ORCAWAVE_L01_FREQUENCIES[-1])
    smaller_span = min(
        np.ptp(AQWA_L01_FREQUENCIES), np.ptp(ORCAWAVE_L01_FREQUENCIES)
    )

    assert (lower, upper, smaller_span) == pytest.approx(
        (
            AQWA_L01_FREQUENCIES[0],
            AQWA_L01_FREQUENCIES[-1],
            np.ptp(AQWA_L01_FREQUENCIES),
        )
    )


def test_l01_grids_are_refused_for_inadequate_sampling():
    """The real L01 grids cannot resolve a resonance, and must be refused.

    AQWA jumps 0.407 -> 0.715 rad/s, a relative gap of 0.757 -- in period terms
    15.4 s to 8.8 s, straight across the heave/pitch resonance band of a ship.
    Eight of its nine intervals exceed the bound. OrcaWave fails eight of
    nineteen, worst 0.500 at 2.094 rad/s.

    Refusing is the contract working as designed: a benchmark cannot report
    agreement on a band neither solver sampled. This is a finding about the L01
    run, not a defect here -- see digitalmodel#714, which owns re-running the
    ship case on an adequate grid.
    """
    api = _api()

    with pytest.raises(api.AbscissaGapError, match=r"^first source relative gap 0\.7567.* \[0\.407000, 0\.715000\] rad/s"):
        api.build_evaluation_grid(AQWA_L01_FREQUENCIES, ORCAWAVE_L01_FREQUENCIES)


def test_adequately_sampled_grids_are_interpolated_onto_the_coarser_grid():
    """The positive path: dense-enough grids align without complaint."""
    api = _api()
    coarse = np.linspace(1.0, 1.4, 9)
    dense = np.linspace(1.0, 1.4, 17)

    aligned = api.align_responses(
        coarse,
        coarse,
        np.zeros(coarse.size),
        dense,
        dense,
        np.zeros(dense.size),
    )

    np.testing.assert_allclose(aligned.frequencies, coarse)
    np.testing.assert_allclose(aligned.second.magnitude, coarse, rtol=0.0, atol=1e-12)


def test_disjoint_grids_raise():
    api = _api()

    with pytest.raises(
        api.AbscissaOverlapError,
        match=f"^{re.escape('abscissae are disjoint')}$",
    ):
        api.build_evaluation_grid(np.array([0.1, 0.2]), np.array([5.0, 6.0]))


def test_coverage_below_minimum_raises_with_exact_coverage():
    api = _api()
    config = api.AbscissaConfig(
        min_coverage=0.5,
        max_relative_gap=20.0,
        justification="Synthetic partial-overlap error-message policy.",
    )

    with pytest.raises(
        api.AbscissaOverlapError,
        match=(
            f"^{re.escape('shared-interval coverage 0.200000 is below minimum 0.500000')}$"
        ),
    ):
        api.build_evaluation_grid(np.array([1.0, 11.0]), np.array([9.0, 19.0]), config)


def test_source_gap_above_maximum_names_the_offending_interval():
    """The message must identify WHICH band is undersampled.

    "some gap was too big" is not actionable; the run owner needs to know where
    to add frequencies. 1.0 -> 2.0 rad/s is a relative gap of exactly 1.0.
    """
    api = _api()
    config = api.AbscissaConfig(
        min_coverage=0.5,
        max_relative_gap=0.5,
        justification="Synthetic gap error-message policy.",
    )

    with pytest.raises(
        api.AbscissaGapError,
        match=re.escape(
            "first source relative gap 1.000000 over [1.000000, 2.000000] rad/s "
            "exceeds maximum 0.500000"
        ),
    ):
        api.build_evaluation_grid(
            np.array([1.0, 2.0, 2.5]),
            np.array([1.0, 1.2, 1.5, 1.8, 2.2, 2.5]),
            config,
        )


def test_non_positive_frequency_is_rejected():
    """A relative gap is undefined at omega = 0, so reject it at the door."""
    api = _api()

    with pytest.raises(
        api.AbscissaOrderError,
        match=f"^{re.escape('first abscissa must be strictly positive')}$",
    ):
        api.build_evaluation_grid(np.array([0.0, 1.0, 2.0]), np.array([1.0, 2.0]))


def test_evaluation_grid_is_coarser_solver_restricted_to_shared_interval():
    api = _api()
    config = api.AbscissaConfig(
        max_relative_gap=2.0,
        min_coverage=0.5,
        justification="Synthetic evaluation-grid policy.",
    )

    grid = api.build_evaluation_grid(
        np.array([1.0, 2.0, 3.0]),
        np.array([1.5, 2.0, 2.5, 3.0, 3.5]),
        config,
    )

    np.testing.assert_array_equal(grid, np.array([2.0, 3.0]))


def test_evaluation_grid_never_extrapolates_past_shared_interval():
    api = _api()
    config = api.AbscissaConfig(
        max_relative_gap=2.0,
        min_coverage=0.5,
        justification="Synthetic no-extrapolation policy.",
    )

    grid = api.build_evaluation_grid(
        np.array([1.0, 2.0, 3.0]),
        np.array([1.5, 2.0, 2.5, 3.5]),
        config,
    )

    assert (grid[0], grid[-1]) == (2.0, 3.0)


def test_complex_interpolation_preserves_branch_cut_phase():
    api = _api()

    interpolated = api.interpolate_complex_response(
        np.array([1.0, 3.0]),
        np.array([1.0, 1.0]),
        np.array([179.0, -179.0]),
        np.array([2.0]),
    )

    assert interpolated.phase_degrees[0] == pytest.approx(180.0, abs=0.5)


def test_three_compared_samples_return_insufficient_sampling():
    api = _api()
    config = api.AbscissaConfig(min_samples=5)

    result = api.assess_sampling(np.ones(3), np.ones(3), config)

    assert result == api.InsufficientSampling(sample_count=3, correlation=None)


def test_sampling_gate_fires_on_identical_three_point_grids():
    api = _api()
    frequencies = np.array([1.0, 2.0, 3.0])
    magnitude = np.array([1.0, 2.0, 1.0])
    phase = np.zeros(3)

    result = api.align_responses(
        frequencies,
        magnitude,
        phase,
        frequencies.copy(),
        magnitude.copy(),
        phase.copy(),
    )

    assert result == api.InsufficientSampling(sample_count=3, correlation=None)


def test_identical_adequate_grids_pass_values_through_unchanged():
    """Identical grids must round-trip values exactly, wrapping included.

    Phase 270 deg is chosen deliberately: a complex round-trip through
    ``angle()`` returns it as -90 deg, which is the same direction but not the
    same number. Asserting 270.0 therefore pins that an identical grid is
    passed through rather than re-derived -- an earlier version of this test
    asserted only ``phase[0] == 270.0`` on an all-270 array, which any
    implementation satisfies, including one that resamples.

    The grid is spaced to satisfy the relative-gap bound (0.05 <= 0.10); a
    previous fixture of [1,2,3,4,5] had adequate COUNT but relative gaps to
    1.0, so it was refused for spacing and never reached this assertion.
    """
    api = _api()
    frequencies = np.linspace(1.0, 1.3, 7)
    magnitude = np.linspace(2.0, 3.5, 7)
    phase = np.full(7, 270.0)

    result = api.align_responses(
        frequencies,
        magnitude,
        phase,
        frequencies.copy(),
        magnitude.copy(),
        phase.copy(),
    )

    np.testing.assert_array_equal(result.frequencies, frequencies)
    np.testing.assert_array_equal(result.first.magnitude, magnitude)
    np.testing.assert_array_equal(result.first.phase_degrees, phase)
