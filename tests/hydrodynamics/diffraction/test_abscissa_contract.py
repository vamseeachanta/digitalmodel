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


def test_default_config_declares_resonant_peak_resolution_basis():
    api = _api()

    assert api.AbscissaConfig() == api.AbscissaConfig(
        min_samples=5,
        min_coverage=0.5,
        max_gap=1.1,
        justification=(
            "Physics-derived, not fitted to benchmark data: for the lowest expected "
            "damping ratio zeta=0.10 and limiting natural frequency omega_n=11 rad/s, "
            "the half-power bandwidth is approximately 2*zeta*omega_n=2.2 rad/s. "
            "Using two intervals across that bandwidth gives MAX_GAP <= "
            "zeta*omega_n=1.1 rad/s; MIN_SAMPLES=5 retains the peak and two samples "
            "on each flank, and MIN_COVERAGE=0.5 requires half the narrower source "
            "domain to be shared."
        ),
    )


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


def test_l01_grids_use_the_aqwa_evaluation_points():
    api = _api()

    grid = api.build_evaluation_grid(AQWA_L01_FREQUENCIES, ORCAWAVE_L01_FREQUENCIES)

    np.testing.assert_array_equal(grid, AQWA_L01_FREQUENCIES)


def test_l01_grids_are_interpolated_on_the_aqwa_grid():
    api = _api()
    aqwa_phase = np.zeros(AQWA_L01_FREQUENCIES.size)
    orcawave_phase = np.zeros(ORCAWAVE_L01_FREQUENCIES.size)

    aligned = api.align_responses(
        AQWA_L01_FREQUENCIES,
        AQWA_L01_FREQUENCIES,
        aqwa_phase,
        ORCAWAVE_L01_FREQUENCIES,
        ORCAWAVE_L01_FREQUENCIES,
        orcawave_phase,
    )

    np.testing.assert_allclose(
        aligned.second.magnitude,
        AQWA_L01_FREQUENCIES,
        rtol=0.0,
        atol=1e-12,
    )


def test_disjoint_grids_raise():
    api = _api()

    with pytest.raises(
        api.AbscissaOverlapError,
        match=f"^{re.escape('abscissae are disjoint')}$",
    ):
        api.build_evaluation_grid(np.array([0.1, 0.2]), np.array([5.0, 6.0]))


def test_coverage_below_minimum_raises_with_exact_coverage():
    api = _api()
    config = api.AbscissaConfig(min_coverage=0.5, max_gap=20.0)

    with pytest.raises(
        api.AbscissaOverlapError,
        match=(
            f"^{re.escape('shared-interval coverage 0.200000 is below minimum 0.500000')}$"
        ),
    ):
        api.build_evaluation_grid(np.array([0.0, 10.0]), np.array([8.0, 18.0]), config)


def test_source_gap_above_maximum_raises_with_exact_gap():
    api = _api()
    config = api.AbscissaConfig(min_coverage=0.5, max_gap=1.0)

    with pytest.raises(
        api.AbscissaGapError,
        match=(
            f"^{re.escape('first source gap 1.500000 exceeds maximum 1.000000')}$"
        ),
    ):
        api.build_evaluation_grid(
            np.array([0.0, 0.5, 2.0]),
            np.array([0.0, 1.0, 2.0]),
            config,
        )


def test_evaluation_grid_is_coarser_solver_restricted_to_shared_interval():
    api = _api()
    config = api.AbscissaConfig(max_gap=2.0)

    grid = api.build_evaluation_grid(
        np.array([0.0, 1.0, 2.0]),
        np.array([0.5, 1.0, 1.5, 2.0, 2.5]),
        config,
    )

    np.testing.assert_array_equal(grid, np.array([1.0, 2.0]))


def test_evaluation_grid_never_extrapolates_past_shared_interval():
    api = _api()
    config = api.AbscissaConfig(max_gap=2.0)

    grid = api.build_evaluation_grid(
        np.array([0.0, 1.0, 2.0]),
        np.array([0.5, 1.0, 1.5, 2.5]),
        config,
    )

    assert (grid[0], grid[-1]) == (1.0, 2.0)


def test_complex_interpolation_preserves_branch_cut_phase():
    api = _api()

    interpolated = api.interpolate_complex_response(
        np.array([0.0, 2.0]),
        np.array([1.0, 1.0]),
        np.array([179.0, -179.0]),
        np.array([1.0]),
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


def test_identical_sufficient_grid_is_not_resampled():
    api = _api()
    frequencies = np.arange(5.0)
    magnitude = np.ones(5)
    phase = np.full(5, 270.0)

    result = api.align_responses(
        frequencies,
        magnitude,
        phase,
        frequencies.copy(),
        magnitude.copy(),
        phase.copy(),
    )

    assert result.first.phase_degrees[0] == 270.0
