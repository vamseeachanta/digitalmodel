# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent fatigue-growth oracles; all other fixtures are synthetic inputs."""

import math

import numpy as np
import pytest

from digitalmodel.fatigue.threshold_growth import (
    _ARRIVAL_DEPTH_FRACTION,
    _DEPTH_RESOLUTION_ULPS,
    _THRESHOLD_ULPS,
    GrowthLaw,
    integrate_growth,
)


def test_classification_bounds_match_guide():
    """Keep docs/domains/asset-integrity/crack-growth-inspection.html aligned."""
    assert _ARRIVAL_DEPTH_FRACTION == 1e-6 and _THRESHOLD_ULPS == _DEPTH_RESOLUTION_ULPS == 4


def test_integration_help_states_classification_values():
    """Public help must state the numeric bounds beside their constant names."""
    doc = integrate_growth.__doc__
    assert "1e-6 (``_ARRIVAL_DEPTH_FRACTION``)" in doc
    assert "four (``_DEPTH_RESOLUTION_ULPS``) ulps" in doc
    assert "four (``_THRESHOLD_ULPS``) ulps" in doc


def law(**overrides):
    """Synthetic parameters; no material qualification is asserted."""
    inputs = dict(
        coefficient=1e-11,
        exponent=3.0,
        threshold=0.0,
        stress_ratio=0.0,
        walker_exponent=1.0,
        basis="Synthetic inputs for independent arithmetic tests",
    )
    return GrowthLaw(**(inputs | overrides))


@pytest.mark.parametrize("steps", [1, 4, 4000])
def test_published_infinite_plate_problem(steps):
    """MIT, Fatigue (2001), Table 1 and Problem 6, pp. 7-8.

    Steel: C=1e-11, m=3; a0=0.1 mm, af=2.5 mm, Delta_sigma=950 MPa.
    K=950*sqrt(pi*a) in MPa sqrt(m); a is converted to metres.
    Integrating da/[C*(950*sqrt(pi*a))**3] by hand gives
    N=2*(100-20)/(1e-11*950**3*pi**1.5) = 3351.4 cycles (rounded).
    This uses the stated Delta_sigma as the range, without doubling it.
    Problem 5's printed general expression has a typographic extra m;
    this oracle integrates Equation (2) directly instead.
    Source: https://web.mit.edu/course/3/3.11/www/modules/fatigue.pdf
    """
    result = integrate_growth(
        law(),
        lambda a: 950.0 * math.sqrt(math.pi * a),
        initial_m=0.0001,
        limit_m=0.0025,
        steps=steps,
    )
    expected = 160.0 / (1e-11 * 950.0**3 * math.pi**1.5)
    assert result.status == "LIMIT_REACHED"
    assert result.life_cycles == pytest.approx(expected, rel=2e-6)
    assert result.history[-1].depth_m == pytest.approx(0.0025)
    assert result.history[-1].cycles == result.life_cycles
    assert 0 <= result.estimated_error_cycles <= 1e-6 + 1e-6 * expected


def test_walker_correction_independent_arithmetic():
    """AFGROW 5.1.2 Eq. 5.1.3: at R=.75, gamma=.5, Keq=2*DeltaK.

    Synthetic C=1e-11, m=3, DeltaK=10 gives 8e-8 m/cycle.
    A 0.004 m extension therefore takes 50,000 cycles.
    """
    result = integrate_growth(
        law(stress_ratio=0.75, walker_exponent=0.5),
        10.0,
        initial_m=0.001,
        limit_m=0.005,
        steps=4,
    )
    assert result.life_cycles == pytest.approx(50_000.0)


@pytest.mark.parametrize("delta_k", [0.0, 4.0])
def test_arrest_has_no_fabricated_endpoint(delta_k):
    """Synthetic threshold=5: below-threshold DeltaK means zero propagation."""
    result = integrate_growth(
        law(threshold=5.0),
        delta_k,
        initial_m=0.001,
        limit_m=0.005,
        steps=4,
    )
    assert result.status == "ARRESTED"
    assert result.life_cycles is None
    assert [point.depth_m for point in result.history] == [0.001]


def test_threshold_is_raw_range_at_service_ratio():
    """Raw 4 is below threshold 5 even though Walker's equivalent is 8."""
    assert law(threshold=5, stress_ratio=0.75, walker_exponent=0.5).rate(4) == 0


@pytest.mark.parametrize("steps", [1, 4, 100])
def test_positive_threshold_arrival_has_finite_life(steps):
    # MIT Fatigue (2001), Eq. (2), integrated with synthetic K=10-1000a:
    # N=(1/25-1/81)/(2000*1e-11)=1,382,716 cycles to a=0.005 m.
    result = integrate_growth(
        law(threshold=5), lambda a: 10 - 1000 * a,
        initial_m=0.001, limit_m=0.005, steps=steps,
    )
    assert result.status == "LIMIT_REACHED"
    assert result.life_cycles == pytest.approx(1_382_716, rel=2e-6)
    assert result.history[-1].depth_m == 0.005
    assert result.history[-1].cycles == result.life_cycles


@pytest.mark.parametrize("fraction", [0.125, 0.25, 0.5, 0.75])
def test_interior_threshold_equality_requires_explicit_resolution(fraction):
    # An isolated touch and an unresolved threshold plateau must never both
    # silently become an infinite life and the maximum inspection interval.
    touch = 0.001 + 0.004 * fraction
    with pytest.raises(ValueError, match="interior threshold equality"):
        integrate_growth(
            law(threshold=5, exponent=1), lambda a: 5 + 1000 * abs(a - touch),
            initial_m=0.001, limit_m=0.005, steps=1,
        )


def test_arrival_at_interior_threshold_plateau_requires_resolution():
    with pytest.raises(ValueError, match="interior threshold equality"):
        integrate_growth(
            law(threshold=5), lambda a: max(5, 10 - 1000 * a),
            initial_m=0.001, limit_m=0.006, steps=5,
        )


def test_threshold_arrival_uses_walker_limiting_rate():
    # AFGROW 5.1.2 Eq. (5.1.3): R=.75, gamma=.5 doubles equivalent K;
    # with m=3, N=(1/25-1/81)/(2000*1e-11*8)=172,839.5 cycles.
    result = integrate_growth(
        law(threshold=5, stress_ratio=0.75, walker_exponent=0.5),
        lambda a: 10 - 1000 * a, initial_m=0.001, limit_m=0.005, steps=4,
    )
    assert result.life_cycles == pytest.approx(172_839.5, rel=2e-6)


@pytest.mark.parametrize("steps", [1, 2, 4])
@pytest.mark.parametrize("delta_k", [math.nextafter(5, 0), 5, math.nextafter(5, math.inf)])
def test_stationary_threshold_comparison_remains_exact(delta_k, steps):
    assert law(threshold=5).rate(delta_k) == (
        0 if delta_k <= 5 else 1e-11 * delta_k**3
    )
    if delta_k <= 5:
        with pytest.raises(ValueError, match="threshold equality"):
            integrate_growth(
                law(threshold=5), delta_k, initial_m=0.001, limit_m=0.005, steps=steps,
            )
        return
    result = integrate_growth(
        law(threshold=5), delta_k, initial_m=0.001, limit_m=0.005, steps=steps,
    )
    assert result.status == ("ARRESTED" if delta_k <= 5 else "LIMIT_REACHED")


def test_threshold_at_initial_depth_requires_resolution():
    with pytest.raises(ValueError, match="threshold equality"):
        integrate_growth(
            law(threshold=5), lambda a: 5 + 1000 * (a - 0.001),
            initial_m=0.001, limit_m=0.005, steps=1,
        )


@pytest.mark.parametrize("direction", [0.0, None, math.inf])
def test_endpoint_threshold_arrival_tolerates_roundoff(direction):
    def delta_k(depth):
        if depth == 0.005 and direction is not None:
            return math.nextafter(5, direction)
        return 10 - 1000 * depth

    result = integrate_growth(
        law(threshold=5), delta_k, initial_m=0.001, limit_m=0.005, steps=4,
    )
    assert result.status == "LIMIT_REACHED"
    assert result.life_cycles == pytest.approx(1_382_716, rel=2e-6)


def test_arrest_inside_interval_preserves_only_reached_depth():
    result = integrate_growth(
        law(threshold=5),
        lambda a: 10 if a < 0.003 else 4,
        initial_m=0.001,
        limit_m=0.005,
        steps=4,
    )
    assert result.status == "ARRESTED"
    assert result.life_cycles is None
    assert result.history[-1].depth_m == pytest.approx(0.002)


@pytest.mark.parametrize(
    "overrides",
    [
        {"coefficient": 0},
        {"exponent": -1},
        {"threshold": -1},
        {"stress_ratio": -0.1},
        {"stress_ratio": 1},
        {"walker_exponent": 1.1},
        {"basis": " "},
        {"coefficient": float("nan")},
        {"threshold": float("inf")},
    ],
)
def test_invalid_material_inputs_rejected(overrides):
    with pytest.raises(ValueError):
        law(**overrides)


@pytest.mark.parametrize("delta_k", [-1, float("nan"), float("inf")])
def test_invalid_k_rejected(delta_k):
    with pytest.raises(ValueError):
        law().rate(delta_k)


@pytest.mark.parametrize("steps", [0, -1, 1.5, True])
def test_invalid_resolution_rejected(steps):
    with pytest.raises(ValueError):
        integrate_growth(law(), 10, initial_m=0.001, limit_m=0.005, steps=steps)


def test_already_at_limit_has_zero_remaining_life():
    result = integrate_growth(law(), 10, initial_m=0.005, limit_m=0.005, steps=1)
    assert result.life_cycles == 0
    assert len(result.history) == 1


def test_sqrt_depth_coarse_starts_converge_to_hand_integral():
    """Synthetic K=100 sqrt(a), C=1e-11, m=2: N=ln(5)/(1e-7).

    Integrand is 1/(1e-7*a), independently integrated as log(a)/1e-7.
    """
    expected = math.log(5) / 1e-7
    results = [
        integrate_growth(
            law(exponent=2),
            lambda a: 100 * math.sqrt(a),
            initial_m=0.001,
            limit_m=0.005,
            steps=n,
        )
        for n in (4, 40)
    ]
    for result in results:
        assert result.life_cycles == pytest.approx(expected, rel=1e-6)
        assert result.estimated_error_cycles <= 1e-6 + 1e-6 * expected


@pytest.mark.parametrize(
    "values",
    [
        {"initial_m": 0},
        {"limit_m": float("inf")},
        {"initial_m": float("nan")},
    ],
)
def test_invalid_depths_rejected(values):
    inputs = dict(initial_m=0.001, limit_m=0.005, steps=4)
    with pytest.raises(ValueError):
        integrate_growth(law(), 10, **(inputs | values))


def test_unconverged_growth_raises_instead_of_returning_life():
    with pytest.raises(ValueError, match="converg"):
        integrate_growth(
            law(), lambda a: math.sqrt(a), initial_m=0.0001,
            limit_m=0.0025, steps=1, max_refinements=2,
            relative_tolerance=1e-12, absolute_tolerance=0,
        )


@pytest.mark.parametrize("fraction", [0.25, 0.125])
def test_refinement_detects_threshold_dip_between_original_nodes(fraction):
    dip = 0.001 + 0.004 * fraction
    result = integrate_growth(
        law(threshold=5), lambda a: 4 if abs(a - dip) < 1e-12 else 10,
        initial_m=0.001, limit_m=0.005, steps=1,
    )
    assert result.status == "ARRESTED"
    assert result.life_cycles is None
    assert result.estimated_error_cycles is None
    assert [point.depth_m for point in result.history] == [0.001]


@pytest.mark.parametrize(
    "controls",
    [
        {"relative_tolerance": -1}, {"relative_tolerance": float("nan")},
        {"absolute_tolerance": -1}, {"absolute_tolerance": float("inf")},
        {"relative_tolerance": 0, "absolute_tolerance": 0},
        {"max_refinements": 0}, {"max_refinements": 1.5},
    ],
)
def test_invalid_error_controls_rejected(controls):
    with pytest.raises(ValueError):
        integrate_growth(
            law(), 10, initial_m=0.001, limit_m=0.005, steps=1, **controls,
        )


def test_partial_history_preserves_cumulative_cycles_on_arrest():
    result = integrate_growth(
        law(threshold=5), lambda a: 10 if a < 0.004 else 0,
        initial_m=0.001, limit_m=0.005, steps=4,
    )
    assert [point.depth_m for point in result.history] == pytest.approx(
        [0.001, 0.002, 0.003]
    )
    assert [point.cycles for point in result.history] == pytest.approx(
        [0, 100_000, 200_000]
    )


def test_nonfinite_reciprocal_rate_rejected():
    with pytest.raises(ValueError):
        integrate_growth(
            law(coefficient=1e-310, exponent=1), 1,
            initial_m=0.001, limit_m=0.005, steps=1,
        )


@pytest.mark.parametrize("value", [True, False, np.bool_(True), "1", None, 1j, float("nan"), float("inf")])
@pytest.mark.parametrize("integer", [False, True])
def test_shared_validator_rejects_invalid_scalars(value, integer):
    from digitalmodel.fatigue.threshold_growth import validate_scalar

    with pytest.raises(ValueError):
        validate_scalar(value, "input", integer=integer)


@pytest.mark.parametrize("value, integer, expected", [(np.int64(3), True, 3), (np.int32(2), False, 2.0), (np.float32(1.5), False, 1.5)])
def test_shared_validator_accepts_numpy_reals_and_integers(value, integer, expected):
    from digitalmodel.fatigue.threshold_growth import validate_scalar

    assert validate_scalar(value, "input", integer=integer) == expected


@pytest.mark.parametrize("field", ["coefficient", "exponent", "threshold", "stress_ratio", "walker_exponent"])
@pytest.mark.parametrize("value", [True, np.bool_(False), "0", None])
def test_material_scalar_types_rejected(field, value):
    with pytest.raises(ValueError):
        law(**{field: value})


@pytest.mark.parametrize("value", [True, np.bool_(False), "10", None])
def test_delta_k_scalar_types_rejected(value):
    with pytest.raises(ValueError):
        law().rate(value)


@pytest.mark.parametrize("field", ["initial_m", "limit_m", "steps", "relative_tolerance", "absolute_tolerance", "max_refinements"])
@pytest.mark.parametrize("value", [True, np.bool_(False), "1", None])
def test_integration_scalar_types_rejected(field, value):
    controls = dict(initial_m=0.001, limit_m=0.005, steps=1)
    controls[field] = value
    with pytest.raises(ValueError):
        integrate_growth(law(), 10, **controls)


def test_numpy_integer_resolution_accepted():
    result = integrate_growth(
        law(), np.float64(10), initial_m=np.float64(0.001),
        limit_m=0.005, steps=np.int64(4), max_refinements=np.int32(20),
    )
    assert result.life_cycles == pytest.approx(400_000)


@pytest.mark.parametrize("delta_k", [True, "10", None])
def test_scalar_delta_k_validated_even_at_limit(delta_k):
    with pytest.raises(ValueError):
        integrate_growth(law(), delta_k, initial_m=0.005, limit_m=0.005, steps=1)


@pytest.mark.parametrize("initial_m", [0.001, 0.005])
def test_one_refinement_rejected_before_constant_growth_or_early_return(initial_m):
    with pytest.raises(ValueError, match="max_refinements.*at least 2"):
        integrate_growth(
            law(), 10, initial_m=initial_m, limit_m=0.005,
            steps=1, max_refinements=1,
        )


def test_two_refinements_accept_constant_growth():
    result = integrate_growth(
        law(), 10, initial_m=0.001, limit_m=0.005,
        steps=1, max_refinements=2,
    )
    assert result.life_cycles == pytest.approx(400_000)


def test_final_history_endpoint_equals_supplied_limit_exactly():
    result = integrate_growth(
        law(), 10, initial_m=0.001, limit_m=0.013, steps=1,
    )
    assert result.history[-1].depth_m == 0.013


@pytest.mark.parametrize("invalid_law", [None, object(), True, "law"])
@pytest.mark.parametrize("initial_m", [0.001, 0.005])
def test_invalid_growth_law_rejected_even_at_limit(invalid_law, initial_m):
    with pytest.raises(ValueError, match="GrowthLaw"):
        integrate_growth(
            invalid_law, 10, initial_m=initial_m, limit_m=0.005, steps=1,
        )


def test_large_absolute_tolerance_keeps_positive_corrected_life():
    result = integrate_growth(
        law(coefficient=1, exponent=1),
        lambda a: 1 / (1 + 1e12 * math.exp(-((a - 0.003) / 1e-7)**2)),
        initial_m=0.001, limit_m=0.005, steps=1, max_refinements=2,
        absolute_tolerance=1e300, relative_tolerance=0,
    )
    assert math.isfinite(result.life_cycles)
    assert result.life_cycles > 0
    assert result.history[-1].cycles > 0


@pytest.mark.parametrize("steps", [1, 4, 100])
def test_power_law_threshold_arrival_tolerates_roundoff(steps):
    initial, limit, amplitude, power = 0.001, 0.00982, 12.936, 0.41611251118884224
    def delta_k(a):
        return amplitude * (initial / a)**power

    assert delta_k(limit) == math.nextafter(5.0, 0.0)
    # Independent primitive of a**(3*power)/(C*amplitude**3*initial**(3*power)).
    exponent = 3 * power + 1
    expected = (limit**exponent - initial**exponent) / (
        exponent * 1e-11 * amplitude**3 * initial**(3 * power)
    )
    result = integrate_growth(
        law(threshold=5), delta_k, initial_m=initial, limit_m=limit, steps=steps,
    )
    assert result.status == "LIMIT_REACHED"
    assert result.life_cycles == pytest.approx(expected, rel=2e-6)
    assert result.history[-1].depth_m == limit
    assert result.history[-1].cycles == result.life_cycles
    assert result.estimated_error_cycles <= 1e-6 + 1e-6 * expected


def test_resolved_arrest_before_limit_is_not_promoted_to_arrival():
    result = integrate_growth(
        law(threshold=5), lambda a: 10 - 1000 * a,
        initial_m=0.001, limit_m=0.00501, steps=4,
    )
    assert result.status == "ARRESTED"
    assert result.life_cycles is None
    assert result.history[-1].depth_m < 0.005


@pytest.mark.parametrize("policy, status", [("arrest", "ARRESTED"), ("propagate", "LIMIT_REACHED")])
def test_explicit_threshold_equality_policy_resolves_constant_input(policy, status):
    result = integrate_growth(
        law(threshold=5), 5, initial_m=0.001, limit_m=0.005, steps=4,
        threshold_equality=policy,
    )
    assert result.status == status
    if policy == "propagate":
        assert result.life_cycles == pytest.approx(0.004 / (1e-11 * 5**3))
    else:
        assert result.life_cycles is None
        assert len(result.history) == 1


def test_default_equality_error_names_resolution_keyword():
    with pytest.raises(ValueError, match="threshold_equality"):
        integrate_growth(law(threshold=5), 5, initial_m=0.001, limit_m=0.005, steps=4)


@pytest.mark.parametrize("policy", [None, True, "ignore", [], {}])
def test_invalid_threshold_equality_policy_rejected_before_early_return(policy):
    with pytest.raises(ValueError, match="threshold_equality"):
        integrate_growth(
            law(), 10, initial_m=0.005, limit_m=0.005, steps=4,
            threshold_equality=policy,
        )


@pytest.mark.parametrize("endpoint_ulps, probe_ulps", [(0, 0), (-1, -1), (-4, -4)])
@pytest.mark.parametrize("policy", ["raise", "arrest", "propagate"])
def test_unresolved_endpoint_threshold_uses_explicit_policy(endpoint_ulps, probe_ulps, policy):
    initial, limit, steps, refinements = 0.001, 0.005, 4, 20
    resolution = math.ldexp((limit - initial) / steps, -refinements)
    probe = limit - resolution

    def delta_k(a):
        if a == limit:
            return 5 + endpoint_ulps * math.ulp(5.0)
        if a == probe:
            return 5 + probe_ulps * math.ulp(5.0)
        # Positive growth at all earlier samples, with an unresolved final
        # threshold comparison. The reciprocal rate is essentially constant.
        return 5 + 1e-7 * (limit - a)

    args = dict(initial_m=initial, limit_m=limit, steps=steps, threshold_equality=policy)
    if policy == "raise":
        with pytest.raises(ValueError, match="endpoint threshold equality.*threshold_equality"):
            integrate_growth(law(threshold=5), delta_k, **args)
        return
    result = integrate_growth(law(threshold=5), delta_k, **args)
    assert result.status == ("ARRESTED" if policy == "arrest" else "LIMIT_REACHED")
    if policy == "arrest":
        assert result.life_cycles is None
        assert result.history[-1].depth_m < limit
    else:
        # Independent constant-rate limit: .004 / (1e-11 * 5**3).
        assert result.life_cycles == pytest.approx(3_200_000, rel=1e-6)
        assert result.history[-1].depth_m == limit


@pytest.mark.parametrize("endpoint_ulps, probe_ulps", [(-5, -1), (-1, -5)])
def test_resolved_subthreshold_endpoint_does_not_use_equality_policy(endpoint_ulps, probe_ulps):
    initial, limit = 0.001, 0.005
    probe = limit - math.ldexp((limit - initial) / 4, -20)

    def delta_k(a):
        if a == limit:
            return 5 + endpoint_ulps * math.ulp(5.0)
        if a == probe:
            return 5 + probe_ulps * math.ulp(5.0)
        return 5 + 1e-7 * (limit - a)

    result = integrate_growth(
        law(threshold=5), delta_k, initial_m=initial, limit_m=limit, steps=4,
    )
    assert result.status == "ARRESTED"
    assert result.life_cycles is None


@pytest.mark.parametrize("gap_ratio, status", [(0.99, "LIMIT_REACHED"), (1.01, "ARRESTED")])
def test_endpoint_depth_resolution_boundary(gap_ratio, status):
    initial, limit, steps = 0.001, 0.005, 4
    resolution = math.ldexp((limit - initial) / steps, -20)
    arrest = limit - gap_ratio * resolution
    result = integrate_growth(
        law(threshold=5), lambda a: 5 + 1000 * (arrest - a),
        initial_m=initial, limit_m=limit, steps=steps,
    )
    assert result.status == status
    if status == "LIMIT_REACHED":
        assert result.life_cycles == pytest.approx(1_382_716.05, rel=2e-6)
        assert result.history[-1].depth_m == limit
    else:
        assert result.life_cycles is None
        assert result.history[-1].depth_m < arrest


def test_adaptive_samples_inside_unresolved_endpoint_gap_use_arrival_rule():
    # A short path keeps this sampled gap within the fixed depth bound.
    limit, refinements = 0.005, 2
    initial = limit - 1e-8
    resolution = math.ldexp(limit - initial, -refinements)
    arrest = limit - 0.99 * resolution
    sampled = []

    def delta_k(a):
        sampled.append(a)
        return 5 + 1000 * (arrest - a)

    result = integrate_growth(
        law(threshold=5), delta_k, initial_m=initial, limit_m=limit, steps=1,
        max_refinements=refinements, absolute_tolerance=1e6, relative_tolerance=0,
    )
    assert any(arrest < a < limit for a in sampled)
    assert result.status == "LIMIT_REACHED"
    assert math.isfinite(result.life_cycles)
    assert result.history[-1].depth_m == limit


@pytest.mark.parametrize("fraction", [0.8, 0.9, 0.97])
@pytest.mark.parametrize("refinements", [2, 3, 4, 6, 20])
def test_resolved_arrest_is_independent_of_quadrature_budget(fraction, refinements):
    initial, limit = 0.001, 0.005
    arrest = initial + fraction * (limit - initial)
    result = integrate_growth(
        law(threshold=5), lambda a: 5 + 1000 * (arrest - a),
        initial_m=initial, limit_m=limit, steps=1,
        max_refinements=refinements, absolute_tolerance=1e6, relative_tolerance=0,
    )
    assert result.status == "ARRESTED"
    assert result.life_cycles is None
    assert result.estimated_error_cycles is None
    assert result.history[-1].depth_m < arrest


@pytest.mark.parametrize("gap_ratio, status", [(0.99, "LIMIT_REACHED"), (1.01, "ARRESTED")])
def test_low_refinement_arrival_gap_has_fixed_relative_depth_bound(gap_ratio, status):
    initial, limit = 0.001, 0.005
    arrest = limit - gap_ratio * (1e-6 * limit)
    result = integrate_growth(
        law(threshold=5), lambda a: 5 + 1000 * (arrest - a),
        initial_m=initial, limit_m=limit, steps=1, max_refinements=2,
        absolute_tolerance=1e6, relative_tolerance=0,
    )
    assert result.status == status
    if status == "LIMIT_REACHED":
        assert math.isfinite(result.life_cycles)
        assert result.history[-1].depth_m == limit
    else:
        assert result.life_cycles is None
        assert result.history[-1].depth_m < arrest


@pytest.mark.parametrize("steps", [1, 2, 4])
@pytest.mark.parametrize("policy", ["raise", "arrest", "propagate"])
def test_near_threshold_initial_sample_uses_explicit_policy(steps, policy):
    args = dict(
        initial_m=0.001, limit_m=0.005, steps=steps, threshold_equality=policy,
    )
    delta_k = math.nextafter(5, 0)
    if policy == "raise":
        with pytest.raises(ValueError, match="initial or interior threshold equality"):
            integrate_growth(law(threshold=5), delta_k, **args)
        return
    result = integrate_growth(law(threshold=5), delta_k, **args)
    assert result.status == ("ARRESTED" if policy == "arrest" else "LIMIT_REACHED")
    if policy == "propagate":
        assert result.life_cycles == pytest.approx(3_200_000)
    else:
        assert result.life_cycles is None
        assert [p.depth_m for p in result.history] == [0.001]


def test_near_threshold_interior_sample_requires_explicit_policy():
    with pytest.raises(ValueError, match="initial or interior threshold equality"):
        integrate_growth(
            law(threshold=5),
            lambda a: math.nextafter(5, 0) if a == 0.003 else 6,
            initial_m=0.001, limit_m=0.005, steps=1,
        )
