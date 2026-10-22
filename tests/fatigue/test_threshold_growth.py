# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent fatigue-growth oracles; all other fixtures are synthetic inputs."""

import math

import numpy as np
import pytest

from digitalmodel.fatigue.threshold_growth import GrowthLaw, integrate_growth


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


@pytest.mark.parametrize("delta_k", [0.0, 4.0, 5.0])
def test_arrest_has_no_fabricated_endpoint(delta_k):
    """Synthetic threshold=5: DeltaK <= threshold means zero propagation."""
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
