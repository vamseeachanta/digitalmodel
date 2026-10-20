# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent fatigue-growth oracles; all other fixtures are synthetic inputs."""

import math

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


def test_published_infinite_plate_problem():
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
        steps=4000,
    )
    expected = 160.0 / (1e-11 * 950.0**3 * math.pi**1.5)
    assert result.status == "LIMIT_REACHED"
    assert result.life_cycles == pytest.approx(expected, rel=2e-6)
    assert result.history[-1].depth_m == pytest.approx(0.0025)
    assert result.history[-1].cycles == result.life_cycles


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


def test_sqrt_depth_integration_converges_to_hand_integral():
    """Synthetic K=100 sqrt(a), C=1e-11, m=2: N=ln(5)/(1e-7).

    Integrand is 1/(1e-7*a), independently integrated as log(a)/1e-7.
    """
    expected = math.log(5) / 1e-7
    coarse, fine = [
        integrate_growth(
            law(exponent=2),
            lambda a: 100 * math.sqrt(a),
            initial_m=0.001,
            limit_m=0.005,
            steps=n,
        ).life_cycles
        for n in (4, 40)
    ]
    assert abs(fine - expected) < abs(coarse - expected)
    assert fine == pytest.approx(expected, rel=1e-6)


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
