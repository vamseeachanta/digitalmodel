"""Collision QRA: independently calculated synthetic examples, no field rates.

Hand case: 1000 transits/year * 0.0002 collisions/transit = 0.2 collisions/year;
0.2 * 0.5 fatalities/collision = 0.1 expected fatalities/year. The probability
is conditional on the counted transits; interaction forces give neither it
nor the consequence. Product basis: IMO FSA (2018), appendix 5 section 3.2.3.
"""

import io

import pytest
from pydantic import ValidationError

from digitalmodel.asset_integrity.safety_case import (
    EvaluationStatus,
    MAHRegister,
    VesselCollisionInputs,
    assess_vessel_collision,
)


def inputs(**changes):
    return VesselCollisionInputs(
        **(
            dict(
                transits_per_year=1000,
                collision_probability_per_transit=0.0002,
                fatalities_per_collision=0.5,
                traffic_basis="Synthetic annual transit count",
                probability_basis="Synthetic conditional collision probability",
                consequence_basis="Synthetic conditional mean fatalities",
            )
            | changes
        )
    )


def assess(traffic=None, **changes):
    return assess_vessel_collision(
        traffic or inputs(),
        **(
            dict(
                hazard_id="passing-collision",
                description="Passing traffic collision",
                barriers=("Traffic control",),
                responsible_roles=("Marine operations",),
            )
            | changes
        ),
    )


def test_hand_calculated_frequency_and_consequence():
    result = assess()
    assert result.hazard.frequency_per_year == pytest.approx(0.2)
    assert result.hazard.expected_fatalities_per_year == pytest.approx(0.1)
    assert result.hazard.status is EvaluationStatus.EVALUATED
    assert result.missing_inputs == ()
    register = MAHRegister(hazards=(result.hazard,))
    assert MAHRegister.from_csv(io.StringIO(register.to_csv())) == register


@pytest.mark.parametrize(
    "probability,consequence,missing",
    [
        (None, 0.5, ("collision_probability_per_transit",)),
        (0.0002, None, ("fatalities_per_collision",)),
        (None, None, ("collision_probability_per_transit", "fatalities_per_collision")),
    ],
)
def test_missing_external_models_are_explicit(probability, consequence, missing):
    result = assess(
        inputs(
            collision_probability_per_transit=probability,
            fatalities_per_collision=consequence,
        )
    )
    assert result.hazard.status is EvaluationStatus.NOT_EVALUATED
    assert result.hazard.expected_fatalities_per_year is None
    assert result.missing_inputs == missing


@pytest.mark.parametrize("probability,expected", [(0, 0), (1, 500)])
def test_probability_boundaries(probability, expected):
    """1000 * 0 * 0.5 = 0; 1000 * 1 * 0.5 = 500 fatalities/year."""
    assert (
        assess(
            inputs(collision_probability_per_transit=probability)
        ).hazard.expected_fatalities_per_year
        == expected
    )


@pytest.mark.parametrize(
    "field,value",
    [
        ("transits_per_year", -1),
        ("transits_per_year", float("inf")),
        ("collision_probability_per_transit", -0.1),
        ("collision_probability_per_transit", 1.1),
        ("collision_probability_per_transit", float("nan")),
        ("collision_probability_per_transit", True),
        ("fatalities_per_collision", -1),
        ("probability_basis", " "),
    ],
)
def test_invalid_inputs_rejected(field, value):
    with pytest.raises(ValidationError):
        inputs(**{field: value})


@pytest.mark.parametrize(
    "forces",
    [
        {"surge": 0, "sway": float("nan"), "yaw": 0},
        {"surge": 0, "sway": 1},
        {"surge": 0, "sway": 1, "yaw": 0, "extra": 1},
    ],
)
def test_bad_force_evidence_rejected(forces):
    with pytest.raises(ValidationError):
        assess(passing_ship_forces=forces)


def test_real_passing_ship_output_is_context_not_collision_consequence():
    """Synthetic dimensions in SI units; no numeric force result is prescribed.

    This tests the real calculator-to-QRA boundary and exact evidence retention.
    The independently calculated QRA answer remains 0.1 fatalities/year for
    either speed because the supplied probability and consequence are unchanged.
    """
    from digitalmodel.hydrodynamics.passing_ship import (
        EnvironmentalConfig,
        PassingShipCalculator,
        VesselConfig,
    )

    dimensions = VesselConfig(length=100, beam=20, draft=5, block_coefficient=0.8)
    calculator = PassingShipCalculator(
        dimensions,
        dimensions,
        environment=EnvironmentalConfig(water_density=1000, current_velocity=0),
    )
    for speed in (1, 2):
        forces = calculator.calculate_forces(separation=100, stagger=0, velocity=speed)
        result = assess(passing_ship_forces=forces)
        assert result.interaction_forces.model_dump() == forces
        assert result.hazard.expected_fatalities_per_year == pytest.approx(0.1)
        incomplete = assess(
            inputs(
                collision_probability_per_transit=None, fatalities_per_collision=None
            ),
            passing_ship_forces=forces,
        )
        assert incomplete.hazard.status is EvaluationStatus.NOT_EVALUATED
