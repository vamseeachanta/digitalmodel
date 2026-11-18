"""Kinetic-energy arithmetic and unit checks with explicit synthetic inputs."""

import pytest
from pydantic import ValidationError

from digitalmodel.foam_system.sizing import Citation
from digitalmodel.technical_safety.accidental_loads import (
    ShipImpactInputs,
    screen_ship_impact,
)
from digitalmodel.technical_safety.criteria import get_criteria


def impact(**changes):
    return ShipImpactInputs(
        **(
            dict(
                mass_kg=5_000_000.0,
                speed_m_s=2.0,
                added_mass_coefficient=0.4,
                added_mass_citation=Citation(
                    "Synthetic scenario", "1", "assumed coefficient"
                ),
            )
            | changes
        )
    )


def test_hand_energy_and_both_applications(project_criteria):
    # a = 0.4 * 5,000,000 = 2,000,000 kg.
    # E = 0.5 * 7,000,000 * 2**2 = 14,000,000 J = 14 MJ.
    for application in ("supply_vessel", "riser_protection"):
        result = screen_ship_impact(
            impact(), project_criteria(), application=application
        )
        assert result.energy_mj == pytest.approx(14.0)
        assert result.within_design_energy
        assert result.margin_mj == pytest.approx(0.0)
        assert result.criterion.citation.edition == "test-A"
        assert result.inputs == impact()


def test_field_units(project_criteria):
    # NIST SP 811 B.8: 1 lb = 0.45359237 kg, 1 ft = 0.3048 m.
    # 10,000 lb at 10 ft/s, Ca=.2:
    # E = .5 * (4535.9237 * 1.2) * 3.048**2 / 1e6 = .02528406605628288 MJ.
    inputs = ShipImpactInputs.from_units(
        mass=10_000.0,
        mass_unit="lb",
        speed=10.0,
        speed_unit="ft/s",
        added_mass_coefficient=0.2,
        added_mass_citation=impact().added_mass_citation,
    )
    result = screen_ship_impact(inputs, project_criteria())
    assert result.energy_mj == pytest.approx(0.02528406605628288)
    assert result.energy_mj == pytest.approx(
        screen_ship_impact(
            impact(mass_kg=4535.9237, speed_m_s=3.048, added_mass_coefficient=0.2),
            project_criteria(),
        ).energy_mj
    )


def test_tonnes_knots(project_criteria):
    inputs = ShipImpactInputs.from_units(
        mass=5000.0,
        mass_unit="tonne",
        speed=2.0 / (1852.0 / 3600.0),
        speed_unit="knot",
        added_mass_coefficient=0.4,
        added_mass_citation=impact().added_mass_citation,
    )
    assert screen_ship_impact(inputs, project_criteria()).energy_mj == pytest.approx(14)


def test_switching_edition_changes_comparator(project_criteria):
    result = screen_ship_impact(impact(), project_criteria("test-B", energy=12.0))
    assert not result.within_design_energy
    assert result.margin_mj == pytest.approx(-2.0)
    assert result.criteria.edition == "test-B"


def test_historical_is_test_only():
    criteria = get_criteria("s_dp_001_1996_rev2")
    with pytest.raises(ValueError, match="superseded"):
        screen_ship_impact(impact(), criteria)
    result = screen_ship_impact(impact(), criteria, allow_test_criteria=True)
    assert result.energy_mj == pytest.approx(14)
    assert result.criteria.status == "superseded"


@pytest.mark.parametrize(
    "field,value",
    [
        ("mass_kg", 0),
        ("mass_kg", -1),
        ("mass_kg", True),
        ("speed_m_s", -1),
        ("speed_m_s", float("nan")),
        ("added_mass_coefficient", -0.1),
        ("added_mass_coefficient", float("inf")),
        ("added_mass_citation", None),
    ],
)
def test_invalid_input(field, value):
    with pytest.raises((ValueError, ValidationError)):
        impact(**{field: value})


@pytest.mark.parametrize(
    "mass_unit,speed_unit",
    [("lbf", "ft/s"), ("ton", "knot"), ("kg", "mph"), ("m", "kg")],
)
def test_ambiguous_or_wrong_units_refused(mass_unit, speed_unit):
    with pytest.raises(ValueError):
        ShipImpactInputs.from_units(
            mass=10.0,
            mass_unit=mass_unit,
            speed=2.0,
            speed_unit=speed_unit,
            added_mass_coefficient=0.4,
            added_mass_citation=impact().added_mass_citation,
        )


def test_zero_speed_and_zero_added_mass(project_criteria):
    assert screen_ship_impact(impact(speed_m_s=0), project_criteria()).energy_mj == 0
    assert (
        screen_ship_impact(
            impact(added_mass_coefficient=0), project_criteria()
        ).energy_mj
        == 10
    )


def test_overflow_refused(project_criteria):
    with pytest.raises(ValueError, match="finite"):
        screen_ship_impact(impact(speed_m_s=1e308), project_criteria())
