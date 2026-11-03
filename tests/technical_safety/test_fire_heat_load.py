"""Scenario selection and explicitly evidenced water credit; synthetic fluxes."""

import pytest

from digitalmodel.technical_safety.criteria import get_criteria
from digitalmodel.technical_safety.fire_heat_load import (
    FireScenario,
    FireWaterEvidence,
    screen_fire_heat_load,
    select_fire_types,
)


def scenario(**changes):
    return FireScenario(
        **(
            dict(
                inventory_kg=1000.0,
                release_rate_kg_s=2.0,
                phase="gas",
                pressurised_release=True,
                liquid_pool_possible=False,
                enclosure="open",
                ventilation="fuel_controlled",
            )
            | changes
        )
    )


def water(**changes):
    return FireWaterEvidence(
        **({key: True for key in FireWaterEvidence.model_fields} | changes)
    )


def test_jet_peak_historical_worked_case():
    # A sustained pressurised gas release selects jet. The issue's historical
    # jet peak = 250 kW/m2; area, duration and absorbed wall load are not calculated.
    criteria = get_criteria("s_dp_001_1996_rev2")
    with pytest.raises(ValueError, match="superseded"):
        screen_fire_heat_load(scenario(), criteria)
    (result,) = screen_fire_heat_load(scenario(), criteria, allow_test_criteria=True)
    assert result.fire_type == "jet"
    assert result.heat_flux_kw_m2 == 250
    assert result.criteria.status == "superseded"
    assert result.scenario == scenario()


def test_pool_worked_case_and_edition_switch(project_criteria):
    release = scenario(
        phase="liquid", pressurised_release=False, liquid_pool_possible=True
    )
    (result,) = screen_fire_heat_load(release, project_criteria(), flux_basis="average")
    assert result.fire_type == "pool_fuel_controlled"
    assert result.heat_flux_kw_m2 == 100
    (changed,) = screen_fire_heat_load(
        release, project_criteria("test-B", pool=150), flux_basis="average"
    )
    assert changed.heat_flux_kw_m2 == 150
    assert changed.criterion.citation.edition == "test-B"


def test_mixed_release_retains_both_cases(project_criteria):
    release = scenario(phase="two_phase", liquid_pool_possible=True)
    assert select_fire_types(release) == ("jet", "pool_fuel_controlled")
    results = screen_fire_heat_load(release, project_criteria())
    assert [result.heat_flux_kw_m2 for result in results] == [250, 100]


def test_enclosed_ventilation_controlled_pool(project_criteria):
    release = scenario(
        phase="liquid",
        pressurised_release=False,
        liquid_pool_possible=True,
        enclosure="enclosed",
        ventilation="ventilation_controlled",
    )
    (result,) = screen_fire_heat_load(release, project_criteria())
    assert result.fire_type == "pool_ventilation_controlled"
    assert result.heat_flux_kw_m2 == 180


def test_water_uses_cited_absolute_flux_not_invented_factor(project_criteria):
    (result,) = screen_fire_heat_load(
        scenario(), project_criteria(), flux_basis="average", fire_water=water()
    )
    # Synthetic uncredited average = 200; separately supplied reduced average = 120.
    # No multiplier is inferred from the presence of a water system.
    assert result.heat_flux_kw_m2 == 120
    assert result.fire_water_credit_used
    assert result.unmet_water_conditions == ()
    assert result.fire_water == water()
    release = scenario(
        phase="liquid", pressurised_release=False, liquid_pool_possible=True
    )
    (pool,) = screen_fire_heat_load(
        release, project_criteria(), flux_basis="average", fire_water=water()
    )
    assert pool.heat_flux_kw_m2 == 60


@pytest.mark.parametrize("condition", list(FireWaterEvidence.model_fields))
def test_each_water_condition_is_required(condition, project_criteria):
    (result,) = screen_fire_heat_load(
        scenario(),
        project_criteria(),
        flux_basis="average",
        fire_water=water(**{condition: False}),
    )
    assert result.heat_flux_kw_m2 == 200
    assert not result.fire_water_credit_used
    assert condition in result.unmet_water_conditions


def test_no_peak_credit(project_criteria):
    (result,) = screen_fire_heat_load(
        scenario(), project_criteria(), fire_water=water()
    )
    assert result.heat_flux_kw_m2 == 250
    assert not result.fire_water_credit_used
    assert "average_flux_required" in result.unmet_water_conditions


def test_missing_pool_or_reduced_criteria_refused(project_criteria):
    with pytest.raises(ValueError, match="missing"):
        screen_fire_heat_load(
            scenario(
                phase="liquid", pressurised_release=False, liquid_pool_possible=True
            ),
            get_criteria("s_dp_001_1996_rev2"),
            allow_test_criteria=True,
        )
    data = project_criteria().model_dump()
    del data["values"]["jet_water_average"]
    criteria = type(project_criteria()).model_validate(data)
    with pytest.raises(ValueError, match="missing"):
        screen_fire_heat_load(
            scenario(), criteria, flux_basis="average", fire_water=water()
        )


def test_reduction_must_not_increase_flux(project_criteria):
    data = project_criteria().model_dump()
    data["values"]["jet_water_average"]["value"] = 300
    with pytest.raises(ValueError, match="reduced"):
        screen_fire_heat_load(
            scenario(),
            type(project_criteria()).model_validate(data),
            flux_basis="average",
            fire_water=water(),
        )


@pytest.mark.parametrize(
    "changes",
    [
        dict(inventory_kg=0),
        dict(release_rate_kg_s=-1),
        dict(release_rate_kg_s=float("inf")),
        dict(pressurised_release="yes"),
        dict(phase="gas", liquid_pool_possible=True),
        dict(pressurised_release=False),
        dict(ventilation="ventilation_controlled"),
        dict(phase="liquid", pressurised_release=False),
    ],
)
def test_invalid_or_unsupported_scenarios(changes):
    with pytest.raises(ValueError):
        select_fire_types(scenario(**changes))


def test_uncited_fire_flux_refused(project_criteria):
    data = project_criteria().model_dump()
    del data["values"]["jet_peak"]["citation"]
    with pytest.raises(ValueError):
        screen_fire_heat_load(scenario(), type(project_criteria()).model_validate(data))
