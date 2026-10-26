# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent phase-demand and lifetime charge checks for deposition factors."""

import pytest

from digitalmodel.cathodic_protection.calcareous_demand import calcareous_demand


def calculate(config, allowance=0):
    """Use synthetic densities, breakdown and capacity for hand calculations."""
    return calcareous_demand(config, {"steel": {
        "area_m2": 100, "i_initial_A_m2": .2, "i_mean_A_m2": .1,
        "i_final_A_m2": .15, "f_ci": .1, "f_cm": .2, "f_cf": .3,
    }}, design_life_years=10, capacity_Ah_kg=2000, utilization_factor=.8,
        individual_mass_kg=20, connected_current_A=allowance)


def test_phase_factors_change_density_demand_and_lifetime_mass():
    result = calculate({"phase_factors": {"initial": 1, "mean": .5, "final": .8}})
    zone = result["zones"]["steel"]
    assert zone["with_calcareous"]["densities_A_m2"] == pytest.approx({
        "initial": .2, "mean": .05, "final": .12,
    })
    assert zone["without_calcareous"]["demands_A"] == pytest.approx({
        "initial": 2, "mean": 2, "final": 4.5,
    })
    assert result["with_calcareous"]["demands_A"] == pytest.approx({
        "initial": 2, "mean": 1, "final": 3.6,
    })
    assert result["without_calcareous"]["total_mass_kg"] == pytest.approx(109.5)
    assert result["with_calcareous"]["total_mass_kg"] == pytest.approx(54.75)
    assert result["with_calcareous"]["count_by_mass"] == 3
    assert result["without_calcareous"]["count_by_mass"] == 6


def test_identity_default_takes_no_deposit_credit():
    result = calculate({})
    assert result["with_calcareous"] == result["without_calcareous"]
    assert result["phase_factors"] == {"initial": 1, "mean": 1, "final": 1}


def test_scalar_factor_applies_to_all_phases_but_not_connected_allowance():
    result = calculate({"factor": .5}, allowance=3)
    assert result["with_calcareous"]["demands_A"] == pytest.approx({
        "initial": 4, "mean": 4, "final": 5.25,
    })
    assert result["with_calcareous"]["total_mass_kg"] == pytest.approx(219)


@pytest.mark.parametrize("config", [
    {"factor": 0}, {"factor": -1}, {"factor": 1.1}, {"factor": float("nan")},
    {"factor": float("inf")}, {"factor": True}, {"factor": "bad"},
    {"phase_factors": {"initial": 1, "mean": .5}},
    {"phase_factors": {"initial": 1, "mean": .5, "final": .7, "typo": 1}},
    {"phase_factors": []}, {"factor": .5, "phase_factors": {}},
    {"factors": .5},
])
def test_invalid_factors_are_rejected(config):
    with pytest.raises(ValueError, match="factor"):
        calculate(config)


def test_non_seawater_zone_and_allowance_keep_reference_demand():
    zone = {"area_m2": 100, "i_initial_A_m2": .2, "i_mean_A_m2": .1,
            "i_final_A_m2": .15, "f_ci": 1, "f_cm": 1, "f_cf": 1,
            "calcareous_applicable": False}
    result = calcareous_demand({"factor": .5}, {"buried": zone},
                              design_life_years=10, capacity_Ah_kg=2000,
                              utilization_factor=.8, individual_mass_kg=20)
    assert result["with_calcareous"] == result["without_calcareous"]
    assert result["zones"]["buried"]["phase_factors"] == dict.fromkeys(("initial", "mean", "final"), 1)
