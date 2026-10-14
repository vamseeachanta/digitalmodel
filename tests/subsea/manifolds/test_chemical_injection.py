# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Multi-well arithmetic example using the existing delivery hydraulics."""

import importlib
import math

import pytest

from digitalmodel.subsea.umbilicals.chemical_flow import ChemicalFluid, UmbilicalTube


def core():
    return importlib.import_module("digitalmodel.subsea.manifolds.chemical_injection")


def demands():
    # Target rates 1e-6 and 2e-6 m3/s from dose fractions times water rates.
    return (core().InjectionDemand(0.001, 0.001, 1000, 0),
            core().InjectionDemand(0.002, 0.001, 2000, 0))


def test_multi_well_distribution_and_balancing_hand_example():
    tube = UmbilicalTube(0.01, 10)
    fluid = ChemicalFluid(1000, 0.01)
    result = core().distribute_chemical(3e-6, demands(), tube, fluid, 10000, 0)
    # Laminar shared supply: dp=128*mu*L*Q/(pi*d^4).
    friction = 128 * 0.01 * 10 * 3e-6 / (math.pi * 0.01**4)
    assert result.delivery.friction_drop == pytest.approx(friction)
    assert result.node_pressure_pa == pytest.approx(10000 - friction)
    assert sum(w.rate_m3_s for w in result.wells) == pytest.approx(3e-6)
    for index, well in enumerate(result.wells):
        q = (index + 1) * 1e-6
        assert well.rate_m3_s == pytest.approx(q)
        assert well.delivered_fraction == pytest.approx(0.001)
        assert well.dose_ratio == pytest.approx(1)
        assert well.restriction_pa_s2_m6 == pytest.approx(
            (result.node_pressure_pa - (index + 1) * 1000) / q**2)
    assert result.hydraulically_feasible


def test_under_dose_and_hydraulic_deficit_are_reported():
    result = core().distribute_chemical(1.5e-6, demands(), UmbilicalTube(0.01, 10),
                                        ChemicalFluid(1000, 0.01), 1500, 0)
    assert [w.dose_ratio for w in result.wells] == pytest.approx([0.5, 0.5])
    assert not result.hydraulically_feasible
    assert result.wells[1].restriction_pa_s2_m6 is None
    assert result.wells[1].pressure_deficit_pa > 0


def test_zero_demand_does_not_call_zero_reynolds_delivery():
    demand = core().InjectionDemand(1, 0, 1000, 0)
    result = core().distribute_chemical(0, (demand,), UmbilicalTube(0.01, 10),
                                        ChemicalFluid(1000, 0.01), 10000, 0)
    assert result.delivery is None
    assert result.wells[0].rate_m3_s == 0
    assert result.wells[0].dose_ratio is None
    with pytest.raises(ValueError):
        core().distribute_chemical(1, (demand,), UmbilicalTube(0.01, 10),
                                    ChemicalFluid(1000, 0.01), 10000, 0)


@pytest.mark.parametrize("args", [(0, 0.1, 1, 0), (1, -0.1, 1, 0),
                                   (1, 0.1, math.nan, 0), (1, 0.1, 1, -1)])
def test_invalid_demand(args):
    with pytest.raises(ValueError):
        core().InjectionDemand(*args)
