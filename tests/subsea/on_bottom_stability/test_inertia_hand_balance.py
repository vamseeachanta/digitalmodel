# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand balances for the Morison inertia component.

Source of all physical numbers: the synthetic case defined here, chosen for
exact arithmetic; these inputs are assumptions, not calibrated design values.
The reference is displaced circular fluid mass times acceleration, multiplied
by the explicitly specified inertia coefficient. Only inertia is evaluated.

Plan-lite: keep the added coverage in this one test file. Derive expected
forces independently from displaced-fluid mass, then validate the seven cases
with the neighboring hydrodynamic and simplified-interface tests. Exclude
production changes, drag, lift, structural reaction distribution, external
data and licensed solvers.
"""

import math

import pytest

from digitalmodel.subsea.on_bottom_stability.simplified import (
    inertia_force_per_meter,
)


@pytest.fixture
def inertia_case():
    """Define a synthetic cylinder and uniform fluid acceleration in SI units.

    Diameter = 1 m, density = 1000 kg/m^3, acceleration = 2 m/s^2 and
    C_M = 1.5. The radius is therefore 0.5 m and displaced cross-sectional
    fluid mass is 250*pi kg/m. Effective mass is 375*pi kg/m, giving
    750*pi N/m. No drag, lift or stability coefficient enters this helper.
    """
    return {
        "pipe_od_m": 1.0,
        "water_density_kg_m3": 1000.0,
        "acceleration_ms2": 2.0,
        "inertia_coeff": 1.5,
    }


def test_morison_uniform_inertia_balances_total_reaction(inertia_case):
    """Integrate a uniform load along a synthetic 8 m cylinder.

    Hand calculation: displaced volume = pi*(0.5 m)^2*8 m = 2*pi m^3;
    displaced fluid mass = 2000*pi kg; effective mass = 3000*pi kg.
    At 2 m/s^2 the total applied force is 6000*pi N, so equilibrium needs
    a total reaction of -6000*pi N. This is the sum of support reactions,
    not a prediction of their distribution or a structural solver result.
    """
    length_m = 8.0
    uniform_load_n_per_m = inertia_force_per_meter(**inertia_case)
    total_reaction_n = -uniform_load_n_per_m * length_m

    # Analytic hand balance above; tolerance permits floating-point roundoff.
    assert total_reaction_n == pytest.approx(-6000.0 * math.pi, rel=1e-12)


@pytest.mark.parametrize(
    ("changes", "expected_pi_n_per_m"),
    [
        pytest.param({}, 750.0, id="positive-acceleration"),
        pytest.param({"acceleration_ms2": -2.0}, -750.0, id="reversed-acceleration"),
        pytest.param({"acceleration_ms2": 0.0}, 0.0, id="zero-acceleration"),
        pytest.param({"acceleration_ms2": 4.0}, 1500.0, id="double-acceleration"),
        pytest.param({"pipe_od_m": 2.0}, 3000.0, id="double-diameter"),
        pytest.param({"water_density_kg_m3": 2000.0}, 1500.0, id="double-density"),
    ],
)
def test_morison_inertia_sign_and_similarity(
    inertia_case, changes, expected_pi_n_per_m
):
    """Check independent variants of the fixture's 750*pi N/m hand result.

    Reversing acceleration gives -750*pi N/m; zero gives zero. Doubling
    acceleration or density doubles force to 1500*pi N/m. Doubling diameter
    makes radius 1 m and displaced mass 1000*pi kg/m: four times the base
    mass, hence 3000*pi N/m. These are hand results, not helper outputs.
    """
    result = inertia_force_per_meter(**{**inertia_case, **changes})

    # Roundoff tolerances only; the absolute tolerance also covers exact zero.
    assert result == pytest.approx(
        expected_pi_n_per_m * math.pi, rel=1e-12, abs=1e-12
    )
