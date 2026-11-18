# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand checks; source details and derivations are in rod-pump-hand-checks.md.

Sources: OpenStax, University Physics Volume 1 (2016), sections 12.3, 14.1,
14.4; College Physics 2e (2022), table 11.1; NIST SP 811 (2008), appendix B.
All unreferenced inputs are synthetic test choices, not material properties.
The supplied modulus is synthetic; the default steel modulus is NOT_EVALUATED.
"""

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump import (
    analyse_rod_string,
    buoyant_rod_weight,
    fluid_load,
    rod_area,
    rod_elastic_constant,
    spring_rate,
)

# Hand values retain 12 significant digits; 1e-11 allows their final rounding
# and binary floating-point arithmetic, without an engineering error allowance.
REL = 1e-11


@pytest.mark.parametrize(
    "diameter,area",
    [(0.75, 0.441786466911), (0.875, 0.601320468851), (1.0, 0.785398163397)],
)
def test_rod_area_in_square_inches(diameter, area):
    """Circle geometry: pi*d*d/4 gives 9*pi/64, 49*pi/256 and pi/4 in²."""
    assert rod_area(diameter) == pytest.approx(area, rel=REL)


def test_elastic_constant_and_spring_rate_from_hookes_law():
    """OpenStax section 12.3: delta = F*L/(E*A), k = E*A/L.

    Choose d=1 in, L=1000 ft=12000 in and E=12000000 psi (synthetic).
    A=pi/4 in²; Er per foot = 12/(12000000*pi/4)
    = 0.000001273239544735 in/lb/ft. Total compliance is
    0.001273239544735 in/lb; k=785.398163397 lb/in.
    """
    assert rod_elastic_constant(1.0, modulus_psi=12_000_000) == pytest.approx(
        0.000001273239544735, rel=REL
    )
    assert spring_rate(1.0, 1000.0, modulus_psi=12_000_000) == pytest.approx(
        785.398163397, rel=REL
    )


@pytest.mark.parametrize(
    "sg,expected", [(0.0, 1000.0), (0.5, 935.897436), (1.0, 871.794872)]
)
def test_buoyant_weight_from_displaced_fluid(sg, expected):
    """Archimedes: Wb=Wa*(1-SG/rho_relative).

    College Physics 2e table 11.1 gives iron/steel 7.8 and water 1.0 g/cm³.
    For synthetic Wa=1000 lb: SG=0, 0.5, 1 give 1000, 935.897436,
    871.794872 lb. Density is tabulated to 0.1 g/cm³; half its last digit
    propagates to less than 0.9 lb at SG=1. Use abs=0.9 lb for that rounding,
    rather than treating a generic density as an exact alloy specification.
    """
    assert buoyant_rod_weight(1000.0, sg) == pytest.approx(expected, abs=0.9)


def test_buoyant_weight_zero_mass_limit():
    """Archimedes: zero rod mass and displaced volume give zero net weight."""
    assert buoyant_rod_weight(0.0, 1.0) == 0.0


def test_fluid_load_from_pressure_difference():
    """OpenStax section 14.1: F=pressure*area.

    Synthetic d=1 in, zero column and tubing/casing pressures 125/25 psi:
    net pressure=100 psi, area=pi/4 in², load=25*pi=78.5398163397 lb.
    The implementation uses full bore area; no separate rod-area subtraction
    is possible through this interface.
    """
    assert fluid_load(1.0, 0.0, 1.0, 125.0, 25.0) == pytest.approx(
        78.5398163397448, rel=REL
    )


def test_fluid_load_from_fresh_water_column():
    """OpenStax section 14.1, p=rho*g*h; NIST SP 811 appendix B units.

    Water at 4°C: rho=1000 kg/m³. Standard g=9.80665 m/s²,
    1 ft=0.3048 m, 1 psi=6894.757 Pa. Thus gradient=0.4335275224 psi/ft.
    A synthetic 1000 ft column on a 1 in bore gives
    1000*0.4335275224*pi/4=340.491719903 lb.
    Use rel=0.002 for the module's approximate water gradient (a 0.2% bound),
    not as a physical uncertainty or design acceptance criterion.
    """
    assert fluid_load(1.0, 1000.0, 1.0) == pytest.approx(340.491719903, rel=0.002)


def test_single_diameter_analysis_matches_hand_load_groups():
    """Combine the independent checks above with explicit weight and modulus.

    d=1 in, L=1000 ft, E=12000000 psi, weight=1 lb/ft, S=100 in,
    pump d=1 in, zero column, SG=0, tubing/casing=125/25 psi.
    Wa=Wb=1000 lb, Er=0.000001273239544735, k=785.398163397 lb/in,
    S*k=78539.8163397 lb, Fo=78.5398163397 lb, Fo/(S*k)=0.001,
    Wb/(S*k)=0.0127323954474. No API weight-table default is assumed.
    """
    result = analyse_rod_string(
        1.0,
        1000.0,
        100.0,
        1.0,
        0.0,
        0.0,
        tubing_pressure_psi=125.0,
        casing_pressure_psi=25.0,
        weight_lb_per_ft=1.0,
        modulus_psi=12_000_000,
    )
    expected = {
        "diameter_in": 1.0,
        "length_ft": 1000.0,
        "weight_in_air_lb": 1000.0,
        "elastic_constant": 0.000001273239544735,
        "spring_rate_lb_per_in": 785.398163397,
        "stroke_spring_product_lb": 78539.8163397448,
        "buoyant_weight_lb": 1000.0,
        "fluid_load_lb": 78.5398163397448,
        "fo_over_skr": 0.001,
        "wrf_over_skr": 0.0127323954473516,
    }
    for field, value in expected.items():
        assert getattr(result, field) == pytest.approx(value, rel=REL), field
