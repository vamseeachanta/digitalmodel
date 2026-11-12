# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Closed-form arithmetic checks for the bounded mudmat slice of issue #264.

Equation locations in src/digitalmodel/geotechnical/mudmat.py:
* Module docstring, lines 11-14: written Nq, Nc and Ngamma equations.
* Function docstring, lines 110-114: dimension_eff = dimension - 2*abs(M)/V.
* Base sliding resistance, lines 198-200: undrained resistance = su*A_eff.

Expected factors follow from the written mathematical limit; expected areas
below are independent rectangular-geometry hand calculations. These checks do
not qualify a standard, soil model, design or the remainder of issue #264.
"""

import math

import pytest

from digitalmodel.geotechnical.mudmat import (
    bearing_capacity_factors,
    mudmat_bearing_capacity,
)


def test_frictionless_bearing_factors_match_closed_form():
    """Derive the zero-angle tuple from the written equations, in radians.

    Nq(0) = exp(0)*tan(pi/4)**2 = 1. The derivative of log(Nq) at zero
    is pi + 2, hence Nq'(0) = pi + 2. Since tan'(0) = 1, the limit of
    (Nq - 1)/tan(phi) is pi + 2. The product (Nq - 1)*tan(phi) tends
    to zero, so Ngamma tends to zero regardless of its finite coefficient.
    """
    assert bearing_capacity_factors(0.0) == (math.pi + 2.0, 1.0, 0.0)


@pytest.mark.parametrize("strength_kpa", [18.0, 31.0])
@pytest.mark.parametrize(
    "axis,moment_knm,expected_width_m,expected_length_m,expected_area_m2",
    [
        ("B", 0.0, 6.0, 10.0, 60.0),
        ("L", 0.0, 6.0, 10.0, 60.0),
        ("B", 1800.0, 3.0, 10.0, 30.0),
        ("L", 1800.0, 6.0, 7.0, 42.0),
        ("L", 4200.0, 6.0, 3.0, 18.0),
    ],
)
def test_undrained_sliding_matches_effective_contact_area(
    strength_kpa,
    axis,
    moment_knm,
    expected_width_m,
    expected_length_m,
    expected_area_m2,
):
    """Use synthetic inputs and independent areas in the cited sliding equation.

    Chosen inputs: B=6 m, L=10 m, V=1200 kN, su in {18, 31} kPa,
    zero embedment, and submerged unit weight 8 kN/m3. The latter two
    inputs do not contribute to undrained base sliding.

    Hand calculations for either moment sign:
    * M=0: e=0, A=6*10=60 m2.
    * abs(M)=1800 kNm: e=1800/1200=1.5 m, reduction=3 m;
      B eccentricity: A=(6-3)*10=30 m2;
      L eccentricity: A=6*(10-3)=42 m2.
    * abs(M)=4200 kNm: e=4200/1200=3.5 m, reduction=7 m;
      L eccentricity: A=6*(10-7)=18 m2, with effective L shorter than B.

    Resistance is the supplied su times these hand-calculated areas
    (kPa*m2 = kN), never an area read back from the calculator. All test
    dimensions, areas and resistances are exactly representable in binary.
    """
    positive = mudmat_bearing_capacity(
        width_b_m=6.0,
        length_l_m=10.0,
        embedment_depth_m=0.0,
        condition="undrained",
        submerged_unit_weight_kn_m3=8.0,
        vertical_load_kn=1200.0,
        moment_knm=moment_knm,
        undrained_shear_strength_kpa=strength_kpa,
        eccentricity_axis=axis,
    )
    negative = mudmat_bearing_capacity(
        width_b_m=6.0,
        length_l_m=10.0,
        embedment_depth_m=0.0,
        condition="undrained",
        submerged_unit_weight_kn_m3=8.0,
        vertical_load_kn=1200.0,
        moment_knm=-moment_knm,
        undrained_shear_strength_kpa=strength_kpa,
        eccentricity_axis=axis,
    )
    for result in (positive, negative):
        assert result.effective_width_m == expected_width_m
        assert result.effective_length_m == expected_length_m
        assert result.effective_area_m2 == expected_area_m2
        assert result.sliding_capacity_kn == strength_kpa * expected_area_m2
    assert positive.effective_area_m2 == negative.effective_area_m2
    assert positive.sliding_capacity_kn == negative.sliding_capacity_kn
