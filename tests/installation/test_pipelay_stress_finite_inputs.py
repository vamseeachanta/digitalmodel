"""Finite-input regressions with synthetic, independently calculated values.

The synthetic control uses E = 120000, OD = 0.8, SMYS = 320 and f = 0.75
in consistent units. By hand: allowable = 0.75 * 320 = 240;
R_min = 120000 * (0.8 / 2) / 240 = 200;
sigma(R_min) = 120000 * 0.4 / 200 = 240.
Extreme cases use Python's binary floating-point limits, rather than physical
design values, to exercise overflow and underflow at each arithmetic step.
"""

import math
import sys

import pytest

from digitalmodel.installation.pipelay.stress import (
    allowable_bend_radius,
    axial_stress,
    bending_stress_from_radius,
    smys_stress_check,
)


BENDING = dict(outer_diameter=0.8, bend_radius=200.0, youngs_modulus=120000.0)
CHECK = dict(**BENDING, smys=320.0, allowable_fraction=0.75)
RADIUS = {name: value for name, value in CHECK.items() if name != "bend_radius"}
# Synthetic axial stress: N/A = 12 / 0.04 = 300, in consistent units.
AXIAL = dict(axial_force=12.0, steel_area=0.04)

INPUT_CASES = [
    (helper, arguments, parameter)
    for helper, arguments in (
        (bending_stress_from_radius, BENDING),
        (axial_stress, AXIAL),
        (smys_stress_check, dict(**CHECK, **AXIAL)),
        # Even when unused in the stress sum, a supplied area must be finite.
        (smys_stress_check, dict(**CHECK, axial_force=0.0, steel_area=0.04)),
        (allowable_bend_radius, RADIUS),
    )
    for parameter in arguments
]


@pytest.mark.parametrize("invalid", [math.nan, math.inf, -math.inf])
@pytest.mark.parametrize("helper,arguments,parameter", INPUT_CASES)
def test_stress_helpers_reject_nonfinite_inputs(helper, arguments, parameter, invalid):
    supplied = dict(arguments, **{parameter: invalid})
    with pytest.raises(ValueError, match=parameter):
        helper(**supplied)


# The factors 2, 4 and 0.5 below respectively expose the outer-fibre OD/2
# factor, overflow of a finite product, and overflow of a finite quotient.
MAX = sys.float_info.max
MIN = sys.float_info.min
SMALLEST = math.ulp(0.0)  # Smallest positive representable float.


@pytest.mark.parametrize(
    "helper,arguments,quantity",
    [
        (
            bending_stress_from_radius,
            dict(outer_diameter=4.0, bend_radius=2.0, youngs_modulus=MAX),
            "bending_stress",
        ),
        (
            bending_stress_from_radius,
            dict(outer_diameter=2.0, bend_radius=0.5, youngs_modulus=MAX),
            "bending_stress",
        ),
        (axial_stress, dict(axial_force=MAX, steel_area=0.5), "axial_stress"),
        (axial_stress, dict(axial_force=-MAX, steel_area=0.5), "axial_stress"),
        # Each component is MAX; their sum overflows.
        (
            smys_stress_check,
            dict(
                outer_diameter=2.0, bend_radius=1.0, youngs_modulus=MAX,
                smys=320.0, allowable_fraction=0.75, axial_force=MAX, steel_area=1.0,
            ),
            "longitudinal_stress",
        ),
        # f * SMYS rounds to zero; reject before dividing by that allowable.
        (
            smys_stress_check,
            dict(CHECK, smys=SMALLEST, allowable_fraction=0.5),
            "allowable_stress",
        ),
        # Unit stress divided by SMALLEST overflows utilisation.
        (
            smys_stress_check,
            dict(
                outer_diameter=2.0, bend_radius=1.0, youngs_modulus=1.0,
                smys=SMALLEST, allowable_fraction=1.0,
            ),
            "utilisation",
        ),
        # Unit stress / MIN is finite, but multiplying by 100 overflows.
        (
            smys_stress_check,
            dict(
                outer_diameter=2.0, bend_radius=1.0, youngs_modulus=1.0,
                smys=MIN, allowable_fraction=1.0,
            ),
            "percent_smys",
        ),
        (
            allowable_bend_radius,
            dict(outer_diameter=4.0, youngs_modulus=MAX,
                 smys=320.0, allowable_fraction=0.75),
            "bend_radius",
        ),
        (
            allowable_bend_radius,
            dict(outer_diameter=2.0, youngs_modulus=MAX,
                 smys=0.5, allowable_fraction=1.0),
            "bend_radius",
        ),
        (
            allowable_bend_radius,
            dict(RADIUS, smys=SMALLEST, allowable_fraction=0.5),
            "allowable_stress",
        ),
    ],
)
def test_stress_helpers_reject_invalid_derived_results(helper, arguments, quantity):
    with pytest.raises(ValueError, match=quantity):
        helper(**arguments)


def test_pure_bending_radius_hand_inverse():
    # Independent hand calculation is given in the module docstring.
    limiting_radius = 200.0
    allowable_stress = 240.0
    assert allowable_bend_radius(**RADIUS) == pytest.approx(limiting_radius)
    assert bending_stress_from_radius(
        **dict(BENDING, bend_radius=limiting_radius)
    ) == pytest.approx(allowable_stress)
    result = smys_stress_check(**dict(CHECK, bend_radius=limiting_radius))
    assert result.longitudinal_stress == pytest.approx(allowable_stress)
    assert result.allowable_stress == pytest.approx(allowable_stress)
    assert result.utilisation == pytest.approx(1.0)
    assert result.passes is True


@pytest.mark.parametrize("steel_area", [None, 0.0, -0.04, 0.04])
def test_zero_axial_force_preserves_optional_area(steel_area):
    # Zero force omits the axial term; the hand-derived bending stress is 240.
    result = smys_stress_check(**CHECK, steel_area=steel_area)
    assert result == smys_stress_check(**CHECK)
    assert result.longitudinal_stress == pytest.approx(240.0)


@pytest.mark.parametrize(
    "force,expected_stress,expected_utilisation,expected_percent,passes",
    [(12.0, 540.0, 2.25, 168.75, False), (-12.0, -60.0, -0.25, -18.75, True)],
)
def test_signed_axial_force_is_preserved(
    force, expected_stress, expected_utilisation, expected_percent, passes
):
    # N/A = +/-12 / 0.04 = +/-300; total = 240 +/- 300 = 540 or -60.
    # Divide by allowable 240 for utilisation, or by SMYS 320 and multiply
    # by 100 for percent SMYS: 2.25/-0.25 and 168.75/-18.75 respectively.
    assert axial_stress(force, AXIAL["steel_area"]) == pytest.approx(
        expected_stress - 240.0
    )
    result = smys_stress_check(**CHECK, axial_force=force, steel_area=AXIAL["steel_area"])
    assert result.longitudinal_stress == pytest.approx(expected_stress)
    assert result.utilisation == pytest.approx(expected_utilisation)
    assert result.percent_smys == pytest.approx(expected_percent)
    assert result.passes is passes
