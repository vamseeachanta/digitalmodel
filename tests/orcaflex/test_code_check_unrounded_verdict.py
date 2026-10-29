"""Independent synthetic arithmetic for verdicts before display rounding.

All geometry, strengths, loads and supplied factors below are synthetic test
choices, not standard recommendations. Unit conversion uses 1000 N per kN.
The boundary offsets of 0.00002 straddle unity but share a four-decimal display;
0.123456 checks that a non-boundary display still rounds to 0.1235.
"""

import math

import numpy as np
import pytest

from digitalmodel.orcaflex.code_check_engine import (
    APIRP2RDInput,
    DNVOSF201Input,
    SafetyClass,
    check_api_rp_2rd,
    check_dnv_os_f201,
)


@pytest.mark.parametrize(
    "target_utilisation, expected_pass, expected_display",
    [
        pytest.param(1.0 - 0.00002, True, 1.0, id="below-limit"),
        pytest.param(1.0 + 0.00002, False, 1.0, id="above-limit"),
        pytest.param(0.123456, True, 0.1235, id="display-precision"),
    ],
)
def test_api_verdict_uses_unrounded_utilization(
    target_utilisation, expected_pass, expected_display
):
    """Hand calculation: OD=2 m, wall=0.5 m, ID=1 m gives A=3*pi/4 m².

    With SMYS=1000 Pa and derating=1, Ty=3*pi/4 kN. For design factor
    0.5 and zero bending, u=abs(T)/(3*pi/8); T=(3*pi/8)*target gives u=target.
    SMTS=2000 Pa is supplied explicitly and does not enter this criterion.
    """
    pipe = APIRP2RDInput(
        outer_diameter=2.0,
        wall_thickness=0.5,
        smys=1000.0,
        smts=2000.0,
        temperature_derating=1.0,
        design_factor=0.5,
    )
    tension_kN = (3.0 * math.pi / 8.0) * target_utilisation

    result, = check_api_rp_2rd(
        pipe, np.array([0.0]), np.array([tension_kN]), np.array([0.0])
    )

    assert math.isfinite(result.utilisation)
    assert result.utilisation == expected_display
    assert result.pass_ is expected_pass
    assert result.model_dump(by_alias=True)["pass"] is expected_pass


@pytest.mark.parametrize(
    "target_utilisation, expected_pass, expected_display",
    [
        pytest.param(1.0 - 0.00002, True, 1.0, id="below-limit"),
        pytest.param(1.0 + 0.00002, False, 1.0, id="above-limit"),
        pytest.param(0.123456, True, 0.1235, id="display-precision"),
    ],
)
def test_dnv_verdict_uses_unrounded_utilization(
    target_utilisation, expected_pass, expected_display
):
    """Exercise the existing dimensionless contract, without qualifying factors.

    The implemented comparison is u=gamma_SC*gamma_m*((M/Mp)^2+(T/Sp)^2)
    <=1, with the squared sum reported directly (no square root). Existing
    DNVOSF201Input definitions use t2=7*wall/8, fy=min(SMYS,SMTS/1.15)*fab
    and NORMAL gamma_SC=1.14. These are existing implementation constants.

    Synthetic OD=2 m and wall=0.5 m give t2=7/16 m and resistance ID=9/8 m.
    Supplied SMYS=1000 Pa, SMTS=2300 Pa and fab=0.8 give fy=800 Pa.
    Thus Mp=(800/1000)*(2^3-(9/8)^3)/6=3367/3840 kN.m independently.
    Supplied gamma_m=1.25 gives gamma_SC*gamma_m=57/40; condition_factor=1.
    With T=0, apply M=(3367/3840)*sqrt((40/57)*target), so u=target.
    """
    pipe = DNVOSF201Input(
        outer_diameter=2.0,
        wall_thickness=0.5,
        smys=1000.0,
        smts=2300.0,
        safety_class=SafetyClass.NORMAL,
        fab_factor=0.8,
        material_factor=1.25,
        condition_factor=1.0,
    )
    moment_kNm = (3367.0 / 3840.0) * math.sqrt(
        (40.0 / 57.0) * target_utilisation
    )

    result, = check_dnv_os_f201(
        pipe, np.array([0.0]), np.array([0.0]), np.array([moment_kNm])
    )

    assert math.isfinite(result.utilisation)
    assert result.utilisation == expected_display
    assert result.pass_ is expected_pass
    assert result.model_dump(by_alias=True)["pass"] is expected_pass


def test_api_exact_limit_is_inclusive():
    """Synthetic OD=2, ID=1, SMYS=6000, derating=1 give Mp=7 kN.m.

    SMTS=12000 Pa is explicit. At M=3.5 kN.m, T=0 and design factor=0.5,
    u=sqrt((3.5/7)^2)/0.5=1 exactly in binary floating-point arithmetic.
    """
    moment_capacity_kNm = 6000.0 * ((2.0**3 - 1.0**3) / 6.0) / 1000.0
    assert moment_capacity_kNm == 7.0
    assert math.sqrt((3.5 / moment_capacity_kNm) ** 2) / 0.5 == 1.0
    pipe = APIRP2RDInput(
        outer_diameter=2.0,
        wall_thickness=0.5,
        smys=6000.0,
        smts=12000.0,
        temperature_derating=1.0,
        design_factor=0.5,
    )

    result, = check_api_rp_2rd(
        pipe, np.array([0.0]), np.array([0.0]), np.array([3.5])
    )

    assert result.utilisation == 1.0
    assert result.pass_ is True


def test_dnv_exact_limit_is_inclusive():
    """Use the same synthetic geometry and existing contract as the DNV probes.

    SMYS=4800000 Pa, SMTS=11040000 Pa and fab=0.8 give fy=3840000 Pa.
    Thus Mp=(3840000/1000)*(2^3-(9/8)^3)/6=4208.75 kN.m. The synthetic
    supplied gamma_m=4/1.14 makes gamma_SC*gamma_m evaluate to exactly 4.
    At T=0 and M=2104.375 kN.m, u=4*(2104.375/4208.75)^2=1 exactly.
    """
    moment_capacity_kNm = 3840000.0 * ((2.0**3 - (9.0 / 8.0)**3) / 6.0) / 1000.0
    material_factor = 4.0 / 1.14
    assert moment_capacity_kNm == 4208.75
    assert 1.14 * material_factor * (2104.375 / moment_capacity_kNm) ** 2 == 1.0
    pipe = DNVOSF201Input(
        outer_diameter=2.0,
        wall_thickness=0.5,
        smys=4800000.0,
        smts=11040000.0,
        safety_class=SafetyClass.NORMAL,
        fab_factor=0.8,
        material_factor=material_factor,
        condition_factor=1.0,
    )

    result, = check_dnv_os_f201(
        pipe, np.array([0.0]), np.array([0.0]), np.array([2104.375])
    )

    assert result.utilisation == 1.0
    assert result.pass_ is True
