# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""ITTC 7.5-04-01-01.1 (2024 rev. 08), G.1, G-1 and section 6.3."""

import math
from dataclasses import replace

import pytest

from digitalmodel.hydrodynamics.added_resistance import (
    OperatingPoint,
    RegularWaves,
    Ship,
    Status,
)
from digitalmodel.hydrodynamics.added_resistance.reflection import (
    ReflectionEvidence,
    stawave1,
    stawave1_equivalent_regular_table,
)


def hull():
    return Ship(100, 20, 80, 6, 1000, 10)


def evidence():
    return ReflectionEvidence(2, 0.1, "measured", True)


def test_closed_form_hand_calculation():
    """G-1: R=(1/16)*rho*g*Hs²*B*sqrt(B/Lbow).

    Chosen rho=1000, g=10, Hs=2, B=20, Lbow=80:
    sqrt(20/80)=1/2; R=(1000*10*4*20/2)/16=25000 N.
    No calibrated ship data or published worked example is used.
    """
    result = stawave1(hull(), OperatingPoint(5, 0), evidence())
    assert result.status == Status.EVALUATED
    assert result.resistance_n == pytest.approx(25000)


def test_equivalent_regular_response_and_normalization():
    """Since Hs²=16*m0 and mean R=2*K*m0, G-1 gives K=8*C.

    C=6250 N/m² from the hand calculation above, so K=50000 N/m².
    At a=[1,2] m, R=[50000,200000] N; rho*g*B²/L=40000 N/m²;
    both dimensionless transfer values equal 1.25.
    omega at lambda=2*pi*10 is 1 rad/s; encounter=1+(1/10)*5=1.5.
    """
    rows = stawave1_equivalent_regular_table(
        hull(),
        OperatingPoint(5, 0),
        RegularWaves([20 * math.pi, 40 * math.pi], [1, 2]),
        evidence(),
    )
    assert [r.resistance_n for r in rows] == pytest.approx([50000, 200000])
    assert [r.transfer for r in rows] == pytest.approx([1.25, 1.25])
    assert rows[0].omega_rad_s == pytest.approx(1)
    assert rows[0].encounter_rad_s == pytest.approx(1.5)
    assert all("equivalent" in r.method for r in rows)


@pytest.mark.parametrize("heading", [-45, 0, 45])
def test_bow_sector_inclusive(heading):
    assert (
        stawave1(hull(), OperatingPoint(5, heading), evidence()).status
        == Status.EVALUATED
    )


@pytest.mark.parametrize("heading", [-45.001, 45.001, 90, 180])
def test_outside_sector_is_unevaluated_not_zero(heading):
    rows = stawave1_equivalent_regular_table(
        hull(), OperatingPoint(5, heading), RegularWaves([50], [1]), evidence()
    )
    assert rows[0].status == Status.NOT_EVALUATED
    assert rows[0].resistance_n is None and rows[0].transfer is None
    assert "45" in rows[0].reason


@pytest.mark.parametrize("acceleration", [0.5, 0.6])
def test_acceleration_limit_is_strict(acceleration):
    """G.1 requires bow acceleration <0.05*g; here g=10, limit=0.5."""
    result = stawave1(
        hull(),
        OperatingPoint(5, 0),
        replace(evidence(), bow_acceleration_m_s2=acceleration),
    )
    assert result.status == Status.NOT_EVALUATED
    assert "acceleration" in result.reason


@pytest.mark.parametrize("basis,limit", [("measured", 2.25), ("observed", 1.5)])
def test_height_gate_at_and_above_limit(basis, limit):
    """Section 6.3: Hs <= factor*sqrt(Lpp/100); Lpp=100 m."""
    at = replace(evidence(), hs_m=limit, height_basis=basis)
    above = replace(at, hs_m=limit + 0.001)
    assert stawave1(hull(), OperatingPoint(5, 0), at).status == Status.EVALUATED
    assert stawave1(hull(), OperatingPoint(5, 0), above).status == Status.NOT_EVALUATED


def test_unconfirmed_short_waves_not_evaluated():
    result = stawave1(
        hull(), OperatingPoint(5, 0), replace(evidence(), short_waves_confirmed=False)
    )
    assert result.status == Status.NOT_EVALUATED
    assert "short" in result.reason


def test_calm_sea_zero_resistance():
    result = stawave1(hull(), OperatingPoint(5, 0), replace(evidence(), hs_m=0))
    assert result.resistance_n == 0


@pytest.mark.parametrize(
    "field,value",
    [
        ("hs_m", -1),
        ("hs_m", float("nan")),
        ("bow_acceleration_m_s2", float("inf")),
        ("height_basis", "unknown"),
        ("short_waves_confirmed", "yes"),
    ],
)
def test_invalid_evidence(field, value):
    with pytest.raises(ValueError):
        replace(evidence(), **{field: value})
