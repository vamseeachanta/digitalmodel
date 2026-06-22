"""Invariant tests for the DNV 2.7-1 offshore-container screening model."""

import math

import pytest

from digitalmodel.structural.offshore_container.dnv_2_7_1 import (
    SHS,
    DesignFactors,
    allowable_stress,
    sling_angle,
    sling_forces,
    utilization,
)


def test_shs_section_props():
    sec = SHS.from_mm(150, 10)
    bi = 0.130
    assert sec.area == pytest.approx(0.150 ** 2 - bi ** 2)
    assert sec.modulus == pytest.approx(sec.inertia / 0.075)


def test_sling_angle_geometry():
    # square plan, lift height equal to half-diagonal -> 45 degrees
    half_diag = 0.5 * math.hypot(4.0, 4.0)
    beta = sling_angle(4.0, 4.0, half_diag)
    assert math.degrees(beta) == pytest.approx(45.0)


def test_skew_doubles_load_vs_four_way():
    f = DesignFactors()
    beta = math.radians(60)
    skew = sling_forces(10_000, beta, f, skew=True)
    even = sling_forces(10_000, beta, f, skew=False)
    assert skew.axial_per_sling == pytest.approx(2 * even.axial_per_sling)


def test_allowable_stress():
    f = DesignFactors(gamma_m=1.15, eta=0.85)
    assert allowable_stress(355e6, f) == pytest.approx(355e6 * 0.85 / 1.15)


def test_util_increases_with_rating():
    sec = SHS.from_mm(150, 10)
    low = utilization(6.0, 2.5, 2.5, 10_000, sec).governing
    high = utilization(6.0, 2.5, 2.5, 30_000, sec).governing
    assert high > low


def test_steeper_sling_reduces_top_rail():
    sec = SHS.from_mm(150, 10)
    half_diag = 0.5 * math.hypot(6.0, 2.5)
    shallow = utilization(6.0, 2.5, half_diag * math.tan(math.radians(30)), 25_000, sec)
    steep = utilization(6.0, 2.5, half_diag * math.tan(math.radians(70)), 25_000, sec)
    # steeper sling -> smaller inward horizontal component -> less top-rail load
    assert steep.top_rail < shallow.top_rail


def test_bigger_section_reduces_utilization():
    small = utilization(6.0, 2.5, 2.5, 25_000, SHS.from_mm(120, 8)).governing
    big = utilization(6.0, 2.5, 2.5, 25_000, SHS.from_mm(200, 10)).governing
    assert big < small
