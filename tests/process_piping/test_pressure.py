"""Independent SI hand checks for the process piping pressure boundary."""

from dataclasses import replace
import math

import pytest

from digitalmodel.process_piping.pressure import (
    PressureDesign,
    pressure_thickness,
    hydrotest_pressure,
)


@pytest.fixture
def design():
    return PressureDesign(
        pressure_pa=4e6,
        outside_diameter_m=0.168275,
        allowable_stress_pa=138e6,
        quality_factor=1,
        weld_strength_factor=1,
        y=0.4,
        corrosion_allowance_m=0.003,
        mechanical_allowance_m=0,
        mill_tolerance_fraction=0.125,
    )


def test_nps6_thickness(design):
    # NPS 6 OD=6.625 in. t=4e6*.168275/[2*(138e6+4e6*.4)]
    # =.002410816619 m; ordered min=(t+.003)/.875=.006183790421 m.
    result = pressure_thickness(design)
    assert result.pressure_thickness_m == pytest.approx(0.002410816619)
    assert result.minimum_nominal_thickness_m == pytest.approx(0.006183790421)
    assert result.citations[0].section.startswith("304")


def test_factors_and_units(design):
    base = pressure_thickness(design)
    assert (
        pressure_thickness(replace(design, quality_factor=0.85)).pressure_thickness_m
        > base.pressure_thickness_m
    )
    assert (
        pressure_thickness(
            replace(design, weld_strength_factor=0.7)
        ).pressure_thickness_m
        > base.pressure_thickness_m
    )
    assert (
        pressure_thickness(
            replace(design, pressure_pa=4, allowable_stress_pa=138)
        ).minimum_nominal_thickness_m
        == base.minimum_nominal_thickness_m
    )


@pytest.mark.parametrize(
    "field,value",
    [
        ("pressure_pa", -1),
        ("pressure_pa", math.nan),
        ("outside_diameter_m", 0),
        ("allowable_stress_pa", math.inf),
        ("quality_factor", 0),
        ("quality_factor", 1.1),
        ("weld_strength_factor", 0),
        ("y", -0.1),
        ("corrosion_allowance_m", -1),
        ("mill_tolerance_fraction", 1),
    ],
)
def test_invalid_design(design, field, value):
    with pytest.raises(ValueError):
        pressure_thickness(replace(design, **{field: value}))


def test_outside_equation_scope(design):
    with pytest.raises(ValueError, match="304.1.2"):
        pressure_thickness(replace(design, pressure_pa=60e6))
    with pytest.raises(ValueError):
        pressure_thickness(replace(design, corrosion_allowance_m=0.1))


def hydro(**changes):
    values = dict(
        design_pressure_pa=4e6,
        design_allowable_pa=138e6,
        test_allowable_pa=150e6,
        design_temperature_k=423.15,
        test_temperature_k=293.15,
        outside_diameter_m=0.168275,
        minimum_test_wall_m=0.006223,
        test_yield_pa=240e6,
        other_longitudinal_stress_pa=0,
        component_test_limit_pa=10e6,
    )
    return hydrotest_pressure(**(values | changes))


def test_hydro_temperature_and_floor():
    # 1.5*4*(150/138)=6.521739130 MPa, below hoop and axial yield caps.
    result = hydro()
    assert result.required_pressure_pa == pytest.approx(6.521739130e6)
    assert result.target_pressure_pa == result.required_pressure_pa
    assert result.component_limit_satisfied
    assert hydro(test_allowable_pa=100e6).required_pressure_pa == 6e6
    assert hydro(design_temperature_k=290).required_pressure_pa == 6e6
    # The 2024 basis does not impose the historic 6.5 ratio cap.
    assert hydro(design_allowable_pa=10e6).required_pressure_pa == 90e6


def test_hydro_yield_reduction_and_component_limit():
    result = hydro(test_yield_pa=60e6)
    assert result.reduced_for_yield
    assert result.target_pressure_pa == result.yield_pressure_limit_pa
    # Lame inner-wall hoop: P*(ro²+ri²)/(ro²-ri²) <= Sy.
    ro, ri = 0.168275 / 2, 0.168275 / 2 - 0.006223
    assert result.target_pressure_pa * (ro**2 + ri**2) / (
        ro**2 - ri**2
    ) == pytest.approx(60e6)
    axial = hydro(other_longitudinal_stress_pa=239e6)
    assert axial.yield_pressure_limit_pa < result.yield_pressure_limit_pa
    assert not hydro(component_test_limit_pa=5e6).component_limit_satisfied


@pytest.mark.parametrize(
    "kwargs",
    [
        dict(test_yield_pa=math.nan),
        dict(minimum_test_wall_m=0),
        dict(test_temperature_k=0),
        dict(other_longitudinal_stress_pa=240e6),
        dict(minimum_test_wall_m=0.05),
        dict(design_pressure_pa=-1),
        dict(other_longitudinal_stress_pa=True),
    ],
)
def test_invalid_hydro(kwargs):
    with pytest.raises(ValueError):
        hydro(**kwargs)
