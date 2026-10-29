# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise existing refusals; missing guards are listed in rod-pump-hand-checks.md.

Inputs are synthetic boundary probes, not physical constants. No production
validation policy is added or inferred from these characterization tests.
"""

import math

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump import (
    analyse_rod_string,
    divergence_onset,
    fluid_load,
    natural_frequency,
    plunger_stroke,
    pump_displacement,
    rod_area,
    rod_elastic_constant,
    spring_rate,
    taper_adjusted_natural_frequency,
    time_at_position,
    time_from_card_position,
    undulations_per_half_stroke,
)


@pytest.mark.parametrize("diameter", [0.0, -1.0, -math.inf])
@pytest.mark.parametrize(
    "evaluate",
    [
        pytest.param(rod_area, id="area"),
        pytest.param(
            lambda d: rod_elastic_constant(d, modulus_psi=12_000_000), id="elasticity"
        ),
        pytest.param(
            lambda d: spring_rate(d, 1000, modulus_psi=12_000_000), id="stiffness"
        ),
        pytest.param(lambda d: fluid_load(d, 1000, 1), id="fluid-load"),
        pytest.param(lambda d: pump_displacement(d, 100, 10), id="displacement"),
        pytest.param(
            lambda d: analyse_rod_string(
                d, 1000, 100, 1, 0, 0, weight_lb_per_ft=1, modulus_psi=12_000_000
            ),
            id="single-string-analysis",
        ),
    ],
)
def test_nonpositive_diameter_refused(diameter, evaluate):
    with pytest.raises(ValueError, match="diameter must be positive"):
        evaluate(diameter)


@pytest.mark.parametrize("length", [0.0, -1.0, -math.inf])
def test_nonpositive_rod_length_refused(length):
    with pytest.raises(ValueError, match="length must be positive"):
        spring_rate(1.0, length, modulus_psi=12_000_000)
    with pytest.raises(ValueError, match="length must be positive"):
        natural_frequency(length)


@pytest.mark.parametrize(
    "surface,load,rate",
    [
        (0.0, 0.0, 100.0),
        (-1.0, 0.0, 100.0),
        (10.0, 1000.0, 100.0),
        (9.0, 1000.0, 100.0),
    ],
)
def test_nonpositive_resulting_plunger_stroke_refused(surface, load, rate):
    """At F=1000 lb and k=100 lb/in, 10 in stretch consumes S<=10 in."""
    with pytest.raises(ValueError, match="not positive"):
        plunger_stroke(surface, load, rate)


@pytest.mark.parametrize("speed", [0.0, -1.0, -math.inf])
def test_nonpositive_undulation_speed_refused(speed):
    with pytest.raises(ValueError, match="SPM must be positive"):
        undulations_per_half_stroke(speed, 120.0)


@pytest.mark.parametrize("factor", [0.0, -1.0, -math.inf])
def test_nonpositive_taper_factor_refused(factor):
    with pytest.raises(ValueError, match="taper factor must be positive"):
        taper_adjusted_natural_frequency(60.0, factor)


@pytest.mark.parametrize("position", [-1.0, 21.0, math.nan, math.inf, -math.inf])
def test_outside_card_position_refused(position):
    with pytest.raises(ValueError, match="outside"):
        time_at_position(position, 20.0, 60.0)
    with pytest.raises(ValueError, match="outside"):
        time_from_card_position(position, 20.0, 60.0, position_uncertainty_in=1.0)


def test_empty_speed_list_refused():
    with pytest.raises(ValueError, match="at least one"):
        divergence_onset([])


def test_nonstandard_rod_requires_explicit_weight():
    """Synthetic 0.9 in diameter cannot silently acquire a tabulated weight."""
    with pytest.raises(ValueError, match="weight_lb_per_ft explicitly"):
        analyse_rod_string(0.9, 1000.0, 100.0, 1.0, 0.0, 1.0, modulus_psi=12_000_000)


def test_inches_family_metres_supplied_as_inches_refused_by_weight_lookup():
    """One inch is 0.0254 m (NIST SP 811 appendix B).

    Passing 0.0254 as diameter_in fails the standard-size lookup. This is an
    incidental refusal without an explicit rod weight; it is not a unit
    detector. The other suffix families have no magnitude-based refusal;
    their probes and outcomes are listed as gaps in the documentation.
    """
    with pytest.raises(ValueError, match="not a standard API size"):
        analyse_rod_string(0.0254, 1000.0, 100.0, 1.0, 0.0, 1.0, modulus_psi=12_000_000)
