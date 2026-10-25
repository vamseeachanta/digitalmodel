# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Axial curve checks with independent arithmetic and negative controls.

The load-transfer equation is publicly reproduced by OpenPile tz_curves,
kraft_modification (1981 formulation). Normalized tables below are invented
arithmetic examples, not API tables. Standard-table conformance is NOT_EVALUATED.
"""

import math

import pytest

from digitalmodel.geotechnical.py_curves import (
    CyclicDegradation,
    clay_qz,
    clay_tz,
    clay_tz_kraft,
)


# Assumed normalized arithmetic controls: (slip / D, resistance / ultimate).
TABLE = ((0, 0), (0.005, 0.5), (0.01, 1))


def tz(**kwargs):
    values = dict(ultimate_kpa=40, diameter_m=2, normalized_points=TABLE,
                  residual_ratio=0.6, residual_displacement_ratio=0.03)
    return clay_tz(**(values | kwargs))


def kraft(**kwargs):
    values = dict(ultimate_kpa=40, diameter_m=2, shear_modulus_kpa=1000,
                  load_ratios=(0, 0.5, 1), influence_radius_ratio=5,
                  curve_factor=0.5, residual_ratio=0.6, residual_slip_m=0.02)
    return clay_tz_kraft(**(values | kwargs))


def test_tz_assumed_table_and_residual():
    """By multiplication: z=D*r gives 0,.01,.02,.06 m, t=0,20,40,24 kPa."""
    result = tz()
    assert result.displacements_m == (0, 0.01, 0.02, 0.06)
    assert result.resistances == (0, 20, 40, 24)
    assert result.resistance_at(0.04) == pytest.approx(32)
    assert result.resistance_at(1) == 24
    assert result.resistance_at(-0.01) == -20
    assert result.resistance_unit == "kPa"


def test_qz_assumed_table_is_compression_only():
    """q_ult=200 kPa and D=2 multiply the assumed normalized table."""
    result = clay_qz(ultimate_kpa=200, diameter_m=2, normalized_points=TABLE)
    assert result.resistances == (0, 100, 200)
    assert result.resistance_at(0.005) == 50
    assert result.resistance_at(-0.005) == 0
    assert result.resistance_at(1) == 200


def test_kraft_independent_equation():
    """Public 1981 equation: z=t D/(2G)*ln((R-Rf*t/tmax)/(1-Rf*t/tmax)).

    Assumed D=2, G=1000, R=5, Rf=.5, tmax=40:
    t=20 -> z=.02 ln(19/3); t=40 -> z=.04 ln(9).
    These expectations are direct substitution, independent of generator logic.
    """
    result = kraft()
    assert result.displacements_m[1] == pytest.approx(0.02 * math.log(19 / 3))
    assert result.displacements_m[2] == pytest.approx(0.04 * math.log(9))
    assert result.displacements_m[3] == pytest.approx(0.04 * math.log(9) + 0.02)
    assert result.resistances == (0, 20, 40, 24)


@pytest.mark.parametrize("generator", [tz, kraft])
def test_axial_strength_and_modulus_degradation(generator):
    """N=100, bs=.5 and bk=1: t scales .1 and z scales .1/.01=10."""
    base = generator()
    degraded = generator(degradation=CyclicDegradation(100, 0.5, 1))
    assert degraded.resistances == pytest.approx([v * 0.1 for v in base.resistances])
    assert degraded.displacements_m == pytest.approx([v * 10 for v in base.displacements_m])


def test_qz_degradation():
    """Independent amplitude/abscissa scaling at homologous points."""
    result = clay_qz(ultimate_kpa=200, diameter_m=2, normalized_points=TABLE,
                     degradation=CyclicDegradation(100, 0.5, 1))
    assert result.resistances == pytest.approx((0, 10, 20))
    assert result.displacements_m == pytest.approx((0, 0.1, 0.2))


@pytest.mark.parametrize("points", [(), ((0, 0),), ((0.1, 0), (1, 1)),
                                      ((0, 0), (1, 0.5)),
                                      ((0, 0), (1, 1), (2, 0.5)),
                                      ((0, 0), (1, 1.1)),
                                      ((0, 0), (math.nan, 1)),
                                      ((0, 0), (1, math.inf))])
def test_reject_invalid_normalized_tables(points):
    with pytest.raises(ValueError):
        tz(normalized_points=points)
    with pytest.raises(ValueError):
        clay_qz(ultimate_kpa=100, diameter_m=1, normalized_points=points)


@pytest.mark.parametrize("kwargs", [dict(residual_ratio=-0.1), dict(residual_ratio=1.1),
                                     dict(residual_displacement_ratio=0.005),
                                     dict(ultimate_kpa=0), dict(diameter_m=0)])
def test_reject_invalid_tz_parameters(kwargs):
    with pytest.raises(ValueError):
        tz(**kwargs)


@pytest.mark.parametrize("kwargs", [dict(shear_modulus_kpa=0),
                                     dict(influence_radius_ratio=1),
                                     dict(curve_factor=1), dict(curve_factor=-1),
                                     dict(residual_slip_m=0),
                                     dict(load_ratios=(0, 1, 0.5)),
                                     dict(load_ratios=(0, 0.5)),
                                     dict(residual_ratio=1.1)])
def test_reject_invalid_kraft_parameters(kwargs):
    with pytest.raises(ValueError):
        kraft(**kwargs)
