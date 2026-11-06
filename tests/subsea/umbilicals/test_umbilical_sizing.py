# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Sizing hand calculations; all dimensions and properties are synthetic inputs.

Basis: OpenStax, University Physics Volume 1 (2016), sections 14.1 and 14.4.
No material property, rating or acceptance factor below is a standard value.
"""

import math
from dataclasses import replace

import pytest

from digitalmodel.subsea.umbilicals import (
    FunctionalElement,
    QualifiedLimit,
    RadialBuildLayer,
    UmbilicalSection,
    check_pressure,
    size_umbilical,
)


def element(**kwargs):
    """Return a synthetic straight tube, with inputs chosen for hand arithmetic."""
    values = dict(
        name="service",
        kind="steel_tube",
        count=2,
        outer_diameter=0.02,
        inner_diameter=0.01,
        wall_density=8000.0,
        contents_density=1000.0,
    )
    values.update(kwargs)
    return FunctionalElement(**values)


def section():
    """The illustrative section used in the documented worksheet."""
    return UmbilicalSection(
        elements=(element(),),
        core_diameter=0.04,
        filler_density=1000.0,
        armour=(RadialBuildLayer(0.005, 8000.0, 0.5),),
        sheath=RadialBuildLayer(0.005, 1000.0, 1.0),
    )


@pytest.mark.parametrize("kind", ["steel_tube", "thermoplastic_hose"])
def test_cross_section_hand_calculation(kind):
    """A=pi*D²/4, mass=rho*A; OpenStax (2016) 14.1, 14.4.

    Core area=.0004*pi; two envelopes=.0002*pi, filler=.0002*pi.
    Walls contribute 1.2*pi kg/m, contents .05*pi, filler .2*pi,
    armour .9*pi (half metal), sheath .275*pi: total 2.625*pi.
    OD=.04+2*.005+2*.005=.06 m; displaced mass=.9*pi kg/m.
    Supplied g=10 gives air weight=26.25*pi and wet weight=17.25*pi.
    """
    model = replace(section(), elements=(element(kind=kind),))
    result = size_umbilical(model, water_density=1000.0, gravity=10.0)
    assert result.outer_diameter == pytest.approx(0.06)
    assert result.functional_element_count == 2
    assert result.filler_area == pytest.approx(0.0002 * math.pi)
    assert result.mass_per_length == pytest.approx(2.625 * math.pi)
    assert result.weight_in_air == pytest.approx(26.25 * math.pi)
    assert result.weight_in_water == pytest.approx(17.25 * math.pi)
    assert result.packing.status == "NOT_EVALUATED"
    assert result.standard_qualification.status == "NOT_EVALUATED"


def test_pressure_differentials_and_limits():
    """p=rho*g*h=1000*10*100=1 MPa (OpenStax 2016, 14.1).

    Steel: internal 3 MPa gives 2 MPa burst differential, at its limit.
    Hose: internal 0 gives 1 MPa collapse differential / .5 MPa = 2.
    Opposite-sign loads clamp to zero; limits are synthetic supplied inputs.
    """
    steel = element(
        burst_limit=QualifiedLimit(2e6, "Synthetic hand worksheet"),
        collapse_limit=QualifiedLimit(1e6, "Synthetic hand worksheet"),
    )
    hose = element(
        name="hose",
        kind="thermoplastic_hose",
        burst_limit=QualifiedLimit(1e6, "Synthetic hand worksheet"),
        collapse_limit=QualifiedLimit(0.5e6, "Synthetic hand worksheet"),
    )
    result = check_pressure(
        (steel, hose),
        {"service": 3e6, "hose": 0.0},
        water_depth=100.0,
        water_density=1000.0,
        gravity=10.0,
    )
    assert result.external_pressure == 1e6
    assert result.elements[0].burst.status == "PASS"
    assert result.elements[0].burst.utilisation == 1.0
    assert result.elements[0].collapse.demand == 0.0
    assert result.elements[1].burst.demand == 0.0
    assert result.elements[1].collapse.status == "FAIL"
    assert result.elements[1].collapse.utilisation == 2.0
    assert result.status == "FAIL"


def test_missing_ratings_cannot_pass_even_at_zero_depth():
    """Absent evidence remains unevaluated for either construction type."""
    result = check_pressure(
        (element(),),
        {"service": 0.0},
        water_depth=0.0,
        water_density=1000.0,
        gravity=10.0,
    )
    assert result.elements[0].burst.status == "NOT_EVALUATED"
    assert result.elements[0].collapse.status == "NOT_EVALUATED"
    assert result.status == "NOT_EVALUATED"


def test_positive_buoyancy_retains_signed_weight():
    """Density 1 vs supplied water density 1000 gives upward net weight."""
    model = replace(
        section(),
        filler_density=1.0,
        armour=(),
        elements=(element(wall_density=1, contents_density=0),),
        sheath=RadialBuildLayer(0.005, 1.0, 1.0),
    )
    assert size_umbilical(model, water_density=1000, gravity=10).weight_in_water < 0


@pytest.mark.parametrize(
    "changes",
    [
        {"count": 0},
        {"count": True},
        {"count": 1.5},
        {"name": " "},
        {"kind": "unknown"},
        {"outer_diameter": math.nan},
        {"inner_diameter": 0.02},
        {"inner_diameter": -1.0},
        {"wall_density": math.inf},
        {"contents_density": -1.0},
    ],
)
def test_invalid_functional_elements(changes):
    with pytest.raises(ValueError):
        element(**changes)


@pytest.mark.parametrize(
    "changes",
    [
        {"elements": ()},
        {"elements": (element(), element())},
        {"core_diameter": 0.01},
        {"core_diameter": math.nan},
        {"filler_density": -1.0},
    ],
)
def test_invalid_cross_sections(changes):
    with pytest.raises(ValueError):
        size_umbilical(replace(section(), **changes), water_density=1000, gravity=10)


@pytest.mark.parametrize(
    "args", [(0, 1, 1), (1, -1, 1), (1, 1, 0), (1, 1, 1.1), (1, 1, math.nan)]
)
def test_invalid_build_layer(args):
    with pytest.raises(ValueError):
        RadialBuildLayer(*args)


@pytest.mark.parametrize("value,basis", [(0, "x"), (math.inf, "x"), (1, " ")])
def test_limits_require_positive_value_and_basis(value, basis):
    with pytest.raises(ValueError):
        QualifiedLimit(value, basis)


@pytest.mark.parametrize(
    "changes",
    [
        {"internal_pressures": {}},
        {"internal_pressures": {"service": math.nan}},
        {"water_depth": -1},
        {"water_density": 0},
        {"gravity": math.inf},
    ],
)
def test_invalid_pressure_inputs(changes):
    values = dict(
        internal_pressures={"service": 1.0},
        water_depth=0,
        water_density=1000,
        gravity=10,
    )
    values.update(changes)
    with pytest.raises(ValueError):
        check_pressure((element(),), **values)
