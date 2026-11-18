# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent elementary limits for bonded isotropic concentric layers.

Basis: OpenStax Prealgebra 2e, section 9.5 (circle area), and University
Physics Volume 1, section 12.3 (stress = modulus * strain).
All numbers are synthetic arithmetic inputs, not material properties.
The relative tolerance 1e-12 permits binary64 rounding only. The extreme
scales exercise binary64 overflow and underflow, not engineering limits.
"""

import math

import pytest

from digitalmodel.subsea.cross_sections.calculations import (
    BondedAxialLayer,
    bonded_axial_stiffness,
)
from digitalmodel.subsea.cross_sections.schema import Provenance, RadialLayer, UnitValue


def layer(inner=0, outer=2, modulus=10, **overrides):
    """Build explicit synthetic geometry and a separately sourced modulus."""
    return BondedAxialLayer(
        **{
            "layer": RadialLayer(
                name="synthetic annulus",
                role="test",
                material="unspecified",
                inner_diameter=UnitValue(value=inner, unit="m"),
                outer_diameter=UnitValue(value=outer, unit="m"),
                provenance=Provenance(
                    source_id="synthetic-geometry", source_type="project_assumption"
                ),
            ),
            "youngs_modulus_pa": modulus,
            "modulus_unit": "Pa",
            "modulus_provenance": Provenance(
                source_id="synthetic-modulus", source_type="project_assumption"
            ),
            **overrides,
        }
    )


def test_bonded_layers_hand_stiffness():
    """Radii 1, 2, 3 give areas 3*pi, 5*pi and EA=(10*3+20*5)*pi.

    Summed material area is 8*pi; Eeq=130/8 Pa excludes the central bore.
    These expressions are hand-derived, independent of geometry utilities.
    """
    result = bonded_axial_stiffness([layer(2, 4, 10), layer(4, 6, 20)])
    assert result.area_m2 == pytest.approx(8 * math.pi, rel=1e-12)
    assert result.axial_stiffness_n == pytest.approx(130 * math.pi, rel=1e-12)
    assert result.equivalent_modulus_pa == pytest.approx(130 / 8, rel=1e-12)


def test_identical_modulus_limit():
    """Solid r=3: partition into r=1 disk and r=1..3 annulus; EA=10*9*pi."""
    result = bonded_axial_stiffness([layer(0, 2, 10), layer(2, 6, 10)])
    assert result.area_m2 == pytest.approx(9 * math.pi, rel=1e-12)
    assert result.axial_stiffness_n == pytest.approx(90 * math.pi, rel=1e-12)
    assert result.equivalent_modulus_pa == pytest.approx(10, rel=1e-12)


def test_single_layer_limit():
    """r=1 disk: EA=10*pi and Eeq=10, with no density input."""
    result = bonded_axial_stiffness([layer()])
    assert result.area_m2 == pytest.approx(math.pi, rel=1e-12)
    assert result.axial_stiffness_n == pytest.approx(10 * math.pi, rel=1e-12)
    assert result.equivalent_modulus_pa == pytest.approx(10, rel=1e-12)


def test_layer_order_invariance():
    """Reordering complete geometry/modulus pairs cannot affect the result."""
    from itertools import permutations

    layers = [layer(0, 2, 10), layer(2, 4, 20), layer(4, 6, 30)]
    reference = bonded_axial_stiffness(layers)
    assert all(bonded_axial_stiffness(list(order)) == reference for order in permutations(layers))


@pytest.mark.parametrize("inner", [1, 3, 2 + 1e-12, 2 - 1e-12])
def test_gap_or_overlap_is_rejected(inner):
    """Interfaces must match exactly; even a small gap/overlap is not bonded."""
    with pytest.raises(ValueError, match="contiguous"):
        bonded_axial_stiffness([layer(), layer(inner, 4)])


def test_duplicate_layer_is_overlap():
    """A repeated disk would count the same material twice."""
    with pytest.raises(ValueError, match="contiguous"):
        bonded_axial_stiffness([layer(), layer()])


def test_empty_stack_is_rejected():
    """No area means no equivalent modulus."""
    with pytest.raises(ValueError, match="at least one"):
        bonded_axial_stiffness([])


@pytest.mark.parametrize("modulus", [0, -10, math.inf, -math.inf, math.nan, None])
def test_invalid_modulus_is_rejected(modulus):
    """Only finite, positive, explicitly supplied moduli are accepted."""
    with pytest.raises(ValueError):
        layer(modulus=modulus)


@pytest.mark.parametrize("field", ["youngs_modulus_pa", "modulus_unit", "modulus_provenance", "layer"])
def test_required_inputs_have_no_defaults(field):
    """Neither a material table nor an implicit unit/source supplies missing data."""
    data = layer().model_dump()
    del data[field]
    with pytest.raises(ValueError):
        BondedAxialLayer.model_validate(data)


@pytest.mark.parametrize("unit", ["MPa", "GPa", "N", "", "kg/m^3"])
def test_incompatible_modulus_units_are_rejected(unit):
    """Conversion to Pa must occur explicitly before calling this API."""
    with pytest.raises(ValueError):
        layer(modulus_unit=unit)


@pytest.mark.parametrize("source", [None, {}, {"source_id": " ", "source_type": "project_assumption"}])
def test_missing_modulus_provenance_is_rejected(source):
    """A modulus must carry usable provenance independently of geometry."""
    with pytest.raises(ValueError):
        layer(modulus_provenance=source)


@pytest.mark.parametrize("geometry", [
    {"inner_diameter": (2, "m"), "thickness": (1, "m")},
    {"outer_diameter": (4, "m"), "thickness": (1, "m")},
])
def test_geometry_derivation_reuses_layer_contract(geometry):
    """r=1..2 gives A=3*pi and EA=30*pi for either derived diameter."""
    data = layer().layer.model_dump()
    for key in ("inner_diameter", "outer_diameter", "thickness"):
        data.pop(key, None)
    data.update({key: {"value": value, "unit": unit} for key, (value, unit) in geometry.items()})
    result = bonded_axial_stiffness([layer(layer=RadialLayer.model_validate(data))])
    assert result.axial_stiffness_n == pytest.approx(30 * math.pi, rel=1e-12)


@pytest.mark.parametrize("geometry", [
    {"inner_diameter": (2, "mm"), "outer_diameter": (4, "mm")},
    {"inner_diameter": (2, "m"), "thickness": (1, "inch")},
    {"outer_diameter": (4, "m"), "thickness": (3, "m")},
    {"inner_diameter": (2, "m")},
    {"inner_diameter": (4, "m"), "outer_diameter": (2, "m")},
    {"inner_diameter": (2, "m"), "outer_diameter": (4, "m"), "thickness": (2, "m")},
])
def test_invalid_geometry_is_rejected(geometry):
    """Reject non-SI, incomplete, inverted, and inconsistent geometry."""
    data = layer().layer.model_dump()
    for key in ("inner_diameter", "outer_diameter", "thickness"):
        data.pop(key, None)
    data.update({key: {"value": value, "unit": unit} for key, (value, unit) in geometry.items()})
    with pytest.raises(ValueError):
        bonded_axial_stiffness([layer(layer=RadialLayer.model_validate(data))])


@pytest.mark.parametrize("field", ["inner_diameter", "outer_diameter", "provenance"])
def test_missing_geometry_inputs_are_rejected(field):
    """Geometry and its provenance remain mandatory."""
    data = layer().layer.model_dump()
    del data[field]
    with pytest.raises(ValueError):
        bonded_axial_stiffness([layer(layer=RadialLayer.model_validate(data))])


@pytest.mark.parametrize("modulus", [1e308, 1e-323])
def test_unrepresentable_stiffness_is_rejected(modulus):
    """The r=2 disk with huge E overflows; a small disk with tiny E underflows."""
    outer = 4 if modulus > 1 else 0.1
    with pytest.raises(ValueError, match="finite and positive"):
        bonded_axial_stiffness([layer(0, outer, modulus)])


@pytest.mark.parametrize("scale", [1e200, 1e-200])
def test_unrepresentable_area_is_rejected(scale):
    """A=pi*scale²/4 overflows or underflows binary64."""
    with pytest.raises(ValueError, match="finite and positive"):
        bonded_axial_stiffness([layer(0, scale)])


def test_mutated_inputs_are_revalidated():
    """A model altered after construction must not bypass parameter checks."""
    item = layer()
    item.layer.outer_diameter.value = math.nan
    with pytest.raises(ValueError):
        bonded_axial_stiffness([item])


def test_unrepresentable_total_stiffness_is_rejected():
    """Each EA=3e307*pi is finite; their sum exceeds binary64's range."""
    with pytest.raises(ValueError, match="finite and positive"):
        bonded_axial_stiffness([layer(0, 2, 3e307), layer(2, 4, 1e307)])


@pytest.mark.parametrize("value", [math.inf, -math.inf, math.nan])
def test_nonfinite_geometry_is_rejected(value):
    """Nonfinite diameters cannot describe an annulus."""
    with pytest.raises(ValueError):
        layer(outer=value)


def test_mutated_modulus_and_provenance_are_revalidated():
    """Provenance and modulus checks also hold after construction."""
    item = layer()
    item.youngs_modulus_pa = 0
    with pytest.raises(ValueError):
        bonded_axial_stiffness([item])
    item = layer()
    item.modulus_provenance.source_id = " "
    with pytest.raises(ValueError):
        bonded_axial_stiffness([item])
