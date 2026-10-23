# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Geometry regressions for static QA; all readings are synthetic, no solver."""

import json
import math

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import (
    assert_static_acceptance,
    evaluate_static_acceptance,
)


def readings():
    """Use synthetic readings inside the existing campaign QA bands.

    Band source: slay-campaign/criteria.json, execution-3 (2026), static_qa;
    these readings are test inputs, not installation design reference values.
    """
    return dict(
        wire_length=10, end_a_reaction=1, last_support_force=0,
        grounded_tension=[0, 1], overbend_strain=0.0026,
        lift_off_arc=55, departure_angle=21,
        reference={"lift_off_arc": 55, "departure_angle": 21},
    )


@pytest.mark.parametrize("evaluate", [evaluate_static_acceptance, assert_static_acceptance])
@pytest.mark.parametrize("missing", [("diameter",), ("radius",), ("diameter", "radius")])
def test_geometry_is_required(evaluate, missing):
    # Existing geometry: case-table.yml (2026), basis.pipeline.dimensions and
    # basis.equipment.stinger. Omitting either input must never select it.
    geometry = dict(diameter=0.762, radius=150)
    for name in missing:
        geometry.pop(name)
    with pytest.raises(TypeError, match="required keyword-only"):
        evaluate(**readings(), **geometry)


@pytest.mark.parametrize("diameter,radius,limit,status", [
    (0.762, 150, 0.002794, "PASS"),
    (0.508, 120, 1397 / 600000, "FAIL"),
    (0.508, 150, 1397 / 750000, "FAIL"),
    (0.762, 120, 0.0034925, "PASS"),
])
def test_s1_uses_and_reports_supplied_geometry(diameter, radius, limit, status):
    """Independent hand calculations, with unchanged S1 from criteria.json.

    Source: execution-3 (2026), criteria.stinger_conformity: 1.10*D/(2R).
    Existing campaign: 1.10*0.762/300 = 0.8382/300 = 0.002794.
    Issue #2715 example: 1.10*0.508/240 = 0.5588/240 = 1397/600000.
    Change radius alone: 0.8382/240 = 0.0034925.
    Change diameter alone: 0.5588/300 = 1397/750000.
    Synthetic strain 0.0026 lies between the two example limits.
    """
    result = evaluate_static_acceptance(**readings(), diameter=diameter, radius=radius)
    assert result["diameter"] == diameter
    assert result["radius"] == radius
    assert result["s1_limit"] == pytest.approx(limit)
    assert result["rows"]["stinger_conformity"]["status"] == status
    assert result["counts"] == {
        "passed": 6 if status == "PASS" else 5,
        "failed": 0 if status == "PASS" else 1,
        "not_evaluated": 3,
    }
    assert result["fully_accepted"] is False
    assert json.loads(json.dumps(result, allow_nan=False)) == result


@pytest.mark.parametrize("evaluate", [evaluate_static_acceptance, assert_static_acceptance])
def test_s1_includes_boundary_and_rejects_next_float(evaluate):
    """Hand limit: 0.8382/300 = 0.002794; criteria.json execution-3 (2026)."""
    inputs = dict(readings(), diameter=0.762, radius=150, overbend_strain=0.002794)
    result = evaluate(**inputs)
    assert result["rows"]["stinger_conformity"]["status"] == "PASS"
    inputs["overbend_strain"] = math.nextafter(0.002794, math.inf)
    if evaluate is assert_static_acceptance:
        with pytest.raises(ValueError, match="S1"):
            evaluate(**inputs)
    else:
        assert evaluate(**inputs)["rows"]["stinger_conformity"]["status"] == "FAIL"


@pytest.mark.parametrize("field", ["diameter", "radius"])
@pytest.mark.parametrize("bad", [0, -1, None, float("nan"), float("inf")])
def test_geometry_must_remain_positive_and_finite(field, bad):
    # Geometry source: case-table.yml (2026); bad values are synthetic guards.
    geometry = dict(diameter=0.762, radius=150)
    geometry[field] = bad
    with pytest.raises(ValueError, match="Finite|Positive"):
        evaluate_static_acceptance(**readings(), **geometry)
