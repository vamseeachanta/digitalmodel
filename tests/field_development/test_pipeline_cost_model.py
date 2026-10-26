# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic arithmetic for issue #1740; no market price basis.

Hand calculation for the synthetic inputs below (all rates chosen for testing):
L = 24 m, OD = 1 m, wall = 0.1 m, density = 1000 kg/m^3.
ID = 1 - 2(0.1) = 0.8 m. Annulus = pi/4 (1^2 - 0.8^2) = 0.09 pi m^2.
Mass = 24(1000)(0.09 pi) = 2160 pi kg; at 2 USD/kg, steel = 4320 pi USD.
External area = pi(1)(24) = 24 pi m^2; at 3 USD/m^2, coating = 72 pi USD.
Two 12 m sections have one connecting weld; at 100 USD/weld, weld/NDT = 100 USD.
Evaluated subtotal = 4392 pi + 100 USD; per metre = 183 pi + 100/24 USD/m.
These expressions derive the oracles independently of the calculator.
Other numeric cases below are synthetic boundary, scaling or rejection probes.
"""

from dataclasses import FrozenInstanceError, asdict
from math import nextafter, pi

import pytest

from digitalmodel.economics.pipeline_cost_model import estimate_pipeline_cost


@pytest.fixture
def inputs():
    """Return the explicitly priced synthetic case derived above."""
    return {
        "length_m": 24.0,
        "outer_diameter_m": 1.0,
        "wall_thickness_m": 0.1,
        "steel_density_kg_m3": 1000.0,
        "steel_rate_per_kg": 2.0,
        "coating_rate_per_m2": 3.0,
        "joint_length_m": 12.0,
        "weld_ndt_rate_per_weld": 100.0,
        "currency": "USD",
        "input_provenance": "Synthetic geometry, density and rates for issue #1740",
    }


def test_annulus_mass_and_explicit_costs(inputs):
    result = estimate_pipeline_cost(**inputs)
    assert result.steel_mass_kg == pytest.approx(2160 * pi)
    assert result.steel_cost == pytest.approx(4320 * pi)
    assert result.coating_area_m2 == pytest.approx(24 * pi)
    assert result.coating_cost == pytest.approx(72 * pi)
    assert result.weld_count == 1
    assert result.weld_ndt_cost == pytest.approx(100)
    assert result.total_cost == pytest.approx(4392 * pi + 100)


def test_length_scaling(inputs):
    """Doubling length doubles material; four sections need three welds."""
    result = estimate_pipeline_cost(**{**inputs, "length_m": 48.0})
    assert result.steel_mass_kg == pytest.approx(4320 * pi)
    assert result.steel_cost == pytest.approx(8640 * pi)
    assert result.coating_cost == pytest.approx(144 * pi)
    assert result.weld_count == 3
    assert result.weld_ndt_cost == pytest.approx(300)
    assert result.total_cost == pytest.approx(8784 * pi + 300)


def test_cost_per_length(inputs):
    result = estimate_pipeline_cost(**inputs)
    assert result.cost_per_m == pytest.approx(183 * pi + 100 / 24)


@pytest.mark.parametrize(
    ("length", "welds"),
    [
        (6.0, 0),
        (12.0, 0),
        (nextafter(12.0, float("inf")), 1),
        (24.0, 1),
        (25.0, 2),
    ],
)
def test_weld_count_excludes_end_connections(inputs, length, welds):
    result = estimate_pipeline_cost(**{**inputs, "length_m": length})
    assert result.weld_count == welds
    assert result.weld_ndt_cost == pytest.approx(welds * 100)


def test_units_provenance_and_unevaluated_categories(inputs):
    result = estimate_pipeline_cost(**inputs)
    assert asdict(result.inputs) == inputs
    assert result.units == {
        "steel_mass_kg": "kg",
        "coating_area_m2": "m^2",
        "weld_count": "1",
        "steel_cost": "USD",
        "coating_cost": "USD",
        "weld_ndt_cost": "USD",
        "total_cost": "USD",
        "cost_per_m": "USD/m",
    }
    assert result.category_status == {
        "steel": "EVALUATED",
        "coating": "EVALUATED",
        "weld_ndt": "EVALUATED",
        "installation": "NOT_EVALUATED",
        "remaining_categories": "NOT_EVALUATED",
    }
    with pytest.raises(FrozenInstanceError):
        result.inputs.length_m = 48.0


def test_zero_rates_are_explicit_evaluated_costs(inputs):
    result = estimate_pipeline_cost(
        **{
            **inputs,
            "steel_rate_per_kg": 0,
            "coating_rate_per_m2": 0,
            "weld_ndt_rate_per_weld": 0,
        }
    )
    assert result.steel_mass_kg == pytest.approx(2160 * pi)
    assert result.steel_cost == result.coating_cost == result.weld_ndt_cost == 0
    assert result.total_cost == result.cost_per_m == 0
    assert result.category_status["installation"] == "NOT_EVALUATED"


NUMERIC_INPUTS = (
    "length_m",
    "outer_diameter_m",
    "wall_thickness_m",
    "steel_density_kg_m3",
    "steel_rate_per_kg",
    "coating_rate_per_m2",
    "joint_length_m",
    "weld_ndt_rate_per_weld",
)


@pytest.mark.parametrize("field", NUMERIC_INPUTS)
@pytest.mark.parametrize(
    "value",
    [float("nan"), float("inf"), -float("inf"), True, False, -1.0,
     "1", None, complex(1, 1)],
)
def test_invalid_numeric_inputs(inputs, field, value):
    with pytest.raises(ValueError, match=field):
        estimate_pipeline_cost(**{**inputs, field: value})


@pytest.mark.parametrize(
    "field",
    [
        "length_m", "outer_diameter_m", "wall_thickness_m",
        "steel_density_kg_m3", "joint_length_m",
    ],
)
def test_zero_physical_inputs(inputs, field):
    with pytest.raises(ValueError, match=field):
        estimate_pipeline_cost(**{**inputs, field: 0})


@pytest.mark.parametrize("wall", [0.5, 0.6, 1.0])
def test_wall_must_leave_a_bore(inputs, wall):
    with pytest.raises(ValueError, match="wall_thickness_m"):
        estimate_pipeline_cost(**{**inputs, "wall_thickness_m": wall})


@pytest.mark.parametrize("field", ["currency", "input_provenance"])
@pytest.mark.parametrize("value", ["", "  ", None, True, 1])
def test_metadata_is_required(inputs, field, value):
    with pytest.raises(ValueError, match=field):
        estimate_pipeline_cost(**{**inputs, field: value})


@pytest.mark.parametrize("field", NUMERIC_INPUTS + ("currency", "input_provenance"))
def test_no_implicit_inputs_or_price_defaults(inputs, field):
    del inputs[field]
    with pytest.raises(TypeError, match=field):
        estimate_pipeline_cost(**inputs)


@pytest.mark.parametrize(
    "changes",
    [
        {"length_m": 1e308},  # Finite inputs whose derived mass overflows.
        {"joint_length_m": 1e-308},  # Unrepresentable section ratio.
        {"steel_rate_per_kg": 1e308},  # Unrepresentable steel subtotal.
        {"length_m": 1e-300, "wall_thickness_m": 1e-300},  # Mass underflows.
        {"steel_density_kg_m3": 10**400},  # Not representable as a float.
        {  # Positive coating cost underflows while mass remains representable.
            "length_m": 1e-100,
            "outer_diameter_m": 1e-100,
            "wall_thickness_m": 1e-101,
            "coating_rate_per_m2": 1e-200,
        },
    ],
)
def test_unrepresentable_arithmetic_is_rejected(inputs, changes):
    with pytest.raises(ValueError):
        estimate_pipeline_cost(**{**inputs, **changes})
