# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic ITC 1969 Annex I Regulations 3 and 4 hand calculations.

Source: UN Treaty Series, volume 1291, I-21264, printed pages 15-16:
https://treaties.un.org/doc/Publication/UNTS/Volume%201291/volume-1291-I-21264-English.pdf

All dimensions and counts below are synthetic test inputs, not measured data.
Regulatory coefficients in the independent arithmetic come from that source.
"""

import math

import pytest

from digitalmodel.alt_fuel_ship_sizing import tonnage as existing_tonnage
from digitalmodel.alt_fuel_ship_sizing.tonnage import TonnageResult
from digitalmodel.naval_architecture import tonnage as facade


@pytest.mark.parametrize("volume, expected", [(10000.0, 2800.0), (100000.0, 30000.0)])
def test_gross_tonnage_hand_calculation(volume, expected):
    """Reg 3: log10(V) = 4 or 5; (0.2 + 0.02 log10(V)) V = GT."""
    assert facade.gross_tonnage(volume) == pytest.approx(expected)


@pytest.mark.parametrize(
    "cargo, draught, n1, n2, factor, cargo_term, passenger_term, nt, cargo_floor, nt_floor",
    [
        (1000.0, 7.5, 0, 0, 1.0, 700.0, 0.0, 840.0, True, True),
        (10000.0, 10.0, 0, 0, 1.0, 2800.0, 0.0, 2800.0, False, False),
        (10000.0, 3.75, 0, 0, 0.25, 700.0, 0.0, 840.0, False, True),
        (10000.0, 7.5, 5, 7, 1.0, 2800.0, 0.0, 2800.0, False, False),
        (10000.0, 7.5, 1, 12, 1.0, 2800.0, 3.52, 2803.52, False, False),
        (1000.0, 7.5, 100, 200, 1.0, 700.0, 192.0, 892.0, True, False),
    ],
)
def test_net_tonnage_provisos(
    cargo, draught, n1, n2, factor, cargo_term, passenger_term, nt, cargo_floor, nt_floor
):
    """Reg 4 independent arithmetic for V=10000 m3, D=10 m, GT=2800.

    K2 is 0.26 for Vc=1000 and 0.28 for Vc=10000; K3=1.6.
    At d=7.5, the factor is unity; d=10 caps (4/3)^2 at unity;
    d=3.75 gives (1/2)^2=0.25. Cargo floor=0.25*2800=700;
    NT floor=0.30*2800=840. Twelve passengers contribute zero;
    thirteen contribute 1.6*(1+12/10)=3.52. With 100 and 200,
    the passenger term is 1.6*(100+200/10)=192, so NT=700+192=892.
    """
    result = facade.tonnage(10000.0, cargo, draught, 10.0, n1, n2)
    assert isinstance(result, TonnageResult)
    assert facade.TonnageResult is TonnageResult
    assert result.gross_tonnage == pytest.approx(2800.0)
    assert result.k1 == pytest.approx(0.28)
    assert result.k2 == pytest.approx(0.26 if cargo == 1000.0 else 0.28)
    assert result.k3 == pytest.approx(1.6)
    assert result.draught_depth_factor == pytest.approx(factor)
    assert result.cargo_term == pytest.approx(cargo_term)
    assert result.passenger_term == pytest.approx(passenger_term)
    assert result.net_tonnage == pytest.approx(nt)
    assert result.cargo_term_floor_applied is cargo_floor
    assert result.nt_floor_applied is nt_floor


_INVALID_MEASUREMENTS = [0, -1, math.nan, math.inf, -math.inf, True, False, None, "10"]


@pytest.mark.parametrize("value", _INVALID_MEASUREMENTS)
def test_invalid_gross_tonnage_inputs(value):
    with pytest.raises(ValueError, match="total_enclosed_volume_m3"):
        facade.gross_tonnage(value)


@pytest.mark.parametrize(
    "name",
    ["total_enclosed_volume_m3", "cargo_volume_m3", "moulded_draught_m", "moulded_depth_m"],
)
@pytest.mark.parametrize("value", _INVALID_MEASUREMENTS)
def test_invalid_tonnage_inputs(name, value):
    inputs = dict(
        total_enclosed_volume_m3=10000.0,
        cargo_volume_m3=1000.0,
        moulded_draught_m=7.5,
        moulded_depth_m=10.0,
    )
    inputs[name] = value
    with pytest.raises(ValueError, match=name):
        facade.tonnage(**inputs)


@pytest.mark.parametrize("position", [4, 5])
@pytest.mark.parametrize("value", [-1, 1.0, 1.5, math.nan, math.inf, True, False, None, "13"])
def test_invalid_passenger_counts(position, value):
    inputs = [10000.0, 1000.0, 7.5, 10.0, 0, 0]
    inputs[position] = value
    with pytest.raises(ValueError, match="passengers_"):
        facade.tonnage(*inputs)


def test_inconsistent_volumes_and_dimensions():
    with pytest.raises(ValueError, match="cargo_volume_m3"):
        facade.tonnage(10000.0, 10001.0, 7.5, 10.0)
    with pytest.raises(ValueError, match="moulded_draught_m"):
        facade.tonnage(10000.0, 1000.0, 11.0, 10.0)


@pytest.mark.parametrize("volume", [1e308, 1e-300])
def test_unrepresentable_gross_tonnage(volume):
    """Synthetic numeric extremes must not return infinite or negative GT."""
    with pytest.raises(ValueError):
        facade.gross_tonnage(volume)


@pytest.mark.parametrize(
    "inputs",
    [
        (1e308, 1000.0, 7.5, 10.0),
        (10000.0, 1000.0, 1e308, 1e308),
        (10000.0, 1e-300, 7.5, 10.0),
        (10000.0, 1000.0, 7.5, 10.0, 10**400),
    ],
)
def test_unrepresentable_net_tonnage(inputs):
    with pytest.raises(ValueError):
        facade.tonnage(*inputs)


@pytest.mark.parametrize(
    "inputs",
    [(10000.0, 1000.0, 7.5, 10.0), (100000.0, 60000.0, 10.0, 15.0, 100, 200)],
)
def test_preserves_existing_result_values(inputs):
    """Compatibility comparison supplements the independent hand calculations."""
    assert facade.tonnage(*inputs) == existing_tonnage(*inputs)


def test_delegates_to_existing_equations(monkeypatch):
    """Both entry points call the shared implementation and return its value."""
    from importlib import import_module

    implementation = import_module("digitalmodel.alt_fuel_ship_sizing.tonnage")
    expected = existing_tonnage(10000.0, 1000.0, 7.5, 10.0)
    gross_calls, net_calls = [], []

    def gross(volume):
        gross_calls.append(volume)
        return expected.gross_tonnage

    def net(*args):
        net_calls.append(args)
        return expected

    monkeypatch.setattr(implementation, "gross_tonnage", gross)
    monkeypatch.setattr(implementation, "tonnage", net)
    assert facade.gross_tonnage(10000.0) == expected.gross_tonnage
    assert facade.tonnage(10000.0, 1000.0, 7.5, 10.0) is expected
    assert gross_calls == [10000.0]
    assert net_calls == [(10000.0, 1000.0, 7.5, 10.0, 0, 0)]
