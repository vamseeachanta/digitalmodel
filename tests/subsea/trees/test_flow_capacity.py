# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Capacity and port-preserving handoff with caller-supplied coefficients."""

import importlib
import math

import pytest


def core():
    return importlib.import_module("digitalmodel.subsea.trees.flow_capacity")


def test_capacity_uses_explicit_si_coefficient():
    # Fixture C=20, rho=100: v=2 m/s; d=1: Q=pi/2 m3/s.
    result = core().bore_capacity(1, 100, 20)
    assert result.velocity_m_s == 2
    assert result.rate_m3_s == pytest.approx(math.pi / 2)
    assert core().bore_capacity(0.5, 100, 20).rate_m3_s == pytest.approx(math.pi / 8)


def test_handoff_preserves_separate_ports_and_pressure():
    production = core().bore_capacity(1, 100, 20)
    annulus = core().bore_capacity(0.5, 100, 20)
    result = core().flow_handoff(1, 0.1, production, annulus, 1000, 200)
    assert result.production_rate_m3_s == 1
    assert result.annulus_rate_m3_s == 0.1
    assert result.downstream_pressure_pa == 800
    assert result.within_capacity
    assert not core().flow_handoff(2, 0.1, production, annulus, 1000, 200).within_capacity
    assert not core().flow_handoff(1, 0.5, production, annulus, 1000, 200).within_capacity


def test_single_bore_cannot_deliver_annulus_flow():
    capacity = core().bore_capacity(1, 100, 20)
    assert core().flow_handoff(1, 0, capacity, None, 1000, 0).within_capacity
    with pytest.raises(ValueError, match="annulus"):
        core().flow_handoff(1, 0.1, capacity, None, 1000, 0)
    with pytest.raises(ValueError, match="pressure"):
        core().flow_handoff(1, 0, capacity, None, 100, 200)


@pytest.mark.parametrize("args", [(0, 1, 1), (1, 0, 1), (1, 1, -1), (1, math.inf, 1)])
def test_invalid_capacity_inputs(args):
    with pytest.raises(ValueError):
        core().bore_capacity(*args)
