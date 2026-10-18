# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Hand calculations against explicitly supplied screening capacities."""

import importlib

import pytest


def core():
    return importlib.import_module("digitalmodel.subsea.manifolds.design_checks")


def test_bearing_and_settlement_have_independent_criteria():
    # 100 N/2 m2 = 50 Pa; 50 Pa/(1000 Pa/m) = 0.05 m.
    result = core().mudmat_screen(100, 2, 100, 1000, 0.1)
    assert result.bearing_utilisation == 0.5
    assert result.settlement_m == 0.05
    assert result.passed
    assert not core().mudmat_screen(100, 2, 40, 1000, 0.1).passed
    assert not core().mudmat_screen(100, 2, 100, 1000, 0.04).passed


def test_header_hoop_and_external_pressure_are_separate_cases():
    # Membrane screen: dp*OD/(2*t) = 100*1/(2*0.01) = 5000 Pa.
    result = core().header_screen(100, 0, 1, 0.01, 10000, 50)
    assert result.hoop_utilisation == 0.5
    assert result.external_utilisation == 0
    assert result.passed
    result = core().header_screen(0, 100, 1, 0.01, 10000, 50)
    assert result.external_utilisation == 2
    assert not result.passed


def test_installation_trawl_and_impact_limits_are_explicit():
    # Kinetic energy = m*v^2/2 = 2*3^2/2 = 9 J.
    result = core().load_case_screen(10, 20, 2, 3, 20, 20, 10)
    assert result.installation_utilisation == 0.5
    assert result.trawl_utilisation == 1
    assert result.impact_utilisation == 0.9
    assert result.passed
    assert not core().load_case_screen(21, 20, 2, 3, 20, 20, 10).passed
    assert not core().load_case_screen(10, 21, 2, 3, 20, 20, 10).passed
    assert not core().load_case_screen(10, 20, 2, 4, 20, 20, 10).passed


@pytest.mark.parametrize("args", [(1, 0, 1, 1, 1), (1, 1, 0, 1, 1), (float("nan"), 1, 1, 1, 1)])
def test_invalid_mudmat_input(args):
    with pytest.raises(ValueError):
        core().mudmat_screen(*args)


def test_impossible_pipe_geometry_rejected():
    with pytest.raises(ValueError):
        core().header_screen(1, 0, 1, 0.5, 1, 1)
