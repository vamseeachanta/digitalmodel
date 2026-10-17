# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Hand-checkable liquid Cv calculations; inputs are arithmetic fixtures."""

import importlib
import math

import pytest


def core():
    return importlib.import_module("digitalmodel.subsea.trees.pressure_control")


def test_linear_and_equal_percentage_trim_and_closed_seat():
    assert core().trim_cv(100, 0.5, "linear") == 50
    assert core().trim_cv(100, 0.5, "equal-percentage", 100) == pytest.approx(10)
    assert core().trim_cv(100, 0, "equal-percentage", 100) == 0
    assert core().trim_cv(100, 1, "equal-percentage", 100) == 100


def test_liquid_valve_losses_add_in_series():
    # SG*(Q/Cv)^2: 1*(20/10)^2 = 4 psi per valve.
    assert core().valve_pressure_drop(20, 10, 1) == 4
    assert core().series_pressure_drop(20, (10, 20), 1) == 5
    assert core().valve_pressure_drop(0, 0, 1) == 0
    with pytest.raises(ValueError, match="closed"):
        core().valve_pressure_drop(1, 0, 1)


def test_liquid_choke_distinguishes_subcritical_and_choked_flow():
    result = core().liquid_choke_flow(100, 96, 10, 1, 25)
    assert result.flow_gpm == 20
    assert result.regime == "subcritical"
    result = core().liquid_choke_flow(100, 50, 10, 1, 25)
    assert result.flow_gpm == 50
    assert result.regime == "critical"
    assert result.effective_drop_psi == 25
    assert core().liquid_choke_flow(100, 75, 10, 1, 25).regime == "critical"


@pytest.mark.parametrize("args", [
    (-1, 0, 1, 1, 1), (1, 2, 1, 1, 1), (1, 0, 1, 0, 1),
    (1, 0, 1, 1, 0), (1, 0, math.nan, 1, 1),
])
def test_invalid_choke_inputs(args):
    with pytest.raises(ValueError):
        core().liquid_choke_flow(*args)


@pytest.mark.parametrize("args", [
    (10, -0.1, "linear"), (10, 1.1, "linear"), (10, 0.5, "unknown"),
    (10, 0.5, "equal-percentage"), (10, 0.5, "equal-percentage", 1),
])
def test_invalid_trim_inputs(args):
    with pytest.raises(ValueError):
        core().trim_cv(*args)
