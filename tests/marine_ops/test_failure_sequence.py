# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Threshold oracles from independent scalar hand calculations."""

import math

import pytest

from digitalmodel.marine_ops.failure_sequence import FailureMode, rank_failures


def mode(name, demand, capacity=lambda factor: 10.0):
    return FailureMode(name, demand, capacity, 'Synthetic hand calculation')


def test_order_ties_initial_failure_and_censoring():
    # D/C: 2f/10 reaches one at 5; f/5 also at 5; 5f/10 at 2.
    # Constant 12/10 already fails at zero; f/100 never fails on [0, 6].
    result = rank_failures([
        mode('slow', lambda f: 2*f), mode('fast', lambda f: 5*f),
        mode('tie', lambda f: f, lambda f: 5),
        mode('initial', lambda f: 12), mode('censored', lambda f: f, lambda f: 100),
    ], [0, 1, 3, 6])
    assert result.governing == ('initial',)
    assert [row.name for row in result.ordered] == ['initial', 'fast', 'slow', 'tie']
    assert [row.first_failure_factor for row in result.ordered] == pytest.approx([0, 2, 5, 5])
    assert result.modes[-1].status == 'not_reached'
    assert result.modes[-1].first_failure_factor is None
    assert result.modes[0].utilizations == pytest.approx([0, 0.2, 0.6, 1.2])


def test_nonlinear_demand_and_changing_capacity():
    # f^2 / 4 = 1 at 2; 2f / (6-f) = 1 at 2, independently.
    result = rank_failures([
        mode('quadratic', lambda f: f*f, lambda f: 4),
        mode('variable', lambda f: 2*f, lambda f: 6-f),
    ], [0, 1, 3])
    assert result.governing == ('quadratic', 'variable')
    assert [m.first_failure_factor for m in result.modes] == pytest.approx([2, 2])


def test_nonmonotone_trace_keeps_first_observed_crossing():
    # f(4-f)/3 reaches 1 at f=1 and f=3, and exceeds it between.
    result = rank_failures([mode('hump', lambda f: f*(4-f), lambda f: 3)], [0, 2, 4])
    assert result.modes[0].first_failure_factor == pytest.approx(1)
    assert result.modes[0].utilizations[-1] == 0


def test_conditional_jump_is_located_without_inventing_a_continuous_root():
    # A conditional consequence starts at f=1.5, jumping from 0 to 2.
    result = rank_failures([mode('jump', lambda f: 20 if f >= 1.5 else 0)], [0, 1, 2])
    row = result.modes[0]
    assert row.first_failure_factor == pytest.approx(1.5)
    assert row.failure_utilization == 2
    assert row.last_nonfailure_factor < row.first_failure_factor


@pytest.mark.parametrize('grid', [[], [1, 2], [0], [0, 0], [0, -1], [0, math.nan], [False, 1]])
def test_invalid_grids(grid):
    with pytest.raises(ValueError):
        rank_failures([mode('one', lambda f: f)], grid)


@pytest.mark.parametrize('demand,capacity', [
    (lambda f: -1, lambda f: 10), (lambda f: math.nan, lambda f: 10),
    (lambda f: 1, lambda f: 0), (lambda f: 1, lambda f: math.inf),
    (lambda f: True, lambda f: 1), (lambda f: 1e308, lambda f: 1e-308),
])
def test_invalid_callable_results(demand, capacity):
    with pytest.raises(ValueError):
        rank_failures([mode('one', demand, capacity)], [0, 1])


def test_empty_duplicate_names_and_missing_basis_rejected():
    for modes in ([], [mode('a', lambda f: f)]*2, [mode('', lambda f: f)],
                  [FailureMode('a', lambda f: f, lambda f: 1, '')]):
        with pytest.raises(ValueError):
            rank_failures(modes, [0, 1])
