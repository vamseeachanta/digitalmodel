# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent hand calculations for the public rainflow counter.

All sample values are synthetic, in arbitrary load units. The numbered hand
calculations below are the source of every expected range and weighted count;
no standard's worked example or production counter supplies the expectations.
Tables retain separate full cycles (weight 1.0) and residual half cycles
(weight 0.5), including duplicate rows, so missing or extra cycles are visible.

Hand calculation 1 -- triangular history [1, 4, 0, 4, 0, 4, 0, 2]:
Appending each repeated peak closes a 4 -> 0 excursion, giving two full
cycles of range 4. The remaining reversal stack is [1, 4, 0, 2]. Its adjacent
segments give half cycles of ranges |4 - 1| = 3, |0 - 4| = 4 and |2 - 0| = 2.
Thus the range-4 weighted count is 2.5 and the total weighted count is 3.5.

Hand calculation 2 -- nested history [0, 12, 3, 9, 5, 7, 1, 14, 4]:
The stack [0, 12, 3, 9, 5, 7] retains the shrinking excursions. Appending 1
closes 5 -> 7 (range 2), leaving [0, 12, 3, 9, 1], then closes 3 -> 9
(range 6), leaving [0, 12, 1]. Appending 14 closes 12 -> 1 (range 11),
leaving [0, 14]. Finally appending 4 leaves [0, 14, 4]: residual half cycles
of ranges 14 and 10. The three full cycles and two halves have total weight 4.

Hand calculation 3 -- transformations of calculations 1 and 2:
Adding the synthetic offset 20 leaves every difference unchanged. Multiplying
by the positive synthetic factor 2.5 multiplies ranges by 2.5 and preserves
weights. Triangular ranges (2, 3, 4) become (5, 7.5, 10); nested ranges
(2, 6, 10, 11, 14) become (5, 15, 25, 27.5, 35). These transformed tables
are written literally below, independently of any counter result.

Hand calculation 4 -- degenerate histories and the residual-cycle policy:
A constant history [7, 7, 7, 7] has no excursion and an empty table. Strictly
increasing [-2, -1, 0, 1, 2, 3] and decreasing [3, 2, 1, 0, -1, -2]
histories each retain only the endpoints: |3 - (-2)| = 5, counted once as
a residual half cycle. The record is not periodically closed.

Gate filtering is disabled with a zero absolute threshold. Cycle combination
is disabled to retain the hand-enumerated rows without range rounding.
These cases document the selected public behaviour, not standards compliance.
"""

import pytest

from digitalmodel.structural.fatigue.rainflow import (
    RainflowCounter,
    RainflowParameters,
)


TRIANGULAR_HISTORY = (1, 4, 0, 4, 0, 4, 0, 2)
NESTED_HISTORY = (0, 12, 3, 9, 5, 7, 1, 14, 4)


def _public_cycle_table(history):
    """Return every public cycle row, ordered only for table comparison."""
    counter = RainflowCounter(
        RainflowParameters(
            gate_value=0.0,
            gate_type="absolute",
            small_cycle_removal=False,
            cycle_combination=False,
        )
    )
    result = counter.count_cycles(list(history))
    assert result["statistics"].filter_removed == 0
    return sorted(
        result["cycles"][["range", "count"]].itertuples(index=False, name=None)
    )


def test_public_counter_triangular_half_cycles_hand_count():
    """Match all full and residual cycles from hand calculation 1."""
    assert _public_cycle_table(TRIANGULAR_HISTORY) == [
        (2.0, 0.5),
        (3.0, 0.5),
        (4.0, 0.5),
        (4.0, 1.0),
        (4.0, 1.0),
    ]


def test_public_counter_nested_history_hand_count():
    """Match the nested stack closures and residuals in hand calculation 2."""
    assert _public_cycle_table(NESTED_HISTORY) == [
        (2.0, 1.0),
        (6.0, 1.0),
        (10.0, 0.5),
        (11.0, 1.0),
        (14.0, 0.5),
    ]


@pytest.mark.parametrize(
    "history, offset, scale, expected",
    [
        pytest.param(
            TRIANGULAR_HISTORY,
            20.0,
            1.0,
            [(2.0, 0.5), (3.0, 0.5), (4.0, 0.5), (4.0, 1.0), (4.0, 1.0)],
            id="triangular-offset",
        ),
        pytest.param(
            NESTED_HISTORY,
            20.0,
            1.0,
            [(2.0, 1.0), (6.0, 1.0), (10.0, 0.5), (11.0, 1.0), (14.0, 0.5)],
            id="nested-offset",
        ),
        pytest.param(
            TRIANGULAR_HISTORY,
            0.0,
            2.5,
            [(5.0, 0.5), (7.5, 0.5), (10.0, 0.5), (10.0, 1.0), (10.0, 1.0)],
            id="triangular-scale",
        ),
        pytest.param(
            NESTED_HISTORY,
            0.0,
            2.5,
            [(5.0, 1.0), (15.0, 1.0), (25.0, 0.5), (27.5, 1.0), (35.0, 0.5)],
            id="nested-scale",
        ),
    ],
)
def test_public_counter_offset_and_scale_hand_tables(history, offset, scale, expected):
    """Use literal transformed expectations from hand calculation 3."""
    transformed_history = [scale * value + offset for value in history]
    assert _public_cycle_table(transformed_history) == expected


@pytest.mark.parametrize(
    "history, expected",
    [
        pytest.param([7, 7, 7, 7], [], id="constant"),
        pytest.param([-2, -1, 0, 1, 2, 3], [(5.0, 0.5)], id="increasing"),
        pytest.param([3, 2, 1, 0, -1, -2], [(5.0, 0.5)], id="decreasing"),
    ],
)
def test_public_counter_degenerate_histories_residual_policy(history, expected):
    """Check the open-record residual policy in hand calculation 4."""
    assert _public_cycle_table(history) == expected
