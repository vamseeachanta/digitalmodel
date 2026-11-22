# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for ideal parallel network arithmetic.

All finite inputs below are synthetic choices, not design criteria. The basis
is Ohm's law, I = V/R, and current balance, I_total = sum(I_branch):

* Calculation A: 36 V across 6, 12 and 18 ohm gives 6, 3 and 2 A;
  total current is 11 A and equivalent resistance is 36/11 ohm.
* Calculation B: 10 V across 2, 5 and 10 ohm gives 5, 2 and 1 A;
  total current is 8 A and equivalent resistance is 10/8 ohm.
* Splitting A's 12 ohm branch into two 24 ohm branches gives 1.5 A
  in each replacement branch, preserving its 3 A total at 36 V.
* Multiplying every resistance in A by a chosen dimensionless factor
  (0.25, 3 or 10) divides each current by that factor at the same voltage,
  so equivalent resistance becomes (36/11) times that factor.

Zero, negative one and non-finite values are invalid-input probes. Floating
point assertions use pytest's default approximation tolerance.
"""

from itertools import permutations

import pytest

from digitalmodel.cathodic_protection import _kernels as k


@pytest.mark.parametrize(
    "potential_v, branches_ohm, hand_currents_a",
    [
        (36.0, [6.0, 12.0, 18.0], [6.0, 3.0, 2.0]),
        (10.0, [2.0, 5.0, 10.0], [5.0, 2.0, 1.0]),
    ],
    ids=["calculation-a", "calculation-b"],
)
def test_parallel_resistance_matches_branch_current_balance(
    potential_v: float, branches_ohm: list[float], hand_currents_a: list[float]
) -> None:
    """Recover V/I_total from independently summed branch currents."""
    branch_currents_a = [potential_v / resistance for resistance in branches_ohm]
    assert branch_currents_a == pytest.approx(hand_currents_a)
    expected_ohm = potential_v / sum(hand_currents_a)

    equivalent_ohm = k.parallel_resistance(branches_ohm)

    assert equivalent_ohm == pytest.approx(expected_ohm)
    assert potential_v / equivalent_ohm == pytest.approx(sum(branch_currents_a))


def test_parallel_branch_split_and_permutation_preserve_equivalence() -> None:
    """Check every ordering of A and its equivalent split against V/11 A."""
    potential_v = 36.0
    expected_current_a = 11.0
    expected_ohm = potential_v / expected_current_a
    original_ohm = [6.0, 12.0, 18.0]
    split_ohm = [6.0, 24.0, 24.0, 18.0]

    for branches_ohm in (original_ohm, split_ohm):
        for ordering in set(permutations(branches_ohm)):
            branch_current_a = sum(potential_v / resistance for resistance in ordering)
            assert branch_current_a == pytest.approx(expected_current_a)
            assert k.parallel_resistance(list(ordering)) == pytest.approx(expected_ohm)


@pytest.mark.parametrize("scale", [0.25, 3.0, 10.0])
def test_parallel_common_resistance_scaling(scale: float) -> None:
    """Check A with resistances scaled down and up using its hand result."""
    branches_ohm = [6.0, 12.0, 18.0]
    expected_ohm = (36.0 / 11.0) * scale

    assert k.parallel_resistance(
        [resistance * scale for resistance in branches_ohm]
    ) == pytest.approx(expected_ohm)


@pytest.mark.parametrize(
    "branches_ohm",
    [
        [],
        [6.0, 0.0, 18.0],
        [6.0, -1.0, 18.0],
        [6.0, float("nan"), 18.0],
        [6.0, float("inf"), 18.0],
        [6.0, float("-inf"), 18.0],
    ],
    ids=["empty", "zero", "negative", "nan", "positive-infinity", "negative-infinity"],
)
def test_parallel_resistance_retains_invalid_input_errors(
    branches_ohm: list[float],
) -> None:
    """Retain ValueError naming the parameter for invalid branch inputs."""
    with pytest.raises(ValueError, match="branch_resistances_ohm"):
        k.parallel_resistance(branches_ohm)
