"""Miner damage must not depend on the CPU's SIMD kernels.

NumPy evaluates ``np.power`` with processor-specific vector kernels, so the
last bits of allowable cycles varied between CI runner processors (one bin's
allowable cycles were 20711746965.703632 on one runner and 20711746965.703636
on another). A pairwise ``ndarray.sum`` also depends on bin order. Both made
committed report pages that embed unrounded damage differ between runners.
"""

import math
import random

import numpy as np
import pandas as pd
import pytest
from pylife.materiallaws.woehlercurve import WoehlerCurve

from digitalmodel.fatigue.damage import miner_damage
from digitalmodel.fatigue.sn_curves import get_sn_curve


def _reference_allowable(stress, curve):
    """Scalar Basquin evaluation, N = ND * (S / SD) ** -k, one bin at a time."""
    sd, nd = float(curve.SD), float(curve.ND)
    k = float(curve.k_1) if stress >= sd else float(curve.k_2)
    if not math.isfinite(k):
        return math.inf
    return nd * math.pow(stress / sd, -k)


def test_total_damage_is_identical_for_every_bin_order():
    # Damage spread over many magnitudes, so a non-exact summation would
    # round differently for different orders.
    curve = WoehlerCurve(pd.Series({"SD": 50.0, "ND": 1e7, "k_1": 3.0, "k_2": 5.0}))
    rng = random.Random(20261031)
    stress = [rng.uniform(5.0, 400.0) for _ in range(257)]
    cycles = [10.0 ** rng.uniform(0.0, 9.0) for _ in range(257)]
    totals = set()
    bins = list(zip(stress, cycles))
    for _ in range(25):
        rng.shuffle(bins)
        frame = pd.DataFrame(bins, columns=["stress_range", "cycles"])
        result = miner_damage(frame, curve)
        totals.add(result.attrs["total_damage"].hex())
        assert result.attrs["total_damage"] == math.fsum(result["damage"])
    assert len(totals) == 1


def test_allowable_cycles_do_not_use_numpy_vector_power(monkeypatch):
    # Perturb NumPy's vector power by one ulp. A CPU-deterministic damage path
    # evaluates Basquin per bin with scalar operations and is unaffected.
    curve = get_sn_curve("F1", "seawater_cp")
    stress = [8.0, 9.0, 12.0, 30.0, 58.0, 59.0, 120.0]
    cycles = [4496958.0, 1.0e5, 2.0e4, 3.0e3, 1.0e3, 1.0e3, 10.0]
    expected = [_reference_allowable(s, curve) for s in stress]

    real_power = np.power

    def perturbed_power(base, exponent, *args, **kwargs):
        return np.nextafter(real_power(base, exponent, *args, **kwargs), np.inf)

    monkeypatch.setattr(np, "power", perturbed_power)
    result = miner_damage(pd.DataFrame({"stress_range": stress, "cycles": cycles}), curve)
    monkeypatch.undo()

    assert [value.hex() for value in result["allowable_cycles"]] == [value.hex() for value in expected]
    damage = [c / n for c, n in zip(cycles, expected)]
    assert [value.hex() for value in result["damage"]] == [value.hex() for value in damage]
    assert result.attrs["total_damage"] == math.fsum(damage)


def test_hangoff_viv_bin_pins_the_runner_independent_value():
    # The bin whose last digits varied between CI runners.
    curve = get_sn_curve("F1", "seawater_cp")
    result = miner_damage(pd.DataFrame({"stress_range": [8.0], "cycles": [4496958.0]}), curve)
    assert result["allowable_cycles"].iloc[0] == 20711746965.703636
    assert result.attrs["total_damage"] == 0.00021712113456419033


@pytest.mark.parametrize("stress, cycles, allowable", [
    (0.0, 10.0, math.inf),     # unloaded stress: no damage
    (50.0, 0.0, 1.0e7),        # unloaded cycles: no damage
    (20.0, 1.0e9, math.inf),   # below SD with perfect endurance
])
def test_scalar_path_keeps_zero_damage_bins(stress, cycles, allowable):
    curve = WoehlerCurve(pd.Series({"SD": 50.0, "ND": 1e7, "k_1": 3.0}))
    result = miner_damage(pd.DataFrame({"stress_range": [stress], "cycles": [cycles]}), curve)
    assert result["allowable_cycles"].iloc[0] == allowable
    assert result.attrs["total_damage"] == 0.0


def test_loaded_bin_with_overflowing_allowable_is_rejected():
    # A tiny stress on a steep curve overflows; pyLife returns inf there, and
    # an infinite allowable for a loaded bin above zero stress is unqualified.
    curve = WoehlerCurve(pd.Series({"SD": 50.0, "ND": 1e7, "k_1": 3.0, "k_2": 400.0}))
    with pytest.raises(ValueError, match="allowable cycles"):
        miner_damage(pd.DataFrame({"stress_range": [1e-3], "cycles": [1.0]}), curve)
