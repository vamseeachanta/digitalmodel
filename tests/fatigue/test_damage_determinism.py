"""Miner damage must not depend on the CPU's floating-point kernels.

NumPy evaluates ``np.power`` with processor-specific vector kernels, so the
last bits of allowable cycles varied between CI runner processors (one bin's
allowable cycles were 20711746965.703632 on one runner and 20711746965.703636
on another). The scalar libm ``pow`` is also dispatched per CPU, and a pairwise
``ndarray.sum`` depends on bin order. Each of these made committed report pages
that embed unrounded damage differ between runners.
"""

import math
import random
from fractions import Fraction

import numpy as np
import pandas as pd
import pytest
from pylife.materiallaws.woehlercurve import WoehlerCurve

from digitalmodel.fatigue import damage as damage_module
from digitalmodel.fatigue.damage import miner_damage
from digitalmodel.fatigue.sn_curves import get_sn_curve


def _exact_allowable(stress, curve):
    """N = ND * (S / SD) ** -k for an integer k, from the float ratio pyLife
    forms, in exact rational arithmetic with one final rounding."""
    sd, nd = float(curve.SD), float(curve.ND)
    k = float(curve.k_1) if stress >= sd else float(curve.k_2)
    if not math.isfinite(k):
        return math.inf
    assert k.is_integer()
    try:
        return float(Fraction(nd) * Fraction(stress / sd) ** -int(k))
    except (OverflowError, ZeroDivisionError):
        return math.inf


def _frame(stress, cycles):
    return pd.DataFrame({"stress_range": stress, "cycles": cycles})


def test_total_damage_is_identical_for_every_bin_order():
    # Damage spread over many magnitudes, so a non-exact summation would
    # round differently for different orders.
    curve = WoehlerCurve(pd.Series({"SD": 50.0, "ND": 1e7, "k_1": 3.0, "k_2": 5.0}))
    rng = random.Random(20261031)
    bins = [(rng.uniform(5.0, 400.0), 10.0 ** rng.uniform(0.0, 9.0)) for _ in range(257)]
    totals = set()
    for _ in range(25):
        rng.shuffle(bins)
        result = miner_damage(pd.DataFrame(bins, columns=["stress_range", "cycles"]), curve)
        totals.add(result.attrs["total_damage"].hex())
        assert result.attrs["total_damage"] == math.fsum(result["damage"])
    assert len(totals) == 1


@pytest.mark.parametrize("environment", ["air", "seawater_cp", "free_corrosion"])
def test_integer_slope_allowable_uses_neither_numpy_nor_libm_power(monkeypatch, environment):
    # Perturb both power kernels by one ulp. Integer-slope curves (every DNV
    # curve in the library) must be evaluated exactly and stay unaffected.
    curve = get_sn_curve("F1", environment)
    stress = [8.0, 9.0, 12.0, 30.0, 58.0, 59.0, 120.0]
    cycles = [4496958.0, 1.0e5, 2.0e4, 3.0e3, 1.0e3, 1.0e3, 10.0]
    expected = [_exact_allowable(s, curve) for s in stress]

    real_np_power, real_math_pow = np.power, math.pow
    monkeypatch.setattr(np, "power", lambda *a, **k: np.nextafter(real_np_power(*a, **k), np.inf))
    monkeypatch.setattr(math, "pow", lambda x, y: math.nextafter(real_math_pow(x, y), math.inf))
    result = miner_damage(_frame(stress, cycles), curve)
    monkeypatch.undo()

    assert [v.hex() for v in result["allowable_cycles"]] == [v.hex() for v in expected]
    damage = [c / n for c, n in zip(cycles, expected)]
    assert [v.hex() for v in result["damage"]] == [v.hex() for v in damage]
    assert result.attrs["total_damage"] == math.fsum(damage)


def test_hangoff_viv_bin_pins_the_runner_independent_value():
    # The bin whose last digits varied between CI runners.
    curve = get_sn_curve("F1", "seawater_cp")
    result = miner_damage(_frame([8.0], [4496958.0]), curve)
    assert result["allowable_cycles"].iloc[0] == _exact_allowable(8.0, curve)
    assert result.attrs["total_damage"] == 4496958.0 / _exact_allowable(8.0, curve)


def test_exact_evaluation_stays_within_rounding_of_pylife():
    curve = get_sn_curve("D", "air")
    stress = np.linspace(1.0, 400.0, 401)
    ours = miner_damage(_frame(stress, np.ones_like(stress)), curve)["allowable_cycles"]
    np.testing.assert_allclose(ours, curve.cycles(stress), rtol=4e-15, atol=0)


@pytest.mark.parametrize("stress, cycles, allowable", [
    (0.0, 10.0, math.inf),     # unloaded stress: no damage
    (50.0, 0.0, 1.0e7),        # unloaded cycles: no damage
    (20.0, 1.0e9, math.inf),   # below SD with perfect endurance
])
def test_exact_path_keeps_zero_damage_bins(stress, cycles, allowable):
    curve = WoehlerCurve(pd.Series({"SD": 50.0, "ND": 1e7, "k_1": 3.0}))
    result = miner_damage(_frame([stress], [cycles]), curve)
    assert result["allowable_cycles"].iloc[0] == allowable
    assert result.attrs["total_damage"] == 0.0


def test_loaded_bin_with_overflowing_allowable_is_rejected():
    curve = WoehlerCurve(pd.Series({"SD": 50.0, "ND": 1e7, "k_1": 3.0, "k_2": 60.0}))
    with pytest.raises(ValueError, match="allowable cycles"):
        miner_damage(_frame([1e-6], [1.0]), curve)


def test_total_damage_overflow_is_rejected_as_value_error():
    # Each bin damage is finite; their sum is not.
    curve = WoehlerCurve(pd.Series({"SD": 1.0, "ND": 1.0, "k_1": 3.0, "k_2": 3.0}))
    with pytest.raises(ValueError, match="total damage"):
        miner_damage(_frame([1.0, 1.0], [1e308, 1e308]), curve)


def test_non_integer_slope_matches_pylife_within_rounding():
    curve = WoehlerCurve(pd.Series({"SD": 50.0, "ND": 1e7, "k_1": 3.5, "k_2": 5.5}))
    stress = [10.0, 49.0, 50.0, 200.0]
    result = miner_damage(_frame(stress, [1.0] * 4), curve)
    np.testing.assert_allclose(result["allowable_cycles"], curve.cycles(np.array(stress)), rtol=4e-15)


class _SteeperCurve(WoehlerCurve):
    def _make_k(self, src, ref, wc):
        return np.full_like(np.asarray(src, dtype=float), 6.0)


def test_subclass_customisation_is_honoured():
    # A subclass inherits cycles and basquin_cycles but changes the slope.
    curve = _SteeperCurve(pd.Series({"SD": 50.0, "ND": 1e7, "k_1": 3.0, "k_2": 5.0}))
    result = miner_damage(_frame([100.0], [1.0]), curve)
    assert result["allowable_cycles"].iloc[0] == pytest.approx(1e7 * 2.0 ** -6, rel=1e-12)


@pytest.mark.parametrize("name", ["_make_k", "broadcast", "transform_to_failure_probability", "basquin_cycles"])
def test_instance_override_of_a_pylife_step_is_honoured(monkeypatch, name):
    curve = WoehlerCurve(pd.Series({"SD": 50.0, "ND": 1e7, "k_1": 3.0, "k_2": 5.0}))
    calls = []
    original = getattr(curve, name)

    def spy(*args, **kwargs):
        calls.append(name)
        return original(*args, **kwargs)

    monkeypatch.setattr(curve, name, spy)
    miner_damage(_frame([100.0], [1.0]), curve)
    assert calls


def test_scattered_curve_off_median_uses_pylife():
    # Off-median scatter: the default-probability handling is pyLife's own.
    curve = WoehlerCurve(pd.Series({"SD": 100.0, "ND": 1e6, "k_1": 3.0, "k_2": 3.0,
                                    "TN": 4.0, "TS": 4.0 ** (1 / 3), "failure_probability": 0.9}))
    stress = np.array([200.0])
    result = miner_damage(_frame(stress, [1.0]), curve)
    assert result["allowable_cycles"].iloc[0] == curve.cycles(stress)[0]
