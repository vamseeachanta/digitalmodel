"""Real (non-mock) tests for the fatigue engine used by ShipFatigueAnalysis.

Issue #776: ``ShipFatigueAnalysis`` imported ``FatigueDamageCalculator`` from
``digitalmodel.signal_processing.signal_analysis.fatigue`` but the code calls
``router``, ``get_default_timetrace_cfg`` and ``get_default_sn_cfg`` -- methods
that live only on the *different* class ``FatigueAnalysis`` in
``digitalmodel.infrastructure.base_solvers.fatigue.fatigue_analysis``. Any real
run therefore raised ``AttributeError``. The import was corrected to point at
``FatigueAnalysis``.

These tests are deliberately NOT mocks. They:
  1. Assert the import wiring is correct (the module-level instance is a
     ``FatigueAnalysis`` exposing the methods the ship code actually calls).
  2. Exercise the real fatigue-damage arithmetic on the ``FatigueAnalysis``
     methods that are computable offline -- i.e. those that take an explicit
     S-N (fatigue) curve and do not depend on the missing
     ``fatigue_data.csv`` lookup -- and assert exact numeric results.

NOTE on scope: the full ``ShipFatigueAnalysis.router`` cannot run offline in
this repo. It requires SEASAM ``seasam_xtract`` ASCII extract files (read via
``read_seasam_xtract``); no such fixture exists anywhere in the tree. The S-N
curve CSV referenced by the default cfgs
(``tests/test_data/fatigue_analysis/fatigue_data.csv``) is also absent. So the
exercisable real compute is the curve-direct damage math below.
"""

import pandas as pd
import pytest

from digitalmodel.infrastructure.base_solvers.fatigue.fatigue_analysis import (
    FatigueAnalysis,
)
import digitalmodel.infrastructure.base_solvers.marine.ship.ship_fatigue_analysis as ship_mod


# A simple power-law S-N curve in the form FatigueAnalysis expects:
#   N = a1 * s ** m1   (m1 negative -> higher stress, fewer cycles)
SN_CURVE = {"a1": 1.0e15, "m1": -3.0}


def test_ship_module_wires_the_correct_fatigue_class():
    """Regression for the #776 wrong-import: the module-level instance the
    ship code calls must be a FatigueAnalysis exposing router /
    get_default_sn_cfg / get_default_timetrace_cfg (NOT FatigueDamageCalculator,
    which has none of these)."""
    assert isinstance(ship_mod.fatigue_analysis, FatigueAnalysis)
    for method in ("router", "get_default_sn_cfg", "get_default_timetrace_cfg"):
        assert hasattr(ship_mod.fatigue_analysis, method), (
            f"ship fatigue engine is missing {method!r}; wrong class imported"
        )


def test_get_cycles_to_failure_matches_power_law():
    fa = FatigueAnalysis()
    # N(100) = 1e15 * 100**-3 = 1e15 / 1e6 = 1e9
    assert fa.get_cycles_to_failure(SN_CURVE, 100.0) == pytest.approx(1.0e9)
    # N(200) = 1e15 * 200**-3 = 1e15 / 8e6 = 1.25e8
    assert fa.get_cycles_to_failure(SN_CURVE, 200.0) == pytest.approx(1.25e8)


def test_damage_from_rainflow_cycles_is_real_miners_sum():
    """Real Miner's-rule accumulation over rainflow-counted cycles -- the same
    routine ShipFatigueAnalysis ultimately drives through router()."""
    fa = FatigueAnalysis()
    rainflow_df = pd.DataFrame(
        {
            "range": [100.0, 50.0],
            "count": [1_000.0, 2_000.0],
        }
    )
    # N(100) = 1e9 ; N(50) = 1e15 * 50**-3 = 1e15 / 1.25e5 = 8e9
    # damage = 1000/1e9 + 2000/8e9 = 1e-6 + 2.5e-7 = 1.25e-6
    damage = fa.damage_from_rainflow_cycles(rainflow_df, SN_CURVE)
    assert damage == pytest.approx(1.25e-6, rel=1e-12)


def test_damage_accumulates_monotonically():
    """Adding cycles can only increase (never decrease) accumulated damage."""
    fa = FatigueAnalysis()
    one = pd.DataFrame({"range": [120.0], "count": [500.0]})
    two = pd.DataFrame({"range": [120.0, 80.0], "count": [500.0, 300.0]})
    assert fa.damage_from_rainflow_cycles(two, SN_CURVE) > fa.damage_from_rainflow_cycles(
        one, SN_CURVE
    )


def test_default_sn_cfg_shape():
    """The default S-N cfg the ship code requests must have the contract the
    router() dispatch keys on (calculation_type=damage, stress_input=sn)."""
    fa = FatigueAnalysis()
    cfg = fa.get_default_sn_cfg()
    assert cfg["inputs"]["calculation_type"] == "damage"
    assert cfg["inputs"]["stress_input"] == "sn"
    assert "SN" in cfg["inputs"]


def test_default_timetrace_cfg_shape():
    fa = FatigueAnalysis()
    cfg = fa.get_default_timetrace_cfg()
    assert cfg["inputs"]["calculation_type"] == "damage"
    assert cfg["inputs"]["stress_input"] == "timetrace"
    assert "timetraces" in cfg["inputs"]
