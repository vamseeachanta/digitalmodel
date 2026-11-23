# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Executable acceptance checks for signal-analysis outputs."""

import numpy as np
import pandas as pd
import pytest

from digitalmodel.signal_processing.signal_analysis import RainflowCounter
from digitalmodel.signal_processing.signal_analysis.fatigue import (
    FatigueDamageCalculator,
    SNCurve,
)


def test_rainflow_csv_matches_cycle_table(tmp_path):
    """Read back the module CSV and compare every cycle row with memory."""
    from digitalmodel.signal_processing.signal_analysis.orcaflex.analyzer import (
        TimeSeriesAnalyzer,
    )

    signal = np.array([0.0, 10.0, -5.0, 20.0, -10.0, 0.0])
    cycles = RainflowCounter().count_cycles(signal)
    assert not cycles.empty
    analyzer = TimeSeriesAnalyzer(config={
        "output": {"organization": {"by_file": True}, "plots": {"enable": False}},
    })
    results = {
        "file": "signal.csv",
        "columns": {"Stress (MPa)": {"rainflow": {"cycles": cycles}}},
    }

    analyzer._generate_outputs(results, tmp_path)

    restored = pd.read_csv(tmp_path / "signal" / "rainflow_Stress_MPa.csv")
    pd.testing.assert_frame_equal(restored, cycles, check_exact=True)


def test_single_slope_damage_closed_form():
    """Check allowable cycles and Miner damage against hand-derived values."""
    # User-defined coefficients, independent of any standards table:
    # N = 1e12 * S**-3, giving 1e6 and 125000 allowable cycles.
    # D = 1000/1e6 + 2000/125000 = 0.017.
    stresses = np.array([100.0, 200.0])
    curve = SNCurve(curve_type="power_law", A=1e12, m=3.0, fatigue_limit=0.0)
    cycles = pd.DataFrame({
        "range": stresses,
        "mean": [0.0, 0.0],
        "count": [1000.0, 2000.0],
    })
    relative_tolerance = 0.001  # 0.1 percent for both output quantities.

    allowable = curve.get_allowable_cycles(stresses)
    damage = FatigueDamageCalculator(method="miners").calculate_damage(
        cycles, {"type": "power_law", "A": 1e12, "m": 3.0, "fatigue_limit": 0.0},
    )

    np.testing.assert_allclose(
        allowable, [1000000.0, 125000.0], rtol=relative_tolerance, atol=0.0,
    )
    assert damage["total_damage"] == pytest.approx(
        0.017, rel=relative_tolerance, abs=0.0,
    )
