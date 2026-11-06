# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check the executable example against the independent hand examples."""

import runpy
from pathlib import Path

import pytest


def test_example_answers_match_hand_values():
    """Oracles: test_wave_velocity.py and test_weibull_current.py hand reductions.

    The rounded bisection answer allows 5e-7 /m; the transfer and quantiles
    allow 1e-10 for rounding the independently tabulated hand values.
    """
    path = (
        Path(__file__).resolve().parents[3] / "examples/subsea/pipeline_calculations.py"
    )
    example = runpy.run_path(str(path))
    result = example["worked_example"]()
    assert result["wave_number_per_m"] == pytest.approx(0.119968, abs=5e-7)
    assert result["velocity_transfer_per_s"] == pytest.approx(1.9190347513, abs=1e-10)
    assert result["current_speed_m_s"] == pytest.approx(
        [0.1503770450, 3.2189490394], abs=1e-10
    )
