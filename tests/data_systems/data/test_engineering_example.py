# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise the documented example through all three synthetic data slices."""

import math
from pathlib import Path
import runpy


def test_worked_example(tmp_path):
    """Hand answers: 1000*10*5=50000 Pa; (2²-1²)*pi=3*pi m²;

    20000*3+50000*2=160000 N; dv/dx-du/dy=5-3=2 s^-1. Sources are
    OpenStax University Physics Vol. 1 (2016), §14.1, Eqs. 14.3 and 14.9;
    Precalculus 2e (2021), §5.1, with θ = 2π; Calculus Vol. 3 (2016),
    §6.5, Eq. 6.17.
    All inputs are synthetic arithmetic choices, with SI units.
    """
    script = (
        Path(__file__).resolve().parents[3]
        / "examples/data/engineering_value_checks.py"
    )
    example = runpy.run_path(str(script))
    answers = example["run_example"](tmp_path)
    assert answers == {
        "deepest_pressure_pa": 50000,
        "pipe_area_m2": 3 * math.pi,
        "panel_force_n": 160000,
        "vorticity_per_s": 2,
    }
