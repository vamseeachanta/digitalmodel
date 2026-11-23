# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand balances for the synthetic Strouhal example."""

from pathlib import Path
import runpy

import pytest


EXAMPLE = (
    Path(__file__).resolve().parents[4]
    / "examples/openfoam/cylinder_dimensionless.py"
)


def test_strouhal_example_hand_balance() -> None:
    """Use a hand calculation as the oracle, rather than a round trip alone."""
    row = runpy.run_path(str(EXAMPLE))["example_results"]()[0]
    # Synthetic inputs: f=3 Hz, D=0.4 m, U=2 m/s.
    # Independent balance: St=3*0.4/2=0.6; inverse f=0.6*2/0.4=3 Hz.
    assert row["frequency_hz"] == 3.0
    assert row["diameter_m"] == 0.4
    assert row["velocity_m_s"] == 2.0
    assert row["strouhal"] == pytest.approx(0.6)
    assert row["recovered_frequency_hz"] == pytest.approx(3.0)


def test_strouhal_example_similarity() -> None:
    """Change all dimensional inputs and retain the independent St balance."""
    base, scaled = runpy.run_path(str(EXAMPLE))["example_results"]()
    # Synthetic scaling: halve f, triple D, multiply U by 1.5.
    # Independent balance: St=1.5*1.2/3=0.6; inverse f=0.6*3/1.2=1.5 Hz.
    assert scaled["frequency_hz"] == 1.5
    assert scaled["diameter_m"] == 1.2
    assert scaled["velocity_m_s"] == 3.0
    assert base["strouhal"] == pytest.approx(0.6)
    assert scaled["strouhal"] == pytest.approx(0.6)
    assert scaled["recovered_frequency_hz"] == pytest.approx(1.5)


def test_strouhal_example_runs_as_script(capsys) -> None:
    """Keep the executable output explicit about the arithmetic-only scope."""
    runpy.run_path(str(EXAMPLE), run_name="__main__")
    output = capsys.readouterr().out
    assert "Synthetic arithmetic only; native cylinder benchmark pending." in output
    # Expected results are the independent balances above, printed with units.
    assert "strouhal=0.6 recovered_frequency_hz=3.0" in output
    assert "strouhal=0.6 recovered_frequency_hz=1.5" in output
