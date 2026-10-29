"""Execute the shipped example and compare with independent hand arithmetic.

Synthetic cases: 0.02 * 0.5 = 0.01 fatalities/year for a line break;
1000 * 0.0002 * 0.5 = 0.1 fatalities/year for collision. Without the
collision probability and consequence there is no evaluated numeric result.
"""

import io
import runpy
from pathlib import Path

import pytest

from digitalmodel.asset_integrity.safety_case import EvaluationStatus, MAHRegister


def test_executable_example_and_csv_exchange(capsys):
    path = (
        Path(__file__).resolve().parents[2]
        / "examples"
        / "asset_integrity"
        / "example_safety_case.py"
    )
    example = runpy.run_path(str(path), run_name="__main__")
    register = example["build_register"]()
    assert [
        h.expected_fatalities_per_year for h in register.hazards[:2]
    ] == pytest.approx([0.01, 0.1])
    assert register.hazards[2].status is EvaluationStatus.NOT_EVALUATED
    assert register.hazards[2].expected_fatalities_per_year is None
    assert MAHRegister.from_csv(io.StringIO(register.to_csv())) == register
    output = capsys.readouterr().out
    assert "1.0e-02" in output
    assert "1.0e-01" in output
    assert "NOT_EVALUATED" in output
