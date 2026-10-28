# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for the cited force-balance example.

Numerical basis: OpenStax, University Physics Volume 1, first edition (2016),
section 6.1, Example 6.6; kinematics: section 3.4, equations 3.12 and 3.13.
See drive_off_balance.md for source links and the uniform-acceleration
idealisation. Cancellation, force switching and invalid inputs are algebraic
probes, not additional published physical scenarios.
"""

import re
import runpy
from pathlib import Path

import pytest


@pytest.fixture
def example():
    """Load the saved example without importing the hydrodynamics package."""
    path = (
        Path(__file__).resolve().parents[2]
        / "examples/domains/hydrodynamics/drive_off_balance.py"
    )
    return runpy.run_path(path.as_posix())


@pytest.fixture
def simulate(example):
    """Use the integrator from the saved example."""
    return example["simulate"]


def test_drive_off_constant_force(simulate):
    """Example 6.6: (224-0)/70 = 3.20 m/s**2 from rest.

    Hand basis at 2.50 s: v=3.2*2.5=8 m/s; x=3.2*2.5**2/2=10 m.
    The numerical partition subdivides the source duration unevenly.
    """
    times_s = [0.0, 0.25, 1.0, 2.5]
    states = simulate(70.0, times_s, [224.0] * 3, [0.0] * 3)
    assert [state.time_s for state in states] == times_s
    # v=3.2*t; x=1.6*t**2, evaluated independently at each boundary.
    assert [state.velocity_m_s for state in states] == pytest.approx(
        [0.0, 0.8, 3.2, 8.0]
    )
    assert [state.position_m for state in states] == pytest.approx(
        [0.0, 0.1, 1.6, 10.0]
    )
    assert states[-1].velocity_m_s / times_s[-1] == pytest.approx(3.2)


def test_printed_example_uses_published_reference(example, capsys):
    """Catch uncited input sets or endpoints in the executable demonstration."""
    example["main"]()
    output = capsys.readouterr().out
    inputs = next(line for line in output.splitlines() if line.startswith("Mass:"))
    assert [float(value) for value in re.findall(r"\d+\.\d+", inputs)] == (
        pytest.approx([70.0, 224.0, 0.0])
    )
    rows = re.findall(
        r"^\s*(\d+\.\d+)\s+(\d+\.\d+)\s+(\d+\.\d+)\s*$", output, re.MULTILINE
    )
    # Source duration 2.50 s; hand basis v=3.2*2.5=8, x=3.2*2.5**2/2=10.
    assert [float(value) for value in rows[-1]] == pytest.approx([2.5, 8.0, 10.0])
    assert "OpenStax" in output and "Example 6.6" in output


def test_drive_off_zero_balance(simulate):
    """Algebraic cancellation of Example 6.6's force: (224-224)/70=0."""
    states = simulate(
        70.0, [0.0, 1.0, 2.5], [224.0, 224.0], [224.0, 224.0]
    )
    assert all(state.position_m == 0.0 for state in states)
    assert all(state.velocity_m_s == 0.0 for state in states)


def test_drive_off_prescribed_shutdown(simulate):
    """Algebraic sign switch using Example 6.6's mass, force and duration.

    First a=224/70=3.2 for 2.5 s gives (v,x)=(8,10). Then a=-224/70=-3.2
    for the same duration gives v=8-3.2*2.5=0 and x=10+8*2.5-3.2*2.5**2/2=20.
    This is a constructed indexing/carry-over check, not a published shutdown.
    """
    states = simulate(70.0, [0.0, 2.5, 5.0], [224.0, 0.0], [0.0, 224.0])
    assert [state.velocity_m_s for state in states] == pytest.approx([0, 8, 0])
    assert [state.position_m for state in states] == pytest.approx([0, 10, 20])


@pytest.mark.parametrize(
    "mass_kg,times_s,thrust_n,drag_n",
    [
        (0.0, [0.0, 1.0], [1.0], [0.0]),
        (-1.0, [0.0, 1.0], [1.0], [0.0]),
        (float("nan"), [0.0, 1.0], [1.0], [0.0]),
        (1.0, [0.0, 0.0], [1.0], [0.0]),
        (1.0, [1.0, 2.0], [1.0], [0.0]),
        (1.0, [0.0, 1.0], [], [0.0]),
        (1.0, [0.0, 1.0], [float("nan")], [0.0]),
        (1.0, [0.0, float("inf")], [1.0], [0.0]),
    ],
)
def test_drive_off_rejects_invalid_inputs(simulate, mass_kg, times_s, thrust_n, drag_n):
    """Reject nonphysical mass, ambiguous intervals and nonfinite inputs."""
    with pytest.raises(ValueError):
        simulate(mass_kg, times_s, thrust_n, drag_n)
