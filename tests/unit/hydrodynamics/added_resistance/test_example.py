# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent arithmetic for the synthetic two-state tanker-form illustration."""

import math
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.added_resistance.example import calculate, render_report


def test_two_state_worked_values():
    """Chosen rho=1000, g=10, B=Lbow=32 yields C=20000 N/m² (G-1).

    At Hs=1,2 the forces are 20000 and 80000 N. With U0=10, eta=0.5,
    added shaft power is 400000 and 1600000 W.
    The calm-water table contains R=1000*U² at U=[0,2,4,6,8,10].
    Available effective power=1000000 W. For the first state the root lies
    in [8,10], where linear R0=18000*U-80000; solve
    18000*U²-60000*U-1000000=0. For the second, root lies in [6,8],
    R0=14000*U-48000, so solve 14000*U²+32000*U-1000000=0.
    These are independent quadratic roots of the supplied interpolation law.
    """
    result = calculate()
    first, second = result["sea_states"]
    expected_speeds = [
        (60000 + math.sqrt(60000**2 + 4 * 18000 * 1000000)) / 36000,
        (-32000 + math.sqrt(32000**2 + 4 * 14000 * 1000000)) / 28000,
    ]
    for row, force, power, speed in zip(
        [first, second], [20000, 80000], [400000, 1600000], expected_speeds
    ):
        assert row["reflection_n"] == pytest.approx(force)
        assert row["spectral_reflection_n"] == pytest.approx(force)
        assert row["added_shaft_power_w"] == pytest.approx(power)
        assert row["reduced_speed_m_s"] == pytest.approx(speed, abs=1e-7)
        assert row["radiated_energy_status"] == "NOT_EVALUATED"
        assert row["radiated_energy_n"] is None
        assert len(row["regular_table"]) == 3


def test_report_is_reproducible_and_limits_are_visible():
    root = Path(__file__).resolve().parents[4]
    report = root / "docs/domains/hydrodynamics/added-resistance-analysis.md"
    assert render_report(calculate()) == report.read_text()
    text = report.read_text()
    assert (
        text.index("## Summary")
        < text.index("## Revision history")
        < text.index("## Inputs")
    )
    for term in [
        "NOT_EVALUATED",
        "assumed, based on previous experience",
        "constant spectral equivalent",
        "#2613",
        "Later slices",
    ]:
        assert term in text


def test_report_rejects_unqualified_case():
    """The example never replaces a failed gate with a zero resistance."""
    with pytest.raises(ValueError, match="NOT_EVALUATED"):
        calculate(short_waves_confirmed=False)
