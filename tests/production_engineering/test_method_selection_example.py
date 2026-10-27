# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise the published case cited in the method-selection usage page.

Basis: Production Engineering II, 2017-2018 course text, Problem (1-4),
pp. 40-41; equations (1.1) and (1.4), pp. 16-17. The independent hand
calculation is J = 250 / (3000 - 2500) = 0.5, q = 0.5 * 500 = 250 bopd.
"""

import runpy
import socket
from pathlib import Path


EXAMPLE = (
    Path(__file__).resolve().parents[2]
    / "examples/production_engineering/method_selection.py"
)


def test_method_selection_example_runs_offline(monkeypatch, capsys):
    """The documented command prints the decision and its evidence gaps offline."""
    def reject_network(*args, **kwargs):
        raise AssertionError("The example must not access the network")

    monkeypatch.setattr(socket, "create_connection", reject_network)
    monkeypatch.setattr(socket, "getaddrinfo", reject_network)
    monkeypatch.setattr(socket.socket, "connect", reject_network)
    monkeypatch.setattr(socket.socket, "connect_ex", reject_network)

    runpy.run_path(EXAMPLE.as_posix(), run_name="__main__")
    output = capsys.readouterr().out
    assert "Scenario: sourced teaching example, undersaturated single-phase oil" in output
    assert "Input basis: Production Engineering II (2017-2018), Problem (1-4)" in output
    assert "Pr = 3000.000 psig; Pwf = 2500.000 psig; Pb = 2130.000 psig" in output
    assert "Criterion: Pr >= Pwf >= Pb; no multirate test or significant skin" in output
    assert "Productivity index: 0.500 bopd/psi" in output
    assert "Selected method: linear" in output
    assert "at or above bubble point" in output
    assert "Flow rate: 250.0 bopd" in output
    assert "Qualification: NOT_EVALUATED" in output
    assert "Sources:" in output
    assert "Numerical applicability envelope: NOT_EVALUATED." in output
    assert "Parameter fit and field calibration: NOT_EVALUATED." in output
    assert "Original-paper worked-example validation: NOT_EVALUATED." in output


def test_example_linear_rate_matches_hand_calculation(capsys):
    """Check the sourced inputs, drawdown, index and rate independently."""
    example = runpy.run_path(EXAMPLE.as_posix())
    assert capsys.readouterr().out == ""
    result = example["run_example"]()

    # Course text, Problem (1-4), pp. 40-41; all pressures share a gauge basis.
    assert result.reservoir.reservoir_pressure_psi == 3000.0
    assert result.reservoir.bubble_point_psi == 2130.0
    assert result.flowing_pressure_psi == 2500.0
    # Hand calculation: drawdown = 3000 - 2500 = 500 psi (Eq. 1.1, p. 16).
    drawdown = result.reservoir.reservoir_pressure_psi - result.flowing_pressure_psi
    assert drawdown == 500.0
    # Hand calculation: J = 250 / 500 = 0.5 bopd/psi (Eq. 1.1, p. 16).
    assert result.reservoir.productivity_index_bopd_psi == 0.5
    # Hand calculation: q = 0.5 * 500 = 250 bopd (Eq. 1.4, p. 17).
    assert result.flow_rate_bopd == 250.0
    # Documented policy: the entire interval exceeds bubble point, no multirate.
    assert result.assessment.method == "linear"
    assert result.assessment.qualification_status == "NOT_EVALUATED"
    assert result.assessment.rationale
    assert result.assessment.sources
    assert result.assessment.open_checks
