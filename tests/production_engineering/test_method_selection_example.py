# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise the synthetic case defined in the method-selection usage page.

Basis: docs/domains/production/method-selection-example.md, Synthetic inputs
and Hand calculation. All pressures and the productivity index are illustrative.
The expected rate is the independent product 2 * (3000 - 2500) = 1000 bopd.
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

    runpy.run_path(str(EXAMPLE), run_name="__main__")
    output = capsys.readouterr().out
    assert "Scenario: synthetic undersaturated, single-phase oil" in output
    assert "Selected method: linear" in output
    assert "at or above bubble point" in output
    assert "Flow rate: 1000.0 bopd" in output
    assert "Qualification: NOT_EVALUATED" in output
    assert "Sources:" in output
    assert "Numerical applicability envelope: NOT_EVALUATED." in output
    assert "Parameter fit and field calibration: NOT_EVALUATED." in output
    assert "Original-paper worked-example validation: NOT_EVALUATED." in output


def test_example_linear_rate_matches_hand_calculation(capsys):
    """Check returned inputs and q=2*(3000-2500)=1000 without model expectations."""
    example = runpy.run_path(str(EXAMPLE))
    assert capsys.readouterr().out == ""
    result = example["run_example"]()

    # Independent synthetic inputs and hand product: module docstring and page.
    assert result.reservoir.reservoir_pressure_psi == 3000.0
    assert result.reservoir.bubble_point_psi == 2000.0
    assert result.reservoir.productivity_index_bopd_psi == 2.0
    assert result.flowing_pressure_psi == 2500.0
    assert result.flow_rate_bopd == 2 * (3000 - 2500) == 1000
    # Documented policy: the entire interval exceeds bubble point, no multirate.
    assert result.assessment.method == "linear"
    assert result.assessment.qualification_status == "NOT_EVALUATED"
    assert result.assessment.rationale
    assert result.assessment.sources
    assert result.assessment.open_checks
