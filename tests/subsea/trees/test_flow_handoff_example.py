# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the example README's Synthetic inputs and Hand calculation sections."""

import importlib.util
import json
import math
import os
from pathlib import Path
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[3]
EXAMPLE = ROOT / "docs/reports/modules/trees/examples/flow_handoff.py"


def load_example():
    """Load the documented script without executing its command entry point."""
    spec = importlib.util.spec_from_file_location("flow_handoff_example", EXAMPLE)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_flow_handoff_example_rebuilds_hand_values():
    """Check README hand arithmetic independently of the production helpers."""
    result = load_example().build_example()
    # README Hand calculation: sqrt(400)=20; 60/20=3 m/s. Circular areas are
    # pi*(1/4)^2/4=pi/64 and pi*(1/8)^2/4=pi/256 m2.
    # Multiplication by 3 gives 3*pi/64 and 3*pi/256 m3/s.
    for port, expected_area, expected_rate in (
        ("production", math.pi / 64, 3 * math.pi / 64),
        ("annulus", math.pi / 256, 3 * math.pi / 256),
    ):
        capacity = result[f"{port}_capacity"]
        assert capacity["velocity_m_s"] == 3
        assert capacity["rate_m3_s"] == pytest.approx(expected_rate)
        assert capacity["rate_m3_s"] / capacity["velocity_m_s"] == pytest.approx(
            expected_area
        )
    # README Synthetic inputs: preserve the separate 0.1 and 0.02 m3/s rates;
    # 2,000,000 - 250,000 = 1,750,000 Pa, with both rates below their ceilings.
    assert result["handoff"] == {
        "production_rate_m3_s": 0.1,
        "annulus_rate_m3_s": 0.02,
        "downstream_pressure_pa": 1_750_000.0,
        "within_capacity": True,
    }


@pytest.mark.parametrize(
    ("port", "ceiling"),
    [("production", 3 * math.pi / 64), ("annulus", 3 * math.pi / 256)],
)
def test_example_port_limits_are_inclusive(port, ceiling):
    """Use hand-calculated ceilings and the next float toward positive infinity."""
    example = load_example()
    # The untouched port stays at its README operating rate, below its ceiling.
    rate_key = f"{port}_rate_m3_s"
    other_key = (
        "annulus_rate_m3_s" if port == "production" else "production_rate_m3_s"
    )
    other_rate = 0.02 if port == "production" else 0.1
    at_limit = example.build_example(**{rate_key: ceiling})
    assert at_limit["handoff"]["within_capacity"] is True
    assert at_limit["handoff"][rate_key] == ceiling
    above = math.nextafter(ceiling, math.inf)
    assert above > ceiling
    exceeded = example.build_example(**{rate_key: above})
    assert exceeded["handoff"]["within_capacity"] is False
    assert exceeded["handoff"][rate_key] == above
    assert at_limit["handoff"][other_key] == other_rate
    assert exceeded["handoff"][other_key] == other_rate
    assert exceeded["handoff"]["downstream_pressure_pa"] == 1_750_000.0


def test_example_import_is_inert(tmp_path, monkeypatch, capsys):
    """Import must neither run a calculation nor print or create output files."""
    from digitalmodel.subsea.trees import flow_capacity

    def unexpected_call(*args, **kwargs):
        pytest.fail("calculation executed during import")

    monkeypatch.setattr(flow_capacity, "bore_capacity", unexpected_call)
    monkeypatch.setattr(flow_capacity, "flow_handoff", unexpected_call)
    monkeypatch.chdir(tmp_path)
    load_example()
    captured = capsys.readouterr()
    assert captured.out == ""
    assert captured.err == ""
    assert list(tmp_path.iterdir()) == []


def test_example_cli_prints_deterministic_strict_json(tmp_path):
    """The command writes only stdout, with the same bytes on repeated runs."""
    env = {**os.environ, "PYTHONPATH": str(ROOT / "src")}
    command = [sys.executable, str(EXAMPLE)]
    first = subprocess.run(
        command, cwd=tmp_path, env=env, check=True, capture_output=True, text=True
    )
    second = subprocess.run(
        command, cwd=tmp_path, env=env, check=True, capture_output=True, text=True
    )

    def reject_nonfinite(value):
        raise ValueError(f"non-finite JSON constant: {value}")

    assert first.stdout == second.stdout
    assert first.stderr == second.stderr == ""
    # README Hand calculation supplies this independent JSON contract too.
    assert json.loads(first.stdout, parse_constant=reject_nonfinite) == {
        "production_capacity": {
            "velocity_m_s": 3.0,
            "rate_m3_s": 3 * math.pi / 64,
        },
        "annulus_capacity": {
            "velocity_m_s": 3.0,
            "rate_m3_s": 3 * math.pi / 256,
        },
        "handoff": {
            "production_rate_m3_s": 0.1,
            "annulus_rate_m3_s": 0.02,
            "downstream_pressure_pa": 1_750_000.0,
            "within_capacity": True,
        },
    }
    assert list(tmp_path.iterdir()) == []
