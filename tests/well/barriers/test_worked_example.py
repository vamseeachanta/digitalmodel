"""Executable synthetic P&A and identical-anomaly comparison."""

import runpy
from pathlib import Path

import pytest


def test_documented_example_numbers():
    root = Path(__file__).resolve().parents[3]
    example = runpy.run_path(str(root / "examples/well/barrier_screening.py"))
    result = example["worked_example"]()
    assert result["permanent_barrier_screen"]["meets_screen"]
    rows = result["ranked_wells"]
    assert [r["source_id"] for r in rows] == ["hp_sour_gas", "lp_water_injector"]
    # Same anomaly: first closed valve is confirmed failed, q_A=1.
    # Synthetic rate band [4, 5, 6]e-7 /h and interval 800 h:
    # q_B=[.00016, .0002, .00024]; AND=1*q_B for either well.
    # Gas weight=3*5*10=150; water weight=1; no physical loss units.
    assert rows[0]["probability"]["nominal"] == pytest.approx(.0002)
    assert rows[1]["probability"] == rows[0]["probability"]
    assert rows[0]["risk"]["nominal"] == pytest.approx(.03)
    assert rows[1]["risk"]["nominal"] == pytest.approx(.0002)
    assert rows[0]["risk"]["low"] == pytest.approx(.024)
    assert rows[0]["risk"]["high"] == pytest.approx(.036)
    assert rows[1]["risk"]["low"] == pytest.approx(.00016)
    assert rows[1]["risk"]["high"] == pytest.approx(.00024)
    assert result["failure_events"][1]["rate"]["source_record"] is None
    assert result["permanent_inputs"]["zones"][0]["criteria"]["basis"].startswith("Synthetic")
