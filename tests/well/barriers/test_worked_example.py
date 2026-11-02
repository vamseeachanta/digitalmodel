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
    # Remaining generic valve: lambda=2.3e-6*0.15=3.45e-7 /h.
    # q_B=3.45e-7*1000/2=0.0001725; AND=1*q_B for either well.
    # Gas weight=3*5*10=150; water weight=1; no physical loss units.
    # Scores=.0001725*150=.025875 and .0001725*1=.0001725.
    # Sensitivity bounds: [2.1,2.6]e-6*.15*1000/2=[.0001575,.000195].
    assert rows[0]["probability"]["nominal"] == pytest.approx(.0001725)
    assert rows[1]["probability"] == rows[0]["probability"]
    assert rows[0]["risk"]["nominal"] == pytest.approx(.025875)
    assert rows[1]["risk"]["nominal"] == pytest.approx(.0001725)
    assert rows[0]["risk"]["low"] == pytest.approx(.023625)
    assert rows[0]["risk"]["high"] == pytest.approx(.02925)
