"""Offline smoke test for the combined wave + VIV riser-fatigue workflow.

Runs the dispatchable example through the digitalmodel engine
(``uv run python -m digitalmodel examples/workflows/riser-fatigue/input.yml``)
with no OrcaFlex / no licence / no network, and asserts a sane fatigue life,
non-zero wave AND VIV damage, and a consistent governing-segment verdict.
"""

import math
from pathlib import Path

import pytest

from digitalmodel.engine import engine
from digitalmodel.riser_fatigue.workflow import router

REPO_ROOT = Path(__file__).resolve().parents[2]
EXAMPLE_INPUT = REPO_ROOT / "examples" / "workflows" / "riser-fatigue" / "input.yml"


def test_riser_fatigue_example_runs_offline():
    cfg = engine(inputfile=str(EXAMPLE_INPUT))

    assert isinstance(cfg, dict)
    assert cfg["basename"] == "riser_fatigue"

    result = cfg["riser_fatigue"]
    # Two segments defined in the example.
    assert len(result["segments"]) == 2

    # Governing (worst) segment drives the top-level verdict.
    gov_damage = result["governing_damage"]
    assert 0.0 < gov_damage < 1.0  # sane: damaged but below failure
    assert result["governing_wave_damage"] > 0.0
    assert result["governing_viv_damage"] > 0.0
    assert result["governing_damage"] == pytest.approx(
        result["governing_wave_damage"] + result["governing_viv_damage"]
    )

    # Finite, sane fatigue life and a DFF check consistent with the damage.
    life = result["governing_fatigue_life_years"]
    assert math.isfinite(life)
    assert life > result["design_life_years"]
    expected_status = "pass" if result["governing_dff_margin"] >= 1.0 else "fail"
    assert result["screening_status"] == expected_status

    # Outputs are written next to the example.
    results_dir = EXAMPLE_INPUT.parent / "results"
    assert (results_dir / "input_riser_fatigue.csv").exists()
    assert (results_dir / "input_riser_fatigue_summary.csv").exists()


def test_riser_fatigue_router_sums_wave_and_viv():
    """A single VIV-only and a single wave-only segment isolate each path."""
    cfg = {
        "basename": "riser_fatigue",
        "_config_dir_path": str(Path(__file__).parent),
        "riser_fatigue": {
            "design_life_years": 25.0,
            "dff": 3.0,
            "sn_curve": {"curve": "F1", "environment": "seawater_cp"},
            "output_dir": "_riser_fatigue_unit_results",
            "segments": [
                {
                    "id": "wave-only",
                    "outer_diameter_mm": 273.1,
                    "wall_thickness_mm": 22.2,
                    "wave": {
                        "scf": 1.0,
                        "histogram_period_years": 1.0,
                        "stress_ranges_MPa": [10.0, 20.0],
                        "cycles": [1.0e6, 5.0e4],
                    },
                },
                {
                    "id": "viv-only",
                    "outer_diameter_mm": 273.1,
                    "wall_thickness_mm": 22.2,
                    "viv_cases": [
                        {
                            "stress_range_MPa": 15.0,
                            "frequency_hz": 0.5,
                            "exposure_fraction": 0.05,
                        }
                    ],
                },
            ],
        },
    }

    out = router(cfg)["riser_fatigue"]
    by_id = {seg["segment_id"]: seg for seg in out["segments"]}

    assert by_id["wave-only"]["wave_damage"] > 0.0
    assert by_id["wave-only"]["viv_damage"] == 0.0
    assert by_id["viv-only"]["viv_damage"] > 0.0
    assert by_id["viv-only"]["wave_damage"] == 0.0

    for seg in out["segments"]:
        assert seg["total_damage"] == pytest.approx(
            seg["wave_damage"] + seg["viv_damage"]
        )

    # Clean up the unit-test output directory.
    out_dir = Path(__file__).parent / "_riser_fatigue_unit_results"
    for child in out_dir.glob("*"):
        child.unlink()
    out_dir.rmdir()
