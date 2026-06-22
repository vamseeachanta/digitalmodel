"""Offline smoke tests for the J-lay / reel-lay pipelay workflows (#958).

These run the dispatchable templates through the engine end-to-end. The whole
workflow is analytical (no OrcFxAPI / OrcaFlex license), so the tests run
offline in CI.
"""

import json
import math
from pathlib import Path

import pytest

from digitalmodel.engine import engine
from digitalmodel.pipelay.workflow import run_jlay, run_reel_lay, router

REPO_ROOT = Path(__file__).resolve().parents[2]
JLAY_INPUT = REPO_ROOT / "examples/workflows/jlay-pipelay/input.yml"
REEL_INPUT = REPO_ROOT / "examples/workflows/reel-lay-pipelay/input.yml"


def _summary(cfg):
    summary_path = Path(cfg["pipelay"]["summary_json"])
    if not summary_path.is_absolute():
        summary_path = REPO_ROOT / summary_path
    return json.loads(summary_path.read_text())


def test_jlay_template_runs_offline():
    cfg = engine(inputfile=str(JLAY_INPUT))

    assert cfg["basename"] == "pipelay"
    result = cfg["pipelay"]
    assert result["method"] == "J-Lay"
    assert result["screening_status"] == "pass"

    sagbend = result["sagbend"]
    # near-vertical departure: a positive, finite catenary parameter / radius
    assert sagbend["catenary_parameter_m"] > 0.0
    assert sagbend["sagbend_radius_m"] == pytest.approx(sagbend["catenary_parameter_m"])
    assert math.isfinite(sagbend["sagbend_radius_m"])
    # suspended length must exceed the water depth (slant >= vertical)
    assert sagbend["suspended_length_m"] > result["water_depth_m"]

    # utilisation is consistent with the reported strain and limit, and passes
    util = result["sagbend_strain_utilisation"]
    assert util == pytest.approx(
        sagbend["sagbend_bending_strain"] / result["allowable_sagbend_strain"],
        rel=1e-3,
    )
    assert util <= 1.0

    # operability table is monotonically non-increasing and cut off above Hs limit
    rates = [
        row["lay_rate_km_per_day"] for row in result["operability"]["lay_rate_table"]
    ]
    assert rates[0] > 0.0
    assert rates[-1] == 0.0
    in_band = [r for r in rates if r > 0.0]
    assert all(b >= a for a, b in zip(in_band[1:], in_band[:-1]))

    summary = _summary(cfg)
    assert summary["screening_status"] == "pass"
    assert summary["method"] == "J-Lay"


def test_reel_lay_template_runs_offline():
    cfg = engine(inputfile=str(REEL_INPUT))

    assert cfg["basename"] == "pipelay"
    result = cfg["pipelay"]
    assert result["method"] == "Reel-Lay"
    assert result["screening_status"] == "pass"

    # reeling strain = D / (2 * R_reel)
    expected_strain = result["pipe_outer_diameter_m"] / (
        2.0 * result["reel_core_radius_m"]
    )
    assert result["reeling_bending_strain"] == pytest.approx(expected_strain, rel=1e-4)
    assert result["reeling_strain_utilisation"] == pytest.approx(
        expected_strain / result["allowable_reeling_strain"], rel=1e-3
    )
    assert result["reeling_strain_utilisation"] <= 1.0

    # straightener removes most curvature -> residual << reeling curvature
    assert 0.0 < result["residual_bending_strain"] < result["reeling_bending_strain"]
    assert result["residual_radius_m"] > result["reel_core_radius_m"]
    assert result["residual_out_of_straightness_m"] > 0.0

    summary = _summary(cfg)
    assert summary["method"] == "Reel-Lay"
    assert summary["screening_status"] == "pass"


def test_reeling_strain_exceeding_limit_fails():
    # a tight reel core drives the reeling strain over the plastic-strain limit
    result = run_reel_lay(
        {
            "config": {
                "reel_core_radius_m": 3.0,
                "allowable_reeling_strain": 0.02,
            },
            "pipe": {"outer_diameter": 0.3238, "wall_thickness": 0.0206},
        }
    )
    assert result["reeling_strain_utilisation"] > 1.0
    assert result["screening_status"] == "fail"


def test_router_rejects_unknown_method():
    with pytest.raises(ValueError, match="Unknown pipelay method"):
        router({"pipelay": {"method": "Tow-Lay"}})


def test_jlay_higher_tension_reduces_sagbend_strain():
    # higher top tension -> larger catenary radius -> lower sagbend strain
    low = run_jlay({"config": {"top_tension_kN": 1500.0, "tower_angle_deg": 3.0}})
    high = run_jlay({"config": {"top_tension_kN": 3000.0, "tower_angle_deg": 3.0}})
    assert (
        high["sagbend"]["sagbend_bending_strain"]
        < low["sagbend"]["sagbend_bending_strain"]
    )
