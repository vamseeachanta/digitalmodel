"""Offline smoke tests for the J-lay / reel-lay pipelay workflows (#958).

These run the dispatchable templates through the engine end-to-end. The whole
workflow is analytical (no OrcFxAPI / OrcaFlex license), so the tests run
offline in CI.
"""

import json
import math
from pathlib import Path

import pytest
import yaml

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


def _settings(path):
    return yaml.safe_load(path.read_text(encoding='utf-8'))['pipelay']


def _run_template(path, tmp_path):
    cfg = yaml.safe_load(path.read_text(encoding='utf-8'))
    cfg['pipelay']['outputs']['directory'] = str(tmp_path)
    input_path = tmp_path / 'input.yml'
    input_path.write_text(yaml.safe_dump(cfg), encoding='utf-8')
    return engine(inputfile=str(input_path))


def test_jlay_template_runs_offline(tmp_path):
    cfg = _run_template(JLAY_INPUT, tmp_path)

    assert cfg["basename"] == "pipelay"
    result = cfg["pipelay"]
    assert result["method"] == "J-Lay"
    assert result["screening_status"] == "pass"

    sagbend = result["sagbend"]
    # Independent E1-E5 anchors in pipelay/docs/screening-basis.md.
    assert result['pipe_submerged_weight_N_per_m'] == pytest.approx(1658.33, rel=1e-3)
    assert result['top_tension_kN'] == pytest.approx(2624.9, rel=1e-3)
    assert sagbend['catenary_parameter_m'] == pytest.approx(82.8394, rel=1e-3)
    assert sagbend['sagbend_bending_strain'] == pytest.approx(0.00195438, rel=1e-3)
    assert sagbend['horizontal_offset_m'] == pytest.approx(301.7453, rel=1e-3)
    assert sagbend['suspended_length_m'] == pytest.approx(1580.6702, rel=1e-3)
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


def test_reel_lay_template_runs_offline(tmp_path):
    cfg = _run_template(REEL_INPUT, tmp_path)

    assert cfg["basename"] == "pipelay"
    result = cfg["pipelay"]
    assert result["method"] == "Reel-Lay"
    assert result["screening_status"] == "pass"

    # reeling strain = D / (2 * R_reel)
    inputs = _settings(REEL_INPUT)
    expected_strain = inputs['pipe']['outer_diameter'] / (2 * inputs['config']['reel_core_radius_m'])
    # Independent E6 anchors, including the residual branch.
    assert result['reeling_bending_strain'] == pytest.approx(0.017069, rel=1e-3)
    assert result['residual_curvature_1_per_m'] == pytest.approx(0.00625, rel=1e-3)
    assert result['residual_radius_m'] == pytest.approx(160.0, rel=1e-3)
    assert result['residual_bending_strain'] == pytest.approx(0.000853, rel=1e-3)
    assert result['residual_out_of_straightness_m'] == pytest.approx(0.1125, rel=1e-3)
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
    settings = _settings(REEL_INPUT)
    settings['config']['reel_core_radius_m'] = 3.0
    settings['pipe'].update(outer_diameter=0.3238, wall_thickness=0.0206)
    result = run_reel_lay(settings)
    assert result["reeling_strain_utilisation"] > 1.0
    assert result["screening_status"] == "fail"


def test_router_rejects_unknown_method():
    with pytest.raises(ValueError, match="Unknown pipelay method"):
        router({"pipelay": {"method": "Tow-Lay"}})


def test_jlay_higher_tension_reduces_sagbend_strain():
    # higher top tension -> larger catenary radius -> lower sagbend strain
    settings = _settings(JLAY_INPUT)
    low = run_jlay(settings)
    # E2 requires the angle to vary too: 3000 kN at this depth gives 9.8362 deg.
    settings['config'].update(top_tension_kN=3000.0, tower_angle_deg=9.8362)
    high = run_jlay(settings)
    assert (
        high["sagbend"]["sagbend_bending_strain"]
        < low["sagbend"]["sagbend_bending_strain"]
    )
