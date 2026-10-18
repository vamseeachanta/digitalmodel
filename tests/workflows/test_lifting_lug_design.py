"""Offline lifting-lug workflow checks without the solver-importing engine."""

import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.lifting_lug.workflow import design_load_kn, lug_checks, router


def test_lifting_lug_example_workflow(tmp_path):
    example = Path(__file__).resolve().parents[2] / "examples/workflows/lifting-lug-design/input.yml"
    cfg = yaml.safe_load(example.read_text())
    load = cfg["lifting_lug"]["load"]
    assert load["num_slings"] == 2
    assert load["skew_load_factor"] == 1.1
    assert "DNVGL-ST-E273" in load["skew_load_factor_source"]
    assert "April 2016" in load["skew_load_factor_source"]
    assert "3.5.5" in load["skew_load_factor_source"]
    cfg["lifting_lug"]["outputs"]["directory"] = str(tmp_path)
    result = router(cfg)["lifting_lug"]
    # P = 420 kN (already the most-loaded leg's static tension) * 2 * 1.1.
    assert result["design_load_kN"] == pytest.approx(924)
    assert result["governing_check"] == "shear_tearout"
    assert result["governing_utilization"] == pytest.approx(1.16197183)
    assert result["screening_status"] == "fail"
    summary = json.loads(Path(result["summary_json"]).read_text())
    assert summary["load"] == load
    assert summary["design_load_kN"] == result["design_load_kN"]


def test_lifting_lug_hand_calculation():
    p = design_load_kn(420, 2, 1.1)
    checks = {c.name: c for c in lug_checks(p, 40, 75, 80, 110, 355)}
    assert p == pytest.approx(924)
    assert checks["pin_bearing"].demand_mpa == pytest.approx(924000 / (75 * 40))
    assert checks["net_tension"].demand_mpa == pytest.approx(924000 / (2 * 40 * 70))
    assert checks["shear_tearout"].utilization == pytest.approx(165 / (0.4 * 355))


def _example_config(tmp_path):
    path = Path(__file__).resolve().parents[2] / "examples/workflows/lifting-lug-design/input.yml"
    cfg = yaml.safe_load(path.read_text())
    cfg["lifting_lug"]["outputs"]["directory"] = str(tmp_path)
    return cfg


def test_omitted_skew_factor_is_an_error(tmp_path):
    cfg = _example_config(tmp_path)
    del cfg["lifting_lug"]["load"]["skew_load_factor"]
    with pytest.raises(ValueError, match="skew_load_factor.*required"):
        router(cfg)
    assert not (tmp_path / "lifting_lug_summary.json").exists()


@pytest.mark.parametrize("legs,minimum", [(2, 1.1), (3, 1.1), (4, 1.33)])
def test_leg_count_floor_blocks_even_a_strong_lug(tmp_path, legs, minimum):
    cfg = _example_config(tmp_path)
    load = cfg["lifting_lug"]["load"]
    load.update(num_slings=legs, skew_load_factor=1.1 if legs == 4 else 1.0,
                static_load_kN=1.0)
    result = router(cfg)["lifting_lug"]
    # DNVGL-ST-E273, April 2016, §3.5.5: P = P_static * DAF * SKL.
    assert result["design_load_kN"] == pytest.approx(2 * minimum)
    assert all(check["passes"] for check in result["checks"])
    assert result["skew_load_factor_input"] == load["skew_load_factor"]
    assert result["skew_load_factor"] == minimum
    assert result["skew_load_factor_below_minimum"] is True
    assert result["screening_status"] == "fail"
    summary = json.loads(Path(result["summary_json"]).read_text())
    assert summary["skew_load_factor"] == minimum
    assert summary["skew_load_factor_below_minimum"] is True


@pytest.mark.parametrize("factor", [float("nan"), float("inf"), 0.9])
def test_invalid_skew_factor_fails_closed(tmp_path, factor):
    cfg = _example_config(tmp_path)
    cfg["lifting_lug"]["load"]["skew_load_factor"] = factor
    with pytest.raises(ValueError, match="skew_load_factor"):
        router(cfg)


@pytest.mark.parametrize("legs", [0, 5, 4.0, True, "4"])
def test_unsupported_leg_count_fails_closed(tmp_path, legs):
    cfg = _example_config(tmp_path)
    cfg["lifting_lug"]["load"]["num_slings"] = legs
    with pytest.raises(ValueError, match="num_slings"):
        router(cfg)
