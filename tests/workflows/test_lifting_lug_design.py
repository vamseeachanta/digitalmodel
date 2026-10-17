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
