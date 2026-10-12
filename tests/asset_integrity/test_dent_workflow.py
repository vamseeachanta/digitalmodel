"""Manual dent geometry workflow and shared decision contract."""
import math
from pathlib import Path

import pytest

from digitalmodel.asset_integrity import dent_assessment


def dent_cfg(**kwargs):
    return {"basename": "api579_dent_screen", "dent_assessment": {
        "component_id": "SYNTHETIC-DENT", "od_in": 24.0, "wt_in": 0.5,
        "dent_depth_in": 1.2, "dent_length_axial_in": 12.0,
        "dent_length_circ_in": 10.0, "on_weld": False,
        "restrained": False, "with_gouge_or_metal_loss": False, **kwargs}}


def test_strain_golden_and_report(tmp_path):
    cfg = dent_cfg(output_dir=str(tmp_path))
    result = dent_assessment.router(cfg)["api579_dent_screen"]
    x = 0.25 * (1 / 12 + 8 * 1.2 / 100)
    y, membrane = 0.25 * 8 * 1.2 / 144, 0.5 * (1.2 / 12) ** 2
    expected = max(math.sqrt(x*x-x*(y+membrane)+(y+membrane)**2),
                   math.sqrt(x*x+x*(membrane-y)+(membrane-y)**2))
    assert result["assessment"]["strain"]["eps_max"] == pytest.approx(expected)
    assert result["decision"]["verdict"] == "ACCEPT"
    report = result["report_html"]
    assert "Appendix R" in report
    assert "manual" in report.lower()
    assert "not evaluated" in report.lower()
    assert "Remaining Strength Factor" not in report
    assert Path(result["report_path"]).read_text() == report


@pytest.mark.parametrize("params,verdict", [
    ({"with_gouge_or_metal_loss": True}, "ESCALATE"),
    ({"restrained": True}, "MONITOR"),
    ({"on_weld": True}, "REPAIR"),
    ({"dent_length_axial_in": 1.0}, "ESCALATE"),
])
def test_shared_decision_preserves_engine_limits(params, verdict):
    result = dent_assessment.router(dent_cfg(**params))["api579_dent_screen"]
    assert result["decision"]["verdict"] == verdict


@pytest.mark.parametrize("params", [
    {"od_in": float("nan")}, {"strain_limit": 0.0},
    {"dent_length_axial_in": None}, {"on_weld": "false"},
])
def test_invalid_or_incomplete_geometry_rejected(params):
    with pytest.raises(ValueError):
        dent_assessment.router(dent_cfg(**params))


@pytest.mark.parametrize("flag", ["on_weld", "restrained", "with_gouge_or_metal_loss"])
def test_unknown_interacting_features_rejected(flag):
    cfg = dent_cfg()
    del cfg["dent_assessment"][flag]
    with pytest.raises(ValueError):
        dent_assessment.router(cfg)


@pytest.mark.parametrize("parameter", ["plain_dent_depth_limit", "weld_dent_depth_limit", "strain_limit"])
def test_workflow_cannot_relax_dent_screening_limits(parameter):
    with pytest.raises(ValueError):
        dent_assessment.router(dent_cfg(**{parameter: 0.99}))


def test_weld_depth_rejection_keeps_repair_disposition_for_sharp_profile():
    result = dent_assessment.router(dent_cfg(on_weld=True, dent_length_axial_in=1.0))["api579_dent_screen"]
    assert result["decision"]["verdict"] == "REPAIR"
