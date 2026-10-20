# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Plate workflow integration with independent arithmetic, not licensed goldens."""

import copy
import json
import math
import sys
from pathlib import Path

import pytest
import yaml

from digitalmodel.asset_integrity.assessment.crack_fad import critical_flaw_depth_mm
from digitalmodel.asset_integrity.assessment.crack_fad_workflow import router
from digitalmodel.asset_integrity.assessment.level3_escalation import escalate_to_level3

ROOT = Path(__file__).resolve().parents[2]


def case():
    """Synthetic test inputs, chosen for the arithmetic written below."""
    return {
        "basename": "crack_fad_level2_handoff",
        "crack_assessment": {
            "component_id": "PLATE",
            "geometry": "plate_surface",
            "a_mm": 5.0,
            "c_mm": 5.0,
            "t_mm": 20.0,
            "width_mm": 50.0,
            "sigma_m_mpa": 100.0,
            "sigma_b_mpa": 0.0,
            "sigma_y_mpa": 400.0,
            "sigma_u_mpa": 600.0,
            "e_mpa": 200000.0,
            "k_mat": 100.0,
            "max_depth_fraction": 0.8,
            "tol_mm": 0.001,
        },
    }


def test_plate_point_matches_independent_arithmetic():
    """Hand calculation using the repo's documented equations.

    BS 7910:2013 Annex P: alpha=(5/20)/(1+20/5)=0.05;
    sigma_ref=100/0.95; Lr=(100/0.95)/400=5/19; cutoff=1000/800.
    1981 surface-plate solution, a/c=1, a/t=1/4:
    Q=2.464, M1=1.04, M2=-0.54+0.89/1.2, M3=0.5-1/1.65.
    At the deepest point g=f_phi=1; fw=sec(pi/40)^0.5.
    K=100*(M1+M2/16+M3/256)*fw*sqrt(pi*0.005/Q).
    At the surface g=1.121875 and f_phi=1; that K governs.
    BS 7910:2013 Cl 7.3.2: mu=0.5 and f(5/19) is evaluated below.
    Constants are transcribed from crack_fad.py's cited equations; this
    validates arithmetic and wiring, not independent published-case fidelity.
    """
    result = router(case())["crack_fad_level2_handoff"]
    point = result["assessment"]
    deep = 100 * (1.04 + (-0.54 + 0.89 / 1.2) / 16 + (0.5 - 1 / 1.65) / 256)
    deep *= math.sqrt(1 / math.cos(math.pi / 40)) * math.sqrt(math.pi * 0.005 / 2.464)
    expected_k = deep * 1.121875
    lr = 5 / 19
    expected_f = (1 + 0.5 * lr**2) ** -0.5 * (0.3 + 0.7 * math.exp(-0.5 * lr**6))
    assert point["sigma_ref_mpa"] == pytest.approx(100 / 0.95)
    assert point["lr"] == pytest.approx(lr)
    assert point["lr_max"] == pytest.approx(1.25)
    assert point["k_applied"] == pytest.approx(expected_k)
    assert point["kr"] == pytest.approx(expected_k / 100)
    assert point["f_lr"] == pytest.approx(expected_f)
    assert point["acceptable"] is True
    assert result["decision"]["verdict"] == "ESCALATE"
    assert result["critical_depth"]["status"] == "SEARCH_BOUND_REACHED"


def test_collapse_controlled_critical_depth_is_eight_mm():
    """Hand solution, BS 7910:2013 Cl 7.3.2 cutoff and Annex P.

    At c=t=20 mm and Pm=sy=400 MPa, alpha=a/40 and Lr=1/(1-a/40).
    With su=600 MPa, cutoff=1.25: a=40*(1-1/1.25)=8 mm.
    Synthetic Kmat=1e6 makes fracture non-governing throughout this search.
    """
    cfg = case()
    cfg["crack_assessment"].update(
        c_mm=20.0, width_mm=100.0, sigma_m_mpa=400.0, k_mat=1e6
    )
    result = router(cfg)["crack_fad_level2_handoff"]
    assert result["critical_depth"]["depth_mm"] == pytest.approx(8.0, abs=0.001)
    assert result["critical_depth"]["status"] == "BRACKETED_BOUNDARY"


def test_critical_depth_preserves_finite_width():
    """Regression: finite-width amplification must enter the depth search."""
    args = dict(
        length_mm=40.0,
        t_mm=20.0,
        sigma_m_mpa=100.0,
        sigma_y_mpa=400.0,
        sigma_u_mpa=600.0,
        k_mat=20.0,
        max_depth_fraction=0.8,
        tol_mm=0.001,
    )
    narrow = critical_flaw_depth_mm(width_mm=41.0, **args)
    wide = critical_flaw_depth_mm(width_mm=10000.0, **args)
    assert 0 < narrow < wide


def test_crack_handoff_does_not_invent_metal_loss_rsf():
    handoff = escalate_to_level3({}, {}, {}, {}, {}, has_crack_like_flaw=True)
    assert handoff.triggered
    assert not any(
        "RSF" in reason or "metal loss" in reason and "severe" in reason
        for reason in handoff.triggers
    )
    assert handoff.open_questions


def test_report_and_handoff_round_trip(tmp_path):
    cfg = case()
    cfg["crack_assessment"].update(
        component_id="<script>bad</script>", output_dir=str(tmp_path)
    )
    result = router(cfg)["crack_fad_level2_handoff"]
    text = Path(result["report_path"]).read_text()
    package = json.loads(Path(result["handoff_path"]).read_text())
    assert text == result["report_html"]
    assert package == result["handoff"]
    assert package["data_package"]["level2_result"]["curve"] == "BS 7910:2013 Option 1"
    assert "BS 7910:2013 Option 1" in text
    assert "Open questions" in text
    assert "Revision history" in text
    assert "NOT_EVALUATED" in text
    assert "<script>" not in text
    assert "&lt;script&gt;" in text
    assert "cdn." not in text
    assert "rsf" not in package["data_package"]["level2_result"]
    assert "fatigue" in text.lower()
    json.dumps(result, allow_nan=False)


@pytest.mark.parametrize(
    "key,value",
    [
        ("k_mat", None),
        ("k_mat", 0),
        ("sigma_y_mpa", float("nan")),
        ("sigma_u_mpa", float("inf")),
        ("sigma_u_mpa", 399),
        ("e_mpa", True),
        ("a_mm", 0),
        ("a_mm", 20),
        ("c_mm", 4),
        ("width_mm", 10),
        ("sigma_m_mpa", -1),
        ("sigma_b_mpa", -1),
        ("max_depth_fraction", 1),
        ("tol_mm", 3),
        ("geometry", "cylinder"),
        ("cvn_joules", 40),
        ("curve", "api579"),
        ("interacting_defects", "false"),
        ("unknown_input", 1),
    ],
)
def test_invalid_or_unsupported_input_rejected(key, value):
    cfg = case()
    cfg["crack_assessment"][key] = value
    with pytest.raises(ValueError):
        router(cfg)


def test_missing_required_input_rejected():
    cfg = case()
    del cfg["crack_assessment"]["e_mpa"]
    with pytest.raises(ValueError, match="e_mpa"):
        router(cfg)


def test_result_does_not_alias_input():
    cfg = case()
    before = copy.deepcopy(cfg["crack_assessment"])
    result = router(cfg)["crack_fad_level2_handoff"]
    cfg["crack_assessment"]["a_mm"] = 1
    assert result["handoff"]["data_package"]["geometry"]["a_mm"] == before["a_mm"]


def test_outside_envelope_and_no_passing_depth_still_handoff():
    """At Pm=800 > flow=500 MPa, the collapse cutoff is exceeded at any depth.

    BS 7910:2013 Clause 7.3.2 and Annex P: Lr >= Pm/sy = 2 > 1.25.
    """
    cfg = case()
    cfg["crack_assessment"].update(
        sigma_m_mpa=800.0, interacting_defects=True, complex_geometry=True
    )
    result = router(cfg)["crack_fad_level2_handoff"]
    assert result["assessment"]["acceptable"] is False
    assert result["critical_depth"]["depth_mm"] == 0
    assert result["critical_depth"]["status"] == "NO_ACCEPTABLE_DEPTH_FOUND"
    assert result["decision"]["verdict"] == "ESCALATE"
    triggers = " ".join(result["handoff"]["triggers"]).lower()
    assert "interacting" in triggers
    assert "complex geometry" in triggers
    assert "severe metal loss" not in triggers


def test_relative_output_resolves_from_input_folder(tmp_path):
    cfg = case()
    cfg["_config_dir_path"] = str(tmp_path)
    cfg["crack_assessment"]["output_dir"] = "results"
    result = router(cfg)["crack_fad_level2_handoff"]
    assert Path(result["report_path"]).parent == tmp_path / "results"
    assert Path(result["handoff_path"]).parent == tmp_path / "results"


def test_registered_example_runs_through_engine(tmp_path, monkeypatch):
    """Exercise the real engine route with every output redirected to scratch."""
    # The engine imports optional wrappers; block the native licensed API.
    monkeypatch.setitem(sys.modules, "OrcFxAPI", None)
    from assetutilities.common.update_deep import AttributeDict
    from digitalmodel.engine import engine

    registry = yaml.safe_load((ROOT / "docs/registry/workflows.yaml").read_text())
    (row,) = [r for r in registry["workflows"] if r["id"] == "crack-fad-level2-handoff"]
    cfg = yaml.safe_load((ROOT / row["input"]).read_text())
    cfg["crack_assessment"]["output_dir"] = str(tmp_path)
    # Supply the engine's configured-input contract and isolate its YAML echo.
    (tmp_path / "results").mkdir()
    cfg["Analysis"] = {"analysis_root_folder": str(tmp_path), "file_name": "plate"}
    routed = engine(cfg=AttributeDict(cfg), config_flag=False, log_to_file=False)
    result = routed[row["basename"]]
    assert result["handoff"]["triggered"]
    assert Path(result["report_path"]).is_file()
    assert Path(result["handoff_path"]).is_file()


def test_catalog_keeps_published_validation_gap_visible():
    from digitalmodel.asset_integrity.offering_catalog import load, validate

    catalog = load()
    entry = catalog.ffs["engines"]["crack-fad"]
    assert entry["status"] == "workflow"
    assert entry["workflow"] == "crack-fad-level2-handoff"
    assert "published" in entry["caveat"].lower()
    assert validate(catalog) == []
