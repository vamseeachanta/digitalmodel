"""Workflow + engine-dispatch tests (digitalmodel #1358)."""

from pathlib import Path

from digitalmodel.motion_forecast import DOF_NAMES
from digitalmodel.motion_forecast.workflow import router


def test_router_produces_results():
    cfg = {
        "basename": "motion_forecast",
        "motion_forecast": {
            "sea": {"hs": 2.5, "tp": 9.0, "horizon": 60.0, "n_components": 32},
            "rao": {"source": "analytic", "preset": "generic_vessel"},
            "asset": {"location": [0.0, 0.0], "dt": 0.25},
        },
    }
    out = router(cfg)
    assert out is cfg  # mutates + returns cfg (repo convention)
    res = cfg["motion_forecast"]["results"]
    assert set(res["dof"]) == set(DOF_NAMES)
    assert len(res["t"]) == len(res["dof"]["heave"])
    assert res["significant"]["heave"] > 0.0
    assert res["n_components"] == 32


def test_router_emits_decision_for_operation():
    cfg = {
        "basename": "motion_forecast",
        "motion_forecast": {
            "sea": {"hs": 3.5, "tp": 8.0, "horizon": 60.0, "n_components": 48},
            "rao": {"source": "analytic", "preset": "generic_vessel"},
            "asset": {"location": [0.0, 0.0], "dt": 0.25},
            "operation": "crane_lift",
        },
    }
    out = router(cfg)
    dec = out["motion_forecast"]["decision"]
    assert dec["operation"] == "crane_lift"
    assert dec["state"] in {"GO", "MARGINAL", "NO_GO"}
    assert dec["display"] in {"GO", "CAUTION", "NO-GO"}
    assert dec["unit"] == "m/s"
    assert dec["caution"] < dec["limit"]


def test_engine_dispatch_wired():
    """Guard: engine.py routes basename 'motion_forecast' to the workflow."""
    engine_src = (
        Path(__file__).resolve().parents[2]
        / "src" / "digitalmodel" / "engine.py"
    ).read_text()
    assert 'basename == "motion_forecast"' in engine_src
    assert "motion_forecast.workflow import" in engine_src
