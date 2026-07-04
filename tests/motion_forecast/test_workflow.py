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


def test_router_measured_mode_emits_status_and_reconciliation():
    import numpy as np
    from digitalmodel.motion_forecast.measured import MeasuredMotion
    from digitalmodel.motion_forecast.models import DOF_NAMES

    tm = np.linspace(0.0, 30.0, 121)  # now=30 lies within the forecast horizon
    measured = MeasuredMotion(
        t=tm, dof={d: (0.2 * np.sin(tm) if d == "heave" else np.zeros_like(tm))
                   for d in DOF_NAMES})
    cfg = {
        "basename": "motion_forecast",
        "motion_forecast": {
            "sea": {"hs": 2.0, "tp": 9.0, "horizon": 60.0, "n_components": 32},
            "rao": {"source": "analytic", "preset": "generic_vessel"},
            "operation": "crane_lift",
            "measured": measured,
        },
    }
    out = router(cfg)["motion_forecast"]
    assert out["measured_status"]["operation"] == "crane_lift"
    assert out["measured_status"]["display"] in {"GO", "CAUTION", "NO-GO"}
    assert set(out["reconciliation"]["seam_offset"]) == set(DOF_NAMES)


def test_router_measured_without_operation_emits_reconciliation_only():
    import numpy as np
    from digitalmodel.motion_forecast.measured import MeasuredMotion
    from digitalmodel.motion_forecast.models import DOF_NAMES

    tm = np.linspace(0.0, 30.0, 121)
    measured = MeasuredMotion(t=tm, dof={d: np.zeros_like(tm) for d in DOF_NAMES})
    cfg = {
        "basename": "motion_forecast",
        "motion_forecast": {
            "sea": {"hs": 2.0, "tp": 9.0, "horizon": 60.0, "n_components": 24},
            "rao": {"source": "analytic"},
            "measured": measured,  # no "operation"
        },
    }
    out = router(cfg)["motion_forecast"]
    assert "measured_status" not in out          # needs an operation
    assert "seam_offset" in out["reconciliation"]  # reconciliation still emitted


def test_router_measured_bad_type_raises():
    import pytest
    cfg = {
        "basename": "motion_forecast",
        "motion_forecast": {
            "sea": {"hs": 2.0, "tp": 9.0, "horizon": 60.0, "n_components": 24},
            "rao": {"source": "analytic"},
            "measured": 123,  # neither MeasuredMotion nor {'csv': ...}
        },
    }
    with pytest.raises(ValueError, match="must be a MeasuredMotion"):
        router(cfg)


def test_engine_dispatch_wired():
    """Guard: engine.py routes basename 'motion_forecast' to the workflow."""
    engine_src = (
        Path(__file__).resolve().parents[2]
        / "src" / "digitalmodel" / "engine.py"
    ).read_text()
    assert 'basename == "motion_forecast"' in engine_src
    assert "motion_forecast.workflow import" in engine_src
