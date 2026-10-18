# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Slope workflow routing and JSON sidecar contract."""

import json

from digitalmodel.geotechnical.slope_workflow import SlopeStabilityWorkflow
from digitalmodel.geotechnical.slope_stability import MethodResult, SlopeResult
from tests.geotechnical.test_slope_profile import profile


def test_workflow_writes_versioned_json(tmp_path, monkeypatch):
    from digitalmodel.geotechnical import slope_stability

    result = SlopeResult("1.0.3", "si", 20, {
        "bishop": MethodResult(1.2, True, None, search_mode="grid_seeds_isolated",
                               search_factors_of_safety={"grid": 1.3, "user_circles": 1.2},
                               grid_thin_surface_filter="coarse_only_5_percent"),
        "spencer": MethodResult(1.3, False, None, search_mode="grid_seeds_isolated"),
    })
    calls = []

    def calculate(data, scratch_directory, *, num_slices):
        calls.append((data, scratch_directory, num_slices))
        return result

    monkeypatch.setattr(slope_stability, "analyze_profile", calculate)
    cfg = {"slope_stability": {"profile": profile(), "num_slices": 20,
                               "outputs": {"directory": str(tmp_path)}}}
    routed = SlopeStabilityWorkflow().router(cfg)
    summary = json.loads((tmp_path / "slope_stability_summary.json").read_text())
    assert summary["result"]["xslope_version"] == "1.0.3"
    bishop = summary["result"]["methods"]["bishop"]
    assert bishop["search_mode"] == "grid_seeds_isolated"
    assert bishop["search_factors_of_safety"] == {"grid": 1.3, "user_circles": 1.2}
    assert bishop["grid_thin_surface_filter"] == "coarse_only_5_percent"
    assert summary["result"]["methods"]["spencer"]["search_converged"] is False
    assert summary["screening_status"] == "not_converged"
    assert routed["outputs"]["summary_json"] == str(tmp_path / "slope_stability_summary.json")
    assert calls[0][2] == 20


def test_engine_routes_slope_stability(monkeypatch, tmp_path):
    from digitalmodel.engine import engine

    def route(self, cfg):
        cfg["slope_routed"] = True
        return cfg

    monkeypatch.setattr(SlopeStabilityWorkflow, "router", route)
    config = tmp_path / "input.yml"
    config.write_text("basename: slope_stability\n")
    result = engine(str(config))
    assert result["slope_routed"] is True
