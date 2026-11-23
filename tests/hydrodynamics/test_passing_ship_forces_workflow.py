"""Workflow (router) tests for the passing_ship_forces basename."""

import csv

import numpy as np
import pytest

from digitalmodel.hydrodynamics.wang_passing_ship import (
    WangVessel,
    wang_passing_forces,
)
from digitalmodel.passing_ship_forces.workflow import router

MOORED = {"length": 658.0, "midship_area": 3936.299}
PASSING = {"length": 902.2, "midship_area": 7099.888}


def _cfg(tmp_path, **overrides):
    settings = {
        "moored_vessel": dict(MOORED),
        "passing_vessel": dict(PASSING),
        "water_density": 1.9905,
        "passing_velocity": 5.0,
        "separation_distance": 221.8,
        "water_depth": 47.0,
        "stagger": {"range": {"start": -2.0, "stop": 2.0, "num": 41, "normalized": True}},
        "output_dir": str(tmp_path / "results"),
    }
    settings.update(overrides)
    return {
        "basename": "passing_ship_forces",
        "passing_ship_forces": settings,
        "_config_dir_path": str(tmp_path),
    }


def _library_reference(stagger, depth=47.0):
    return wang_passing_forces(
        stagger,
        221.8,
        WangVessel(**MOORED),
        WangVessel(**PASSING),
        5.0,
        1.9905,
        depth=depth,
    )


def test_router_matches_library(tmp_path):
    cfg = router(_cfg(tmp_path))
    result = cfg["passing_ship_forces"]
    stagger = np.linspace(-2.0 * MOORED["length"], 2.0 * MOORED["length"], 41)
    expected = _library_reference(stagger)
    rows = result["forces"]
    assert len(rows) == 41
    for row, xi, fx, fy, mz in zip(
        rows, stagger, expected.surge, expected.sway, expected.yaw
    ):
        assert row["stagger"] == pytest.approx(xi, rel=1e-12)
        assert row["surge_force"] == pytest.approx(fx, rel=1e-12)
        assert row["sway_force"] == pytest.approx(fy, rel=1e-12)
        assert row["yaw_moment"] == pytest.approx(mz, rel=1e-12)
    assert result["depth_mode"] == "finite_image_method"
    assert result["n_images"] == 10


def test_router_peak_summary(tmp_path):
    cfg = router(_cfg(tmp_path))
    result = cfg["passing_ship_forces"]
    sway = [row["sway_force"] for row in cfg["passing_ship_forces"]["forces"]]
    assert result["max_sway_attraction"] == pytest.approx(max(sway), rel=1e-12)
    assert result["max_sway_repulsion"] == pytest.approx(min(sway), rel=1e-12)
    # abeam attraction dominates for this case; peak at zero stagger
    assert result["peak_sway_stagger"] == pytest.approx(0.0, abs=1e-9)
    assert result["peak_sway_force"] == pytest.approx(48031.0326275, rel=1e-6)


def test_router_writes_csvs(tmp_path):
    cfg = router(_cfg(tmp_path))
    result = cfg["passing_ship_forces"]
    results_csv = tmp_path / "results" / "passing_ship_forces_passing_ship_forces.csv"
    summary_csv = (
        tmp_path / "results" / "passing_ship_forces_passing_ship_forces_summary.csv"
    )
    assert results_csv.exists()
    assert summary_csv.exists()
    with results_csv.open() as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == 41
    assert set(rows[0]) == {"stagger", "surge_force", "sway_force", "yaw_moment"}
    assert result["forces"][0]["sway_force"] == pytest.approx(
        float(rows[0]["sway_force"]), rel=1e-9
    )


def test_router_explicit_stagger_deep_water(tmp_path):
    settings_overrides = {
        "stagger": {"values": [-658.0, 0.0, 658.0]},
    }
    cfg = _cfg(tmp_path, **settings_overrides)
    del cfg["passing_ship_forces"]["water_depth"]
    cfg = router(cfg)
    result = cfg["passing_ship_forces"]
    assert result["depth_mode"] == "deep"
    assert result["n_images"] is None
    expected = _library_reference([-658.0, 0.0, 658.0], depth=None)
    for row, fy in zip(result["forces"], expected.sway):
        assert row["sway_force"] == pytest.approx(fy, rel=1e-12)


def test_router_requires_vessels(tmp_path):
    cfg = _cfg(tmp_path)
    del cfg["passing_ship_forces"]["moored_vessel"]
    with pytest.raises(ValueError, match="moored_vessel"):
        router(cfg)


def test_router_rejects_stagger_with_both_forms(tmp_path):
    cfg = _cfg(
        tmp_path,
        stagger={
            "values": [0.0],
            "range": {"start": -1.0, "stop": 1.0, "num": 3},
        },
    )
    with pytest.raises(ValueError, match="exactly one"):
        router(cfg)

def test_router_rejects_short_range(tmp_path):
    cfg = _cfg(tmp_path, stagger={"range": {"start": -1.0, "stop": 1.0, "num": 1}})
    with pytest.raises(ValueError, match="num"):
        router(cfg)


def test_router_rejects_negative_depth(tmp_path):
    cfg = _cfg(tmp_path, water_depth=-5.0)
    with pytest.raises(ValueError, match="water_depth"):
        router(cfg)
