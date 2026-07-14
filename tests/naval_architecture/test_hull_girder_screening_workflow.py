# ABOUTME: Workflow (router) tests for the hull_girder_screening basename —
# ABOUTME: YAML-config driven SF/BM screening, CSV outputs, status gating.
"""Workflow tests for the ``hull_girder_screening`` basename.

The loading condition is the synthetic box-barge golden fixture derived in
``test_hull_girder_screening.py`` (PNA Vol. I / Hughes & Paik / Barrass &
Derrett box-barge form): L = 100 m, W = 10 000 t over the middle half,
uniform buoyancy -> SF extreme W/4 = 2 500 t at the quarter points and
midship BM = -W L/16 = -62 500 t·m (sagging), closing to ~0 at the ends.
"""

import csv

import pytest

from digitalmodel.hull_girder_screening.workflow import router

L = 100.0
W = 10_000.0


def _cfg(tmp_path, **overrides):
    settings = {
        "length_m": L,
        "n_stations": 401,
        "weights": {
            "lightship": [
                {"name": "cargo", "weight_t": W, "x_start_m": 25.0, "x_end_m": 75.0},
            ],
        },
        "buoyancy": {"method": "box"},
        "allowables": {
            "shear_force": {
                "x_m": [0.0, 25.0, 75.0, 100.0],
                "positive_t": [1000.0, 5000.0, 5000.0, 1000.0],
            },
            "bending_moment": {
                "x_m": [0.0, 50.0, 100.0],
                "hogging_t_m": [20000.0, 125000.0, 20000.0],
                "sagging_t_m": [20000.0, 125000.0, 20000.0],
            },
        },
        "frames": [
            {"name": "Fr 25", "x_m": 25.0},
            {"name": "Midship", "x_m": 50.0},
            {"name": "Fr 75", "x_m": 75.0},
        ],
        "output_dir": str(tmp_path / "results"),
    }
    settings.update(overrides)
    return {
        "basename": "hull_girder_screening",
        "hull_girder_screening": settings,
        "_config_dir_path": str(tmp_path),
    }


def test_router_matches_golden_fixture(tmp_path):
    cfg = router(_cfg(tmp_path))
    result = cfg["hull_girder_screening"]
    assert result["displacement_t"] == pytest.approx(W, rel=1e-6)
    assert result["lcg_m"] == pytest.approx(50.0, rel=1e-6)
    assert result["max_sagging_t_m"] == pytest.approx(-62_500.0, rel=1e-3)
    assert result["max_sagging_kn_m"] == pytest.approx(-613_125.0, rel=1e-3)
    assert result["max_hogging_t_m"] == 0.0
    assert abs(result["max_shear_t"]) == pytest.approx(2_500.0, rel=1e-2)
    assert result["closure_ok"] is True
    # SF/BM close at the free ends
    last = result["stations"][-1]
    assert abs(last["shear_force_t"]) < 1e-6
    assert abs(last["bending_moment_t_m"]) < 1e-6
    assert cfg["screening_status"] == "pass"
    assert "Screening tier only" in result["governance"]


def test_router_utilization_table(tmp_path):
    cfg = router(_cfg(tmp_path))
    rows = {row["frame"]: row for row in cfg["hull_girder_screening"]["utilization"]}
    assert rows["Midship"]["bending_utilization"] == pytest.approx(0.5, rel=1e-2)
    assert rows["Fr 25"]["shear_utilization"] == pytest.approx(0.5, rel=1e-2)
    assert all(row["status"] == "pass" for row in rows.values())
    assert cfg["hull_girder_screening"]["max_bending_utilization"] == pytest.approx(
        0.5, rel=1e-2
    )


def test_router_flags_allowable_exceedance(tmp_path):
    cfg = _cfg(
        tmp_path,
        allowables={
            "bending_moment": {
                "x_m": [0.0, 100.0],
                "hogging_t_m": [50000.0, 50000.0],
                "sagging_t_m": [50000.0, 50000.0],
            }
        },
    )
    cfg = router(cfg)
    result = cfg["hull_girder_screening"]
    assert cfg["screening_status"] == "fail"
    mid = [r for r in result["utilization"] if r["frame"] == "Midship"][0]
    assert mid["status"] == "fail"
    assert mid["bending_utilization"] == pytest.approx(1.25, rel=1e-2)


def test_router_writes_report_pack_ready_csvs(tmp_path):
    cfg = router(_cfg(tmp_path))
    result = cfg["hull_girder_screening"]
    stations_csv = (
        tmp_path / "results" / "hull_girder_screening_hull_girder_stations.csv"
    )
    utilization_csv = (
        tmp_path / "results" / "hull_girder_screening_hull_girder_utilization.csv"
    )
    summary_csv = (
        tmp_path / "results" / "hull_girder_screening_hull_girder_summary.csv"
    )
    for path in (stations_csv, utilization_csv, summary_csv):
        assert path.exists()
    with stations_csv.open() as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == 401
    assert set(rows[0]) == {
        "x_m", "weight_per_m_t", "buoyancy_per_m_t",
        "shear_force_t", "bending_moment_t_m",
    }
    with summary_csv.open() as stream:
        summary = list(csv.DictReader(stream))[0]
    assert summary["screening_status"] == "pass"
    assert float(summary["max_sagging_t_m"]) == pytest.approx(
        result["max_sagging_t_m"], rel=1e-9
    )


def test_router_default_frames_from_allowable_knots(tmp_path):
    cfg = _cfg(tmp_path)
    del cfg["hull_girder_screening"]["frames"]
    cfg = router(cfg)
    frames = [r["x_m"] for r in cfg["hull_girder_screening"]["utilization"]]
    assert frames == [0.0, 25.0, 50.0, 75.0, 100.0]


def test_router_hydrostatics_table_mode(tmp_path):
    cfg = _cfg(
        tmp_path,
        buoyancy={
            "method": "hydrostatics_table",
            "stations": [
                {"x_m": 0.0, "drafts_m": [0.0, 10.0], "areas_m2": [0.0, 200.0]},
                {"x_m": 100.0, "drafts_m": [0.0, 10.0], "areas_m2": [0.0, 200.0]},
            ],
        },
        water_density_t_m3=1.025,
    )
    cfg = router(cfg)
    result = cfg["hull_girder_screening"]
    # box hull B=20 m: even-keel draft = W/(rho*B*L) = 4.878 m
    assert result["equilibrium"]["method"] == "hydrostatics_table"
    assert result["equilibrium"]["draft_mean_m"] == pytest.approx(
        W / (1.025 * 20.0 * L), rel=1e-3
    )
    assert abs(result["equilibrium"]["trim_m"]) < 1e-3
    assert result["closure_ok"] is True
    assert result["max_sagging_t_m"] == pytest.approx(-62_500.0, rel=1e-3)


def test_router_direct_buoyancy_mode(tmp_path):
    cfg = _cfg(
        tmp_path,
        buoyancy={
            "method": "direct",
            "x_m": [0.0, 100.0],
            "buoyancy_per_m_t": [100.0, 100.0],
        },
    )
    cfg = router(cfg)
    result = cfg["hull_girder_screening"]
    assert result["equilibrium"]["method"] == "direct"
    assert result["max_sagging_t_m"] == pytest.approx(-62_500.0, rel=1e-3)


def test_router_section_modulus_approved(tmp_path):
    cfg = _cfg(
        tmp_path,
        section_modulus={
            "yield_mpa": 235.0,
            "approved_sm": {"deck_m3": 5.0, "keel_m3": 5.5, "source": "loading manual"},
        },
    )
    cfg = router(cfg)
    result = cfg["hull_girder_screening"]
    screens = result["section_modulus"]
    assert len(screens) == 4  # deck/keel x hog/sag
    sag_deck = [
        s for s in screens if s["condition"] == "sagging" and s["location"] == "deck"
    ][0]
    assert sag_deck["check"]["stress_mpa"] == pytest.approx(122.625, rel=1e-3)
    assert sag_deck["check"]["utilization"] == pytest.approx(0.7007, rel=1e-3)
    assert result["max_section_modulus_utilization"] == pytest.approx(0.7007, rel=1e-3)
    assert cfg["screening_status"] == "pass"


def test_router_section_modulus_scantlings(tmp_path):
    cfg = _cfg(
        tmp_path,
        section_modulus={
            "yield_mpa": 235.0,
            "scantlings": {
                "depth_m": 5.0,
                "elements": [
                    {"name": "deck", "breadth_m": 10.0, "thickness_m": 0.02, "z_m": 5.0},
                    {"name": "bottom", "breadth_m": 10.0, "thickness_m": 0.03, "z_m": 0.0},
                    {"name": "side_p", "height_m": 5.0, "thickness_m": 0.01, "z_m": 2.5},
                    {"name": "side_s", "height_m": 5.0, "thickness_m": 0.01, "z_m": 2.5},
                ],
            },
        },
    )
    cfg = router(cfg)
    result = cfg["hull_girder_screening"]
    assert result["section_properties"]["neutral_axis_m"] == pytest.approx(
        25.0 / 12.0, rel=1e-6
    )
    assert result["section_modulus"][0]["sm_source"] == "scantlings"


def test_router_requires_weights(tmp_path):
    cfg = _cfg(tmp_path)
    del cfg["hull_girder_screening"]["weights"]
    with pytest.raises(ValueError, match="weights"):
        router(cfg)


def test_router_rejects_unknown_buoyancy_method(tmp_path):
    cfg = _cfg(tmp_path, buoyancy={"method": "magic"})
    with pytest.raises(ValueError, match="buoyancy.method"):
        router(cfg)


def test_router_rejects_double_section_modulus_source(tmp_path):
    cfg = _cfg(
        tmp_path,
        section_modulus={
            "yield_mpa": 235.0,
            "approved_sm": {"deck_m3": 5.0},
            "scantlings": {"elements": []},
        },
    )
    with pytest.raises(ValueError, match="exactly one"):
        router(cfg)


def test_engine_registers_basename():
    from pathlib import Path

    import digitalmodel

    source = (Path(digitalmodel.__file__).parent / "engine.py").read_text(
        encoding="utf-8"
    )
    assert 'basename == "hull_girder_screening"' in source
