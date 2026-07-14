# ABOUTME: Workflow (router) tests for the vessel_stability_screening basename —
# ABOUTME: YAML-config driven intact stability screening, CSV outputs, status.
"""Workflow tests for the ``vessel_stability_screening`` basename.

Fixture: the synthetic box-barge golden fixture derived and hand-verified in
``test_vessel_stability_screening.py`` (Barrass & Derrett / PNA box-barge
closed forms — public textbook values only): L = 100 m, B = 20 m, seawater;
condition W = 8200 t -> T = 4.0 m, KM = 10.3333 m, KG = 4.9024 m,
FSC = 0.0500 m, GM_fluid = 5.3809 m, trim 0.2341 m by the stern; wall-sided
KN cross-curve; 46 CFR 170.170 weather GM_required = 0.0807 m; crane lift
100 t x 15 m -> equilibrium heel 1.9377 deg; governing max-KG = KM - 0.15
= 10.1833 m (IMO GM0).
"""

import csv
import math

import pytest

from digitalmodel.vessel_stability_screening.workflow import router

KM_4 = 2.0 + 400.0 / 48.0
BMT_4 = 400.0 / 48.0
HEELS = [0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0]


def _kn(heel_deg):
    out = []
    for phi in heel_deg:
        r = math.radians(phi)
        out.append(
            KM_4 * math.sin(r) + 0.5 * BMT_4 * math.tan(r) ** 2 * math.sin(r)
        )
    return out


def _hydro_rows():
    rows = []
    for t in (2.0, 3.0, 4.0, 5.0, 6.0, 8.0):
        rows.append(
            {
                "draft_m": t,
                "displacement_t": 1.025 * 100.0 * 20.0 * t,
                "km_m": t / 2.0 + 400.0 / (12.0 * t),
                "lcb_m": 50.0,
                "lcf_m": 50.0,
                "mct_t_m_per_cm": 170.8333333,
                "tpc_t_per_cm": 20.5,
            }
        )
    return rows


def _settings(**overrides):
    settings = {
        "vessel": {
            "name": "synthetic box barge",
            "lbp_m": 100.0,
            "deck_edge_immersion_deg": 30.9637565,
        },
        "hydrostatics": {"table": _hydro_rows()},
        "loading_condition": {
            "name": "departure",
            "items": [
                {"name": "lightship", "weight_t": 4000.0, "vcg_m": 5.0, "lcg_m": 50.0},
                {"name": "cargo", "weight_t": 3000.0, "vcg_m": 6.0, "lcg_m": 52.0},
                {"name": "fuel", "weight_t": 1000.0, "vcg_m": 2.0, "lcg_m": 40.0,
                 "fsm_t_m": 410.0},
                {"name": "ballast", "weight_t": 200.0, "vcg_m": 1.0, "lcg_m": 50.0},
            ],
        },
        "gz": {"heel_deg": list(HEELS), "kn_m": _kn(HEELS)},
        "criteria": {
            "weather_cfr_170_170": {
                "wind_pressure_t_m2": 0.055,
                "windage_area_m2": 600.0,
                "windage_lever_m": 5.0,
                "max_heel_deg": 14.0,
                "citation": {
                    "standard": "46 CFR 170.170",
                    "edition": "2024",
                    "clause": "170.170(a)",
                },
            },
        },
        "lifting": {
            "name": "main crane lift",
            "hook_load_t": 100.0,
            "transverse_outreach_m": 15.0,
            "criteria": {
                "max_equilibrium_heel_deg": 5.0,
                "residual_area_ratio_min": 1.4,
                "citation": {
                    "standard": "46 CFR Part 173 Subpart B",
                    "edition": "2024",
                    "clause": "173.005-series",
                },
            },
        },
        "max_kg": {"enabled": True},
    }
    settings.update(overrides)
    return settings


def _cfg(tmp_path, **overrides):
    settings = _settings(**overrides)
    settings.setdefault("output_dir", str(tmp_path / "results"))
    return {
        "basename": "vessel_stability_screening",
        "vessel_stability_screening": settings,
        "_config_dir_path": str(tmp_path),
    }


def test_router_matches_golden_fixture(tmp_path):
    cfg = router(_cfg(tmp_path))
    result = cfg["vessel_stability_screening"]
    assert result["displacement_t"] == pytest.approx(8200.0)
    assert result["draft_m"] == pytest.approx(4.0)
    assert result["kg_m"] == pytest.approx(4.9024390, abs=1e-6)
    assert result["fsc_m"] == pytest.approx(0.05, abs=1e-9)
    assert result["kg_fluid_m"] == pytest.approx(4.9524390, abs=1e-6)
    assert result["gm_fluid_m"] == pytest.approx(5.3808943, abs=1e-6)
    assert result["trim_m"] == pytest.approx(0.2341463, abs=1e-6)
    assert result["equilibrium_result"]["draft_aft_m"] == pytest.approx(
        4.1170732, abs=1e-6
    )
    assert cfg["screening_status"] == "pass"
    assert "Screening tier only" in result["governance"]


def test_router_criteria_table_cited_and_passing(tmp_path):
    cfg = router(_cfg(tmp_path))
    rows = cfg["vessel_stability_screening"]["criteria_results"]
    by_key = {row["key"]: row for row in rows}
    # 6 IMO + 1 weather + 2 lifting = 9 criteria rows
    assert len(rows) == 9
    assert all(row["status"] == "pass" for row in rows)
    assert all(row["citation"] for row in rows)
    assert "MSC.267(85)" in by_key["imo_gm0"]["citation"]
    assert by_key["cfr_170_170_weather_gm"]["required"] == pytest.approx(
        0.0807046, abs=1e-6
    )
    assert "46 CFR 170.170" in by_key["cfr_170_170_weather_gm"]["citation"]
    assert by_key["imo_gz_30"]["value"] == pytest.approx(3.3848916, abs=1e-5)
    assert by_key["lifting_equilibrium_heel"]["value"] == pytest.approx(
        1.9377, abs=5e-3
    )
    assert "46 CFR Part 173" in by_key["lifting_equilibrium_heel"]["citation"]


def test_router_lifting_result_block(tmp_path):
    cfg = router(_cfg(tmp_path))
    lifting = cfg["vessel_stability_screening"]["lifting_result"]
    assert lifting["heeling_moment_t_m"] == pytest.approx(1500.0)
    assert lifting["heeling_arm_0_m"] == pytest.approx(0.1829268, abs=1e-6)
    assert lifting["equilibrium_heel_deg"] == pytest.approx(1.9377, abs=5e-3)
    assert lifting["status"] == "pass"


def test_router_max_kg_limits_and_governing(tmp_path):
    cfg = router(_cfg(tmp_path))
    result = cfg["vessel_stability_screening"]
    by_key = {row["key"]: row for row in result["kg_limits"]}
    assert by_key["imo_gm0"]["kg_limit_m"] == pytest.approx(10.1833333, abs=1e-3)
    assert by_key["cfr_170_170_weather_gm"]["kg_limit_m"] == pytest.approx(
        10.2526287, abs=1e-3
    )
    # Lifting equilibrium-heel limit (hand value, see core tests): 8.2744 m
    assert by_key["lifting_equilibrium_heel"]["kg_limit_m"] == pytest.approx(
        8.2744, abs=5e-3
    )
    assert result["governing_kg_criterion"] == "lifting_equilibrium_heel"
    assert result["governing_kg_limit_m"] == pytest.approx(8.2744, abs=5e-3)


def test_router_writes_report_pack_ready_csvs(tmp_path):
    router(_cfg(tmp_path))
    stem = "vessel_stability_screening"
    results_dir = tmp_path / "results"
    for suffix in (
        "loading_condition.csv",
        "equilibrium.csv",
        "criteria.csv",
        "gz_curve.csv",
        "kg_limits.csv",
        "summary.csv",
    ):
        assert (results_dir / f"{stem}_{suffix}").exists(), suffix
    with (results_dir / f"{stem}_criteria.csv").open() as stream:
        rows = list(csv.DictReader(stream))
    assert {"key", "value", "required", "unit", "status", "citation"} <= set(rows[0])
    assert len(rows) == 9
    with (results_dir / f"{stem}_gz_curve.csv").open() as stream:
        gz_rows = list(csv.DictReader(stream))
    assert {"heel_deg", "gz_m", "heeling_arm_m", "net_arm_m"} <= set(gz_rows[0])
    assert len(gz_rows) == len(HEELS)
    with (results_dir / f"{stem}_summary.csv").open() as stream:
        summary = list(csv.DictReader(stream))[0]
    assert summary["screening_status"] == "pass"


def test_router_hydrostatics_from_csv(tmp_path):
    csv_path = tmp_path / "hydrostatics.csv"
    rows = _hydro_rows()
    with csv_path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)
    cfg = router(_cfg(tmp_path, hydrostatics={"csv": "hydrostatics.csv"}))
    assert cfg["vessel_stability_screening"]["draft_m"] == pytest.approx(4.0)
    assert cfg["screening_status"] == "pass"


def test_router_cross_curve_grid_interpolation(tmp_path):
    # Sandwich the true KN curve between two grid rows equidistant in W:
    # linear interpolation at W = 8200 recovers the true curve.
    kn = _kn(HEELS)
    grid = {
        "displacements_t": [6150.0, 10250.0],
        "kn_m": [
            [v - 0.5 for v in kn],
            [v + 0.5 for v in kn],
        ],
    }
    cfg = router(
        _cfg(tmp_path, gz={"heel_deg": list(HEELS), "cross_curves": grid})
    )
    by_key = {
        row["key"]: row
        for row in cfg["vessel_stability_screening"]["criteria_results"]
    }
    assert by_key["imo_gz_30"]["value"] == pytest.approx(3.3848916, abs=1e-5)


def test_router_direct_gz_table_skips_max_kg_requirement(tmp_path):
    kn = _kn(HEELS)
    kg_fluid = 4.9524390
    gz = [k - kg_fluid * math.sin(math.radians(p)) for p, k in zip(HEELS, kn)]
    cfg = router(
        _cfg(
            tmp_path,
            gz={"heel_deg": list(HEELS), "gz_m": gz},
            max_kg=None,
            lifting=None,
        )
    )
    by_key = {
        row["key"]: row
        for row in cfg["vessel_stability_screening"]["criteria_results"]
    }
    assert by_key["imo_gz_30"]["value"] == pytest.approx(3.3848916, abs=1e-5)
    assert cfg["vessel_stability_screening"]["kg_limits"] == []


def test_router_max_kg_with_gz_table_rejected(tmp_path):
    gz = [0.0] + [1.0] * (len(HEELS) - 1)
    cfg = _cfg(tmp_path, gz={"heel_deg": list(HEELS), "gz_m": gz}, lifting=None)
    with pytest.raises(ValueError, match="max_kg requires KN input"):
        router(cfg)


def test_router_free_surface_tank_dimensions(tmp_path):
    settings = _settings()
    settings["loading_condition"]["items"][2] = {
        "name": "fuel",
        "weight_t": 1000.0,
        "vcg_m": 2.0,
        "lcg_m": 40.0,
        # rho*l*b^3/12 = 1.025 * 20 * (2.7144176)^3/12 -> pick exact 410:
        # use l = 12 m, b = 8 m, rho = 0.4270833 -> 0.4270833*12*512/12 = 218.67
        # keep it simple: l = 12, b = 8, rho = 0.85 -> FSM = 435.2 t*m
        "free_surface": {"length_m": 12.0, "breadth_m": 8.0, "density_t_m3": 0.85},
    }
    settings["output_dir"] = str(tmp_path / "results")
    cfg = router(
        {
            "basename": "vessel_stability_screening",
            "vessel_stability_screening": settings,
            "_config_dir_path": str(tmp_path),
        }
    )
    result = cfg["vessel_stability_screening"]
    assert result["fsc_m"] == pytest.approx(435.2 / 8200.0, abs=1e-9)


def test_router_fsm_dominance_fails_gm(tmp_path):
    # Free-surface loss dominates: FSC = 82000/8200 = 10 m -> GM_fluid < 0.
    settings = _settings()
    settings["loading_condition"]["items"][2]["fsm_t_m"] = 82000.0
    settings["output_dir"] = str(tmp_path / "results")
    cfg = router(
        {
            "basename": "vessel_stability_screening",
            "vessel_stability_screening": settings,
            "_config_dir_path": str(tmp_path),
        }
    )
    result = cfg["vessel_stability_screening"]
    assert result["fsc_m"] == pytest.approx(10.0)
    assert result["gm_fluid_m"] < 0.0
    assert cfg["screening_status"] == "fail"
    by_key = {row["key"]: row for row in result["criteria_results"]}
    assert by_key["imo_gm0"]["status"] == "fail"
    assert by_key["cfr_170_170_weather_gm"]["status"] == "fail"


def test_router_uncited_weather_criterion_rejected(tmp_path):
    settings = _settings()
    del settings["criteria"]["weather_cfr_170_170"]["citation"]
    settings["output_dir"] = str(tmp_path / "results")
    cfg = {
        "basename": "vessel_stability_screening",
        "vessel_stability_screening": settings,
        "_config_dir_path": str(tmp_path),
    }
    with pytest.raises(ValueError, match="citation is required"):
        router(cfg)


def test_router_uncited_lifting_criteria_rejected(tmp_path):
    settings = _settings()
    del settings["lifting"]["criteria"]["citation"]
    settings["output_dir"] = str(tmp_path / "results")
    cfg = {
        "basename": "vessel_stability_screening",
        "vessel_stability_screening": settings,
        "_config_dir_path": str(tmp_path),
    }
    with pytest.raises(ValueError, match="citation is required"):
        router(cfg)


def test_router_lifting_without_gz_rejected(tmp_path):
    cfg = _cfg(tmp_path, gz=None, max_kg=None)
    with pytest.raises(ValueError, match="lifting requires gz input"):
        router(cfg)


def test_router_no_criteria_rejected(tmp_path):
    cfg = _cfg(tmp_path, gz=None, max_kg=None, lifting=None, criteria=None)
    with pytest.raises(ValueError, match="evaluated no criteria"):
        router(cfg)


def test_router_requires_hydrostatics_and_condition(tmp_path):
    cfg = _cfg(tmp_path, hydrostatics=None)
    with pytest.raises(ValueError, match="hydrostatics mapping is required"):
        router(cfg)
    cfg = _cfg(tmp_path, loading_condition=None)
    with pytest.raises(ValueError, match="loading_condition mapping is required"):
        router(cfg)


def test_router_engine_arm_routes_basename():
    # The engine arm imports lazily; exercise the routed import path directly.
    from digitalmodel import vessel_stability_screening as package

    assert package.router is router
