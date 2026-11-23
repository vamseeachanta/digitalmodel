# ABOUTME: Workflow (router) tests for the damage_cases section of the
# ABOUTME: vessel_stability_screening basename — CSVs, status, edge cases.
"""Workflow tests for the ``damage_cases`` section of the
``vessel_stability_screening`` basename (slice 2).

Fixture: the synthetic box-barge flooding fixture derived and hand-verified
in ``test_damage_stability_screening.py`` (public textbook closed forms
only): intact W = 8200 t (slice-1 fixture); flooded wing compartment
500 m3 x 0.8 permeability = 410 t at tcg 4.0 m with floodwater FSM 451 t*m
-> W' = 8610 t, T' = 4.2 m, KM' = 10.1 m, KG'_f = 4.8642276 m,
GM'_f = 5.2357724 m, curve equilibrium heel 2.0737 deg (small-angle moment
value 2.0835 deg), trim 0.7141 m by the stern; with downflooding at 30 deg
the margin is 27.93 deg and the residual area ~0.692 m*rad.
"""

import csv
import math

import pytest

from digitalmodel.vessel_stability_screening.workflow import router

KM_4 = 2.0 + 400.0 / 48.0
BMT_4 = 400.0 / 48.0
KM_42 = 10.1  # table-interpolated KM at W' = 8610 t
BMT_42 = 400.0 / (12.0 * 4.2)
KG_FLUID_DAMAGED = 4.8642276
HEELS = [0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0]

CITATION = {
    "standard": "IMO MODU Code 2009 (Res. A.1023(26))",
    "edition": "2009",
    "clause": "3.6",
}


def _kn(heel_deg, km, bmt):
    out = []
    for phi in heel_deg:
        r = math.radians(phi)
        out.append(km * math.sin(r) + 0.5 * bmt * math.tan(r) ** 2 * math.sin(r))
    return out


def _damaged_gz():
    kn = _kn(HEELS, KM_42, BMT_42)
    return [
        k - KG_FLUID_DAMAGED * math.sin(math.radians(p))
        for p, k in zip(HEELS, kn)
    ]


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


def _damage_section(**overrides):
    section = {
        "criteria": {
            "max_equilibrium_heel_deg": 15.0,
            "min_gm_m": 0.05,
            "min_downflooding_margin_deg": 7.0,
            "min_range_beyond_equilibrium_deg": 7.0,
            "min_residual_area_m_rad": 0.05,
            "citation": dict(CITATION),
        },
        "cases": [
            {
                "name": "wing tank flooded",
                "compartments": [
                    {
                        "name": "wing tank",
                        "volume_m3": 500.0,
                        "permeability": 0.8,
                        "vcg_m": 2.0,
                        "lcg_m": 30.0,
                        "tcg_m": 4.0,
                        "fsm_t_m": 451.0,
                        "free_communication": True,
                    }
                ],
                "downflooding_angle_deg": 30.0,
                "gz": {
                    "heel_deg": list(HEELS),
                    "kn_m": _kn(HEELS, KM_42, BMT_42),
                },
            },
            {
                "name": "ghs supplied case",
                "condition": {
                    "displacement_t": 8610.0,
                    "gm_fluid_m": 5.2357724,
                    "heel_deg": 2.0835,
                    "trim_m": 0.7141463,
                    "kg_fluid_m": KG_FLUID_DAMAGED,
                },
                "downflooding_angle_deg": 30.0,
                "gz": {"heel_deg": list(HEELS), "gz_m": _damaged_gz()},
            },
        ],
    }
    section.update(overrides)
    return section


def _settings(**overrides):
    settings = {
        "vessel": {"name": "synthetic box barge", "lbp_m": 100.0},
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
        "gz": {"heel_deg": list(HEELS), "kn_m": _kn(HEELS, KM_4, BMT_4)},
        "max_kg": {"enabled": False},
        "damage_cases": _damage_section(),
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


def test_router_added_weight_case_matches_golden_fixture(tmp_path):
    cfg = router(_cfg(tmp_path))
    result = cfg["vessel_stability_screening"]
    rows = result["damage_case_results"]
    assert len(rows) == 2
    added = rows[0]
    assert added["case"] == "wing tank flooded"
    assert added["method"] == "added_weight"
    assert added["displacement_t"] == pytest.approx(8610.0)
    assert added["draft_m"] == pytest.approx(4.2, abs=1e-9)
    assert added["floodwater_t"] == pytest.approx(410.0)
    assert added["kg_fluid_m"] == pytest.approx(4.8642276, abs=1e-6)
    assert added["gm_fluid_m"] == pytest.approx(5.2357724, abs=1e-6)
    assert added["trim_m"] == pytest.approx(0.7141463, abs=1e-6)
    # Curve equilibrium (flooding-moment arm), not the small-angle value:
    assert added["heel_deg"] == pytest.approx(2.0737, abs=5e-3)
    assert added["downflooding_margin_deg"] == pytest.approx(27.9263, abs=5e-3)
    assert added["range_beyond_equilibrium_deg"] == pytest.approx(27.9263, abs=5e-3)
    assert added["residual_area_m_rad"] == pytest.approx(0.6919, rel=1e-2)
    assert added["status"] == "pass"
    assert cfg["screening_status"] == "pass"


def test_router_direct_case_uses_supplied_heel(tmp_path):
    cfg = router(_cfg(tmp_path))
    direct = cfg["vessel_stability_screening"]["damage_case_results"][1]
    assert direct["method"] == "direct"
    assert direct["floodwater_t"] is None
    assert direct["heel_deg"] == pytest.approx(2.0835)
    assert direct["downflooding_margin_deg"] == pytest.approx(27.9165, abs=1e-6)
    assert direct["range_beyond_equilibrium_deg"] == pytest.approx(27.9165, abs=1e-6)
    assert direct["residual_area_m_rad"] == pytest.approx(0.7825, rel=1e-2)
    assert direct["status"] == "pass"


def test_router_damage_criteria_rows_cited(tmp_path):
    cfg = router(_cfg(tmp_path))
    rows = cfg["vessel_stability_screening"]["damage_criteria_results"]
    # 2 cases x 5 thresholds
    assert len(rows) == 10
    assert all(row["status"] == "pass" for row in rows)
    assert all("A.1023(26)" in row["citation"] for row in rows)
    assert {row["case"] for row in rows} == {"wing tank flooded", "ghs supplied case"}
    keys = {row["key"] for row in rows}
    assert keys == {
        "damage_equilibrium_heel",
        "damage_gm",
        "damage_downflooding_margin",
        "damage_range",
        "damage_residual_area",
    }


def test_router_writes_damage_csvs_and_summary(tmp_path):
    cfg = router(_cfg(tmp_path))
    result = cfg["vessel_stability_screening"]
    stem = "vessel_stability_screening"
    results_dir = tmp_path / "results"
    for suffix in ("damage_cases.csv", "damage_criteria.csv"):
        assert (results_dir / f"{stem}_{suffix}").exists(), suffix
    with (results_dir / f"{stem}_damage_cases.csv").open() as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == 2
    assert {"case", "method", "gm_fluid_m", "heel_deg", "status"} <= set(rows[0])
    with (results_dir / f"{stem}_damage_criteria.csv").open() as stream:
        criteria_rows = list(csv.DictReader(stream))
    assert len(criteria_rows) == 10
    assert {"case", "key", "value", "required", "citation"} <= set(criteria_rows[0])
    assert result["damage_cases_evaluated"] == 2
    assert result["damage_cases_failed"] == 0
    assert result["method"] == "vessel_stability_screening_v2"
    with (results_dir / f"{stem}_summary.csv").open() as stream:
        summary = list(csv.DictReader(stream))[0]
    assert summary["damage_cases_evaluated"] == "2"
    assert summary["screening_status"] == "pass"


def test_router_damage_only_run_without_intact_criteria(tmp_path):
    cfg = router(_cfg(tmp_path, gz=None, max_kg=None))
    result = cfg["vessel_stability_screening"]
    assert result["criteria_results"] == []
    assert result["damage_cases_evaluated"] == 2
    assert cfg["screening_status"] == "pass"


def test_router_negative_post_damage_gm_fails_screening(tmp_path):
    damage = _damage_section()
    case = damage["cases"][0]
    case["compartments"][0]["fsm_t_m"] = 90000.0
    del case["gz"]  # unstable case screened on heel/GM/margin only
    case["criteria"] = {
        "max_equilibrium_heel_deg": 15.0,
        "min_gm_m": 0.05,
        "min_downflooding_margin_deg": 7.0,
        "citation": dict(CITATION),
    }
    cfg = router(_cfg(tmp_path, damage_cases=damage))
    result = cfg["vessel_stability_screening"]
    rows = result["damage_case_results"]
    assert rows[0]["gm_fluid_m"] < 0.0
    assert rows[0]["heel_deg"] is None
    assert rows[0]["status"] == "fail"
    assert rows[1]["status"] == "pass"
    assert result["damage_cases_failed"] == 1
    assert cfg["screening_status"] == "fail"
    by_key = {
        (row["case"], row["key"]): row
        for row in result["damage_criteria_results"]
    }
    assert by_key[("wing tank flooded", "damage_gm")]["status"] == "fail"
    assert by_key[("wing tank flooded", "damage_equilibrium_heel")]["value"] is None


def test_router_equilibrium_beyond_downflooding_fails(tmp_path):
    damage = _damage_section()
    damage["cases"][0]["downflooding_angle_deg"] = 1.5
    cfg = router(_cfg(tmp_path, damage_cases=damage))
    rows = cfg["vessel_stability_screening"]["damage_case_results"]
    assert rows[0]["downflooding_margin_deg"] == pytest.approx(
        1.5 - 2.0737, abs=5e-3
    )
    assert rows[0]["range_beyond_equilibrium_deg"] == 0.0
    assert rows[0]["status"] == "fail"
    assert cfg["screening_status"] == "fail"


def test_router_margin_line_immersion_caps_limit(tmp_path):
    damage = _damage_section()
    damage["cases"][0]["margin_line_immersion_deg"] = 20.0
    cfg = router(_cfg(tmp_path, damage_cases=damage))
    row = cfg["vessel_stability_screening"]["damage_case_results"][0]
    assert row["limit_angle_deg"] == pytest.approx(20.0)
    assert row["downflooding_margin_deg"] == pytest.approx(20.0 - 2.0737, abs=5e-3)


def test_router_vessel_level_downflooding_is_fallback(tmp_path):
    damage = _damage_section()
    del damage["cases"][0]["downflooding_angle_deg"]
    settings = _settings(damage_cases=damage)
    settings["vessel"]["downflooding_angle_deg"] = 25.0
    settings["output_dir"] = str(tmp_path / "results")
    cfg = router(
        {
            "basename": "vessel_stability_screening",
            "vessel_stability_screening": settings,
            "_config_dir_path": str(tmp_path),
        }
    )
    row = cfg["vessel_stability_screening"]["damage_case_results"][0]
    assert row["limit_angle_deg"] == pytest.approx(25.0)


def test_router_wind_heeling_moment_area_ratio(tmp_path):
    damage = _damage_section()
    case = damage["cases"][1]
    case["wind_heeling_moment_t_m"] = 1640.0  # constant arm 0.1904762 m
    case["criteria"] = {
        "min_area_ratio": 1.0,
        "min_range_beyond_equilibrium_deg": 7.0,
        "citation": dict(CITATION),
    }
    cfg = router(_cfg(tmp_path, damage_cases=damage))
    row = cfg["vessel_stability_screening"]["damage_case_results"][1]
    # Constant-arm equilibrium hand value: 2.0750 deg
    assert row["heel_deg"] == pytest.approx(2.0750, abs=5e-3)
    assert row["area_ratio"] > 1.0
    assert row["status"] == "pass"


def test_router_damage_rejects_uncited_criteria(tmp_path):
    damage = _damage_section()
    del damage["criteria"]["citation"]
    with pytest.raises(ValueError, match="citation is required"):
        router(_cfg(tmp_path, damage_cases=damage))


def test_router_damage_rejects_missing_criteria(tmp_path):
    damage = _damage_section()
    del damage["criteria"]
    with pytest.raises(ValueError, match="cited criteria are required"):
        router(_cfg(tmp_path, damage_cases=damage))


def test_router_damage_rejects_unknown_criteria_field(tmp_path):
    damage = _damage_section()
    damage["criteria"]["min_gz_m"] = 0.1
    with pytest.raises(ValueError, match="unknown fields"):
        router(_cfg(tmp_path, damage_cases=damage))


def test_router_damage_case_needs_exactly_one_input_path(tmp_path):
    damage = _damage_section()
    damage["cases"][0]["condition"] = {"displacement_t": 8610.0, "gm_fluid_m": 5.0}
    with pytest.raises(ValueError, match="exactly one of"):
        router(_cfg(tmp_path, damage_cases=damage))
    damage = _damage_section()
    del damage["cases"][0]["compartments"]
    with pytest.raises(ValueError, match="exactly one of"):
        router(_cfg(tmp_path, damage_cases=damage))


def test_router_direct_kn_requires_damaged_kg_fluid(tmp_path):
    damage = _damage_section()
    case = damage["cases"][1]
    del case["condition"]["kg_fluid_m"]
    case["gz"] = {"heel_deg": list(HEELS), "kn_m": _kn(HEELS, KM_42, BMT_42)}
    with pytest.raises(ValueError, match="requires the damaged fluid KG"):
        router(_cfg(tmp_path, damage_cases=damage))


def test_router_wind_moment_without_gz_rejected(tmp_path):
    damage = _damage_section()
    case = damage["cases"][1]
    del case["gz"]
    case["wind_heeling_moment_t_m"] = 1640.0
    case["criteria"] = {
        "max_equilibrium_heel_deg": 15.0,
        "citation": dict(CITATION),
    }
    with pytest.raises(ValueError, match="requires damaged gz input"):
        router(_cfg(tmp_path, damage_cases=damage))


def test_router_gz_thresholds_without_gz_rejected(tmp_path):
    damage = _damage_section()
    del damage["cases"][0]["gz"]
    with pytest.raises(ValueError, match="require damaged GZ or KN data"):
        router(_cfg(tmp_path, damage_cases=damage))


def test_router_damage_cases_must_be_nonempty(tmp_path):
    with pytest.raises(ValueError, match="non-empty list"):
        router(_cfg(tmp_path, damage_cases={"criteria": {"min_gm_m": 0.05,
                                                         "citation": dict(CITATION)},
                                            "cases": []}))


def test_router_free_surface_dims_for_floodwater(tmp_path):
    damage = _damage_section()
    comp = damage["cases"][0]["compartments"][0]
    del comp["fsm_t_m"]
    # rho*l*b^3/12 = 1.025*12*8^3/12 = 524.8 t*m (floodwater density default)
    comp["free_surface"] = {"length_m": 12.0, "breadth_m": 8.0}
    cfg = router(_cfg(tmp_path, damage_cases=damage))
    row = cfg["vessel_stability_screening"]["damage_case_results"][0]
    # FSC' = (410 + 524.8)/8610 -> KG'_f = 4.7642276 + 0.1085714 = 4.8727990
    assert row["kg_fluid_m"] == pytest.approx(4.7642276 + 934.8 / 8610.0, abs=1e-6)
    assert row["status"] == "pass"


def test_router_intact_results_unchanged_by_damage_section(tmp_path):
    cfg = router(_cfg(tmp_path))
    result = cfg["vessel_stability_screening"]
    # Slice-1 golden values still hold with the damage section present.
    assert result["displacement_t"] == pytest.approx(8200.0)
    assert result["draft_m"] == pytest.approx(4.0)
    assert result["gm_fluid_m"] == pytest.approx(5.3808943, abs=1e-6)
    by_key = {row["key"]: row for row in result["criteria_results"]}
    assert by_key["imo_gz_30"]["value"] == pytest.approx(3.3848916, abs=1e-5)
