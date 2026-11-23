# ABOUTME: Workflow (router) tests for the alt_fuel_ship_sizing basename,
# ABOUTME: including the synthetic 40k-DWT-bulker YAML fixture end to end.
"""Router tests for basename ``alt_fuel_ship_sizing``.

The synthetic fixture is the hand-verified ~40k DWT bulker-like concept from
``test_alt_fuel_ship_sizing.py``: 4000 nm at 12 kn, 4750 kW shaft + 500 kW
hotel, 50 % fuel cell x 95 % drivetrain, LHV 120 MJ/kg (round-number
override) -> 330 kg/h, 121000 kg voyage fuel, 1856.34 m3 gross tank.
"""

import csv
from pathlib import Path

import pytest
import yaml

from digitalmodel.alt_fuel_ship_sizing.workflow import router

FIXTURE = Path(__file__).parent / "fixtures" / "alt_fuel_ship_sizing_bulker40k.yml"


def _fixture_cfg(tmp_path):
    cfg = yaml.safe_load(FIXTURE.read_text())
    cfg["_config_dir_path"] = str(tmp_path)
    cfg["_config_file_path"] = str(tmp_path / FIXTURE.name)
    return cfg


def _no_wind_cfg(tmp_path):
    cfg = _fixture_cfg(tmp_path)
    del cfg["alt_fuel_ship_sizing"]["wind_assist"]
    cfg["alt_fuel_ship_sizing"]["trade_study"]["wind_assist"] = [False]
    return cfg


def test_fixture_fuel_chain_hand_verified(tmp_path):
    """No-wind design point reproduces the hand arithmetic:
    5500 kW electrical, 11000 kW LHV, 330 kg/h, 121000 kg, 1856.34 m3."""
    cfg = router(_no_wind_cfg(tmp_path))
    summary = cfg["alt_fuel_ship_sizing"]["summary"]
    assert summary["electrical_power_kw"] == pytest.approx(5500.0, rel=1e-12)
    assert summary["fuel_lhv_power_kw"] == pytest.approx(11000.0, rel=1e-12)
    assert summary["fuel_mass_flow_kg_per_h"] == pytest.approx(330.0, rel=1e-12)
    assert summary["required_fuel_mass_kg"] == pytest.approx(121000.0, rel=1e-9)
    assert summary["gross_tank_volume_m3"] == pytest.approx(1856.341, rel=1e-6)
    assert summary["bog_rate_percent_per_day"] == pytest.approx(0.251804, rel=1e-5)
    assert summary["fuel_cost"] == pytest.approx(726000.0, rel=1e-9)
    assert cfg["alt_fuel_ship_sizing"]["warnings"] == []


def test_fixture_wind_assist_design_point(tmp_path):
    """With the fixture rose the expected saving is 485.05 kW (hand-verified
    in the library tests); the design-point fuel drops accordingly."""
    cfg = router(_fixture_cfg(tmp_path))
    result = cfg["alt_fuel_ship_sizing"]
    assert result["summary"]["wind_assist_enabled"] is True
    assert result["summary"]["wind_power_saving_kw"] == pytest.approx(
        485.0476, rel=1e-6
    )
    assert result["summary"]["required_fuel_mass_kg"] < 121000.0
    assert result["wind_assist"]["saving_fraction"] == pytest.approx(
        0.1021153, rel=1e-5
    )
    assert len(result["wind_assist"]["bins"]) == 2


def test_fixture_trade_study_and_tonnage(tmp_path):
    cfg = router(_fixture_cfg(tmp_path))
    result = cfg["alt_fuel_ship_sizing"]

    rows = result["trade_study"]
    assert len(rows) == 6  # 3 speeds x (wind off, wind on)
    by_id = {row["case_id"]: row for row in rows}
    assert by_id["v12kn_nowind"]["required_fuel_mass_kg"] == pytest.approx(
        121000.0, rel=1e-9
    )
    # cube law: P(14) = 4750 * (14/12)^3 = 7542.8 kW
    assert by_id["v14kn_nowind"]["shaft_power_kw"] == pytest.approx(
        4750.0 * (14.0 / 12.0) ** 3, rel=1e-12
    )
    assert all(
        by_id[f"v{s:g}kn_wind"]["required_fuel_mass_kg"]
        < by_id[f"v{s:g}kn_nowind"]["required_fuel_mass_kg"]
        for s in (10, 12, 14)
    )

    # ITC 69: V=60000 -> K1 = 0.2 + 0.02*log10(60000) = 0.2955630,
    # GT = 17733.78; Vc=47000, d=10, D=15 -> factor (40/45)^2, no floors.
    ton = result["tonnage"]
    assert ton["gross_tonnage"] == pytest.approx(17733.7815, rel=1e-8)
    assert ton["draught_depth_factor"] == pytest.approx((40.0 / 45.0) ** 2)
    assert not ton["nt_floor_applied"]
    assert ton["net_tonnage"] == pytest.approx(ton["cargo_term"], rel=1e-12)


def test_router_writes_csv_outputs(tmp_path):
    cfg = router(_fixture_cfg(tmp_path))
    result = cfg["alt_fuel_ship_sizing"]
    stem = FIXTURE.stem
    for suffix, key in (
        ("fuel_chain_summary", "fuel_chain_summary_csv"),
        ("wind_assist_bins", "wind_assist_bins_csv"),
        ("trade_study", "trade_study_csv"),
        ("tonnage", "tonnage_csv"),
    ):
        path = tmp_path / "results" / f"{stem}_{suffix}.csv"
        assert path.exists(), suffix
        assert key in result
    with (tmp_path / "results" / f"{stem}_trade_study.csv").open() as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == 6
    assert float(rows[0]["required_fuel_mass_kg"]) > 0.0


def test_router_optional_blocks_omitted(tmp_path):
    cfg = _no_wind_cfg(tmp_path)
    del cfg["alt_fuel_ship_sizing"]["trade_study"]
    del cfg["alt_fuel_ship_sizing"]["tonnage"]
    result = router(cfg)["alt_fuel_ship_sizing"]
    assert "trade_study" not in result
    assert "tonnage" not in result
    assert "wind_assist" not in result
    assert result["summary"]["wind_assist_enabled"] is False


def test_router_rejects_bad_inputs(tmp_path):
    cfg = _fixture_cfg(tmp_path)
    del cfg["alt_fuel_ship_sizing"]["route"]
    with pytest.raises(ValueError):
        router(cfg)

    cfg = _fixture_cfg(tmp_path)
    cfg["alt_fuel_ship_sizing"]["efficiency"]["fuel_cell_lhv"] = -0.5
    with pytest.raises(ValueError):
        router(cfg)

    cfg = _fixture_cfg(tmp_path)
    cfg["alt_fuel_ship_sizing"]["tank"]["bog_handling"] = "flare"
    with pytest.raises(ValueError):
        router(cfg)

    cfg = _fixture_cfg(tmp_path)
    cfg["alt_fuel_ship_sizing"]["wind_assist"]["wind_rose"] = [
        {"tws_mps": 10.0, "twa_deg": 90.0, "probability": 0.9},
        {"tws_mps": 5.0, "twa_deg": 90.0, "probability": 0.9},
    ]
    with pytest.raises(ValueError):
        router(cfg)


def _legs_cfg(tmp_path, dwell_h=720.0):
    """No-wind fixture with the route split into a voyage profile:
    2000 nm at 12 kn -> port dwell -> 2000 nm at 12 kn."""
    cfg = _no_wind_cfg(tmp_path)
    cfg["alt_fuel_ship_sizing"]["route"] = {
        "service_speed_kn": 12.0,
        "legs": [
            {"type": "sea", "distance_nm": 2000.0, "speed_kn": 12.0},
            {"type": "port", "dwell_h": dwell_h},
            {"type": "sea", "distance_nm": 2000.0, "speed_kn": 12.0},
        ],
    }
    return cfg


def test_voyage_profile_port_dwell_drives_tank_size(tmp_path):
    """The legs route (30-day dwell) needs more fuel and tank than the
    single-passage fixture (121000 kg / 1856.34 m3): hand-verified in the
    library tests as 130618.7 kg / 2003.91 m3 (port BOG accrues with zero
    burn). distance_nm is derived from the sea legs."""
    result = router(_legs_cfg(tmp_path))["alt_fuel_ship_sizing"]
    summary = result["summary"]
    assert summary["voyage_profile"] is True
    assert summary["distance_nm"] == pytest.approx(4000.0)
    assert summary["endurance_hours"] == pytest.approx(4000.0 / 12.0, rel=1e-12)
    assert summary["total_voyage_hours"] == pytest.approx(
        4000.0 / 12.0 + 720.0, rel=1e-12
    )
    assert summary["port_dwell_h"] == pytest.approx(720.0)
    assert summary["required_fuel_mass_kg"] == pytest.approx(130618.687, rel=1e-6)
    assert summary["gross_tank_volume_m3"] == pytest.approx(2003.907, rel=1e-6)
    assert summary["bog_lost_kg"] > 0.0

    legs = result["voyage_legs"]
    assert [leg["leg_type"] for leg in legs] == ["sea", "port", "sea"]
    assert legs[1]["fuel_burned_kg"] == 0.0
    assert legs[1]["bog_lost_kg"] == pytest.approx(summary["bog_lost_kg"], rel=1e-12)


def test_voyage_profile_writes_legs_csv(tmp_path):
    result = router(_legs_cfg(tmp_path))["alt_fuel_ship_sizing"]
    assert "voyage_legs_csv" in result
    path = tmp_path / "results" / f"{FIXTURE.stem}_voyage_legs.csv"
    assert path.exists()
    with path.open() as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == 3
    assert rows[1]["leg_type"] == "port"
    assert float(rows[1]["bog_lost_kg"]) > 0.0


def test_single_passage_route_unchanged_without_legs(tmp_path):
    """Backward compatibility: no ``route.legs`` -> no voyage columns, no
    legs CSV, and the original hand-verified numbers."""
    result = router(_no_wind_cfg(tmp_path))["alt_fuel_ship_sizing"]
    assert "voyage_legs" not in result
    assert "voyage_profile" not in result["summary"]
    assert not (tmp_path / "results" / f"{FIXTURE.stem}_voyage_legs.csv").exists()
    assert result["summary"]["required_fuel_mass_kg"] == pytest.approx(
        121000.0, rel=1e-9
    )


def test_voyage_profile_rejects_bad_legs(tmp_path):
    cfg = _legs_cfg(tmp_path)
    cfg["alt_fuel_ship_sizing"]["route"]["legs"][1] = {"type": "anchorage"}
    with pytest.raises(ValueError, match="sea.*port|port.*sea"):
        router(cfg)

    cfg = _legs_cfg(tmp_path)
    cfg["alt_fuel_ship_sizing"]["route"]["legs"] = [
        {"type": "port", "dwell_h": 24.0}
    ]
    with pytest.raises(ValueError, match="sea leg"):
        router(cfg)

    cfg = _legs_cfg(tmp_path)
    del cfg["alt_fuel_ship_sizing"]["route"]["legs"]
    with pytest.raises(ValueError, match="distance_nm"):
        router(cfg)  # no legs and no distance_nm


def test_fuel_preset_nh3(tmp_path):
    """fuel.preset nh3_refrigerated: 11000 kW LHV x 3.6 / 18.6 MJ/kg =
    2129.03 kg/h; net volume uses the 682 kg/m3 refrigerated density."""
    cfg = _no_wind_cfg(tmp_path)
    cfg["alt_fuel_ship_sizing"]["fuel"] = {
        "preset": "nh3_refrigerated",
        "margin_fraction": 0.10,
    }
    summary = router(cfg)["alt_fuel_ship_sizing"]["summary"]
    assert summary["fuel_mass_flow_kg_per_h"] == pytest.approx(
        2129.0323, rel=1e-7
    )
    assert summary["net_tank_volume_m3"] == pytest.approx(
        summary["required_fuel_mass_kg"] / 682.0, rel=1e-12
    )


def test_fuel_preset_with_explicit_override(tmp_path):
    """Explicit values override the preset: methanol density kept, LHV
    overridden to a round 20.0 MJ/kg."""
    cfg = _no_wind_cfg(tmp_path)
    cfg["alt_fuel_ship_sizing"]["fuel"] = {
        "preset": "methanol",
        "lhv_mj_per_kg": 20.0,
        "margin_fraction": 0.10,
    }
    summary = router(cfg)["alt_fuel_ship_sizing"]["summary"]
    assert summary["fuel_mass_flow_kg_per_h"] == pytest.approx(
        11000.0 * 3.6 / 20.0, rel=1e-12
    )
    assert summary["net_tank_volume_m3"] == pytest.approx(
        summary["required_fuel_mass_kg"] / 791.4, rel=1e-12
    )


def test_unknown_fuel_preset_rejected(tmp_path):
    cfg = _no_wind_cfg(tmp_path)
    cfg["alt_fuel_ship_sizing"]["fuel"] = {"preset": "lng"}
    with pytest.raises(ValueError, match="unknown fuel preset"):
        router(cfg)


def test_engine_registers_basename():
    """The engine dispatch table includes basename alt_fuel_ship_sizing.

    Checked against the source file (not an engine import) to keep this
    test lightweight; the router itself is exercised above.
    """
    import importlib.util

    spec = importlib.util.find_spec("digitalmodel.engine")
    source = Path(spec.origin).read_text(encoding="utf-8", errors="ignore")
    assert 'basename == "alt_fuel_ship_sizing"' in source
    assert "from digitalmodel.alt_fuel_ship_sizing.workflow import" in source
