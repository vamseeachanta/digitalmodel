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
