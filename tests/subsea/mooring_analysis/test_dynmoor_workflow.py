"""Workflow-level tests for the ``dynmoor_mooring`` routed basename."""

import csv
from pathlib import Path

import pytest

from digitalmodel.dynmoor_mooring.workflow import router


def _base_config(tmp_path: Path) -> dict:
    return {
        "basename": "dynmoor_mooring",
        "_config_dir_path": str(tmp_path),
        "dynmoor_mooring": {
            "vessel": {
                "lbp_ft": 400.0,
                "beam_ft": 100.0,
                "draft_ft": 10.0,
                "displacement_kips": 20000.0,
                "water_depth_ft": 100.0,
            },
            "line_types": [
                {
                    "segments": [
                        {
                            "material": "chain",
                            "length_ft": 1000.0,
                            "weight_air_lb_ft": 60.0,
                            "area_in2": 10.0,
                            "modulus_ksi": 3000.0,
                            "breaking_strength_kips": 500.0,
                            "size_in": 2.5,
                        }
                    ],
                    "anchor": {
                        "weight_kips": 10.0,
                        "mud_efficiency": 1.0,
                        "sand_efficiency": 1.5,
                    },
                    "vertical_distance_anchor_to_fairlead_ft": 100.0,
                    "bottom_friction_percent": 2.0,
                }
            ],
            "lines": [
                {"type": 1, "fairlead_x_ft": 200.0, "fairlead_y_ft": -50.0,
                 "angle_deg": 300.0, "pretension_percent": 15.0},
                {"type": 1, "fairlead_x_ft": 200.0, "fairlead_y_ft": 50.0,
                 "angle_deg": 60.0, "pretension_percent": 15.0},
                {"type": 1, "fairlead_x_ft": -200.0, "fairlead_y_ft": 50.0,
                 "angle_deg": 120.0, "pretension_percent": 15.0},
                {"type": 1, "fairlead_x_ft": -200.0, "fairlead_y_ft": -50.0,
                 "angle_deg": 240.0, "pretension_percent": 15.0},
            ],
            "offsets": [
                {"surge_ft": 0.0, "sway_ft": 0.0, "yaw_deg": 0.0},
                {"surge_ft": 10.0, "sway_ft": 5.0, "yaw_deg": 1.0},
            ],
            "output_dir": "results",
        },
    }


def _read_csv(path: Path) -> list[dict]:
    with path.open() as stream:
        return list(csv.DictReader(stream))


def test_router_static_outputs(tmp_path):
    cfg = router(_base_config(tmp_path))
    out = cfg["dynmoor_mooring"]
    assert out["method"] == "dynmoor_legacy_static_v610"
    assert out["time_domain_ported"] is False

    # pretension summary matches the legacy binary run of the same deck
    line1 = out["lines"][0]
    assert line1["pretension_kips"] == pytest.approx(75.0, abs=0.05)
    assert line1["horizontal_distance_to_anchor_ft"] == pytest.approx(989.6, abs=0.06)
    assert line1["fairlead_angle_deg"] == pytest.approx(21.5, abs=0.01)
    assert line1["anchor_holding_mud_kips"] == 10.0
    assert line1["anchor_holding_sand_kips"] == 15.0

    tables = _read_csv(tmp_path / "results" / "dynmoor_mooring_dynmoor_line_tables.csv")
    assert len(tables) == 50
    assert float(tables[-1]["horizontal_tension_kips"]) == pytest.approx(500.0)

    offsets = _read_csv(tmp_path / "results" / "dynmoor_mooring_dynmoor_offsets.csv")
    assert len(offsets) == 2
    # symmetric spread at zero offset -> zero restoring force
    assert float(offsets[0]["restoring_surge_kips"]) == pytest.approx(0.0, abs=1e-6)
    # positive surge offset -> negative (restoring) surge force
    assert float(offsets[1]["restoring_surge_kips"]) < 0.0


def test_router_passing_ship(tmp_path):
    cfg = _base_config(tmp_path)
    cfg["dynmoor_mooring"]["vessel"].update(
        {"lbp_ft": 600.0, "draft_ft": 30.0, "displacement_kips": 30000.0,
         "water_depth_ft": 50.0}
    )
    cfg["dynmoor_mooring"]["line_types"][0][
        "vertical_distance_anchor_to_fairlead_ft"
    ] = 50.0
    cfg["dynmoor_mooring"]["passing_ship"] = {
        "mode": "parallel",
        "nsteps": 300,
        "start_step": 10,
        "direction": -1.0,
        "speed_knots": 6.0,
        "separation_ft": 300.0,
        "passing_length_ft": 650.0,
        "passing_beam_ft": 100.0,
        "current_knots": 0.5,
        "current_direction_deg": 0.0,
        "moored_midship_area_ft2": 3000.0,
        "passing_midship_area_ft2": 3200.0,
    }
    out = router(cfg)["dynmoor_mooring"]
    rows = _read_csv(tmp_path / "results" / "dynmoor_mooring_dynmoor_passing_ship.csv")
    assert len(rows) == 237  # matches the legacy .PAS row count
    peak = out["passing_ship_peak_abs"]
    # legacy binary printed |sway| up to 37.4 kips near x/L = 0 and |yaw|
    # up to ~3.4e3 kip-ft near |x/L| = 0.36 for this deck
    assert peak["sway_kips"] == pytest.approx(37.4, abs=0.5)
    assert peak["yaw_kip_ft"] == pytest.approx(3400.0, rel=0.05)


def test_router_time_domain_not_ported(tmp_path):
    cfg = _base_config(tmp_path)
    cfg["dynmoor_mooring"]["time_domain"] = {"nsteps": 100}
    with pytest.raises(NotImplementedError, match="time_domain"):
        router(cfg)


def test_router_validation_errors(tmp_path):
    cfg = _base_config(tmp_path)
    del cfg["dynmoor_mooring"]["line_types"]
    with pytest.raises(ValueError, match="line_types"):
        router(cfg)

    cfg = _base_config(tmp_path)
    cfg["dynmoor_mooring"]["lines"][0]["type"] = 4
    with pytest.raises(ValueError, match="type"):
        router(cfg)


def test_engine_routes_basename():
    from digitalmodel import engine as engine_module

    source = Path(engine_module.__file__).read_text()
    assert 'basename == "dynmoor_mooring"' in source
