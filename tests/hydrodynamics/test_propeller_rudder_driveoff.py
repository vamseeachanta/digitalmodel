"""Offline drive-off checks against revision 2's independent SI hand values.

The registry parameters describe a preliminary example, not measured data.
Relative tolerances check arithmetic, not hydrodynamic model accuracy.
"""
import csv
import math
import runpy
from dataclasses import FrozenInstanceError, asdict, replace
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics import propeller_rudder as pr

ROOT = Path(__file__).resolve().parents[2]
VESSEL = pr.VesselPropulsion(4.0, (0.35, -0.30), 0.18, 0.25, 0.50)
RUDDER = pr.RudderGeometry(18.0, 1.8, 50.0)
SPEED = 6.173328  # m/s; 12 kn with the workflow's 0.514444 conversion
GRID = [i / 4.0 for i in range(9)]
RATED = {
    "J": 0.5787495, "K_T": 0.17637515, "T": 185123.3574,
    "C_th_primary": 1.099537651, "C_T_fallback": 1.340899574,
    "slipstream_axial": 6.724528305,
    "primary_F_sway_N": -43875.27328, "fallback_F_sway_N": -433377.3073,
}


def point(speed=SPEED, n=2.0, vessel=VESSEL):
    result = pr.driveoff_sweep(speed, [n], 20.0, vessel, RUDDER)
    assert len(result) == 1
    return result[0]


def assert_hand_values(result, expected):
    for name, value in expected.items():
        if "_F_sway_N" in name:
            actual = getattr(result, name.split("_")[0]).F_sway
        else:
            actual = getattr(result, name)
        assert actual == pytest.approx(value, rel=1e-8), name


def test_rated_point_hand_values():
    result = point()
    assert isinstance(result, pr.DriveOffPoint)
    assert result.regime == "ahead" and result.primary_clamped is False
    assert result.V_s == SPEED and result.n == 2.0
    assert result.V_A == pytest.approx(4.629996, rel=1e-8)
    assert_hand_values(result, RATED)
    with pytest.raises(FrozenInstanceError):
        result.n = 3.0


def test_engine_off_point():
    result = point(n=0.0)
    assert result.regime == "engine_off" and result.primary_clamped is False
    assert result.J is result.K_T is result.C_th_primary is result.C_T_fallback is None
    assert result.T == 0.0 and result.slipstream_axial == result.V_A
    assert result.primary.F_sway == 0.0
    assert result.fallback == pr.ad_flat_plate_forces(SPEED, 0.0, 20.0, VESSEL, RUDDER)
    for speed, n in ((-1.0, 0.0), (SPEED, -0.25)):
        with pytest.raises(ValueError):
            point(speed, n)
    assert point(speed=0.0, n=0.0).regime == "engine_off"


def test_bollard_point_is_finite():
    result = point(speed=0.0)
    assert result.regime == "bollard" and result.primary_clamped is False
    assert result.J == 0.0 and result.K_T == 0.35
    assert result.T == pytest.approx(367360.0, rel=1e-8)
    assert result.C_th_primary == result.C_T_fallback == math.inf
    assert result.slipstream_axial == pytest.approx(6.446511447, rel=1e-8)
    assert result.primary.F_sway == pytest.approx(-51514.25, rel=1e-6)
    assert result.fallback is None


def test_bollard_slipstream_continuity():
    thrust, advance = 367360.0, 1e-3
    loading = thrust / (0.5 * 1025.0 * advance**2 * (4.0 * math.pi))
    limit = pr._bollard_slipstream_velocity(thrust, 1025.0, 4.0, 2.0)
    near = pr.actuator_disk_velocity(advance, loading, 2.0, 2.0)
    assert limit == pytest.approx(near, rel=1e-4)


def test_negative_thrust_row():
    result = point(n=0.75)
    assert result.regime == "braking" and result.primary_clamped is False
    assert result.primary.F_sway > 0
    assert_hand_values(result, {
        "J": 1.543332, "K_T": -0.1129996, "T": -16678.74096,
        "C_th_primary": -0.09906315391, "C_T_fallback": -0.1208087243,
        "slipstream_axial": 4.629996, "primary_F_sway_N": 4802.8928,
        "fallback_F_sway_N": -205448.8651,
    })


def test_primary_clamp_flag():
    result = point(speed=4.0 / 0.75, n=1.0, vessel=replace(VESSEL, kt_coeffs=(-2.0,)))
    assert result.J == 1.0 and result.regime == "braking"
    assert result.C_th_primary == pytest.approx(-4.176225707, rel=1e-8)
    assert result.primary_clamped is True
    assert (
        result.primary.F_surge == result.primary.F_sway == result.primary.F_yaw == 0.0
    )


def test_j_to_infinity_limit():
    result = point(n=1e-6)
    assert result.regime == "braking" and result.J > 1e6
    assert abs(result.primary.F_sway) < 0.03
    assert result.slipstream_axial == result.V_A


def test_grid_regimes_and_monotonic_ahead():
    results = pr.driveoff_sweep(SPEED, iter(GRID), 20.0, VESSEL, RUDDER)
    assert [p.n for p in results] == GRID
    expected = ["engine_off"] + ["braking"] * 3 + ["ahead"] * 5
    assert [p.regime for p in results] == expected
    for method in ("primary", "fallback"):
        forces = [
            abs(getattr(p, method).F_sway) for p in results if p.regime == "ahead"
        ]
        assert forces == sorted(forces)


def test_no_nan_anywhere():
    results = pr.driveoff_sweep(SPEED, GRID, 20.0, VESSEL, RUDDER) + [point(speed=0.0)]
    for result in results:
        values = asdict(result)
        for key, value in values.items():
            if result.regime == "bollard" and key in ("C_th_primary", "C_T_fallback"):
                assert value == math.inf
            elif isinstance(value, float):
                assert math.isfinite(value), key
            elif isinstance(value, dict):
                assert all(
                    math.isfinite(v) for v in value.values() if isinstance(v, float)
                )


def test_example_writes_csv_and_plots(tmp_path):
    namespace = runpy.run_path(str(ROOT / "examples/propeller_rudder_driveoff.py"))
    manifest = namespace["main"](output_dir=tmp_path)
    with (tmp_path / "driveoff.csv").open(encoding="utf-8", newline="") as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == 9
    assert [float(row["n"]) for row in rows] == GRID
    for name, value in RATED.items():
        assert float(rows[-1][name]) == pytest.approx(value, rel=1e-8), name
    assert rows[0]["J"] == rows[0]["K_T"] == rows[0]["C_th_primary"] == ""
    assert rows[0]["C_T_fallback"] == ""
    assert rows[0]["label"] == "engine off: propeller drag and windmilling not modelled"
    assert all(
        row["label"] == "K_T <= 0: primary increment reported as computed"
        for row in rows[1:4]
    )
    plots = list(tmp_path.glob("*.png"))
    assert len(plots) == 2
    assert all(
        path.stat().st_size > 0 and path.read_bytes().startswith(b"\x89PNG\r\n\x1a\n")
        for path in plots
    )
    assert {Path(path) for path in manifest.values()} == {
        tmp_path / "driveoff.csv", *plots,
    }
