"""Independent algebra anchors for the legacy routed force contract.

The theory equations are recorded in hydrodynamics/wang_passing_ship.py,
lines 20-46 at the pack pin; public-paper qualification is NOT_EVALUATED.
Synthetic equal hulls have length 2 m and midship area 1 m², separation 1 m,
rho 1 kg/m³, deep water. No empirical hydrodynamic coefficient is supplied.

At zero stagger, S'(x)=-2x and the sway integral is
I = 4 int[-1,1] int[-1,1] x*y/(1+(y-x)^2)^(3/2) dy dx.
Set u=y-x. Symmetry gives
I = 8 int[0,2] (2/3-u+u³/6)/(1+u²)^(3/2) du.
An antiderivative is (2u/3+7/6)/sqrt(1+u²)+sqrt(1+u²)/6;
I = (16*sqrt(5)-32)/3. Thus Y = I/pi for U=1,
and 4*I/pi for U=2. The literal decimals below are evaluations of
these closed forms, independent of the production quadrature. Surge and yaw
vanish by odd symmetry. Tolerances allow numerical quadrature roundoff only.
"""

import csv

import pytest

from digitalmodel.passing_ship_forces.workflow import router


def _config(tmp_path, velocity):
    return {
        "basename": "passing_ship_forces",
        "_config_dir_path": str(tmp_path),
        "passing_ship_forces": {
            "moored_vessel": {"length": 2, "midship_area": 1},
            "passing_vessel": {"length": 2, "midship_area": 1},
            "water_density": 1,
            "passing_velocity": velocity,
            "separation_distance": 1,
            "stagger": {"values": [0]},
            "output_dir": str(tmp_path / "results"),
        },
    }


def test_abeam_closed_integral(tmp_path):
    """Y=(16*sqrt(5)-32)/(3*pi) N; X=N_yaw=0 by odd symmetry."""
    result = router(_config(tmp_path, 1))["passing_ship_forces"]
    row = result["forces"][0]
    assert row["surge_force"] == pytest.approx(0.0, abs=1e-12)
    assert row["sway_force"] == pytest.approx(0.400761445597844, rel=1e-12)
    assert row["yaw_moment"] == pytest.approx(0.0, abs=1e-12)
    assert result["peak_sway_force"] == pytest.approx(0.400761445597844, rel=1e-12)
    assert result["peak_sway_stagger"] == 0.0
    assert result["depth_mode"] == "deep"


def test_doubled_speed_closed_integral(tmp_path):
    """rho*U² multiplies the integral: 4*(16*sqrt(5)-32)/(3*pi) N."""
    result = router(_config(tmp_path, 2))["passing_ship_forces"]
    assert result["forces"][0]["sway_force"] == pytest.approx(
        1.603045782391376, rel=1e-12
    )


def test_stationary_zero_forces_and_csv(tmp_path):
    """At U=0 the common rho*U² factor is zero; every component is zero."""
    cfg = _config(tmp_path, 0)
    cfg["passing_ship_forces"]["stagger"] = {"values": [-1, 0, 1]}
    result = router(cfg)["passing_ship_forces"]
    for row in result["forces"]:
        for key in ("surge_force", "sway_force", "yaw_moment"):
            assert row[key] == pytest.approx(0.0, abs=1e-12)
    path = tmp_path / "results" / "passing_ship_forces_passing_ship_forces.csv"
    with path.open() as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == 3
    assert [float(row["stagger"]) for row in rows] == [-1.0, 0.0, 1.0]
    assert all(float(row["sway_force"]) == 0.0 for row in rows)


def test_normalized_range_literal_staggers(tmp_path):
    """linspace(-1,1,3) times length 2 m gives [-2,0,2] m by hand."""
    cfg = _config(tmp_path, 0)
    cfg["passing_ship_forces"]["stagger"] = {
        "range": {"start": -1, "stop": 1, "num": 3, "normalized": True}
    }
    result = router(cfg)["passing_ship_forces"]
    assert [row["stagger"] for row in result["forces"]] == [-2.0, 0.0, 2.0]
