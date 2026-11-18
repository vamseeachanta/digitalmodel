# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic checks of the actuator-disk sweep adapter.

Calculation basis: steady axial actuator-disk momentum balance, followed by
finite-aspect-ratio thin-airfoil lift and the selected model's drag projection.
The equations and dimensional hand calculation are stated below; no expected
force is obtained from a physics helper or a named reference fixture.

All fluid, geometry and operating numbers are synthetic choices, not measured
or calibrated data. SI inputs give forces in kg*m/s**2 = N and moments in N*m;
shaft speed is rev/s (90 rpm / 60 = 1.5 rev/s), and angles are supplied in degrees.
The adapter's six-decimal knot convention is 1 kn = 0.514444 m/s; its contract
is stated in propeller_rudder_workflow.py (KNOT_TO_M_PER_S). This conversion
constant is used only to check the speed columns, never to derive forces.
"""

import math
from collections import Counter
from itertools import product

import pytest

from digitalmodel.hydrodynamics.propeller_rudder_workflow import (
    run_propeller_rudder_sweep,
    validate_propeller_rudder_input,
)

# Numerical comparison tolerance: relative roundoff allowance, not model accuracy.
REL_TOL = 1e-12


@pytest.fixture
def synthetic_input(tmp_path):
    """Supply every fluid/geometry input explicitly for the hand calculation.

    Chosen values: rho=1000 kg/m**3, D=2 m, constant K_T=pi/8, wake=1/4,
    rudder area=3 m**2, aspect ratio=2, CG lever arm=5 m, dihedral=60 deg.
    Nonzero thrust deduction=1/5 and interaction coefficient=7/10 are explicit
    inputs required by the adapter; the actuator-disk model does not use them.
    The single operating point is V_s=4 m/s, n=1.5 rev/s, delta=30 deg.
    Output locations are temporary; these tests run the in-memory sweep only.
    """
    return {
        "case": {"id": "synthetic-constant-thrust"},
        "method": "actuator_disk_flat_plate",
        "propulsion": {
            "D_m": 2.0,
            "kt_coeffs": [math.pi / 8.0],
            "thrust_deduction_t": 0.2,
            "wake_fraction_w": 0.25,
            "rudder_propeller_coeff_C_rp": 0.7,
            "rho_kg_m3": 1000.0,
        },
        "rudder": {
            "area_m2": 3.0,
            "aspect_ratio": 2.0,
            "x_from_cg_m": 5.0,
            "dihedral_deg": 60.0,
        },
        "sweep": {
            "ship_speeds": {"units": "m/s", "values": [4.0]},
            "shaft_speeds_rev_s": [1.5],
            "rudder_angles_deg": [30.0],
        },
        "outputs": {"directory": str(tmp_path / "sweep"), "tables": ["csv", "json"]},
    }


def test_actuator_disk_sweep_hand_values(synthetic_input):
    """Check a nonzero returned row against this independent SI calculation.

    V_A = (1 - 1/4)*4 = 3 m/s; J = 3/(1.5*2) = 1.
    T = rho*n**2*D**4*K_T = 1000*(1.5)**2*2**4*pi/8 = 4500*pi N.
    Disc area = pi*D**2/4 = pi m**2, hence C_T = T/(rho*V_A**2*A/2) = 1.
    Momentum balance gives induction a = (sqrt(2)-1)/2. With the selected
    model's rudder spacing x=D/2=1 m and radius R=1 m, x/sqrt(x**2+R**2)
    is 1/sqrt(2), so V_R = 3*(1 + 1/(2*sqrt(2))) m/s.
    Thus q_R = rho*V_R**2/2 = 4500*(9/8 + 1/sqrt(2)) N/m**2.

    At aspect ratio 2, lift slope = 2*pi*2/(2+2) = pi per radian.
    delta=30 deg=pi/6 rad is below the model's 35 deg stall clamp:
    C_L=pi**2/6 and C_D=abs(C_L)*sin(pi/6)=pi**2/12.
    Lift=q_R*3*C_L=q_R*pi**2/2 N; drag=q_R*3*C_D=q_R*pi**2/4 N.
    With cos(60 deg)=1/2, surge=-drag, sway=-lift/2, yaw=5*sway N*m.
    Signs follow the adapter's positive-forward/port/bow-to-port convention.
    """
    config = validate_propeller_rudder_input(synthetic_input)
    result = run_propeller_rudder_sweep(config)

    assert len(result["rows"]) == 1
    row = result["rows"][0]
    q_rudder = 4500.0 * (9.0 / 8.0 + 1.0 / math.sqrt(2.0))
    drag = q_rudder * math.pi**2 / 4.0
    assert row["F_surge_N"] == pytest.approx(-drag, rel=REL_TOL)
    assert row["F_sway_N"] == pytest.approx(-drag, rel=REL_TOL)
    assert row["F_yaw_Nm"] == pytest.approx(-5.0 * drag, rel=REL_TOL)
    assert row["method"] == "actuator_disk_flat_plate"


@pytest.mark.parametrize("speed_units", ["m/s", "kn"])
def test_sweep_cartesian_cases_and_units(synthetic_input, speed_units):
    """Each chosen 2-speed x 3-shaft-speed x 3-angle case must occur once.

    Synthetic axes are speeds (2, 4) in the selected unit, shaft speeds
    (0, 0.75, 1.5) rev/s, and angles (-30, 0, 30) deg. Both input speed-unit
    branches must preserve the paired columns using the stated knot convention.
    The 5 m lever arm converts each returned sway force in N to yaw in N*m;
    the separate hand-value test fixes the absolute force scale.
    """
    speeds = [2.0, 4.0]
    shafts = [0.0, 0.75, 1.5]
    angles = [-30.0, 0.0, 30.0]
    synthetic_input["sweep"] = {
        "ship_speeds": {"units": speed_units, "values": speeds},
        "shaft_speeds_rev_s": shafts,
        "rudder_angles_deg": angles,
    }
    config = validate_propeller_rudder_input(synthetic_input)
    result = run_propeller_rudder_sweep(config)
    rows = result["rows"]

    speed_column = "ship_speed_m_s" if speed_units == "m/s" else "ship_speed_kn"
    actual = Counter(
        (row[speed_column], row["shaft_speed_rev_s"], row["rudder_angle_deg"])
        for row in rows
    )
    assert actual == Counter(product(speeds, shafts, angles))
    assert result["metadata"]["case_id"] == synthetic_input["case"]["id"]
    assert result["metadata"]["method"] == "actuator_disk_flat_plate"
    assert result["metadata"]["units"] == {
        "ship_speed_m_s": "m/s",
        "ship_speed_kn": "kn",
        "shaft_speed_rev_s": "rev/s",
        "rudder_angle_deg": "deg",
        "F_surge_N": "N (positive forward)",
        "F_sway_N": "N (positive to port)",
        "F_yaw_Nm": "N*m (positive bow to port)",
    }
    for row in rows:
        assert row["case_id"] == synthetic_input["case"]["id"]
        assert row["method"] == "actuator_disk_flat_plate"
        assert row["ship_speed_m_s"] == pytest.approx(
            row["ship_speed_kn"] * 0.514444, rel=REL_TOL
        )
        assert row["F_yaw_Nm"] == pytest.approx(5.0 * row["F_sway_N"], rel=REL_TOL)
        assert all(
            math.isfinite(row[key]) for key in ("F_surge_N", "F_sway_N", "F_yaw_Nm")
        )
        assert row["F_surge_N"] <= 0.0
        if row["rudder_angle_deg"] == 0.0:
            assert row["F_surge_N"] == row["F_sway_N"] == row["F_yaw_Nm"] == 0.0
        else:
            assert row["F_sway_N"] * row["rudder_angle_deg"] < 0.0


@pytest.mark.parametrize("ship_speed", [0.0, 4.0], ids=["stationary", "moving"])
@pytest.mark.parametrize(
    "angle_sign", [-1.0, 1.0], ids=["negative-helm", "positive-helm"]
)
def test_sweep_zero_shaft_speed_boundary(synthetic_input, ship_speed, angle_sign):
    """n=0 has no thrust acceleration but retains wake-reduced ship inflow.

    For the chosen moving case, V_R=4*(1-1/4)=3 m/s and q_R=4500 N/m**2.
    At rest, the existing model regularizes V_R to 1e-12 m/s, so its pressure
    is 1000*(1e-12)**2/2 N/m**2 and forces are tiny rather than exactly zero.
    This floor records the existing ad_flat_plate_forces boundary, not a
    physical engine-off resistance or windmilling model. For +/-30 deg the
    hand-derived drag and |sway| are q_R*pi**2/4 N; |yaw| is five times that.
    Relative-only comparisons ensure the stationary floor is actually checked.
    """
    synthetic_input["sweep"] = {
        "ship_speeds": {"units": "m/s", "values": [ship_speed]},
        "shaft_speeds_rev_s": [0.0],
        "rudder_angles_deg": [angle_sign * 30.0],
    }
    config = validate_propeller_rudder_input(synthetic_input)
    rows = run_propeller_rudder_sweep(config)["rows"]

    assert len(rows) == 1
    row = rows[0]
    q_rudder = 4500.0 if ship_speed else 1000.0 * (1e-12)**2 / 2.0
    drag = q_rudder * math.pi**2 / 4.0
    assert row["shaft_speed_rev_s"] == 0.0
    assert row["ship_speed_m_s"] == ship_speed
    assert row["F_surge_N"] == pytest.approx(-drag, rel=REL_TOL, abs=0.0)
    assert row["F_sway_N"] == pytest.approx(-angle_sign * drag, rel=REL_TOL, abs=0.0)
    assert row["F_yaw_Nm"] == pytest.approx(
        -angle_sign * 5.0 * drag, rel=REL_TOL, abs=0.0
    )
