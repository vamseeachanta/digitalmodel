# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for steady axial actuator-disk velocity.

Calculation basis: let U be advance speed, U_d disk speed, U_w far-wake
speed, rho density and A disk area. Momentum conservation and Bernoulli's
pressure jump give T = rho*A*U_d*(U_w-U) = rho*A*(U_w**2-U**2)/2.
Consequently U_d = (U+U_w)/2 and C_T = T/(rho*A*U**2/2) = (U_w/U)**2-1.
The synthetic cases below choose U and U_w first, then hand-calculate C_T
and U_d. No induction helper or implementation expression supplies an oracle.

All dimensional inputs and scale factors are synthetic test choices, not
empirical coefficients or measured data. The 1e-12 relative tolerance allows
floating-point roundoff; the 1e-6 far-wake tolerance is justified by the bound
in test_actuator_disk_far_wake_limit, rather than by model accuracy.
"""

import pytest

from digitalmodel.hydrodynamics.propeller_rudder import actuator_disk_velocity


# Hand calculations using the momentum balance above (speeds in m/s):
# U=4, U_w=8: C_T=(8/4)**2-1=3; U_d=(4+8)/2=6.
# U=3, U_w=9: C_T=(9/3)**2-1=8; U_d=(3+9)/2=6.
# U=5, U_w=6: C_T=(6/5)**2-1=11/25; U_d=(5+6)/2=11/2.
MOMENTUM_CASES = [
    (4.0, 3.0, 6.0, 8.0),
    (3.0, 8.0, 6.0, 9.0),
    (5.0, 11.0 / 25.0, 11.0 / 2.0, 6.0),
]
REL_TOL = 1e-12


@pytest.mark.parametrize("advance,loading,disk_speed,wake_speed", MOMENTUM_CASES)
def test_actuator_disk_zero_thrust_and_disk_plane(
    advance, loading, disk_speed, wake_speed
):
    """Zero momentum addition preserves U; disk speed is the mean of U and U_w."""
    # Synthetic radius 4 m; distances span the disk, one radius and far wake.
    radius = 4.0
    for distance in (0.0, radius, 100.0 * radius):
        assert actuator_disk_velocity(advance, 0.0, distance, radius) == pytest.approx(
            advance, rel=REL_TOL, abs=0.0
        )
    assert actuator_disk_velocity(advance, loading, 0.0, radius) == pytest.approx(
        disk_speed, rel=REL_TOL, abs=0.0
    )
    assert advance < disk_speed < wake_speed


@pytest.mark.parametrize("scale", [1e-3, 0.5, 1.0, 2.0, 1e3])
def test_actuator_disk_geometric_similarity(scale):
    """Scaling both lengths preserves the independently calculated local speed.

    For synthetic x=3 m, R=4 m, the axial geometric fraction is 3/5 by
    the 3-4-5 triangle. The selected spatial interpolation gives
    U(x)=U_d+(U_w-U_d)*3/5=6+(8-6)*3/5=36/5 m/s for the first momentum case.
    The scale choices cover reduction and enlargement across six orders.
    """
    actual = actuator_disk_velocity(4.0, 3.0, scale * 3.0, scale * 4.0)
    assert actual == pytest.approx(36.0 / 5.0, rel=REL_TOL, abs=0.0)


@pytest.mark.parametrize("speed_scale", [0.0, 0.25, 1.0, 3.0])
def test_actuator_disk_linear_advance_velocity_scaling(speed_scale):
    """At fixed C_T and geometry, all speeds scale with the advance velocity.

    Scaling U and U_w together preserves C_T=(U_w/U)**2-1, with thrust
    scaling as speed squared. The geometric-similarity hand calculation gives
    36/5 m/s at U=4 m/s, hence speed_scale*36/5 at the scaled advance speed.
    Zero checks the continuous zero-speed extension at fixed loading.
    The synthetic factors cover zero, reduction, identity and enlargement.
    """
    actual = actuator_disk_velocity(speed_scale * 4.0, 3.0, 3.0, 4.0)
    assert actual == pytest.approx(speed_scale * 36.0 / 5.0, rel=REL_TOL, abs=0.0)


@pytest.mark.parametrize("advance,loading,disk_speed,wake_speed", MOMENTUM_CASES)
def test_actuator_disk_far_wake_limit(advance, loading, disk_speed, wake_speed):
    """Approach the momentum-balance wake speed from below with bounded error.

    For positive x/R, write z=(R/x)**2. The interpolation's remaining
    geometric fraction is 1-1/sqrt(1+z), bounded strictly between 0 and z/2.
    Thus 0 < U_w-U(x) < (U_w-U_d)/(2*(x/R)**2).
    Synthetic distances x/R=10, 100, 1000 test convergence; at the last,
    relative error is below 1/(2*1000**2), which is less than 1e-6.
    """
    radius = 4.0  # Synthetic radius; only x/R governs this limit.
    previous_error = wake_speed - disk_speed
    for distance_ratio in (10.0, 100.0, 1000.0):
        actual = actuator_disk_velocity(
            advance, loading, distance_ratio * radius, radius
        )
        error = wake_speed - actual
        bound = (wake_speed - disk_speed) / (2.0 * distance_ratio**2)
        assert 0.0 < error < bound
        assert error < previous_error
        previous_error = error
    assert actual == pytest.approx(wake_speed, rel=1e-6, abs=0.0)
