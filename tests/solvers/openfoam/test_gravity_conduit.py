#!/usr/bin/env python3
"""
ABOUTME: Tests for the gravity-driven conduit exchange model (#1528 slice 2).
Covers signed hydrostatic head, the Cd/A/conduit-loss flow law, flow reversal,
volume/mass conservation under coupled draining, and dry-out/overflow rejection.

The numerical integrator is checked against an independent closed-form Torricelli
draining oracle -- the analytic answer never comes from the code under test. No
OpenFOAM installation or CFD data is required.
"""

import math

import pytest

from digitalmodel.solvers.openfoam.gravity_conduit import (
    ConduitGeometry,
    GravityExchangeResult,
    TankState,
    check_transfer_feasibility,
    conduit_flow_rate,
    signed_hydrostatic_head,
    simulate_gravity_exchange,
    G_STANDARD,
)


# ============================================================================
# TankState geometry + validation
# ============================================================================


def test_tank_state_derived_geometry():
    tank = TankState(volume=6.0, plan_area=4.0, height=3.0, floor_elevation=1.0)
    assert tank.capacity == pytest.approx(12.0)
    assert tank.fill_fraction == pytest.approx(0.5)
    assert tank.liquid_depth == pytest.approx(1.5)          # 6 / 4
    assert tank.surface_elevation == pytest.approx(2.5)     # floor 1.0 + depth 1.5
    assert tank.free_volume == pytest.approx(6.0)


@pytest.mark.parametrize(
    "kwargs",
    [
        dict(volume=-1.0, plan_area=1.0, height=1.0),   # dry-out below empty
        dict(volume=2.0, plan_area=1.0, height=1.0),    # overflow above capacity
        dict(volume=0.5, plan_area=0.0, height=1.0),    # non-physical area
        dict(volume=0.5, plan_area=1.0, height=0.0),    # non-physical height
    ],
)
def test_tank_state_rejects_impossible_states(kwargs):
    with pytest.raises(ValueError):
        TankState(**kwargs)


# ============================================================================
# Signed hydrostatic head
# ============================================================================


def test_head_zero_when_surfaces_level():
    a = TankState(volume=5.0, plan_area=2.0, height=5.0)          # depth 2.5
    b = TankState(volume=2.5, plan_area=1.0, height=5.0)          # depth 2.5
    assert signed_hydrostatic_head(a, b) == pytest.approx(0.0)


def test_head_sign_follows_higher_surface():
    high = TankState(volume=4.0, plan_area=1.0, height=5.0)       # surf 4.0
    low = TankState(volume=1.0, plan_area=1.0, height=5.0)        # surf 1.0
    assert signed_hydrostatic_head(high, low) == pytest.approx(3.0)
    assert signed_hydrostatic_head(low, high) == pytest.approx(-3.0)


def test_head_accounts_for_floor_elevation():
    # Same liquid depth (1 m) but source floor is 2 m higher -> head +2 m.
    src = TankState(volume=1.0, plan_area=1.0, height=5.0, floor_elevation=2.0)
    dst = TankState(volume=1.0, plan_area=1.0, height=5.0, floor_elevation=0.0)
    assert signed_hydrostatic_head(src, dst) == pytest.approx(2.0)


# ============================================================================
# Cd / A / conduit-loss flow law
# ============================================================================


def test_flow_law_matches_orifice_equation():
    conduit = ConduitGeometry(area=0.02, discharge_coefficient=0.6)
    head = 1.5
    expected = 0.6 * 0.02 * math.sqrt(2.0 * G_STANDARD * head)
    assert conduit_flow_rate(head, conduit) == pytest.approx(expected)


def test_zero_head_gives_zero_flow():
    conduit = ConduitGeometry(area=0.02)
    assert conduit_flow_rate(0.0, conduit) == 0.0


def test_minor_and_friction_losses_reduce_effective_cd():
    lossless = ConduitGeometry(area=0.02, discharge_coefficient=1.0)
    # K_total = minor 3.0 + friction f*L/D = 0.02*50/0.1 = 10 -> 13
    lossy = ConduitGeometry(
        area=0.02,
        discharge_coefficient=1.0,
        loss_coefficient=3.0,
        friction_factor=0.02,
        length=50.0,
        diameter=0.1,
    )
    assert lossy.total_loss_coefficient == pytest.approx(13.0)
    assert lossy.effective_discharge_coefficient == pytest.approx(
        1.0 / math.sqrt(1.0 + 13.0)
    )
    # More loss -> strictly less flow for the same head.
    assert abs(conduit_flow_rate(2.0, lossy)) < abs(conduit_flow_rate(2.0, lossless))


def test_conduit_rejects_partial_friction_spec():
    with pytest.raises(ValueError):
        ConduitGeometry(area=0.02, friction_factor=0.02, length=50.0)  # diameter missing


@pytest.mark.parametrize(
    "kwargs",
    [
        dict(area=0.0),
        dict(area=0.02, discharge_coefficient=0.0),
        dict(area=0.02, discharge_coefficient=1.5),
        dict(area=0.02, loss_coefficient=-1.0),
    ],
)
def test_conduit_rejects_impossible_parameters(kwargs):
    with pytest.raises(ValueError):
        ConduitGeometry(**kwargs)


# ============================================================================
# Flow reversal
# ============================================================================


def test_flow_reverses_with_head_sign():
    conduit = ConduitGeometry(area=0.02)
    forward = conduit_flow_rate(1.0, conduit)
    backward = conduit_flow_rate(-1.0, conduit)
    assert forward > 0.0
    assert backward < 0.0
    assert forward == pytest.approx(-backward)          # symmetric magnitude


def test_reversal_flagged_and_conserved_under_oscillating_external_head():
    # A steady geometric head of +0.5 m plus a larger sinusoidal head (roll-like)
    # forces the sign of the driving head -- and thus the flow -- to flip.
    src = TankState(volume=5.0, plan_area=2.0, height=5.0)
    dst = TankState(volume=5.0, plan_area=2.0, height=5.0)
    conduit = ConduitGeometry(area=0.005)
    result = simulate_gravity_exchange(
        src,
        dst,
        conduit,
        duration=20.0,
        dt=0.01,
        external_head=lambda t: 1.0 * math.sin(2.0 * math.pi * t / 5.0),
    )
    assert result.reversed_flow is True
    assert min(result.flow_rate) < 0.0 < max(result.flow_rate)
    # Volume conserved exactly even as the head changes sign.
    total0 = result.source_volume[0] + result.dest_volume[0]
    for vs, vd in zip(result.source_volume, result.dest_volume):
        assert vs + vd == pytest.approx(total0, abs=1e-9)


# ============================================================================
# Volume and mass conservation (+ analytic drain oracle)
# ============================================================================


def test_numeric_drain_matches_torricelli_analytic():
    # Tank draining through a bottom conduit into an effectively fixed reservoir.
    plan_area = 1.0
    h0 = 1.0
    src = TankState(volume=h0 * plan_area, plan_area=plan_area, height=2.0)
    # Huge, near-empty reservoir -> its surface stays ~0, so head ~= source depth.
    reservoir = TankState(volume=0.0, plan_area=1.0e12, height=1.0e6)
    conduit = ConduitGeometry(area=0.01, discharge_coefficient=0.6)

    duration = 30.0
    result = simulate_gravity_exchange(
        src, reservoir, conduit, duration=duration, dt=0.005, density=1000.0
    )

    cd_eff = conduit.effective_discharge_coefficient
    k = cd_eff * conduit.area * math.sqrt(2.0 * G_STANDARD) / (2.0 * plan_area)
    t_empty = math.sqrt(h0) / k
    t = min(duration, 0.9 * t_empty)
    sqrt_h = math.sqrt(h0) - k * t
    h_analytic = sqrt_h * sqrt_h
    transferred_analytic = (h0 - h_analytic) * plan_area

    # Find the simulated transferred volume at time t.
    idx = min(range(len(result.time)), key=lambda i: abs(result.time[i] - t))
    transferred_numeric = result.source_volume[0] - result.source_volume[idx]
    assert transferred_numeric == pytest.approx(transferred_analytic, rel=2e-3)


def test_two_tank_exchange_reaches_level_equilibrium_and_conserves_mass():
    src = TankState(volume=8.0, plan_area=2.0, height=5.0)      # surf 4.0
    dst = TankState(volume=1.0, plan_area=1.0, height=5.0)      # surf 1.0
    conduit = ConduitGeometry(area=0.05)
    density = 1025.0
    result = simulate_gravity_exchange(
        src, dst, conduit, duration=600.0, dt=0.02, density=density
    )

    assert result.termination == "equilibrium"

    # Analytic equilibrium: equal surface elevations, total volume preserved.
    v_total = 8.0 + 1.0
    # floor_s + Vs/As = floor_d + Vd/Ad ; Vs + Vd = v_total ; floors = 0
    #  Vs/2 = (v_total - Vs)/1  -> Vs = 2*v_total/3
    vs_eq = 2.0 * v_total / 3.0
    assert result.source_volume[-1] == pytest.approx(vs_eq, abs=1e-3)

    # Volume + mass conservation.
    assert result.source_volume[-1] + result.dest_volume[-1] == pytest.approx(
        v_total, abs=1e-9
    )
    assert result.mass_residual == pytest.approx(0.0, abs=1e-6)
    assert result.transferred_volume == pytest.approx(8.0 - vs_eq, abs=1e-3)


# ============================================================================
# Dry-out / overflow rejection
# ============================================================================


def test_transfer_feasibility_rejects_dryout_and_overflow():
    src = TankState(volume=2.0, plan_area=1.0, height=10.0)     # 2 m3 available
    dst = TankState(volume=9.0, plan_area=1.0, height=10.0)     # 1 m3 free
    check_transfer_feasibility(src, dst, 1.0)                   # ok: within both
    with pytest.raises(ValueError, match="dry"):
        check_transfer_feasibility(src, dst, 3.0)              # more than source has
    with pytest.raises(ValueError, match="overflow"):
        check_transfer_feasibility(src, dst, 1.5)             # more than dest free vol


def test_simulation_rejects_overflow_when_requested():
    # Elevated source (floor +5 m) drives flow down into a nearly full
    # destination; reject_overflow -> raise before the destination brims over.
    src = TankState(volume=9.0, plan_area=5.0, height=2.0, floor_elevation=5.0)  # surf 6.8
    dst = TankState(volume=1.95, plan_area=1.0, height=2.0)     # 0.05 m3 free, surf 1.95
    conduit = ConduitGeometry(area=0.05)
    with pytest.raises(ValueError, match="overflow"):
        simulate_gravity_exchange(
            src, dst, conduit, duration=100.0, dt=0.01, reject_overflow=True
        )


def test_simulation_rejects_dryout_when_requested():
    # Source empties while the (much lower) reservoir keeps the head positive, so
    # the flow law would keep pulling liquid that no longer exists -> genuine
    # dry-out, distinct from a level equilibrium.
    src = TankState(volume=0.05, plan_area=1.0, height=2.0)                 # surf 0.05
    dst = TankState(volume=0.0, plan_area=1.0e9, height=1.0e3, floor_elevation=-50.0)
    conduit = ConduitGeometry(area=0.02)
    with pytest.raises(ValueError, match="dry"):
        simulate_gravity_exchange(
            src, dst, conduit, duration=100.0, dt=0.01, reject_dryout=True
        )


def test_simulation_clamps_and_flags_dryout_without_reject():
    src = TankState(volume=0.05, plan_area=1.0, height=2.0)
    dst = TankState(volume=0.0, plan_area=1.0e9, height=1.0e3, floor_elevation=-50.0)
    conduit = ConduitGeometry(area=0.02)
    result = simulate_gravity_exchange(
        src, dst, conduit, duration=100.0, dt=0.01, reject_dryout=False
    )
    assert result.termination == "source_dry"
    # Never goes negative and never loses mass.
    assert min(result.source_volume) >= 0.0
    assert result.source_volume[-1] == pytest.approx(0.0, abs=1e-9)
    assert result.source_volume[-1] + result.dest_volume[-1] == pytest.approx(
        0.05, abs=1e-9
    )


def test_no_flow_when_conduit_invert_above_both_surfaces():
    # Conduit connection sits above both liquid surfaces -> not submerged -> no flow.
    src = TankState(volume=1.0, plan_area=1.0, height=10.0)     # surf 1.0
    dst = TankState(volume=0.5, plan_area=1.0, height=10.0)     # surf 0.5
    conduit = ConduitGeometry(area=0.05, invert_elevation=5.0)
    result = simulate_gravity_exchange(src, dst, conduit, duration=50.0, dt=0.05)
    assert result.transferred_volume == pytest.approx(0.0, abs=1e-12)
    assert result.termination == "no_flow"


def test_result_type_is_returned():
    src = TankState(volume=2.0, plan_area=1.0, height=5.0)
    dst = TankState(volume=1.0, plan_area=1.0, height=5.0)
    result = simulate_gravity_exchange(
        src, dst, ConduitGeometry(area=0.02), duration=1.0, dt=0.1
    )
    assert isinstance(result, GravityExchangeResult)
    assert len(result.time) == len(result.flow_rate) == len(result.source_volume)
