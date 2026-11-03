"""Independent synthetic arithmetic for the pinned containment knowledge pack."""

import runpy
from pathlib import Path

import pytest

from digitalmodel.subsea.capping_stack import (
    CappingStack,
    DeploymentScenario,
    check_containment,
    feasibility,
    linear_inflow,
    solve_operating_point,
    staged_shut_in,
)

ROOT = Path(__file__).resolve().parents[3]


def test_pressure_hand_calculation():
    """DOE-HDBK-1012/3-92 HT-03 hydrostatics; NIST SP811 (2008), B.8 g.

    Synthetic p_res=10000000 Pa, rho=200 kg/m3, h=1000 m, RWP=10000000 Pa.
    Head=200*9.80665*1000=1961330 Pa; SIWHP=10000000-1961330=8038670 Pa.
    These are arithmetic inputs, not qualified fluid properties or ratings.
    """
    result = check_containment(10000000, 10000000, 1000, column_density=200)
    assert result.hydrostatic_head == pytest.approx(1961330)
    assert result.siwhp == pytest.approx(8038670)
    assert result.is_contained


def test_lift_hand_calculation():
    """DOE-HDBK-1012/3-92 HT-03 buoyancy; NIST SP811 (2008), B.8 g.

    Synthetic m=200000 kg, rho=1000 kg/m3, V=100 m3, DAF=1, SWL=1961330 N.
    W=(200000-1000*100)*9.80665=980665 N; hook=980665 N; U=0.5.
    Hs=1 m <= limit=2 m. Inputs have no lifting-standard qualification.
    """
    result = feasibility(
        CappingStack(200000, displaced_volume=100),
        DeploymentScenario(1961330, water_depth=100, hs=1, hs_limit=2, daf=1),
        seawater_density=1000,
    )
    assert result.submerged_weight == pytest.approx(980665)
    assert result.hook_load == pytest.approx(980665)
    assert result.lift_utilisation == 0.5
    assert result.feasible


def well_inputs():
    """Synthetic coefficients for the explicitly linear algebra example below."""
    return dict(
        inflow=lambda p: linear_inflow(p, 1000, 0.01),
        reservoir_pressure=1000, column_density=10, vertical_depth=1,
        gravity=10, bore_length=1, bore_diameter=1, friction_factor=0,
        outlet_pressure=100, choke_capacity=lambda p: 0.01 * (p - 100),
    )


def test_operating_point_hand_calculation():
    """TR94-12 (1994) 2.4.1 eq 2-7; DOE-HDBK-1012/3-92 HT-03 eq 3-12.

    Synthetic head=10*10*1=100 Pa, friction=0. q=.01*(1000-p-100);
    specified linear choke q=.01*(p-100). Thus 900-p=p-100, p=500 Pa,
    bottom=600 Pa, q=4 m3/s. The callback is an algebraic fixture, not a valve.
    """
    result = solve_operating_point(opening=1, **well_inputs())
    assert result.wellhead_pressure_pa == pytest.approx(500)
    assert result.bottomhole_pressure_pa == pytest.approx(600)
    assert result.flow_rate_m3_s == pytest.approx(4)
    assert result.ipr_residual_m3_s == pytest.approx(0, abs=1e-12)
    assert result.choke_residual_m3_s == pytest.approx(0, abs=1e-12)


def test_staged_peak_hand_calculation():
    """Independent bound arithmetic; no standard acceptance limit is supplied.

    Previous equilibrium p=500 Pa; closed SIWHP=1000-100=900 Pa. With
    synthetic allowances (500,0) Pa, max(500+500,900+0)=1000 Pa at t=0 s.
    Synthetic RWP=950 Pa fails the peak and passes the 900 Pa terminal state.
    """
    result = staged_shut_in(
        time_s=[0, 1], openings=[1, 0], transient_pressure_allowance_pa=[500, 0],
        rated_working_pressure=950, **well_inputs(),
    )
    assert result.peak_pressure_pa == pytest.approx(1000)
    assert result.peak_time_s == 0
    assert result.terminal_containment.siwhp == pytest.approx(900)
    assert result.terminal_containment.is_contained
    assert not result.peak_containment.is_contained


def test_pack_node_ids_collect(tmp_path):
    collect = runpy.run_path(str(ROOT / "tests/docs/test_agent_pack_node_ids.py"))["collect_pack"]
    collect(ROOT / "src/digitalmodel/subsea/capping_stack/docs/agent-pack.md",
            ROOT, tmp_path / "collection", include_gates=True)
