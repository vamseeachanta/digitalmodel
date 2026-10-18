"""Independent algebra checks; derivations are in docs/reference-cases.md."""

import math

import numpy as np
import pytest

from digitalmodel.power.analysis.load_flow import (
    Branch, Bus, BusType, Network, build_ybus, solve_power_flow,
)


@pytest.mark.parametrize("tap", [0.95, 1.05])
def test_tapped_charging_terminal_powers(tap):
    """Reference equations T1–T3: hand admittance and both terminal powers."""
    net = Network(
        [Bus(1, BusType.SLACK), Bus(2, BusType.PQ, p_load_pu=0.5, q_load_pu=0.2)],
        [Branch(1, 2, 0.01, 0.1, b_pu=0.2, tap_ratio=tap)], 100.0,
    )
    # 1/(.01+j.1) = (100-j1000)/101, b/2=.1 (T1).
    y = complex(100 / 101, -1000 / 101)
    expected = np.array([[(y + 0.1j) / tap**2, -y / tap],
                         [-y / tap, y + 0.1j]])
    np.testing.assert_allclose(build_ybus(net), expected, rtol=0, atol=1e-12)
    result = solve_power_flow(net, tol=1e-12)
    assert result.converged
    slack, load = result.bus_results
    flow = result.branch_results[0]
    u = (load["v_pu"] * complex(math.cos(math.radians(load["angle_deg"])),
                                  math.sin(math.radians(load["angle_deg"]))))
    # T2: current on the pi side; T3: ideal tap conserves complex power.
    series_current = (1 / tap - u) * y
    sending = (1 / tap) * (series_current + 0.1j / tap).conjugate()
    receiving = u * (-series_current + 0.1j * u).conjugate()
    for key, value in [("p_flow_pu", sending.real), ("q_flow_pu", sending.imag),
                       ("p_loss_pu", (sending + receiving).real),
                       ("q_loss_pu", (sending + receiving).imag)]:
        assert flow[key] == pytest.approx(value, rel=0, abs=1e-8)
    assert flow["p_flow_pu"] == pytest.approx(slack["p_net_pu"], rel=0, abs=1e-8)
    assert flow["q_flow_pu"] == pytest.approx(slack["q_net_pu"], rel=0, abs=1e-8)
    assert receiving == pytest.approx(-0.5 - 0.2j, rel=0, abs=1e-8)
    assert receiving == pytest.approx(complex(load["p_net_pu"], load["q_net_pu"]),
                                      rel=0, abs=1e-8)


def reference_network():
    """R1–R3 derive scheduled powers from exact rectangular phasors."""
    return Network(
        [Bus(1, BusType.SLACK), Bus(2, BusType.PV, p_gen_pu=2.0288),
         Bus(3, BusType.PQ, p_load_pu=1.4688, q_load_pu=0.024)],
        [Branch(1, 2, 0, 0.5), Branch(2, 3, 0, 0.25)], 100.0,
    )


def assert_reference_solution(result):
    """R3–R4: compare solved outputs, even when the flag is ignored."""
    expected = [(1.0, 0.0, -0.56, 0.08),
                (1.0, 16.26020470831196, 2.0288, 0.7584),
                (math.sqrt(0.8244), -7.594643368591445, -1.4688, -0.024)]
    for bus, (v, angle, p, q) in zip(result.bus_results, expected, strict=True):
        assert bus["v_pu"] == pytest.approx(v, rel=0, abs=1e-8)
        assert bus["angle_deg"] == pytest.approx(angle, rel=0, abs=1e-6)
        assert bus["p_net_pu"] == pytest.approx(p, rel=0, abs=1e-8)
        assert bus["q_net_pu"] == pytest.approx(q, rel=0, abs=1e-8)


def test_independently_derived_solved_network():
    result = solve_power_flow(reference_network(), tol=1e-12)
    assert result.converged
    assert_reference_solution(result)


def test_reference_criteria_reject_one_iteration():
    result = solve_power_flow(reference_network(), tol=1e-12, max_iter=1)
    assert not result.converged
    with pytest.raises(AssertionError):
        assert_reference_solution(result)
