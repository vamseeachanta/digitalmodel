"""Tests for power flow / load flow calculator.

TDD tests covering Bus/Branch/Network dataclasses, Y-bus assembly,
Newton-Raphson solver, and result formatting.

References
----------
Glover, Sarma & Overbye — Power Systems Analysis and Design, 6th ed.
"""

from __future__ import annotations

import math

import numpy as np
import pytest

from digitalmodel.power.analysis.load_flow import (
    Branch,
    Bus,
    BusType,
    Network,
    PowerFlowResult,
    build_ybus,
    solve_power_flow,
)


# ── BusType enum ──────────────────────────────────────────────────────


class TestBusType:
    def test_bus_type_values(self) -> None:
        assert BusType.SLACK.value == "slack"
        assert BusType.PV.value == "pv"
        assert BusType.PQ.value == "pq"

    def test_bus_type_members(self) -> None:
        assert set(BusType) == {BusType.SLACK, BusType.PV, BusType.PQ}


# ── Bus dataclass ─────────────────────────────────────────────────────


class TestBus:
    def test_bus_defaults(self) -> None:
        bus = Bus(bus_id=1, bus_type=BusType.PQ)
        assert bus.voltage_pu == 1.0
        assert bus.angle_rad == 0.0
        assert bus.p_gen_pu == 0.0
        assert bus.q_gen_pu == 0.0
        assert bus.p_load_pu == 0.0
        assert bus.q_load_pu == 0.0
        assert bus.v_min_pu == 0.9
        assert bus.v_max_pu == 1.1

    def test_bus_custom_values(self) -> None:
        bus = Bus(
            bus_id=2,
            bus_type=BusType.PV,
            voltage_pu=1.05,
            p_gen_pu=0.5,
            v_min_pu=0.95,
            v_max_pu=1.05,
        )
        assert bus.bus_id == 2
        assert bus.bus_type == BusType.PV
        assert bus.voltage_pu == 1.05
        assert bus.p_gen_pu == 0.5

    def test_bus_voltage_min_validation(self) -> None:
        with pytest.raises(ValueError, match="v_min_pu must be positive"):
            Bus(bus_id=1, bus_type=BusType.PQ, v_min_pu=-0.1)

    def test_bus_voltage_range_validation(self) -> None:
        with pytest.raises(ValueError, match="v_min_pu must be <= v_max_pu"):
            Bus(bus_id=1, bus_type=BusType.PQ, v_min_pu=1.1, v_max_pu=0.9)


# ── Branch dataclass ──────────────────────────────────────────────────


class TestBranch:
    def test_branch_defaults(self) -> None:
        br = Branch(from_bus=1, to_bus=2, r_pu=0.01, x_pu=0.1)
        assert br.b_pu == 0.0
        assert br.tap_ratio == 1.0

    def test_branch_with_shunt(self) -> None:
        br = Branch(from_bus=1, to_bus=2, r_pu=0.01, x_pu=0.1, b_pu=0.02)
        assert br.b_pu == 0.02

    def test_branch_zero_impedance_validation(self) -> None:
        with pytest.raises(ValueError, match="impedance.*zero"):
            Branch(from_bus=1, to_bus=2, r_pu=0.0, x_pu=0.0)

    def test_branch_negative_tap_validation(self) -> None:
        with pytest.raises(ValueError, match="tap_ratio must be positive"):
            Branch(from_bus=1, to_bus=2, r_pu=0.01, x_pu=0.1, tap_ratio=-1.0)


# ── Network dataclass ─────────────────────────────────────────────────


class TestNetwork:
    def test_network_construction(self) -> None:
        buses = [
            Bus(bus_id=1, bus_type=BusType.SLACK),
            Bus(bus_id=2, bus_type=BusType.PQ, p_load_pu=0.5, q_load_pu=0.2),
        ]
        branches = [Branch(from_bus=1, to_bus=2, r_pu=0.01, x_pu=0.1)]
        net = Network(buses=buses, branches=branches, base_mva=100.0)
        assert len(net.buses) == 2
        assert len(net.branches) == 1
        assert net.base_mva == 100.0

    def test_network_no_slack_bus(self) -> None:
        buses = [
            Bus(bus_id=1, bus_type=BusType.PQ),
            Bus(bus_id=2, bus_type=BusType.PQ),
        ]
        branches = [Branch(from_bus=1, to_bus=2, r_pu=0.01, x_pu=0.1)]
        with pytest.raises(ValueError, match="exactly one SLACK bus"):
            Network(buses=buses, branches=branches, base_mva=100.0)

    def test_network_multiple_slack_buses(self) -> None:
        buses = [
            Bus(bus_id=1, bus_type=BusType.SLACK),
            Bus(bus_id=2, bus_type=BusType.SLACK),
        ]
        branches = [Branch(from_bus=1, to_bus=2, r_pu=0.01, x_pu=0.1)]
        with pytest.raises(ValueError, match="exactly one SLACK bus"):
            Network(buses=buses, branches=branches, base_mva=100.0)

    def test_network_duplicate_bus_ids(self) -> None:
        buses = [
            Bus(bus_id=1, bus_type=BusType.SLACK),
            Bus(bus_id=1, bus_type=BusType.PQ),
        ]
        branches = [Branch(from_bus=1, to_bus=1, r_pu=0.01, x_pu=0.1)]
        with pytest.raises(ValueError, match="duplicate bus_id"):
            Network(buses=buses, branches=branches, base_mva=100.0)


# ── Y-bus assembly ────────────────────────────────────────────────────


def _make_3bus_network() -> Network:
    """3-bus radial: slack(1) --br1-- PV(2) --br2-- PQ(3)."""
    buses = [
        Bus(bus_id=1, bus_type=BusType.SLACK, voltage_pu=1.0),
        Bus(bus_id=2, bus_type=BusType.PV, voltage_pu=1.02, p_gen_pu=0.4),
        Bus(
            bus_id=3,
            bus_type=BusType.PQ,
            p_load_pu=0.6,
            q_load_pu=0.3,
        ),
    ]
    branches = [
        Branch(from_bus=1, to_bus=2, r_pu=0.02, x_pu=0.06),
        Branch(from_bus=2, to_bus=3, r_pu=0.01, x_pu=0.04),
    ]
    return Network(buses=buses, branches=branches, base_mva=100.0)


class TestYBus:
    def test_ybus_shape(self) -> None:
        net = _make_3bus_network()
        ybus = build_ybus(net)
        assert ybus.shape == (3, 3)

    def test_ybus_symmetry(self) -> None:
        net = _make_3bus_network()
        ybus = build_ybus(net)
        np.testing.assert_array_almost_equal(ybus, ybus.T)

    def test_ybus_diagonal_sum(self) -> None:
        """Row sums of Y-bus equal shunt admittance (zero if no shunt)."""
        net = _make_3bus_network()
        ybus = build_ybus(net)
        # With no shunt elements, row sums should be ~0
        row_sums = ybus.sum(axis=1)
        np.testing.assert_array_almost_equal(row_sums, 0.0, decimal=10)

    def test_ybus_off_diagonal_values(self) -> None:
        """Off-diagonal Y_ij = -y_ij for directly connected buses."""
        net = _make_3bus_network()
        ybus = build_ybus(net)
        # Branch 1-2: z = 0.02 + j0.06, y = 1/z
        z12 = complex(0.02, 0.06)
        y12 = 1.0 / z12
        np.testing.assert_almost_equal(ybus[0, 1], -y12)
        # Buses 0 and 2 not directly connected
        assert abs(ybus[0, 2]) < 1e-12

    def test_ybus_with_shunt(self) -> None:
        """Shunt susceptance adds to diagonal elements."""
        buses = [
            Bus(bus_id=1, bus_type=BusType.SLACK),
            Bus(bus_id=2, bus_type=BusType.PQ, p_load_pu=0.3),
        ]
        branches = [
            Branch(from_bus=1, to_bus=2, r_pu=0.01, x_pu=0.1, b_pu=0.04),
        ]
        net = Network(buses=buses, branches=branches, base_mva=100.0)
        ybus = build_ybus(net)
        z = complex(0.01, 0.1)
        y_series = 1.0 / z
        b_shunt = 0.04
        expected_diag = y_series + 1j * b_shunt / 2
        np.testing.assert_almost_equal(ybus[0, 0], expected_diag)


# ── Newton-Raphson solver ─────────────────────────────────────────────


class TestSolverConvergence:
    def test_3bus_converges(self) -> None:
        net = _make_3bus_network()
        result = solve_power_flow(net)
        assert result.converged is True
        assert result.iterations < 20

    def test_3bus_slack_voltage_fixed(self) -> None:
        net = _make_3bus_network()
        result = solve_power_flow(net)
        slack_result = next(
            r for r in result.bus_results if r["bus_id"] == 1
        )
        assert slack_result["v_pu"] == pytest.approx(1.0, abs=1e-8)
        assert slack_result["angle_deg"] == pytest.approx(0.0, abs=1e-8)

    def test_3bus_pv_voltage_held(self) -> None:
        net = _make_3bus_network()
        result = solve_power_flow(net)
        pv_result = next(
            r for r in result.bus_results if r["bus_id"] == 2
        )
        assert pv_result["v_pu"] == pytest.approx(1.02, abs=1e-6)

    def test_3bus_power_balance(self) -> None:
        """Total generation = total load + losses."""
        net = _make_3bus_network()
        result = solve_power_flow(net)
        total_p_gen = sum(
            r["p_net_pu"] for r in result.bus_results if r["p_net_pu"] > 0
        )
        total_p_load = sum(
            -r["p_net_pu"] for r in result.bus_results if r["p_net_pu"] < 0
        )
        total_p_loss = sum(r["p_loss_pu"] for r in result.branch_results)
        assert total_p_gen == pytest.approx(
            total_p_load + total_p_loss, abs=1e-4
        )


class TestSolverNonConvergence:
    def test_max_iterations_exceeded(self) -> None:
        """With max_iter=1, solver should not converge for nontrivial case."""
        net = _make_3bus_network()
        result = solve_power_flow(net, max_iter=1)
        assert result.converged is False
        assert result.iterations == 1


# ── 5-bus system ──────────────────────────────────────────────────────


def _make_5bus_network() -> Network:
    """5-bus system with slack, 1 PV, 3 PQ buses."""
    buses = [
        Bus(bus_id=1, bus_type=BusType.SLACK, voltage_pu=1.06),
        Bus(bus_id=2, bus_type=BusType.PV, voltage_pu=1.04, p_gen_pu=0.40),
        Bus(bus_id=3, bus_type=BusType.PQ, p_load_pu=0.45, q_load_pu=0.15),
        Bus(bus_id=4, bus_type=BusType.PQ, p_load_pu=0.40, q_load_pu=0.05),
        Bus(bus_id=5, bus_type=BusType.PQ, p_load_pu=0.60, q_load_pu=0.10),
    ]
    branches = [
        Branch(from_bus=1, to_bus=2, r_pu=0.02, x_pu=0.06),
        Branch(from_bus=1, to_bus=3, r_pu=0.08, x_pu=0.24),
        Branch(from_bus=2, to_bus=3, r_pu=0.06, x_pu=0.18),
        Branch(from_bus=2, to_bus=4, r_pu=0.06, x_pu=0.18),
        Branch(from_bus=2, to_bus=5, r_pu=0.04, x_pu=0.12),
        Branch(from_bus=3, to_bus=4, r_pu=0.01, x_pu=0.03),
        Branch(from_bus=4, to_bus=5, r_pu=0.08, x_pu=0.24),
    ]
    return Network(buses=buses, branches=branches, base_mva=100.0)


class TestFiveBusSystem:
    def test_5bus_converges(self) -> None:
        net = _make_5bus_network()
        result = solve_power_flow(net)
        assert result.converged is True
        assert result.iterations < 20

    def test_5bus_voltages_in_range(self) -> None:
        net = _make_5bus_network()
        result = solve_power_flow(net)
        for br in result.bus_results:
            assert 0.8 < br["v_pu"] < 1.2, f"Bus {br['bus_id']} out of range"

    def test_5bus_branch_results_count(self) -> None:
        net = _make_5bus_network()
        result = solve_power_flow(net)
        assert len(result.branch_results) == 7

    def test_5bus_losses_nonnegative(self) -> None:
        net = _make_5bus_network()
        result = solve_power_flow(net)
        for br in result.branch_results:
            assert br["p_loss_pu"] >= -1e-10, (
                f"Branch {br['from_bus']}-{br['to_bus']} negative P loss"
            )


# ── Per-unit conversion ──────────────────────────────────────────────


class TestPerUnit:
    def test_base_mva_scaling(self) -> None:
        """Changing base_mva should not affect per-unit results."""
        buses_100 = [
            Bus(bus_id=1, bus_type=BusType.SLACK),
            Bus(bus_id=2, bus_type=BusType.PQ, p_load_pu=0.5, q_load_pu=0.2),
        ]
        branches_100 = [
            Branch(from_bus=1, to_bus=2, r_pu=0.01, x_pu=0.1),
        ]
        net_100 = Network(
            buses=buses_100, branches=branches_100, base_mva=100.0
        )
        result_100 = solve_power_flow(net_100)

        # Same per-unit values, different base — results should match
        buses_200 = [
            Bus(bus_id=1, bus_type=BusType.SLACK),
            Bus(bus_id=2, bus_type=BusType.PQ, p_load_pu=0.5, q_load_pu=0.2),
        ]
        branches_200 = [
            Branch(from_bus=1, to_bus=2, r_pu=0.01, x_pu=0.1),
        ]
        net_200 = Network(
            buses=buses_200, branches=branches_200, base_mva=200.0
        )
        result_200 = solve_power_flow(net_200)

        for r100, r200 in zip(
            result_100.bus_results, result_200.bus_results
        ):
            assert r100["v_pu"] == pytest.approx(r200["v_pu"], abs=1e-6)
            assert r100["angle_deg"] == pytest.approx(
                r200["angle_deg"], abs=1e-4
            )


# ── Result format ─────────────────────────────────────────────────────


class TestResultFormat:
    def test_bus_result_keys(self) -> None:
        net = _make_3bus_network()
        result = solve_power_flow(net)
        expected_keys = {"bus_id", "v_pu", "angle_deg", "p_net_pu", "q_net_pu"}
        for br in result.bus_results:
            assert set(br.keys()) == expected_keys

    def test_branch_result_keys(self) -> None:
        net = _make_3bus_network()
        result = solve_power_flow(net)
        expected_keys = {
            "from_bus",
            "to_bus",
            "p_flow_pu",
            "q_flow_pu",
            "p_loss_pu",
            "q_loss_pu",
        }
        for br in result.branch_results:
            assert set(br.keys()) == expected_keys

    def test_result_bus_count(self) -> None:
        net = _make_3bus_network()
        result = solve_power_flow(net)
        assert len(result.bus_results) == 3
