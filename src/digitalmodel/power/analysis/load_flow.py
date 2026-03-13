"""Power flow (load flow) analysis using Newton-Raphson method.

Solves the AC power flow equations for a network of buses and branches,
computing voltage magnitudes, angles, and power flows.

References
----------
Glover, Sarma & Overbye — Power Systems Analysis and Design, 6th ed.
IEEE Std 399 — Recommended Practice for Industrial and Commercial
Power Systems Analysis (Brown Book).
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from enum import Enum
from typing import Any

import numpy as np


class BusType(Enum):
    """Power system bus classification.

    Attributes
    ----------
    SLACK : str
        Reference bus — fixed voltage magnitude and angle.
    PV : str
        Generator bus — fixed active power and voltage magnitude.
    PQ : str
        Load bus — fixed active and reactive power.
    """

    SLACK = "slack"
    PV = "pv"
    PQ = "pq"


@dataclass
class Bus:
    """Power system bus specification.

    Parameters
    ----------
    bus_id : int
        Unique bus identifier.
    bus_type : BusType
        Bus classification (SLACK, PV, or PQ).
    voltage_pu : float
        Voltage magnitude [per unit].
    angle_rad : float
        Voltage angle [radians].
    p_gen_pu : float
        Active power generation [per unit].
    q_gen_pu : float
        Reactive power generation [per unit].
    p_load_pu : float
        Active power load [per unit].
    q_load_pu : float
        Reactive power load [per unit].
    v_min_pu : float
        Minimum voltage limit [per unit].
    v_max_pu : float
        Maximum voltage limit [per unit].
    """

    bus_id: int
    bus_type: BusType
    voltage_pu: float = 1.0
    angle_rad: float = 0.0
    p_gen_pu: float = 0.0
    q_gen_pu: float = 0.0
    p_load_pu: float = 0.0
    q_load_pu: float = 0.0
    v_min_pu: float = 0.9
    v_max_pu: float = 1.1

    def __post_init__(self) -> None:
        if self.v_min_pu <= 0:
            raise ValueError(
                f"v_min_pu must be positive, got {self.v_min_pu}"
            )
        if self.v_min_pu > self.v_max_pu:
            raise ValueError(
                f"v_min_pu must be <= v_max_pu, got "
                f"{self.v_min_pu} > {self.v_max_pu}"
            )


@dataclass
class Branch:
    """Transmission line or transformer branch.

    Parameters
    ----------
    from_bus : int
        Sending-end bus ID.
    to_bus : int
        Receiving-end bus ID.
    r_pu : float
        Series resistance [per unit].
    x_pu : float
        Series reactance [per unit].
    b_pu : float
        Total line charging susceptance [per unit].
    tap_ratio : float
        Off-nominal tap ratio (1.0 for lines).
    """

    from_bus: int
    to_bus: int
    r_pu: float
    x_pu: float
    b_pu: float = 0.0
    tap_ratio: float = 1.0

    def __post_init__(self) -> None:
        if self.r_pu == 0.0 and self.x_pu == 0.0:
            raise ValueError(
                "Branch impedance cannot be zero (r_pu and x_pu both zero)"
            )
        if self.tap_ratio <= 0:
            raise ValueError(
                f"tap_ratio must be positive, got {self.tap_ratio}"
            )


@dataclass
class Network:
    """Power system network model.

    Parameters
    ----------
    buses : list[Bus]
        List of bus specifications.
    branches : list[Branch]
        List of branch specifications.
    base_mva : float
        System base power [MVA].
    """

    buses: list[Bus]
    branches: list[Branch]
    base_mva: float

    def __post_init__(self) -> None:
        bus_ids = [b.bus_id for b in self.buses]
        if len(bus_ids) != len(set(bus_ids)):
            raise ValueError("Network contains duplicate bus_id values")
        slack_count = sum(
            1 for b in self.buses if b.bus_type == BusType.SLACK
        )
        if slack_count != 1:
            raise ValueError(
                f"Network must have exactly one SLACK bus, found {slack_count}"
            )


@dataclass
class PowerFlowResult:
    """Results from a power flow solution.

    Parameters
    ----------
    converged : bool
        Whether the solver converged within tolerance.
    iterations : int
        Number of iterations performed.
    bus_results : list[dict[str, Any]]
        Per-bus results with keys: bus_id, v_pu, angle_deg,
        p_net_pu, q_net_pu.
    branch_results : list[dict[str, Any]]
        Per-branch results with keys: from_bus, to_bus,
        p_flow_pu, q_flow_pu, p_loss_pu, q_loss_pu.
    """

    converged: bool
    iterations: int
    bus_results: list[dict[str, Any]]
    branch_results: list[dict[str, Any]]


def build_ybus(network: Network) -> np.ndarray:
    """Build the bus admittance matrix (Y-bus).

    Parameters
    ----------
    network : Network
        Power system network model.

    Returns
    -------
    np.ndarray
        Complex admittance matrix of shape (n_bus, n_bus).
    """
    n = len(network.buses)
    bus_id_to_idx = {b.bus_id: i for i, b in enumerate(network.buses)}
    ybus = np.zeros((n, n), dtype=complex)

    for br in network.branches:
        i = bus_id_to_idx[br.from_bus]
        j = bus_id_to_idx[br.to_bus]
        z = complex(br.r_pu, br.x_pu)
        y_series = 1.0 / z
        b_half = 1j * br.b_pu / 2.0
        tap = br.tap_ratio

        # Pi-model with tap ratio
        ybus[i, i] += y_series / (tap * tap) + b_half
        ybus[j, j] += y_series + b_half
        ybus[i, j] -= y_series / tap
        ybus[j, i] -= y_series / tap

    return ybus


def solve_power_flow(
    network: Network,
    tol: float = 1e-6,
    max_iter: int = 50,
) -> PowerFlowResult:
    """Solve AC power flow using Newton-Raphson method.

    Parameters
    ----------
    network : Network
        Power system network model.
    tol : float
        Convergence tolerance for power mismatches [per unit].
    max_iter : int
        Maximum number of iterations.

    Returns
    -------
    PowerFlowResult
        Solution containing bus voltages, angles, and branch flows.
    """
    n = len(network.buses)
    bus_id_to_idx = {b.bus_id: i for i, b in enumerate(network.buses)}
    ybus = build_ybus(network)
    g = ybus.real
    b = ybus.imag

    # Initial voltage vector
    v = np.array([bus.voltage_pu for bus in network.buses])
    theta = np.array([bus.angle_rad for bus in network.buses])

    # Scheduled power injections (generation - load)
    p_sched = np.array(
        [bus.p_gen_pu - bus.p_load_pu for bus in network.buses]
    )
    q_sched = np.array(
        [bus.q_gen_pu - bus.q_load_pu for bus in network.buses]
    )

    # Index sets
    slack_idx = [
        i for i, bus in enumerate(network.buses)
        if bus.bus_type == BusType.SLACK
    ]
    pv_idx = [
        i for i, bus in enumerate(network.buses)
        if bus.bus_type == BusType.PV
    ]
    pq_idx = [
        i for i, bus in enumerate(network.buses)
        if bus.bus_type == BusType.PQ
    ]

    # Indices for unknowns: theta for PV+PQ, V for PQ only
    pv_pq_idx = sorted(pv_idx + pq_idx)

    converged = False
    iteration = 0

    for iteration in range(1, max_iter + 1):
        # Compute power injections from current V, theta
        p_calc = np.zeros(n)
        q_calc = np.zeros(n)
        for i in range(n):
            for j in range(n):
                angle_diff = theta[i] - theta[j]
                p_calc[i] += v[i] * v[j] * (
                    g[i, j] * math.cos(angle_diff)
                    + b[i, j] * math.sin(angle_diff)
                )
                q_calc[i] += v[i] * v[j] * (
                    g[i, j] * math.sin(angle_diff)
                    - b[i, j] * math.cos(angle_diff)
                )

        # Mismatches
        dp = p_sched - p_calc
        dq = q_sched - q_calc

        # Build mismatch vector [dP(pv+pq), dQ(pq)]
        mismatch = np.concatenate([dp[pv_pq_idx], dq[pq_idx]])

        if np.max(np.abs(mismatch)) < tol:
            converged = True
            break

        # Build Jacobian
        n_pv_pq = len(pv_pq_idx)
        n_pq = len(pq_idx)
        j_dim = n_pv_pq + n_pq
        jacobian = np.zeros((j_dim, j_dim))

        # J1: dP/dtheta (pv+pq rows, pv+pq cols)
        for ri, i in enumerate(pv_pq_idx):
            for ci, j in enumerate(pv_pq_idx):
                if i == j:
                    jacobian[ri, ci] = -q_calc[i] - b[i, i] * v[i] * v[i]
                else:
                    angle_diff = theta[i] - theta[j]
                    jacobian[ri, ci] = v[i] * v[j] * (
                        g[i, j] * math.sin(angle_diff)
                        - b[i, j] * math.cos(angle_diff)
                    )

        # J2: dP/dV (pv+pq rows, pq cols)
        for ri, i in enumerate(pv_pq_idx):
            for ci, j in enumerate(pq_idx):
                col = n_pv_pq + ci
                if i == j:
                    jacobian[ri, col] = (
                        p_calc[i] / v[i] + g[i, i] * v[i]
                    )
                else:
                    angle_diff = theta[i] - theta[j]
                    jacobian[ri, col] = v[i] * (
                        g[i, j] * math.cos(angle_diff)
                        + b[i, j] * math.sin(angle_diff)
                    )

        # J3: dQ/dtheta (pq rows, pv+pq cols)
        for ri, i in enumerate(pq_idx):
            row = n_pv_pq + ri
            for ci, j in enumerate(pv_pq_idx):
                if i == j:
                    jacobian[row, ci] = p_calc[i] - g[i, i] * v[i] * v[i]
                else:
                    angle_diff = theta[i] - theta[j]
                    jacobian[row, ci] = -v[i] * v[j] * (
                        g[i, j] * math.cos(angle_diff)
                        + b[i, j] * math.sin(angle_diff)
                    )

        # J4: dQ/dV (pq rows, pq cols)
        for ri, i in enumerate(pq_idx):
            row = n_pv_pq + ri
            for ci, j in enumerate(pq_idx):
                col = n_pv_pq + ci
                if i == j:
                    jacobian[row, col] = (
                        q_calc[i] / v[i] - b[i, i] * v[i]
                    )
                else:
                    angle_diff = theta[i] - theta[j]
                    jacobian[row, col] = v[i] * (
                        g[i, j] * math.sin(angle_diff)
                        - b[i, j] * math.cos(angle_diff)
                    )

        # Solve J * dx = mismatch
        dx = np.linalg.solve(jacobian, mismatch)

        # Update state
        for ci, i in enumerate(pv_pq_idx):
            theta[i] += dx[ci]
        for ci, i in enumerate(pq_idx):
            v[i] += dx[n_pv_pq + ci]

    # Recompute final power injections
    p_calc = np.zeros(n)
    q_calc = np.zeros(n)
    for i in range(n):
        for j in range(n):
            angle_diff = theta[i] - theta[j]
            p_calc[i] += v[i] * v[j] * (
                g[i, j] * math.cos(angle_diff)
                + b[i, j] * math.sin(angle_diff)
            )
            q_calc[i] += v[i] * v[j] * (
                g[i, j] * math.sin(angle_diff)
                - b[i, j] * math.cos(angle_diff)
            )

    # Bus results
    bus_results: list[dict[str, Any]] = []
    for i, bus in enumerate(network.buses):
        bus_results.append({
            "bus_id": bus.bus_id,
            "v_pu": float(v[i]),
            "angle_deg": float(math.degrees(theta[i])),
            "p_net_pu": float(p_calc[i]),
            "q_net_pu": float(q_calc[i]),
        })

    # Branch results
    branch_results: list[dict[str, Any]] = []
    for br in network.branches:
        i = bus_id_to_idx[br.from_bus]
        j = bus_id_to_idx[br.to_bus]
        z = complex(br.r_pu, br.x_pu)
        y_series = 1.0 / z
        b_half = 1j * br.b_pu / 2.0
        tap = br.tap_ratio

        vi = v[i] * np.exp(1j * theta[i])
        vj = v[j] * np.exp(1j * theta[j])

        # Current from i to j
        i_ij = (vi / tap - vj) * y_series + vi * b_half / (tap * tap)
        # Current from j to i
        i_ji = (vj - vi / tap) * y_series + vj * b_half

        s_ij = vi / tap * np.conj(i_ij)
        s_ji = vj * np.conj(i_ji)

        p_loss = float(s_ij.real + s_ji.real)
        q_loss = float(s_ij.imag + s_ji.imag)

        branch_results.append({
            "from_bus": br.from_bus,
            "to_bus": br.to_bus,
            "p_flow_pu": float(s_ij.real),
            "q_flow_pu": float(s_ij.imag),
            "p_loss_pu": p_loss,
            "q_loss_pu": q_loss,
        })

    return PowerFlowResult(
        converged=converged,
        iterations=iteration,
        bus_results=bus_results,
        branch_results=branch_results,
    )
