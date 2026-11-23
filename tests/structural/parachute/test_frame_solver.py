"""
Tests for 2D frame solver (Child-D).

Verification cases:
1. Simple cantilever beam with tip load (textbook)
2. Two-bar truss (axial only, known solution)
3. GT1R frame equilibrium check (sum reactions = applied load)
"""

import math
import numpy as np
import pytest

from digitalmodel.structural.parachute.frame_solver import (
    FrameElement2D,
    solve_frame,
    FrameResult,
)
from digitalmodel.structural.parachute.frame_model import (
    build_gt1r_frame,
    CHROMOLY_4130,
)


class TestFrameElement2D:
    """Unit tests for 2D frame element stiffness matrix."""

    def test_horizontal_element_shape(self):
        elem = FrameElement2D(
            elem_id=0, node_i=0, node_j=1,
            xi=0.0, yi=0.0, xj=10.0, yj=0.0,
            E=29e6, A=1.0, I=1.0,
        )
        k = elem.local_stiffness()
        assert k.shape == (6, 6)

    def test_stiffness_symmetric(self):
        elem = FrameElement2D(
            elem_id=0, node_i=0, node_j=1,
            xi=0.0, yi=0.0, xj=10.0, yj=0.0,
            E=29e6, A=1.0, I=1.0,
        )
        k = elem.local_stiffness()
        np.testing.assert_allclose(k, k.T, atol=1e-10)

    def test_global_stiffness_symmetric(self):
        elem = FrameElement2D(
            elem_id=0, node_i=0, node_j=1,
            xi=0.0, yi=0.0, xj=10.0, yj=5.0,
            E=29e6, A=1.0, I=1.0,
        )
        kg = elem.global_stiffness()
        np.testing.assert_allclose(kg, kg.T, atol=1e-10)

    def test_horizontal_axial_stiffness(self):
        """EA/L for axial DOF."""
        E, A, L = 29e6, 2.0, 100.0
        elem = FrameElement2D(
            elem_id=0, node_i=0, node_j=1,
            xi=0.0, yi=0.0, xj=L, yj=0.0,
            E=E, A=A, I=1.0,
        )
        k = elem.local_stiffness()
        assert abs(k[0, 0] - E * A / L) < 1.0

    def test_element_length(self):
        elem = FrameElement2D(
            elem_id=0, node_i=0, node_j=1,
            xi=0.0, yi=0.0, xj=3.0, yj=4.0,
            E=29e6, A=1.0, I=1.0,
        )
        assert abs(elem.L - 5.0) < 1e-10

    def test_zero_length_raises(self):
        with pytest.raises(ValueError):
            FrameElement2D(
                elem_id=0, node_i=0, node_j=1,
                xi=5.0, yi=5.0, xj=5.0, yj=5.0,
                E=29e6, A=1.0, I=1.0,
            )


class TestCantileverBeam:
    """
    Cantilever beam with tip load — textbook verification.

    Fixed at left (node 0), free at right (node 1).
    Tip load P applied downward at node 1.

    Expected:
      delta_tip = P * L^3 / (3 * E * I)
      M_fixed = P * L
      V_fixed = P
    """

    def test_tip_deflection(self):
        L = 100.0  # inches
        E = 29e6   # psi
        I = 10.0   # in^4
        A = 5.0    # in^2
        P = -1000.0  # lbs (downward)

        result = solve_frame(
            nodes={0: (0.0, 0.0), 1: (L, 0.0)},
            members=[{"id": 0, "nodes": (0, 1),
                      "section": {"A": A, "I": I}}],
            fixed_nodes=[0],
            forces={1: (0.0, P, 0.0)},
            E=E,
        )

        expected_delta = P * L**3 / (3 * E * I)
        tip_uy = result.displacements[1 * 3 + 1]
        assert abs(tip_uy - expected_delta) / abs(expected_delta) < 0.001

    def test_fixed_end_reaction(self):
        L = 100.0
        E = 29e6
        I = 10.0
        A = 5.0
        P = -1000.0

        result = solve_frame(
            nodes={0: (0.0, 0.0), 1: (L, 0.0)},
            members=[{"id": 0, "nodes": (0, 1),
                      "section": {"A": A, "I": I}}],
            fixed_nodes=[0],
            forces={1: (0.0, P, 0.0)},
            E=E,
        )

        # Fy reaction at node 0 should equal -P (upward)
        ry = result.reactions[0]["fy"]
        assert abs(ry - (-P)) / abs(P) < 0.001

    def test_fixed_end_moment(self):
        L = 100.0
        E = 29e6
        I = 10.0
        A = 5.0
        P = -1000.0

        result = solve_frame(
            nodes={0: (0.0, 0.0), 1: (L, 0.0)},
            members=[{"id": 0, "nodes": (0, 1),
                      "section": {"A": A, "I": I}}],
            fixed_nodes=[0],
            forces={1: (0.0, P, 0.0)},
            E=E,
        )

        # Reaction moment magnitude = |P| * L (sign depends on convention)
        mz = result.reactions[0]["mz"]
        assert abs(abs(mz) - abs(P * L)) / abs(P * L) < 0.01


class TestTwoBarTruss:
    """
    Two-bar truss (V-shape) — axial forces only.

    Node 0 at (0, 0) fixed, Node 1 at (20, 0) fixed,
    Node 2 at (10, -10) with downward load P.

    By symmetry, each bar carries F = P / (2 * sin(45°))
    """

    def test_symmetric_reactions(self):
        P = -5000.0

        result = solve_frame(
            nodes={0: (0.0, 0.0), 1: (20.0, 0.0), 2: (10.0, -10.0)},
            members=[
                {"id": 0, "nodes": (0, 2), "section": {"A": 1.0, "I": 0.1}},
                {"id": 1, "nodes": (1, 2), "section": {"A": 1.0, "I": 0.1}},
            ],
            fixed_nodes=[0, 1],
            forces={2: (0.0, P, 0.0)},
            E=29e6,
        )

        ry0 = result.reactions[0]["fy"]
        ry1 = result.reactions[1]["fy"]
        assert abs(ry0 - ry1) < abs(P) * 0.001
        assert abs(ry0 + ry1 - (-P)) / abs(P) < 0.001


class TestGT1RFrameEquilibrium:
    """
    Verify global equilibrium on the GT1R frame model.
    Sum of reactions must equal applied load (within 0.1%).
    """

    def _solve_gt1r(self, force_lbs):
        model = build_gt1r_frame()
        E = CHROMOLY_4130["E_psi"]

        forces = {model.load_node: (0.0, -force_lbs, 0.0)}

        return solve_frame(
            nodes=model.nodes,
            members=model.members,
            fixed_nodes=model.fixed_nodes,
            forces=forces,
            E=E,
        )

    def test_equilibrium_fy_200mph(self):
        """Approximate drag force at 200 MPH ~ 24,000 lbs."""
        F = 24000.0
        result = self._solve_gt1r(F)

        total_ry = sum(r["fy"] for r in result.reactions.values())
        assert abs(total_ry - F) / F < 0.001

    def test_equilibrium_fx(self):
        """No horizontal load applied — sum of Fx reactions = 0."""
        F = 24000.0
        result = self._solve_gt1r(F)

        total_rx = sum(r["fx"] for r in result.reactions.values())
        assert abs(total_rx) < 1.0

    def test_member_forces_exist(self):
        result = self._solve_gt1r(24000.0)
        assert len(result.member_forces) == 6

    def test_both_load_cases(self):
        """200 and 250 MPH cases both solve without error."""
        for F in [24000.0, 37900.0]:
            result = self._solve_gt1r(F)
            total_ry = sum(r["fy"] for r in result.reactions.values())
            assert abs(total_ry - F) / F < 0.001
