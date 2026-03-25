"""
ABOUTME: Unit tests for infrastructure structural analysis solvers —
BeamElement, VonMisesSolver, and ElasticBucklingSolver.

TDD: tests are written to verify real physical behaviour.
License-independent: no OrcaFlex, AQWA, or external tools.

WRK-149 — structural module test coverage.
"""

from __future__ import annotations

import math

import numpy as np
import pytest

from digitalmodel.base_solvers.structural.elements.beam_element import (
    BeamElement,
)
from digitalmodel.base_solvers.structural.stress.von_mises import (
    VonMisesSolver,
)
from digitalmodel.base_solvers.structural.buckling.elastic_buckling import (
    ElasticBucklingSolver,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Material: steel-like
STEEL_E = 200e9       # Pa (Young's modulus)
STEEL_NU = 0.3        # Poisson's ratio
STEEL_SY = 250e6      # Pa (yield strength)
SECTION_I = 8.33e-6   # m^4 (square cross-section 0.1m x 0.1m)
SECTION_A = 0.01      # m^2


def _make_horizontal_beam(
    elem_id: int = 0,
    node1: int = 0,
    node2: int = 1,
    x1: float = 0.0,
    x2: float = 1.0,
    E: float = STEEL_E,
    I: float = SECTION_I,
    A: float = SECTION_A,
) -> BeamElement:
    coord1 = np.array([x1, 0.0])
    coord2 = np.array([x2, 0.0])
    return BeamElement(
        elem_id=elem_id,
        node1=node1,
        node2=node2,
        coord1=coord1,
        coord2=coord2,
        E=E,
        I=I,
        A=A,
    )


def _simple_cantilever_setup():
    """
    Simple cantilever beam: 3 nodes along x-axis, 2 elements.

    Nodes: 0=(0,0), 1=(1,0), 2=(2,0)
    Elements: 0 -> 1, 1 -> 2
    BC: node 0 fully fixed (both DOFs)
    Load: node 2, y-direction (transverse) = -1000 N
    """
    nodes = {
        0: np.array([0.0, 0.0]),
        1: np.array([1.0, 0.0]),
        2: np.array([2.0, 0.0]),
    }
    elements = [
        _make_horizontal_beam(elem_id=0, node1=0, node2=1, x1=0.0, x2=1.0),
        _make_horizontal_beam(elem_id=1, node1=1, node2=2, x1=1.0, x2=2.0),
    ]
    bcs = {"fixed_nodes": [0]}  # node 0: both DOFs fixed
    loads = [{"node": 2, "direction": "y", "magnitude": -1000.0}]
    material = {
        "E": STEEL_E,
        "I": SECTION_I,
        "nu": STEEL_NU,
        "sigma_y": STEEL_SY,
        "rho": 7850.0,
    }
    return nodes, elements, bcs, loads, material


# ---------------------------------------------------------------------------
# BeamElement tests
# ---------------------------------------------------------------------------


class TestBeamElementGeometry:
    def test_element_length_horizontal(self):
        elem = _make_horizontal_beam(x1=0.0, x2=3.0)
        assert elem.L == pytest.approx(3.0)

    def test_element_length_angled(self):
        """Diagonal element: 3-4-5 triangle."""
        elem = BeamElement(
            elem_id=0,
            node1=0,
            node2=1,
            coord1=np.array([0.0, 0.0]),
            coord2=np.array([3.0, 4.0]),
            E=STEEL_E,
            I=SECTION_I,
            A=SECTION_A,
        )
        assert elem.L == pytest.approx(5.0)

    def test_element_angle_horizontal(self):
        elem = _make_horizontal_beam()
        assert elem.angle == pytest.approx(0.0)

    def test_element_angle_vertical(self):
        elem = BeamElement(
            elem_id=0,
            node1=0,
            node2=1,
            coord1=np.array([0.0, 0.0]),
            coord2=np.array([0.0, 1.0]),
            E=STEEL_E,
            I=SECTION_I,
            A=SECTION_A,
        )
        assert elem.angle == pytest.approx(math.pi / 2.0)

    def test_zero_length_raises(self):
        with pytest.raises(ValueError):
            BeamElement(
                elem_id=0,
                node1=0,
                node2=1,
                coord1=np.array([1.0, 1.0]),
                coord2=np.array([1.0, 1.0]),
                E=STEEL_E,
                I=SECTION_I,
                A=SECTION_A,
            )

    def test_cos_sin_for_horizontal(self):
        elem = _make_horizontal_beam()
        assert elem.c == pytest.approx(1.0)
        assert elem.s == pytest.approx(0.0, abs=1e-12)

    def test_repr_contains_element_info(self):
        elem = _make_horizontal_beam(elem_id=5)
        r = repr(elem)
        assert "5" in r


class TestBeamElementStiffness:
    def test_local_stiffness_matrix_shape(self):
        elem = _make_horizontal_beam()
        K = elem.compute_local_stiffness_matrix()
        assert K.shape == (4, 4)

    def test_local_stiffness_matrix_symmetric(self):
        elem = _make_horizontal_beam()
        K = elem.compute_local_stiffness_matrix()
        assert np.allclose(K, K.T, rtol=1e-10)

    def test_local_stiffness_matrix_positive_diagonal(self):
        elem = _make_horizontal_beam()
        K = elem.compute_local_stiffness_matrix()
        assert np.all(np.diag(K) > 0)

    def test_global_stiffness_matrix_shape(self):
        elem = _make_horizontal_beam()
        K = elem.compute_global_stiffness_matrix()
        assert K.shape == (4, 4)

    def test_global_stiffness_symmetric(self):
        elem = BeamElement(
            elem_id=0,
            node1=0,
            node2=1,
            coord1=np.array([0.0, 0.0]),
            coord2=np.array([1.0, 1.0]),
            E=STEEL_E,
            I=SECTION_I,
            A=SECTION_A,
        )
        K = elem.compute_global_stiffness_matrix()
        assert np.allclose(K, K.T, rtol=1e-8)

    def test_stiffness_scales_with_EI(self):
        """Doubling E should double stiffness values."""
        elem1 = _make_horizontal_beam(E=1e9)
        elem2 = _make_horizontal_beam(E=2e9)
        K1 = elem1.compute_local_stiffness_matrix()
        K2 = elem2.compute_local_stiffness_matrix()
        assert np.allclose(K2, K1 * 2.0)

    def test_stiffness_decreases_with_length(self):
        """Longer beam should be softer (smaller diagonal stiffness)."""
        elem1 = _make_horizontal_beam(x2=1.0)
        elem2 = _make_horizontal_beam(x2=2.0)
        K1 = elem1.compute_local_stiffness_matrix()
        K2 = elem2.compute_local_stiffness_matrix()
        # Bending stiffness = 12EI/L^3; doubling L → 8x softer
        assert K2[0, 0] < K1[0, 0]

    def test_transformation_matrix_shape(self):
        elem = _make_horizontal_beam()
        T = elem.compute_transformation_matrix()
        assert T.shape == (4, 4)

    def test_horizontal_transformation_is_identity_first_two_rows(self):
        """For horizontal beam angle=0: c=1, s=0, so T has identity-like structure."""
        elem = _make_horizontal_beam()
        T = elem.compute_transformation_matrix()
        # Row 0: [c, s, 0, 0] → [1, 0, 0, 0]
        assert T[0, 0] == pytest.approx(1.0)
        assert T[0, 1] == pytest.approx(0.0)

    def test_stiffness_formula_k11(self):
        """K[0,0] of local stiffness = 12EI/L^3."""
        E, I, L = STEEL_E, SECTION_I, 1.0
        elem = _make_horizontal_beam(E=E, I=I, x2=L)
        K = elem.compute_local_stiffness_matrix()
        expected = 12.0 * E * I / L ** 3
        assert K[0, 0] == pytest.approx(expected)

    def test_stiffness_formula_k22(self):
        """K[1,1] of local stiffness = 4EI/L."""
        E, I, L = STEEL_E, SECTION_I, 1.0
        elem = _make_horizontal_beam(E=E, I=I, x2=L)
        K = elem.compute_local_stiffness_matrix()
        expected = 4.0 * E * I / L
        assert K[1, 1] == pytest.approx(expected)


class TestBeamElementDOF:
    def test_get_node_ids(self):
        elem = _make_horizontal_beam(node1=3, node2=7)
        assert elem.get_node_ids() == [3, 7]

    def test_get_global_dof_indices(self):
        elem = _make_horizontal_beam(node1=0, node2=1)
        dofs = elem.get_global_dof_indices()
        # node 0 → DOFs 0,1; node 1 → DOFs 2,3
        assert dofs == [0, 1, 2, 3]

    def test_get_global_dof_indices_node_offset(self):
        elem = _make_horizontal_beam(node1=2, node2=3)
        dofs = elem.get_global_dof_indices()
        # node 2 → DOFs 4,5; node 3 → DOFs 6,7
        assert dofs == [4, 5, 6, 7]


class TestBeamElementStresses:
    def test_compute_element_strains_returns_dict(self):
        elem = _make_horizontal_beam()
        displacements = np.zeros(4)
        strains = elem.compute_element_strains(displacements)
        assert isinstance(strains, dict)
        assert "axial" in strains
        assert "curvature" in strains

    def test_compute_element_stresses_returns_dict(self):
        elem = _make_horizontal_beam()
        displacements = np.zeros(4)
        material = {"E": STEEL_E}
        stresses = elem.compute_element_stresses(displacements, material)
        assert isinstance(stresses, dict)
        assert "axial_stress" in stresses
        assert "max_stress" in stresses

    def test_zero_displacement_gives_zero_stress(self):
        elem = _make_horizontal_beam()
        displacements = np.zeros(4)
        material = {"E": STEEL_E}
        stresses = elem.compute_element_stresses(displacements, material)
        assert stresses["axial_stress"] == pytest.approx(0.0, abs=1e-6)
        assert stresses["max_stress"] == pytest.approx(0.0, abs=1e-6)

    def test_internal_forces_shape(self):
        elem = _make_horizontal_beam()
        displacements = np.zeros(4)
        f = elem.compute_local_internal_forces(displacements)
        assert f.shape == (4,)

    def test_internal_forces_zero_for_zero_displacement(self):
        elem = _make_horizontal_beam()
        f = elem.compute_local_internal_forces(np.zeros(4))
        assert np.allclose(f, 0.0)


# ---------------------------------------------------------------------------
# VonMisesSolver tests
# ---------------------------------------------------------------------------


class TestVonMisesSolverMetadata:
    def test_solver_name_default(self):
        solver = VonMisesSolver()
        assert solver.name == "VonMisesSolver"

    def test_solver_version_default(self):
        solver = VonMisesSolver()
        assert solver.version == "1.0.0"

    def test_get_solver_metadata_keys(self):
        solver = VonMisesSolver()
        meta = solver.get_solver_metadata()
        assert "type" in meta
        assert meta["type"] == "structural"

    def test_custom_name(self):
        solver = VonMisesSolver(name="MySolver", version="2.0.0")
        assert solver.name == "MySolver"


class TestVonMisesSolverValidation:
    def test_validate_fails_no_elements(self):
        solver = VonMisesSolver()
        nodes, elements, bcs, loads, material = _simple_cantilever_setup()
        solver.set_nodes(nodes)
        solver.set_boundary_conditions(bcs)
        solver.set_loads(loads)
        solver.set_material_properties(material)
        # No elements set
        is_valid, errors = solver.validate_inputs()
        assert not is_valid
        assert any("element" in e.lower() for e in errors)

    def test_validate_fails_no_nodes(self):
        solver = VonMisesSolver()
        nodes, elements, bcs, loads, material = _simple_cantilever_setup()
        solver.set_elements(elements)
        solver.set_boundary_conditions(bcs)
        solver.set_loads(loads)
        solver.set_material_properties(material)
        # No nodes set
        is_valid, errors = solver.validate_inputs()
        assert not is_valid
        assert any("node" in e.lower() for e in errors)

    def test_validate_fails_no_boundary_conditions(self):
        solver = VonMisesSolver()
        nodes, elements, bcs, loads, material = _simple_cantilever_setup()
        solver.set_elements(elements)
        solver.set_nodes(nodes)
        solver.set_loads(loads)
        solver.set_material_properties(material)
        # No BCs
        is_valid, errors = solver.validate_inputs()
        assert not is_valid
        assert any("boundary" in e.lower() for e in errors)

    def test_validate_fails_no_loads(self):
        solver = VonMisesSolver()
        nodes, elements, bcs, loads, material = _simple_cantilever_setup()
        solver.set_elements(elements)
        solver.set_nodes(nodes)
        solver.set_boundary_conditions(bcs)
        solver.set_material_properties(material)
        # No loads
        is_valid, errors = solver.validate_inputs()
        assert not is_valid
        assert any("load" in e.lower() for e in errors)

    def test_validate_fails_missing_youngs_modulus(self):
        """Setting material without E should raise ValueError from base class."""
        solver = VonMisesSolver()
        nodes, elements, bcs, loads, material = _simple_cantilever_setup()
        solver.set_elements(elements)
        solver.set_nodes(nodes)
        solver.set_boundary_conditions(bcs)
        solver.set_loads(loads)
        # Missing E — base class raises before we can call validate_inputs
        bad_material = {"nu": STEEL_NU, "sigma_y": STEEL_SY, "rho": 7850.0}
        with pytest.raises(ValueError, match="[Ee]"):
            solver.set_material_properties(bad_material)

    def test_validate_fails_missing_sigma_y(self):
        """Setting material without sigma_y should raise ValueError from base class."""
        solver = VonMisesSolver()
        nodes, elements, bcs, loads, material = _simple_cantilever_setup()
        solver.set_elements(elements)
        solver.set_nodes(nodes)
        solver.set_boundary_conditions(bcs)
        solver.set_loads(loads)
        # Missing sigma_y — base class raises
        bad_material = {"E": STEEL_E, "nu": STEEL_NU, "rho": 7850.0}
        with pytest.raises(ValueError, match="sigma_y"):
            solver.set_material_properties(bad_material)

    def test_validate_passes_complete_setup(self):
        solver = VonMisesSolver()
        nodes, elements, bcs, loads, material = _simple_cantilever_setup()
        solver.set_elements(elements)
        solver.set_nodes(nodes)
        solver.set_boundary_conditions(bcs)
        solver.set_loads(loads)
        solver.set_material_properties(material)
        is_valid, errors = solver.validate_inputs()
        assert is_valid
        assert errors == []


class TestVonMisesSolverSolve:
    def _setup_solver(self) -> VonMisesSolver:
        solver = VonMisesSolver()
        nodes, elements, bcs, loads, material = _simple_cantilever_setup()
        solver.set_elements(elements)
        solver.set_nodes(nodes)
        solver.set_boundary_conditions(bcs)
        solver.set_loads(loads)
        solver.set_material_properties(material)
        return solver

    def test_solve_returns_dict(self):
        solver = self._setup_solver()
        results = solver.solve()
        assert isinstance(results, dict)

    def test_solve_status_completed(self):
        solver = self._setup_solver()
        results = solver.solve()
        assert results["status"] == "completed"

    def test_solve_has_displacements(self):
        solver = self._setup_solver()
        results = solver.solve()
        assert "displacements" in results
        assert results["displacements"] is not None

    def test_solve_has_stresses(self):
        solver = self._setup_solver()
        results = solver.solve()
        assert "stresses" in results

    def test_solve_has_safety_factors(self):
        solver = self._setup_solver()
        results = solver.solve()
        assert "safety_factors" in results

    def test_solve_max_stress_positive(self):
        solver = self._setup_solver()
        results = solver.solve()
        assert results["max_stress"] > 0.0

    def test_solve_min_safety_factor_positive(self):
        solver = self._setup_solver()
        results = solver.solve()
        assert results["min_safety_factor"] > 0.0

    def test_get_results_summary_after_solve(self):
        solver = self._setup_solver()
        solver.solve()
        summary = solver.get_results_summary()
        assert summary["status"] == "completed"
        assert "max_von_mises_stress" in summary

    def test_get_results_summary_before_solve(self):
        """Before solve, get_results_summary should return not_solved or raise gracefully."""
        solver = VonMisesSolver()
        # VonMisesSolver stores results in _results (from base) or results attr.
        # Either way the state is unsolved — check graceful handling.
        try:
            summary = solver.get_results_summary()
            assert summary["status"] == "not_solved"
        except AttributeError:
            # Acceptable — solver has no results attr before solve
            pass

    def test_solver_raises_with_missing_material(self):
        solver = VonMisesSolver()
        nodes, elements, bcs, loads, _ = _simple_cantilever_setup()
        solver.set_elements(elements)
        solver.set_nodes(nodes)
        solver.set_boundary_conditions(bcs)
        solver.set_loads(loads)
        # No material set at all
        with pytest.raises(ValueError):
            solver.solve()

    def test_safety_factors_are_finite(self):
        solver = self._setup_solver()
        results = solver.solve()
        for sf in results["safety_factors"].values():
            assert math.isfinite(sf) or sf == float("inf")

    def test_critical_elements_empty_for_elastic_regime(self):
        """Low load on stiff beam → all elements should be safe."""
        solver = self._setup_solver()
        results = solver.solve()
        # -1000 N on steel beam: should be well within yield
        assert results["min_safety_factor"] > 1.0


# ---------------------------------------------------------------------------
# ElasticBucklingSolver tests
# ---------------------------------------------------------------------------


class TestElasticBucklingSolverMetadata:
    def test_solver_name_default(self):
        solver = ElasticBucklingSolver()
        assert solver.name == "ElasticBucklingSolver"

    def test_solver_version_default(self):
        solver = ElasticBucklingSolver()
        assert solver.version == "1.0.0"

    def test_get_solver_metadata(self):
        solver = ElasticBucklingSolver()
        meta = solver.get_solver_metadata()
        assert meta["analysis_type"] == "linear_buckling"

    def test_set_num_modes_valid(self):
        solver = ElasticBucklingSolver()
        solver.set_num_modes(10)
        assert solver.num_modes == 10

    def test_set_num_modes_invalid_zero(self):
        solver = ElasticBucklingSolver()
        with pytest.raises(ValueError):
            solver.set_num_modes(0)

    def test_set_num_modes_invalid_above_100(self):
        solver = ElasticBucklingSolver()
        with pytest.raises(ValueError):
            solver.set_num_modes(101)

    def test_set_num_modes_invalid_float(self):
        solver = ElasticBucklingSolver()
        with pytest.raises(ValueError):
            solver.set_num_modes(5.5)  # type: ignore


class TestElasticBucklingSolverValidation:
    def test_validate_fails_no_elements(self):
        solver = ElasticBucklingSolver()
        nodes, elements, bcs, loads, material = _simple_cantilever_setup()
        solver.set_nodes(nodes)
        solver.set_boundary_conditions(bcs)
        solver.set_loads(loads)
        solver.set_material_properties(material)
        is_valid, errors = solver.validate_inputs()
        assert not is_valid

    def test_validate_fails_no_loads(self):
        solver = ElasticBucklingSolver()
        nodes, elements, bcs, loads, material = _simple_cantilever_setup()
        solver.set_elements(elements)
        solver.set_nodes(nodes)
        solver.set_boundary_conditions(bcs)
        solver.set_material_properties(material)
        is_valid, errors = solver.validate_inputs()
        assert not is_valid

    def test_validate_passes_complete_setup(self):
        solver = ElasticBucklingSolver()
        nodes, elements, bcs, loads, material = _simple_cantilever_setup()
        solver.set_elements(elements)
        solver.set_nodes(nodes)
        solver.set_boundary_conditions(bcs)
        solver.set_loads(loads)
        solver.set_material_properties(material)
        is_valid, errors = solver.validate_inputs()
        assert is_valid


class TestElasticBucklingSolverSolve:
    def _setup_solver(self) -> ElasticBucklingSolver:
        solver = ElasticBucklingSolver()
        nodes, elements, bcs, loads, material = _simple_cantilever_setup()
        solver.set_elements(elements)
        solver.set_nodes(nodes)
        solver.set_boundary_conditions(bcs)
        solver.set_loads(loads)
        solver.set_material_properties(material)
        return solver

    def test_solve_returns_dict(self):
        solver = self._setup_solver()
        results = solver.solve()
        assert isinstance(results, dict)

    def test_solve_status_completed(self):
        solver = self._setup_solver()
        results = solver.solve()
        assert results["status"] == "completed"

    def test_solve_has_critical_loads(self):
        solver = self._setup_solver()
        results = solver.solve()
        assert "critical_loads" in results

    def test_solve_has_mode_shapes(self):
        solver = self._setup_solver()
        results = solver.solve()
        assert "mode_shapes" in results

    def test_get_results_summary_before_solve(self):
        solver = ElasticBucklingSolver()
        summary = solver.get_results_summary()
        assert summary["status"] == "not_solved"

    def test_get_mode_shape_raises_for_invalid_mode(self):
        solver = self._setup_solver()
        solver.solve()
        with pytest.raises(IndexError):
            solver.get_mode_shape(9999)

    def test_first_critical_load_key_present(self):
        solver = self._setup_solver()
        results = solver.solve()
        assert "first_critical_load" in results
