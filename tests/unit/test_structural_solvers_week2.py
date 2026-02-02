"""
ABOUTME: Unit and integration tests for Phase 2.3 Week 2 solvers
ABOUTME: Tests Von Mises, Buckling solvers, and utility modules
"""

import pytest
import numpy as np
from unittest.mock import MagicMock, patch

# Import solvers
from digitalmodel.base_solvers.structural.stress.von_mises import VonMisesSolver
from digitalmodel.base_solvers.structural.buckling.elastic_buckling import ElasticBucklingSolver
from digitalmodel.base_solvers.structural.utils.matrix_operations import (
    assemble_sparse_matrix,
    extract_submatrix,
    enforce_boundary_conditions,
    condition_number
)
from digitalmodel.base_solvers.structural.utils.post_processing import (
    compute_von_mises,
    compute_safety_factors,
    extract_critical_elements,
    compute_displacement_magnitude,
    compute_strain_energy,
    compute_compliance
)
from digitalmodel.base_solvers.structural.elements.beam_element import BeamElement


# ============================================================================
# TEST: VonMisesSolver
# ============================================================================

class TestVonMisesSolver:
    """Tests for Von Mises stress solver."""

    @pytest.fixture
    def von_mises_solver(self):
        """Create Von Mises solver instance."""
        return VonMisesSolver(name="TestVonMises", version="1.0.0")

    @pytest.fixture
    def setup_cantilever_beam(self, von_mises_solver):
        """Setup simple cantilever beam for testing."""
        solver = von_mises_solver

        # Nodes: {0: [0, 0], 1: [1, 0]} (1m beam) - dictionary format required
        solver.set_nodes({
            0: np.array([0.0, 0.0]),
            1: np.array([1.0, 0.0])
        })

        # Single beam element
        elem = BeamElement(
            elem_id=0,
            node1=0, node2=1,
            coord1=np.array([0.0, 0.0]),
            coord2=np.array([1.0, 0.0]),
            E=2.1e11,
            I=1.0e-5,
            A=1.0e-3
        )
        solver.set_elements([elem])

        # Boundary conditions: fixed at node 0
        solver.set_boundary_conditions({
            'fixed_nodes': [0],
            'pinned_nodes': [],
            'prescribed_displacements': {}
        })

        # Load: point load at node 1
        solver.set_loads([
            {'node': 1, 'direction': 'y', 'magnitude': -1000.0}
        ])

        # Material properties
        solver.set_material_properties({
            'E': 2.1e11,
            'nu': 0.3,
            'rho': 7850,
            'sigma_y': 2.5e8
        })

        return solver

    def test_initialization(self, von_mises_solver):
        """Test Von Mises solver initialization."""
        assert von_mises_solver.name == "TestVonMises"
        assert von_mises_solver.version == "1.0.0"
        assert len(von_mises_solver.stress_results) == 0
        assert len(von_mises_solver.safety_factors) == 0
        assert len(von_mises_solver.critical_elements) == 0
        assert von_mises_solver.max_stress == 0.0
        assert von_mises_solver.min_safety_factor == float('inf')

    def test_validate_inputs_missing_elements(self, von_mises_solver):
        """Test validation fails when elements not defined."""
        von_mises_solver.set_nodes({0: np.array([0.0, 0.0])})
        is_valid, errors = von_mises_solver.validate_inputs()
        assert not is_valid
        assert any("No elements" in e for e in errors)

    def test_validate_inputs_missing_bc(self, von_mises_solver):
        """Test validation fails when boundary conditions not defined."""
        elem = BeamElement(0, 0, 1, np.array([0.0, 0.0]), np.array([1.0, 0.0]), 2.1e11, 1.0e-5, 1.0e-3)
        von_mises_solver.set_elements([elem])
        von_mises_solver.set_nodes({0: np.array([0.0, 0.0]), 1: np.array([1.0, 0.0])})
        is_valid, errors = von_mises_solver.validate_inputs()
        assert not is_valid
        assert any("No boundary conditions" in e for e in errors)

    def test_validate_inputs_missing_material(self, von_mises_solver):
        """Test validation fails when material properties not defined."""
        elem = BeamElement(0, 0, 1, np.array([0.0, 0.0]), np.array([1.0, 0.0]), 2.1e11, 1.0e-5, 1.0e-3)
        von_mises_solver.set_elements([elem])
        von_mises_solver.set_nodes({0: np.array([0.0, 0.0]), 1: np.array([1.0, 0.0])})
        von_mises_solver.set_boundary_conditions({'fixed_nodes': [0], 'pinned_nodes': [], 'prescribed_displacements': {}})
        is_valid, errors = von_mises_solver.validate_inputs()
        assert not is_valid
        assert any("Young's modulus" in e for e in errors)

    def test_valid_inputs(self, setup_cantilever_beam):
        """Test validation passes with complete setup."""
        solver = setup_cantilever_beam
        is_valid, errors = solver.validate_inputs()
        assert is_valid
        assert len(errors) == 0

    def test_solve_cantilever_beam(self, setup_cantilever_beam):
        """Test Von Mises solver on cantilever beam."""
        solver = setup_cantilever_beam
        result = solver.solve()

        assert result['status'] == 'completed'
        assert 'displacements' in result
        assert 'stresses' in result
        assert 'safety_factors' in result
        assert result['max_stress'] > 0
        assert result['min_safety_factor'] > 0

    def test_safety_factors_computation(self, setup_cantilever_beam):
        """Test safety factors are computed correctly."""
        solver = setup_cantilever_beam
        result = solver.solve()

        # SF should be yield_strength / von_mises
        yield_strength = 2.5e8
        for elem_id, stress in result['stresses'].items():
            expected_sf = yield_strength / stress['von_mises']
            assert abs(result['safety_factors'][elem_id] - expected_sf) < 1e-6

    def test_results_summary(self, setup_cantilever_beam):
        """Test results summary generation."""
        solver = setup_cantilever_beam
        solver.solve()
        summary = solver.get_results_summary()

        assert summary['status'] == 'completed'
        assert summary['num_elements'] == 1
        assert summary['num_nodes'] == 2
        assert summary['max_von_mises_stress'] > 0
        assert summary['min_safety_factor'] > 0

    def test_solver_metadata(self, von_mises_solver):
        """Test solver metadata."""
        metadata = von_mises_solver.get_solver_metadata()
        assert metadata['name'] == "TestVonMises"
        assert metadata['version'] == "1.0.0"
        assert metadata['type'] == 'structural'
        assert metadata['analysis_type'] == 'static_linear_elastic'


# ============================================================================
# TEST: ElasticBucklingSolver
# ============================================================================

class TestElasticBucklingSolver:
    """Tests for elastic buckling solver."""

    @pytest.fixture
    def buckling_solver(self):
        """Create buckling solver instance."""
        return ElasticBucklingSolver(name="TestBuckling", version="1.0.0")

    @pytest.fixture
    def setup_column(self, buckling_solver):
        """Setup simple column for buckling analysis."""
        solver = buckling_solver

        # Nodes: {0: [0, 0], 1: [0, 1]} (1m vertical column) - dictionary format required
        solver.set_nodes({
            0: np.array([0.0, 0.0]),
            1: np.array([0.0, 1.0])
        })

        # Single beam element
        elem = BeamElement(
            elem_id=0,
            node1=0, node2=1,
            coord1=np.array([0.0, 0.0]),
            coord2=np.array([0.0, 1.0]),
            E=2.1e11,
            I=1.0e-5,
            A=1.0e-3
        )
        solver.set_elements([elem])

        # Boundary conditions: pinned-pinned
        solver.set_boundary_conditions({
            'fixed_nodes': [0, 1],
            'pinned_nodes': [],
            'prescribed_displacements': {}
        })

        # Axial load at top
        solver.set_loads([
            {'node': 1, 'direction': 'y', 'magnitude': -100000.0}
        ])

        # Material properties
        solver.set_material_properties({
            'E': 2.1e11,
            'nu': 0.3,
            'rho': 7850,
            'sigma_y': 2.5e8
        })

        return solver

    def test_initialization(self, buckling_solver):
        """Test buckling solver initialization."""
        assert buckling_solver.name == "TestBuckling"
        assert buckling_solver.num_modes == 5
        assert len(buckling_solver.eigenvalues) == 0
        assert len(buckling_solver.critical_loads) == 0

    def test_set_num_modes_valid(self, buckling_solver):
        """Test setting number of modes."""
        buckling_solver.set_num_modes(3)
        assert buckling_solver.num_modes == 3

    def test_set_num_modes_invalid(self, buckling_solver):
        """Test invalid number of modes raises error."""
        with pytest.raises(ValueError):
            buckling_solver.set_num_modes(0)

        with pytest.raises(ValueError):
            buckling_solver.set_num_modes(-1)

        with pytest.raises(ValueError):
            buckling_solver.set_num_modes(101)

    def test_validate_inputs_missing_loads(self, buckling_solver):
        """Test validation fails when loads not applied."""
        elem = BeamElement(0, 0, 1, np.array([0.0, 0.0]), np.array([1.0, 0.0]), 2.1e11, 1.0e-5, 1.0e-3)
        buckling_solver.set_elements([elem])
        buckling_solver.set_nodes({0: np.array([0.0, 0.0]), 1: np.array([1.0, 0.0])})
        buckling_solver.set_boundary_conditions({'fixed_nodes': [0], 'pinned_nodes': [], 'prescribed_displacements': {}})

        is_valid, errors = buckling_solver.validate_inputs()
        assert not is_valid
        assert any("No loads" in e for e in errors)

    def test_valid_inputs(self, setup_column):
        """Test validation passes with complete setup."""
        solver = setup_column
        is_valid, errors = solver.validate_inputs()
        assert is_valid
        assert len(errors) == 0

    def test_solve_column_buckling(self, setup_column):
        """Test buckling solver on column."""
        solver = setup_column
        solver.set_num_modes(1)  # Only first mode
        result = solver.solve()

        assert result['status'] == 'completed'
        assert 'critical_loads' in result
        assert 'mode_shapes' in result
        assert 'eigenvalues' in result
        assert result['first_critical_load'] > 0

    def test_critical_loads_positive(self, setup_column):
        """Test critical loads are positive."""
        solver = setup_column
        solver.set_num_modes(3)
        result = solver.solve()

        assert all(load > 0 for load in result['critical_loads'].values())

    def test_first_mode_lowest_load(self, setup_column):
        """Test first buckling load is lowest."""
        solver = setup_column
        solver.set_num_modes(3)
        result = solver.solve()

        critical_loads = list(result['critical_loads'].values())
        for i in range(len(critical_loads) - 1):
            assert critical_loads[i] <= critical_loads[i + 1]

    def test_mode_shapes_vectors(self, setup_column):
        """Test mode shapes are vectors."""
        solver = setup_column
        solver.set_num_modes(2)
        result = solver.solve()

        assert len(result['mode_shapes']) == 2
        for mode_id, shape in result['mode_shapes'].items():
            assert isinstance(shape, np.ndarray)
            assert len(shape) > 0

    def test_solver_metadata(self, buckling_solver):
        """Test buckling solver metadata."""
        metadata = buckling_solver.get_solver_metadata()
        assert metadata['name'] == "TestBuckling"
        assert metadata['type'] == 'structural'
        assert metadata['analysis_type'] == 'linear_buckling'


# ============================================================================
# TEST: Matrix Operations Utilities
# ============================================================================

class TestMatrixOperations:
    """Tests for matrix operation utilities."""

    def test_extract_submatrix(self):
        """Test submatrix extraction."""
        K = np.array([
            [1, 2, 3, 4],
            [2, 5, 6, 7],
            [3, 6, 8, 9],
            [4, 7, 9, 10]
        ])

        free_dof = [0, 2]
        submatrix = extract_submatrix(K, free_dof)

        assert submatrix.shape == (2, 2)
        assert submatrix[0, 0] == 1
        assert submatrix[0, 1] == 3
        assert submatrix[1, 0] == 3
        assert submatrix[1, 1] == 8

    def test_enforce_boundary_conditions(self):
        """Test boundary condition enforcement."""
        K = np.ones((4, 4))
        F = np.array([1.0, 2.0, 3.0, 4.0])
        fixed_dof = [1, 3]

        K_mod, F_mod = enforce_boundary_conditions(K, F, fixed_dof)

        # Check fixed DOFs
        assert K_mod[1, 1] == 1.0
        assert K_mod[3, 3] == 1.0
        assert F_mod[1] == 0.0
        assert F_mod[3] == 0.0

        # Check fixed DOF rows/columns are zeroed
        assert np.all(K_mod[1, [0, 2, 3]] == 0.0)
        assert np.all(K_mod[[0, 2, 3], 1] == 0.0)

    def test_condition_number_identity(self):
        """Test condition number of identity matrix."""
        I = np.eye(5)
        cn = condition_number(I)
        assert abs(cn - 1.0) < 1e-10

    def test_condition_number_ill_conditioned(self):
        """Test condition number of ill-conditioned matrix."""
        # Create ill-conditioned matrix
        A = np.array([
            [1.0, 1.0],
            [1.0, 1.0000001]
        ])
        cn = condition_number(A)
        assert cn > 1e6  # Very high condition number


# ============================================================================
# TEST: Post-Processing Utilities
# ============================================================================

class TestPostProcessingUtilities:
    """Tests for post-processing utilities."""

    def test_compute_von_mises_hydrostatic(self):
        """Test Von Mises stress with hydrostatic loading."""
        # Hydrostatic stress: σ_x = σ_y = σ_z = p
        # Von Mises should be zero
        sigma_vm = compute_von_mises(1e6, 1e6, 1e6)
        assert abs(sigma_vm) < 1e-10

    def test_compute_von_mises_uniaxial(self):
        """Test Von Mises stress with uniaxial loading."""
        # Uniaxial stress: σ_x = σ, others = 0
        # Von Mises = σ
        sigma = 1e8
        sigma_vm = compute_von_mises(sigma, 0.0)
        assert abs(sigma_vm - sigma) < 1e-10

    def test_compute_von_mises_shear(self):
        """Test Von Mises stress with pure shear."""
        # Pure shear: σ_x = 0, σ_y = 0, τ_xy = τ
        # Von Mises = √3 * τ
        tau = 1e8
        sigma_vm = compute_von_mises(0.0, 0.0, 0.0, tau)
        expected = np.sqrt(3) * tau
        assert abs(sigma_vm - expected) < 1e-6

    def test_compute_safety_factors(self):
        """Test safety factor computation."""
        von_mises = {0: 1e8, 1: 2e8, 2: 1e6}
        yield_strength = 2.5e8

        sf = compute_safety_factors(von_mises, yield_strength)

        assert abs(sf[0] - 2.5) < 1e-6
        assert abs(sf[1] - 1.25) < 1e-6
        assert abs(sf[2] - 250.0) < 1e-6

    def test_extract_critical_elements(self):
        """Test critical element extraction."""
        sf = {0: 0.8, 1: 1.2, 2: 0.5, 3: 2.0}
        critical = extract_critical_elements(sf, threshold=1.0)

        assert len(critical) == 2
        assert critical[0]['element_id'] == 2  # Most critical
        assert critical[1]['element_id'] == 0
        assert all(c['safety_factor'] < 1.0 for c in critical)

    def test_compute_displacement_magnitude(self):
        """Test displacement magnitude computation."""
        # Displacement: [1, 1, 2, 2, 3, 3] (3 nodes, 2 DOF each)
        u = np.array([1.0, 1.0, 2.0, 2.0, 3.0, 3.0])
        magnitudes = compute_displacement_magnitude(u, dof_per_node=2)

        assert len(magnitudes) == 3
        assert abs(magnitudes[0] - np.sqrt(2)) < 1e-10
        assert abs(magnitudes[1] - 2*np.sqrt(2)) < 1e-10
        assert abs(magnitudes[2] - 3*np.sqrt(2)) < 1e-10

    def test_compute_strain_energy(self):
        """Test strain energy computation."""
        K = 2.0 * np.eye(3)
        u = np.array([1.0, 2.0, 3.0])

        U = compute_strain_energy(K, u)

        # U = 0.5 * u^T * K * u = 0.5 * (2*1 + 8 + 18) = 14
        expected = 0.5 * (2*1 + 2*4 + 2*9)
        assert abs(U - expected) < 1e-10


# ============================================================================
# TEST: Integration Tests
# ============================================================================

class TestStructuralSolversIntegration:
    """Integration tests for structural solvers."""

    def test_von_mises_to_buckling_workflow(self):
        """Test workflow from Von Mises to buckling analysis."""
        # Setup Von Mises solver
        vm_solver = VonMisesSolver()
        vm_solver.set_nodes({0: np.array([0.0, 0.0]), 1: np.array([1.0, 0.0])})

        elem = BeamElement(0, 0, 1, np.array([0.0, 0.0]), np.array([1.0, 0.0]), 2.1e11, 1.0e-5, 1.0e-3)
        vm_solver.set_elements([elem])
        vm_solver.set_boundary_conditions({'fixed_nodes': [0], 'pinned_nodes': [], 'prescribed_displacements': {}})
        vm_solver.set_loads([{'node': 1, 'direction': 'y', 'magnitude': -1000.0}])
        vm_solver.set_material_properties({'E': 2.1e11, 'nu': 0.3, 'rho': 7850, 'sigma_y': 2.5e8})

        vm_result = vm_solver.solve()
        assert vm_result['status'] == 'completed'

        # Setup buckling solver with same geometry
        bk_solver = ElasticBucklingSolver()
        bk_solver.set_nodes({0: np.array([0.0, 0.0]), 1: np.array([1.0, 0.0])})
        bk_solver.set_elements([elem])
        bk_solver.set_boundary_conditions({'fixed_nodes': [0], 'pinned_nodes': [], 'prescribed_displacements': {}})
        bk_solver.set_loads([{'node': 1, 'direction': 'y', 'magnitude': -100000.0}])
        bk_solver.set_material_properties({'E': 2.1e11, 'nu': 0.3, 'rho': 7850, 'sigma_y': 2.5e8})
        bk_solver.set_num_modes(3)

        bk_result = bk_solver.solve()
        assert bk_result['status'] == 'completed'

    def test_multiple_elements_analysis(self):
        """Test analysis with multiple elements."""
        solver = VonMisesSolver()

        # 2-element beam
        nodes_array = np.array([[0.0, 0.0], [0.5, 0.0], [1.0, 0.0]])
        nodes_dict = {0: nodes_array[0], 1: nodes_array[1], 2: nodes_array[2]}
        solver.set_nodes(nodes_dict)

        elem0 = BeamElement(0, 0, 1, nodes_array[0], nodes_array[1], 2.1e11, 1.0e-5, 1.0e-3)
        elem1 = BeamElement(1, 1, 2, nodes_array[1], nodes_array[2], 2.1e11, 1.0e-5, 1.0e-3)
        solver.set_elements([elem0, elem1])

        solver.set_boundary_conditions({'fixed_nodes': [0], 'pinned_nodes': [], 'prescribed_displacements': {}})
        solver.set_loads([{'node': 2, 'direction': 'y', 'magnitude': -1000.0}])
        solver.set_material_properties({'E': 2.1e11, 'nu': 0.3, 'rho': 7850, 'sigma_y': 2.5e8})

        result = solver.solve()
        assert result['status'] == 'completed'
        assert result['num_elements'] == 2


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
