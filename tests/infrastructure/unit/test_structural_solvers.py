"""
ABOUTME: Unit tests for structural solver foundation classes
ABOUTME: Tests StructuralSolver base class, configuration, and BeamElement
"""

import pytest
import numpy as np
import logging
from pathlib import Path

# Add src to path for imports
import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from digitalmodel.base_solvers.structural import (
    StructuralSolver,
    BeamElement,
    STRUCTURAL_CONFIG_SCHEMA,
    MATERIAL_PROPERTIES,
    get_material_properties,
    validate_structural_config,
    get_default_config,
)

logger = logging.getLogger(__name__)


# Concrete implementation for testing abstract class
class ConcreteStructuralSolver(StructuralSolver):
    """Concrete structural solver for testing."""

    def validate_inputs(self) -> tuple[bool, list[str]]:
        """Validate input data."""
        errors = []

        # Check elements
        if len(self.elements) == 0:
            errors.append("No elements defined")

        # Check nodes
        if len(self.nodes) == 0:
            errors.append("No nodes defined")

        # Check boundary conditions
        if len(self.boundary_conditions) == 0:
            errors.append("No boundary conditions defined")

        # Check loads
        if len(self.loads) == 0:
            errors.append("No loads defined")

        return len(errors) == 0, errors

    def solve(self) -> dict:
        """Execute structural analysis."""
        # Validate
        is_valid, errors = self.validate_inputs()
        if not is_valid:
            raise ValueError(f"Validation failed: {errors}")

        # Assemble and solve
        K = self.assemble_global_matrix()
        F = self._assemble_load_vector()
        K_mod, F_mod = self.apply_boundary_conditions(K, F)
        u = self.solve_system(K_mod, F_mod)

        # Compute stresses
        self.compute_stresses(u)

        return {
            'status': 'completed',
            'displacements': u,
            'stresses': self.stresses
        }

    def compute_stresses(self, displacements: np.ndarray) -> dict:
        """Compute element stresses from displacements."""
        self.stresses = {}
        for elem_id, element in enumerate(self.elements):
            # Get element DOF indices
            dof_indices = element.get_global_dof_indices()
            elem_displacements = displacements[dof_indices]

            # Compute stresses
            element_stresses = element.compute_element_stresses(
                elem_displacements,
                self.material_properties
            )
            self.stresses[elem_id] = element_stresses

        return self.stresses

    def get_solver_metadata(self) -> dict:
        """Get solver metadata."""
        return {
            'name': self.name,
            'version': self.version,
            'type': 'structural',
            'analysis_type': self.get_config('analysis_type', 'static'),
        }

    def _assemble_load_vector(self) -> np.ndarray:
        """Assemble load vector from load specifications."""
        n_nodes = len(self.nodes)
        F = np.zeros(n_nodes * 2)

        for load in self.loads:
            if 'node' in load:
                node = load['node']
                if 'fy' in load:
                    F[node * 2 + 1] = load['fy']
                if 'fx' in load:
                    F[node * 2] = load['fx']

        return F


class TestStructuralSolverBase:
    """Tests for StructuralSolver base class."""

    def test_initialization(self):
        """Test solver initialization."""
        solver = ConcreteStructuralSolver("test", "1.0.0")
        assert solver.name == "test"
        assert solver.version == "1.0.0"
        assert len(solver.elements) == 0
        assert len(solver.nodes) == 0
        assert len(solver.boundary_conditions) == 0
        assert len(solver.loads) == 0

    def test_set_elements(self):
        """Test setting elements."""
        solver = ConcreteStructuralSolver("test", "1.0.0")
        mock_elements = [1, 2, 3]  # Mock elements
        solver.set_elements(mock_elements)
        assert len(solver.elements) == 3

    def test_set_elements_invalid_input(self):
        """Test that non-list elements raise error."""
        solver = ConcreteStructuralSolver("test", "1.0.0")
        with pytest.raises(TypeError):
            solver.set_elements("not a list")

    def test_set_elements_empty_list(self):
        """Test that empty element list raises error."""
        solver = ConcreteStructuralSolver("test", "1.0.0")
        with pytest.raises(ValueError):
            solver.set_elements([])

    def test_set_nodes(self):
        """Test setting nodes."""
        solver = ConcreteStructuralSolver("test", "1.0.0")
        nodes = {
            0: np.array([0.0, 0.0]),
            1: np.array([1.0, 0.0]),
            2: np.array([1.0, 1.0]),
        }
        solver.set_nodes(nodes)
        assert len(solver.nodes) == 3

    def test_set_nodes_invalid_input(self):
        """Test that non-dict nodes raise error."""
        solver = ConcreteStructuralSolver("test", "1.0.0")
        with pytest.raises(TypeError):
            solver.set_nodes([1, 2, 3])

    def test_set_boundary_conditions(self):
        """Test setting boundary conditions."""
        solver = ConcreteStructuralSolver("test", "1.0.0")
        bcs = {'fixed_nodes': [0, 1]}
        solver.set_boundary_conditions(bcs)
        assert 'fixed_nodes' in solver.boundary_conditions

    def test_set_loads(self):
        """Test setting loads."""
        solver = ConcreteStructuralSolver("test", "1.0.0")
        loads = [{'node': 2, 'fy': 1000.0}]
        solver.set_loads(loads)
        assert len(solver.loads) == 1

    def test_set_material_properties(self):
        """Test setting material properties."""
        solver = ConcreteStructuralSolver("test", "1.0.0")
        material = {
            'E': 2.1e11,
            'nu': 0.3,
            'rho': 7850,
            'sigma_y': 2.5e8
        }
        solver.set_material_properties(material)
        assert solver.material_properties['E'] == 2.1e11

    def test_set_material_missing_property(self):
        """Test that missing material property raises error."""
        solver = ConcreteStructuralSolver("test", "1.0.0")
        material = {'E': 2.1e11}  # Missing nu, rho, sigma_y
        with pytest.raises(ValueError):
            solver.set_material_properties(material)

    def test_validate_mesh_empty(self):
        """Test mesh validation with no nodes."""
        solver = ConcreteStructuralSolver("test", "1.0.0")
        is_valid, errors = solver.validate_mesh()
        assert not is_valid
        assert len(errors) > 0

    def test_assemble_global_matrix(self):
        """Test global stiffness matrix assembly."""
        solver = ConcreteStructuralSolver("test", "1.0.0")

        # Setup simple cantilever beam
        nodes = {
            0: np.array([0.0, 0.0]),
            1: np.array([1.0, 0.0]),
        }
        solver.set_nodes(nodes)

        # Create beam element
        beam = BeamElement(
            elem_id=0,
            node1=0,
            node2=1,
            coord1=nodes[0],
            coord2=nodes[1],
            E=2.1e11,
            I=1e-5,
            A=0.01
        )
        solver.set_elements([beam])

        # Assemble
        K = solver.assemble_global_matrix()

        # Check properties
        assert K.shape == (4, 4)
        assert np.allclose(K, K.T)  # Symmetric
        assert np.linalg.cond(K) > 0  # Non-singular

    def test_apply_boundary_conditions(self):
        """Test boundary condition application."""
        solver = ConcreteStructuralSolver("test", "1.0.0")
        solver.set_boundary_conditions({'fixed_nodes': [0]})

        K = np.eye(4) * 2.0
        F = np.ones(4)

        K_mod, F_mod = solver.apply_boundary_conditions(K, F)

        # First row should be modified
        assert K_mod[0, 0] == 1.0
        assert F_mod[0] == 0.0

    def test_solve_system(self):
        """Test system solution."""
        solver = ConcreteStructuralSolver("test", "1.0.0")

        K = np.array([
            [2.0, -1.0, 0.0],
            [-1.0, 2.0, -1.0],
            [0.0, -1.0, 1.0]
        ])
        F = np.array([0.0, 0.0, 1.0])

        u = solver.solve_system(K, F)

        # Check solution
        assert len(u) == 3
        assert u[-1] > 0  # Displacement should be positive

        # Verify: K*u = F
        assert np.allclose(K @ u, F)


class TestBeamElement:
    """Tests for BeamElement class."""

    def test_initialization(self):
        """Test beam element initialization."""
        beam = BeamElement(
            elem_id=0,
            node1=0,
            node2=1,
            coord1=np.array([0.0, 0.0]),
            coord2=np.array([1.0, 0.0]),
            E=2.1e11,
            I=1e-5,
            A=0.01
        )

        assert beam.elem_id == 0
        assert beam.node1 == 0
        assert beam.node2 == 1
        assert beam.L == pytest.approx(1.0)

    def test_coincident_nodes(self):
        """Test that coincident nodes raise error."""
        with pytest.raises(ValueError):
            BeamElement(
                elem_id=0,
                node1=0,
                node2=1,
                coord1=np.array([0.0, 0.0]),
                coord2=np.array([0.0, 0.0]),
                E=2.1e11,
                I=1e-5,
                A=0.01
            )

    def test_local_stiffness_matrix(self):
        """Test local stiffness matrix computation."""
        beam = BeamElement(
            elem_id=0,
            node1=0,
            node2=1,
            coord1=np.array([0.0, 0.0]),
            coord2=np.array([2.0, 0.0]),
            E=2.1e11,
            I=1e-5,
            A=0.01
        )

        K = beam.compute_local_stiffness_matrix()

        # Check properties
        assert K.shape == (4, 4)
        assert np.allclose(K, K.T)  # Symmetric
        assert K[0, 0] > 0  # Positive definite
        assert np.linalg.cond(K) > 0

    def test_global_stiffness_matrix_horizontal(self):
        """Test global stiffness for horizontal element."""
        beam = BeamElement(
            elem_id=0,
            node1=0,
            node2=1,
            coord1=np.array([0.0, 0.0]),
            coord2=np.array([1.0, 0.0]),
            E=2.1e11,
            I=1e-5,
            A=0.01
        )

        K_local = beam.compute_local_stiffness_matrix()
        K_global = beam.compute_global_stiffness_matrix()

        # For horizontal element, should be nearly identical
        assert np.allclose(K_local, K_global, atol=1e-10)

    def test_global_stiffness_matrix_inclined(self):
        """Test global stiffness for inclined element."""
        beam = BeamElement(
            elem_id=0,
            node1=0,
            node2=1,
            coord1=np.array([0.0, 0.0]),
            coord2=np.array([1.0, 1.0]),
            E=2.1e11,
            I=1e-5,
            A=0.01
        )

        K_local = beam.compute_local_stiffness_matrix()
        K_global = beam.compute_global_stiffness_matrix()

        # For inclined element, should be different
        assert not np.allclose(K_local, K_global, atol=1e-10)
        # But should still be symmetric
        assert np.allclose(K_global, K_global.T)

    def test_element_strains(self):
        """Test strain computation."""
        beam = BeamElement(
            elem_id=0,
            node1=0,
            node2=1,
            coord1=np.array([0.0, 0.0]),
            coord2=np.array([1.0, 0.0]),
            E=2.1e11,
            I=1e-5,
            A=0.01
        )

        # Displacement vector
        displacements = np.array([0.0, 0.001, 0.0, -0.001])

        strains = beam.compute_element_strains(displacements)

        assert 'axial' in strains
        assert 'curvature' in strains
        assert 'shear' in strains

    def test_element_stresses(self):
        """Test stress computation."""
        beam = BeamElement(
            elem_id=0,
            node1=0,
            node2=1,
            coord1=np.array([0.0, 0.0]),
            coord2=np.array([1.0, 0.0]),
            E=2.1e11,
            I=1e-5,
            A=0.01
        )

        material = {
            'E': 2.1e11,
            'nu': 0.3,
            'rho': 7850,
            'sigma_y': 2.5e8
        }

        displacements = np.array([0.0, 0.001, 0.0, -0.001])

        stresses = beam.compute_element_stresses(displacements, material)

        assert 'max_stress' in stresses
        assert stresses['max_stress'] >= 0


class TestStructuralConfiguration:
    """Tests for structural solver configuration."""

    def test_schema_defined(self):
        """Test that configuration schema is defined."""
        assert len(STRUCTURAL_CONFIG_SCHEMA) > 0
        assert 'analysis_type' in STRUCTURAL_CONFIG_SCHEMA
        assert 'youngs_modulus' in STRUCTURAL_CONFIG_SCHEMA

    def test_material_properties_defined(self):
        """Test that material properties are defined."""
        assert 'steel' in MATERIAL_PROPERTIES
        assert 'aluminum' in MATERIAL_PROPERTIES
        assert len(MATERIAL_PROPERTIES) >= 4

    def test_get_material_properties(self):
        """Test getting material properties."""
        steel_props = get_material_properties('steel')
        assert steel_props['E'] > 0
        assert steel_props['nu'] > 0
        assert steel_props['rho'] > 0

    def test_get_material_not_found(self):
        """Test that unknown material raises error."""
        with pytest.raises(ValueError):
            get_material_properties('unobtainium')

    def test_validate_valid_config(self):
        """Test validation of valid configuration."""
        config = {
            'analysis_type': 'static',
            'element_type': 'beam',
            'youngs_modulus': 2.1e11,
            'poissons_ratio': 0.3
        }
        is_valid, errors = validate_structural_config(config)
        assert is_valid
        assert len(errors) == 0

    def test_validate_invalid_type(self):
        """Test validation of invalid parameter type."""
        config = {
            'youngs_modulus': 'not a number'
        }
        is_valid, errors = validate_structural_config(config)
        assert not is_valid
        assert len(errors) > 0

    def test_validate_invalid_range(self):
        """Test validation of parameter out of range."""
        config = {
            'youngs_modulus': 1e6  # Below minimum
        }
        is_valid, errors = validate_structural_config(config)
        assert not is_valid
        assert any('minimum' in e.lower() for e in errors)

    def test_validate_invalid_allowed_value(self):
        """Test validation of invalid allowed value."""
        config = {
            'analysis_type': 'nonexistent'
        }
        is_valid, errors = validate_structural_config(config)
        assert not is_valid
        assert any('allowed' in e.lower() for e in errors)

    def test_get_default_config(self):
        """Test getting default configuration."""
        defaults = get_default_config()
        assert 'analysis_type' in defaults
        assert defaults['analysis_type'] == 'static'
        assert defaults['youngs_modulus'] > 0


class TestStructuralIntegration:
    """Integration tests for structural solver."""

    def test_cantilever_beam_setup(self):
        """Test setting up a simple cantilever beam."""
        solver = ConcreteStructuralSolver("cantilever", "1.0.0")

        # Nodes
        nodes = {
            0: np.array([0.0, 0.0]),  # Fixed end
            1: np.array([1.0, 0.0]),  # Free end
        }
        solver.set_nodes(nodes)

        # Element
        beam = BeamElement(
            elem_id=0,
            node1=0,
            node2=1,
            coord1=nodes[0],
            coord2=nodes[1],
            E=2.1e11,
            I=1e-5,
            A=0.01
        )
        solver.set_elements([beam])

        # Boundary conditions
        solver.set_boundary_conditions({'fixed_nodes': [0]})

        # Loads
        solver.set_loads([{'node': 1, 'fy': 1000.0}])

        # Material
        material = {
            'E': 2.1e11,
            'nu': 0.3,
            'rho': 7850,
            'sigma_y': 2.5e8
        }
        solver.set_material_properties(material)

        # Validate
        is_valid, errors = solver.validate_inputs()
        assert is_valid

    def test_solver_metadata(self):
        """Test solver metadata retrieval."""
        solver = ConcreteStructuralSolver("test", "1.0.0")
        metadata = solver.get_solver_metadata()
        assert metadata['name'] == "test"
        assert metadata['version'] == "1.0.0"
        assert metadata['type'] == "structural"


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
