#!/usr/bin/env python3
"""
ABOUTME: Unit tests for GMSH meshing module covering mesh generation, quality
analysis, and mesh statistics calculations.
"""

import pytest
import numpy as np

from digitalmodel.gmsh_meshing.models import (
    MeshQuality,
    MeshStatistics,
    GeometryType,
    ElementType,
    MeshAlgorithm,
)
from digitalmodel.gmsh_meshing.quality_analyzer import MeshQualityAnalyzer

# Skip GMSH tests if not installed
try:
    from digitalmodel.gmsh_meshing.mesh_generator import GMSHMeshGenerator, GMSH_AVAILABLE
except ImportError:
    GMSH_AVAILABLE = False


# ============================================================================
# Models Tests
# ============================================================================

class TestMeshQuality:
    """Test mesh quality model"""

    def test_quality_creation(self):
        """Test mesh quality creation"""
        quality = MeshQuality(
            min_jacobian=0.5,
            max_jacobian=1.0,
            mean_jacobian=0.75,
            min_aspect_ratio=1.0,
            max_aspect_ratio=3.0,
            mean_aspect_ratio=2.0,
            min_skewness=0.0,
            max_skewness=0.5,
            mean_skewness=0.25
        )

        assert quality.is_good
        assert quality.is_acceptable
        assert quality.quality_score > 80.0

    def test_poor_quality(self):
        """Test poor quality detection"""
        quality = MeshQuality(
            min_jacobian=0.05,  # Poor
            max_jacobian=1.0,
            mean_jacobian=0.5,
            min_aspect_ratio=1.0,
            max_aspect_ratio=15.0,  # Poor
            mean_aspect_ratio=8.0,
            min_skewness=0.0,
            max_skewness=0.95,  # Poor
            mean_skewness=0.6
        )

        assert not quality.is_good
        assert not quality.is_acceptable


class TestMeshStatistics:
    """Test mesh statistics model"""

    def test_statistics_creation(self):
        """Test mesh statistics creation"""
        stats = MeshStatistics(
            n_nodes=1000,
            n_elements=5000,
            element_type="tetrahedron",
            dimension=3,
            min_element_size=0.5,
            max_element_size=2.0,
            mean_element_size=1.0
        )

        assert stats.n_nodes == 1000
        assert stats.n_elements == 5000
        assert stats.elements_per_node == 5.0


# ============================================================================
# Quality Analyzer Tests
# ============================================================================

class TestMeshQualityAnalyzer:
    """Test mesh quality analyzer"""

    def test_simple_tetrahedron_quality(self):
        """Test quality analysis for simple tetrahedron"""
        analyzer = MeshQualityAnalyzer()

        # Create simple regular tetrahedron
        nodes = np.array([
            [0, 0, 0],
            [1, 0, 0],
            [0.5, np.sqrt(3)/2, 0],
            [0.5, np.sqrt(3)/6, np.sqrt(2/3)]
        ])

        elements = np.array([[0, 1, 2, 3]])

        quality = analyzer.analyze_tetrahedral_mesh(nodes, elements)

        # Regular tetrahedron should have good quality
        assert quality.min_jacobian > 0.3
        assert quality.max_aspect_ratio < 5.0
        assert quality.is_good

    def test_jacobian_calculation(self):
        """Test Jacobian calculation"""
        analyzer = MeshQualityAnalyzer()

        # Regular tetrahedron
        pts = np.array([
            [0, 0, 0],
            [1, 0, 0],
            [0, 1, 0],
            [0, 0, 1]
        ])

        jacobian = analyzer._tetrahedron_jacobian(pts)

        # Should be positive and reasonable
        assert jacobian > 0
        assert jacobian < 1.0

    def test_aspect_ratio_calculation(self):
        """Test aspect ratio calculation"""
        analyzer = MeshQualityAnalyzer()

        # Regular tetrahedron should have low aspect ratio
        pts = np.array([
            [0, 0, 0],
            [1, 0, 0],
            [0, 1, 0],
            [0, 0, 1]
        ])

        aspect_ratio = analyzer._tetrahedron_aspect_ratio(pts)

        # Regular tetrahedron should have aspect ratio around 1-2
        assert aspect_ratio < 5.0


# ============================================================================
# Mesh Generator Tests (require GMSH)
# ============================================================================

@pytest.mark.skipif(not GMSH_AVAILABLE, reason="GMSH not installed")
class TestGMSHMeshGenerator:
    """Test GMSH mesh generation (requires gmsh package)"""

    def test_box_mesh_generation(self):
        """Test box mesh generation"""
        with GMSHMeshGenerator() as generator:
            mesh_data = generator.generate_simple_box_mesh(
                dimensions=(2.0, 1.0, 1.0),
                element_size=0.5
            )

            assert 'nodes' in mesh_data
            assert 'elements' in mesh_data
            assert len(mesh_data['nodes']) > 0

    def test_cylinder_mesh_generation(self):
        """Test cylinder mesh generation"""
        with GMSHMeshGenerator() as generator:
            mesh_data = generator.generate_cylinder_mesh(
                radius=1.0,
                height=5.0,
                element_size=0.5
            )

            assert 'nodes' in mesh_data
            assert 'elements' in mesh_data
            assert len(mesh_data['nodes']) > 0

    def test_mesh_statistics(self):
        """Test mesh statistics extraction"""
        with GMSHMeshGenerator() as generator:
            mesh_data = generator.generate_simple_box_mesh(
                dimensions=(1.0, 1.0, 1.0),
                element_size=0.3
            )

            stats = generator.get_mesh_statistics()

            assert stats['n_nodes'] > 0
            assert stats['n_elements'] > 0
            assert len(stats['element_types']) > 0


# ============================================================================
# Integration Tests
# ============================================================================

@pytest.mark.skipif(not GMSH_AVAILABLE, reason="GMSH not installed")
class TestIntegration:
    """Integration tests combining mesh generation and quality analysis"""

    def test_box_mesh_with_quality_analysis(self):
        """Test complete workflow: generate mesh → analyze quality"""
        with GMSHMeshGenerator() as generator:
            mesh_data = generator.generate_simple_box_mesh(
                dimensions=(1.0, 1.0, 1.0),
                element_size=0.5
            )

            nodes = mesh_data['nodes']
            elements = mesh_data['elements']

            # Analyze quality
            if '4-node tetrahedron' in elements:
                analyzer = MeshQualityAnalyzer()
                elem_conn = elements['4-node tetrahedron']['connectivity']

                quality = analyzer.analyze_tetrahedral_mesh(nodes, elem_conn)

                assert quality.min_jacobian > 0
                assert quality.max_aspect_ratio < 100.0  # Reasonable bound
                assert quality.quality_score >= 0
