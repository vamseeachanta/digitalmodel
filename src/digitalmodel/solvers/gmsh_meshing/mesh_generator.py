#!/usr/bin/env python3
"""
ABOUTME: GMSH mesh generation for 1D/2D/3D finite element analysis with geometry
import and adaptive element sizing.
"""

import numpy as np
from pathlib import Path
from typing import Optional, Dict, Tuple, List
from .models import GeometryType, ElementType, MeshAlgorithm

try:
    import gmsh
    GMSH_AVAILABLE = True
except ImportError:
    GMSH_AVAILABLE = False


class GMSHMeshGenerator:
    """
    Finite element mesh generation using GMSH

    Supports 1D/2D/3D meshing with geometry import and quality control.
    Requires gmsh Python package: pip install gmsh
    """

    # GMSH algorithm codes
    ALGORITHM_CODES = {
        MeshAlgorithm.MESHADAPT: 1,
        MeshAlgorithm.AUTOMATIC: 2,
        MeshAlgorithm.DELAUNAY: 5,
        MeshAlgorithm.FRONTAL: 6,
        MeshAlgorithm.FRONTAL_DELAUNAY: 6,  # Default
        MeshAlgorithm.PACKED_PARALLELOGRAMS: 7,
    }

    def __init__(self):
        """Initialize GMSH mesh generator"""
        if not GMSH_AVAILABLE:
            raise ImportError(
                "GMSH Python package required. Install: pip install gmsh"
            )

        self.initialized = False

    def initialize(self):
        """Initialize GMSH"""
        if not self.initialized:
            gmsh.initialize()
            gmsh.option.setNumber("General.Terminal", 0)  # Suppress output
            self.initialized = True

    def finalize(self):
        """Finalize GMSH"""
        if self.initialized:
            gmsh.finalize()
            self.initialized = False

    def generate_simple_box_mesh(
        self,
        dimensions: Tuple[float, float, float],
        element_size: float = 1.0,
        algorithm: MeshAlgorithm = MeshAlgorithm.FRONTAL_DELAUNAY
    ) -> Dict:
        """
        Generate tetrahedral mesh for a box geometry

        Args:
            dimensions: (length, width, height) in meters
            element_size: Target element size
            algorithm: Meshing algorithm

        Returns:
            Dictionary with nodes, elements, and metadata
        """
        self.initialize()

        try:
            gmsh.clear()
            gmsh.model.add("box")

            # Create box
            lx, ly, lz = dimensions
            box = gmsh.model.occ.addBox(0, 0, 0, lx, ly, lz)

            gmsh.model.occ.synchronize()

            # Set element size
            gmsh.model.mesh.setSize(gmsh.model.getEntities(0), element_size)

            # Set meshing algorithm
            alg_code = self.ALGORITHM_CODES.get(algorithm, 6)
            gmsh.option.setNumber("Mesh.Algorithm3D", alg_code)

            # Generate mesh
            gmsh.model.mesh.generate(3)

            # Extract mesh data
            nodes, elements = self._extract_mesh_data()

            return {
                'nodes': nodes,
                'elements': elements,
                'geometry': 'box',
                'dimensions': dimensions,
                'element_size': element_size,
            }

        finally:
            pass  # Keep GMSH initialized for reuse

    def generate_cylinder_mesh(
        self,
        radius: float,
        height: float,
        element_size: float = 1.0
    ) -> Dict:
        """
        Generate mesh for cylindrical geometry (risers, pipes, etc.)

        Args:
            radius: Cylinder radius (m)
            height: Cylinder height (m)
            element_size: Target element size

        Returns:
            Dictionary with mesh data
        """
        self.initialize()

        try:
            gmsh.clear()
            gmsh.model.add("cylinder")

            # Create cylinder
            cylinder = gmsh.model.occ.addCylinder(0, 0, 0, 0, 0, height, radius)

            gmsh.model.occ.synchronize()

            # Set element size
            gmsh.model.mesh.setSize(gmsh.model.getEntities(0), element_size)

            # Generate mesh
            gmsh.model.mesh.generate(3)

            nodes, elements = self._extract_mesh_data()

            return {
                'nodes': nodes,
                'elements': elements,
                'geometry': 'cylinder',
                'radius': radius,
                'height': height,
            }

        finally:
            pass

    def generate_surface_mesh_from_stl(
        self,
        stl_file: str,
        element_size: float = 1.0
    ) -> Dict:
        """
        Generate surface mesh from STL geometry

        Args:
            stl_file: Path to STL file
            element_size: Target element size

        Returns:
            Dictionary with mesh data
        """
        self.initialize()

        stl_path = Path(stl_file)
        if not stl_path.exists():
            raise FileNotFoundError(f"STL file not found: {stl_file}")

        try:
            gmsh.clear()
            gmsh.model.add("stl_mesh")

            # Import STL
            gmsh.merge(str(stl_path))

            # Set element size
            gmsh.model.mesh.setSize(gmsh.model.getEntities(0), element_size)

            # Generate 2D surface mesh
            gmsh.model.mesh.generate(2)

            nodes, elements = self._extract_mesh_data()

            return {
                'nodes': nodes,
                'elements': elements,
                'geometry': 'stl',
                'source_file': str(stl_path),
            }

        finally:
            pass

    def _extract_mesh_data(self) -> Tuple[np.ndarray, Dict]:
        """
        Extract nodes and elements from current GMSH model

        Returns:
            (nodes, elements) where:
            - nodes: N×3 array of node coordinates
            - elements: dict with element connectivity by type
        """
        # Get node data
        node_tags, node_coords, _ = gmsh.model.mesh.getNodes()
        nodes = node_coords.reshape(-1, 3)

        # Get element data
        elements = {}
        element_types = gmsh.model.mesh.getElementTypes()

        for elem_type in element_types:
            elem_tags, elem_nodes = gmsh.model.mesh.getElementsByType(elem_type)

            # Get element properties
            elem_name, dim, order, num_nodes_per_elem, _ = gmsh.model.mesh.getElementProperties(elem_type)

            # Reshape connectivity
            connectivity = elem_nodes.reshape(-1, num_nodes_per_elem)

            elements[elem_name] = {
                'tags': elem_tags,
                'connectivity': connectivity - 1,  # Convert to 0-based indexing
                'dimension': dim,
            }

        return nodes, elements

    def save_mesh_vtk(self, output_file: str):
        """
        Save current mesh to VTK format

        Args:
            output_file: Output VTK file path
        """
        if not self.initialized:
            raise RuntimeError("GMSH not initialized")

        gmsh.write(output_file)

    def save_mesh_msh(self, output_file: str):
        """
        Save current mesh to MSH format (GMSH native)

        Args:
            output_file: Output MSH file path
        """
        if not self.initialized:
            raise RuntimeError("GMSH not initialized")

        gmsh.write(output_file)

    def get_mesh_statistics(self) -> Dict:
        """
        Get basic statistics for current mesh

        Returns:
            Dictionary with mesh statistics
        """
        if not self.initialized:
            raise RuntimeError("GMSH not initialized")

        nodes, elements = self._extract_mesh_data()

        n_nodes = len(nodes)
        n_elements = sum(len(e['tags']) for e in elements.values())

        return {
            'n_nodes': n_nodes,
            'n_elements': n_elements,
            'element_types': list(elements.keys()),
        }

    def __enter__(self):
        """Context manager entry"""
        self.initialize()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit"""
        self.finalize()
