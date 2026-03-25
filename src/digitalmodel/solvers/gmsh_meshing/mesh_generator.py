#!/usr/bin/env python3
"""
ABOUTME: GMSH mesh generation for 1D/2D/3D finite element analysis with geometry
import, STEP/OCC support, and CalculiX INP export.
"""

import numpy as np
from pathlib import Path
from typing import Optional, Dict, Tuple, List
from .models import GeometryType, ElementType, MeshAlgorithm

try:
    import gmsh
    GMSH_AVAILABLE = True
except (ImportError, OSError):
    GMSH_AVAILABLE = False


class GMSHMeshGenerator:
    """Finite element mesh generation using GMSH.

    Supports 1D/2D/3D meshing with geometry import and quality control.
    """

    # GMSH Algorithm3D codes: 1=Delaunay(HXT), 4=Frontal, 10=HXT
    ALGORITHM_CODES = {
        MeshAlgorithm.MESHADAPT: 4,
        MeshAlgorithm.AUTOMATIC: 1,
        MeshAlgorithm.DELAUNAY: 1,
        MeshAlgorithm.FRONTAL: 4,
        MeshAlgorithm.FRONTAL_DELAUNAY: 1,
        MeshAlgorithm.PACKED_PARALLELOGRAMS: 10,
    }

    def __init__(self):
        if not GMSH_AVAILABLE:
            raise ImportError(
                "GMSH Python package required. Install: pip install gmsh"
            )
        self.initialized = False

    def initialize(self):
        """Initialize GMSH engine."""
        if not self.initialized:
            gmsh.initialize()
            gmsh.option.setNumber("General.Terminal", 0)
            self.initialized = True

    def finalize(self):
        """Finalize GMSH engine."""
        if self.initialized:
            gmsh.finalize()
            self.initialized = False

    def _setup_mesh(self, name, element_size, algorithm=None):
        """Common setup: clear, add model, set size after OCC sync."""
        gmsh.clear()
        gmsh.model.add(name)
        return name

    def _apply_mesh_settings(self, element_size, algorithm=None):
        """Set element size and algorithm after geometry is defined."""
        gmsh.model.mesh.setSize(gmsh.model.getEntities(0), element_size)
        if algorithm is not None:
            alg_code = self.ALGORITHM_CODES.get(algorithm, 1)
            gmsh.option.setNumber("Mesh.Algorithm3D", alg_code)

    def generate_simple_box_mesh(
        self,
        dimensions: Tuple[float, float, float],
        element_size: float = 1.0,
        algorithm: MeshAlgorithm = MeshAlgorithm.FRONTAL_DELAUNAY,
    ) -> Dict:
        """Generate tetrahedral mesh for a box geometry."""
        self.initialize()
        try:
            self._setup_mesh("box", element_size)
            lx, ly, lz = dimensions
            gmsh.model.occ.addBox(0, 0, 0, lx, ly, lz)
            gmsh.model.occ.synchronize()
            self._apply_mesh_settings(element_size, algorithm)
            gmsh.model.mesh.generate(3)
            nodes, elements = self._extract_mesh_data()
            return {
                'nodes': nodes, 'elements': elements,
                'geometry': 'box', 'dimensions': dimensions,
                'element_size': element_size,
            }
        finally:
            pass

    def generate_cylinder_mesh(
        self, radius: float, height: float, element_size: float = 1.0,
    ) -> Dict:
        """Generate mesh for cylindrical geometry."""
        self.initialize()
        try:
            self._setup_mesh("cylinder", element_size)
            gmsh.model.occ.addCylinder(0, 0, 0, 0, 0, height, radius)
            gmsh.model.occ.synchronize()
            self._apply_mesh_settings(element_size)
            gmsh.model.mesh.generate(3)
            nodes, elements = self._extract_mesh_data()
            return {
                'nodes': nodes, 'elements': elements,
                'geometry': 'cylinder', 'radius': radius, 'height': height,
            }
        finally:
            pass

    def generate_surface_mesh_from_stl(
        self, stl_file: str, element_size: float = 1.0,
    ) -> Dict:
        """Generate surface mesh from STL geometry."""
        self.initialize()
        stl_path = Path(stl_file)
        if not stl_path.exists():
            raise FileNotFoundError(f"STL file not found: {stl_file}")
        try:
            self._setup_mesh("stl_mesh", element_size)
            gmsh.merge(str(stl_path))
            self._apply_mesh_settings(element_size)
            gmsh.model.mesh.generate(2)
            nodes, elements = self._extract_mesh_data()
            return {
                'nodes': nodes, 'elements': elements,
                'geometry': 'stl', 'source_file': str(stl_path),
            }
        finally:
            pass

    def generate_mesh_from_step(
        self,
        step_file: str,
        element_size: float = 1.0,
        dimension: int = 3,
        algorithm: MeshAlgorithm = MeshAlgorithm.FRONTAL_DELAUNAY,
    ) -> Dict:
        """Import STEP geometry via OCC kernel and generate volume mesh."""
        self.initialize()
        step_path = Path(step_file)
        if not step_path.exists():
            raise FileNotFoundError(f"STEP file not found: {step_file}")
        try:
            self._setup_mesh("step_import", element_size)
            gmsh.model.occ.importShapes(str(step_path))
            gmsh.model.occ.synchronize()
            self._apply_mesh_settings(element_size, algorithm)
            gmsh.model.mesh.generate(dimension)
            nodes, elements = self._extract_mesh_data()
            return {
                'nodes': nodes, 'elements': elements,
                'geometry': 'step', 'source_file': str(step_path),
                'dimension': dimension, 'element_size': element_size,
            }
        finally:
            pass

    def _extract_mesh_data(self) -> Tuple[np.ndarray, Dict]:
        """Extract nodes (N x 3) and elements dict from current GMSH model."""
        node_tags, node_coords, _ = gmsh.model.mesh.getNodes()
        nodes = node_coords.reshape(-1, 3)

        elements = {}
        for elem_type in gmsh.model.mesh.getElementTypes():
            elem_tags, elem_nodes = gmsh.model.mesh.getElementsByType(
                elem_type
            )
            props = gmsh.model.mesh.getElementProperties(elem_type)
            elem_name, dim, order, num_nodes_per_elem = (
                props[0], props[1], props[2], props[3]
            )
            connectivity = elem_nodes.reshape(-1, num_nodes_per_elem)
            elements[elem_name] = {
                'tags': elem_tags,
                'connectivity': connectivity - 1,  # 0-based indexing
                'dimension': dim,
            }
        return nodes, elements

    def save_mesh_vtk(self, output_file: str):
        """Save current mesh to VTK format."""
        if not self.initialized:
            raise RuntimeError("GMSH not initialized")
        gmsh.write(output_file)

    def save_mesh_msh(self, output_file: str):
        """Save current mesh to MSH format (GMSH native)."""
        if not self.initialized:
            raise RuntimeError("GMSH not initialized")
        gmsh.write(output_file)

    def get_mesh_statistics(self) -> Dict:
        """Get basic statistics for current mesh."""
        if not self.initialized:
            raise RuntimeError("GMSH not initialized")
        nodes, elements = self._extract_mesh_data()
        n_elements = sum(len(e['tags']) for e in elements.values())
        return {
            'n_nodes': len(nodes),
            'n_elements': n_elements,
            'element_types': list(elements.keys()),
        }

    def export_mesh_inp(
        self,
        output_file: str,
        element_type_map: Optional[Dict[str, str]] = None,
        title: str = "GMSH mesh export",
    ) -> Path:
        """Export current mesh to CalculiX INP format.

        Args:
            output_file: Output .inp file path
            element_type_map: Gmsh element name to CalculiX type mapping
            title: INP file heading

        Returns:
            Path to written INP file
        """
        if not self.initialized:
            raise RuntimeError("GMSH not initialized — generate a mesh first")

        if element_type_map is None:
            element_type_map = {
                'Tetrahedron 4': 'C3D4', 'Tetrahedron 10': 'C3D10',
                'Hexahedron 8': 'C3D8', 'Hexahedron 20': 'C3D20',
                'Triangle 3': 'CPS3', 'Triangle 6': 'CPS6',
            }

        nodes, elements = self._extract_mesh_data()
        output_path = Path(output_file)

        with open(output_path, 'w') as f:
            f.write(f"*HEADING\n{title}\n")
            f.write("*NODE\n")
            for i, (x, y, z) in enumerate(nodes):
                f.write(f"{i + 1}, {x:.10g}, {y:.10g}, {z:.10g}\n")

            for elem_name, elem_data in elements.items():
                ccx_type = element_type_map.get(elem_name)
                if ccx_type is None or elem_data['dimension'] < 2:
                    continue
                elset_name = f"E{ccx_type}"
                f.write(
                    f"*ELEMENT, TYPE={ccx_type}, ELSET={elset_name}\n"
                )
                for j, conn in enumerate(elem_data['connectivity']):
                    node_ids = ", ".join(str(int(n) + 1) for n in conn)
                    f.write(f"{j + 1}, {node_ids}\n")

            f.write("*NSET, NSET=NALL, GENERATE\n")
            f.write(f"1, {len(nodes)}, 1\n")

        return output_path

    def create_step_geometry(
        self, geometry_type: str, output_file: str, **params,
    ) -> Path:
        """Create a simple STEP geometry file using gmsh OCC kernel.

        Supported types: 'box', 'cylinder', 'sphere'.
        """
        self.initialize()
        try:
            self._setup_mesh("step_export", 1.0)
            if geometry_type == 'box':
                dims = params.get('dimensions', (1.0, 1.0, 1.0))
                gmsh.model.occ.addBox(0, 0, 0, *dims)
            elif geometry_type == 'cylinder':
                r = params.get('radius', 1.0)
                h = params.get('height', 2.0)
                gmsh.model.occ.addCylinder(0, 0, 0, 0, 0, h, r)
            elif geometry_type == 'sphere':
                r = params.get('radius', 1.0)
                gmsh.model.occ.addSphere(0, 0, 0, r)
            else:
                raise ValueError(f"Unknown geometry type: {geometry_type}")
            gmsh.model.occ.synchronize()
            output_path = Path(output_file)
            gmsh.write(str(output_path))
            return output_path
        finally:
            pass

    def __enter__(self):
        """Context manager entry."""
        self.initialize()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit."""
        self.finalize()
