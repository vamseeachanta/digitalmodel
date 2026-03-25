"""
Mesh Generation Utilities for GMSH Agent
Core functionality for 1D, 2D, and 3D mesh generation
"""

import logging
from typing import Dict, List, Optional, Tuple, Any
import numpy as np

try:
    import gmsh
    GMSH_AVAILABLE = True
except ImportError:
    GMSH_AVAILABLE = False
    gmsh = None

logger = logging.getLogger(__name__)


class MeshGenerator:
    """Core mesh generation functionality using GMSH"""
    
    def __init__(self, config: Optional[Dict] = None):
        """
        Initialize mesh generator
        
        Args:
            config: Configuration dictionary
        """
        self.config = config or {}
        self.gmsh_initialized = False
        
        if GMSH_AVAILABLE:
            self.initialize_gmsh()
    
    def initialize_gmsh(self):
        """Initialize GMSH if not already initialized"""
        if not self.gmsh_initialized and GMSH_AVAILABLE:
            gmsh.initialize()
            self.gmsh_initialized = True
            
            # Set default options
            gmsh.option.setNumber("General.Terminal", 1)
            gmsh.option.setNumber("Mesh.Algorithm", 6)  # Frontal-Delaunay
    
    def cleanup(self):
        """Clean up GMSH resources"""
        if self.gmsh_initialized and GMSH_AVAILABLE:
            gmsh.finalize()
            self.gmsh_initialized = False
    
    def generate_1d_mesh(self, 
                        points: List[Tuple[float, float, float]],
                        element_size: float = 1.0,
                        order: int = 1) -> Dict:
        """
        Generate 1D line mesh from points
        
        Args:
            points: List of (x, y, z) coordinates
            element_size: Target element size
            order: Element order (1 or 2)
            
        Returns:
            Dictionary with mesh data
        """
        if not GMSH_AVAILABLE:
            raise RuntimeError("GMSH not available")
        
        logger.info(f"Generating 1D mesh with {len(points)} points")
        
        try:
            gmsh.clear()
            gmsh.model.add("line_mesh")
            
            # Create points
            point_tags = []
            for i, (x, y, z) in enumerate(points):
                tag = gmsh.model.geo.addPoint(x, y, z, element_size)
                point_tags.append(tag)
            
            # Create lines between consecutive points
            line_tags = []
            for i in range(len(point_tags) - 1):
                line = gmsh.model.geo.addLine(point_tags[i], point_tags[i + 1])
                line_tags.append(line)
            
            # Create physical group
            gmsh.model.geo.addPhysicalGroup(1, line_tags, tag=1)
            gmsh.model.setPhysicalName(1, 1, "Lines")
            
            # Synchronize geometry
            gmsh.model.geo.synchronize()
            
            # Generate mesh
            gmsh.model.mesh.generate(1)
            gmsh.model.mesh.setOrder(order)
            
            # Extract mesh data
            node_tags, node_coords, _ = gmsh.model.mesh.getNodes()
            element_types, element_tags, element_nodes = gmsh.model.mesh.getElements()
            
            mesh_data = {
                "dimension": 1,
                "num_nodes": len(node_tags),
                "num_elements": sum(len(tags) for tags in element_tags),
                "node_coords": node_coords.reshape(-1, 3),
                "element_connectivity": element_nodes,
                "element_order": order
            }
            
            logger.info(f"Generated 1D mesh: {mesh_data['num_nodes']} nodes, {mesh_data['num_elements']} elements")
            return mesh_data
            
        except Exception as e:
            logger.error(f"Failed to generate 1D mesh: {e}")
            raise
    
    def generate_2d_mesh(self,
                        surface_points: List[Tuple[float, float, float]],
                        algorithm: str = "frontal-delaunay",
                        element_type: str = "triangle",
                        element_size: float = 1.0) -> Dict:
        """
        Generate 2D surface mesh
        
        Args:
            surface_points: Boundary points of the surface
            algorithm: Meshing algorithm
            element_type: Element type (triangle, quad, mixed)
            element_size: Target element size
            
        Returns:
            Dictionary with mesh data
        """
        if not GMSH_AVAILABLE:
            raise RuntimeError("GMSH not available")
        
        logger.info(f"Generating 2D {element_type} mesh using {algorithm}")
        
        try:
            gmsh.clear()
            gmsh.model.add("surface_mesh")
            
            # Set algorithm
            algorithms = {
                "frontal-delaunay": 6,
                "delaunay": 5,
                "frontal": 1,
                "meshadapt": 2
            }
            gmsh.option.setNumber("Mesh.Algorithm", algorithms.get(algorithm, 6))
            
            # Set element type
            if element_type == "quad":
                gmsh.option.setNumber("Mesh.RecombineAll", 1)
                gmsh.option.setNumber("Mesh.RecombinationAlgorithm", 1)
            
            # Create boundary points
            point_tags = []
            for x, y, z in surface_points:
                tag = gmsh.model.geo.addPoint(x, y, z, element_size)
                point_tags.append(tag)
            
            # Create boundary lines
            line_tags = []
            for i in range(len(point_tags)):
                j = (i + 1) % len(point_tags)
                line = gmsh.model.geo.addLine(point_tags[i], point_tags[j])
                line_tags.append(line)
            
            # Create curve loop and surface
            curve_loop = gmsh.model.geo.addCurveLoop(line_tags)
            surface = gmsh.model.geo.addPlaneSurface([curve_loop])
            
            # Create physical group
            gmsh.model.geo.addPhysicalGroup(2, [surface], tag=1)
            gmsh.model.setPhysicalName(2, 1, "Surface")
            
            # Synchronize and generate mesh
            gmsh.model.geo.synchronize()
            gmsh.model.mesh.generate(2)
            
            # Recombine if quad elements requested
            if element_type == "quad":
                gmsh.model.mesh.recombine()
            
            # Extract mesh data
            node_tags, node_coords, _ = gmsh.model.mesh.getNodes()
            element_types, element_tags, element_nodes = gmsh.model.mesh.getElements(2)
            
            mesh_data = {
                "dimension": 2,
                "num_nodes": len(node_tags),
                "num_elements": sum(len(tags) for tags in element_tags),
                "node_coords": node_coords.reshape(-1, 3),
                "element_connectivity": element_nodes,
                "element_type": element_type,
                "algorithm": algorithm
            }
            
            logger.info(f"Generated 2D mesh: {mesh_data['num_nodes']} nodes, {mesh_data['num_elements']} elements")
            return mesh_data
            
        except Exception as e:
            logger.error(f"Failed to generate 2D mesh: {e}")
            raise
    
    def generate_3d_mesh(self,
                        volume_geometry: str,
                        algorithm: str = "frontal-delaunay",
                        element_type: str = "tetrahedron",
                        element_size: float = 1.0) -> Dict:
        """
        Generate 3D volume mesh
        
        Args:
            volume_geometry: Path to geometry file or geometry definition
            algorithm: Meshing algorithm
            element_type: Element type (tet, hex, prism, pyramid, mixed)
            element_size: Target element size
            
        Returns:
            Dictionary with mesh data
        """
        if not GMSH_AVAILABLE:
            raise RuntimeError("GMSH not available")
        
        logger.info(f"Generating 3D {element_type} mesh using {algorithm}")
        
        try:
            gmsh.clear()
            gmsh.model.add("volume_mesh")
            
            # Set algorithm
            algorithms = {
                "frontal-delaunay": 6,
                "delaunay": 5,
                "frontal": 1,
                "hxt": 10  # High-quality tet mesher
            }
            gmsh.option.setNumber("Mesh.Algorithm3D", algorithms.get(algorithm, 1))
            
            # Set element size
            gmsh.option.setNumber("Mesh.CharacteristicLengthMin", element_size * 0.1)
            gmsh.option.setNumber("Mesh.CharacteristicLengthMax", element_size)
            
            # Handle different element types
            if element_type == "hexahedron" or element_type == "hex":
                gmsh.option.setNumber("Mesh.SubdivisionAlgorithm", 2)
                gmsh.option.setNumber("Mesh.RecombineAll", 1)
                gmsh.option.setNumber("Mesh.Recombine3DAll", 1)
            
            # Create or load geometry
            if isinstance(volume_geometry, str) and volume_geometry.endswith(('.step', '.stp', '.iges', '.igs', '.brep')):
                # Import from file
                gmsh.merge(volume_geometry)
            else:
                # Create simple box for demonstration
                box = gmsh.model.occ.addBox(0, 0, 0, 10, 10, 10)
                gmsh.model.occ.synchronize()
                gmsh.model.addPhysicalGroup(3, [box], tag=1)
                gmsh.model.setPhysicalName(3, 1, "Volume")
            
            # Generate mesh
            gmsh.model.mesh.generate(3)
            
            # Optimize if requested
            if self.config.get("optimize_3d", True):
                gmsh.model.mesh.optimize("Netgen")
            
            # Extract mesh data
            node_tags, node_coords, _ = gmsh.model.mesh.getNodes()
            element_types, element_tags, element_nodes = gmsh.model.mesh.getElements(3)
            
            # Count element types
            element_counts = {}
            for etype, etags in zip(element_types, element_tags):
                element_name = gmsh.model.mesh.getElementProperties(etype)[0]
                element_counts[element_name] = len(etags)
            
            mesh_data = {
                "dimension": 3,
                "num_nodes": len(node_tags),
                "num_elements": sum(len(tags) for tags in element_tags),
                "node_coords": node_coords.reshape(-1, 3),
                "element_connectivity": element_nodes,
                "element_type": element_type,
                "element_counts": element_counts,
                "algorithm": algorithm
            }
            
            logger.info(f"Generated 3D mesh: {mesh_data['num_nodes']} nodes, {mesh_data['num_elements']} elements")
            logger.info(f"Element distribution: {element_counts}")
            return mesh_data
            
        except Exception as e:
            logger.error(f"Failed to generate 3D mesh: {e}")
            raise
    
    def set_element_type(self, element_type: str, dimension: int):
        """
        Configure element type for mesh generation
        
        Args:
            element_type: Type of element
            dimension: Mesh dimension (1, 2, or 3)
        """
        if not GMSH_AVAILABLE:
            return
        
        if dimension == 2:
            if element_type in ["quad", "quadrilateral"]:
                gmsh.option.setNumber("Mesh.RecombineAll", 1)
            elif element_type == "triangle":
                gmsh.option.setNumber("Mesh.RecombineAll", 0)
        elif dimension == 3:
            if element_type in ["hex", "hexahedron"]:
                gmsh.option.setNumber("Mesh.RecombineAll", 1)
                gmsh.option.setNumber("Mesh.Recombine3DAll", 1)
            elif element_type in ["tet", "tetrahedron"]:
                gmsh.option.setNumber("Mesh.RecombineAll", 0)
                gmsh.option.setNumber("Mesh.Recombine3DAll", 0)
            elif element_type == "prism":
                gmsh.option.setNumber("Mesh.SubdivisionAlgorithm", 1)
    
    def control_mesh_density(self,
                           field_type: str = "box",
                           parameters: Dict[str, float] = None) -> None:
        """
        Control mesh density using fields
        
        Args:
            field_type: Type of field (box, ball, cylinder, distance)
            parameters: Field parameters
        """
        if not GMSH_AVAILABLE:
            return
        
        parameters = parameters or {}
        
        if field_type == "box":
            # Create box field for local refinement
            field = gmsh.model.mesh.field.add("Box")
            gmsh.model.mesh.field.setNumber(field, "VIn", parameters.get("size_in", 0.1))
            gmsh.model.mesh.field.setNumber(field, "VOut", parameters.get("size_out", 1.0))
            gmsh.model.mesh.field.setNumber(field, "XMin", parameters.get("x_min", -1))
            gmsh.model.mesh.field.setNumber(field, "XMax", parameters.get("x_max", 1))
            gmsh.model.mesh.field.setNumber(field, "YMin", parameters.get("y_min", -1))
            gmsh.model.mesh.field.setNumber(field, "YMax", parameters.get("y_max", 1))
            gmsh.model.mesh.field.setNumber(field, "ZMin", parameters.get("z_min", -1))
            gmsh.model.mesh.field.setNumber(field, "ZMax", parameters.get("z_max", 1))
            
        elif field_type == "ball":
            # Create ball field for spherical refinement
            field = gmsh.model.mesh.field.add("Ball")
            gmsh.model.mesh.field.setNumber(field, "Radius", parameters.get("radius", 1.0))
            gmsh.model.mesh.field.setNumber(field, "VIn", parameters.get("size_in", 0.1))
            gmsh.model.mesh.field.setNumber(field, "VOut", parameters.get("size_out", 1.0))
            gmsh.model.mesh.field.setNumber(field, "XCenter", parameters.get("x_center", 0))
            gmsh.model.mesh.field.setNumber(field, "YCenter", parameters.get("y_center", 0))
            gmsh.model.mesh.field.setNumber(field, "ZCenter", parameters.get("z_center", 0))
            
        elif field_type == "distance":
            # Create distance field from edges/points
            field = gmsh.model.mesh.field.add("Distance")
            if "edge_list" in parameters:
                gmsh.model.mesh.field.setNumbers(field, "EdgesList", parameters["edge_list"])
            if "point_list" in parameters:
                gmsh.model.mesh.field.setNumbers(field, "PointsList", parameters["point_list"])
            
            # Create threshold field based on distance
            threshold = gmsh.model.mesh.field.add("Threshold")
            gmsh.model.mesh.field.setNumber(threshold, "IField", field)
            gmsh.model.mesh.field.setNumber(threshold, "LcMin", parameters.get("size_min", 0.1))
            gmsh.model.mesh.field.setNumber(threshold, "LcMax", parameters.get("size_max", 1.0))
            gmsh.model.mesh.field.setNumber(threshold, "DistMin", parameters.get("dist_min", 0.1))
            gmsh.model.mesh.field.setNumber(threshold, "DistMax", parameters.get("dist_max", 2.0))
            field = threshold
        
        # Set as background field
        gmsh.model.mesh.field.setAsBackgroundMesh(field)
        logger.info(f"Applied {field_type} mesh density control")
    
    def save_mesh(self, filename: str, format: Optional[str] = None) -> str:
        """
        Save mesh to file
        
        Args:
            filename: Output filename
            format: Output format (auto-detected if None)
            
        Returns:
            Path to saved file
        """
        if not GMSH_AVAILABLE:
            raise RuntimeError("GMSH not available")
        
        # Auto-detect format from extension if not specified
        if format is None:
            if filename.endswith('.msh'):
                format = 'msh'
            elif filename.endswith('.vtk'):
                format = 'vtk'
            elif filename.endswith('.stl'):
                format = 'stl'
            elif filename.endswith('.inp'):
                format = 'inp'
            else:
                format = 'msh'  # Default
        
        # Set format-specific options
        if format == 'msh':
            gmsh.option.setNumber("Mesh.MshFileVersion", 4.1)
        elif format == 'vtk':
            gmsh.option.setNumber("Mesh.SaveAll", 1)
        
        # Save mesh
        gmsh.write(filename)
        logger.info(f"Saved mesh to {filename} (format: {format})")
        return filename
    
    def __enter__(self):
        """Context manager entry"""
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit"""
        self.cleanup()