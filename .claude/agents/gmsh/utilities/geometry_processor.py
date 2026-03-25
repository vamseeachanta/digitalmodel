"""
Geometry Processing Utilities for GMSH Agent
Import, heal, and manipulate CAD geometries
"""

import logging
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any
import numpy as np

try:
    import gmsh
    GMSH_AVAILABLE = True
except ImportError:
    GMSH_AVAILABLE = False
    gmsh = None

logger = logging.getLogger(__name__)


class GeometryProcessor:
    """Geometry import, healing, and manipulation using GMSH"""
    
    def __init__(self, config: Optional[Dict] = None):
        """
        Initialize geometry processor
        
        Args:
            config: Configuration dictionary
        """
        self.config = config or {}
        self.gmsh_initialized = False
        self.current_model = None
        
        if GMSH_AVAILABLE:
            self.initialize_gmsh()
    
    def initialize_gmsh(self):
        """Initialize GMSH if not already initialized"""
        if not self.gmsh_initialized and GMSH_AVAILABLE:
            if not gmsh.isInitialized():
                gmsh.initialize()
            self.gmsh_initialized = True
            
            # Set default options for geometry
            gmsh.option.setNumber("Geometry.Tolerance", 1e-6)
            gmsh.option.setNumber("Geometry.OCCFixDegenerated", 1)
            gmsh.option.setNumber("Geometry.OCCFixSmallEdges", 1)
            gmsh.option.setNumber("Geometry.OCCFixSmallFaces", 1)
    
    def import_step(self, file_path: str, heal: bool = True) -> Dict:
        """
        Import STEP file
        
        Args:
            file_path: Path to STEP file
            heal: Attempt to heal geometry defects
            
        Returns:
            Dictionary with import information
        """
        if not GMSH_AVAILABLE:
            raise RuntimeError("GMSH not available")
        
        if not Path(file_path).exists():
            raise FileNotFoundError(f"STEP file not found: {file_path}")
        
        logger.info(f"Importing STEP file: {file_path}")
        
        try:
            gmsh.clear()
            gmsh.model.add(Path(file_path).stem)
            
            # Import STEP file using OpenCASCADE kernel
            gmsh.model.occ.importShapes(file_path)
            
            if heal:
                self.heal_geometry()
            
            # Synchronize to create the model
            gmsh.model.occ.synchronize()
            
            # Get geometry information
            dim_tags = gmsh.model.getEntities()
            volumes = [tag for dim, tag in dim_tags if dim == 3]
            surfaces = [tag for dim, tag in dim_tags if dim == 2]
            curves = [tag for dim, tag in dim_tags if dim == 1]
            points = [tag for dim, tag in dim_tags if dim == 0]
            
            import_info = {
                "file": file_path,
                "format": "STEP",
                "num_volumes": len(volumes),
                "num_surfaces": len(surfaces),
                "num_curves": len(curves),
                "num_points": len(points),
                "healed": heal,
                "bounding_box": self.get_bounding_box()
            }
            
            logger.info(f"STEP import successful: {import_info}")
            return import_info
            
        except Exception as e:
            logger.error(f"Failed to import STEP file: {e}")
            raise
    
    def import_iges(self, file_path: str, heal: bool = True) -> Dict:
        """
        Import IGES file
        
        Args:
            file_path: Path to IGES file
            heal: Attempt to heal geometry defects
            
        Returns:
            Dictionary with import information
        """
        if not GMSH_AVAILABLE:
            raise RuntimeError("GMSH not available")
        
        if not Path(file_path).exists():
            raise FileNotFoundError(f"IGES file not found: {file_path}")
        
        logger.info(f"Importing IGES file: {file_path}")
        
        try:
            gmsh.clear()
            gmsh.model.add(Path(file_path).stem)
            
            # Import IGES file
            gmsh.model.occ.importShapes(file_path)
            
            if heal:
                self.heal_geometry()
            
            gmsh.model.occ.synchronize()
            
            # Get geometry information
            dim_tags = gmsh.model.getEntities()
            
            import_info = {
                "file": file_path,
                "format": "IGES",
                "num_entities": len(dim_tags),
                "healed": heal,
                "bounding_box": self.get_bounding_box()
            }
            
            logger.info(f"IGES import successful: {import_info}")
            return import_info
            
        except Exception as e:
            logger.error(f"Failed to import IGES file: {e}")
            raise
    
    def import_stl(self, file_path: str, merge_tolerance: float = 1e-6) -> Dict:
        """
        Import STL file
        
        Args:
            file_path: Path to STL file
            merge_tolerance: Tolerance for merging duplicate vertices
            
        Returns:
            Dictionary with import information
        """
        if not GMSH_AVAILABLE:
            raise RuntimeError("GMSH not available")
        
        if not Path(file_path).exists():
            raise FileNotFoundError(f"STL file not found: {file_path}")
        
        logger.info(f"Importing STL file: {file_path}")
        
        try:
            gmsh.clear()
            gmsh.model.add(Path(file_path).stem)
            
            # Merge STL file
            gmsh.merge(file_path)
            
            # Create surface from STL mesh
            gmsh.model.mesh.createTopology()
            
            # Remove duplicate nodes
            gmsh.model.mesh.removeDuplicateNodes()
            
            # Get mesh information
            node_tags, node_coords, _ = gmsh.model.mesh.getNodes()
            element_types, element_tags, _ = gmsh.model.mesh.getElements()
            
            import_info = {
                "file": file_path,
                "format": "STL",
                "num_nodes": len(node_tags),
                "num_triangles": sum(len(tags) for tags in element_tags),
                "merge_tolerance": merge_tolerance,
                "bounding_box": self.get_bounding_box()
            }
            
            logger.info(f"STL import successful: {import_info}")
            return import_info
            
        except Exception as e:
            logger.error(f"Failed to import STL file: {e}")
            raise
    
    def import_brep(self, file_path: str, heal: bool = True) -> Dict:
        """
        Import BREP (OpenCASCADE native) file
        
        Args:
            file_path: Path to BREP file
            heal: Attempt to heal geometry defects
            
        Returns:
            Dictionary with import information
        """
        if not GMSH_AVAILABLE:
            raise RuntimeError("GMSH not available")
        
        if not Path(file_path).exists():
            raise FileNotFoundError(f"BREP file not found: {file_path}")
        
        logger.info(f"Importing BREP file: {file_path}")
        
        try:
            gmsh.clear()
            gmsh.model.add(Path(file_path).stem)
            
            # Import BREP file
            gmsh.model.occ.importShapes(file_path)
            
            if heal:
                self.heal_geometry()
            
            gmsh.model.occ.synchronize()
            
            import_info = {
                "file": file_path,
                "format": "BREP",
                "healed": heal,
                "bounding_box": self.get_bounding_box()
            }
            
            logger.info(f"BREP import successful: {import_info}")
            return import_info
            
        except Exception as e:
            logger.error(f"Failed to import BREP file: {e}")
            raise
    
    def heal_geometry(self, 
                     tolerance: float = 1e-6,
                     fix_small_edges: bool = True,
                     fix_small_faces: bool = True,
                     sew_faces: bool = True) -> Dict:
        """
        Heal geometry defects
        
        Args:
            tolerance: Healing tolerance
            fix_small_edges: Remove small edges
            fix_small_faces: Remove small faces
            sew_faces: Sew disconnected faces
            
        Returns:
            Dictionary with healing results
        """
        if not GMSH_AVAILABLE:
            raise RuntimeError("GMSH not available")
        
        logger.info("Healing geometry defects")
        
        try:
            # Get all entities before healing
            entities_before = gmsh.model.occ.getEntities()
            
            # Heal the geometry
            gmsh.model.occ.healShapes()
            
            # Remove small features if requested
            if fix_small_edges or fix_small_faces:
                dim_tags = gmsh.model.occ.getEntities()
                
                if fix_small_edges:
                    edges = [(1, tag) for dim, tag in dim_tags if dim == 1]
                    if edges:
                        gmsh.model.occ.removeAllDuplicates()
                
                if fix_small_faces:
                    # Remove very small faces
                    faces = [(2, tag) for dim, tag in dim_tags if dim == 2]
                    for face_dim, face_tag in faces:
                        try:
                            mass = gmsh.model.occ.getMass(face_dim, face_tag)
                            if mass < tolerance:
                                gmsh.model.occ.remove([(face_dim, face_tag)])
                        except:
                            pass
            
            # Sew faces if requested
            if sew_faces:
                gmsh.model.occ.removeAllDuplicates()
            
            # Synchronize after healing
            gmsh.model.occ.synchronize()
            
            # Get entities after healing
            entities_after = gmsh.model.occ.getEntities()
            
            healing_results = {
                "tolerance": tolerance,
                "entities_before": len(entities_before),
                "entities_after": len(entities_after),
                "small_edges_fixed": fix_small_edges,
                "small_faces_fixed": fix_small_faces,
                "faces_sewn": sew_faces
            }
            
            logger.info(f"Geometry healing complete: {healing_results}")
            return healing_results
            
        except Exception as e:
            logger.error(f"Geometry healing failed: {e}")
            raise
    
    def boolean_union(self, 
                     objects1: List[Tuple[int, int]], 
                     objects2: List[Tuple[int, int]],
                     remove_originals: bool = True) -> List[Tuple[int, int]]:
        """
        Perform boolean union operation
        
        Args:
            objects1: First set of objects (dim, tag) pairs
            objects2: Second set of objects (dim, tag) pairs
            remove_originals: Remove original objects after operation
            
        Returns:
            List of resulting objects
        """
        if not GMSH_AVAILABLE:
            raise RuntimeError("GMSH not available")
        
        logger.info(f"Performing boolean union: {len(objects1)} + {len(objects2)} objects")
        
        try:
            result, result_map = gmsh.model.occ.fuse(
                objects1, objects2, 
                removeObject=remove_originals, 
                removeTool=remove_originals
            )
            
            gmsh.model.occ.synchronize()
            
            logger.info(f"Boolean union complete: {len(result)} resulting objects")
            return result
            
        except Exception as e:
            logger.error(f"Boolean union failed: {e}")
            raise
    
    def boolean_intersection(self,
                           objects1: List[Tuple[int, int]], 
                           objects2: List[Tuple[int, int]],
                           remove_originals: bool = True) -> List[Tuple[int, int]]:
        """
        Perform boolean intersection operation
        
        Args:
            objects1: First set of objects (dim, tag) pairs
            objects2: Second set of objects (dim, tag) pairs
            remove_originals: Remove original objects after operation
            
        Returns:
            List of resulting objects
        """
        if not GMSH_AVAILABLE:
            raise RuntimeError("GMSH not available")
        
        logger.info(f"Performing boolean intersection")
        
        try:
            result, result_map = gmsh.model.occ.intersect(
                objects1, objects2,
                removeObject=remove_originals,
                removeTool=remove_originals
            )
            
            gmsh.model.occ.synchronize()
            
            logger.info(f"Boolean intersection complete: {len(result)} resulting objects")
            return result
            
        except Exception as e:
            logger.error(f"Boolean intersection failed: {e}")
            raise
    
    def boolean_difference(self,
                         objects: List[Tuple[int, int]], 
                         tools: List[Tuple[int, int]],
                         remove_originals: bool = True) -> List[Tuple[int, int]]:
        """
        Perform boolean difference operation (subtract tools from objects)
        
        Args:
            objects: Objects to subtract from (dim, tag) pairs
            tools: Objects to subtract (dim, tag) pairs
            remove_originals: Remove original objects after operation
            
        Returns:
            List of resulting objects
        """
        if not GMSH_AVAILABLE:
            raise RuntimeError("GMSH not available")
        
        logger.info(f"Performing boolean difference")
        
        try:
            result, result_map = gmsh.model.occ.cut(
                objects, tools,
                removeObject=remove_originals,
                removeTool=remove_originals
            )
            
            gmsh.model.occ.synchronize()
            
            logger.info(f"Boolean difference complete: {len(result)} resulting objects")
            return result
            
        except Exception as e:
            logger.error(f"Boolean difference failed: {e}")
            raise
    
    def get_bounding_box(self, dim: int = -1, tag: int = -1) -> Dict:
        """
        Get bounding box of geometry
        
        Args:
            dim: Dimension of entity (-1 for all)
            tag: Tag of entity (-1 for all)
            
        Returns:
            Dictionary with bounding box coordinates
        """
        if not GMSH_AVAILABLE:
            return {"min": [0, 0, 0], "max": [0, 0, 0]}
        
        try:
            if dim == -1 or tag == -1:
                # Get bounding box of entire model
                x_min, y_min, z_min, x_max, y_max, z_max = gmsh.model.getBoundingBox(-1, -1)
            else:
                # Get bounding box of specific entity
                x_min, y_min, z_min, x_max, y_max, z_max = gmsh.model.getBoundingBox(dim, tag)
            
            return {
                "min": [x_min, y_min, z_min],
                "max": [x_max, y_max, z_max],
                "size": [x_max - x_min, y_max - y_min, z_max - z_min],
                "center": [(x_min + x_max) / 2, (y_min + y_max) / 2, (z_min + z_max) / 2]
            }
        except:
            return {"min": [0, 0, 0], "max": [0, 0, 0]}
    
    def export_geometry(self, file_path: str, format: Optional[str] = None) -> str:
        """
        Export geometry to file
        
        Args:
            file_path: Output file path
            format: Export format (auto-detected if None)
            
        Returns:
            Path to exported file
        """
        if not GMSH_AVAILABLE:
            raise RuntimeError("GMSH not available")
        
        # Auto-detect format from extension
        if format is None:
            ext = Path(file_path).suffix.lower()
            format_map = {
                '.step': 'step',
                '.stp': 'step',
                '.iges': 'iges',
                '.igs': 'iges',
                '.brep': 'brep',
                '.stl': 'stl'
            }
            format = format_map.get(ext, 'step')
        
        logger.info(f"Exporting geometry to {file_path} (format: {format})")
        
        try:
            if format.lower() in ['step', 'iges', 'brep']:
                gmsh.model.occ.exportShapes(file_path)
            elif format.lower() == 'stl':
                gmsh.write(file_path)
            else:
                raise ValueError(f"Unsupported export format: {format}")
            
            logger.info(f"Geometry exported successfully: {file_path}")
            return file_path
            
        except Exception as e:
            logger.error(f"Failed to export geometry: {e}")
            raise
    
    def cleanup(self):
        """Clean up resources"""
        if self.gmsh_initialized and GMSH_AVAILABLE:
            if gmsh.isInitialized():
                gmsh.clear()
    
    def __enter__(self):
        """Context manager entry"""
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit"""
        self.cleanup()