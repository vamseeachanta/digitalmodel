"""
FreeCAD API Wrapper - Core interface for FreeCAD operations
"""

import sys
import os
from pathlib import Path
from typing import Optional, List, Dict, Any, Union
from loguru import logger

# Try to import FreeCAD
try:
    import FreeCAD
    import FreeCADGui
    import Part
    import Sketcher
    import Draft
    FREECAD_AVAILABLE = True
except ImportError:
    FREECAD_AVAILABLE = False
    logger.warning("FreeCAD Python bindings not found. Running in limited mode.")
    # Create mock objects for development
    class FreeCAD:
        ActiveDocument = None
        @staticmethod
        def newDocument(name): return None
        @staticmethod
        def openDocument(path): return None
        @staticmethod
        def closeDocument(name): return None
    
    class Part:
        @staticmethod
        def makeBox(l, w, h): return None
        @staticmethod
        def makeCylinder(r, h): return None
        @staticmethod
        def makeSphere(r): return None


class FreeCADAPIWrapper:
    """
    Wrapper class for FreeCAD Python API operations
    Provides a unified interface for CAD operations
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize the FreeCAD API wrapper
        
        Args:
            config: Configuration dictionary
        """
        self.config = config or {}
        self.freecad_available = FREECAD_AVAILABLE
        self.active_document = None
        self.documents = {}
        
        # Set up FreeCAD library path if provided
        freecad_lib = self.config.get('paths', {}).get('freecad_lib')
        if freecad_lib and not FREECAD_AVAILABLE:
            self._setup_freecad_path(freecad_lib)
        
        logger.info(f"FreeCAD API Wrapper initialized - Available: {self.freecad_available}")
    
    def _setup_freecad_path(self, freecad_path: str) -> None:
        """Add FreeCAD to Python path"""
        if os.path.exists(freecad_path):
            sys.path.append(freecad_path)
            try:
                import FreeCAD
                self.freecad_available = True
                logger.info(f"FreeCAD loaded from: {freecad_path}")
            except ImportError:
                logger.error(f"Failed to import FreeCAD from: {freecad_path}")
    
    # Document Management Functions
    
    def create_document(self, name: str = "Unnamed") -> Any:
        """
        Create a new FreeCAD document
        
        Args:
            name: Name of the document
            
        Returns:
            FreeCAD document object
        """
        if not self.freecad_available:
            logger.warning("FreeCAD not available - returning mock document")
            return None
        
        try:
            doc = FreeCAD.newDocument(name)
            self.active_document = doc
            self.documents[name] = doc
            logger.info(f"Created new document: {name}")
            return doc
        except Exception as e:
            logger.error(f"Failed to create document: {e}")
            raise
    
    def open_document(self, file_path: str) -> Any:
        """
        Open an existing FreeCAD document
        
        Args:
            file_path: Path to the FreeCAD file
            
        Returns:
            FreeCAD document object
        """
        if not self.freecad_available:
            logger.warning("FreeCAD not available - cannot open document")
            return None
        
        try:
            path = Path(file_path).resolve()
            if not path.exists():
                raise FileNotFoundError(f"File not found: {file_path}")
            
            doc = FreeCAD.openDocument(str(path))
            self.active_document = doc
            doc_name = path.stem
            self.documents[doc_name] = doc
            logger.info(f"Opened document: {file_path}")
            return doc
        except Exception as e:
            logger.error(f"Failed to open document: {e}")
            raise
    
    def save_document(self, file_path: Optional[str] = None, document: Optional[Any] = None) -> bool:
        """
        Save a FreeCAD document
        
        Args:
            file_path: Path where to save the document
            document: Document to save (uses active if not provided)
            
        Returns:
            True if successful
        """
        if not self.freecad_available:
            logger.warning("FreeCAD not available - cannot save document")
            return False
        
        doc = document or self.active_document
        if not doc:
            logger.error("No document to save")
            return False
        
        try:
            if file_path:
                doc.saveAs(file_path)
                logger.info(f"Document saved to: {file_path}")
            else:
                doc.save()
                logger.info(f"Document saved: {doc.Name}")
            return True
        except Exception as e:
            logger.error(f"Failed to save document: {e}")
            return False
    
    def close_document(self, name: Optional[str] = None) -> bool:
        """
        Close a FreeCAD document
        
        Args:
            name: Name of document to close (uses active if not provided)
            
        Returns:
            True if successful
        """
        if not self.freecad_available:
            return False
        
        try:
            if name:
                FreeCAD.closeDocument(name)
                if name in self.documents:
                    del self.documents[name]
            elif self.active_document:
                FreeCAD.closeDocument(self.active_document.Name)
                name = self.active_document.Name
                if name in self.documents:
                    del self.documents[name]
                self.active_document = None
            
            logger.info(f"Closed document: {name}")
            return True
        except Exception as e:
            logger.error(f"Failed to close document: {e}")
            return False
    
    def export_document(self, file_path: str, format: Optional[str] = None) -> bool:
        """
        Export document to various formats
        
        Args:
            file_path: Output file path
            format: Export format (STEP, IGES, STL, etc.)
            
        Returns:
            True if successful
        """
        if not self.freecad_available or not self.active_document:
            return False
        
        try:
            # Determine format from extension if not provided
            if not format:
                format = Path(file_path).suffix[1:].upper()
            
            # Get objects to export
            objects = self.active_document.Objects
            
            if format in ['STEP', 'STP']:
                Part.export(objects, file_path)
            elif format in ['IGES', 'IGS']:
                Part.export(objects, file_path)
            elif format == 'STL':
                import Mesh
                Mesh.export(objects, file_path)
            elif format == 'DXF':
                import importDXF
                importDXF.export(objects, file_path)
            else:
                logger.error(f"Unsupported export format: {format}")
                return False
            
            logger.info(f"Exported to {format}: {file_path}")
            return True
        except Exception as e:
            logger.error(f"Export failed: {e}")
            return False
    
    # Object Creation Functions
    
    def create_box(self, length: float, width: float, height: float, 
                   position: Optional[tuple] = None, name: Optional[str] = None) -> Any:
        """
        Create a box/cuboid object
        
        Args:
            length: Length of the box (X)
            width: Width of the box (Y)
            height: Height of the box (Z)
            position: Position tuple (x, y, z)
            name: Object name
            
        Returns:
            Created object
        """
        if not self.freecad_available or not self.active_document:
            return None
        
        try:
            box = Part.makeBox(length, width, height)
            
            if position:
                box.Placement.Base = FreeCAD.Vector(*position)
            
            obj = self.active_document.addObject("Part::Feature", name or "Box")
            obj.Shape = box
            
            logger.info(f"Created box: {name or 'Box'} ({length}x{width}x{height})")
            return obj
        except Exception as e:
            logger.error(f"Failed to create box: {e}")
            return None
    
    def create_cylinder(self, radius: float, height: float,
                       position: Optional[tuple] = None, name: Optional[str] = None) -> Any:
        """
        Create a cylinder object
        
        Args:
            radius: Cylinder radius
            height: Cylinder height
            position: Position tuple (x, y, z)
            name: Object name
            
        Returns:
            Created object
        """
        if not self.freecad_available or not self.active_document:
            return None
        
        try:
            cylinder = Part.makeCylinder(radius, height)
            
            if position:
                cylinder.Placement.Base = FreeCAD.Vector(*position)
            
            obj = self.active_document.addObject("Part::Feature", name or "Cylinder")
            obj.Shape = cylinder
            
            logger.info(f"Created cylinder: {name or 'Cylinder'} (r={radius}, h={height})")
            return obj
        except Exception as e:
            logger.error(f"Failed to create cylinder: {e}")
            return None
    
    def create_sphere(self, radius: float, position: Optional[tuple] = None,
                     name: Optional[str] = None) -> Any:
        """
        Create a sphere object
        
        Args:
            radius: Sphere radius
            position: Position tuple (x, y, z)
            name: Object name
            
        Returns:
            Created object
        """
        if not self.freecad_available or not self.active_document:
            return None
        
        try:
            sphere = Part.makeSphere(radius)
            
            if position:
                sphere.Placement.Base = FreeCAD.Vector(*position)
            
            obj = self.active_document.addObject("Part::Feature", name or "Sphere")
            obj.Shape = sphere
            
            logger.info(f"Created sphere: {name or 'Sphere'} (r={radius})")
            return obj
        except Exception as e:
            logger.error(f"Failed to create sphere: {e}")
            return None
    
    # Object Manipulation Functions
    
    def move_object(self, obj: Any, position: tuple) -> bool:
        """
        Move an object to a new position
        
        Args:
            obj: Object to move
            position: New position (x, y, z)
            
        Returns:
            True if successful
        """
        if not self.freecad_available:
            return False
        
        try:
            obj.Placement.Base = FreeCAD.Vector(*position)
            logger.debug(f"Moved object to: {position}")
            return True
        except Exception as e:
            logger.error(f"Failed to move object: {e}")
            return False
    
    def rotate_object(self, obj: Any, axis: tuple, angle: float) -> bool:
        """
        Rotate an object around an axis
        
        Args:
            obj: Object to rotate
            axis: Rotation axis (x, y, z)
            angle: Rotation angle in degrees
            
        Returns:
            True if successful
        """
        if not self.freecad_available:
            return False
        
        try:
            rotation = FreeCAD.Rotation(FreeCAD.Vector(*axis), angle)
            obj.Placement.Rotation = rotation
            logger.debug(f"Rotated object by {angle} degrees")
            return True
        except Exception as e:
            logger.error(f"Failed to rotate object: {e}")
            return False
    
    def scale_object(self, obj: Any, scale_factor: Union[float, tuple]) -> bool:
        """
        Scale an object
        
        Args:
            obj: Object to scale
            scale_factor: Scale factor (uniform or per-axis)
            
        Returns:
            True if successful
        """
        if not self.freecad_available:
            return False
        
        try:
            if isinstance(scale_factor, (int, float)):
                scale = (scale_factor, scale_factor, scale_factor)
            else:
                scale = scale_factor
            
            # Create scaled shape
            matrix = FreeCAD.Matrix()
            matrix.scale(*scale)
            obj.Shape = obj.Shape.transformGeometry(matrix)
            
            logger.debug(f"Scaled object by: {scale}")
            return True
        except Exception as e:
            logger.error(f"Failed to scale object: {e}")
            return False
    
    # Utility Functions
    
    def get_object_by_name(self, name: str) -> Optional[Any]:
        """Get an object from the active document by name"""
        if not self.active_document:
            return None
        
        try:
            return self.active_document.getObject(name)
        except:
            return None
    
    def list_objects(self) -> List[str]:
        """List all objects in the active document"""
        if not self.active_document:
            return []
        
        return [obj.Name for obj in self.active_document.Objects]
    
    def get_object_properties(self, obj: Any) -> Dict[str, Any]:
        """Get properties of an object"""
        if not obj:
            return {}
        
        properties = {}
        try:
            properties['Name'] = obj.Name
            properties['Label'] = obj.Label
            if hasattr(obj, 'Shape'):
                shape = obj.Shape
                properties['Volume'] = shape.Volume
                properties['Area'] = shape.Area
                properties['BoundBox'] = {
                    'Min': (shape.BoundBox.XMin, shape.BoundBox.YMin, shape.BoundBox.ZMin),
                    'Max': (shape.BoundBox.XMax, shape.BoundBox.YMax, shape.BoundBox.ZMax)
                }
        except Exception as e:
            logger.error(f"Failed to get object properties: {e}")
        
        return properties
    
    def recompute(self) -> bool:
        """Recompute the active document"""
        if not self.active_document:
            return False
        
        try:
            self.active_document.recompute()
            logger.debug("Document recomputed")
            return True
        except Exception as e:
            logger.error(f"Failed to recompute: {e}")
            return False