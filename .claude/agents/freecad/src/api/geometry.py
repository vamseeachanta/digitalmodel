"""
Geometry and constraint handling for FreeCAD operations
"""

from typing import List, Tuple, Optional, Dict, Any
from loguru import logger

try:
    import FreeCAD
    import Part
    import Sketcher
    FREECAD_AVAILABLE = True
except ImportError:
    FREECAD_AVAILABLE = False
    logger.warning("FreeCAD not available for geometry operations")


class GeometryHandler:
    """
    Handle complex geometry operations and constraints
    """
    
    def __init__(self, document: Any = None):
        """
        Initialize geometry handler
        
        Args:
            document: FreeCAD document to work with
        """
        self.document = document
        self.freecad_available = FREECAD_AVAILABLE
    
    def create_sketch(self, name: str = "Sketch", support: Optional[Any] = None) -> Any:
        """
        Create a new sketch
        
        Args:
            name: Name of the sketch
            support: Support face or plane for the sketch
            
        Returns:
            Created sketch object
        """
        if not self.freecad_available or not self.document:
            return None
        
        try:
            sketch = self.document.addObject('Sketcher::SketchObject', name)
            
            if support:
                sketch.Support = support
            else:
                # Create on XY plane by default
                sketch.Placement = FreeCAD.Placement(
                    FreeCAD.Vector(0, 0, 0),
                    FreeCAD.Rotation(FreeCAD.Vector(0, 0, 1), 0)
                )
            
            logger.info(f"Created sketch: {name}")
            return sketch
        except Exception as e:
            logger.error(f"Failed to create sketch: {e}")
            return None
    
    def add_line_to_sketch(self, sketch: Any, start: Tuple[float, float], 
                           end: Tuple[float, float]) -> int:
        """
        Add a line to a sketch
        
        Args:
            sketch: Sketch object
            start: Start point (x, y)
            end: End point (x, y)
            
        Returns:
            Index of the created line
        """
        if not self.freecad_available:
            return -1
        
        try:
            line_index = sketch.addGeometry(
                Part.LineSegment(
                    FreeCAD.Vector(start[0], start[1], 0),
                    FreeCAD.Vector(end[0], end[1], 0)
                )
            )
            logger.debug(f"Added line to sketch: {start} -> {end}")
            return line_index
        except Exception as e:
            logger.error(f"Failed to add line: {e}")
            return -1
    
    def add_circle_to_sketch(self, sketch: Any, center: Tuple[float, float], 
                            radius: float) -> int:
        """
        Add a circle to a sketch
        
        Args:
            sketch: Sketch object
            center: Center point (x, y)
            radius: Circle radius
            
        Returns:
            Index of the created circle
        """
        if not self.freecad_available:
            return -1
        
        try:
            circle_index = sketch.addGeometry(
                Part.Circle(
                    FreeCAD.Vector(center[0], center[1], 0),
                    FreeCAD.Vector(0, 0, 1),
                    radius
                )
            )
            logger.debug(f"Added circle to sketch: center={center}, r={radius}")
            return circle_index
        except Exception as e:
            logger.error(f"Failed to add circle: {e}")
            return -1
    
    def add_arc_to_sketch(self, sketch: Any, center: Tuple[float, float],
                         radius: float, start_angle: float, end_angle: float) -> int:
        """
        Add an arc to a sketch
        
        Args:
            sketch: Sketch object
            center: Center point (x, y)
            radius: Arc radius
            start_angle: Start angle in degrees
            end_angle: End angle in degrees
            
        Returns:
            Index of the created arc
        """
        if not self.freecad_available:
            return -1
        
        try:
            import math
            start_rad = math.radians(start_angle)
            end_rad = math.radians(end_angle)
            
            arc = Part.ArcOfCircle(
                Part.Circle(
                    FreeCAD.Vector(center[0], center[1], 0),
                    FreeCAD.Vector(0, 0, 1),
                    radius
                ),
                start_rad,
                end_rad
            )
            
            arc_index = sketch.addGeometry(arc)
            logger.debug(f"Added arc to sketch: center={center}, r={radius}, angles={start_angle}-{end_angle}")
            return arc_index
        except Exception as e:
            logger.error(f"Failed to add arc: {e}")
            return -1
    
    def add_constraint_coincident(self, sketch: Any, geo1: int, vertex1: int,
                                 geo2: int, vertex2: int) -> bool:
        """
        Add coincident constraint between two points
        
        Args:
            sketch: Sketch object
            geo1: First geometry index
            vertex1: First vertex index (1=start, 2=end)
            geo2: Second geometry index
            vertex2: Second vertex index
            
        Returns:
            True if successful
        """
        if not self.freecad_available:
            return False
        
        try:
            sketch.addConstraint(
                Sketcher.Constraint('Coincident', geo1, vertex1, geo2, vertex2)
            )
            logger.debug(f"Added coincident constraint")
            return True
        except Exception as e:
            logger.error(f"Failed to add coincident constraint: {e}")
            return False
    
    def add_constraint_horizontal(self, sketch: Any, geo_index: int) -> bool:
        """
        Add horizontal constraint to a line
        
        Args:
            sketch: Sketch object
            geo_index: Geometry index
            
        Returns:
            True if successful
        """
        if not self.freecad_available:
            return False
        
        try:
            sketch.addConstraint(Sketcher.Constraint('Horizontal', geo_index))
            logger.debug(f"Added horizontal constraint to geometry {geo_index}")
            return True
        except Exception as e:
            logger.error(f"Failed to add horizontal constraint: {e}")
            return False
    
    def add_constraint_vertical(self, sketch: Any, geo_index: int) -> bool:
        """
        Add vertical constraint to a line
        
        Args:
            sketch: Sketch object
            geo_index: Geometry index
            
        Returns:
            True if successful
        """
        if not self.freecad_available:
            return False
        
        try:
            sketch.addConstraint(Sketcher.Constraint('Vertical', geo_index))
            logger.debug(f"Added vertical constraint to geometry {geo_index}")
            return True
        except Exception as e:
            logger.error(f"Failed to add vertical constraint: {e}")
            return False
    
    def add_constraint_distance(self, sketch: Any, geo_index: int, 
                               distance: float, point_pos: int = 0) -> bool:
        """
        Add distance/length constraint
        
        Args:
            sketch: Sketch object
            geo_index: Geometry index
            distance: Distance value
            point_pos: Point position (0=line length, 1=start, 2=end)
            
        Returns:
            True if successful
        """
        if not self.freecad_available:
            return False
        
        try:
            if point_pos == 0:
                # Line length
                sketch.addConstraint(
                    Sketcher.Constraint('Distance', geo_index, distance)
                )
            else:
                # Point to origin distance
                sketch.addConstraint(
                    Sketcher.Constraint('Distance', geo_index, point_pos, distance)
                )
            
            logger.debug(f"Added distance constraint: {distance}")
            return True
        except Exception as e:
            logger.error(f"Failed to add distance constraint: {e}")
            return False
    
    def add_constraint_radius(self, sketch: Any, geo_index: int, radius: float) -> bool:
        """
        Add radius constraint to circle or arc
        
        Args:
            sketch: Sketch object
            geo_index: Geometry index
            radius: Radius value
            
        Returns:
            True if successful
        """
        if not self.freecad_available:
            return False
        
        try:
            sketch.addConstraint(
                Sketcher.Constraint('Radius', geo_index, radius)
            )
            logger.debug(f"Added radius constraint: {radius}")
            return True
        except Exception as e:
            logger.error(f"Failed to add radius constraint: {e}")
            return False
    
    def extrude_sketch(self, sketch: Any, length: float, name: str = "Extrusion") -> Any:
        """
        Extrude a sketch to create a solid
        
        Args:
            sketch: Sketch to extrude
            length: Extrusion length
            name: Name of the created object
            
        Returns:
            Created extrusion object
        """
        if not self.freecad_available or not self.document:
            return None
        
        try:
            extrusion = self.document.addObject("Part::Extrusion", name)
            extrusion.Base = sketch
            extrusion.DirMode = "Normal"
            extrusion.LengthFwd = length
            extrusion.Solid = True
            
            self.document.recompute()
            logger.info(f"Created extrusion: {name} (length={length})")
            return extrusion
        except Exception as e:
            logger.error(f"Failed to extrude sketch: {e}")
            return None
    
    def revolve_sketch(self, sketch: Any, angle: float = 360, 
                      axis: Optional[Tuple[float, float, float]] = None,
                      name: str = "Revolution") -> Any:
        """
        Revolve a sketch around an axis
        
        Args:
            sketch: Sketch to revolve
            angle: Revolution angle in degrees
            axis: Revolution axis (default is Y axis)
            name: Name of the created object
            
        Returns:
            Created revolution object
        """
        if not self.freecad_available or not self.document:
            return None
        
        try:
            revolution = self.document.addObject("Part::Revolution", name)
            revolution.Source = sketch
            revolution.Angle = angle
            
            if axis:
                revolution.Axis = FreeCAD.Vector(*axis)
            else:
                revolution.Axis = FreeCAD.Vector(0, 1, 0)  # Y axis
            
            revolution.Solid = True
            
            self.document.recompute()
            logger.info(f"Created revolution: {name} (angle={angle})")
            return revolution
        except Exception as e:
            logger.error(f"Failed to revolve sketch: {e}")
            return None
    
    def create_fillet(self, obj: Any, edges: List[int], radius: float) -> Any:
        """
        Create fillet on edges
        
        Args:
            obj: Object to fillet
            edges: List of edge indices
            radius: Fillet radius
            
        Returns:
            Filleted object
        """
        if not self.freecad_available or not self.document:
            return None
        
        try:
            fillet = self.document.addObject("Part::Fillet", f"{obj.Name}_Fillet")
            fillet.Base = obj
            
            edge_list = [(obj, f"Edge{i}") for i in edges]
            fillet.Edges = edge_list
            fillet.Radius = radius
            
            self.document.recompute()
            logger.info(f"Created fillet with radius {radius}")
            return fillet
        except Exception as e:
            logger.error(f"Failed to create fillet: {e}")
            return None
    
    def create_chamfer(self, obj: Any, edges: List[int], distance: float) -> Any:
        """
        Create chamfer on edges
        
        Args:
            obj: Object to chamfer
            edges: List of edge indices
            distance: Chamfer distance
            
        Returns:
            Chamfered object
        """
        if not self.freecad_available or not self.document:
            return None
        
        try:
            chamfer = self.document.addObject("Part::Chamfer", f"{obj.Name}_Chamfer")
            chamfer.Base = obj
            
            edge_list = [(obj, f"Edge{i}") for i in edges]
            chamfer.Edges = edge_list
            chamfer.Length = distance
            
            self.document.recompute()
            logger.info(f"Created chamfer with distance {distance}")
            return chamfer
        except Exception as e:
            logger.error(f"Failed to create chamfer: {e}")
            return None
    
    def boolean_cut(self, tool: Any, base: Any, name: str = "Cut") -> Any:
        """
        Perform boolean cut operation
        
        Args:
            tool: Cutting tool object
            base: Base object to cut from
            name: Name of result
            
        Returns:
            Result object
        """
        if not self.freecad_available or not self.document:
            return None
        
        try:
            cut = self.document.addObject("Part::Cut", name)
            cut.Base = base
            cut.Tool = tool
            
            self.document.recompute()
            logger.info(f"Created boolean cut: {name}")
            return cut
        except Exception as e:
            logger.error(f"Failed to create cut: {e}")
            return None
    
    def boolean_union(self, objects: List[Any], name: str = "Union") -> Any:
        """
        Perform boolean union operation
        
        Args:
            objects: List of objects to unite
            name: Name of result
            
        Returns:
            Result object
        """
        if not self.freecad_available or not self.document:
            return None
        
        try:
            union = self.document.addObject("Part::MultiFuse", name)
            union.Shapes = objects
            
            self.document.recompute()
            logger.info(f"Created boolean union: {name}")
            return union
        except Exception as e:
            logger.error(f"Failed to create union: {e}")
            return None