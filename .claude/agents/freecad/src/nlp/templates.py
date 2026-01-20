"""
Template Library for FreeCAD Script Generation
Provides reusable templates for common CAD operations
"""

import json
from pathlib import Path
from typing import Dict, Any, Optional, List
from string import Template
from loguru import logger


class ScriptTemplate:
    """
    Represents a reusable script template
    """
    
    def __init__(self, name: str, template_str: str, 
                 parameters: List[str], description: str = ""):
        """
        Initialize a script template
        
        Args:
            name: Template name
            template_str: Template string with placeholders
            parameters: List of required parameters
            description: Template description
        """
        self.name = name
        self.template = Template(template_str)
        self.parameters = parameters
        self.description = description
    
    def render(self, params: Dict[str, Any]) -> str:
        """
        Render the template with given parameters
        
        Args:
            params: Parameter values
            
        Returns:
            Rendered script
        """
        # Check for missing parameters
        missing = [p for p in self.parameters if p not in params]
        if missing:
            logger.warning(f"Missing parameters for template {self.name}: {missing}")
            # Use defaults for missing parameters
            for param in missing:
                params[param] = self._get_default_value(param)
        
        try:
            return self.template.safe_substitute(params)
        except Exception as e:
            logger.error(f"Template rendering failed: {e}")
            return ""
    
    def _get_default_value(self, param: str) -> Any:
        """Get default value for a parameter"""
        defaults = {
            'length': 100,
            'width': 100,
            'height': 100,
            'radius': 50,
            'angle': 90,
            'thickness': 10,
            'name': 'Object',
            'position': '(0, 0, 0)',
            'color': 'gray',
            'material': 'steel'
        }
        return defaults.get(param, 0)


class TemplateLibrary:
    """
    Manages a collection of script templates
    """
    
    def __init__(self, template_dir: Optional[str] = None):
        """
        Initialize the template library
        
        Args:
            template_dir: Directory containing template files
        """
        self.templates: Dict[str, ScriptTemplate] = {}
        self.template_dir = Path(template_dir) if template_dir else None
        
        # Load built-in templates
        self._load_builtin_templates()
        
        # Load custom templates from directory
        if self.template_dir and self.template_dir.exists():
            self._load_custom_templates()
    
    def _load_builtin_templates(self) -> None:
        """Load built-in template definitions"""
        
        # Parametric Box Template
        self.add_template(
            name="parametric_box",
            template_str="""
import FreeCAD
import Part

# Parametric Box
doc = FreeCAD.ActiveDocument or FreeCAD.newDocument("ParametricDesign")

# Create spreadsheet for parameters
sheet = doc.addObject('Spreadsheet::Sheet', 'Parameters')
sheet.set('A1', 'Length')
sheet.set('B1', '$length')
sheet.set('A2', 'Width')
sheet.set('B2', '$width')
sheet.set('A3', 'Height')
sheet.set('B3', '$height')

# Create box with parametric dimensions
box = doc.addObject("Part::Box", "$name")
box.Length = '$lengthm'
box.Width = '$widthm'
box.Height = '$heightmm'

# Link to spreadsheet
box.setExpression('Length', 'Parameters.B1')
box.setExpression('Width', 'Parameters.B2')
box.setExpression('Height', 'Parameters.B3')

doc.recompute()
print("Created parametric box: $name")
""",
            parameters=['name', 'length', 'width', 'height'],
            description="Creates a box with parametric dimensions linked to spreadsheet"
        )
        
        # Gear Template
        self.add_template(
            name="gear",
            template_str="""
import FreeCAD
import Part
import math

# Gear parameters
teeth = $teeth
module = $module  # mm
pressure_angle = $pressure_angle  # degrees
thickness = $thickness  # mm

doc = FreeCAD.ActiveDocument or FreeCAD.newDocument("GearDesign")

# Calculate gear dimensions
pitch_diameter = teeth * module
base_diameter = pitch_diameter * math.cos(math.radians(pressure_angle))
addendum = module
dedendum = 1.25 * module
outside_diameter = pitch_diameter + 2 * addendum
root_diameter = pitch_diameter - 2 * dedendum

# Create gear profile
import Draft
gear_profile = []

for i in range(teeth):
    angle = (360.0 / teeth) * i
    # Simplified tooth profile (for demonstration)
    tooth_angle = math.radians(angle)
    
    # Add tooth points
    x1 = (pitch_diameter / 2) * math.cos(tooth_angle - math.radians(360/teeth/4))
    y1 = (pitch_diameter / 2) * math.sin(tooth_angle - math.radians(360/teeth/4))
    
    x2 = (outside_diameter / 2) * math.cos(tooth_angle)
    y2 = (outside_diameter / 2) * math.sin(tooth_angle)
    
    x3 = (pitch_diameter / 2) * math.cos(tooth_angle + math.radians(360/teeth/4))
    y3 = (pitch_diameter / 2) * math.sin(tooth_angle + math.radians(360/teeth/4))
    
    gear_profile.extend([
        FreeCAD.Vector(x1, y1, 0),
        FreeCAD.Vector(x2, y2, 0),
        FreeCAD.Vector(x3, y3, 0)
    ])

# Create wire from points
wire = Part.makePolygon(gear_profile + [gear_profile[0]])

# Create face and extrude
face = Part.Face(wire)
gear = face.extrude(FreeCAD.Vector(0, 0, thickness))

# Add center hole
hole = Part.makeCylinder(module * 2, thickness + 1)
gear = gear.cut(hole)

# Add to document
gear_obj = doc.addObject("Part::Feature", "$name")
gear_obj.Shape = gear

doc.recompute()
print(f"Created gear: $name (teeth=$teeth, module=$module)")
""",
            parameters=['name', 'teeth', 'module', 'pressure_angle', 'thickness'],
            description="Creates a parametric gear"
        )
        
        # Pipe/Tube Template
        self.add_template(
            name="pipe",
            template_str="""
import FreeCAD
import Part

# Pipe parameters
outer_radius = $outer_radius
inner_radius = $inner_radius
length = $length

doc = FreeCAD.ActiveDocument or FreeCAD.newDocument("PipeDesign")

# Create pipe
outer_cylinder = Part.makeCylinder(outer_radius, length)
inner_cylinder = Part.makeCylinder(inner_radius, length)
pipe = outer_cylinder.cut(inner_cylinder)

# Add to document
pipe_obj = doc.addObject("Part::Feature", "$name")
pipe_obj.Shape = pipe

# Position if specified
pipe_obj.Placement.Base = FreeCAD.Vector$position

doc.recompute()
print(f"Created pipe: $name (OD=${outer_radius*2}mm, ID=${inner_radius*2}mm, L=${length}mm)")
""",
            parameters=['name', 'outer_radius', 'inner_radius', 'length', 'position'],
            description="Creates a hollow pipe/tube"
        )
        
        # Flange Template
        self.add_template(
            name="flange",
            template_str="""
import FreeCAD
import Part

# Flange parameters
outer_diameter = $outer_diameter
inner_diameter = $inner_diameter
thickness = $thickness
bolt_circle_diameter = $bolt_circle_diameter
bolt_count = $bolt_count
bolt_diameter = $bolt_diameter

doc = FreeCAD.ActiveDocument or FreeCAD.newDocument("FlangeDesign")

# Create flange body
flange_outer = Part.makeCylinder(outer_diameter/2, thickness)
flange_inner = Part.makeCylinder(inner_diameter/2, thickness + 1)
flange = flange_outer.cut(flange_inner)

# Add bolt holes
for i in range(bolt_count):
    angle = (360.0 / bolt_count) * i
    x = (bolt_circle_diameter/2) * math.cos(math.radians(angle))
    y = (bolt_circle_diameter/2) * math.sin(math.radians(angle))
    
    bolt_hole = Part.makeCylinder(bolt_diameter/2, thickness + 1)
    bolt_hole.Placement.Base = FreeCAD.Vector(x, y, -0.5)
    flange = flange.cut(bolt_hole)

# Add to document
flange_obj = doc.addObject("Part::Feature", "$name")
flange_obj.Shape = flange

doc.recompute()
print(f"Created flange: $name (OD=${outer_diameter}mm, ID=${inner_diameter}mm, {bolt_count} bolts)")
""",
            parameters=['name', 'outer_diameter', 'inner_diameter', 'thickness', 
                       'bolt_circle_diameter', 'bolt_count', 'bolt_diameter'],
            description="Creates a pipe flange with bolt holes"
        )
        
        # Marine Bulkhead Template
        self.add_template(
            name="bulkhead",
            template_str="""
import FreeCAD
import Part

# Bulkhead parameters
width = $width  # mm
height = $height  # mm
thickness = $thickness  # mm
stiffener_spacing = $stiffener_spacing  # mm
stiffener_height = $stiffener_height  # mm

doc = FreeCAD.ActiveDocument or FreeCAD.newDocument("MarineDesign")

# Create main plate
plate = Part.makeBox(width, thickness, height)

# Add vertical stiffeners
num_stiffeners = int(width / stiffener_spacing)
for i in range(1, num_stiffeners):
    x_pos = i * stiffener_spacing
    stiffener = Part.makeBox(thickness, stiffener_height, height)
    stiffener.Placement.Base = FreeCAD.Vector(x_pos - thickness/2, thickness, 0)
    plate = plate.fuse(stiffener)

# Add horizontal stiffeners
h_stiffener_spacing = height / 3
for i in range(1, 3):
    z_pos = i * h_stiffener_spacing
    h_stiffener = Part.makeBox(width, stiffener_height, thickness)
    h_stiffener.Placement.Base = FreeCAD.Vector(0, thickness, z_pos - thickness/2)
    plate = plate.fuse(h_stiffener)

# Add to document
bulkhead_obj = doc.addObject("Part::Feature", "$name")
bulkhead_obj.Shape = plate

doc.recompute()
print(f"Created marine bulkhead: $name ({width}x{height}mm, t={thickness}mm)")
""",
            parameters=['name', 'width', 'height', 'thickness', 
                       'stiffener_spacing', 'stiffener_height'],
            description="Creates a marine bulkhead with stiffeners"
        )
        
        logger.info(f"Loaded {len(self.templates)} built-in templates")
    
    def _load_custom_templates(self) -> None:
        """Load custom templates from directory"""
        if not self.template_dir:
            return
        
        template_files = self.template_dir.glob("*.json")
        for file in template_files:
            try:
                with open(file, 'r') as f:
                    data = json.load(f)
                    self.add_template(
                        name=data['name'],
                        template_str=data['template'],
                        parameters=data['parameters'],
                        description=data.get('description', '')
                    )
                    logger.debug(f"Loaded custom template: {data['name']}")
            except Exception as e:
                logger.error(f"Failed to load template {file}: {e}")
    
    def add_template(self, name: str, template_str: str, 
                    parameters: List[str], description: str = "") -> None:
        """
        Add a template to the library
        
        Args:
            name: Template name
            template_str: Template string
            parameters: Required parameters
            description: Template description
        """
        template = ScriptTemplate(name, template_str, parameters, description)
        self.templates[name] = template
    
    def get_template(self, name: str) -> Optional[ScriptTemplate]:
        """
        Get a template by name
        
        Args:
            name: Template name
            
        Returns:
            Template object or None
        """
        return self.templates.get(name)
    
    def list_templates(self) -> List[Dict[str, Any]]:
        """
        List all available templates
        
        Returns:
            List of template information
        """
        return [
            {
                'name': name,
                'description': template.description,
                'parameters': template.parameters
            }
            for name, template in self.templates.items()
        ]
    
    def save_template(self, name: str, filename: str) -> bool:
        """
        Save a template to file
        
        Args:
            name: Template name
            filename: Output filename
            
        Returns:
            True if successful
        """
        template = self.templates.get(name)
        if not template:
            logger.error(f"Template not found: {name}")
            return False
        
        try:
            data = {
                'name': template.name,
                'template': template.template.template,
                'parameters': template.parameters,
                'description': template.description
            }
            
            with open(filename, 'w') as f:
                json.dump(data, f, indent=2)
            
            logger.info(f"Template saved to: {filename}")
            return True
        except Exception as e:
            logger.error(f"Failed to save template: {e}")
            return False