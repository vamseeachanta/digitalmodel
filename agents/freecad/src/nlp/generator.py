"""
Python Script Generator for FreeCAD Agent
Converts parsed commands into executable FreeCAD Python scripts
"""

import json
from typing import Dict, List, Any, Optional
from pathlib import Path
from datetime import datetime
from loguru import logger

from .parser import ParsedCommand, CommandIntent, ObjectType
from .templates import ScriptTemplate, TemplateLibrary


class ScriptGenerator:
    """
    Generates FreeCAD Python scripts from parsed commands
    """
    
    def __init__(self, template_dir: Optional[str] = None):
        """
        Initialize the script generator
        
        Args:
            template_dir: Directory containing script templates
        """
        self.template_library = TemplateLibrary(template_dir)
        self.script_history = []
        self.optimization_enabled = True
        self.validation_enabled = True
        
    def generate(self, command: ParsedCommand) -> str:
        """
        Generate a Python script from a parsed command
        
        Args:
            command: Parsed command object
            
        Returns:
            Generated Python script as string
        """
        logger.debug(f"Generating script for intent: {command.intent.value}")
        
        # Select generation method based on intent
        if command.intent == CommandIntent.CREATE_OBJECT:
            script = self._generate_create_object(command)
        elif command.intent == CommandIntent.MODIFY_OBJECT:
            script = self._generate_modify_object(command)
        elif command.intent == CommandIntent.DELETE_OBJECT:
            script = self._generate_delete_object(command)
        elif command.intent == CommandIntent.EXPORT_FILE:
            script = self._generate_export_file(command)
        elif command.intent == CommandIntent.CREATE_SKETCH:
            script = self._generate_create_sketch(command)
        elif command.intent == CommandIntent.ADD_CONSTRAINT:
            script = self._generate_add_constraint(command)
        elif command.intent == CommandIntent.ASSEMBLY_OPERATION:
            script = self._generate_assembly_operation(command)
        else:
            script = self._generate_generic(command)
        
        # Validate script if enabled
        if self.validation_enabled:
            script = self._validate_script(script)
        
        # Optimize script if enabled
        if self.optimization_enabled:
            script = self._optimize_script(script)
        
        # Add to history
        self.script_history.append({
            'timestamp': datetime.now().isoformat(),
            'command': command.raw_prompt,
            'script': script
        })
        
        return script
    
    def _generate_create_object(self, command: ParsedCommand) -> str:
        """Generate script for object creation"""
        params = command.parameters
        obj_type = command.object_type
        
        if obj_type == ObjectType.BOX:
            return self._generate_box_script(params)
        elif obj_type == ObjectType.CYLINDER:
            return self._generate_cylinder_script(params)
        elif obj_type == ObjectType.SPHERE:
            return self._generate_sphere_script(params)
        elif obj_type == ObjectType.HULL:
            return self._generate_hull_script(params)
        elif obj_type == ObjectType.BEAM:
            return self._generate_beam_script(params)
        else:
            # Use template if available
            template = self.template_library.get_template(f"create_{obj_type.value}")
            if template:
                return template.render(params)
            else:
                return self._generate_generic_create(obj_type, params)
    
    def _generate_box_script(self, params: Dict[str, Any]) -> str:
        """Generate script for box creation"""
        length = params.get('length', 100)
        width = params.get('width', 100)
        height = params.get('height', 100)
        position = params.get('position', (0, 0, 0))
        name = params.get('name', 'Box')
        
        script = f"""
import FreeCAD
import Part

# Create box
box = Part.makeBox({length}, {width}, {height})

# Set position
box.Placement.Base = FreeCAD.Vector{position}

# Add to document
doc = FreeCAD.ActiveDocument
if not doc:
    doc = FreeCAD.newDocument("GeneratedDoc")

obj = doc.addObject("Part::Feature", "{name}")
obj.Shape = box

# Recompute
doc.recompute()

print("Created box: {name} ({length}x{width}x{height}) at {position}")
"""
        return script.strip()
    
    def _generate_cylinder_script(self, params: Dict[str, Any]) -> str:
        """Generate script for cylinder creation"""
        radius = params.get('radius', 50)
        height = params.get('height', 100)
        position = params.get('position', (0, 0, 0))
        name = params.get('name', 'Cylinder')
        
        script = f"""
import FreeCAD
import Part

# Create cylinder
cylinder = Part.makeCylinder({radius}, {height})

# Set position
cylinder.Placement.Base = FreeCAD.Vector{position}

# Add to document
doc = FreeCAD.ActiveDocument
if not doc:
    doc = FreeCAD.newDocument("GeneratedDoc")

obj = doc.addObject("Part::Feature", "{name}")
obj.Shape = cylinder

# Recompute
doc.recompute()

print("Created cylinder: {name} (r={radius}, h={height}) at {position}")
"""
        return script.strip()
    
    def _generate_sphere_script(self, params: Dict[str, Any]) -> str:
        """Generate script for sphere creation"""
        radius = params.get('radius', 50)
        position = params.get('position', (0, 0, 0))
        name = params.get('name', 'Sphere')
        
        script = f"""
import FreeCAD
import Part

# Create sphere
sphere = Part.makeSphere({radius})

# Set position
sphere.Placement.Base = FreeCAD.Vector{position}

# Add to document
doc = FreeCAD.ActiveDocument
if not doc:
    doc = FreeCAD.newDocument("GeneratedDoc")

obj = doc.addObject("Part::Feature", "{name}")
obj.Shape = sphere

# Recompute
doc.recompute()

print("Created sphere: {name} (r={radius}) at {position}")
"""
        return script.strip()
    
    def _generate_hull_script(self, params: Dict[str, Any]) -> str:
        """Generate script for hull creation (marine engineering)"""
        length = params.get('length', 150000)  # 150m default
        beam = params.get('width', 25000)  # 25m beam
        depth = params.get('height', 15000)  # 15m depth
        name = params.get('name', 'Hull')
        
        script = f"""
import FreeCAD
import Part
import Draft

# Hull parameters (in mm)
length = {length}
beam = {beam}
depth = {depth}

# Create hull profile points
points = []
# Bow
points.append(FreeCAD.Vector(0, 0, 0))
points.append(FreeCAD.Vector(length * 0.1, beam * 0.3, 0))
points.append(FreeCAD.Vector(length * 0.2, beam * 0.45, 0))
# Midship
points.append(FreeCAD.Vector(length * 0.5, beam * 0.5, 0))
# Stern
points.append(FreeCAD.Vector(length * 0.8, beam * 0.45, 0))
points.append(FreeCAD.Vector(length * 0.9, beam * 0.3, 0))
points.append(FreeCAD.Vector(length, 0, 0))

# Create hull shape using loft
doc = FreeCAD.ActiveDocument
if not doc:
    doc = FreeCAD.newDocument("HullDesign")

# Create waterline sections
sections = []
for z in [0, depth * 0.3, depth * 0.6, depth]:
    section_points = []
    scale = 1.0 - (z / depth) * 0.3  # Taper towards keel
    for p in points:
        section_points.append(FreeCAD.Vector(p.x, p.y * scale, z))
    
    # Create BSpline for this section
    spline = Part.BSplineCurve()
    spline.interpolate(section_points)
    sections.append(spline.toShape())

# Create loft through sections
loft = Part.makeLoft(sections, True, False, False)

# Add to document
hull_obj = doc.addObject("Part::Feature", "{name}")
hull_obj.Shape = loft

# Mirror for full hull
mirror = doc.addObject("Part::Mirroring", "{name}_Full")
mirror.Source = hull_obj
mirror.Normal = (0, 1, 0)

doc.recompute()

print("Created hull: {name} (L={length/1000}m, B={beam/1000}m, D={depth/1000}m)")
"""
        return script.strip()
    
    def _generate_beam_script(self, params: Dict[str, Any]) -> str:
        """Generate script for structural beam creation"""
        length = params.get('length', 5000)  # 5m default
        profile = params.get('profile', 'I-beam')
        width = params.get('width', 200)
        height = params.get('height', 400)
        thickness = params.get('thickness', 20)
        name = params.get('name', 'Beam')
        
        script = f"""
import FreeCAD
import Part

# Beam parameters
length = {length}
width = {width}
height = {height}
thickness = {thickness}

doc = FreeCAD.ActiveDocument
if not doc:
    doc = FreeCAD.newDocument("StructuralDesign")

# Create I-beam profile
# Flanges
top_flange = Part.makeBox(width, thickness, length)
top_flange.Placement.Base = FreeCAD.Vector(0, height - thickness, 0)

bottom_flange = Part.makeBox(width, thickness, length)

# Web
web = Part.makeBox(thickness, height - 2 * thickness, length)
web.Placement.Base = FreeCAD.Vector((width - thickness) / 2, thickness, 0)

# Combine parts
beam = top_flange.fuse(bottom_flange)
beam = beam.fuse(web)

# Add to document
beam_obj = doc.addObject("Part::Feature", "{name}")
beam_obj.Shape = beam

doc.recompute()

print("Created I-beam: {name} (L={length}mm, H={height}mm, W={width}mm)")
"""
        return script.strip()
    
    def _generate_generic_create(self, obj_type: ObjectType, params: Dict[str, Any]) -> str:
        """Generate generic creation script"""
        name = params.get('name', obj_type.value.capitalize())
        
        script = f"""
import FreeCAD
import Part

# Generic object creation for {obj_type.value}
doc = FreeCAD.ActiveDocument
if not doc:
    doc = FreeCAD.newDocument("GeneratedDoc")

# Create placeholder object
# TODO: Implement specific geometry for {obj_type.value}
obj = doc.addObject("Part::Feature", "{name}")

# Add your specific geometry creation code here
# obj.Shape = Part.make{obj_type.value.capitalize()}(...)

doc.recompute()

print("Created {obj_type.value}: {name}")
print("Note: This is a placeholder. Implement specific geometry for {obj_type.value}")
"""
        return script.strip()
    
    def _generate_modify_object(self, command: ParsedCommand) -> str:
        """Generate script for object modification"""
        params = command.parameters
        name = params.get('name', 'Object')
        
        modifications = []
        if 'position' in params:
            modifications.append(f"obj.Placement.Base = FreeCAD.Vector{params['position']}")
        if 'rotation' in params:
            modifications.append(f"obj.Placement.Rotation = FreeCAD.Rotation({params['rotation']})")
        if 'scale' in params:
            modifications.append(f"# Scale object by factor {params['scale']}")
        
        script = f"""
import FreeCAD

doc = FreeCAD.ActiveDocument
if not doc:
    print("No active document")
    exit()

# Find object
obj = doc.getObject("{name}")
if not obj:
    print("Object '{name}' not found")
    exit()

# Apply modifications
{chr(10).join(modifications)}

doc.recompute()
print("Modified object: {name}")
"""
        return script.strip()
    
    def _generate_delete_object(self, command: ParsedCommand) -> str:
        """Generate script for object deletion"""
        name = command.parameters.get('name', 'Object')
        
        script = f"""
import FreeCAD

doc = FreeCAD.ActiveDocument
if not doc:
    print("No active document")
    exit()

# Find and remove object
obj = doc.getObject("{name}")
if obj:
    doc.removeObject("{name}")
    doc.recompute()
    print("Deleted object: {name}")
else:
    print("Object '{name}' not found")
"""
        return script.strip()
    
    def _generate_export_file(self, command: ParsedCommand) -> str:
        """Generate script for file export"""
        params = command.parameters
        format = params.get('format', 'STEP')
        filename = params.get('filename', f"export.{format.lower()}")
        
        script = f"""
import FreeCAD
import Part

doc = FreeCAD.ActiveDocument
if not doc:
    print("No active document to export")
    exit()

# Get all objects to export
objects = [obj for obj in doc.Objects if hasattr(obj, 'Shape')]

if not objects:
    print("No objects to export")
    exit()

# Export to {format}
shapes = [obj.Shape for obj in objects]
Part.export(shapes, "{filename}")

print("Exported {len(objects)} objects to {filename}")
"""
        return script.strip()
    
    def _generate_create_sketch(self, command: ParsedCommand) -> str:
        """Generate script for sketch creation"""
        params = command.parameters
        name = params.get('name', 'Sketch')
        
        script = f"""
import FreeCAD
import Sketcher

doc = FreeCAD.ActiveDocument
if not doc:
    doc = FreeCAD.newDocument("SketchDoc")

# Create sketch
sketch = doc.addObject('Sketcher::SketchObject', '{name}')

# Set sketch plane (XY plane)
sketch.Placement = FreeCAD.Placement(
    FreeCAD.Vector(0, 0, 0),
    FreeCAD.Rotation(FreeCAD.Vector(0, 0, 1), 0)
)

# Add geometry based on parameters
# TODO: Add specific sketch geometry based on command parameters

doc.recompute()
print("Created sketch: {name}")
"""
        return script.strip()
    
    def _generate_add_constraint(self, command: ParsedCommand) -> str:
        """Generate script for adding constraints"""
        script = """
import FreeCAD
import Sketcher

# TODO: Implement constraint addition based on command
print("Constraint addition not yet implemented")
"""
        return script.strip()
    
    def _generate_assembly_operation(self, command: ParsedCommand) -> str:
        """Generate script for assembly operations"""
        script = """
import FreeCAD

# TODO: Implement assembly operations
print("Assembly operations not yet implemented")
"""
        return script.strip()
    
    def _generate_generic(self, command: ParsedCommand) -> str:
        """Generate generic script for unknown commands"""
        script = f"""
import FreeCAD

# Command: {command.raw_prompt}
# Intent: {command.intent.value}
# Parameters: {json.dumps(command.parameters, indent=2)}

print("Generic command execution")
print("Intent: {command.intent.value}")
print("This command type is not yet fully implemented")
"""
        return script.strip()
    
    def _validate_script(self, script: str) -> str:
        """
        Validate the generated script for syntax errors
        
        Args:
            script: Python script to validate
            
        Returns:
            Validated script (potentially with fixes)
        """
        try:
            compile(script, '<string>', 'exec')
            logger.debug("Script validation passed")
            return script
        except SyntaxError as e:
            logger.warning(f"Script syntax error: {e}")
            # Attempt basic fixes
            # Add missing imports if needed
            if 'FreeCAD' in script and 'import FreeCAD' not in script:
                script = "import FreeCAD\n" + script
            if 'Part.' in script and 'import Part' not in script:
                script = "import Part\n" + script
            return script
    
    def _optimize_script(self, script: str) -> str:
        """
        Optimize the generated script for performance
        
        Args:
            script: Script to optimize
            
        Returns:
            Optimized script
        """
        # Remove redundant recompute calls
        lines = script.split('\n')
        optimized_lines = []
        recompute_count = 0
        
        for line in lines:
            if 'recompute()' in line:
                recompute_count += 1
                # Keep only the last recompute
                if recompute_count == 1:
                    optimized_lines.append(line)
            else:
                optimized_lines.append(line)
        
        return '\n'.join(optimized_lines)
    
    def batch_generate(self, commands: List[ParsedCommand]) -> str:
        """
        Generate a combined script from multiple commands
        
        Args:
            commands: List of parsed commands
            
        Returns:
            Combined Python script
        """
        scripts = []
        scripts.append("# Batch generated script")
        scripts.append("import FreeCAD")
        scripts.append("import Part")
        scripts.append("")
        
        for i, command in enumerate(commands):
            scripts.append(f"# Command {i+1}: {command.raw_prompt}")
            individual_script = self.generate(command)
            # Remove imports from individual scripts
            lines = [l for l in individual_script.split('\n') 
                    if not l.strip().startswith('import')]
            scripts.extend(lines)
            scripts.append("")
        
        scripts.append("# Final recompute")
        scripts.append("FreeCAD.ActiveDocument.recompute()")
        
        return '\n'.join(scripts)
    
    def save_script(self, script: str, filename: str) -> bool:
        """
        Save generated script to file
        
        Args:
            script: Script content
            filename: Output filename
            
        Returns:
            True if successful
        """
        try:
            path = Path(filename)
            path.write_text(script)
            logger.info(f"Script saved to: {filename}")
            return True
        except Exception as e:
            logger.error(f"Failed to save script: {e}")
            return False
    
    def get_history(self, limit: int = 10) -> List[Dict[str, Any]]:
        """Get script generation history"""
        return self.script_history[-limit:]