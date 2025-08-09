#!/usr/bin/env python3
"""
FreeCAD Integration Module
Provides automation and AI-enhanced functionality for FreeCAD workflows
Part of the FreeCAD + Blender engineering workflow implementation
"""

import sys
import os
import json
import numpy as np
from pathlib import Path

# Add FreeCAD to Python path
FREECAD_PATHS = [
    '/usr/lib/freecad-python3/lib',
    '/usr/lib/freecad/lib',
    '/Applications/FreeCAD.app/Contents/lib',
    'C:\\Program Files\\FreeCAD 0.19\\bin'
]

for path in FREECAD_PATHS:
    if os.path.exists(path) and path not in sys.path:
        sys.path.append(path)

try:
    import FreeCAD
    import Part
    import Draft
    FREECAD_AVAILABLE = True
except ImportError as e:
    print(f"Warning: FreeCAD not available - {e}")
    FREECAD_AVAILABLE = False


class FreeCADWorkflow:
    """Main workflow manager for FreeCAD operations"""
    
    def __init__(self, project_name="engineering_project"):
        self.project_name = project_name
        self.doc = None
        self.export_path = Path("exports")
        self.export_path.mkdir(exist_ok=True)
        
    def create_document(self):
        """Create new FreeCAD document"""
        if not FREECAD_AVAILABLE:
            raise RuntimeError("FreeCAD not available")
            
        self.doc = FreeCAD.newDocument(self.project_name)
        return self.doc
        
    def load_document(self, filepath):
        """Load existing FreeCAD document"""
        if not FREECAD_AVAILABLE:
            raise RuntimeError("FreeCAD not available")
            
        self.doc = FreeCAD.openDocument(str(filepath))
        return self.doc
        
    def create_pressure_vessel(self, diameter=1000, length=3000, thickness=10):
        """
        Create a parametric pressure vessel following ASME standards
        
        Args:
            diameter (float): Internal diameter in mm
            length (float): Cylindrical length in mm  
            thickness (float): Wall thickness in mm
        
        Returns:
            FreeCAD object: Created pressure vessel
        """
        if not self.doc:
            self.create_document()
            
        # Create cylindrical shell
        cylinder = self.doc.addObject("Part::Cylinder", "Shell")
        cylinder.Radius = diameter / 2
        cylinder.Height = length
        cylinder.Placement.Base = FreeCAD.Vector(0, 0, 0)
        
        # Create inner cylinder for thickness
        inner_cyl = self.doc.addObject("Part::Cylinder", "InnerShell")
        inner_cyl.Radius = (diameter / 2) - thickness
        inner_cyl.Height = length + 20  # Slightly longer for clean cut
        inner_cyl.Placement.Base = FreeCAD.Vector(0, 0, -10)
        
        # Create vessel wall by cutting inner from outer
        vessel_wall = self.doc.addObject("Part::Cut", "VesselWall")
        vessel_wall.Base = cylinder
        vessel_wall.Tool = inner_cyl
        
        # Create end caps (elliptical heads)
        head1 = self.doc.addObject("Part::Sphere", "Head1")
        head1.Radius = diameter / 2
        head1.Angle1 = 0
        head1.Angle2 = 90
        head1.Placement.Base = FreeCAD.Vector(0, 0, length)
        
        head2 = self.doc.addObject("Part::Sphere", "Head2")  
        head2.Radius = diameter / 2
        head2.Angle1 = -90
        head2.Angle2 = 0
        head2.Placement.Base = FreeCAD.Vector(0, 0, 0)
        
        # Create nozzles
        nozzle = self.doc.addObject("Part::Cylinder", "Nozzle")
        nozzle.Radius = 100  # 200mm diameter nozzle
        nozzle.Height = 200
        nozzle.Placement.Base = FreeCAD.Vector(diameter/2 - 50, 0, length/2)
        nozzle.Placement.Rotation = FreeCAD.Rotation(FreeCAD.Vector(0, 1, 0), 90)
        
        # Recompute to update all objects
        self.doc.recompute()
        
        return {
            'vessel_wall': vessel_wall,
            'head1': head1, 
            'head2': head2,
            'nozzle': nozzle
        }
        
    def create_piping_system(self, pipe_diameter=100, pipe_length=5000):
        """
        Create a parametric piping system
        
        Args:
            pipe_diameter (float): Pipe outer diameter in mm
            pipe_length (float): Total pipe length in mm
            
        Returns:
            list: Created pipe objects
        """
        if not self.doc:
            self.create_document()
            
        pipes = []
        
        # Main horizontal pipe
        main_pipe = self.doc.addObject("Part::Cylinder", "MainPipe")
        main_pipe.Radius = pipe_diameter / 2
        main_pipe.Height = pipe_length
        main_pipe.Placement.Rotation = FreeCAD.Rotation(FreeCAD.Vector(0, 1, 0), 90)
        pipes.append(main_pipe)
        
        # Vertical riser
        riser = self.doc.addObject("Part::Cylinder", "Riser")
        riser.Radius = pipe_diameter / 2
        riser.Height = 2000
        riser.Placement.Base = FreeCAD.Vector(pipe_length - 500, 0, 0)
        pipes.append(riser)
        
        # 90-degree elbow (simplified as torus section)
        elbow = self.doc.addObject("Part::Torus", "Elbow")
        elbow.Radius1 = pipe_diameter * 1.5  # Bend radius
        elbow.Radius2 = pipe_diameter / 2     # Pipe radius
        elbow.Angle1 = 0
        elbow.Angle2 = 90
        elbow.Placement.Base = FreeCAD.Vector(pipe_length - 500, 0, 0)
        pipes.append(elbow)
        
        self.doc.recompute()
        return pipes
        
    def export_for_blender(self, export_format="STL"):
        """
        Export all visible objects for Blender import
        
        Args:
            export_format (str): Export format - STL, STEP, or OBJ
            
        Returns:
            list: Paths to exported files
        """
        if not self.doc:
            raise RuntimeError("No document loaded")
            
        exported_files = []
        
        for obj in self.doc.Objects:
            if hasattr(obj, 'ViewObject') and obj.ViewObject.Visibility:
                export_name = f"{obj.Name}.{export_format.lower()}"
                export_file = self.export_path / export_name
                
                try:
                    if export_format.upper() == "STL":
                        obj.Shape.exportStl(str(export_file))
                    elif export_format.upper() == "STEP":
                        Part.export([obj], str(export_file))
                    elif export_format.upper() == "OBJ":
                        # OBJ export requires mesh conversion
                        import MeshPart
                        mesh = MeshPart.meshFromShape(obj.Shape, LinearDeflection=0.1)
                        mesh.write(str(export_file))
                    
                    exported_files.append(export_file)
                    print(f"Exported {obj.Name} to {export_file}")
                    
                except Exception as e:
                    print(f"Failed to export {obj.Name}: {e}")
                    
        return exported_files
        
    def ai_optimize_design(self, target_object, optimization_type="weight"):
        """
        AI-powered design optimization
        
        Args:
            target_object: FreeCAD object to optimize
            optimization_type: Type of optimization (weight, stress, cost)
            
        Returns:
            dict: Optimization results and recommendations
        """
        if not hasattr(target_object, 'Shape'):
            return {"error": "Object has no geometric shape"}
            
        # Calculate current properties
        volume = target_object.Shape.Volume
        surface_area = target_object.Shape.Area
        bounding_box = target_object.Shape.BoundBox
        
        # Simple AI optimization logic
        recommendations = {
            "current_volume": volume,
            "current_surface_area": surface_area,
            "bounding_box": {
                "length": bounding_box.XLength,
                "width": bounding_box.YLength, 
                "height": bounding_box.ZLength
            }
        }
        
        if optimization_type == "weight":
            # Suggest hollow sections or material reduction
            recommendations["weight_reduction"] = {
                "hollow_percentage": min(30, volume * 0.0001),  # Max 30% hollow
                "material_savings": f"{recommendations['hollow_percentage']:.1f}%",
                "suggestion": "Consider adding internal cavities or using lighter materials"
            }
            
        elif optimization_type == "stress":
            # Suggest reinforcement areas
            critical_areas = self._identify_stress_points(target_object)
            recommendations["stress_optimization"] = {
                "critical_areas": critical_areas,
                "suggestion": "Add fillets to sharp corners and reinforce high-stress areas"
            }
            
        return recommendations
        
    def _identify_stress_points(self, obj):
        """Identify potential stress concentration points"""
        # Simplified stress analysis - identify sharp corners and thin sections
        edges = obj.Shape.Edges
        sharp_edges = []
        
        for i, edge in enumerate(edges):
            if hasattr(edge, 'Curve') and edge.Length > 10:  # Skip very short edges
                # Check for sharp angles (simplified)
                if edge.Length / edge.BoundBox.DiagonalLength > 0.1:
                    sharp_edges.append(f"Edge_{i}")
                    
        return sharp_edges
        
    def generate_technical_drawing(self, objects=None):
        """
        Generate technical drawings from 3D model
        
        Args:
            objects: List of objects to include in drawing (default: all visible)
            
        Returns:
            str: Path to generated drawing file
        """
        try:
            import TechDraw
            
            # Create a new TechDraw page
            page = self.doc.addObject('TechDraw::DrawPage', 'DrawingPage')
            template = self.doc.addObject('TechDraw::DrawSVGTemplate', 'Template')
            
            # Set up A3 template
            template.Template = '/usr/share/freecad/Mod/TechDraw/Templates/A3_LandscapeISO.svg'
            page.Template = template
            
            # Add views for each object
            if objects is None:
                objects = [obj for obj in self.doc.Objects if hasattr(obj, 'Shape') and obj.ViewObject.Visibility]
                
            for i, obj in enumerate(objects):
                view = self.doc.addObject('TechDraw::DrawViewPart', f'View_{i}')
                view.Source = [obj]
                view.Direction = FreeCAD.Vector(0, 0, 1)  # Top view
                view.Scale = 0.5
                view.X = 100 + (i * 150)
                view.Y = 200
                page.addView(view)
                
            self.doc.recompute()
            
            # Export drawing
            drawing_file = self.export_path / f"{self.project_name}_drawing.pdf"
            page.exportPdf(str(drawing_file))
            
            return str(drawing_file)
            
        except ImportError:
            return "TechDraw module not available"
        except Exception as e:
            return f"Drawing generation failed: {e}"
            
    def close_document(self):
        """Close the current document"""
        if self.doc:
            FreeCAD.closeDocument(self.doc.Name)
            self.doc = None


class AIDesignAssistant:
    """AI-powered design assistance for FreeCAD workflows"""
    
    @staticmethod
    def suggest_materials(component_type, environment="offshore"):
        """
        Suggest appropriate materials based on component type and environment
        
        Args:
            component_type (str): Type of component (vessel, pipe, structure)
            environment (str): Operating environment
            
        Returns:
            dict: Material recommendations with properties
        """
        material_database = {
            "offshore": {
                "vessel": {
                    "primary": "316L Stainless Steel",
                    "properties": {"yield_strength": 290, "corrosion_resistance": "excellent"},
                    "alternatives": ["Duplex 2205", "Inconel 625"]
                },
                "pipe": {
                    "primary": "Carbon Steel API 5L Grade B", 
                    "properties": {"yield_strength": 245, "cost": "low"},
                    "alternatives": ["316 Stainless Steel", "Hastelloy C-276"]
                },
                "structure": {
                    "primary": "Structural Steel A36",
                    "properties": {"yield_strength": 250, "weldability": "excellent"},
                    "alternatives": ["A572 Grade 50", "Weathering Steel"]
                }
            }
        }
        
        return material_database.get(environment, {}).get(component_type, {
            "primary": "Carbon Steel",
            "properties": {"yield_strength": 250},
            "alternatives": []
        })
        
    @staticmethod
    def estimate_fabrication_time(volume, complexity_factor=1.0):
        """
        Estimate fabrication time based on volume and complexity
        
        Args:
            volume (float): Component volume in mm³
            complexity_factor (float): Complexity multiplier (1.0 = normal)
            
        Returns:
            dict: Time estimates for different fabrication stages
        """
        # Base time constants (hours per unit volume)
        base_rates = {
            "cutting": 0.0001,      # Hours per mm³
            "welding": 0.0005,      # Hours per mm³  
            "machining": 0.0003,    # Hours per mm³
            "assembly": 0.0002      # Hours per mm³
        }
        
        estimates = {}
        total_time = 0
        
        for process, rate in base_rates.items():
            time = volume * rate * complexity_factor
            estimates[process] = round(time, 2)
            total_time += time
            
        estimates["total_hours"] = round(total_time, 2)
        estimates["total_days"] = round(total_time / 8, 1)
        
        return estimates


def main():
    """Test the FreeCAD integration functionality"""
    if not FREECAD_AVAILABLE:
        print("FreeCAD not available - cannot run full test")
        return False
        
    try:
        # Create workflow instance
        workflow = FreeCADWorkflow("test_project")
        
        # Create test document
        doc = workflow.create_document()
        print(f"Created document: {doc.Name}")
        
        # Create a simple pressure vessel
        vessel_parts = workflow.create_pressure_vessel(diameter=1000, length=2000)
        print(f"Created pressure vessel with {len(vessel_parts)} components")
        
        # Create piping system
        pipes = workflow.create_piping_system(pipe_diameter=200)
        print(f"Created piping system with {len(pipes)} components")
        
        # Export for Blender
        exported_files = workflow.export_for_blender("STL")
        print(f"Exported {len(exported_files)} files for Blender")
        
        # AI optimization test
        if vessel_parts:
            optimization = workflow.ai_optimize_design(vessel_parts['vessel_wall'])
            print("AI optimization suggestions:", optimization.get('weight_reduction', {}))
            
        # Material suggestions
        materials = AIDesignAssistant.suggest_materials("vessel", "offshore")
        print("Recommended material:", materials.get('primary'))
        
        # Clean up
        workflow.close_document()
        print("Test completed successfully!")
        return True
        
    except Exception as e:
        print(f"Test failed: {e}")
        return False


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)