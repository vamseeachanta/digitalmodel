#!/usr/bin/env python3
"""
Integrated FreeCAD + Blender Workflow
Complete automation pipeline for engineering design and visualization
Part of the FreeCAD + Blender engineering workflow implementation
"""

import sys
import os
import json
import time
from pathlib import Path
from typing import Dict, List, Optional, Union

# Import custom modules
try:
    from freecad_integration import FreeCADWorkflow, AIDesignAssistant, FREECAD_AVAILABLE
    from blender_integration import BlenderWorkflow, AIVisualizationAssistant, BLENDER_EXECUTABLE
except ImportError as e:
    print(f"Warning: Could not import integration modules - {e}")
    FREECAD_AVAILABLE = False
    BLENDER_EXECUTABLE = None


class IntegratedWorkflow:
    """
    Complete FreeCAD + Blender engineering workflow
    Handles design, optimization, visualization, and documentation
    """
    
    def __init__(self, project_name: str, project_path: Optional[str] = None):
        self.project_name = project_name
        self.project_path = Path(project_path) if project_path else Path(f"projects/{project_name}")
        self.project_path.mkdir(parents=True, exist_ok=True)
        
        # Initialize workflow components
        self.freecad_workflow = None
        self.blender_workflow = None
        
        # Project structure
        self.structure = {
            'cad_files': self.project_path / 'cad',
            'exports': self.project_path / 'exports', 
            'renders': self.project_path / 'renders',
            'documentation': self.project_path / 'docs',
            'analysis': self.project_path / 'analysis'
        }
        
        # Create project structure
        for path in self.structure.values():
            path.mkdir(exist_ok=True)
            
        # Project metadata
        self.metadata_file = self.project_path / 'project_metadata.json'
        self.load_or_create_metadata()
        
    def load_or_create_metadata(self):
        """Load existing project metadata or create new"""
        if self.metadata_file.exists():
            with open(self.metadata_file, 'r') as f:
                self.metadata = json.load(f)
        else:
            self.metadata = {
                'project_name': self.project_name,
                'created_date': time.strftime('%Y-%m-%d %H:%M:%S'),
                'version': '1.0',
                'components': [],
                'workflow_history': [],
                'ai_optimizations': []
            }
            self.save_metadata()
            
    def save_metadata(self):
        """Save project metadata to file"""
        with open(self.metadata_file, 'w') as f:
            json.dump(self.metadata, f, indent=2)
            
    def log_workflow_step(self, step: str, status: str, details: Optional[Dict] = None):
        """Log workflow step to project history"""
        entry = {
            'timestamp': time.strftime('%Y-%m-%d %H:%M:%S'),
            'step': step,
            'status': status,
            'details': details or {}
        }
        self.metadata['workflow_history'].append(entry)
        self.save_metadata()
        
    def initialize_freecad(self) -> bool:
        """Initialize FreeCAD workflow component"""
        if not FREECAD_AVAILABLE:
            self.log_workflow_step('freecad_init', 'failed', {'error': 'FreeCAD not available'})
            return False
            
        try:
            # Set up FreeCAD workflow with project-specific paths
            os.chdir(self.structure['cad_files'])
            self.freecad_workflow = FreeCADWorkflow(self.project_name)
            self.freecad_workflow.export_path = self.structure['exports']
            
            self.log_workflow_step('freecad_init', 'success')
            return True
            
        except Exception as e:
            self.log_workflow_step('freecad_init', 'failed', {'error': str(e)})
            return False
            
    def initialize_blender(self) -> bool:
        """Initialize Blender workflow component"""
        if not BLENDER_EXECUTABLE:
            self.log_workflow_step('blender_init', 'failed', {'error': 'Blender not available'})
            return False
            
        try:
            # Set up Blender workflow with project-specific paths
            os.chdir(self.structure['renders'])
            self.blender_workflow = BlenderWorkflow(self.project_name)
            self.blender_workflow.import_path = self.structure['exports']
            self.blender_workflow.render_path = self.structure['renders']
            self.blender_workflow.project_path = self.structure['cad_files'] / f"{self.project_name}.blend"
            
            self.log_workflow_step('blender_init', 'success')
            return True
            
        except Exception as e:
            self.log_workflow_step('blender_init', 'failed', {'error': str(e)})
            return False
            
    def design_pressure_vessel(self, specifications: Dict) -> bool:
        """
        Complete pressure vessel design workflow
        
        Args:
            specifications (dict): Vessel specifications
                - diameter: float (mm)
                - length: float (mm) 
                - thickness: float (mm)
                - pressure: float (bar)
                - temperature: float (°C)
                - material: str
                
        Returns:
            bool: Success status
        """
        if not self.initialize_freecad():
            return False
            
        try:
            # Create parametric vessel design
            vessel_parts = self.freecad_workflow.create_pressure_vessel(
                diameter=specifications.get('diameter', 1000),
                length=specifications.get('length', 3000),
                thickness=specifications.get('thickness', 10)
            )
            
            # AI optimization
            if vessel_parts and 'vessel_wall' in vessel_parts:
                optimization = self.freecad_workflow.ai_optimize_design(
                    vessel_parts['vessel_wall'], 
                    'stress'
                )
                self.metadata['ai_optimizations'].append({
                    'component': 'pressure_vessel',
                    'optimization': optimization,
                    'timestamp': time.strftime('%Y-%m-%d %H:%M:%S')
                })
                
            # Material selection AI
            material_rec = AIDesignAssistant.suggest_materials('vessel', 'offshore')
            
            # Fabrication time estimation
            if vessel_parts and 'vessel_wall' in vessel_parts:
                fab_time = AIDesignAssistant.estimate_fabrication_time(
                    vessel_parts['vessel_wall'].Shape.Volume, 
                    complexity_factor=1.2
                )
                
            # Export for visualization
            exported_files = self.freecad_workflow.export_for_blender('STL')
            
            # Update project metadata
            self.metadata['components'].append({
                'name': 'pressure_vessel',
                'type': 'vessel',
                'specifications': specifications,
                'material_recommendation': material_rec,
                'fabrication_estimate': fab_time if 'fab_time' in locals() else None,
                'exported_files': [str(f) for f in exported_files]
            })
            
            self.log_workflow_step('design_pressure_vessel', 'success', {
                'parts_created': len(vessel_parts),
                'files_exported': len(exported_files),
                'material': material_rec.get('primary'),
                'optimization_applied': True
            })
            
            return True
            
        except Exception as e:
            self.log_workflow_step('design_pressure_vessel', 'failed', {'error': str(e)})
            return False
            
    def design_piping_system(self, specifications: Dict) -> bool:
        """
        Complete piping system design workflow
        
        Args:
            specifications (dict): Piping specifications
                - diameter: float (mm)
                - length: float (mm)
                - pressure: float (bar)
                - fluid: str
                - layout: list of coordinates
                
        Returns:
            bool: Success status
        """
        if not self.initialize_freecad():
            return False
            
        try:
            # Create parametric piping system
            pipes = self.freecad_workflow.create_piping_system(
                pipe_diameter=specifications.get('diameter', 100),
                pipe_length=specifications.get('length', 5000)
            )
            
            # Material selection for piping
            material_rec = AIDesignAssistant.suggest_materials('pipe', 'offshore')
            
            # Export for visualization
            exported_files = self.freecad_workflow.export_for_blender('STL')
            
            # Update project metadata
            self.metadata['components'].append({
                'name': 'piping_system',
                'type': 'piping',
                'specifications': specifications,
                'material_recommendation': material_rec,
                'pipe_count': len(pipes),
                'exported_files': [str(f) for f in exported_files]
            })
            
            self.log_workflow_step('design_piping_system', 'success', {
                'pipes_created': len(pipes),
                'files_exported': len(exported_files),
                'material': material_rec.get('primary')
            })
            
            return True
            
        except Exception as e:
            self.log_workflow_step('design_piping_system', 'failed', {'error': str(e)})
            return False
            
    def create_visualization(self, render_types: List[str] = None) -> Dict[str, List[str]]:
        """
        Create engineering visualizations in Blender
        
        Args:
            render_types (list): Types of renders to create
                - 'technical': Technical documentation views
                - 'presentation': High-quality presentation renders
                - 'animation': Assembly animation
                - 'vr': VR/AR export
                
        Returns:
            dict: Rendered files organized by type
        """
        if not self.initialize_blender():
            return {}
            
        if render_types is None:
            render_types = ['technical', 'presentation']
            
        results = {}
        
        try:
            # Create engineering scene with imported CAD models
            import_files = list(self.structure['exports'].glob("*.stl"))
            scene_result = self.blender_workflow.create_engineering_scene(import_files)
            
            if scene_result[0] != 0:
                raise RuntimeError(f"Scene creation failed: {scene_result[2]}")
                
            # Technical views
            if 'technical' in render_types:
                technical_renders = self.blender_workflow.render_technical_views(['PNG'])
                results['technical'] = [str(f) for f in technical_renders]
                
            # Presentation renders
            if 'presentation' in render_types:
                presentation_renders = self.blender_workflow.render_technical_views(['JPEG'])
                results['presentation'] = [str(f) for f in presentation_renders]
                
            # Assembly animation
            if 'animation' in render_types:
                animation_file = self.blender_workflow.create_assembly_animation()
                if animation_file:
                    results['animation'] = [animation_file]
                    
            # VR export
            if 'vr' in render_types:
                vr_file = self.blender_workflow.export_for_vr('GLTF')
                if vr_file:
                    results['vr'] = [vr_file]
                    
            self.log_workflow_step('create_visualization', 'success', {
                'render_types': render_types,
                'files_created': sum(len(files) for files in results.values()),
                'import_files': len(import_files)
            })
            
            return results
            
        except Exception as e:
            self.log_workflow_step('create_visualization', 'failed', {'error': str(e)})
            return {}
            
    def generate_documentation(self) -> str:
        """
        Generate comprehensive project documentation
        
        Returns:
            str: Path to generated documentation
        """
        try:
            doc_file = self.structure['documentation'] / f"{self.project_name}_report.md"
            
            with open(doc_file, 'w') as f:
                # Header
                f.write(f"# {self.project_name} Engineering Report\n\n")
                f.write(f"Generated: {time.strftime('%Y-%m-%d %H:%M:%S')}\n\n")
                
                # Project overview
                f.write("## Project Overview\n\n")
                f.write(f"**Project Name:** {self.metadata['project_name']}\n")
                f.write(f"**Created:** {self.metadata['created_date']}\n") 
                f.write(f"**Version:** {self.metadata['version']}\n\n")
                
                # Components
                f.write("## Components Designed\n\n")
                for component in self.metadata['components']:
                    f.write(f"### {component['name'].title()}\n")
                    f.write(f"**Type:** {component['type']}\n")
                    
                    if 'specifications' in component:
                        f.write("**Specifications:**\n")
                        for key, value in component['specifications'].items():
                            f.write(f"- {key}: {value}\n")
                            
                    if 'material_recommendation' in component:
                        mat = component['material_recommendation']
                        if isinstance(mat, dict) and 'primary' in mat:
                            f.write(f"**Recommended Material:** {mat['primary']}\n")
                            
                    if 'fabrication_estimate' in component and component['fabrication_estimate']:
                        fab = component['fabrication_estimate']
                        if isinstance(fab, dict):
                            f.write(f"**Fabrication Time:** {fab.get('total_days', 'N/A')} days\n")
                    f.write("\n")
                    
                # AI optimizations
                if self.metadata['ai_optimizations']:
                    f.write("## AI Optimization Results\n\n")
                    for opt in self.metadata['ai_optimizations']:
                        f.write(f"### {opt['component'].title()}\n")
                        f.write(f"**Timestamp:** {opt['timestamp']}\n")
                        if 'weight_reduction' in opt['optimization']:
                            wr = opt['optimization']['weight_reduction']
                            f.write(f"**Weight Reduction Potential:** {wr.get('material_savings', 'N/A')}\n")
                        f.write("\n")
                        
                # Workflow history
                f.write("## Workflow History\n\n")
                for step in self.metadata['workflow_history']:
                    status_icon = "✅" if step['status'] == 'success' else "❌"
                    f.write(f"{status_icon} **{step['step']}** - {step['timestamp']}\n")
                    if step['status'] == 'failed' and 'error' in step.get('details', {}):
                        f.write(f"   Error: {step['details']['error']}\n")
                    f.write("\n")
                    
                # File locations
                f.write("## Project Files\n\n")
                f.write(f"**Project Path:** `{self.project_path}`\n")
                f.write(f"**CAD Files:** `{self.structure['cad_files']}`\n")
                f.write(f"**Exports:** `{self.structure['exports']}`\n")
                f.write(f"**Renders:** `{self.structure['renders']}`\n")
                f.write(f"**Analysis:** `{self.structure['analysis']}`\n\n")
                
            self.log_workflow_step('generate_documentation', 'success', {
                'doc_file': str(doc_file)
            })
            
            return str(doc_file)
            
        except Exception as e:
            self.log_workflow_step('generate_documentation', 'failed', {'error': str(e)})
            return ""
            
    def get_project_status(self) -> Dict:
        """
        Get comprehensive project status
        
        Returns:
            dict: Project status information
        """
        return {
            'project_name': self.project_name,
            'project_path': str(self.project_path),
            'components_designed': len(self.metadata['components']),
            'workflow_steps': len(self.metadata['workflow_history']),
            'ai_optimizations': len(self.metadata['ai_optimizations']),
            'last_activity': self.metadata['workflow_history'][-1] if self.metadata['workflow_history'] else None,
            'freecad_available': FREECAD_AVAILABLE,
            'blender_available': BLENDER_EXECUTABLE is not None,
            'files': {
                'cad_files': len(list(self.structure['cad_files'].glob("*"))),
                'exports': len(list(self.structure['exports'].glob("*"))),
                'renders': len(list(self.structure['renders'].glob("*"))),
                'docs': len(list(self.structure['documentation'].glob("*")))
            }
        }


def create_sample_project():
    """Create a sample engineering project to demonstrate the workflow"""
    # Initialize project
    project = IntegratedWorkflow("sample_offshore_platform")
    
    print(f"🚀 Starting sample project: {project.project_name}")
    print(f"📁 Project location: {project.project_path}")
    
    # Pressure vessel specifications
    vessel_specs = {
        'diameter': 2000,  # 2m diameter
        'length': 6000,    # 6m length
        'thickness': 15,   # 15mm wall thickness
        'pressure': 10,    # 10 bar operating pressure
        'temperature': 60, # 60°C operating temperature
        'material': 'Carbon Steel'
    }
    
    # Design pressure vessel
    print("\n⚙️ Designing pressure vessel...")
    vessel_success = project.design_pressure_vessel(vessel_specs)
    print(f"   {'✅ Success' if vessel_success else '❌ Failed'}")
    
    # Piping system specifications  
    piping_specs = {
        'diameter': 150,   # 150mm diameter
        'length': 10000,   # 10m total length
        'pressure': 15,    # 15 bar
        'fluid': 'Natural Gas',
        'layout': [(0, 0, 0), (5000, 0, 0), (5000, 2000, 0)]
    }
    
    # Design piping system
    print("\n🔧 Designing piping system...")
    piping_success = project.design_piping_system(piping_specs)
    print(f"   {'✅ Success' if piping_success else '❌ Failed'}")
    
    # Create visualizations
    print("\n🎨 Creating visualizations...")
    renders = project.create_visualization(['technical', 'presentation'])
    print(f"   Created {sum(len(files) for files in renders.values())} visualization files")
    
    # Generate documentation
    print("\n📄 Generating documentation...")
    doc_file = project.generate_documentation()
    print(f"   {'✅ Documentation created' if doc_file else '❌ Documentation failed'}")
    
    # Final status
    status = project.get_project_status()
    print(f"\n📊 Project Status:")
    print(f"   Components: {status['components_designed']}")
    print(f"   Workflow steps: {status['workflow_steps']}")
    print(f"   AI optimizations: {status['ai_optimizations']}")
    print(f"   Files created: {sum(status['files'].values())}")
    
    return project


def main():
    """Main function to test the integrated workflow"""
    print("🔧 FreeCAD + Blender Integrated Workflow Test")
    print("=" * 50)
    
    # Check system capabilities
    print(f"FreeCAD Available: {'✅' if FREECAD_AVAILABLE else '❌'}")
    print(f"Blender Available: {'✅' if BLENDER_EXECUTABLE else '❌'}")
    
    if not FREECAD_AVAILABLE and not BLENDER_EXECUTABLE:
        print("\n❌ Neither FreeCAD nor Blender available - cannot run full workflow")
        return False
        
    # Run sample project
    try:
        project = create_sample_project()
        
        if project:
            print(f"\n🎉 Sample project completed successfully!")
            print(f"📁 Check results in: {project.project_path}")
            return True
        else:
            print("\n❌ Sample project failed")
            return False
            
    except Exception as e:
        print(f"\n❌ Workflow test failed: {e}")
        return False


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)