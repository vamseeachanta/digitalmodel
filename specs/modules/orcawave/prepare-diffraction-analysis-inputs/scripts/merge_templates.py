#!/usr/bin/env python3
"""
Merge vessel data with OrcaWave templates.
Combines hydrodynamic properties with go-by templates to create complete configurations.
"""

import yaml
import json
from pathlib import Path
from typing import Dict, Any, List
import logging
from datetime import datetime
import sys

# Add parent directory to path for imports
sys.path.append(str(Path(__file__).parent))

from load_templates import TemplateLoader
from variable_substitution import SubstitutionEngine
from orcawave_yaml_parser import OrcaWaveYamlParser

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


class TemplateMerger:
    """Merges vessel data with OrcaWave templates."""
    
    def __init__(self, base_dir: Path = None):
        """Initialize template merger.
        
        Args:
            base_dir: Base directory for inputs/outputs
        """
        if base_dir is None:
            base_dir = Path(__file__).parent.parent
        self.base_dir = Path(base_dir)
        self.outputs_dir = self.base_dir / "outputs"
        self.template_loader = TemplateLoader(self.outputs_dir / "orcawave_configs" / "go-by")
        self.substitution_engine = SubstitutionEngine()
        self.orcawave_parser = OrcaWaveYamlParser()
        
    def load_hydrodynamic_data(self) -> Dict[str, Any]:
        """Load hydrodynamic properties from YAML.
        
        Returns:
            Dictionary of hydrodynamic properties
        """
        hydro_file = self.outputs_dir / "hydrodynamic.yml"
        
        if not hydro_file.exists():
            raise FileNotFoundError(f"Hydrodynamic data not found: {hydro_file}")
            
        logger.info(f"Loading hydrodynamic data from: {hydro_file}")
        
        with open(hydro_file, 'r') as f:
            data = yaml.safe_load(f)
            
        return data
        
    def prepare_variables(self, config_name: str, config_data: Dict[str, Any], 
                         vessel_data: Dict[str, Any]) -> Dict[str, Any]:
        """Prepare variables for template substitution.
        
        Args:
            config_name: Name of vessel configuration
            config_data: Configuration-specific data
            vessel_data: General vessel data
            
        Returns:
            Dictionary of variables for substitution
        """
        # Get vessel dimensions
        dims = vessel_data.get('dimensions', {})
        
        # Prepare variables
        variables = {
            'config_name': config_name,
            'config_description': config_data.get('description', config_name),
            'vessel_name': vessel_data.get('name', 'Vessel'),
            'vessel_type': vessel_data.get('type', 'Supply Vessel'),
            
            # Mass properties
            'mass': config_data.get('mass', 0),
            'cog': config_data.get('cog', [0, 0, 0]),
            'cog_x': config_data['cog'][0] if 'cog' in config_data else 0,
            'cog_y': config_data['cog'][1] if 'cog' in config_data else 0,
            'cog_z': config_data['cog'][2] if 'cog' in config_data else 0,
            
            # Inertia properties
            'inertia_matrix': config_data.get('inertia_matrix', [[0]*3]*3),
            'ixx': config_data['inertia_matrix'][0][0] if 'inertia_matrix' in config_data else 0,
            'iyy': config_data['inertia_matrix'][1][1] if 'inertia_matrix' in config_data else 0,
            'izz': config_data['inertia_matrix'][2][2] if 'inertia_matrix' in config_data else 0,
            
            # Gyradii
            'gyradii': config_data.get('gyradii', [0, 0, 0]),
            'kxx': config_data['gyradii'][0] if 'gyradii' in config_data else 0,
            'kyy': config_data['gyradii'][1] if 'gyradii' in config_data else 0,
            'kzz': config_data['gyradii'][2] if 'gyradii' in config_data else 0,
            
            # Vessel dimensions
            'draft': config_data.get('draft', 0),
            'lbp': dims.get('LBP', {}).get('value', 0),
            'beam': dims.get('beam', {}).get('value', 0),
            'depth': dims.get('depth', {}).get('value', 0),
            
            # File paths (relative paths for portability)
            'mesh_file': '../../../data/Sea Cypress_0.25 Mesh_Ascii.msh',
            
            # Analysis parameters
            'water_depth': 500,  # Default water depth
            'water_density': 1.025,
            
            # Metadata
            'created_date': datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            'created_by': 'OrcaWave Template Merger',
        }
        
        return variables
        
    def merge_with_template(self, template_name: str, variables: Dict[str, Any]) -> Dict[str, Any]:
        """Merge variables with template.
        
        Args:
            template_name: Name of template to use
            variables: Variables for substitution
            
        Returns:
            Merged configuration
        """
        # Load template
        template = self.template_loader.load_template(template_name)
        
        # Set variables in substitution engine
        self.substitution_engine.set_variables(variables)
        
        # Perform substitution
        merged = self.substitution_engine.substitute_template(template)
        
        # Update specific fields based on variables
        if 'Bodies' in merged and len(merged['Bodies']) > 0:
            body = merged['Bodies'][0]
            
            # Update mass properties
            # OrcaWave expects mass in tonnes (Te), not kg
            # Round to 4 decimals for clean input
            mass_tonnes = round(variables['mass'] / 1000.0, 4)
            body['BodyMass'] = mass_tonnes
            body['BodyCentreOfMass'] = [
                round(variables['cog_x'], 4),
                round(variables['cog_y'], 4),
                round(variables['cog_z'], 4)
            ]
            
            # Update inertia tensor
            # OrcaWave expects inertia in Te.m^2, not kg.m^2
            # Round to 4 decimals for clean input
            ixx_tonnes = round(variables['ixx'] / 1000.0, 4)
            iyy_tonnes = round(variables['iyy'] / 1000.0, 4)
            izz_tonnes = round(variables['izz'] / 1000.0, 4)
            body['BodyInertiaTensorRx, BodyInertiaTensorRy, BodyInertiaTensorRz'] = [
                [ixx_tonnes, 0, 0],
                [0, iyy_tonnes, 0],
                [0, 0, izz_tonnes]]
            
            logger.info(f"Converted mass: {variables['mass']} kg → {mass_tonnes} Te")
            logger.info(f"Converted inertia: Ixx={ixx_tonnes} Te.m^2, Iyy={iyy_tonnes} Te.m^2, Izz={izz_tonnes} Te.m^2")
            
            # Update body name
            body['BodyName'] = f"{variables['vessel_name']}_{variables['config_name']}"
            
            # Update mesh file path
            body['BodyMeshFileName'] = variables['mesh_file']
            
            # CRITICAL: Update mesh position Z to reflect actual draft
            # The mesh Z position needs to be adjusted so the vessel sits at the correct draft
            # Typically, Z position = -draft (negative because Z is positive upward in OrcaWave)
            # This positions the mesh so the waterline is at Z=0
            if 'BodyMeshPosition' in body:
                # Keep X and Y, but set Z based on draft (rounded to 4 decimals)
                body['BodyMeshPosition'][2] = round(-variables['draft'], 4)
                logger.info(f"Set mesh position Z to {round(-variables['draft'], 4)} for draft {variables['draft']}m")
            
        # Ensure FreeSurfacePanelledZoneMeshFileName is empty string
        if 'FreeSurfacePanelledZoneMeshFileName' in merged:
            merged['FreeSurfacePanelledZoneMeshFileName'] = ''
            
        return merged
        
    def create_all_configurations(self, template_name: str = 'go-by-template_rev2.yml') -> Dict[str, Dict[str, Any]]:
        """Create OrcaWave configurations for all vessel configurations.
        
        Args:
            template_name: Name of template to use
            
        Returns:
            Dictionary of all configurations
        """
        # Load hydrodynamic data
        hydro_data = self.load_hydrodynamic_data()
        vessel_data = hydro_data.get('vessel', {})
        configurations = vessel_data.get('configurations', {})
        
        if not configurations:
            logger.error("No vessel configurations found in hydrodynamic data")
            return {}
            
        results = {}
        
        for config_name, config_data in configurations.items():
            logger.info(f"Processing configuration: {config_name}")
            
            # Prepare variables
            variables = self.prepare_variables(config_name, config_data, vessel_data)
            
            # Merge with template
            merged_config = self.merge_with_template(template_name, variables)
            
            # Add metadata
            merged_config['# Configuration'] = config_name
            merged_config['# Description'] = config_data.get('description', '')
            
            results[config_name] = merged_config
            
        return results
        
    def save_configurations(self, configurations: Dict[str, Dict[str, Any]], 
                           output_dir: Path = None) -> List[Path]:
        """Save configurations to YAML files using OrcaWave-specific formatting.
        
        Args:
            configurations: Dictionary of configurations
            output_dir: Output directory (optional)
            
        Returns:
            List of saved file paths
        """
        if output_dir is None:
            output_dir = self.outputs_dir / "orcawave_configs" / "merged"
            
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        
        saved_files = []
        
        for config_name, config_data in configurations.items():
            output_file = output_dir / f"orcawave_{config_name}.yml"
            
            logger.info(f"Saving configuration to: {output_file}")
            
            # Remove metadata comments from data
            clean_data = {k: v for k, v in config_data.items() 
                        if not k.startswith('#')}
            
            # Use custom OrcaWave YAML parser
            self.orcawave_parser.write_orcawave_yaml(clean_data, output_file, config_name)
            
            saved_files.append(output_file)
            
        logger.info(f"Saved {len(saved_files)} configuration files with OrcaWave formatting")
        return saved_files


def main():
    """Test template merger functionality."""
    merger = TemplateMerger()
    
    try:
        # Create all configurations
        configurations = merger.create_all_configurations()
        
        if configurations:
            print(f"Created {len(configurations)} configurations:")
            for name in configurations:
                print(f"  - {name}")
                
            # Save configurations
            saved_files = merger.save_configurations(configurations)
            
            print(f"\nSaved {len(saved_files)} files:")
            for file_path in saved_files:
                print(f"  - {file_path.name}")
                
            # Display sample of first configuration
            first_config = list(configurations.keys())[0]
            print(f"\nSample from {first_config}:")
            sample = configurations[first_config]
            if 'Bodies' in sample and sample['Bodies']:
                body = sample['Bodies'][0]
                print(f"  Body Name: {body.get('BodyName', 'N/A')}")
                print(f"  Mass: {body.get('BodyMass', 'N/A')} kg")
                print(f"  CoG: {body.get('BodyCentreOfMass', 'N/A')}")
                print(f"  Mesh File: {body.get('BodyMeshFileName', 'N/A')}")
                
        else:
            print("No configurations created")
            
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()