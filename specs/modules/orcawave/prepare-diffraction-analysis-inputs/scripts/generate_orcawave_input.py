"""
Generate OrcaWave input files from templates and extracted data.
"""

import yaml
import json
from pathlib import Path
import logging
import argparse
from typing import Dict, Any, List
import re
from datetime import datetime

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class OrcaWaveInputGenerator:
    """Generate OrcaWave input files from templates."""
    
    def __init__(self, template_path: Path):
        """Initialize with template file path."""
        self.template_path = Path(template_path)
        self.template = None
        self.variables = {}
        
    def load_template(self) -> bool:
        """Load OrcaWave template file."""
        try:
            with open(self.template_path, 'r') as f:
                self.template = yaml.safe_load(f)
            logger.info(f"Loaded template: {self.template_path}")
            return True
        except Exception as e:
            logger.error(f"Error loading template: {e}")
            return False
            
    def set_variables(self, variables: Dict[str, Any]):
        """Set variables for template substitution."""
        self.variables = variables
        logger.info(f"Set {len(variables)} template variables")
        
    def update_geometry(self, gdf_path: Path, position: List[float] = None):
        """Update geometry file path and position."""
        if 'Bodies' in self.template and len(self.template['Bodies']) > 0:
            body = self.template['Bodies'][0]
            
            # Update mesh file path (relative to OrcaWave file)
            rel_path = Path(gdf_path).relative_to(self.template_path.parent.parent)
            body['BodyMeshFileName'] = str(rel_path).replace('\\', '/')
            
            # Update position if provided
            if position:
                body['BodyMeshPosition'] = position
                
            logger.info(f"Updated geometry: {rel_path}")
            
    def update_vessel_properties(self, properties: Dict[str, Any]):
        """Update vessel properties in template."""
        if 'Bodies' in self.template and len(self.template['Bodies']) > 0:
            body = self.template['Bodies'][0]
            
            # Map properties to body parameters
            mappings = {
                'mass': 'BodyMass',
                'centre_of_mass': 'BodyCentreOfMass',
                'ixx': 'BodyInertiaTensorRx',
                'iyy': 'BodyInertiaTensorRy',
                'izz': 'BodyInertiaTensorRz',
                'length': 'BodyOrcaFlexImportLength'
            }
            
            for prop_key, body_key in mappings.items():
                if prop_key in properties:
                    value = properties[prop_key]
                    
                    # Handle inertia tensor format
                    if prop_key in ['ixx', 'iyy', 'izz']:
                        # Create diagonal matrix
                        if prop_key == 'ixx':
                            body[body_key] = [value, 0, 0]
                        elif prop_key == 'iyy':
                            body[body_key] = [0, value, 0]
                        else:  # izz
                            body[body_key] = [0, 0, value]
                    else:
                        body[body_key] = value
                        
            logger.info(f"Updated {len(properties)} vessel properties")
            
    def update_environment(self, water_depth: float = None, water_density: float = None):
        """Update environmental parameters."""
        if water_depth is not None:
            self.template['WaterDepth'] = water_depth
            logger.info(f"Set water depth: {water_depth} m")
            
        if water_density is not None:
            self.template['WaterDensity'] = water_density
            logger.info(f"Set water density: {water_density} t/m³")
            
    def set_wave_conditions(self, periods: List[float] = None, headings: List[float] = None):
        """Set wave periods and headings for analysis."""
        if periods:
            self.template['PeriodOrFrequency'] = sorted(periods)
            logger.info(f"Set {len(periods)} wave periods")
            
        if headings:
            self.template['WaveHeading'] = sorted(headings)
            logger.info(f"Set {len(headings)} wave headings")
            
    def set_qtf_parameters(self, calculate_qtf: bool = True, qtf_range: Dict[str, float] = None):
        """Configure QTF calculation parameters."""
        if calculate_qtf:
            self.template['SolveType'] = 'Full QTF calculation'
            
            if qtf_range:
                if 'min_period' in qtf_range:
                    self.template['QTFMinPeriodOrFrequency'] = qtf_range['min_period']
                if 'max_period' in qtf_range:
                    self.template['QTFMaxPeriodOrFrequency'] = qtf_range['max_period']
                if 'min_angle' in qtf_range:
                    self.template['QTFMinCrossingAngle'] = qtf_range['min_angle']
                if 'max_angle' in qtf_range:
                    self.template['QTFMaxCrossingAngle'] = qtf_range['max_angle']
                    
            logger.info("Enabled QTF calculation")
        else:
            self.template['SolveType'] = 'Radiation and diffraction'
            logger.info("Disabled QTF calculation")
            
    def substitute_variables(self):
        """Substitute template variables with actual values."""
        def replace_vars(obj):
            if isinstance(obj, str):
                # Replace ${VAR_NAME} with value from variables
                pattern = r'\$\{(\w+)\}'
                def replacer(match):
                    var_name = match.group(1)
                    return str(self.variables.get(var_name, match.group(0)))
                return re.sub(pattern, replacer, obj)
            elif isinstance(obj, dict):
                return {k: replace_vars(v) for k, v in obj.items()}
            elif isinstance(obj, list):
                return [replace_vars(item) for item in obj]
            else:
                return obj
                
        self.template = replace_vars(self.template)
        logger.info("Completed variable substitution")
        
    def write_output(self, output_path: Path):
        """Write OrcaWave input file."""
        try:
            # Update header with current timestamp
            header = [
                "%YAML 1.1",
                "# Type: Diffraction",
                "# Program: OrcaWave 11.5e",
                f"# File: {output_path}",
                f"# Created: {datetime.now().strftime('%I:%M %p on %m/%d/%Y')}",
                "# Generated by: OrcaWave Input Generator",
                "---"
            ]
            
            # Convert template to YAML string
            yaml_content = yaml.dump(
                self.template,
                default_flow_style=False,
                sort_keys=False,
                allow_unicode=True
            )
            
            # Write with header
            with open(output_path, 'w') as f:
                f.write('\n'.join(header) + '\n')
                f.write(yaml_content)
                f.write('...\n')
                
            logger.info(f"Wrote OrcaWave input: {output_path}")
            return True
            
        except Exception as e:
            logger.error(f"Error writing output: {e}")
            return False
            
    def validate_configuration(self) -> Dict[str, Any]:
        """Validate OrcaWave configuration."""
        validation = {
            'is_valid': True,
            'errors': [],
            'warnings': []
        }
        
        # Check required fields
        required = ['UnitsSystem', 'WaterDepth', 'WaterDensity', 'Bodies']
        for field in required:
            if field not in self.template:
                validation['errors'].append(f"Missing required field: {field}")
                validation['is_valid'] = False
                
        # Check Bodies configuration
        if 'Bodies' in self.template:
            if len(self.template['Bodies']) == 0:
                validation['errors'].append("No bodies defined")
                validation['is_valid'] = False
            else:
                body = self.template['Bodies'][0]
                
                # Check required body fields
                body_required = ['BodyMeshFileName', 'BodyMass', 'BodyCentreOfMass']
                for field in body_required:
                    if field not in body:
                        validation['errors'].append(f"Missing body field: {field}")
                        validation['is_valid'] = False
                        
                # Check mesh file exists
                if 'BodyMeshFileName' in body:
                    mesh_path = self.template_path.parent.parent / body['BodyMeshFileName']
                    if not mesh_path.exists():
                        validation['warnings'].append(f"Mesh file not found: {mesh_path}")
                        
        # Check wave conditions
        if 'PeriodOrFrequency' in self.template:
            periods = self.template['PeriodOrFrequency']
            if len(periods) < 2:
                validation['warnings'].append("Less than 2 wave periods defined")
        else:
            validation['errors'].append("No wave periods defined")
            validation['is_valid'] = False
            
        if 'WaveHeading' in self.template:
            headings = self.template['WaveHeading']
            if len(headings) < 3:
                validation['warnings'].append("Less than 3 wave headings defined")
        else:
            validation['errors'].append("No wave headings defined")
            validation['is_valid'] = False
            
        return validation


def main():
    """Main execution function."""
    parser = argparse.ArgumentParser(description='Generate OrcaWave input files')
    parser.add_argument('template', help='Template YAML file path')
    parser.add_argument('--output', required=True, help='Output YAML file path')
    parser.add_argument('--gdf', help='GDF geometry file path')
    parser.add_argument('--excel', help='Excel file with vessel data')
    parser.add_argument('--water-depth', type=float, help='Water depth in meters')
    parser.add_argument('--validate', action='store_true', help='Validate configuration')
    parser.add_argument('--verbose', action='store_true', help='Enable verbose logging')
    
    args = parser.parse_args()
    
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
        
    # Generate input
    generator = OrcaWaveInputGenerator(Path(args.template))
    
    if generator.load_template():
        # Update geometry if provided
        if args.gdf:
            generator.update_geometry(Path(args.gdf))
            
        # Update water depth if provided
        if args.water_depth:
            generator.update_environment(water_depth=args.water_depth)
            
        # Load vessel data from Excel if provided
        if args.excel:
            # Import the Excel extractor
            import sys
            sys.path.append(str(Path(__file__).parent))
            from extract_excel_data import VesselDataExtractor
            
            extractor = VesselDataExtractor(Path(args.excel))
            vessel_data = extractor.extract_parameters()
            orcawave_data = extractor.format_for_orcawave()
            generator.update_vessel_properties(orcawave_data)
            
        # Validate if requested
        if args.validate:
            validation = generator.validate_configuration()
            print("\n=== Configuration Validation ===")
            print(f"Valid: {validation['is_valid']}")
            
            if validation['errors']:
                print("\nErrors:")
                for error in validation['errors']:
                    print(f"  ❌ {error}")
                    
            if validation['warnings']:
                print("\nWarnings:")
                for warning in validation['warnings']:
                    print(f"  ⚠️ {warning}")
                    
        # Write output
        if generator.write_output(Path(args.output)):
            print(f"\n✅ Generated OrcaWave input: {args.output}")
        else:
            print(f"\n❌ Failed to generate OrcaWave input")
            

if __name__ == "__main__":
    main()