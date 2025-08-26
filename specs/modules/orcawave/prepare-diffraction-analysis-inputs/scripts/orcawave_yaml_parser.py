#!/usr/bin/env python3
"""
Custom YAML parser for OrcaWave that preserves exact formatting.
OrcaWave is very particular about YAML formatting and requires specific representations.
"""

import yaml
import re
from pathlib import Path
from typing import Dict, Any, List, Union
import logging

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


class OrcaWaveYamlDumper(yaml.SafeDumper):
    """Custom YAML dumper that preserves OrcaWave-specific formatting."""
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        # Don't use aliases/anchors
        self.ignore_aliases = lambda *args: True


class OrcaWaveYamlParser:
    """Parser that maintains OrcaWave-specific YAML formatting conventions."""
    
    def __init__(self):
        """Initialize the OrcaWave YAML parser."""
        # OrcaWave-specific boolean mappings
        self.boolean_mappings = {
            # Fields that use Yes/No
            'yes_no_fields': [
                'QuadraticLoadPressureIntegration',
                'QuadraticLoadControlSurface', 
                'QuadraticLoadMomentumConservation',
                'DivideNonPlanarPanels',
                'OutputPanelPressures',
                'OutputPanelVelocities',
                'OutputBodyWireFrames',
                'OutputIntermediateResults',
                'ValidatePanelArrangement',
                'HasWaveSpectrumForDragLinearisation',
                'HasResonanceDampingLid',
                'BodyIncludedInAnalysis',
                'BodyImportDryPanels',
                'BodyAddInteriorSurfacePanels',
                'BodyControlSurfaceIncludeFreeSurface',
                'BodyIncreaseRollDampingToTarget',
                'BodyFixedDOFx',
                'BodyFixedDOFy', 
                'BodyFixedDOFz',
                'BodyFixedDOFRx',
                'BodyFixedDOFRy',
                'BodyFixedDOFRz',
                'DetectAndSkipFieldPointsInsideBodies',
                'IncludeMeanDriftFullQTFs'
            ],
            # Fields that use true/false (lowercase)
            'true_false_fields': [
                # Currently none in template, but keeping for potential future use
            ]
        }
        
        # Fields that should use ~ instead of null/None/empty
        self.tilde_fields = [
            'BodyMeshDipolePanels',
            'BodyOrcaFlexImportLength'
        ]
        
        # Fields that should use empty string instead of ~
        self.empty_string_fields = [
            'FreeSurfacePanelledZoneMeshFileName'  # Should be empty, not ~
        ]
        
        # Fields that should use specific representations
        self.special_values = {
            'Infinity': ['QTFMaxPeriodOrFrequency', 'WaterDepth'],
            'N/A': ['BodyCentreOfMass']  # When placeholder needed
        }
        
    def load_template(self, template_path: Path) -> Dict[str, Any]:
        """Load template and analyze its formatting conventions.
        
        Args:
            template_path: Path to OrcaWave template file
            
        Returns:
            Dictionary containing template data
        """
        with open(template_path, 'r', encoding='utf-8-sig') as f:
            # Read raw content to analyze formatting
            raw_content = f.read()
            
        # Parse the YAML
        template_data = yaml.safe_load(raw_content)
        
        # Analyze boolean representations used in template
        self._analyze_boolean_format(raw_content)
        
        return template_data
        
    def _analyze_boolean_format(self, content: str):
        """Analyze the template to determine boolean formatting preferences.
        
        Args:
            content: Raw YAML content
        """
        # Check for Yes/No usage
        yes_no_pattern = re.compile(r'^(\w+):\s*(Yes|No)\s*$', re.MULTILINE)
        yes_no_matches = yes_no_pattern.findall(content)
        
        # Check for true/false usage  
        true_false_pattern = re.compile(r'^(\w+):\s*(true|false|True|False)\s*$', re.MULTILINE)
        true_false_matches = true_false_pattern.findall(content)
        
        logger.debug(f"Found {len(yes_no_matches)} Yes/No fields")
        logger.debug(f"Found {len(true_false_matches)} true/false fields")
        
    def format_value(self, key: str, value: Any) -> Any:
        """Format a value according to OrcaWave conventions.
        
        Args:
            key: The field name
            value: The value to format
            
        Returns:
            Properly formatted value for OrcaWave
        """
        # Handle empty string fields first
        if key in self.empty_string_fields:
            return ''  # Always return empty string for these fields
            
        # Handle None/null values
        if value is None:
            if key in self.tilde_fields:
                return None  # Will be represented as ~
            return None
            
        # Handle boolean values
        if isinstance(value, bool):
            if key in self.boolean_mappings['yes_no_fields']:
                return 'Yes' if value else 'No'
            else:
                # Default to Yes/No for OrcaWave
                return 'Yes' if value else 'No'
                
        # Handle infinity
        if isinstance(value, (int, float)):
            if value == float('inf'):
                return 'Infinity'
            elif value == float('-inf'):
                return '-Infinity'
                
        # Handle special string values
        if isinstance(value, str):
            # Convert boolean strings to proper format
            if value.lower() in ['true', 'yes', '1']:
                return self.format_value(key, True)
            elif value.lower() in ['false', 'no', '0']:
                return self.format_value(key, False)
                
        return value
        
    def format_data(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Recursively format all values in data structure.
        
        Args:
            data: Data structure to format
            
        Returns:
            Formatted data structure
        """
        if isinstance(data, dict):
            formatted = {}
            for key, value in data.items():
                if isinstance(value, (dict, list)):
                    formatted[key] = self.format_data(value)
                else:
                    formatted[key] = self.format_value(key, value)
            return formatted
        elif isinstance(data, list):
            return [self.format_data(item) if isinstance(item, (dict, list)) else item 
                   for item in data]
        else:
            return data
            
    def write_orcawave_yaml(self, data: Dict[str, Any], output_path: Path, 
                           config_name: str = None):
        """Write data to YAML file with OrcaWave-specific formatting.
        
        Args:
            data: Data to write
            output_path: Output file path
            config_name: Optional configuration name for header
        """
        # Format the data
        formatted_data = self.format_data(data)
        
        # Custom representers for OrcaWave format
        def represent_none(dumper, data):
            return dumper.represent_scalar('tag:yaml.org,2002:null', '~')
            
        def represent_str(dumper, data):
            # Special handling for certain values - unquoted for OrcaWave
            if data == 'Infinity':
                return dumper.represent_scalar('tag:yaml.org,2002:str', 'Infinity', style='')
            elif data in ['Yes', 'No']:
                # Return unquoted Yes/No for OrcaWave
                return dumper.represent_scalar('tag:yaml.org,2002:bool', data, style='')
            return dumper.represent_str(data)
            
        def represent_bool(dumper, data):
            # This shouldn't be called if format_data works correctly
            return dumper.represent_scalar('tag:yaml.org,2002:str', 'Yes' if data else 'No', style='')
            
        def represent_float(dumper, data):
            if data == float('inf'):
                return dumper.represent_scalar('tag:yaml.org,2002:str', 'Infinity', style='')
            elif data == float('-inf'):
                return dumper.represent_scalar('tag:yaml.org,2002:str', '-Infinity', style='')
            elif data != data:  # NaN
                return dumper.represent_scalar('tag:yaml.org,2002:null', '~')
            else:
                # Use default float representation
                return dumper.represent_float(data)
                
        # Configure dumper
        OrcaWaveYamlDumper.add_representer(type(None), represent_none)
        OrcaWaveYamlDumper.add_representer(str, represent_str)
        OrcaWaveYamlDumper.add_representer(bool, represent_bool)
        OrcaWaveYamlDumper.add_representer(float, represent_float)
        
        # Write with UTF-8 BOM
        with open(output_path, 'w', encoding='utf-8-sig') as f:
            # Write header
            f.write("%YAML 1.1\n")
            f.write("# Type: Diffraction\n")
            f.write("# Program: OrcaWave 11.5e\n")
            f.write(f"# File: {output_path}\n")
            
            # Format date/time
            from datetime import datetime
            now = datetime.now()
            time_str = now.strftime('%I:%M %p').lstrip('0')
            date_str = f"{now.month}/{now.day}/{now.year}"
            f.write(f"# Created: {time_str} on {date_str}\n")
            
            f.write("# User: OrcaWave Template Merger\n")
            if config_name:
                f.write(f"# Configuration: {config_name}\n")
            f.write("---\n")
            
            # Write YAML content
            yaml.dump(formatted_data, f,
                     Dumper=OrcaWaveYamlDumper,
                     default_flow_style=False,
                     sort_keys=False,
                     allow_unicode=True,
                     width=1000,  # Prevent line wrapping
                     indent=2,
                     explicit_start=False,  # We already wrote ---
                     explicit_end=True)  # Write ...
                     
    def compare_with_template(self, generated_path: Path, template_path: Path) -> List[str]:
        """Compare generated file with template to identify formatting differences.
        
        Args:
            generated_path: Path to generated file
            template_path: Path to template file
            
        Returns:
            List of differences found
        """
        differences = []
        
        # Load both files as raw text
        with open(generated_path, 'r', encoding='utf-8-sig') as f:
            generated_lines = f.readlines()
            
        with open(template_path, 'r', encoding='utf-8-sig') as f:
            template_lines = f.readlines()
            
        # Check key formatting patterns
        patterns_to_check = [
            (r':\s*Yes\s*$', 'Yes/No format'),
            (r':\s*No\s*$', 'Yes/No format'),
            (r':\s*true\s*$', 'true/false format'),
            (r':\s*false\s*$', 'true/false format'),
            (r':\s*~\s*$', 'tilde for null'),
            (r':\s*Infinity\s*$', 'Infinity format'),
        ]
        
        for pattern, description in patterns_to_check:
            template_count = sum(1 for line in template_lines if re.search(pattern, line))
            generated_count = sum(1 for line in generated_lines if re.search(pattern, line))
            
            if template_count > 0 and generated_count != template_count:
                differences.append(f"{description}: template has {template_count}, generated has {generated_count}")
                
        return differences


def main():
    """Test the OrcaWave YAML parser."""
    parser = OrcaWaveYamlParser()
    
    # Test data with various value types
    test_data = {
        'UnitsSystem': 'SI',
        'SolveType': 'Full QTF calculation',
        'QuadraticLoadPressureIntegration': True,  # Should become Yes
        'QuadraticLoadControlSurface': False,  # Should become No
        'WaterDepth': float('inf'),  # Should become Infinity
        'WaterDensity': 1.025,
        'OutputPanelPressures': 'true',  # String that should become Yes
        'ValidatePanelArrangement': 'no',  # String that should become No
        'Bodies': [{
            'BodyName': 'TestBody',
            'BodyIncludedInAnalysis': True,  # Should become Yes
            'BodyMeshDipolePanels': None,  # Should become ~
            'BodyOrcaFlexImportLength': None,  # Should become ~
            'BodyFixedDOFx': False,  # Should become No
        }]
    }
    
    # Format the data
    formatted = parser.format_data(test_data)
    
    print("Formatted data:")
    for key, value in formatted.items():
        if key == 'Bodies':
            print(f"  {key}:")
            for body in value:
                for bkey, bvalue in body.items():
                    print(f"    {bkey}: {bvalue}")
        else:
            print(f"  {key}: {value}")
            
    # Test writing to file
    output_path = Path(__file__).parent.parent / "outputs" / "test_orcawave.yml"
    parser.write_orcawave_yaml(formatted, output_path, "test_config")
    print(f"\nTest file written to: {output_path}")


if __name__ == "__main__":
    main()