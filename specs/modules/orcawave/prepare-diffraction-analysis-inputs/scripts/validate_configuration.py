#!/usr/bin/env python3
"""
Validate OrcaWave configuration files.
Checks YAML syntax, required fields, parameter ranges, and generates validation report.
"""

import yaml
import json
from pathlib import Path
from typing import Dict, Any, List, Tuple
import logging
from datetime import datetime

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


class ConfigurationValidator:
    """Validates OrcaWave configuration files."""
    
    def __init__(self):
        """Initialize configuration validator."""
        self.errors = []
        self.warnings = []
        self.info = []
        
        # Define required fields for OrcaWave configuration
        self.required_fields = {
            'root': ['UnitsSystem', 'SolveType', 'WaterDepth', 'WaterDensity', 'Bodies'],
            'body': ['BodyName', 'BodyMeshFileName', 'BodyMass', 'BodyCentreOfMass'],
        }
        
        # Define parameter ranges
        self.param_ranges = {
            'WaterDepth': (0.1, 10000),  # meters
            'WaterDensity': (900, 1100),  # kg/m3
            'BodyMass': (1, 1e9),  # kg
            'LengthTolerance': (1e-12, 1e-3),
            'WaterlineZTolerance': (1e-9, 1),
            'WaterlineGapTolerance': (1e-9, 1),
            'PanelAspectRatioWarningLevel': (1, 100),
            'PanelsPerWavelengthWarningLevel': (1, 20),
        }
        
        # Valid values for specific fields
        self.valid_values = {
            'UnitsSystem': ['SI', 'Imperial'],
            'SolveType': ['Potential formulation only', 'Full QTF calculation', 
                         'Haskind formulation only'],
            'LoadRAOCalculationMethod': ['Pressure integration', 'Haskind', 'Both'],
            'LinearSolverMethod': ['Direct LU', 'Iterative'],
            'WavesReferredToBy': ['period (s)', 'frequency (Hz)', 'frequency (rad/s)'],
            'BodyMeshFormat': ['Wamit gdf', 'Wamit dat', 'Aqwa dat', 'Gmsh msh', 
                              'OrcaWave panel mesh'],
            'BodyMeshSymmetry': ['None', 'xz plane', 'yz plane', 'xz and yz planes'],
            'BodyInertiaSpecifiedBy': ['Matrix (for a general body)', 
                                       'Radii of gyration (for a free-floating body)'],
        }
        
    def reset(self):
        """Reset validation state."""
        self.errors = []
        self.warnings = []
        self.info = []
        
    def validate_yaml_syntax(self, file_path: Path) -> bool:
        """Check YAML syntax.
        
        Args:
            file_path: Path to YAML file
            
        Returns:
            True if syntax is valid
        """
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                yaml.safe_load(f)
            self.info.append(f"YAML syntax valid for {file_path.name}")
            return True
        except yaml.YAMLError as e:
            self.errors.append(f"YAML syntax error in {file_path.name}: {e}")
            return False
        except Exception as e:
            self.errors.append(f"Error reading {file_path.name}: {e}")
            return False
            
    def check_required_fields(self, config: Dict[str, Any], file_name: str = ""):
        """Check for required fields.
        
        Args:
            config: Configuration dictionary
            file_name: Name of file being validated
        """
        prefix = f"[{file_name}] " if file_name else ""
        
        # Check root-level required fields
        for field in self.required_fields['root']:
            if field not in config:
                self.errors.append(f"{prefix}Missing required field: {field}")
                
        # Check body-level required fields
        if 'Bodies' in config:
            if not isinstance(config['Bodies'], list) or len(config['Bodies']) == 0:
                self.errors.append(f"{prefix}Bodies must be a non-empty list")
            else:
                for i, body in enumerate(config['Bodies']):
                    for field in self.required_fields['body']:
                        if field not in body:
                            self.errors.append(
                                f"{prefix}Body {i}: Missing required field: {field}"
                            )
                            
    def validate_parameter_ranges(self, config: Dict[str, Any], file_name: str = ""):
        """Validate parameter ranges.
        
        Args:
            config: Configuration dictionary
            file_name: Name of file being validated
        """
        prefix = f"[{file_name}] " if file_name else ""
        
        # Check root-level parameters
        for param, (min_val, max_val) in self.param_ranges.items():
            if param in config:
                value = config[param]
                if isinstance(value, (int, float)):
                    if value == 'Infinity':
                        continue  # Special case for infinite water depth
                    if not (min_val <= value <= max_val):
                        self.warnings.append(
                            f"{prefix}{param} = {value} is outside expected range "
                            f"[{min_val}, {max_val}]"
                        )
                        
        # Check body parameters
        if 'Bodies' in config:
            for i, body in enumerate(config['Bodies']):
                if 'BodyMass' in body:
                    mass = body['BodyMass']
                    min_val, max_val = self.param_ranges['BodyMass']
                    if not (min_val <= mass <= max_val):
                        self.warnings.append(
                            f"{prefix}Body {i}: Mass = {mass} kg is outside expected range "
                            f"[{min_val}, {max_val}]"
                        )
                        
    def validate_field_values(self, config: Dict[str, Any], file_name: str = ""):
        """Validate field values against known valid options.
        
        Args:
            config: Configuration dictionary
            file_name: Name of file being validated
        """
        prefix = f"[{file_name}] " if file_name else ""
        
        # Check root-level field values
        for field, valid_options in self.valid_values.items():
            if field in config:
                value = config[field]
                if value not in valid_options:
                    self.warnings.append(
                        f"{prefix}{field} = '{value}' is not in known valid options: "
                        f"{valid_options}"
                    )
                    
        # Check body field values
        if 'Bodies' in config:
            for i, body in enumerate(config['Bodies']):
                for field, valid_options in self.valid_values.items():
                    if field in body:
                        value = body[field]
                        if value not in valid_options:
                            self.warnings.append(
                                f"{prefix}Body {i}: {field} = '{value}' is not in known valid options: "
                                f"{valid_options}"
                            )
                            
    def check_file_references(self, config: Dict[str, Any], file_name: str = ""):
        """Check that referenced files exist.
        
        Args:
            config: Configuration dictionary
            file_name: Name of file being validated
        """
        prefix = f"[{file_name}] " if file_name else ""
        
        if 'Bodies' in config:
            for i, body in enumerate(config['Bodies']):
                if 'BodyMeshFileName' in body:
                    mesh_file = body['BodyMeshFileName']
                    # Check if it's a relative or absolute path
                    mesh_path = Path(mesh_file)
                    
                    # For relative paths, check from script location
                    if not mesh_path.is_absolute():
                        base_dir = Path(__file__).parent.parent
                        mesh_path = base_dir / "outputs" / "orcawave_configs" / "merged" / mesh_file
                        
                    # Note: Don't fail if file doesn't exist, just inform
                    if not mesh_path.exists():
                        self.info.append(
                            f"{prefix}Body {i}: Mesh file not found at expected location: {mesh_file}"
                        )
                    else:
                        self.info.append(
                            f"{prefix}Body {i}: Mesh file reference valid: {mesh_file}"
                        )
                        
    def validate_wave_data(self, config: Dict[str, Any], file_name: str = ""):
        """Validate wave period/frequency and heading data.
        
        Args:
            config: Configuration dictionary
            file_name: Name of file being validated
        """
        prefix = f"[{file_name}] " if file_name else ""
        
        # Check wave periods/frequencies
        if 'PeriodOrFrequency' in config:
            periods = config['PeriodOrFrequency']
            if not isinstance(periods, list) or len(periods) == 0:
                self.errors.append(f"{prefix}PeriodOrFrequency must be a non-empty list")
            else:
                # Check for reasonable wave period range (2-30 seconds typical)
                for period in periods:
                    if isinstance(period, (int, float)):
                        if not (0.5 <= period <= 100):
                            self.warnings.append(
                                f"{prefix}Wave period {period}s is outside typical range [0.5, 100]"
                            )
                            
        # Check wave headings
        if 'WaveHeading' in config:
            headings = config['WaveHeading']
            if not isinstance(headings, list) or len(headings) == 0:
                self.errors.append(f"{prefix}WaveHeading must be a non-empty list")
            else:
                # Check heading range (0-360 degrees)
                for heading in headings:
                    if isinstance(heading, (int, float)):
                        if not (0 <= heading <= 360):
                            self.errors.append(
                                f"{prefix}Wave heading {heading} degrees is outside valid range [0, 360]"
                            )
                            
    def validate_configuration(self, config: Dict[str, Any], file_name: str = "") -> bool:
        """Perform complete validation of configuration.
        
        Args:
            config: Configuration dictionary
            file_name: Name of file being validated
            
        Returns:
            True if configuration is valid (no errors)
        """
        self.check_required_fields(config, file_name)
        self.validate_parameter_ranges(config, file_name)
        self.validate_field_values(config, file_name)
        self.check_file_references(config, file_name)
        self.validate_wave_data(config, file_name)
        
        return len(self.errors) == 0
        
    def validate_file(self, file_path: Path) -> bool:
        """Validate a single configuration file.
        
        Args:
            file_path: Path to configuration file
            
        Returns:
            True if file is valid
        """
        file_name = file_path.name
        
        # Check YAML syntax
        if not self.validate_yaml_syntax(file_path):
            return False
            
        # Load configuration
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                config = yaml.safe_load(f)
        except Exception as e:
            self.errors.append(f"Error loading {file_name}: {e}")
            return False
            
        # Validate configuration
        return self.validate_configuration(config, file_name)
        
    def generate_report(self) -> str:
        """Generate validation report.
        
        Returns:
            Formatted validation report
        """
        report_lines = []
        report_lines.append("="*60)
        report_lines.append("ORCAWAVE CONFIGURATION VALIDATION REPORT")
        report_lines.append("="*60)
        report_lines.append(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        report_lines.append("")
        
        # Summary
        report_lines.append("SUMMARY:")
        report_lines.append(f"  Errors: {len(self.errors)}")
        report_lines.append(f"  Warnings: {len(self.warnings)}")
        report_lines.append(f"  Info: {len(self.info)}")
        report_lines.append("")
        
        # Errors
        if self.errors:
            report_lines.append("ERRORS:")
            for error in self.errors:
                report_lines.append(f"  ERROR: {error}")
            report_lines.append("")
            
        # Warnings
        if self.warnings:
            report_lines.append("WARNINGS:")
            for warning in self.warnings:
                report_lines.append(f"  WARNING: {warning}")
            report_lines.append("")
            
        # Info
        if self.info:
            report_lines.append("INFORMATION:")
            for info in self.info:
                report_lines.append(f"  INFO: {info}")
            report_lines.append("")
            
        # Result
        report_lines.append("VALIDATION RESULT:")
        if len(self.errors) == 0:
            report_lines.append("  PASSED - Configuration is valid")
        else:
            report_lines.append("  FAILED - Configuration has errors that must be fixed")
            
        report_lines.append("="*60)
        
        return "\n".join(report_lines)


def main():
    """Validate all merged configuration files."""
    validator = ConfigurationValidator()
    
    # Find all merged configuration files
    merged_dir = Path(__file__).parent.parent / "outputs" / "orcawave_configs" / "merged"
    
    if not merged_dir.exists():
        print(f"Merged configurations directory not found: {merged_dir}")
        print("Please run merge_templates.py first to create configurations")
        return
        
    config_files = list(merged_dir.glob("*.yml"))
    
    if not config_files:
        print(f"No configuration files found in: {merged_dir}")
        return
        
    print(f"Found {len(config_files)} configuration files to validate")
    print()
    
    # Validate each file
    all_valid = True
    for config_file in config_files:
        print(f"Validating: {config_file.name}")
        validator.reset()
        
        if validator.validate_file(config_file):
            print(f"  PASSED")
        else:
            print(f"  FAILED - {len(validator.errors)} errors")
            all_valid = False
            
    # Generate combined report
    print("\n" + validator.generate_report())
    
    # Save report
    report_file = Path(__file__).parent.parent / "outputs" / "orcawave_configs" / "validation_report.txt"
    with open(report_file, 'w') as f:
        f.write(validator.generate_report())
    print(f"\nValidation report saved to: {report_file}")
    
    return all_valid


if __name__ == "__main__":
    success = main()
    exit(0 if success else 1)