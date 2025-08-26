#!/usr/bin/env python3
"""
Validate hydrodynamic data for consistency and physical constraints.

This script performs comprehensive validation of:
1. Hydrodynamic properties from Excel extraction
2. OrcaWave configuration files
3. Physical constraints and unit consistency
"""

import os
import yaml
import numpy as np
from pathlib import Path
from typing import Dict, Any, List, Tuple
from datetime import datetime


class HydrodynamicValidator:
    """Validator for hydrodynamic properties and configurations."""
    
    def __init__(self):
        """Initialize validator with physical constraints."""
        # Physical constraints
        self.min_mass = 1000.0  # kg (minimum reasonable vessel mass)
        self.max_mass = 1e9     # kg (maximum reasonable vessel mass)
        
        # Density of water
        self.water_density = 1025.0  # kg/m³ (seawater)
        
        # Validation results
        self.errors = []
        self.warnings = []
        self.info = []
    
    def validate_mass_properties(self, mass_props: Dict[str, Any], vessel_name: str = "") -> bool:
        """
        Validate mass properties for physical consistency.
        
        Args:
            mass_props: Dictionary containing mass properties
            vessel_name: Name of vessel configuration
            
        Returns:
            True if valid, False otherwise
        """
        valid = True
        prefix = f"[{vessel_name}] " if vessel_name else ""
        
        # Check mass
        mass = mass_props.get('mass', {}).get('value', 0)
        if mass <= 0:
            self.errors.append(f"{prefix}Mass must be positive (found: {mass} kg)")
            valid = False
        elif mass < self.min_mass:
            self.warnings.append(f"{prefix}Mass seems too small ({mass} kg)")
        elif mass > self.max_mass:
            self.warnings.append(f"{prefix}Mass seems too large ({mass} kg)")
        
        # Check center of gravity
        cog = mass_props.get('center_of_gravity', {})
        cog_x = cog.get('x', {}).get('value', 0)
        cog_y = cog.get('y', {}).get('value', 0)
        cog_z = cog.get('z', {}).get('value', 0)
        
        # Check if CoG is reasonable
        if cog_z < 0:
            self.warnings.append(f"{prefix}CoG Z is below baseline ({cog_z} m)")
        
        # Check moments of inertia
        moments = mass_props.get('moments_of_inertia', {})
        ixx = moments.get('ixx', {}).get('value', 0)
        iyy = moments.get('iyy', {}).get('value', 0)
        izz = moments.get('izz', {}).get('value', 0)
        
        if ixx <= 0:
            self.errors.append(f"{prefix}Ixx must be positive (found: {ixx} kg.m²)")
            valid = False
        if iyy <= 0:
            self.errors.append(f"{prefix}Iyy must be positive (found: {iyy} kg.m²)")
            valid = False
        if izz <= 0:
            self.errors.append(f"{prefix}Izz must be positive (found: {izz} kg.m²)")
            valid = False
        
        # Check triangle inequality for moments of inertia
        if ixx + iyy < izz:
            self.warnings.append(f"{prefix}Moments of inertia may violate triangle inequality")
        
        # Check radii of gyration
        gyradii = mass_props.get('radii_of_gyration', {})
        kxx = gyradii.get('kxx', {}).get('value', 0)
        kyy = gyradii.get('kyy', {}).get('value', 0)
        kzz = gyradii.get('kzz', {}).get('value', 0)
        
        if kxx <= 0:
            self.errors.append(f"{prefix}Kxx must be positive (found: {kxx} m)")
            valid = False
        if kyy <= 0:
            self.errors.append(f"{prefix}Kyy must be positive (found: {kyy} m)")
            valid = False
        if kzz <= 0:
            self.errors.append(f"{prefix}Kzz must be positive (found: {kzz} m)")
            valid = False
        
        # Cross-check inertia and gyradii consistency
        if mass > 0:
            calculated_ixx = mass * kxx * kxx
            calculated_iyy = mass * kyy * kyy
            calculated_izz = mass * kzz * kzz
            
            # Check within 1% tolerance
            tolerance = 0.01
            if abs(calculated_ixx - ixx) / ixx > tolerance:
                self.warnings.append(
                    f"{prefix}Inconsistency between Ixx and Kxx "
                    f"(calculated: {calculated_ixx:.2f}, given: {ixx:.2f})"
                )
            if abs(calculated_iyy - iyy) / iyy > tolerance:
                self.warnings.append(
                    f"{prefix}Inconsistency between Iyy and Kyy "
                    f"(calculated: {calculated_iyy:.2f}, given: {iyy:.2f})"
                )
            if abs(calculated_izz - izz) / izz > tolerance:
                self.warnings.append(
                    f"{prefix}Inconsistency between Izz and Kzz "
                    f"(calculated: {calculated_izz:.2f}, given: {izz:.2f})"
                )
        
        return valid
    
    def validate_vessel_dimensions(self, vessel_dims: Dict[str, Any], draft: float = None) -> bool:
        """
        Validate vessel dimensions for consistency.
        
        Args:
            vessel_dims: Dictionary containing vessel dimensions
            draft: Draft value in meters
            
        Returns:
            True if valid, False otherwise
        """
        valid = True
        
        # Extract dimensions
        lbp = vessel_dims.get('LBP', {}).get('value', 0)
        beam = vessel_dims.get('beam', {}).get('value', 0)
        depth = vessel_dims.get('depth', {}).get('value', 0)
        
        # Check dimensions are positive
        if lbp <= 0:
            self.errors.append(f"LBP must be positive (found: {lbp} m)")
            valid = False
        if beam <= 0:
            self.errors.append(f"Beam must be positive (found: {beam} m)")
            valid = False
        if depth <= 0:
            self.errors.append(f"Depth must be positive (found: {depth} m)")
            valid = False
        
        # Check aspect ratios
        if lbp > 0 and beam > 0:
            lb_ratio = lbp / beam
            if lb_ratio < 2:
                self.warnings.append(f"Length/Beam ratio seems small ({lb_ratio:.2f})")
            elif lb_ratio > 10:
                self.warnings.append(f"Length/Beam ratio seems large ({lb_ratio:.2f})")
        
        # Check draft
        if draft is not None:
            if draft <= 0:
                self.errors.append(f"Draft must be positive (found: {draft} m)")
                valid = False
            elif draft >= depth:
                self.errors.append(f"Draft ({draft} m) must be less than depth ({depth} m)")
                valid = False
            elif draft > 0.9 * depth:
                self.warnings.append(f"Draft ({draft} m) is very close to depth ({depth} m)")
        
        return valid
    
    def validate_hydrostatic_consistency(self, config_data: Dict[str, Any], 
                                        vessel_dims: Dict[str, Any]) -> bool:
        """
        Validate hydrostatic consistency (approximate checks).
        
        Args:
            config_data: Configuration data including mass and draft
            vessel_dims: Vessel dimensions
            
        Returns:
            True if valid, False otherwise
        """
        valid = True
        
        # Extract values
        mass = config_data.get('mass', 0)
        draft = config_data.get('draft', 0)
        lbp = vessel_dims.get('LBP', {}).get('value', 0)
        beam = vessel_dims.get('beam', {}).get('value', 0)
        
        if mass > 0 and draft > 0 and lbp > 0 and beam > 0:
            # Estimate displaced volume (very approximate)
            # Using block coefficient of ~0.7 for supply vessel
            block_coeff = 0.7
            displaced_volume = lbp * beam * draft * block_coeff
            
            # Calculate displacement
            displacement = displaced_volume * self.water_density
            
            # Check if mass and displacement are reasonably close
            # (They won't match exactly due to approximations)
            diff_percent = abs(mass - displacement) / mass * 100
            
            if diff_percent > 30:
                self.warnings.append(
                    f"Large difference between mass ({mass:.0f} kg) and "
                    f"estimated displacement ({displacement:.0f} kg) - {diff_percent:.1f}%"
                )
                self.info.append(
                    f"Note: This is an approximate check. Actual displacement "
                    f"depends on hull form and exact waterline."
                )
        
        return valid
    
    def validate_unit_consistency(self, data: Dict[str, Any]) -> bool:
        """
        Validate that all units are consistent (SI units).
        
        Args:
            data: Data dictionary to check
            
        Returns:
            True if consistent, False otherwise
        """
        valid = True
        
        # Expected SI units
        expected_units = {
            'mass': 'kg',
            'length': 'm',
            'inertia': 'kg.m^2',
            'time': 's',
            'force': 'N',
            'moment': 'N.m'
        }
        
        # Check units in configuration
        def check_units(d, path=""):
            if isinstance(d, dict):
                for key, value in d.items():
                    if key == 'unit':
                        if value not in expected_units.values():
                            if value not in ['ft', 'LT', 'LT.ft^2']:  # Original units are OK
                                self.warnings.append(f"Unexpected unit at {path}: {value}")
                    elif isinstance(value, dict):
                        check_units(value, f"{path}/{key}" if path else key)
        
        check_units(data)
        return valid
    
    def validate_file_structure(self, base_path: Path) -> bool:
        """
        Validate that all expected files exist.
        
        Args:
            base_path: Base path for validation
            
        Returns:
            True if all files exist, False otherwise
        """
        valid = True
        
        # Expected files
        expected_files = [
            'outputs/hydrodynamic.yml',
            'outputs/hydrodynamic_detailed.yml',
            'outputs/orcawave_configs/master_config.yml',
            'outputs/orcawave_configs/batch_analysis.yml',
        ]
        
        for file_path in expected_files:
            full_path = base_path / file_path
            if not full_path.exists():
                self.errors.append(f"Missing expected file: {file_path}")
                valid = False
            else:
                self.info.append(f"Found: {file_path}")
        
        return valid
    
    def generate_report(self) -> str:
        """
        Generate validation report.
        
        Returns:
            Formatted validation report
        """
        report = []
        report.append("=" * 60)
        report.append("HYDRODYNAMIC DATA VALIDATION REPORT")
        report.append("=" * 60)
        report.append(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        report.append("")
        
        # Summary
        report.append("SUMMARY")
        report.append("-" * 30)
        report.append(f"Errors:   {len(self.errors)}")
        report.append(f"Warnings: {len(self.warnings)}")
        report.append(f"Info:     {len(self.info)}")
        report.append("")
        
        # Errors
        if self.errors:
            report.append("ERRORS (Must be fixed)")
            report.append("-" * 30)
            for error in self.errors:
                report.append(f"ERROR: {error}")
            report.append("")
        
        # Warnings
        if self.warnings:
            report.append("WARNINGS (Should be reviewed)")
            report.append("-" * 30)
            for warning in self.warnings:
                report.append(f"WARNING: {warning}")
            report.append("")
        
        # Info
        if self.info:
            report.append("INFORMATION")
            report.append("-" * 30)
            for info in self.info:
                report.append(f"INFO: {info}")
            report.append("")
        
        # Status
        report.append("VALIDATION STATUS")
        report.append("-" * 30)
        if self.errors:
            report.append("FAILED - Errors must be resolved")
        elif self.warnings:
            report.append("PASSED WITH WARNINGS - Review warnings before proceeding")
        else:
            report.append("PASSED - All validations successful")
        
        report.append("=" * 60)
        
        return "\n".join(report)


def main():
    """Main execution function."""
    # Set up paths
    script_dir = Path(__file__).parent
    base_dir = script_dir.parent
    
    # Initialize validator
    validator = HydrodynamicValidator()
    
    print("Starting hydrodynamic data validation...")
    print("-" * 60)
    
    # Validate file structure
    print("Checking file structure...")
    validator.validate_file_structure(base_dir)
    
    # Load and validate hydrodynamic data
    hydro_file = base_dir / 'outputs' / 'hydrodynamic_detailed.yml'
    if hydro_file.exists():
        print(f"Loading: {hydro_file}")
        with open(hydro_file, 'r') as f:
            hydro_data = yaml.safe_load(f)
        
        # Validate vessel dimensions
        print("Validating vessel dimensions...")
        vessel_dims = hydro_data['hydrodynamic_properties'].get('vessel_dimensions', {})
        validator.validate_vessel_dimensions(vessel_dims)
        
        # Validate each configuration
        configs = hydro_data['hydrodynamic_properties'].get('vessel_configurations', [])
        print(f"Validating {len(configs)} vessel configurations...")
        
        for config in configs:
            name = config.get('name', 'unknown')
            print(f"  Validating: {name}")
            
            # Validate mass properties
            mass_props = config.get('mass_properties', {})
            validator.validate_mass_properties(mass_props, name)
            
            # Validate draft
            draft = config.get('draft', {}).get('value', 0)
            validator.validate_vessel_dimensions(vessel_dims, draft)
            
            # Validate hydrostatic consistency
            simple_config = {
                'mass': mass_props.get('mass', {}).get('value', 0),
                'draft': draft
            }
            validator.validate_hydrostatic_consistency(simple_config, vessel_dims)
        
        # Validate unit consistency
        print("Checking unit consistency...")
        validator.validate_unit_consistency(hydro_data)
    
    # Validate OrcaWave configurations
    orcawave_dir = base_dir / 'outputs' / 'orcawave_configs'
    if orcawave_dir.exists():
        vessel_types_dir = orcawave_dir / 'vessel_types'
        if vessel_types_dir.exists():
            print(f"Validating OrcaWave vessel types...")
            for vessel_file in vessel_types_dir.glob('*.yml'):
                print(f"  Checking: {vessel_file.name}")
                with open(vessel_file, 'r') as f:
                    vessel_data = yaml.safe_load(f)
                # Basic check for required fields
                if 'VesselType' not in vessel_data:
                    validator.errors.append(f"Missing VesselType in {vessel_file.name}")
    
    # Generate and save report
    report = validator.generate_report()
    report_file = base_dir / 'outputs' / 'validation_report.txt'
    with open(report_file, 'w', encoding='utf-8') as f:
        f.write(report)
    
    # Print report
    print("\n" + report)
    print(f"\nValidation report saved to: {report_file}")
    
    # Return status
    return len(validator.errors) == 0


if __name__ == "__main__":
    import sys
    success = main()
    sys.exit(0 if success else 1)