#!/usr/bin/env python
"""
Load and validate master input configuration for fatigue analysis
"""

import yaml
import sys
from pathlib import Path
import pandas as pd
import numpy as np

class FatigueConfigLoader:
    """Load and validate fatigue analysis configuration"""
    
    def __init__(self, config_file):
        """Initialize with configuration file path"""
        self.config_file = Path(config_file)
        self.config = None
        self.validation_errors = []
        self.validation_warnings = []
        
    def load_config(self):
        """Load YAML configuration file"""
        print(f"Loading configuration: {self.config_file}")
        
        if not self.config_file.exists():
            raise FileNotFoundError(f"Config file not found: {self.config_file}")
            
        with open(self.config_file, 'r') as f:
            self.config = yaml.safe_load(f)
            
        return self.config
        
    def validate_config(self):
        """Validate configuration completeness and consistency"""
        print("\nValidating configuration...")
        
        if not self.config:
            self.validation_errors.append("No configuration loaded")
            return False
            
        # Check required sections
        required_sections = [
            'default', 
            'metadata'
        ]
        for section in required_sections:
            if section not in self.config:
                self.validation_errors.append(f"Missing required section: {section}")
                
        # Get default config
        cfg = self.config.get('default', {})
        
        # Validate fatigue conditions
        self._validate_fatigue_conditions(cfg)
        
        # Validate file paths
        self._validate_file_paths(cfg)
        
        # Validate parameters
        self._validate_parameters(cfg)
        
        # Validate S-N curve
        self._validate_sn_curve(cfg)
        
        # Validate tension-to-stress
        self._validate_tension_stress(cfg)
        
        return len(self.validation_errors) == 0
        
    def _validate_fatigue_conditions(self, cfg):
        """Validate fatigue conditions sum to 100%"""
        fc_data = cfg.get('fatigue_conditions', {}).get('data', {})
        
        if not fc_data:
            self.validation_errors.append("No fatigue conditions defined")
            return
            
        total_occurrence = sum(fc['occurrence_pct'] for fc in fc_data.values())
        tolerance = cfg.get('validation', {}).get('occurrence_sum_tolerance', 0.01)
        
        if abs(total_occurrence - 100.0) > tolerance:
            self.validation_errors.append(
                f"Fatigue conditions occurrence sum = {total_occurrence:.2f}%, expected 100%"
            )
            
        # Check each FC has required fields
        required_fc_fields = ['wind_speed_mps', 'hs_m', 'occurrence_pct']
        for fc_id, fc_data in fc_data.items():
            for field in required_fc_fields:
                if field not in fc_data:
                    self.validation_errors.append(
                        f"Fatigue condition {fc_id} missing field: {field}"
                    )
                    
    def _validate_file_paths(self, cfg):
        """Validate input file paths exist"""
        # For sample config, check if files exist
        if 'sample' in str(self.config_file).lower():
            ref_files = cfg.get('reference_files', {})
            base_dir = Path(ref_files.get('base_directory', ''))
            
            if base_dir.exists():
                for file in ref_files.get('files', []):
                    file_path = base_dir / file
                    if not file_path.exists():
                        self.validation_warnings.append(
                            f"Reference file not found: {file_path}"
                        )
                        
            # Check seastate files
            ss_files = cfg.get('seastate_files', {})
            base_dir = Path(ss_files.get('base_directory', ''))
            
            if base_dir.exists():
                for fc_id, file in ss_files.get('files', {}).items():
                    file_path = base_dir / file
                    if not file_path.exists():
                        self.validation_warnings.append(
                            f"Seastate file not found for {fc_id}: {file_path}"
                        )
                        
    def _validate_parameters(self, cfg):
        """Validate analysis parameters"""
        params = cfg.get('parameters', {})
        
        # Check required parameters
        required_params = [
            'sample_duration_seconds',
            'time_step_seconds', 
            'seconds_per_year',
            'design_life_years'
        ]
        
        for param in required_params:
            if param not in params:
                self.validation_errors.append(f"Missing parameter: {param}")
            else:
                value = params[param]
                if isinstance(value, (int, float)) and value <= 0:
                    self.validation_errors.append(f"Parameter must be positive: {param}")
                
        # Check sample points consistency
        if 'sample_points' in params:
            expected_points = int(params['sample_duration_seconds'] / params['time_step_seconds'])
            if abs(params['sample_points'] - expected_points) > 1:
                self.validation_warnings.append(
                    f"Sample points mismatch: specified={params['sample_points']}, "
                    f"expected={expected_points}"
                )
                
    def _validate_sn_curve(self, cfg):
        """Validate S-N curve parameters"""
        sn = cfg.get('sn_curve', {})
        
        if not sn:
            self.validation_errors.append("No S-N curve defined")
            return
            
        params = sn.get('parameters', {})
        if 'a' not in params or 'm' not in params:
            self.validation_errors.append("S-N curve missing 'a' or 'm' parameters")
            
        a_value = params.get('a', 0)
        m_value = params.get('m', 0)
        
        if isinstance(a_value, (int, float)) and a_value <= 0:
            self.validation_errors.append("S-N parameter 'a' must be positive")
            
        if isinstance(m_value, (int, float)) and m_value <= 0:
            self.validation_errors.append("S-N parameter 'm' must be positive")
            
    def _validate_tension_stress(self, cfg):
        """Validate tension-to-stress conversion"""
        ts = cfg.get('tension_to_stress', {})
        
        if not ts:
            self.validation_errors.append("No tension-to-stress conversion defined")
            return
            
        table = ts.get('conversion_table', [])
        if len(table) < 2:
            self.validation_errors.append(
                "Tension-to-stress table needs at least 2 points"
            )
            
        # Check monotonic increase
        tensions = [pt['tension_kN'] for pt in table]
        stresses = [pt['stress_MPa'] for pt in table]
        
        if tensions != sorted(tensions):
            self.validation_errors.append(
                "Tension values must be monotonically increasing"
            )
            
        if stresses != sorted(stresses):
            self.validation_warnings.append(
                "Stress values should be monotonically increasing"
            )
            
    def print_summary(self):
        """Print configuration summary"""
        if not self.config:
            print("No configuration loaded")
            return
            
        cfg = self.config.get('default', {})
        
        print("\n" + "="*60)
        print("CONFIGURATION SUMMARY")
        print("="*60)
        
        # Basic info
        print(f"\nAnalysis: {cfg.get('analysis_name', 'Unknown')}")
        print(f"Project: {cfg.get('project_id', 'Unknown')}")
        print(f"Structure: {cfg.get('structure', 'Unknown')}")
        
        # Parameters
        params = cfg.get('parameters', {})
        print(f"\nAnalysis Parameters:")
        print(f"  Sample Duration: {params.get('sample_duration_seconds', 0)} seconds")
        print(f"  Design Life: {params.get('design_life_years', 0)} years")
        
        # Fatigue conditions
        fc_data = cfg.get('fatigue_conditions', {}).get('data', {})
        print(f"\nFatigue Conditions: {len(fc_data)}")
        total_occ = sum(fc['occurrence_pct'] for fc in fc_data.values())
        print(f"  Total Occurrence: {total_occ:.1f}%")
        
        # S-N Curve
        sn = cfg.get('sn_curve', {})
        print(f"\nS-N Curve: {sn.get('curve_type', 'Unknown')}")
        params = sn.get('parameters', {})
        a_val = params.get('a', 0)
        m_val = params.get('m', 0)
        if isinstance(a_val, (int, float)):
            print(f"  a = {a_val:.1e}")
        else:
            print(f"  a = {a_val}")
        if isinstance(m_val, (int, float)):
            print(f"  m = {m_val:.1f}")
        else:
            print(f"  m = {m_val}")
        
        # Validation results
        print(f"\nValidation Results:")
        print(f"  Errors: {len(self.validation_errors)}")
        print(f"  Warnings: {len(self.validation_warnings)}")
        
        if self.validation_errors:
            print("\nERRORS:")
            for error in self.validation_errors:
                print(f"  - {error}")
                
        if self.validation_warnings:
            print("\nWARNINGS:")
            for warning in self.validation_warnings:
                print(f"  - {warning}")
                
    def get_parameter(self, param_path, default=None):
        """Get parameter value from config using dot notation
        
        Example: get_parameter('default.parameters.design_life_years')
        """
        if not self.config:
            return default
            
        keys = param_path.split('.')
        value = self.config
        
        for key in keys:
            if isinstance(value, dict) and key in value:
                value = value[key]
            else:
                return default
                
        return value
        

def main():
    """Main execution"""
    # Test with sample configuration
    config_file = Path("../input/master_input_config_sample.yml")
    
    loader = FatigueConfigLoader(config_file)
    
    try:
        # Load configuration
        config = loader.load_config()
        print(f"[SUCCESS] Configuration loaded")
        
        # Validate configuration
        is_valid = loader.validate_config()
        
        # Print summary
        loader.print_summary()
        
        if is_valid:
            print("\n[SUCCESS] Configuration is valid")
            
            # Example: Access specific parameters
            design_life = loader.get_parameter('default.parameters.design_life_years')
            print(f"\nDesign Life: {design_life} years")
            
            # Get S-N curve parameters
            a = loader.get_parameter('default.sn_curve.parameters.a')
            m = loader.get_parameter('default.sn_curve.parameters.m')
            if isinstance(a, (int, float)) and isinstance(m, (int, float)):
                print(f"S-N Curve: N = {a:.1e} × S^(-{m})")
            else:
                print(f"S-N Curve: N = {a} × S^(-{m})")
            
        else:
            print("\n[ERROR] Configuration has validation errors")
            return 1
            
    except Exception as e:
        print(f"\n[ERROR] Failed to load configuration: {e}")
        return 1
        
    return 0
    

if __name__ == "__main__":
    sys.exit(main())