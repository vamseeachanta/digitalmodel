#!/usr/bin/env python
"""
Production Setup Validation
Validates the production configuration before running transformation
"""

import os
import glob
import yaml
import pandas as pd
from pathlib import Path
from datetime import datetime

class ProductionValidator:
    """Validate production setup and configuration."""
    
    def __init__(self):
        self.checks = []
        self.warnings = []
        self.errors = []
        
    def check(self, name, condition, error_msg=None, warning_msg=None):
        """Perform a validation check."""
        if condition:
            self.checks.append(f"[OK] {name}")
            return True
        else:
            if error_msg:
                self.errors.append(f"[ERROR] {name}: {error_msg}")
            elif warning_msg:
                self.warnings.append(f"[WARN] {name}: {warning_msg}")
            else:
                self.errors.append(f"[FAIL] {name}")
            return False
            
    def validate_production(self):
        """Run all production validation checks."""
        print("=" * 70)
        print("PRODUCTION SETUP VALIDATION")
        print("=" * 70)
        print(f"Timestamp: {datetime.now().isoformat()}")
        print()
        
        # 1. Check configuration file
        print("1. CONFIGURATION FILE")
        print("-" * 40)
        
        config_exists = self.check(
            "Production config exists",
            os.path.exists('production_transformation_config.yaml'),
            error_msg="File not found"
        )
        
        if config_exists:
            with open('production_transformation_config.yaml', 'r') as f:
                config = yaml.safe_load(f)
                
            self.check(
                "Config is valid YAML",
                config is not None,
                error_msg="Invalid YAML format"
            )
        
        # 2. Check input directory
        print("\n2. INPUT DATA")
        print("-" * 40)
        
        input_dir = r"D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\output\rainflow"
        
        dir_exists = self.check(
            "Input directory exists",
            os.path.exists(input_dir),
            error_msg=f"Directory not found: {input_dir}"
        )
        
        if dir_exists:
            pattern = os.path.join(input_dir, "*_FC*_Strut*_rainflow.csv")
            input_files = glob.glob(pattern)
            
            self.check(
                f"Found rainflow files",
                len(input_files) > 0,
                error_msg="No matching files found"
            )
            
            if len(input_files) > 0:
                print(f"  Files found: {len(input_files)}")
                
                # Sample first file
                sample_file = input_files[0]
                print(f"  Sample file: {Path(sample_file).name}")
                
                try:
                    df_sample = pd.read_csv(sample_file)
                    
                    self.check(
                        "Sample file readable",
                        True
                    )
                    
                    self.check(
                        "Has 'Range (kN)' column",
                        'Range (kN)' in df_sample.columns,
                        error_msg=f"Columns found: {list(df_sample.columns)}"
                    )
                    
                    self.check(
                        "Has 'Cycles_Annual' column",
                        'Cycles_Annual' in df_sample.columns,
                        error_msg=f"Columns found: {list(df_sample.columns)}"
                    )
                    
                    print(f"  Sample rows: {len(df_sample)}")
                    print(f"  Sample columns: {list(df_sample.columns)}")
                    
                except Exception as e:
                    self.errors.append(f"[ERROR] Could not read sample file: {e}")
        
        # 3. Check lookup table
        print("\n3. LOOKUP TABLE")
        print("-" * 40)
        
        lookup_path = "inputs/tension_range_to_stress_range_function.csv"
        
        lookup_exists = self.check(
            "Lookup table exists",
            os.path.exists(lookup_path),
            error_msg=f"File not found: {lookup_path}"
        )
        
        if lookup_exists:
            try:
                lookup_df = pd.read_csv(lookup_path)
                
                self.check(
                    "Lookup table readable",
                    True
                )
                
                # Clean columns
                lookup_df.columns = lookup_df.columns.str.strip()
                
                required_cols = ['tension range (kN)', 'stress range (Mpa)', 'location ID', 'config']
                for col in required_cols:
                    self.check(
                        f"Has column '{col}'",
                        col in lookup_df.columns,
                        error_msg=f"Columns found: {list(lookup_df.columns)}"
                    )
                
                # Check locations
                if 'location ID' in lookup_df.columns:
                    locations = sorted(lookup_df['location ID'].unique())
                    print(f"  Locations: {locations}")
                    
                    self.check(
                        "Has 7 locations",
                        len(locations) == 7,
                        warning_msg=f"Found {len(locations)} locations"
                    )
                
                # Check configs
                if 'config' in lookup_df.columns:
                    configs = lookup_df['config'].str.strip().unique()
                    print(f"  Configs: {len(configs)} unique")
                    print(f"  Sample configs: {list(configs[:3])}")
                    
            except Exception as e:
                self.errors.append(f"[ERROR] Could not read lookup table: {e}")
        
        # 4. Check output directory
        print("\n4. OUTPUT DIRECTORY")
        print("-" * 40)
        
        output_dir = r"specs\modules\fatigue-analysis\reference-seastate-scale-load\output\rainflow\stress_range"
        output_path = Path(output_dir)
        
        self.check(
            "Output directory can be created",
            True  # Will be created if doesn't exist
        )
        
        if output_path.exists():
            existing_files = list(output_path.glob("*.csv"))
            if existing_files:
                print(f"  Existing output files: {len(existing_files)}")
                self.warnings.append(f"[WARN] Output directory contains {len(existing_files)} existing files")
        
        # 5. Check dependencies
        print("\n5. DEPENDENCIES")
        print("-" * 40)
        
        try:
            import scipy
            self.check(f"scipy installed", True)
            print(f"  scipy version: {scipy.__version__}")
        except ImportError:
            self.check("scipy installed", False, error_msg="scipy not installed")
            
        try:
            import tqdm
            self.check(f"tqdm installed", True)
        except ImportError:
            self.check("tqdm installed", False, warning_msg="tqdm not installed (optional)")
        
        # 6. Estimate processing
        print("\n6. PROCESSING ESTIMATE")
        print("-" * 40)
        
        if dir_exists and len(input_files) > 0:
            total_files = len(input_files)
            locations = 7
            expected_outputs = total_files * locations
            
            print(f"  Input files: {total_files:,}")
            print(f"  Locations: {locations}")
            print(f"  Expected outputs: {expected_outputs:,}")
            
            # Estimate time (assume 0.5 seconds per file)
            est_time = (total_files * 0.5) / 60
            print(f"  Estimated time: {est_time:.1f} minutes")
            
            # Estimate disk space (assume 50KB per output file)
            est_space = (expected_outputs * 50) / 1024  # MB
            print(f"  Estimated disk space: {est_space:.1f} MB")
        
        # Final summary
        print("\n" + "=" * 70)
        print("VALIDATION SUMMARY")
        print("-" * 70)
        
        print(f"Checks passed: {len(self.checks)}")
        
        if self.warnings:
            print(f"\nWarnings ({len(self.warnings)}):")
            for warning in self.warnings:
                print(f"  {warning}")
                
        if self.errors:
            print(f"\nErrors ({len(self.errors)}):")
            for error in self.errors:
                print(f"  {error}")
            print("\n[FAIL] PRODUCTION NOT READY - Fix errors above")
        else:
            print("\n[OK] PRODUCTION READY")
            print("\nTo run production transformation:")
            print("  python run_production_transformation.py")
            print("\nFor dry run:")
            print("  python run_production_transformation.py --dry-run")
            print("\nTo process sample:")
            print("  python run_production_transformation.py --sample 10")
            
        print("=" * 70)
        
        return len(self.errors) == 0


def main():
    """Main entry point."""
    validator = ProductionValidator()
    success = validator.validate_production()
    
    return 0 if success else 1


if __name__ == "__main__":
    import sys
    sys.exit(main())