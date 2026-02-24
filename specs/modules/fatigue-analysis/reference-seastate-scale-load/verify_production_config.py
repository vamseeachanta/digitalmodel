#!/usr/bin/env python
"""
Production Configuration Verification Script
============================================
Checks if the production configuration will run successfully.
"""

import sys
import os
from pathlib import Path
import yaml
import pandas as pd

def check_production_config():
    """Verify production configuration file and all dependencies"""
    
    print("="*60)
    print("PRODUCTION CONFIGURATION VERIFICATION")
    print("="*60)
    
    config_file = Path("input/load_scaling_config_production.yml")
    issues = []
    warnings = []
    
    # 1. Check config file exists
    print("\n1. Checking configuration file...")
    if not config_file.exists():
        issues.append(f"Configuration file not found: {config_file}")
        print(f"   [X] Configuration file not found")
        return False
    else:
        print(f"   [OK] Configuration file exists: {config_file}")
    
    # 2. Load and validate YAML
    print("\n2. Loading configuration...")
    try:
        with open(config_file, 'r') as f:
            config = yaml.safe_load(f)
        print(f"   [OK] Valid YAML format")
    except Exception as e:
        issues.append(f"Invalid YAML: {e}")
        print(f"   [X] Invalid YAML: {e}")
        return False
    
    # 3. Check input files
    print("\n3. Checking input files...")
    
    # Reference metadata
    ref_meta = config['input_data']['reference_seastate']['metadata_file']
    if not Path(ref_meta).exists():
        issues.append(f"Reference metadata not found: {ref_meta}")
        print(f"   [X] Reference metadata not found: {ref_meta}")
    else:
        print(f"   [OK] Reference metadata found: {ref_meta}")
        # Load and check
        ref_df = pd.read_csv(ref_meta)
        print(f"       - Contains {len(ref_df)} reference definitions")
    
    # Fatigue seastates
    fatigue_file = config['input_data']['fatigue_seastates']['metadata_file']
    if not Path(fatigue_file).exists():
        issues.append(f"Fatigue seastates not found: {fatigue_file}")
        print(f"   [X] Fatigue seastates not found: {fatigue_file}")
    else:
        print(f"   [OK] Fatigue seastates found: {fatigue_file}")
        # Load and check
        fatigue_df = pd.read_csv(fatigue_file)
        print(f"       - Contains {len(fatigue_df)} fatigue conditions")
        
        # Check occurrence sum
        total_occ = fatigue_df['Occurrence (%)'].sum()
        if abs(total_occ - 100.0) > 0.1:
            warnings.append(f"Occurrence sum = {total_occ:.2f}% (expected 100%)")
            print(f"   [!] WARNING: Occurrence sum = {total_occ:.2f}% (expected 100%)")
        else:
            print(f"   [OK] Occurrence sum = {total_occ:.2f}%")
    
    # 4. Check reference data folder
    print("\n4. Checking reference data folder...")
    ref_folder = Path(config['input_data']['reference_seastate']['data_folder'])
    
    if not ref_folder.exists():
        issues.append(f"Reference data folder not found: {ref_folder}")
        print(f"   [X] Reference data folder not found: {ref_folder}")
    else:
        print(f"   [OK] Reference data folder exists: {ref_folder}")
        
        # Count CSV files
        csv_files = list(ref_folder.glob("*.csv"))
        print(f"       - Contains {len(csv_files)} CSV files")
        
        # Check file naming pattern
        if csv_files:
            sample_file = csv_files[0].name
            print(f"       - Sample file: {sample_file}")
            
            # Check if files match expected pattern
            expected_pattern = config['input_data']['reference_seastate'].get('file_pattern', '')
            if expected_pattern:
                # Check for expected naming components
                configs = config['input_data']['vessel_configurations']['configs']
                found_configs = []
                
                for cfg in configs:
                    cfg_id = cfg['id']
                    matching = [f for f in csv_files if cfg_id in f.name]
                    if matching:
                        found_configs.append(cfg_id)
                        print(f"       - Found files for config: {cfg_id} ({len(matching)} files)")
                    else:
                        warnings.append(f"No files found for config: {cfg_id}")
                        print(f"   [!] WARNING: No files found for config: {cfg_id}")
    
    # 5. Check output folder permissions
    print("\n5. Checking output folder...")
    output_folder = Path(config['output']['base_folder'])
    
    # Try to create if doesn't exist
    try:
        output_folder.mkdir(parents=True, exist_ok=True)
        print(f"   [OK] Output folder accessible: {output_folder}")
    except Exception as e:
        issues.append(f"Cannot create output folder: {output_folder} - {e}")
        print(f"   [X] Cannot create output folder: {output_folder}")
    
    # 6. Check vessel configurations
    print("\n6. Checking vessel configurations...")
    configs = config['input_data']['vessel_configurations']['configs']
    struts = config['input_data']['vessel_configurations']['struts']
    
    print(f"   [OK] {len(configs)} vessel configurations defined")
    for cfg in configs:
        print(f"       - {cfg['id']}: {cfg['name']}")
    
    print(f"   [OK] {len(struts)} struts defined: {struts}")
    
    # 7. Calculate expected outputs
    print("\n7. Expected Processing Summary:")
    if 'fatigue_df' in locals() and 'configs' in locals() and 'struts' in locals():
        total_cases = len(fatigue_df) * len(configs) * len(struts)
        print(f"   - Fatigue conditions: {len(fatigue_df)}")
        print(f"   - Vessel configs: {len(configs)}")
        print(f"   - Struts: {len(struts)}")
        print(f"   - Total load cases: {total_cases}")
        print(f"   - Expected output files: {total_cases + 1} (including summary)")
    
    # 8. Report results
    print("\n" + "="*60)
    print("VERIFICATION SUMMARY")
    print("="*60)
    
    if issues:
        print("\nCRITICAL ISSUES FOUND:")
        for issue in issues:
            print(f"  [X] {issue}")
    
    if warnings:
        print("\nWARNINGS:")
        for warning in warnings:
            print(f"  [!] {warning}")
    
    if not issues:
        print("\n[PASSED] Production configuration is ready to run!")
        print("\nTo run the analysis:")
        print("  python run_load_scaling.py input/load_scaling_config_production.yml")
        return True
    else:
        print("\n[FAILED] Please fix the critical issues before running.")
        return False

def check_reference_file_naming():
    """Additional check for reference file naming convention"""
    
    print("\n" + "="*60)
    print("REFERENCE FILE NAMING ANALYSIS")
    print("="*60)
    
    # Load config
    config_file = Path("input/load_scaling_config_production.yml")
    with open(config_file, 'r') as f:
        config = yaml.safe_load(f)
    
    ref_folder = Path(config['input_data']['reference_seastate']['data_folder'])
    
    if ref_folder.exists():
        csv_files = list(ref_folder.glob("*.csv"))
        
        # Analyze naming patterns
        print(f"\nAnalyzing {len(csv_files)} files...")
        
        # Group by patterns
        patterns = {}
        for f in csv_files[:10]:  # Sample first 10
            # Extract components
            name_parts = f.stem.split('_')
            pattern = f"Parts: {len(name_parts)}, Prefix: {name_parts[0] if name_parts else 'none'}"
            patterns[pattern] = patterns.get(pattern, 0) + 1
            
        print("\nNaming patterns found:")
        for pattern, count in patterns.items():
            print(f"  - {pattern}: {count} files")
        
        # Check for expected strut files
        print("\nChecking for Strut files:")
        strut_files = [f for f in csv_files if 'strut' in f.name.lower()]
        print(f"  - Found {len(strut_files)} files with 'Strut' in name")
        
        if not strut_files:
            print("\n[!] WARNING: No files with 'Strut' in filename found.")
            print("    The reference data may need to be renamed or")
            print("    the program may need adjustment for the naming convention.")
            
            # Show actual file examples
            print("\nActual file examples:")
            for f in csv_files[:5]:
                print(f"    - {f.name}")

if __name__ == "__main__":
    # Run verification
    success = check_production_config()
    
    # Additional naming analysis
    check_reference_file_naming()
    
    sys.exit(0 if success else 1)