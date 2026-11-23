#!/usr/bin/env python
"""
Standalone verification script for OrcaFlex configuration files.
Tests configurations without requiring the full digitalmodel engine.
"""

import os
import yaml
from pathlib import Path
from typing import Dict, Any, List


def verify_configuration(config_path: str) -> Dict[str, Any]:
    """Verify a configuration file meets all requirements."""
    results = {
        "path": config_path,
        "valid": True,
        "errors": [],
        "warnings": [],
        "info": []
    }
    
    # Load configuration
    try:
        with open(config_path, 'r') as f:
            config = yaml.safe_load(f)
    except Exception as e:
        results["valid"] = False
        results["errors"].append(f"Failed to load YAML: {e}")
        return results
    
    # Check mandatory sections
    required_sections = ['meta', 'default', 'orcaflex', 'file_management']
    for section in required_sections:
        if section not in config:
            results["valid"] = False
            results["errors"].append(f"Missing required section: {section}")
    
    if not results["valid"]:
        return results
    
    # Verify meta section
    meta = config.get('meta', {})
    if meta.get('library') != 'digitalmodel':
        results["errors"].append("meta.library must be 'digitalmodel'")
        results["valid"] = False
    
    if 'basename' not in meta:
        results["errors"].append("meta.basename is required")
        results["valid"] = False
    
    results["info"].append(f"Description: {meta.get('description', 'No description')}")
    
    # Verify units in default section
    default = config.get('default', {})
    if 'units' not in default:
        results["warnings"].append("default.units section is missing")
    else:
        units = default['units']
        essential_units = ['effective_tension', 'length', 'time']
        for unit in essential_units:
            if unit not in units:
                results["warnings"].append(f"Missing recommended unit: {unit}")
    
    # Check file management
    file_mgmt = config.get('file_management', {})
    if not file_mgmt.get('flag', False):
        results["warnings"].append("file_management is disabled")
    
    # Check for input files
    input_dir = Path(file_mgmt.get('input_directory', '.'))
    if not input_dir.is_absolute():
        input_dir = Path(config_path).parent / input_dir
    
    input_files = []
    
    # Check specific files
    specific_files = file_mgmt.get('input_files', {})
    if specific_files:
        for file_type, file_list in specific_files.items():
            if isinstance(file_list, list):
                for filename in file_list:
                    file_path = input_dir / filename
                    input_files.append((filename, file_path.exists()))
    
    # Report file status
    existing = [f for f, exists in input_files if exists]
    missing = [f for f, exists in input_files if not exists]
    
    if existing:
        results["info"].append(f"Found {len(existing)} input files: {', '.join(existing)}")
    if missing:
        results["warnings"].append(f"Missing {len(missing)} input files: {', '.join(missing)}")
    
    # Check analysis configuration
    analysis = config.get('orcaflex', {}).get('analysis', {})
    if analysis.get('static'):
        results["info"].append("Static analysis: ENABLED")
    if analysis.get('dynamic'):
        results["info"].append(f"Dynamic analysis: ENABLED ({analysis.get('simulation_duration', 'unknown')}s)")
    
    # Check post-processing
    postprocess = config.get('orcaflex', {}).get('postprocess', {})
    enabled_post = []
    for key in ['visualization', 'summary', 'range_graph', 'time_series']:
        if postprocess.get(key, {}).get('flag', False):
            enabled_post.append(key)
    
    if enabled_post:
        results["info"].append(f"Post-processing: {', '.join(enabled_post)}")
    
    return results


def main():
    """Verify all configuration files."""
    print("="*70)
    print("OrcaFlex Configuration Verification")
    print("="*70)
    
    config_files = [
        "run_orcaflex_analysis.yml",
        "run_orcaflex_simple.yml",
        "run_orcaflex_batch.yml"
    ]
    
    all_valid = True
    
    for config_file in config_files:
        config_path = Path(__file__).parent / config_file
        
        if not config_path.exists():
            print(f"\n[MISSING] {config_file}: FILE NOT FOUND")
            all_valid = False
            continue
        
        print(f"\n[FILE] Verifying: {config_file}")
        print("-"*50)
        
        results = verify_configuration(str(config_path))
        
        # Display results
        if results["valid"]:
            print("[OK] VALID CONFIGURATION")
        else:
            print("[ERROR] INVALID CONFIGURATION")
            all_valid = False
        
        if results["errors"]:
            print("\n[ERRORS]:")
            for error in results["errors"]:
                print(f"   - {error}")
        
        if results["warnings"]:
            print("\n[WARNINGS]:")
            for warning in results["warnings"]:
                print(f"   - {warning}")
        
        if results["info"]:
            print("\n[INFO]:")
            for info in results["info"]:
                print(f"   - {info}")
    
    # Summary
    print("\n" + "="*70)
    if all_valid:
        print("[SUCCESS] All configurations are valid and ready to use!")
    else:
        print("[WARNING] Some configurations have issues. Please review the errors above.")
    print("="*70)
    
    # Test file discovery
    print("\n[FILES] Available OrcaFlex Files in Directory:")
    print("-"*50)
    
    test_dir = Path(__file__).parent
    for ext in ['*.yml', '*.sim', '*.dat']:
        files = list(test_dir.glob(ext))
        if files:
            print(f"\n{ext} files:")
            for f in files:
                # Skip our config files
                if not f.name.startswith('run_orcaflex'):
                    print(f"   - {f.name}")
    
    return all_valid


if __name__ == "__main__":
    import sys
    valid = main()
    sys.exit(0 if valid else 1)