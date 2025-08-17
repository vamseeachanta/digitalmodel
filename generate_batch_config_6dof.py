#!/usr/bin/env python
"""
Generate batch configuration for all fsts_*vessel_statics_6dof.yml files.
This script dynamically finds all matching files and creates a batch configuration.
"""

import os
import yaml
from pathlib import Path
from datetime import datetime
import argparse

def find_matching_files(base_dir, pattern="fsts_*vessel_statics_6dof.yml"):
    """Find all files matching the pattern in the specified directory."""
    base_path = Path(base_dir)
    matching_files = sorted(base_path.glob(pattern))
    return [f.name for f in matching_files]

def parse_filename(filename):
    """Parse filename to extract metadata for description."""
    # Example: fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.yml
    parts = filename.replace('.yml', '').split('_')
    
    # Extract components
    loading = parts[1].upper()  # L015 or L095
    water_level = parts[2].upper()  # HWL, MWL, LWL
    capacity = parts[3].replace('km3', 'km3')  # 125km3 or 180km3
    
    # Determine port/starboard and bow
    if 'pb' in filename:
        side = "Port Bow"
    elif 'sb' in filename:
        side = "Starboard Bow"
    else:
        side = ""
    
    # Extract L number (l000 or l100)
    l_number = None
    for part in parts:
        if part.startswith('l') and part[1:].isdigit():
            l_number = part.upper()
            break
    
    description = f"FSTS {loading} {water_level} {capacity}"
    if l_number:
        description += f" {l_number}"
    if side:
        description += f" {side}"
    
    return description

def generate_batch_config(base_dir, output_file="batch_all_fsts_vessel_statics_6dof.yml"):
    """Generate batch configuration file for all matching models."""
    
    # Find all matching files
    model_files = find_matching_files(base_dir)
    
    if not model_files:
        print(f"No matching files found in {base_dir}")
        return False
    
    print(f"Found {len(model_files)} matching files")
    
    # Create models list with descriptions
    models = []
    for model_file in model_files:
        description = parse_filename(model_file)
        models.append({
            'model_file': model_file,
            'description': description
        })
        print(f"  - {model_file}: {description}")
    
    # Create batch configuration
    config = {
        'batch_info': {
            'name': 'FSTS LNGC All Vessel Statics 6DOF Batch Processing',
            'description': 'Batch processing for all FSTS vessel statics 6DOF models',
            'base_directory': str(base_dir),
            'output_directory': './output_batch_all_6dof',
            'timestamp': datetime.now().strftime('%Y-%m-%d')
        },
        'simulation_settings': {
            'analysis_type': 'statics',
            'calculate_statics': True,
            'save_simulation_file': True,
            'save_results': True,
            'continue_on_error': True,
            'parallel_processing': True,
            'max_workers': 30  # Use 30 workers for maximum parallel processing
        },
        'output_settings': {
            'save_csv': True,
            'save_simulation': True,
            'save_sim_in_model_directory': True,
            'csv_output_folder': 'csv',
            'sim_output_folder': 'sim',
            'include_summary_report': True
        },
        'models': models,
        'mooring_parameters': {
            'lines_to_check': [f'Line{i:02d}' for i in range(1, 19)],
            'tension_tolerance': 0.01,
            'section_to_modify': 2
        },
        'processing_options': {
            'max_iterations': 10,
            'damping_factor': 0.7,
            'create_backup': True,
            'generate_report': True,
            'log_level': 'INFO'
        }
    }
    
    # Write configuration to file
    output_path = Path(output_file)
    with open(output_path, 'w') as f:
        yaml.dump(config, f, default_flow_style=False, sort_keys=False)
    
    print(f"\nBatch configuration saved to: {output_path}")
    print(f"Total models to process: {len(models)}")
    
    return True

def main():
    parser = argparse.ArgumentParser(
        description='Generate batch configuration for FSTS vessel statics 6DOF models'
    )
    
    parser.add_argument(
        '--base-dir',
        default='D:/1522/ctr7/orcaflex/rev_a08/base_files/fsts_lngc_pretension',
        help='Base directory containing the model files'
    )
    
    parser.add_argument(
        '--output',
        default='batch_all_fsts_vessel_statics_6dof.yml',
        help='Output batch configuration file name'
    )
    
    parser.add_argument(
        '--pattern',
        default='fsts_*vessel_statics_6dof.yml',
        help='File pattern to match'
    )
    
    args = parser.parse_args()
    
    print(f"Searching for files in: {args.base_dir}")
    print(f"Pattern: {args.pattern}")
    print("-" * 60)
    
    success = generate_batch_config(args.base_dir, args.output)
    
    if success:
        print("\n" + "=" * 60)
        print("TO RUN THE BATCH PROCESSING:")
        print("-" * 60)
        print(f"python src/modules/orcaflex/mooring_tension_iteration/batch_processing/orcaflex_batch_runner.py \\")
        print(f"    --config {args.output}")
        print("=" * 60)

if __name__ == "__main__":
    main()