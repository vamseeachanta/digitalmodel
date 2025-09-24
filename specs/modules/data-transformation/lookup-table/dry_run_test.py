"""
Dry Run Test - Validates configuration without writing output files
"""

import os
import sys
import pandas as pd
import yaml
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from test_process_transformation import TensionToStressTransformer

def dry_run():
    """Perform a dry run to show what files would be created."""
    
    print("="*60)
    print("DRY RUN - CONFIGURATION VALIDATION")
    print("="*60)
    
    # Load configuration
    config_file = "specs/modules/data-transformation/lookup-table/inputs/test_transformation_config.yaml"
    
    with open(config_file, 'r') as f:
        config = yaml.safe_load(f)
    
    # Display current configuration
    print("\n[CONFIGURATION SETTINGS]")
    print("-" * 40)
    print(f"Input folder:  {config['raw_data']['folder']}")
    print(f"File pattern:  {config['raw_data']['file_pattern']}")
    print(f"Output folder: {config['output']['folder']}")
    print(f"Output pattern: {config['output']['file_pattern']}")
    
    # Find input files
    input_folder = Path(config['raw_data']['folder'])
    input_pattern = config['raw_data']['file_pattern']
    input_files = list(input_folder.glob(input_pattern))
    
    print("\n[INPUT FILES FOUND]")
    print("-" * 40)
    for i, file in enumerate(input_files, 1):
        print(f"{i}. {file.name}")
    
    # Load lookup table to get location IDs
    lookup_file = config['transformation']['lookup_table']['file']
    lookup_df = pd.read_csv(lookup_file)
    lookup_df['config'] = lookup_df['config'].str.strip()
    location_ids = lookup_df['location ID'].unique()
    
    print(f"\n[LOCATION IDs TO PROCESS]")
    print("-" * 40)
    print(f"Location IDs: {sorted(location_ids)}")
    
    # Calculate expected output files
    print("\n[EXPECTED OUTPUT FILES]")
    print("-" * 40)
    print(f"Output directory: {config['output']['folder']}")
    print(f"\nFiles to be created:")
    
    total_files = 0
    for input_file in input_files:
        filename = input_file.name
        # Extract metadata from filename using simple parsing
        parts = filename.replace('_rainflow.csv', '').split('_')
        
        # Find FC and Strut indices
        fc_idx = next(i for i, p in enumerate(parts) if p.startswith('FC'))
        strut_idx = next(i for i, p in enumerate(parts) if p.startswith('Strut'))
        
        config_name = '_'.join(parts[:fc_idx])
        fc_number = parts[fc_idx][2:]
        strut_number = parts[strut_idx][5:]
        
        print(f"\n  From: {filename}")
        for loc_id in sorted(location_ids):
            output_name = config['output']['file_pattern'].format(
                config=config_name,
                fc_number=fc_number,
                strut_number=strut_number,
                location_id=int(loc_id)
            )
            print(f"    -> {output_name}")
            total_files += 1
    
    print("\n" + "="*60)
    print(f"SUMMARY: {len(input_files)} input files × {len(location_ids)} locations")
    print(f"         = {total_files} output files will be created")
    print("="*60)
    
    # Check if output directory exists
    output_dir = Path(config['output']['folder'])
    if not output_dir.exists():
        print(f"\n[NOTE] Output directory will be created: {output_dir}")
    else:
        print(f"\n[OK] Output directory exists: {output_dir}")
        existing_files = list(output_dir.glob("*.csv"))
        if existing_files:
            print(f"[WARNING] Directory contains {len(existing_files)} existing files")
    
    return config

def confirm_and_run():
    """Ask for confirmation and optionally run the actual transformation."""
    config = dry_run()
    
    print("\n" + "="*60)
    response = input("Do you want to proceed with the actual transformation? (yes/no): ")
    
    if response.lower() in ['yes', 'y']:
        print("\n[STARTING TRANSFORMATION]")
        print("-" * 40)
        
        config_file = "specs/modules/data-transformation/lookup-table/inputs/test_transformation_config.yaml"
        transformer = TensionToStressTransformer(config_file)
        transformer.run()
        
        print("\n[COMPLETE] Transformation finished!")
        
        # Check created files
        output_dir = Path(config['output']['folder'])
        created_files = list(output_dir.glob("*.csv"))
        print(f"[SUCCESS] Created {len(created_files)} files in {output_dir}")
    else:
        print("\n[CANCELLED] No files were created.")

if __name__ == "__main__":
    confirm_and_run()