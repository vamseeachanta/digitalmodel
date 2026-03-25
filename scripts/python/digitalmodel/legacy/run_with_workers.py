#!/usr/bin/env python
"""
Example script showing how to run digitalmodel with max_workers configuration
"""

import sys
import json
from digitalmodel.engine import engine

# Example 1: Using YAML configuration file
def run_with_yaml_config():
    """Run using a YAML configuration file with max_workers"""
    config_file = "dm_fsts_lngc.yml"
    result = engine(config_file)
    return result

# Example 2: Using dictionary configuration
def run_with_dict_config():
    """Run using a dictionary configuration with max_workers"""
    cfg = {
        'meta': {
            'label': 'fsts_l015_lwl_180km3_l100_pb',
            'basename': 'orcaflex_post_process'
        },
        'file_management': {
            'input_directory': '../02c_005yr',
            'output_directory': '../output/csv/02c_005yr',
            'filename': {
                'pattern': 'fsts_l015_lwl_180km3_l100_pb'
            }
        },
        'parallel_processing': {
            'enabled': True,
            'max_workers': 8,  # Specify number of workers
            'task_type': 'mixed',
            'auto_optimize': True,
            'timeout_per_file': 3600,
            'save_error_reports': True
        }
    }
    result = engine(cfg=cfg)
    return result

# Example 3: Override max_workers via command line
def run_with_override():
    """Run with command line override of max_workers"""
    import subprocess
    
    # Command line override syntax
    override_dict = {
        'parallel_processing': {
            'max_workers': 16
        }
    }
    
    cmd = [
        sys.executable, 
        '-m', 
        'digitalmodel',
        'dm_fsts_lngc.yml',
        json.dumps(override_dict)
    ]
    
    result = subprocess.run(cmd, capture_output=True, text=True)
    print(result.stdout)
    if result.stderr:
        print("Errors:", result.stderr)
    return result.returncode

if __name__ == "__main__":
    print("Digital Model - max_workers Configuration Examples")
    print("=" * 50)
    
    if len(sys.argv) > 1:
        if sys.argv[1] == "yaml":
            print("Running with YAML configuration...")
            run_with_yaml_config()
        elif sys.argv[1] == "dict":
            print("Running with dictionary configuration...")
            run_with_dict_config()
        elif sys.argv[1] == "override":
            print("Running with command line override...")
            run_with_override()
    else:
        print("Usage:")
        print("  python run_with_workers.py yaml     # Use YAML config")
        print("  python run_with_workers.py dict     # Use dict config")
        print("  python run_with_workers.py override # Use CLI override")
        print("\nDirect command line usage:")
        print('  python -m digitalmodel dm_fsts_lngc.yml "{\\"parallel_processing\\": {\\"max_workers\\": 8}}"')