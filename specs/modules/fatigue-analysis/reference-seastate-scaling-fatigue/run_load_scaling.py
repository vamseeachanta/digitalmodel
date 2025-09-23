#!/usr/bin/env python
"""
Test Runner for Load Scaling Program
=====================================
Script to test the load scaling program with the configured inputs.
"""

import sys
import os
from pathlib import Path

# Add src directory to path
project_root = Path(__file__).parent.parent.parent.parent.parent
sys.path.insert(0, str(project_root / 'src'))

from digitalmodel.modules.fatigue_analysis.load_scaling import LoadScalingProcessor


def main():
    """Run the load scaling analysis with the configured YAML file"""
    
    # Path to configuration file
    config_file = Path(__file__).parent / 'input' / 'load_scaling_config.yml'
    
    if not config_file.exists():
        print(f"Error: Configuration file not found: {config_file}")
        return 1
    
    print("="*60)
    print("LOAD SCALING ANALYSIS")
    print("="*60)
    print(f"Configuration: {config_file}")
    print()
    
    try:
        # Initialize and run processor
        processor = LoadScalingProcessor(str(config_file))
        processor.run()
        
        print()
        print("="*60)
        print("Analysis completed successfully!")
        print(f"  Output folder: {processor.config['output']['base_folder']}")
        print(f"  Cases processed: {len(processor.results)}")
        print("="*60)
        
        return 0
        
    except Exception as e:
        print(f"Analysis failed: {e}")
        return 1


if __name__ == "__main__":
    sys.exit(main())