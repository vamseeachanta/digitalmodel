#!/usr/bin/env python
"""
Production Rainflow Analysis Runner
Version: 1.1.0
Date: 2025-01-24

This script runs the rainflow analysis with production configuration.
It handles absolute paths properly for Windows environments.
"""

import sys
import os
from pathlib import Path

# Add the repository root to path
repo_root = Path(__file__).parent.parent.parent.parent.parent
sys.path.insert(0, str(repo_root))

def main():
    """Run production analysis"""
    # Import the main analyzer after path is set
    from specs.modules.signal_analysis.rainflow_with_visualization.run_rainflow_analysis import RainflowAnalyzer
    
    # Production config path
    config_file = Path(__file__).parent / 'input' / 'rainflow_analysis_config_production.yml'
    
    if not config_file.exists():
        print(f"Error: Configuration file not found: {config_file}")
        sys.exit(1)
    
    print("="*60)
    print("PRODUCTION RAINFLOW ANALYSIS")
    print("="*60)
    print(f"Config: {config_file}")
    print(f"Version: 1.1.0")
    print("="*60)
    
    # Check if input path exists
    input_path = Path(r"D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\output\scaled_tension")
    if not input_path.exists():
        print(f"Warning: Input path does not exist: {input_path}")
        response = input("Continue anyway? (y/n): ")
        if response.lower() != 'y':
            print("Aborted by user")
            sys.exit(1)
    
    # Check if output path exists
    output_path = Path(r"D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\output")
    if not output_path.exists():
        print(f"Creating output path: {output_path}")
        output_path.mkdir(parents=True, exist_ok=True)
    
    try:
        # Run the analyzer
        analyzer = RainflowAnalyzer(str(config_file))
        analyzer.run()
        
        print("\n" + "="*60)
        print("PRODUCTION ANALYSIS COMPLETED SUCCESSFULLY")
        print("="*60)
        
    except Exception as e:
        print(f"\nError during production analysis: {str(e)}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

if __name__ == "__main__":
    main()