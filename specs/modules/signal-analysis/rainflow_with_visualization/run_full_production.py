#!/usr/bin/env python
"""
Full Production Rainflow Analysis Runner
Version: 1.2.0
Date: 2025-01-24

This script runs the complete rainflow analysis on all production files
with progress tracking and time estimation.
"""

import sys
import os
from pathlib import Path
import time
from datetime import datetime, timedelta

# Add the repository root to path
repo_root = Path(__file__).parent.parent.parent.parent.parent
sys.path.insert(0, str(repo_root))

def main():
    """Run full production analysis with progress tracking"""
    
    print("="*70)
    print("FULL PRODUCTION RAINFLOW ANALYSIS")
    print("="*70)
    print(f"Start Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print("="*70)
    
    # Production paths
    input_path = Path(r"D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\output\scaled_tension")
    output_path = Path(r"D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\output")
    config_file = Path(__file__).parent / 'input' / 'rainflow_analysis_config_production.yml'
    
    # Check paths
    if not input_path.exists():
        print(f"ERROR: Input path does not exist: {input_path}")
        sys.exit(1)
        
    # Count input files
    input_files = list(input_path.glob("*_scaled_tension.csv"))
    num_files = len(input_files)
    
    print(f"Input Directory: {input_path}")
    print(f"Output Directory: {output_path}")
    print(f"Configuration: {config_file.name}")
    print(f"Files to Process: {num_files}")
    print("="*70)
    
    if num_files == 0:
        print("ERROR: No input files found!")
        sys.exit(1)
    
    # Estimate processing time (based on ~5.4 seconds per file from test)
    estimated_time = num_files * 5.4  # seconds
    estimated_minutes = estimated_time / 60
    estimated_finish = datetime.now() + timedelta(seconds=estimated_time)
    
    print(f"Estimated Processing Time: {estimated_minutes:.1f} minutes")
    print(f"Estimated Completion: {estimated_finish.strftime('%H:%M:%S')}")
    print("="*70)
    
    # Ask for confirmation
    response = input(f"\nProceed with processing {num_files} files? (y/n): ")
    if response.lower() != 'y':
        print("Analysis cancelled by user.")
        sys.exit(0)
    
    print("\nStarting analysis...")
    print("="*70)
    
    # Import and run the analyzer
    try:
        # Direct import from the same directory
        sys.path.insert(0, str(Path(__file__).parent))
        from run_rainflow_analysis import RainflowAnalyzer
        
        start_time = time.time()
        
        # Initialize analyzer
        analyzer = RainflowAnalyzer(str(config_file))
        
        # Run analysis (the analyzer will handle progress internally)
        analyzer.run()
        
        # Calculate actual time
        end_time = time.time()
        total_time = end_time - start_time
        total_minutes = total_time / 60
        
        print("\n" + "="*70)
        print("PRODUCTION ANALYSIS COMPLETED SUCCESSFULLY")
        print("="*70)
        print(f"Total Files Processed: {num_files}")
        print(f"Total Processing Time: {total_minutes:.1f} minutes")
        print(f"Average Time per File: {total_time/num_files:.1f} seconds")
        print(f"End Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print("="*70)
        
        # Output locations
        print("\nOutput Files Located In:")
        print(f"  Rainflow Results: {output_path}/rainflow/")
        print(f"  FFT Results: {output_path}/rainflow/")
        print(f"  Visualizations: {output_path}/visualization/")
        print(f"  Summary Report: {output_path}/rainflow/rainflow_analysis_summary.csv")
        print("="*70)
        
    except Exception as e:
        print(f"\nERROR during production analysis: {str(e)}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

if __name__ == "__main__":
    main()