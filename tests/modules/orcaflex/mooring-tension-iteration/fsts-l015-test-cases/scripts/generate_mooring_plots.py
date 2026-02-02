#!/usr/bin/env python
"""
Generate mooring force and stiffness visualization plots for the go-by example.

This script reads the analysis CSV files and creates comprehensive visualizations
for mooring forces and stiffness characteristics.
"""

import os
import sys
import pandas as pd
from pathlib import Path

# Add parent directory to path for imports
repo_root = Path(__file__).resolve().parents[5]
sys.path.insert(0, str(repo_root))

from digitalmodel.orcaflex.visualization import MooringVisualization


def main():
    """Generate mooring visualization plots."""
    
    # Define paths
    base_dir = Path(__file__).parent.parent
    csv_dir = base_dir / 'output' / 'collate' / 'csv'
    plots_dir = base_dir / 'output' / 'plots'
    
    # Create plots directory
    plots_dir.mkdir(parents=True, exist_ok=True)
    
    # File paths
    pretension_file = csv_dir / 'fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv'
    stiffness_analysis_file = csv_dir / 'fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_mooring_stiffness_analysis.csv'
    stiffness_summary_file = csv_dir / 'fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_mooring_stiffness_summary.csv'
    
    # Check if files exist
    if not pretension_file.exists():
        print(f"Error: Pretension analysis file not found: {pretension_file}")
        return 1
    
    # Load data
    print(f"Loading pretension data from: {pretension_file}")
    pretension_df = pd.read_csv(pretension_file)
    
    # Load stiffness data if available
    stiffness_df = None
    stiffness_analysis_df = None
    
    if stiffness_summary_file.exists():
        print(f"Loading stiffness summary from: {stiffness_summary_file}")
        stiffness_df = pd.read_csv(stiffness_summary_file)
    
    if stiffness_analysis_file.exists():
        print(f"Loading stiffness analysis from: {stiffness_analysis_file}")
        stiffness_analysis_df = pd.read_csv(stiffness_analysis_file)
        
        # Add force columns if not present (for correlation plots)
        if 'Fx' not in stiffness_analysis_df.columns:
            stiffness_analysis_df['Fx'] = pretension_df['end_Gx_force'].values
            stiffness_analysis_df['Fy'] = pretension_df['end_Gy_force'].values
            stiffness_analysis_df['Fz'] = pretension_df['end_Gz_force'].values
    
    # Initialize visualization module
    print(f"\nCreating visualizations in: {plots_dir}")
    viz = MooringVisualization(output_directory=str(plots_dir))
    
    # Generate force visualization
    print("\nGenerating mooring force plots...")
    force_fig = viz.plot_mooring_forces(
        pretension_df, 
        stiffness_df, 
        save_name='mooring_forces_visualization'
    )
    
    # Generate stiffness visualization if data available
    if stiffness_analysis_df is not None and stiffness_df is not None:
        print("\nGenerating stiffness characteristic plots...")
        stiffness_fig = viz.plot_stiffness_characteristics(
            stiffness_analysis_df,
            stiffness_df,
            save_name='mooring_stiffness_visualization'
        )
    else:
        print("\nSkipping stiffness plots - data not available")
    
    print("\n" + "="*60)
    print("VISUALIZATION COMPLETE")
    print("="*60)
    print(f"Output directory: {plots_dir}")
    print("\nGenerated files:")
    for plot_file in plots_dir.glob('*.png'):
        print(f"  - {plot_file.name}")
    
    # Summary statistics
    print("\n" + "-"*60)
    print("MOORING SYSTEM SUMMARY")
    print("-"*60)
    
    # Force summary
    total_fx = pretension_df['end_Gx_force'].sum()
    total_fy = pretension_df['end_Gy_force'].sum()
    total_fz = pretension_df['end_Gz_force'].sum()
    
    print(f"\nNet Forces:")
    print(f"  Fx (surge): {total_fx:8.2f} kN")
    print(f"  Fy (sway):  {total_fy:8.2f} kN")
    print(f"  Fz (heave): {total_fz:8.2f} kN")
    
    # Directional force breakdown
    fx_positive = pretension_df[pretension_df['end_Gx_force'] > 0]['end_Gx_force'].sum()
    fx_negative = pretension_df[pretension_df['end_Gx_force'] < 0]['end_Gx_force'].sum()
    
    print(f"\nDirectional Forces (X):")
    print(f"  Positive (+X): {fx_positive:8.2f} kN")
    print(f"  Negative (-X): {fx_negative:8.2f} kN")
    print(f"  Net:          {fx_positive + fx_negative:8.2f} kN")
    
    # Stiffness summary if available
    if stiffness_df is not None and not stiffness_df.empty:
        summary = stiffness_df.iloc[0]
        print(f"\nSystem Stiffness:")
        print(f"  Kxx (total):    {summary.get('K_xx_total', 0):8.2f} kN/m")
        print(f"  Kxx (positive): {summary.get('K_xx_positive', 0):8.2f} kN/m")
        print(f"  Kxx (negative): {summary.get('K_xx_negative', 0):8.2f} kN/m")
        print(f"  Kxx (net):      {summary.get('K_xx_net', 0):8.2f} kN/m")
        print(f"  Kyy:           {summary.get('K_yy_total', 0):8.2f} kN/m")
        print(f"  Kzz:           {summary.get('K_zz_total', 0):8.2f} kN/m")
        
        print(f"\nNatural Periods:")
        print(f"  Surge: {summary.get('T_surge', 0):6.2f} s")
        print(f"  Sway:  {summary.get('T_sway', 0):6.2f} s")
        print(f"  Heave: {summary.get('T_heave', 0):6.2f} s")
    
    # Tension convergence status
    if 'tension_diff_percent' in pretension_df.columns:
        max_diff = pretension_df['tension_diff_percent'].abs().max()
        unconverged = pretension_df[pretension_df['tension_diff_percent'].abs() > 10]
        
        print(f"\nTension Convergence:")
        print(f"  Max difference: {max_diff:.1f}%")
        print(f"  Unconverged lines: {len(unconverged)} of {len(pretension_df)}")
        
        if len(unconverged) > 0:
            print(f"\n  Lines requiring iteration:")
            for _, row in unconverged.iterrows():
                print(f"    - {row['ObjectName']}: {row['tension_diff_percent']:.1f}% "
                      f"(Target: {row['target_tension']:.1f} kN, "
                      f"Actual: {row['current_tension']:.1f} kN)")
    
    print("\n" + "="*60)
    
    return 0


if __name__ == '__main__':
    sys.exit(main())