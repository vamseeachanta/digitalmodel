#!/usr/bin/env python
"""Monitor progress of production rainflow analysis"""
import time
import os
from pathlib import Path

output_dir = Path("specs/modules/signal-analysis/rainflow_with_visualization/output")
viz_dir = Path("specs/modules/signal-analysis/rainflow_with_visualization/visualization")
total_files = 2592

print("Monitoring production rainflow analysis progress...")
print(f"Total files to process: {total_files}")
print("-" * 60)

start_time = time.time()

while True:
    # Count processed files
    rainflow_files = list(output_dir.glob("*_rainflow.csv")) if output_dir.exists() else []
    matrix_plots = list(viz_dir.glob("*_rainflow_matrix.png")) if viz_dir.exists() else []
    
    processed = len(rainflow_files)
    plots_generated = len(matrix_plots)
    
    # Calculate progress
    progress = (processed / total_files) * 100 if total_files > 0 else 0
    
    # Estimate time remaining
    elapsed_time = time.time() - start_time
    if processed > 0:
        avg_time_per_file = elapsed_time / processed
        remaining_files = total_files - processed
        est_remaining_time = avg_time_per_file * remaining_files
        
        # Format time
        hours = int(est_remaining_time // 3600)
        minutes = int((est_remaining_time % 3600) // 60)
        
        print(f"\rProgress: {processed}/{total_files} ({progress:.1f}%) | "
              f"Plots: {plots_generated} | "
              f"Est. remaining: {hours}h {minutes}m", end="")
    else:
        print(f"\rProgress: {processed}/{total_files} ({progress:.1f}%) | Starting...", end="")
    
    if processed >= total_files:
        print("\n\nAnalysis complete!")
        break
        
    time.sleep(10)  # Check every 10 seconds

print(f"\nTotal time: {elapsed_time/3600:.1f} hours")
print(f"Files processed: {processed}")
print(f"3D plots generated: {plots_generated}")