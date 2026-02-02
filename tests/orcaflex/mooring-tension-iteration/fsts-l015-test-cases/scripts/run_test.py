#!/usr/bin/env python
"""
Run mooring tension iteration tests with updated folder structure.

This script coordinates all the test steps with the new directory organization:
- config/ for YAML configuration files
- run_files/dat/ for .dat input files  
- run_files/sim/ for .sim output files
- output/ for analysis results
"""

import os
import sys
import subprocess
from pathlib import Path
import time

def run_command(cmd, description=None):
    """Run a command and capture output."""
    if description:
        print(f"\n{description}")
    print(f"Running: {' '.join(cmd) if isinstance(cmd, list) else cmd}")
    
    try:
        result = subprocess.run(
            cmd,
            shell=True if isinstance(cmd, str) else False,
            capture_output=True,
            text=True
        )
        if result.stdout:
            print(result.stdout)
        if result.stderr:
            print(f"Warnings/Info: {result.stderr}")
        if result.returncode != 0:
            print(f"Command exited with code {result.returncode}")
        return result.returncode == 0
    except Exception as e:
        print(f"Error running command: {e}")
        return False

def main():
    """Run the mooring tension iteration test sequence."""
    
    print("=" * 60)
    print("Mooring Tension Iteration Test Runner")
    print("=" * 60)
    
    # Set up paths
    script_dir = Path(__file__).parent
    base_dir = script_dir.parent
    config_dir = script_dir / "config"
    run_files_dir = base_dir / "run_files"
    dat_dir = run_files_dir / "dat"
    sim_dir = run_files_dir / "sim"
    output_dir = base_dir / "output"
    
    # Python executable
    if sys.platform == "win32":
        python_exe = Path("D:/github/digitalmodel/.venv/Scripts/python.exe")
    else:
        python_exe = Path("/d/github/digitalmodel/.venv/Scripts/python")
    
    if not python_exe.exists():
        print(f"Error: Python executable not found at {python_exe}")
        print("Please ensure the virtual environment is set up")
        return 1
    
    print(f"Configuration directory: {config_dir}")
    print(f"DAT files directory: {dat_dir}")
    print(f"SIM files directory: {sim_dir}")
    print(f"Output directory: {output_dir}")
    
    # Ensure directories exist
    sim_dir.mkdir(parents=True, exist_ok=True)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Step 1: Run tension analysis for the two main configurations
    success = True
    
    configs = [
        "dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml",
        "dm_ofx_anal_mooring_fsts_l015_125km3_sb.yml"
    ]
    
    for config in configs:
        config_path = config_dir / config
        if config_path.exists():
            cmd = [str(python_exe), "-m", "digitalmodel", str(config_path)]
            success &= run_command(
                cmd,
                f"Step 1: Running tension analysis for {config}"
            )
        else:
            print(f"Warning: Config file not found: {config_path}")
    
    # Step 2: Generate .sim files from .dat files
    if dat_dir.exists() and list(dat_dir.glob("*.dat")):
        cmd = [
            str(python_exe),
            str(script_dir / "run_models_to_sim.py"),
            "dat=true",
            f"input_directory={dat_dir}",
            f"output_directory={sim_dir}"
        ]
        success &= run_command(
            cmd,
            "Step 2: Generating .sim files from .dat files"
        )
    else:
        print(f"Step 2: Skipping .sim generation (no .dat files in {dat_dir})")
    
    # Step 3: Post-process results
    post_config = config_dir / "dm_ofx_post_fsts_lngc.yml"
    if post_config.exists():
        cmd = [
            str(python_exe), "-m", "digitalmodel",
            str(post_config),
            "--workers", "30"
        ]
        success &= run_command(
            cmd,
            "Step 3: Post-processing results"
        )
    else:
        print(f"Step 3: Skipping post-processing (config not found: {post_config})")
    
    # Step 4: Collate results (if assetutilities is available)
    au_python = Path("D:/github/assetutilities/.venv/Scripts/python.exe")
    au_config = config_dir / "au_collate.yml"
    
    if au_python.exists() and au_config.exists():
        cmd = [
            str(au_python), "-m", "assetutilities",
            str(au_config)
        ]
        success &= run_command(
            cmd,
            "Step 4: Collating results with assetutilities"
        )
    else:
        print("Step 4: Skipping collation (assetutilities not available)")
    
    # Step 5: Generate visualizations
    viz_config = config_dir / "viz.yml"
    if viz_config.exists() and sim_dir.exists():
        # Build the override parameters for visualization
        override_params = {
            'meta': {'label': 'viz_fsts_'},
            'file_management': {
                'input_directory': str(sim_dir),
                'output_directory': str(output_dir / 'visual'),
                'filename': {'pattern': 'fsts_*_vessel_statics_6dof'}
            }
        }
        
        cmd = [
            str(python_exe), "-m", "digitalmodel",
            str(viz_config),
            str(override_params),
            "--workers", "30"
        ]
        success &= run_command(
            cmd,
            "Step 5: Generating visualizations"
        )
    else:
        print(f"Step 5: Skipping visualization (config or sim files not found)")
    
    # Step 6: Generate plots
    plot_script = script_dir / "generate_mooring_plots.py"
    if plot_script.exists():
        cmd = [str(python_exe), str(plot_script)]
        success &= run_command(
            cmd,
            "Step 6: Generating mooring plots"
        )
    else:
        print(f"Step 6: Skipping plot generation (script not found: {plot_script})")
    
    # Summary
    print("\n" + "=" * 60)
    if success:
        print("Test execution complete!")
    else:
        print("Test execution completed with some errors")
    print(f"Check output directory: {output_dir}")
    print("=" * 60)
    
    return 0 if success else 1

if __name__ == "__main__":
    sys.exit(main())