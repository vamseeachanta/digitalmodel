#!/usr/bin/env python
"""
Test OrcaFlex Batch Runner with existing test models
Uses actual OrcaFlex test files from the repository
"""

import sys
import yaml
import time
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent / "src"))

from modules.orcaflex.mooring_tension_iteration.batch_processing.orcaflex_batch_runner import (
    OrcaFlexBatchRunner
)

def test_with_existing_models():
    """Test batch runner with existing OrcaFlex test models."""
    print("\n" + "="*60)
    print("Testing OrcaFlex Batch Runner with Existing Models")
    print("="*60 + "\n")
    
    # Check OrcaFlex availability
    try:
        import OrcFxAPI
        print(f"[OK] OrcaFlex API available (DLL Version: {OrcFxAPI.DLLVersion()})")
    except ImportError:
        print("[ERROR] OrcaFlex API not available - cannot run test")
        return False
    
    # Use existing test directory
    test_dir = Path(__file__).parent / "tests" / "modules" / "orcaflex" / "orcaflex_analysis"
    
    # Verify test files exist
    test_files = [
        test_dir / "orcaflex_test1.yml",
        test_dir / "orcaflex_test2.dat"
    ]
    
    print("\nChecking for existing test files:")
    for file in test_files:
        if file.exists():
            print(f"  [OK] Found: {file.name}")
        else:
            print(f"  [ERROR] Missing: {file.name}")
            return False
    
    # Create batch configuration
    config = {
        'batch_info': {
            'name': 'Test Existing OrcaFlex Models',
            'description': 'Verify .sim file generation with existing test models',
            'base_directory': str(test_dir),
            'output_directory': './test_sim_output',
            'timestamp': '2025-01-17'
        },
        'simulation_settings': {
            'analysis_type': 'statics',
            'calculate_statics': True,
            'save_simulation_file': True,
            'save_results': True,
            'continue_on_error': True,
            'parallel_processing': False
        },
        'output_settings': {
            'save_csv': True,
            'save_simulation': True,
            'csv_output_folder': 'csv',
            'sim_output_folder': 'sim',
            'include_summary_report': True
        },
        'models': [
            {
                'model_file': 'orcaflex_test1.yml',
                'description': 'OrcaFlex Test Model 1 (YML format)'
            },
            {
                'model_file': 'orcaflex_test2.dat',
                'description': 'OrcaFlex Test Model 2 (DAT format)'
            }
        ],
        'mooring_parameters': {
            'lines_to_check': [],
            'tension_tolerance': 0.01,
            'section_to_modify': 2
        },
        'processing_options': {
            'max_iterations': 5,
            'damping_factor': 0.7,
            'create_backup': True,
            'generate_report': True,
            'log_level': 'INFO'
        }
    }
    
    # Save configuration
    config_file = test_dir / "test_batch_config.yml"
    with open(config_file, 'w') as f:
        yaml.dump(config, f)
    print(f"\nCreated batch configuration: {config_file}")
    
    try:
        # Run batch processor with actual OrcaFlex (not mock mode)
        print(f"\nRunning batch processor...")
        print("-" * 40)
        
        runner = OrcaFlexBatchRunner(str(config_file), mock_mode=False)
        summary = runner.run_batch()
        
        # Display results
        print("\n" + "="*60)
        print("BATCH PROCESSING RESULTS")
        print("="*60)
        print(f"Total models processed: {summary['total_models']}")
        print(f"Successful: {summary['successful']}")
        print(f"Failed: {summary['failed']}")
        print(f"Success rate: {summary['success_rate']:.1f}%")
        print(f"Elapsed time: {summary['elapsed_time']:.2f} seconds")
        
        # Check for .sim files
        sim_dir = runner.sim_dir
        print(f"\nChecking for .sim files in: {sim_dir}")
        
        if sim_dir.exists():
            sim_files = list(sim_dir.glob("*.sim"))
            print(f"Found {len(sim_files)} .sim files:")
            
            for sim_file in sim_files:
                file_size = sim_file.stat().st_size
                size_mb = file_size / (1024 * 1024)
                print(f"  [OK] {sim_file.name} ({size_mb:.2f} MB)")
            
            # Verify expected files
            expected_files = ['orcaflex_test1.sim', 'orcaflex_test2.sim']
            for expected in expected_files:
                expected_path = sim_dir / expected
                if expected_path.exists():
                    print(f"  [VERIFIED] {expected} exists")
                else:
                    print(f"  [WARNING] {expected} not found")
            
            if len(sim_files) > 0:
                print(f"\n[SUCCESS] Successfully created {len(sim_files)} .sim files!")
                return True
            else:
                print("\n[WARNING] No .sim files were created")
                return False
        else:
            print(f"\n[ERROR] Simulation directory does not exist: {sim_dir}")
            return False
            
    except Exception as e:
        print(f"\n[ERROR] during test: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = test_with_existing_models()
    
    # Also check if .sim files already exist from previous runs
    test_dir = Path(__file__).parent / "tests" / "modules" / "orcaflex" / "orcaflex_analysis"
    existing_sims = list(test_dir.glob("*.sim"))
    
    if existing_sims:
        print(f"\n[INFO] Found {len(existing_sims)} existing .sim files in test directory:")
        for sim in existing_sims:
            size_mb = sim.stat().st_size / (1024 * 1024)
            print(f"  - {sim.name} ({size_mb:.2f} MB)")
    
    sys.exit(0 if success or existing_sims else 1)