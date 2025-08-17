#!/usr/bin/env python
"""
Test OrcaFlex Batch Runner with actual OrcaFlex API
Creates real .sim files to verify functionality
"""

import sys
import yaml
import tempfile
import shutil
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent / "src"))

from modules.orcaflex.mooring_tension_iteration.batch_processing.orcaflex_batch_runner import (
    OrcaFlexBatchRunner
)

def create_test_environment():
    """Create test directory with model files."""
    test_dir = tempfile.mkdtemp(prefix="orcaflex_test_")
    print(f"Created test directory: {test_dir}")
    
    base_dir = Path(test_dir) / "models"
    base_dir.mkdir(parents=True)
    
    # Create a simple OrcaFlex model file using valid format
    model_content = """﻿%YAML 1.1
# Type: Model
# Program: OrcaFlex 11.5e
# File: test_model.yml
# Created: Test File
---
General:
  UnitsSystem: SI
  StageDuration:
    - 8
    - 16
  ImplicitConstantTimeStep: 0.1
  DynamicsSolutionMethod: Implicit time domain

Environment:
  WaterDepth: 1500.0
  Density: 1.025
  KinematicViscosity: 1.35e-6
  SeabedModel: Flat
  SeabedNormalStiffness: 100

LineTypes:
  Chain:
    Category: General
    OD: 0.1
    ID: 0
    MassPerUnitLength: 100
    EA: 1e8
    EI: 0
    GJ: 0
    CompressionIsLimited: No
    MinRadius: ~

Lines:
  Line1:
    LineType: Chain
    Length[1]: 10.0
    Length[2]: 850.0
    Length[3]: 10.0
    TargetSegmentLength[1]: 1.0
    TargetSegmentLength[2]: 10.0
    TargetSegmentLength[3]: 1.0
    Connection:
      ConnectionType: Fixed
      StageMode: None
    EndAConnection: Fixed
    EndBConnection: Fixed
    EndAX: -500
    EndAY: 0
    EndAZ: -50
    EndBX: 500
    EndBY: 0
    EndBZ: -50
"""
    
    # Save model files
    model_files = []
    for i in range(1, 4):
        model_file = base_dir / f"test_model_{i}.yml"
        with open(model_file, 'w', encoding='utf-8') as f:
            f.write(model_content)
        model_files.append(model_file.name)
        print(f"Created model file: {model_file}")
    
    # Create batch configuration
    config = {
        'batch_info': {
            'name': 'OrcaFlex License Test',
            'description': 'Test .sim file generation with actual OrcaFlex',
            'base_directory': str(base_dir),
            'output_directory': './output',
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
                'model_file': model_file,
                'description': f'Test Model {i+1}'
            }
            for i, model_file in enumerate(model_files)
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
    
    config_file = base_dir / "batch_config.yml"
    with open(config_file, 'w') as f:
        yaml.dump(config, f)
    
    print(f"Created batch configuration: {config_file}")
    
    return test_dir, config_file

def run_test():
    """Run the batch processor and verify .sim files."""
    print("\n" + "="*60)
    print("Testing OrcaFlex Batch Runner with License")
    print("="*60 + "\n")
    
    # Check OrcaFlex availability
    try:
        import OrcFxAPI
        print(f"[OK] OrcaFlex API available (DLL Version: {OrcFxAPI.DLLVersion()})")
    except ImportError:
        print("[ERROR] OrcaFlex API not available - cannot run test")
        return False
    
    # Create test environment
    test_dir, config_file = create_test_environment()
    
    try:
        # Run batch processor with actual OrcaFlex (not mock mode)
        print(f"\nRunning batch processor with config: {config_file}")
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
        
        # Verify .sim files were created
        sim_dir = runner.sim_dir
        print(f"\nChecking for .sim files in: {sim_dir}")
        
        sim_files = list(sim_dir.glob("*.sim"))
        print(f"Found {len(sim_files)} .sim files:")
        
        success = True
        for sim_file in sim_files:
            file_size = sim_file.stat().st_size
            print(f"  [OK] {sim_file.name} ({file_size:,} bytes)")
            if file_size < 1000:
                print(f"    WARNING: File seems too small!")
                success = False
        
        # Expected files check
        expected_count = summary['successful']
        if len(sim_files) != expected_count:
            print(f"\n[ERROR] Expected {expected_count} .sim files but found {len(sim_files)}")
            success = False
        else:
            print(f"\n[SUCCESS] All {expected_count} .sim files were created successfully!")
        
        # Display file paths
        if sim_files:
            print("\nGenerated .sim file paths:")
            for result in summary['results']:
                if result['success'] and 'sim_output' in result:
                    print(f"  - {result['sim_output']}")
        
        return success
        
    except Exception as e:
        print(f"\n[ERROR] during test: {e}")
        import traceback
        traceback.print_exc()
        return False
        
    finally:
        # Clean up
        print(f"\nCleaning up test directory: {test_dir}")
        try:
            shutil.rmtree(test_dir)
            print("[OK] Cleanup complete")
        except Exception as e:
            print(f"[ERROR] Cleanup failed: {e}")

if __name__ == "__main__":
    success = run_test()
    sys.exit(0 if success else 1)