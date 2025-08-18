#!/usr/bin/env python
"""
Test Batch Runner with Actual OrcaFlex Test Models
==================================================
This script tests the batch runner using actual OrcaFlex model files
from: D:\\github\\digitalmodel\\tests\\modules\\orcaflex\\orcaflex_analysis
"""

import sys
import os
import yaml
import shutil
import tempfile
from pathlib import Path
from datetime import datetime

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent.parent / "src"))

from modules.orcaflex.batch_processing.orcaflex_batch_runner import (
    OrcaFlexBatchRunner
)
from modules.orcaflex.mooring_tension_iteration.file_type_detector import (
    FileTypeDetector, FileType, FileClassifier
)


def display_failure_summary(summary: dict, output_dir: Path):
    """
    Display a detailed failure summary for easy analysis and rerun.
    
    Args:
        summary: Batch processing summary with results
        output_dir: Output directory where failure files are saved
    """
    failed_models = [r for r in summary['results'] if not r['success']]
    
    if not failed_models:
        print("No failures to analyze")
        return
    
    # Group failures by error type
    error_groups = {}
    for model in failed_models:
        error = model.get('error', 'Unknown error')
        error_key = error.split('\n')[0][:80]  # First line, max 80 chars
        if error_key not in error_groups:
            error_groups[error_key] = []
        error_groups[error_key].append(model)
    
    # Display grouped failures
    print(f"\nTotal Failed Models: {len(failed_models)}")
    print("\nFailures by Error Type:")
    print("-" * 40)
    
    for error_type, models in error_groups.items():
        print(f"\n• {error_type}")
        print(f"  Count: {len(models)} models")
        print("  Affected files:")
        for model in models[:3]:  # Show first 3 models
            print(f"    - {model['model_file']}")
        if len(models) > 3:
            print(f"    ... and {len(models) - 3} more")
    
    # Check for generated failure files
    print("\n" + "-" * 40)
    print("Generated Failure Analysis Files:")
    
    # Look for failure summary files
    failure_files = list(output_dir.glob("failed_runs_*.txt"))
    rerun_configs = list(output_dir.glob("rerun_failed_*.yml"))
    
    if failure_files:
        latest_failure = sorted(failure_files)[-1]
        print(f"  • Failure Summary: {latest_failure.name}")
    
    if rerun_configs:
        latest_rerun = sorted(rerun_configs)[-1]
        print(f"  • Rerun Config: {latest_rerun.name}")
        print(f"\nTo rerun failed models:")
        print(f"  python run_batch.py --config {latest_rerun}")
    
    # Provide quick action suggestions
    print("\n" + "-" * 40)
    print("Quick Actions:")
    
    if any('license' in str(eg).lower() for eg in error_groups.keys()):
        print("  ⚠ License issues detected - Check OrcaFlex license")
    
    if any('file not found' in str(eg).lower() for eg in error_groups.keys()):
        print("  ⚠ Missing files detected - Verify model paths")
    
    if any('convergence' in str(eg).lower() for eg in error_groups.keys()):
        print("  ⚠ Convergence issues - Consider adjusting parameters")


def test_failure_recovery():
    """
    Test the failure recovery and rerun mechanism.
    Intentionally creates failures to test the summary generation.
    """
    print("\n" + "=" * 80)
    print("Testing Failure Recovery Mechanism")
    print("=" * 80)
    
    with tempfile.TemporaryDirectory() as temp_dir:
        test_dir = Path(temp_dir)
        
        # Create a mix of valid and invalid model configurations
        models = [
            {'model_file': 'valid_model.yml', 'description': 'Valid test model'},
            {'model_file': 'missing_file.dat', 'description': 'File that does not exist'},
            {'model_file': 'invalid_format.txt', 'description': 'Invalid file format'},
            {'model_file': 'convergence_fail.yml', 'description': 'Model with convergence issues'},
        ]
        
        # Create only the valid model file
        valid_model = test_dir / 'valid_model.yml'
        valid_model.write_text('''%YAML 1.1
# Type: Model
---
General:
  UnitsSystem: SI
''')
        
        # Create batch config with mixed models
        config = {
            'batch_info': {
                'name': 'Test Failure Recovery',
                'description': 'Testing failure summary and recovery',
                'base_directory': str(test_dir),
                'output_directory': './output',
                'timestamp': datetime.now().strftime('%Y-%m-%d')
            },
            'simulation_settings': {
                'analysis_type': 'statics',
                'continue_on_error': True,  # Important for testing
                'parallel_processing': False
            },
            'output_settings': {
                'save_csv': True,
                'save_simulation': True,
                'csv_output_folder': 'csv',
                'sim_output_folder': 'sim',
                'include_summary_report': True
            },
            'models': models,
            'mooring_parameters': {
                'tension_tolerance': 0.01,
                'section_to_modify': 2
            },
            'processing_options': {
                'max_iterations': 10,
                'damping_factor': 0.7,
                'generate_report': True,
                'log_level': 'INFO'
            }
        }
        
        config_file = test_dir / 'test_failure_config.yml'
        with open(config_file, 'w') as f:
            yaml.dump(config, f)
        
        print(f"\nTest configuration created with:")
        print(f"  - 1 valid model")
        print(f"  - 3 models designed to fail")
        print(f"  - continue_on_error: True")
        
        # Run batch with expected failures
        print("\nRunning batch (expecting failures)...")
        runner = OrcaFlexBatchRunner(str(config_file), mock_mode=True)
        summary = runner.run_batch()
        
        # Display results
        print(f"\nResults:")
        print(f"  Successful: {summary['successful']}")
        print(f"  Failed: {summary['failed']}")
        
        # Display failure analysis
        if summary['failed'] > 0:
            display_failure_summary(summary, runner.output_dir)
            
            # Check if rerun config was created
            rerun_configs = list(runner.output_dir.glob("rerun_failed_*.yml"))
            if rerun_configs:
                print("\n" + "=" * 80)
                print("Testing Rerun Capability")
                print("=" * 80)
                
                latest_rerun = sorted(rerun_configs)[-1]
                print(f"\nRerunning with: {latest_rerun.name}")
                
                # Load and display rerun config
                with open(latest_rerun) as f:
                    rerun_data = yaml.safe_load(f)
                
                print(f"Rerun configuration contains:")
                print(f"  - {len(rerun_data['models'])} failed models")
                print(f"  - Original config: {rerun_data['batch_info'].get('original_config', 'N/A')}")
                
                # Attempt rerun (will still fail in mock, but tests the mechanism)
                print("\nAttempting rerun...")
                rerun_runner = OrcaFlexBatchRunner(str(latest_rerun), mock_mode=True)
                rerun_summary = rerun_runner.run_batch()
                
                print(f"\nRerun Results:")
                print(f"  Attempted: {rerun_summary['total_models']}")
                print(f"  Successful: {rerun_summary['successful']}")
                print(f"  Still Failed: {rerun_summary['failed']}")
        
        return summary['failed'] > 0  # Test passes if failures were properly handled


def create_test_batch_config(test_dir: Path, model_files: list) -> Path:
    """Create a batch configuration for test models."""
    config = {
        'batch_info': {
            'name': 'Test with Actual OrcaFlex Models',
            'description': 'Testing batch runner with real OrcaFlex test files',
            'base_directory': str(test_dir),
            'output_directory': './output/test_results',
            'timestamp': datetime.now().strftime('%Y-%m-%d')
        },
        'simulation_settings': {
            'analysis_type': 'statics',
            'calculate_statics': True,
            'save_simulation_file': True,
            'save_results': True,
            'continue_on_error': True,
            'parallel_processing': True  # Enable parallel processing
        },
        'output_settings': {
            'save_csv': True,
            'save_simulation': True,
            'csv_output_folder': 'csv',
            'sim_output_folder': 'sim',
            'include_summary_report': True
        },
        'models': [],
        'mooring_parameters': {
            'lines_to_check': ['Line01', 'Line02'],
            'tension_tolerance': 0.01,
            'section_to_modify': 2
        },
        'processing_options': {
            'max_iterations': 10,
            'damping_factor': 0.7,
            'create_backup': True,
            'generate_report': True,
            'log_level': 'INFO'
        }
    }
    
    # Add model configurations
    for model_file in model_files:
        model_name = Path(model_file).stem
        config['models'].append({
            'model_file': model_file,
            'description': f'Test model: {model_name}'
        })
    
    # Save configuration
    config_file = test_dir / 'test_batch_config.yml'
    with open(config_file, 'w') as f:
        yaml.dump(config, f)
    
    return config_file


def test_actual_models():
    """Test with actual OrcaFlex models from the test directory."""
    print("=" * 80)
    print("Testing Batch Runner with Actual OrcaFlex Models")
    print("=" * 80)
    
    # Path to actual test models
    orcaflex_test_dir = Path(r"D:\github\digitalmodel\tests\modules\orcaflex\orcaflex_analysis")
    
    if not orcaflex_test_dir.exists():
        print(f"Error: Test directory not found: {orcaflex_test_dir}")
        return False
    
    # Find actual OrcaFlex model files
    print(f"\nSearching for OrcaFlex models in: {orcaflex_test_dir}")
    
    # Use file type detector to identify OrcaFlex models
    classifier = FileClassifier()
    classification = classifier.classify_directory(orcaflex_test_dir)
    
    print("\nFile Classification Results:")
    print("-" * 40)
    for category, files in classification.items():
        if files:
            print(f"{category}: {len(files)} files")
    
    # Get OrcaFlex model files
    model_files = []
    
    # Check for specific test models we know exist
    known_test_models = [
        'orcaflex_test1.yml',
        'orcaflex_test2.dat',
        'run_orcaflex_simple.yml',
        'run_orcaflex_batch.yml'
    ]
    
    print("\nChecking for known test models:")
    for model_name in known_test_models:
        model_path = orcaflex_test_dir / model_name
        if model_path.exists():
            print(f"  [OK] Found: {model_name}")
            model_files.append(model_name)
        else:
            print(f"  [X] Not found: {model_name}")
    
    if not model_files:
        print("\nNo test models found!")
        return False
    
    # Create temporary test directory
    with tempfile.TemporaryDirectory() as temp_dir:
        test_dir = Path(temp_dir)
        print(f"\nTemporary test directory: {test_dir}")
        
        # Copy model files to test directory
        print("\nCopying model files...")
        copied_files = []
        for model_file in model_files[:2]:  # Use first 2 models for test
            src = orcaflex_test_dir / model_file
            dst = test_dir / model_file
            
            if src.exists():
                shutil.copy(src, dst)
                copied_files.append(model_file)
                print(f"  Copied: {model_file}")
        
        if not copied_files:
            print("No files copied!")
            return False
        
        # Create batch configuration
        print("\nCreating batch configuration...")
        config_file = create_test_batch_config(test_dir, copied_files)
        print(f"  Configuration saved: {config_file.name}")
        
        # Run file type detection on copied files
        print("\nFile Type Detection:")
        print("-" * 40)
        detector = FileTypeDetector()
        
        for model_file in copied_files:
            file_path = test_dir / model_file
            file_type, metadata = detector.detect_file_type(file_path)
            print(f"\n{model_file}:")
            print(f"  Type: {file_type.value}")
            print(f"  Description: {metadata.get('description', 'N/A')}")
            
            if file_type == FileType.ORCAFLEX_MODEL_YML:
                print(f"  [OK] Correctly identified as OrcaFlex YAML model")
            elif file_type == FileType.ORCAFLEX_MODEL_DAT:
                print(f"  [OK] Correctly identified as OrcaFlex DAT model")
            elif file_type == FileType.DIGITALMODEL_CONFIG:
                print(f"  [INFO] Identified as DigitalModel config")
            else:
                print(f"  [WARN] Type: {file_type.value}")
        
        # Initialize batch runner
        print("\n" + "=" * 80)
        print("Running Batch Processing")
        print("=" * 80)
        
        try:
            # Run in mock mode (no OrcaFlex license required)
            runner = OrcaFlexBatchRunner(str(config_file), mock_mode=True)
            
            print(f"Models to process: {len(runner.config['models'])}")
            print(f"Parallel processing: {runner.parallel_processing}")
            print(f"Output directory: {runner.output_dir}")
            
            # Run batch
            print("\nStarting batch processing...")
            summary = runner.run_batch()
            
            # Display results
            print("\n" + "=" * 80)
            print("Batch Processing Results")
            print("=" * 80)
            print(f"Total models: {summary['total_models']}")
            print(f"Successful: {summary['successful']}")
            print(f"Failed: {summary['failed']}")
            print(f"Success rate: {summary['success_rate']:.1f}%")
            print(f"Elapsed time: {summary['elapsed_time']:.2f} seconds")
            
            # Display failure summary if there are failures
            if summary['failed'] > 0:
                print("\n" + "=" * 80)
                print("FAILURE ANALYSIS")
                print("=" * 80)
                display_failure_summary(summary, runner.output_dir)
            
            # Check simulation file paths
            print("\nSimulation Files (would be created with license):")
            sim_count = 0
            for result in summary['results']:
                if result.get('success'):
                    model_name = result.get('model_file', 'unknown')
                    if 'sim_output' in result:
                        sim_path = Path(result['sim_output'])
                        print(f"  {model_name} -> {sim_path.name}")
                        sim_count += 1
            
            print(f"\nTotal: {sim_count} .sim files would be generated")
            
            # Verify the test worked correctly
            if summary['successful'] == len(copied_files):
                print("\n[OK] TEST PASSED: All models processed successfully")
                return True
            else:
                print(f"\n[FAILED] TEST FAILED: Only {summary['successful']}/{len(copied_files)} models processed")
                return False
                
        except Exception as e:
            print(f"\n[ERROR] Error running batch: {e}")
            import traceback
            traceback.print_exc()
            return False


def test_parallel_vs_sequential():
    """Compare parallel vs sequential processing."""
    print("\n" + "=" * 80)
    print("Testing Parallel vs Sequential Processing")
    print("=" * 80)
    
    orcaflex_test_dir = Path(r"D:\github\digitalmodel\tests\modules\orcaflex\orcaflex_analysis")
    
    # Get test models
    test_models = []
    for ext in ['*.yml', '*.dat']:
        test_models.extend(list(orcaflex_test_dir.glob(ext))[:5])  # Get up to 5 models
    
    if len(test_models) < 2:
        print("Not enough test models for comparison")
        return
    
    print(f"\nUsing {len(test_models)} test models for comparison")
    
    with tempfile.TemporaryDirectory() as temp_dir:
        test_dir = Path(temp_dir)
        
        # Copy models
        copied_files = []
        for model_path in test_models:
            dst = test_dir / model_path.name
            shutil.copy(model_path, dst)
            copied_files.append(model_path.name)
        
        # Create configurations for both modes
        config_seq = create_test_batch_config(test_dir, copied_files)
        config_par = test_dir / 'test_batch_parallel.yml'
        
        # Load and modify for parallel
        with open(config_seq) as f:
            config_data = yaml.safe_load(f)
        
        # Sequential configuration
        config_data['simulation_settings']['parallel_processing'] = False
        with open(config_seq, 'w') as f:
            yaml.dump(config_data, f)
        
        # Parallel configuration
        config_data['simulation_settings']['parallel_processing'] = True
        with open(config_par, 'w') as f:
            yaml.dump(config_data, f)
        
        # Test sequential
        print("\nTesting SEQUENTIAL processing...")
        runner_seq = OrcaFlexBatchRunner(str(config_seq), mock_mode=True)
        import time
        start = time.time()
        summary_seq = runner_seq.run_batch()
        time_seq = time.time() - start
        
        # Test parallel
        print("\nTesting PARALLEL processing...")
        runner_par = OrcaFlexBatchRunner(str(config_par), mock_mode=True)
        start = time.time()
        summary_par = runner_par.run_batch()
        time_par = time.time() - start
        
        # Compare results
        print("\n" + "=" * 80)
        print("Performance Comparison")
        print("=" * 80)
        print(f"Models processed: {len(copied_files)}")
        print(f"\nSequential:")
        print(f"  Time: {time_seq:.2f} seconds")
        print(f"  Success: {summary_seq['successful']}/{summary_seq['total_models']}")
        print(f"\nParallel:")
        print(f"  Time: {time_par:.2f} seconds")
        print(f"  Success: {summary_par['successful']}/{summary_par['total_models']}")
        
        if time_par < time_seq:
            speedup = time_seq / time_par
            print(f"\n[OK] Parallel is {speedup:.1f}x faster!")
        else:
            print(f"\n[INFO] Sequential was faster (likely due to mock mode overhead)")


def main():
    """Main test function."""
    print("\n" + "=" * 80)
    print("OrcaFlex Batch Runner - Test with Actual Models")
    print("=" * 80)
    print(f"\nTest models directory: D:\\github\\digitalmodel\\tests\\modules\\orcaflex\\orcaflex_analysis")
    print("\nThis test will:")
    print("1. Find actual OrcaFlex model files in the test directory")
    print("2. Use file type detection to identify them correctly")
    print("3. Run batch processing with parallel execution")
    print("4. Verify .sim file paths are generated")
    print("5. Test failure recovery and rerun mechanism")
    
    # Run main test
    success = test_actual_models()
    
    # Run comparison test
    test_parallel_vs_sequential()
    
    # Run failure recovery test
    print("\n" + "=" * 80)
    print("TESTING FAILURE RECOVERY FEATURES")
    print("=" * 80)
    recovery_success = test_failure_recovery()
    
    print("\n" + "=" * 80)
    print("TEST SUMMARY")
    print("=" * 80)
    
    if success:
        print("✓ Model processing test: PASSED")
    else:
        print("✗ Model processing test: FAILED")
    
    if recovery_success:
        print("✓ Failure recovery test: PASSED")
    else:
        print("✗ Failure recovery test: FAILED")
    
    print("\nThe batch runner features:")
    print("  - Identifies OrcaFlex models vs DigitalModel configs")
    print("  - Processes models in parallel")
    print("  - Generates .sim file paths for each model")
    print("  - Creates detailed failure summaries")
    print("  - Generates rerun configurations for failed models")
    print("  - Groups failures by error type")
    print("  - Provides actionable suggestions for common errors")
    
    overall_success = success and recovery_success
    if overall_success:
        print("\n[OK] All tests completed successfully!")
    else:
        print("\n[PARTIAL] Some tests failed. Check output above for details.")
    print("=" * 80)


if __name__ == "__main__":
    main()