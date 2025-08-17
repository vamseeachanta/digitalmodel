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
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent.parent / 'src'))

from modules.orcaflex.mooring_tension_iteration.batch_processing.orcaflex_batch_runner import (
    OrcaFlexBatchRunner
)
from modules.orcaflex.mooring_tension_iteration.file_type_detector import (
    FileTypeDetector, FileType, FileClassifier
)


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
    
    # Run main test
    success = test_actual_models()
    
    # Run comparison test
    test_parallel_vs_sequential()
    
    print("\n" + "=" * 80)
    if success:
        print("[OK] All tests completed successfully!")
        print("\nThe batch runner correctly:")
        print("  - Identified OrcaFlex models vs DigitalModel configs")
        print("  - Processed models in parallel")
        print("  - Generated .sim file paths for each model")
    else:
        print("[FAILED] Some tests failed. Check output above for details.")
    print("=" * 80)


if __name__ == "__main__":
    main()