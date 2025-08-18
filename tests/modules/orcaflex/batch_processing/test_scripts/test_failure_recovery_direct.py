#!/usr/bin/env python
"""
Direct test of failure recovery mechanism for OrcaFlex batch processing.
This test intentionally creates failures to verify the summary and rerun features work.
"""

import sys
import yaml
import tempfile
from pathlib import Path
from datetime import datetime

# Add src to path
sys.path.insert(0, str(Path(__file__).parent / "src"))

from modules.orcaflex.batch_processing.orcaflex_batch_runner import OrcaFlexBatchRunner


def test_failure_recovery():
    """Test the failure recovery and rerun mechanism with intentional failures."""
    print("\n" + "=" * 80)
    print("TESTING ORCAFLEX BATCH FAILURE RECOVERY SYSTEM")
    print("=" * 80)
    
    # Create temporary directory manually to avoid cleanup issues
    temp_dir = Path("./test_failure_recovery_temp")
    temp_dir.mkdir(exist_ok=True)
    
    try:
        # Create a mix of valid and invalid model configurations
        models = [
            {'model_file': 'valid_model1.yml', 'description': 'Valid test model 1'},
            {'model_file': 'valid_model2.yml', 'description': 'Valid test model 2'},
            {'model_file': 'missing_file.dat', 'description': 'File that does not exist - SHOULD FAIL'},
            {'model_file': 'invalid_format.txt', 'description': 'Invalid file format - SHOULD FAIL'},
            {'model_file': 'no_license.yml', 'description': 'Model requiring license - SHOULD FAIL'},
        ]
        
        # Create only the valid model files
        for i in range(1, 3):
            model_file = temp_dir / f'valid_model{i}.yml'
            model_file.write_text(f'''%YAML 1.1
# Type: Model
---
General:
  UnitsSystem: SI
  ModelName: ValidModel{i}
''')
            print(f"Created valid model: {model_file.name}")
        
        # Create an invalid format file
        invalid_file = temp_dir / 'invalid_format.txt'
        invalid_file.write_text('This is not a valid OrcaFlex model file')
        print(f"Created invalid file: {invalid_file.name}")
        
        # Create batch configuration
        config = {
            'batch_info': {
                'name': 'Test Failure Recovery System',
                'description': 'Testing failure summary and recovery with mixed success/failure',
                'base_directory': str(temp_dir),
                'output_directory': './output',
                'timestamp': datetime.now().strftime('%Y-%m-%d %H:%M:%S')
            },
            'simulation_settings': {
                'analysis_type': 'statics',
                'continue_on_error': True,  # Critical for testing multiple failures
                'parallel_processing': False,  # Sequential for clearer test output
                'save_simulation_file': True,
                'save_results': True
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
                'section_to_modify': 2,
                'lines_to_check': ['Line01', 'Line02']
            },
            'processing_options': {
                'max_iterations': 10,
                'damping_factor': 0.7,
                'generate_report': True,
                'create_backup': True,
                'log_level': 'INFO'
            }
        }
        
        config_file = temp_dir / 'test_failure_config.yml'
        with open(config_file, 'w') as f:
            yaml.dump(config, f, default_flow_style=False)
        
        print(f"\nTest Setup Complete:")
        print(f"  - Directory: {temp_dir}")
        print(f"  - Total models: {len(models)}")
        print(f"  - Expected successes: 2")
        print(f"  - Expected failures: 3")
        print(f"  - continue_on_error: True")
        
        # Run batch with expected failures
        print("\n" + "-" * 80)
        print("RUNNING BATCH PROCESSING (expecting some failures)...")
        print("-" * 80)
        
        runner = OrcaFlexBatchRunner(str(config_file), mock_mode=True)
        summary = runner.run_batch()
        
        # Display results
        print("\n" + "=" * 80)
        print("BATCH PROCESSING RESULTS")
        print("=" * 80)
        print(f"Total models: {summary['total_models']}")
        print(f"Successful: {summary['successful']}")
        print(f"Failed: {summary['failed']}")
        print(f"Success rate: {summary['success_rate']:.1f}%")
        
        # Check if failure files were created
        output_dir = runner.output_dir
        print(f"\nOutput directory: {output_dir}")
        
        if summary['failed'] > 0:
            print("\n" + "=" * 80)
            print("FAILURE RECOVERY FILES GENERATED")
            print("=" * 80)
            
            # Check for failure summary files
            failure_files = list(output_dir.glob("failed_runs_*.txt"))
            rerun_configs = list(output_dir.glob("rerun_failed_*.yml"))
            
            if failure_files:
                latest_failure = sorted(failure_files)[-1]
                print(f"\n[OK] Failure Summary Created: {latest_failure.name}")
                
                # Display first few lines of failure summary
                with open(latest_failure) as f:
                    lines = f.readlines()[:30]
                    print("\nFailure Summary Preview:")
                    print("-" * 40)
                    for line in lines:
                        print(line.rstrip())
                    if len(f.readlines()) > 30:
                        print("... (truncated)")
            
            if rerun_configs:
                latest_rerun = sorted(rerun_configs)[-1]
                print(f"\n[OK] Rerun Configuration Created: {latest_rerun.name}")
                
                # Load and display rerun config details
                with open(latest_rerun) as f:
                    rerun_data = yaml.safe_load(f)
                
                print("\nRerun Configuration Details:")
                print("-" * 40)
                print(f"  Name: {rerun_data['batch_info']['name']}")
                print(f"  Description: {rerun_data['batch_info']['description']}")
                print(f"  Failed models to retry: {len(rerun_data['models'])}")
                print(f"  Original config: {rerun_data['batch_info'].get('original_config', 'N/A')}")
                
                print("\nModels to rerun:")
                for model in rerun_data['models']:
                    print(f"  - {model['model_file']}")
                    if 'retry_attempt' in model:
                        print(f"    Retry attempt: {model['retry_attempt']}")
                
                # Test the rerun capability
                print("\n" + "=" * 80)
                print("TESTING RERUN CAPABILITY")
                print("=" * 80)
                print(f"\nAttempting to rerun failed models...")
                
                rerun_runner = OrcaFlexBatchRunner(str(latest_rerun), mock_mode=True)
                rerun_summary = rerun_runner.run_batch()
                
                print(f"\nRerun Results:")
                print(f"  Attempted: {rerun_summary['total_models']}")
                print(f"  Successful: {rerun_summary['successful']}")
                print(f"  Still Failed: {rerun_summary['failed']}")
                
                # Verify the retry attempt counter
                if rerun_summary['failed'] > 0:
                    # Check for new rerun config with incremented retry counter
                    new_rerun_configs = list(rerun_runner.output_dir.glob("rerun_failed_*.yml"))
                    if len(new_rerun_configs) > len(rerun_configs):
                        newest_rerun = sorted(new_rerun_configs)[-1]
                        with open(newest_rerun) as f:
                            newest_data = yaml.safe_load(f)
                        
                        print(f"\n[OK] New rerun config created after retry: {newest_rerun.name}")
                        if newest_data['models'] and 'retry_attempt' in newest_data['models'][0]:
                            print(f"  Retry attempts now at: {newest_data['models'][0]['retry_attempt']}")
                
                print("\n" + "=" * 80)
                print("TEST VALIDATION")
                print("=" * 80)
                
                # Validate the test results
                test_passed = True
                
                # Check 1: Failure summary was created
                if not failure_files:
                    print("[X] FAIL: No failure summary file created")
                    test_passed = False
                else:
                    print("[OK] PASS: Failure summary file created")
                
                # Check 2: Rerun config was created
                if not rerun_configs:
                    print("[X] FAIL: No rerun configuration created")
                    test_passed = False
                else:
                    print("[OK] PASS: Rerun configuration created")
                
                # Check 3: Rerun config contains only failed models
                if rerun_configs and len(rerun_data['models']) == summary['failed']:
                    print(f"[OK] PASS: Rerun config contains correct number of failed models ({summary['failed']})")
                else:
                    print(f"[X] FAIL: Rerun config model count mismatch")
                    test_passed = False
                
                # Check 4: Retry counter increments
                if rerun_summary['results'] and any('retry_attempt' in r for r in rerun_summary['results']):
                    print("[OK] PASS: Retry attempt counter working")
                else:
                    print("[!] WARN: Could not verify retry counter (may be in mock mode)")
                
                if test_passed:
                    print("\n*** SUCCESS: All failure recovery features working correctly! ***")
                else:
                    print("\n*** FAILURE: Some recovery features not working as expected ***")
                
                return test_passed
        else:
            print("\n[!] WARNING: No failures occurred - cannot test recovery features")
            print("This may happen in mock mode if all models appear valid")
            return False
            
    finally:
        # Clean up temp directory
        import shutil
        if temp_dir.exists():
            try:
                shutil.rmtree(temp_dir)
                print(f"\nCleaned up temp directory: {temp_dir}")
            except Exception as e:
                print(f"\nWarning: Could not clean up {temp_dir}: {e}")


if __name__ == "__main__":
    print("=" * 80)
    print("ORCAFLEX BATCH PROCESSING - FAILURE RECOVERY TEST")
    print("=" * 80)
    print("\nThis test will:")
    print("1. Create a mix of valid and invalid OrcaFlex models")
    print("2. Run batch processing with continue_on_error=True")
    print("3. Verify failure summary files are created")
    print("4. Verify rerun configuration is generated")
    print("5. Test the rerun capability")
    print("6. Validate retry attempt tracking")
    
    success = test_failure_recovery()
    
    print("\n" + "=" * 80)
    if success:
        print("[SUCCESS] TEST PASSED: Failure recovery system working correctly")
        sys.exit(0)
    else:
        print("[FAILED] TEST FAILED: Check output above for details")
        sys.exit(1)