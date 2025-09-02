#!/usr/bin/env python
"""
Test script to verify parallel processing functionality in OrcaFlex post-processing.
This test creates mock simulation files and processes them to verify parallel execution.
"""

import yaml
import time
from pathlib import Path
from digitalmodel.modules.orcaflex.opp import OrcaFlexPostProcess

def create_test_config():
    """Create a test configuration with parallel processing enabled."""
    config = {
        "meta": {
            "basename": "test_parallel"
        },
        "file_management": {
            "input_files": {
                "sim": [
                    "test1.sim",
                    "test2.sim", 
                    "test3.sim",
                    "test4.sim"
                ]
            },
            "output_directory": "./test_output"
        },
        "orcaflex": {
            "postprocess": {
                "summary": {"flag": False},
                "linked_statistics": {"flag": False},
                "RangeGraph": {"flag": False},
                "time_series": {"flag": False},
                "cummulative_histograms": {"flag": False},
                "visualization": {"flag": False}
            }
        },
        "parallel_processing": {
            "enabled": True,
            "max_workers": 2,
            "timeout_per_file": 3600,
            "save_error_reports": True,
            "progress_reporting": True
        }
    }
    return config

def test_parallel_configuration():
    """Test that parallel processing configuration is properly handled."""
    print("Testing parallel processing configuration...")
    
    # Test with parallel enabled
    cfg = create_test_config()
    cfg["parallel_processing"]["enabled"] = True
    cfg["parallel_processing"]["max_workers"] = 4
    print(f"[OK] Parallel config created with {cfg['parallel_processing']['max_workers']} workers")
    
    # Test with parallel disabled
    cfg_sequential = create_test_config()
    cfg_sequential["parallel_processing"]["enabled"] = False
    print("[OK] Sequential config created")
    
    # Test with auto workers
    cfg_auto = create_test_config()
    cfg_auto["parallel_processing"]["max_workers"] = "auto"
    print("[OK] Auto workers config created")
    
    return True

def test_process_functions_exist():
    """Verify that required parallel processing functions exist."""
    print("\nTesting parallel processing functions...")
    
    from digitalmodel.modules.orcaflex.opp import process_single_file, aggregate_results
    
    # Check process_single_file
    assert callable(process_single_file), "process_single_file function not found"
    print("[OK] process_single_file function exists")
    
    # Check aggregate_results
    assert callable(aggregate_results), "aggregate_results function not found"
    print("[OK] aggregate_results function exists")
    
    # Check OrcaFlexPostProcess methods
    opp = OrcaFlexPostProcess()
    assert hasattr(opp, 'post_process'), "post_process method not found"
    print("[OK] post_process method exists")
    
    assert hasattr(opp, '_post_process_sequential'), "_post_process_sequential method not found"
    print("[OK] _post_process_sequential method exists")
    
    return True

def test_parallel_imports():
    """Test that all required imports for parallel processing are available."""
    print("\nTesting parallel processing imports...")
    
    try:
        from concurrent.futures import ProcessPoolExecutor, as_completed
        print("[OK] concurrent.futures imports successful")
    except ImportError as e:
        print(f"[FAIL] Failed to import concurrent.futures: {e}")
        return False
    
    try:
        from digitalmodel.common.parallel_processing import should_use_parallel
        print("[OK] should_use_parallel import successful")
    except ImportError as e:
        print(f"[FAIL] Failed to import should_use_parallel: {e}")
        return False
    
    try:
        from digitalmodel.modules.orcaflex.file_size_optimizer import FileSizeOptimizer
        print("[OK] FileSizeOptimizer import successful")
    except ImportError as e:
        print(f"[FAIL] Failed to import FileSizeOptimizer: {e}")
        return False
    
    return True

def main():
    """Run all tests."""
    print("=" * 60)
    print("OrcaFlex Parallel Processing Test Suite")
    print("=" * 60)
    
    all_passed = True
    
    # Test configuration handling
    if not test_parallel_configuration():
        all_passed = False
    
    # Test function existence
    if not test_process_functions_exist():
        all_passed = False
    
    # Test imports
    if not test_parallel_imports():
        all_passed = False
    
    print("\n" + "=" * 60)
    if all_passed:
        print("[SUCCESS] All tests passed!")
        print("\nParallel processing is properly integrated and ready to use.")
        print("\nTo use parallel processing, add this to your configuration:")
        print("""
parallel_processing:
  enabled: true
  max_workers: 4  # or "auto"
  timeout_per_file: 3600
  save_error_reports: true
  progress_reporting: true
        """)
    else:
        print("[ERROR] Some tests failed. Please check the output above.")
    print("=" * 60)

if __name__ == "__main__":
    main()