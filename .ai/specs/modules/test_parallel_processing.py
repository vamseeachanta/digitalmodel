#!/usr/bin/env python
"""
Test script for parallel processing in OrcaFlex post-processing.
This script demonstrates how to use the new parallel processing feature.
"""

import time
import logging
from digitalmodel.modules.orcaflex.opp import OrcaFlexPostProcess

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)

def create_test_config():
    """Create a test configuration with parallel processing enabled."""
    return {
        "meta": {
            "basename": "test_parallel"
        },
        "orcaflex": {
            "postprocess": {
                "summary": {"flag": True},
                "linked_statistics": {"flag": True},
                "RangeGraph": {"flag": True},
                "time_series": {"flag": True},
                "cummulative_histograms": {"flag": False},
                "visualization": {"flag": False}
            }
        },
        "file_management": {
            "input_files": {
                "sim": [
                    "simulation1.sim",
                    "simulation2.sim",
                    "simulation3.sim",
                    "simulation4.sim"
                ]
            }
        },
        "parallel_processing": {
            "enabled": True,
            "max_workers": 4,
            "timeout_per_file": 3600,
            "save_error_reports": True,
            "progress_reporting": True
        },
        "summary_settings": {
            "groups": []
        },
        "linked_statistics_settings": {
            "groups": []
        },
        "time_series_settings": {
            "data": True,
            "groups": []
        }
    }

def test_parallel_processing():
    """Test the parallel processing functionality."""
    print("Testing OrcaFlex Parallel Post-Processing")
    print("=" * 50)
    
    # Create configuration
    cfg = create_test_config()
    
    # Create post-processor instance
    opp = OrcaFlexPostProcess()
    
    # Test with parallel processing enabled
    print("\n1. Testing with parallel processing ENABLED:")
    start_time = time.time()
    try:
        result_cfg = opp.post_process_router(cfg)
        elapsed = time.time() - start_time
        print(f"   Completed in {elapsed:.2f} seconds")
    except Exception as e:
        print(f"   Error: {str(e)}")
    
    # Test with parallel processing disabled
    print("\n2. Testing with parallel processing DISABLED:")
    cfg["parallel_processing"]["enabled"] = False
    start_time = time.time()
    try:
        result_cfg = opp.post_process_router(cfg)
        elapsed = time.time() - start_time
        print(f"   Completed in {elapsed:.2f} seconds")
    except Exception as e:
        print(f"   Error: {str(e)}")
    
    print("\n" + "=" * 50)
    print("Test completed!")

def benchmark_performance():
    """Benchmark parallel vs sequential performance."""
    print("\nPerformance Benchmark")
    print("=" * 50)
    
    # Test with different numbers of files
    file_counts = [1, 5, 10, 20]
    
    for count in file_counts:
        print(f"\nTesting with {count} files:")
        
        # Create config with specified number of files
        cfg = create_test_config()
        cfg["file_management"]["input_files"]["sim"] = [
            f"simulation{i}.sim" for i in range(1, count + 1)
        ]
        
        opp = OrcaFlexPostProcess()
        
        # Sequential timing
        cfg["parallel_processing"]["enabled"] = False
        start = time.time()
        try:
            opp.post_process_router(cfg)
            seq_time = time.time() - start
        except:
            seq_time = None
        
        # Parallel timing
        cfg["parallel_processing"]["enabled"] = True
        start = time.time()
        try:
            opp.post_process_router(cfg)
            par_time = time.time() - start
        except:
            par_time = None
        
        if seq_time and par_time:
            speedup = seq_time / par_time
            print(f"  Sequential: {seq_time:.2f}s")
            print(f"  Parallel:   {par_time:.2f}s")
            print(f"  Speedup:    {speedup:.2f}x")
        else:
            print("  Error during processing")

if __name__ == "__main__":
    test_parallel_processing()
    # Uncomment to run performance benchmark
    # benchmark_performance()