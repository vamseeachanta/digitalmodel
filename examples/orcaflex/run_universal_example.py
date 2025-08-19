#!/usr/bin/env python
"""
Example script showing how to use the Universal OrcaFlex Runner from Python.

This script demonstrates various usage patterns and options.
"""

import sys
from pathlib import Path

# Add src to path if running as script
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from digitalmodel.modules.orcaflex.universal import (
    UniversalOrcaFlexRunner,
    StatusReporter
)


def example_basic_usage():
    """Basic usage example."""
    print("=" * 80)
    print("EXAMPLE 1: Basic Usage")
    print("=" * 80)
    
    # Initialize runner
    runner = UniversalOrcaFlexRunner(
        mock_mode=True,  # Use mock mode for demo
        max_workers=10
    )
    
    # Run simulations
    results = runner.run(
        pattern="*.yml",
        input_directory="./test_models",
        output_directory="./test_output"
    )
    
    # Display results
    print(f"\nResults:")
    print(f"  Total: {results.total}")
    print(f"  Successful: {results.successful}")
    print(f"  Failed: {results.failed}")
    print(f"  Success Rate: {results.success_rate:.1f}%")
    
    return results


def example_with_status_reporting():
    """Example with live status reporting."""
    print("\n" + "=" * 80)
    print("EXAMPLE 2: With Status Reporting")
    print("=" * 80)
    
    # Initialize components
    runner = UniversalOrcaFlexRunner(mock_mode=True, verbose=True)
    status_reporter = StatusReporter(enable_colors=True)
    
    # Run with status reporting
    results = runner.run(
        pattern="fsts_*.yml",
        input_directory="./test_models",
        output_directory="./test_output",
        status_reporter=status_reporter
    )
    
    # Display summary
    status_reporter.display_summary()
    
    # Save report
    report_file = Path("./test_report.json")
    status_reporter.save_report(report_file)
    print(f"\nReport saved to: {report_file}")
    
    return results


def example_specific_files():
    """Example processing specific files."""
    print("\n" + "=" * 80)
    print("EXAMPLE 3: Specific Files")
    print("=" * 80)
    
    runner = UniversalOrcaFlexRunner(mock_mode=True)
    
    # Process specific files
    model_files = [
        "model1.yml",
        "model2.dat",
        "subfolder/model3.yml"
    ]
    
    results = runner.run(
        models=model_files,
        output_directory="./specific_output"
    )
    
    print(f"\nProcessed {results.total} specific files")
    print(f"Created: {[f.name for f in results.sim_files_created]}")
    
    return results


def example_with_exclusions():
    """Example with exclusion patterns."""
    print("\n" + "=" * 80)
    print("EXAMPLE 4: With Exclusions")
    print("=" * 80)
    
    runner = UniversalOrcaFlexRunner(mock_mode=True)
    
    results = runner.run(
        pattern="*.yml",
        input_directory="./models",
        output_directory="./filtered_output",
        recursive=True,
        exclude_patterns=[
            "*backup*",
            "*test*",
            "*old*",
            "temp/*"
        ]
    )
    
    print(f"\nProcessed {results.total} files (after exclusions)")
    
    return results


def example_configuration_file():
    """Example using configuration file."""
    print("\n" + "=" * 80)
    print("EXAMPLE 5: Configuration File")
    print("=" * 80)
    
    runner = UniversalOrcaFlexRunner(mock_mode=True)
    
    # Assuming config file exists
    config_file = Path("./batch_config.yml")
    
    if config_file.exists():
        results = runner.run(config_file=config_file)
        print(f"\nProcessed using config: {config_file}")
    else:
        print(f"\nConfig file not found: {config_file}")
        print("Create it with: /orcaflex-universal --create-config")
        results = None
    
    return results


def example_parallel_vs_sequential():
    """Example comparing parallel vs sequential processing."""
    print("\n" + "=" * 80)
    print("EXAMPLE 6: Parallel vs Sequential")
    print("=" * 80)
    
    runner = UniversalOrcaFlexRunner(mock_mode=True)
    
    # Parallel processing
    print("\nParallel Processing:")
    results_parallel = runner.run(
        pattern="*.yml",
        input_directory="./models",
        output_directory="./parallel_output",
        parallel=True,
        max_workers=10
    )
    print(f"  Time: {results_parallel.total_time:.2f}s")
    
    # Sequential processing
    print("\nSequential Processing:")
    results_sequential = runner.run(
        pattern="*.yml",
        input_directory="./models",
        output_directory="./sequential_output",
        parallel=False
    )
    print(f"  Time: {results_sequential.total_time:.2f}s")
    
    if results_parallel.total_time > 0 and results_sequential.total_time > 0:
        speedup = results_sequential.total_time / results_parallel.total_time
        print(f"\nSpeedup: {speedup:.2f}x")
    
    return results_parallel, results_sequential


def example_error_handling():
    """Example showing error handling."""
    print("\n" + "=" * 80)
    print("EXAMPLE 7: Error Handling")
    print("=" * 80)
    
    runner = UniversalOrcaFlexRunner(mock_mode=True)
    status_reporter = StatusReporter()
    
    # Process with some problematic files
    results = runner.run(
        pattern="*",  # All files (including non-models)
        input_directory="./mixed_files",
        output_directory="./error_test_output",
        status_reporter=status_reporter
    )
    
    print(f"\nProcessing Results:")
    print(f"  Total: {results.total}")
    print(f"  Successful: {results.successful}")
    print(f"  Failed: {results.failed}")
    
    if results.error_details:
        print(f"\nErrors encountered:")
        for error in results.error_details[:3]:
            print(f"  - {error['model']}: {error['error'][:50]}...")
    
    return results


def main():
    """Run all examples."""
    print("UNIVERSAL ORCAFLEX RUNNER - EXAMPLES")
    print("=" * 80)
    
    # Create test models for demo
    test_dir = Path("./test_models")
    test_dir.mkdir(exist_ok=True)
    
    # Create some test files
    for i in range(5):
        model_file = test_dir / f"model_{i}.yml"
        model_file.write_text(f"# Test model {i}\nGeneral:\n  SimTime: {100 * (i+1)}")
    
    for i in range(3):
        fsts_file = test_dir / f"fsts_model_{i}.yml"
        fsts_file.write_text(f"# FSTS model {i}\nEnvironment:\n  WaterDepth: {50 * (i+1)}")
    
    # Run examples
    try:
        # Run each example
        example_basic_usage()
        example_with_status_reporting()
        # example_specific_files()  # Skip if files don't exist
        # example_with_exclusions()
        # example_configuration_file()
        # example_parallel_vs_sequential()
        # example_error_handling()
        
        print("\n" + "=" * 80)
        print("ALL EXAMPLES COMPLETED SUCCESSFULLY")
        print("=" * 80)
        
    except Exception as e:
        print(f"\nError running examples: {e}")
        import traceback
        traceback.print_exc()
    
    # Cleanup
    import shutil
    if test_dir.exists():
        shutil.rmtree(test_dir)
    
    for output_dir in ["./test_output", "./specific_output", "./filtered_output",
                       "./parallel_output", "./sequential_output", "./error_test_output"]:
        if Path(output_dir).exists():
            shutil.rmtree(output_dir)


if __name__ == "__main__":
    main()