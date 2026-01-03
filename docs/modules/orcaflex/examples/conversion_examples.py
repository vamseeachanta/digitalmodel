#!/usr/bin/env python3
"""
ABOUTME: Practical usage examples for the enhanced OrcaFlex file converter.

OrcaFlex Converter - Usage Examples
====================================

Demonstrates all conversion capabilities with practical examples:
- Single file conversions
- Batch processing
- Bidirectional workflows
- Round-trip validation
- Integration patterns
"""

from pathlib import Path
from digitalmodel.modules.orcaflex.orcaflex_converter_enhanced import OrcaFlexConverterEnhanced


# ============================================================================
# EXAMPLE 1: Basic Single File Conversion (.dat → .yml)
# ============================================================================

def example_1_single_dat_to_yml():
    """Convert a single .dat file to .yml format."""
    print("\n" + "="*70)
    print("EXAMPLE 1: Single File Conversion (.dat → .yml)")
    print("="*70)

    # Create converter
    converter = OrcaFlexConverterEnhanced(
        output_format='yml',
        use_mock=False,  # Set to True if no OrcaFlex license
        validate=True
    )

    # Convert file
    input_file = Path("docs/modules/orcaflex/examples/raw/A01/braceline.dat")

    if not input_file.exists():
        print(f"⚠ File not found: {input_file}")
        print("This example requires OrcaFlex example files.")
        return

    success, output_file, error = converter.convert_file(input_file)

    if success:
        print(f"✓ Success!")
        print(f"  Input:  {input_file}")
        print(f"  Output: {output_file}")
        print(f"  Size:   {output_file.stat().st_size} bytes")
    else:
        print(f"✗ Failed: {error}")


# ============================================================================
# EXAMPLE 2: Bidirectional Conversion (.yml → .dat)
# ============================================================================

def example_2_yml_to_dat():
    """Convert a YAML file back to .dat format."""
    print("\n" + "="*70)
    print("EXAMPLE 2: Bidirectional Conversion (.yml → .dat)")
    print("="*70)

    # First: Create a YAML file from .dat
    converter_to_yml = OrcaFlexConverterEnhanced(
        output_dir=Path("temp_conversion"),
        output_format='yml',
        use_mock=False
    )

    dat_file = Path("docs/modules/orcaflex/examples/raw/A01/braceline.dat")

    if not dat_file.exists():
        print(f"⚠ File not found: {dat_file}")
        return

    print("Step 1: Converting .dat → .yml")
    success, yml_file, _ = converter_to_yml.convert_file(dat_file)

    if not success:
        print("  ✗ Failed to create YAML")
        return

    print(f"  ✓ Created: {yml_file}")

    # Second: Convert YAML back to .dat
    converter_to_dat = OrcaFlexConverterEnhanced(
        output_dir=Path("temp_conversion"),
        output_format='dat',
        use_mock=False
    )

    print("\nStep 2: Converting .yml → .dat")
    success, new_dat, _ = converter_to_dat.convert_file(yml_file)

    if success:
        print(f"  ✓ Created: {new_dat}")
        print(f"  Original size: {dat_file.stat().st_size} bytes")
        print(f"  New size:      {new_dat.stat().st_size} bytes")

        # Calculate size difference
        size_diff = abs(new_dat.stat().st_size - dat_file.stat().st_size)
        size_diff_pct = (size_diff / dat_file.stat().st_size) * 100
        print(f"  Difference:    {size_diff_pct:.2f}%")
    else:
        print(f"  ✗ Failed")


# ============================================================================
# EXAMPLE 3: Batch Conversion with Pattern Matching
# ============================================================================

def example_3_batch_conversion():
    """Convert multiple files in batch with pattern matching."""
    print("\n" + "="*70)
    print("EXAMPLE 3: Batch Conversion with Pattern")
    print("="*70)

    input_dir = Path("docs/modules/orcaflex/examples/raw")

    if not input_dir.exists():
        print(f"⚠ Directory not found: {input_dir}")
        return

    # Create converter
    converter = OrcaFlexConverterEnhanced(
        input_dir=input_dir,
        output_dir=Path("docs/modules/orcaflex/examples/yaml"),
        output_format='yml',
        use_mock=False,
        validate=True
    )

    print(f"Input directory:  {input_dir}")
    print(f"Output directory: {converter.output_dir}")
    print(f"Pattern: A*.dat (files starting with 'A')")

    # Convert files matching pattern
    results = converter.convert_batch(pattern='A*.dat')
    stats = results['statistics']

    # Display results
    print("\nResults:")
    print(f"  Total files:  {stats['total_files']}")
    print(f"  Successful:   {stats['successful']}")
    print(f"  Failed:       {stats['failed']}")
    print(f"  Skipped:      {stats['skipped']}")
    print(f"  Time:         {stats['processing_time']:.2f}s")


# ============================================================================
# EXAMPLE 4: Parallel Batch Processing
# ============================================================================

def example_4_parallel_processing():
    """Convert files in parallel for faster processing."""
    print("\n" + "="*70)
    print("EXAMPLE 4: Parallel Batch Processing")
    print("="*70)

    input_dir = Path("docs/modules/orcaflex/examples/raw")

    if not input_dir.exists():
        print(f"⚠ Directory not found: {input_dir}")
        return

    # Create converter with parallel processing
    converter = OrcaFlexConverterEnhanced(
        input_dir=input_dir,
        output_dir=Path("docs/modules/orcaflex/examples/yaml_parallel"),
        output_format='yml',
        use_mock=False,  # Set to True for testing without license
        parallel=True,
        max_workers=4
    )

    print(f"Parallel workers: {converter.max_workers}")
    print("Converting all .dat files...")

    # Convert all .dat files
    results = converter.convert_batch(pattern='*.dat')
    stats = results['statistics']

    # Display results
    print("\nResults:")
    print(f"  Total files:     {stats['total_files']}")
    print(f"  Successful:      {stats['successful']}")
    print(f"  Time:            {stats['processing_time']:.2f}s")

    if stats['total_files'] > 0:
        avg_time = stats['processing_time'] / stats['total_files']
        print(f"  Avg per file:    {avg_time:.2f}s")
        print(f"  Throughput:      {stats['total_files']/stats['processing_time']:.2f} files/s")


# ============================================================================
# EXAMPLE 5: Round-Trip Validation
# ============================================================================

def example_5_round_trip_validation():
    """Validate conversion quality with round-trip test."""
    print("\n" + "="*70)
    print("EXAMPLE 5: Round-Trip Validation")
    print("="*70)

    original_dat = Path("docs/modules/orcaflex/examples/raw/A01/braceline.dat")

    if not original_dat.exists():
        print(f"⚠ File not found: {original_dat}")
        return

    temp_dir = Path("temp_roundtrip")
    temp_dir.mkdir(exist_ok=True)

    print(f"Original file: {original_dat.name}")
    print(f"Size:          {original_dat.stat().st_size} bytes\n")

    # Step 1: .dat → .yml
    print("Step 1: .dat → .yml")
    converter1 = OrcaFlexConverterEnhanced(
        output_dir=temp_dir,
        output_format='yml'
    )
    success1, yml_file, _ = converter1.convert_file(original_dat)

    if not success1:
        print("  ✗ Failed")
        return

    print(f"  ✓ YAML file: {yml_file.name} ({yml_file.stat().st_size} bytes)")

    # Step 2: .yml → .dat
    print("\nStep 2: .yml → .dat")
    converter2 = OrcaFlexConverterEnhanced(
        output_dir=temp_dir,
        output_format='dat'
    )
    success2, new_dat, _ = converter2.convert_file(yml_file)

    if not success2:
        print("  ✗ Failed")
        return

    print(f"  ✓ New DAT file: {new_dat.name} ({new_dat.stat().st_size} bytes)")

    # Validation
    print("\nValidation:")
    original_size = original_dat.stat().st_size
    new_size = new_dat.stat().st_size
    size_diff = abs(new_size - original_size)
    size_diff_pct = (size_diff / original_size) * 100

    print(f"  Size difference: {size_diff} bytes ({size_diff_pct:.2f}%)")

    if size_diff_pct < 5:
        print("  ✓ PASS: Round-trip conversion validated!")
    elif size_diff_pct < 10:
        print("  ⚠ WARNING: Moderate size difference")
    else:
        print("  ✗ FAIL: Large size difference")


# ============================================================================
# EXAMPLE 6: Mock Mode (No OrcaFlex License)
# ============================================================================

def example_6_mock_mode():
    """Demonstrate mock mode for testing without OrcaFlex."""
    print("\n" + "="*70)
    print("EXAMPLE 6: Mock Mode (No License Required)")
    print("="*70)

    # Create converter in mock mode
    converter = OrcaFlexConverterEnhanced(
        output_dir=Path("temp_mock"),
        output_format='yml',
        use_mock=True,  # Force mock mode
        validate=False
    )

    print("Mode: MOCK (OrcaFlex license not required)")

    input_file = Path("docs/modules/orcaflex/examples/raw/A01/braceline.dat")

    if not input_file.exists():
        print(f"⚠ File not found: {input_file}")
        # Create a dummy file for demonstration
        input_file = Path("temp_mock/test.dat")
        input_file.parent.mkdir(exist_ok=True)
        input_file.write_text("DUMMY DAT FILE")
        print(f"Created dummy file: {input_file}")

    # Convert in mock mode
    success, output_file, error = converter.convert_file(input_file)

    if success:
        print(f"\n✓ Mock conversion successful")
        print(f"  Output: {output_file}")

        # Show mock YAML structure
        import yaml
        with open(output_file) as f:
            data = yaml.safe_load(f)

        print("\nMock YAML structure:")
        print(f"  MockConversion: {data['OrcaFlexModel']['MockConversion']}")
        print(f"  FileType:       {data['OrcaFlexModel']['FileType']}")
    else:
        print(f"✗ Failed: {error}")


# ============================================================================
# EXAMPLE 7: CLI Usage
# ============================================================================

def example_7_cli_usage():
    """Demonstrate command-line interface usage."""
    print("\n" + "="*70)
    print("EXAMPLE 7: Command-Line Interface")
    print("="*70)

    print("\nCLI Commands:\n")

    print("1. Single file conversion:")
    print("   python -m digitalmodel.modules.orcaflex.convert_cli model.dat")

    print("\n2. Specify output format:")
    print("   python -m digitalmodel.modules.orcaflex.convert_cli model.yml --format dat")

    print("\n3. Batch conversion:")
    print("   python -m digitalmodel.modules.orcaflex.convert_cli --batch models/ output/")

    print("\n4. Batch with pattern:")
    print("   python -m digitalmodel.modules.orcaflex.convert_cli --batch models/ output/ --pattern '*.dat'")

    print("\n5. Parallel processing:")
    print("   python -m digitalmodel.modules.orcaflex.convert_cli --batch models/ output/ --parallel --workers 8")

    print("\n6. Mock mode:")
    print("   python -m digitalmodel.modules.orcaflex.convert_cli --batch models/ output/ --mock")

    print("\n7. Get help:")
    print("   python -m digitalmodel.modules.orcaflex.convert_cli --help")


# ============================================================================
# EXAMPLE 8: Integration with Existing Code
# ============================================================================

def example_8_integration():
    """Show integration with existing OrcaFlex workflows."""
    print("\n" + "="*70)
    print("EXAMPLE 8: Integration with OrcaFlex Workflows")
    print("="*70)

    print("\nIntegration Pattern:")
    print("""
# 1. Convert .dat files to .yml for version control
converter = OrcaFlexConverterEnhanced(
    input_dir=Path("models/"),
    output_dir=Path("models_yml/"),
    output_format='yml'
)
converter.convert_batch()

# 2. Modify YAML files (version control friendly)
# ...edit YAML files with your favorite editor...

# 3. Convert back to .dat for OrcaFlex execution
converter_to_dat = OrcaFlexConverterEnhanced(
    input_dir=Path("models_yml/"),
    output_dir=Path("models_executable/"),
    output_format='dat'
)
converter_to_dat.convert_batch()

# 4. Run OrcaFlex simulations with universal runner
from digitalmodel.modules.orcaflex.universal import UniversalOrcaFlexRunner

runner = UniversalOrcaFlexRunner(
    input_directory="models_executable/",
    output_directory="results/.sim/"
)
runner.run_batch(pattern="*.dat")

# 5. Post-process results
from digitalmodel.modules.orcaflex.opp import OrcaFlexPostProcess

opp = OrcaFlexPostProcess()
opp.process_all(sim_directory="results/.sim/")
    """)


# ============================================================================
# Main Runner
# ============================================================================

def run_all_examples():
    """Run all examples."""
    examples = [
        ("Single File Conversion", example_1_single_dat_to_yml),
        ("Bidirectional Conversion", example_2_yml_to_dat),
        ("Batch Conversion", example_3_batch_conversion),
        ("Parallel Processing", example_4_parallel_processing),
        ("Round-Trip Validation", example_5_round_trip_validation),
        ("Mock Mode", example_6_mock_mode),
        ("CLI Usage", example_7_cli_usage),
        ("Integration", example_8_integration),
    ]

    print("\n" + "="*70)
    print("ORCAFLEX CONVERTER - USAGE EXAMPLES")
    print("="*70)

    print("\nAvailable Examples:")
    for i, (name, _) in enumerate(examples, 1):
        print(f"  {i}. {name}")

    print("\nRunning all examples...\n")

    for name, example_func in examples:
        try:
            example_func()
        except Exception as e:
            print(f"\n✗ Example failed: {e}")

    print("\n" + "="*70)
    print("ALL EXAMPLES COMPLETED")
    print("="*70)


if __name__ == "__main__":
    # Run all examples
    run_all_examples()

    # Or run specific examples:
    # example_1_single_dat_to_yml()
    # example_3_batch_conversion()
    # example_4_parallel_processing()
