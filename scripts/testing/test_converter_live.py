#!/usr/bin/env python3
"""Live test of the enhanced OrcaFlex converter with example files."""

from pathlib import Path
from digitalmodel.modules.orcaflex.orcaflex_converter_enhanced import OrcaFlexConverterEnhanced

print("="*70)
print("ORCAFLEX CONVERTER - LIVE TEST WITH EXAMPLE FILES")
print("="*70)

# Test 1: Single file conversion (mock mode)
print("\n[TEST 1] Single File Conversion (Mock Mode)")
print("-"*70)

converter = OrcaFlexConverterEnhanced(
    output_dir=Path("temp_test_conversion"),
    output_format='yml',
    use_mock=True  # Use mock since we don't have OrcaFlex license
)

input_file = Path("docs/modules/orcaflex/examples/raw/A01/A01 Catenary riser.dat")

if input_file.exists():
    print(f"Input:  {input_file}")
    print(f"Size:   {input_file.stat().st_size:,} bytes")

    success, output_file, error = converter.convert_file(input_file)

    if success:
        print(f"Status: SUCCESS")
        print(f"Output: {output_file}")
        print(f"Size:   {output_file.stat().st_size:,} bytes")
    else:
        print(f"Status: FAILED")
        print(f"Error:  {error}")
else:
    print(f"ERROR: File not found: {input_file}")

# Test 2: Batch conversion (A01 directory, mock mode)
print("\n[TEST 2] Batch Conversion - A01 Directory (Mock Mode)")
print("-"*70)

converter_batch = OrcaFlexConverterEnhanced(
    input_dir=Path("docs/modules/orcaflex/examples/raw/A01"),
    output_dir=Path("temp_test_conversion/A01_yml"),
    output_format='yml',
    use_mock=True,
    validate=False,
    parallel=False
)

results = converter_batch.convert_batch(pattern='*.dat')
stats = results['statistics']

print(f"Total files:    {stats['total_files']}")
print(f"Successful:     {stats['successful']}")
print(f"Failed:         {stats['failed']}")
print(f"Skipped:        {stats['skipped']}")
print(f"Time:           {stats['processing_time']:.2f}s")

if stats['total_files'] > 0:
    avg_time = stats['processing_time'] / stats['total_files']
    print(f"Avg per file:   {avg_time:.3f}s")

# Test 3: Check output files
print("\n[TEST 3] Verify Output Files")
print("-"*70)

output_files = list(Path("temp_test_conversion/A01_yml").glob("*.yml"))
print(f"YAML files created: {len(output_files)}")

if output_files:
    print("\nSample output files:")
    for f in output_files[:3]:
        print(f"  - {f.name} ({f.stat().st_size:,} bytes)")

# Test 4: Check conversion report
print("\n[TEST 4] Conversion Reports")
print("-"*70)

report_md = Path("temp_test_conversion/A01_yml/conversion_report.md")
report_json = Path("temp_test_conversion/A01_yml/conversion_report.json")

if report_md.exists():
    print(f"[OK] Markdown report: {report_md.name} ({report_md.stat().st_size:,} bytes)")
else:
    print("[MISSING] Markdown report not found")

if report_json.exists():
    print(f"[OK] JSON report: {report_json.name} ({report_json.stat().st_size:,} bytes)")
else:
    print("[MISSING] JSON report not found")

# Test 5: Validate YAML content
print("\n[TEST 5] Validate YAML Content")
print("-"*70)

if output_files:
    import yaml

    sample_file = output_files[0]
    print(f"Inspecting: {sample_file.name}")

    with open(sample_file) as f:
        data = yaml.safe_load(f)

    if 'OrcaFlexModel' in data:
        print("[OK] Contains OrcaFlexModel structure")
        print(f"  - SourceFile: {data['OrcaFlexModel'].get('SourceFile', 'N/A')}")
        print(f"  - FileType: {data['OrcaFlexModel'].get('FileType', 'N/A')}")
        print(f"  - MockConversion: {data['OrcaFlexModel'].get('MockConversion', False)}")
        print(f"  - FileSize: {data['OrcaFlexModel'].get('FileSize', 0):,} bytes")
    else:
        print("[WARNING] Unexpected YAML structure")

print("\n" + "="*70)
print("TEST COMPLETE")
print("="*70)

# Summary
print("\nSummary:")
print(f"  - Single file conversion: {'PASS' if success else 'FAIL'}")
print(f"  - Batch conversion: {'PASS' if stats['successful'] > 0 else 'FAIL'}")
print(f"  - Report generation: {'PASS' if report_md.exists() else 'FAIL'}")
print(f"  - YAML validation: {'PASS' if 'OrcaFlexModel' in data else 'FAIL'}")
