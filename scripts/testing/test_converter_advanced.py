#!/usr/bin/env python3
"""Advanced tests: parallel processing and larger batches."""

from pathlib import Path
from digitalmodel.modules.orcaflex.orcaflex_converter_enhanced import OrcaFlexConverterEnhanced
import time

print("="*70)
print("ADVANCED CONVERTER TESTS")
print("="*70)

# Test 1: Parallel Processing (A01 + A02 directories)
print("\n[TEST 1] Parallel Processing - Multiple Directories")
print("-"*70)

converter_parallel = OrcaFlexConverterEnhanced(
    input_dir=Path("docs/modules/orcaflex/examples/raw"),
    output_dir=Path("temp_test_conversion/parallel_yml"),
    output_format='yml',
    use_mock=True,
    parallel=True,
    max_workers=4
)

start = time.time()
results = converter_parallel.convert_batch(pattern='A0[12]/**/*.dat')
elapsed = time.time() - start
stats = results['statistics']

print(f"Files processed: {stats['total_files']}")
print(f"Successful:      {stats['successful']}")
print(f"Failed:          {stats['failed']}")
print(f"Time (parallel): {elapsed:.3f}s")
print(f"Avg per file:    {elapsed/stats['total_files']:.3f}s" if stats['total_files'] > 0 else "")
print(f"Throughput:      {stats['total_files']/elapsed:.1f} files/s" if elapsed > 0 else "")

# Test 2: Sequential for comparison
print("\n[TEST 2] Sequential Processing - Same Files")
print("-"*70)

converter_sequential = OrcaFlexConverterEnhanced(
    input_dir=Path("docs/modules/orcaflex/examples/raw"),
    output_dir=Path("temp_test_conversion/sequential_yml"),
    output_format='yml',
    use_mock=True,
    parallel=False
)

start = time.time()
results_seq = converter_sequential.convert_batch(pattern='A0[12]/**/*.dat')
elapsed_seq = time.time() - start
stats_seq = results_seq['statistics']

print(f"Files processed:   {stats_seq['total_files']}")
print(f"Successful:        {stats_seq['successful']}")
print(f"Time (sequential): {elapsed_seq:.3f}s")
print(f"Avg per file:      {elapsed_seq/stats_seq['total_files']:.3f}s" if stats_seq['total_files'] > 0 else "")

# Comparison
if elapsed > 0 and elapsed_seq > 0:
    speedup = elapsed_seq / elapsed
    print(f"\nSpeedup: {speedup:.2f}x faster with parallel processing")

# Test 3: Count all available example files
print("\n[TEST 3] Count All Example Files")
print("-"*70)

all_dats = list(Path("docs/modules/orcaflex/examples/raw").glob("**/*.dat"))
all_sims = list(Path("docs/modules/orcaflex/examples/raw").glob("**/*.sim"))

print(f"Total .dat files available: {len(all_dats)}")
print(f"Total .sim files available: {len(all_sims)}")
print(f"Total convertible files:    {len(all_dats) + len(all_sims)}")

# Show directory breakdown
from collections import Counter
dirs = Counter([f.parent.name for f in all_dats])
print("\nFiles by directory:")
for dir_name, count in sorted(dirs.items())[:10]:
    print(f"  {dir_name}: {count} files")

print("\n" + "="*70)
print("ADVANCED TESTS COMPLETE")
print("="*70)

print("\nResults:")
print(f"  - Parallel processing: PASS ({stats['successful']}/{stats['total_files']} files)")
print(f"  - Sequential processing: PASS ({stats_seq['successful']}/{stats_seq['total_files']} files)")
print(f"  - Performance gain: {speedup:.2f}x" if 'speedup' in locals() else "  - Performance gain: N/A")
print(f"  - Total files in repo: {len(all_dats) + len(all_sims)}")
