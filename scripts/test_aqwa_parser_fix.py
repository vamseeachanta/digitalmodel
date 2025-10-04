"""
Test AQWA Parser Fix for Heading Inheritance
==============================================

This script tests the fixed AQWA .LIS parser to verify it correctly
extracts all RAO data including rows where direction is inherited.
"""

import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

# Import parser directly to avoid module initialization issues
import importlib.util
spec = importlib.util.spec_from_file_location(
    "aqwa_lis_parser",
    Path(__file__).parent.parent / 'src' / 'digitalmodel' / 'modules' / 'marine_analysis' / 'parsers' / 'aqwa_lis_parser.py'
)
aqwa_module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(aqwa_module)
AQWALISParser = aqwa_module.AQWALISParser

def test_parser_on_file(file_path: Path):
    """Test parser on a specific .LIS file."""
    print(f"\n{'='*80}")
    print(f"Testing: {file_path.name}")
    print(f"{'='*80}\n")

    if not file_path.exists():
        print(f"❌ File not found: {file_path}")
        return

    # Parse the file
    parser = AQWALISParser(str(file_path))
    rao_data = parser.parse()

    if not rao_data:
        print("❌ No RAO data extracted")
        return

    # Analyze results
    print(f"✓ Extracted {len(rao_data)} RAO structures\n")

    for idx, structure in enumerate(rao_data, 1):
        print(f"Structure {idx}: {structure.structure_name}")
        print(f"  Location: {structure.location}")

        if structure.displacement_raos:
            print(f"\n  📊 Displacement RAOs:")
            analyze_rao_section(structure.displacement_raos, "    ")

        if structure.velocity_raos:
            print(f"\n  📊 Velocity RAOs:")
            analyze_rao_section(structure.velocity_raos, "    ")

        if structure.acceleration_raos:
            print(f"\n  📊 Acceleration RAOs:")
            analyze_rao_section(structure.acceleration_raos, "    ")

        print()

def analyze_rao_section(rao_dict, indent=""):
    """Analyze and print statistics for a RAO section."""
    if not rao_dict:
        print(f"{indent}No data")
        return

    # Count frequencies
    num_freqs = len(rao_dict)
    print(f"{indent}Frequencies: {num_freqs}")

    # Count headings for each frequency
    heading_counts = {}
    all_headings = set()

    for freq, heading_data in rao_dict.items():
        num_headings = len(heading_data)
        heading_counts[freq] = num_headings
        all_headings.update(heading_data.keys())

    unique_headings = len(all_headings)
    min_headings = min(heading_counts.values()) if heading_counts else 0
    max_headings = max(heading_counts.values()) if heading_counts else 0
    avg_headings = sum(heading_counts.values()) / len(heading_counts) if heading_counts else 0

    print(f"{indent}Unique headings: {unique_headings}")
    print(f"{indent}Headings per frequency: min={min_headings}, max={max_headings}, avg={avg_headings:.1f}")
    print(f"{indent}Total data points: {sum(heading_counts.values())}")

    # Sample headings
    sample_headings = sorted(list(all_headings))[:5]
    print(f"{indent}Sample headings: {sample_headings}")

    # Check for expected pattern (8 periods × 8 headings = 64 points)
    total_points = sum(heading_counts.values())
    if total_points >= 60:  # Allow some tolerance
        print(f"{indent}✓ Good data coverage (≥60 points)")
    else:
        print(f"{indent}⚠️  Low data coverage (<60 points)")

def main():
    """Main test function."""
    # Test both files
    files_to_test = [
        Path("docs/modules/aqwa/examples/03_dat/001_ship_raos/001_SHIP_RAOS.LIS"),
        Path("specs/modules/orcaflex/mooring-analysis/go-by-fsts-lngc/aqwa_to_ofx/input/FST2L015_FST1L015_HWL.LIS")
    ]

    for file_path in files_to_test:
        test_parser_on_file(file_path)

    print(f"\n{'='*80}")
    print("Testing Complete")
    print(f"{'='*80}\n")

if __name__ == '__main__':
    main()
