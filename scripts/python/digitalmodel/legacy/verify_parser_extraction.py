"""
Verify AQWA Parser Extraction
==============================

Direct test of the parser implementation on actual .LIS files.
"""

import sys
from pathlib import Path
import re

# Add src to path
repo_root = Path(__file__).parent.parent
sys.path.insert(0, str(repo_root / 'src'))

# Import parser components directly
exec(open(repo_root / 'src' / 'digitalmodel' / 'modules' / 'marine_analysis' / 'parsers' / 'aqwa_lis_parser.py').read())

def test_file_extraction(file_path):
    """Test extraction on a file."""
    print(f"\n{'='*80}")
    print(f"Testing: {file_path.name}")
    print(f"{'='*80}\n")

    if not file_path.exists():
        print(f"[ERR] File not found: {file_path}")
        return

    # Read file
    with open(file_path, 'r') as f:
        content = f.read()

    # Find RAO sections
    sections = []
    pos = 0
    while True:
        rao_start = content.find('R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY', pos)
        if rao_start < 0:
            break

        # Get next ~100 lines
        section_end = content.find('R.A.O.S-VARIATION', rao_start + 50)
        if section_end < 0:
            section_end = len(content)

        section_text = content[rao_start:section_end]
        sections.append((rao_start, section_text))
        pos = rao_start + 50

    print(f"Found {len(sections)} RAO sections")

    # Test first section
    if sections:
        print(f"\nTesting first section at position {sections[0][0]}...")
        test_rao_section(sections[0][1])

def test_rao_section(section_text):
    """Parse a RAO section and report results."""
    lines = section_text.split('\n')

    # Find header
    header_pattern = re.compile(r'PERIOD\s+FREQ', re.IGNORECASE)
    header_idx = -1
    for i, line in enumerate(lines):
        if header_pattern.search(line):
            header_idx = i
            break

    if header_idx < 0:
        print("[ERR] No header found")
        return

    print(f"Header found at line {header_idx}")

    # Parse data (skip header + 4 lines)
    data_start = header_idx + 4

    rao_data = {}
    current_heading = None
    data_count = 0
    inherited_count = 0

    for line_idx in range(data_start, min(data_start + 100, len(lines))):
        line = lines[line_idx]

        # Skip empty lines
        if not line.strip():
            continue

        # Skip separator lines
        stripped = line.strip()
        if stripped.startswith('-') and not any(c.isdigit() for c in stripped[:15]):
            continue

        # Stop at section boundary
        if '*' in line and len(line.replace('*', '').replace(' ', '')) < 5:
            break

        # Must be long enough for data
        if len(line) < 27:
            continue

        try:
            # Check for data line with period + frequency
            period_str = line[0:8].strip()
            freq_str = line[8:16].strip()

            if period_str and freq_str:
                # Line has period and frequency
                period = float(period_str)
                freq = float(freq_str)
                direction_str = line[16:27].strip()

                # Check if direction is provided on this line
                if direction_str:
                    # New heading block
                    try:
                        direction = float(direction_str)
                        if -180 <= direction <= 360:
                            current_heading = direction
                        else:
                            continue
                    except ValueError:
                        continue
                else:
                    # Inherit heading
                    if current_heading is None:
                        continue
                    direction = current_heading
                    inherited_count += 1

                # Store data
                if freq not in rao_data:
                    rao_data[freq] = {}
                rao_data[freq][direction] = {'period': period}
                data_count += 1

        except (ValueError, IndexError):
            continue

    print(f"\nResults:")
    print(f"  Total data points: {data_count}")
    print(f"  Inherited headings: {inherited_count}")
    print(f"  Explicit headings: {data_count - inherited_count}")
    print(f"  Unique frequencies: {len(rao_data)}")

    # Analyze headings per frequency
    if rao_data:
        all_headings = set()
        for freq, heading_data in rao_data.items():
            all_headings.update(heading_data.keys())

        print(f"  Unique headings: {len(all_headings)}")
        print(f"  Sample headings: {sorted(list(all_headings))[:5]}")

        # Expected: 8 periods x 8 headings = 64 data points
        if data_count >= 60:
            print(f"\n[PASS] Good coverage: {data_count} data points")
        else:
            print(f"\n[WARN] Low coverage: {data_count} data points (expected >=60)")

        # Show breakdown
        heading_counts = {}
        for freq, heading_data in rao_data.items():
            for heading in heading_data.keys():
                if heading not in heading_counts:
                    heading_counts[heading] = 0
                heading_counts[heading] += 1

        print(f"\nData points per heading:")
        for heading in sorted(heading_counts.keys()):
            count = heading_counts[heading]
            print(f"  {heading:7.2f} deg: {count} periods")

def main():
    """Main test."""
    files = [
        Path("docs/modules/aqwa/examples/03_dat/001_ship_raos/001_SHIP_RAOS.LIS"),
        Path("specs/modules/orcaflex/mooring-analysis/go-by-fsts-lngc/aqwa_to_ofx/input/FST2L015_FST1L015_HWL.LIS")
    ]

    for file_path in files:
        test_file_extraction(file_path)

    print(f"\n{'='*80}")
    print("Extraction verification complete")
    print(f"{'='*80}\n")

if __name__ == '__main__':
    main()
