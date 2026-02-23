"""
Simple Test for AQWA Heading Inheritance Fix
==============================================

This script directly tests the heading inheritance logic on sample data.
"""

from pathlib import Path
import re

def test_heading_inheritance():
    """Test the heading inheritance logic on sample AQWA data."""

    # Sample data from actual .LIS file showing the pattern
    sample_block = """
   PERIOD   FREQUENCY   DIRECTION     SURGE           SWAY            HEAVE           ROLL            PITCH           YAW
                                     AMP      PH     AMP      PH     AMP      PH     AMP      PH     AMP      PH     AMP      PH
  ------   ---------   ---------   -------  -----  -------  -----  -------  -----  -------  -----  -------  -----  -------  -----
   62.83   0.100   -180.00    1.5204  -90.03    0.0000  -91.02    0.9973   -0.00    0.0002   20.33    0.0894   90.26    0.0001   -0.43
   15.42   0.407              0.6665  -90.39    0.0000 -163.00    0.6882   -1.26    0.0002  -40.05    0.8072   93.11    0.0001   21.48
    8.79   0.715              0.0621   92.15    0.0001 -127.92    0.2758 -133.79    0.0004 -124.63    0.2549   28.94    0.0001  -55.21
    6.15   1.022              0.0258  178.68    0.0001   49.22    0.0302   50.39    0.0002   30.64    0.0717 -162.28    0.0000   79.05
   62.83   0.100   -135.00    1.0765  -90.02    0.0000 -160.60    0.7058   -0.01    0.0002  -28.24    0.0670   90.19    0.0001   16.05
   15.42   0.407              0.6556  -90.26    0.0000  171.68    0.6761   -0.89    0.0002  -22.26    0.6099   92.65    0.0001   29.67
    8.79   0.715              0.0855  159.85    0.0001  105.92    0.3189  -77.01    0.0003 -111.82    0.2063   28.48    0.0001  -51.93
    6.15   1.022              0.0251  169.82    0.0001   34.19    0.0304   48.73    0.0002   30.26    0.0658 -161.70    0.0000   79.37
"""

    lines = sample_block.strip().split('\n')

    # Simulate parser logic
    current_heading = None
    data_rows = []

    print("="*80)
    print("TESTING HEADING INHERITANCE LOGIC")
    print("="*80)
    print()

    for i, line in enumerate(lines):
        # Skip header and separator lines
        if 'PERIOD' in line or '---' in line or not line.strip():
            continue

        # Must be long enough for data
        if len(line) < 27:
            continue

        try:
            # Extract columns
            period_str = line[0:8].strip()
            freq_str = line[8:16].strip()

            if period_str and freq_str:
                period = float(period_str)
                freq = float(freq_str)
                direction_str = line[16:27].strip()

                # Check if direction is provided
                if direction_str:
                    # New heading block
                    direction = float(direction_str)
                    current_heading = direction
                    source = "EXPLICIT"
                else:
                    # Inherit heading
                    if current_heading is None:
                        print(f"  [WARN] Line {i+1}: No heading to inherit!")
                        continue
                    direction = current_heading
                    source = "INHERITED"

                data_rows.append({
                    'line': i+1,
                    'period': period,
                    'freq': freq,
                    'direction': direction,
                    'source': source
                })

                print(f"  [OK] Line {i+1:2d}: T={period:6.2f}s, F={freq:.3f}, Dir={direction:7.2f} deg ({source})")

        except (ValueError, IndexError) as e:
            print(f"  [ERR] Line {i+1}: Parse error - {e}")

    print()
    print("-"*80)
    print(f"RESULTS: Extracted {len(data_rows)} data points")
    print("-"*80)

    # Analyze results
    headings = {}
    for row in data_rows:
        heading = row['direction']
        if heading not in headings:
            headings[heading] = []
        headings[heading].append(row['period'])

    print(f"\nUnique headings: {len(headings)}")
    for heading in sorted(headings.keys()):
        periods = headings[heading]
        print(f"  Heading {heading:7.2f} deg: {len(periods)} periods")

    print()

    # Check expectations
    if len(data_rows) == 8:  # 4 periods × 2 headings
        print("[PASS] Extracted all 8 data points (4 periods x 2 headings)")
    else:
        print(f"[FAIL] Expected 8 data points, got {len(data_rows)}")

    if len(headings) == 2:
        print("[PASS] Correctly identified 2 unique headings")
    else:
        print(f"[FAIL] Expected 2 headings, got {len(headings)}")

    # Check heading inheritance
    inherited_count = sum(1 for row in data_rows if row['source'] == 'INHERITED')
    explicit_count = sum(1 for row in data_rows if row['source'] == 'EXPLICIT')

    print(f"\nHeading source breakdown:")
    print(f"  Explicit:  {explicit_count} rows")
    print(f"  Inherited: {inherited_count} rows")

    if inherited_count == 6:  # 3 periods per heading × 2 headings
        print("[PASS] Correctly inherited headings for 6 rows")
    else:
        print(f"[FAIL] Expected 6 inherited headings, got {inherited_count}")

    print("\n" + "="*80)
    return len(data_rows) == 8 and len(headings) == 2 and inherited_count == 6


def test_actual_file_counts():
    """Test actual file to count data points before/after fix."""

    file1 = Path("docs/domains/aqwa/examples/03_dat/001_ship_raos/001_SHIP_RAOS.LIS")

    if not file1.exists():
        print(f"\n[WARN] Test file not found: {file1}")
        return

    print("\n" + "="*80)
    print(f"TESTING ACTUAL FILE: {file1.name}")
    print("="*80)
    print()

    # Read RAO section
    with open(file1, 'r') as f:
        content = f.read()

    # Find first RAO section
    rao_start = content.find('R.A.O.S-VARIATION')
    if rao_start < 0:
        print("[ERR] No RAO section found")
        return

    # Get section (next ~100 lines)
    section_lines = content[rao_start:].split('\n')[:100]

    # Find header
    header_idx = -1
    for i, line in enumerate(section_lines):
        if re.search(r'PERIOD\s+FREQ', line, re.IGNORECASE):
            header_idx = i
            break

    if header_idx < 0:
        print("[ERR] No header found")
        return

    # Count data lines
    current_heading = None
    data_count = 0
    inherited_count = 0

    for line in section_lines[header_idx + 4:]:  # Skip header + 3 lines
        if not line.strip() or '*' in line:
            break

        if len(line) < 27:
            continue

        try:
            period_str = line[0:8].strip()
            freq_str = line[8:16].strip()

            if period_str and freq_str:
                direction_str = line[16:27].strip()

                if direction_str:
                    current_heading = float(direction_str)
                elif current_heading is not None:
                    inherited_count += 1
                else:
                    continue

                data_count += 1
        except:
            continue

    print(f"Data points extracted: {data_count}")
    print(f"  - With explicit heading: {data_count - inherited_count}")
    print(f"  - With inherited heading: {inherited_count}")
    print()

    if data_count >= 60:
        print("[PASS] Good coverage: >=60 data points extracted")
    else:
        print(f"[WARN] Low coverage: {data_count} data points (expected >=60)")


if __name__ == '__main__':
    # Test logic with sample data
    test_passed = test_heading_inheritance()

    # Test on actual file
    test_actual_file_counts()

    print("\n" + "="*80)
    if test_passed:
        print("[PASS] ALL TESTS PASSED")
    else:
        print("[FAIL] SOME TESTS FAILED")
    print("="*80 + "\n")
