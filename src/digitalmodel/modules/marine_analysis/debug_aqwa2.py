"""Debug AQWA parsing in detail."""

import re
from src.digitalmodel.modules.marine_analysis.aqwa_reader import AQWAReader

def debug_parsing():
    """Debug the actual parsing step by step."""
    
    # Get the last section
    aqwa_file = "tests/modules/rao_analysis/NO_DAMP_FST1_L015.LIS"
    
    with open(aqwa_file, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    
    reader = AQWAReader()
    sections = reader._find_all_rao_sections(content)
    last_section = sections[-1]
    
    # Show the section more clearly
    lines = last_section.split('\n')
    print("Section lines (first 20):")
    for i, line in enumerate(lines[:20]):
        print(f"{i:2d}: '{line}'")
    
    # Test the header pattern
    print(f"\nTesting header pattern...")
    for i, line in enumerate(lines):
        if reader.header_pattern.search(line):
            print(f"Header found at line {i}: '{line}'")
            break
    
    # Test the data pattern on a specific line
    print(f"\nTesting data patterns...")
    test_line = "    4.00   1.571   -180.00    0.0088   36.77    0.0000  -88.09    0.0019  -94.72    0.0000  -75.57    0.0030   99.03    0.0000   -4.76"
    
    match = reader.data_line_pattern.match(test_line)
    if match:
        print(f"Data line matched!")
        print(f"  Period: {match.group(1)}")
        print(f"  Freq: {match.group(2)}")
        print(f"  Direction: {match.group(3)}")
        print(f"  Data portion: '{match.group(4)}'")
        
        # Test DOF extraction
        dof_values = reader._extract_dof_values(match.group(4))
        print(f"  DOF values: {dof_values}")
    else:
        print("Data line did NOT match")
        print(f"Test line: '{test_line}'")

if __name__ == "__main__":
    debug_parsing()