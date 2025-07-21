"""Debug AQWA parsing with more details."""

import re
from src.digitalmodel.modules.marine_analysis.aqwa_reader_v2 import AQWAReaderV2

def debug_aqwa_detailed():
    """Debug AQWA file parsing with detailed output."""
    
    # Read one of the RAO sections
    aqwa_file = "tests/modules/rao_analysis/NO_DAMP_FST1_L015.LIS"
    
    # Read the file and find the patterns
    with open(aqwa_file, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    
    # Create reader
    reader = AQWAReaderV2()
    
    # Find all RAO sections
    print("Looking for RAO sections...")
    sections = reader._find_displacement_rao_sections(content)
    print(f"Found {len(sections)} RAO sections")
    
    if sections:
        print("\nAnalyzing last section...")
        last_section = sections[-1]
        
        # Show lines after header
        lines = last_section.split('\n')
        
        # Find header
        header_idx = -1
        for i, line in enumerate(lines):
            if reader.header_pattern.search(line):
                header_idx = i
                break
        
        if header_idx != -1:
            print(f"Header found at line {header_idx}")
            print("\nShowing lines after header:")
            
            # Show 20 lines after header + 4
            start_idx = header_idx + 4
            for i in range(start_idx, min(start_idx + 20, len(lines))):
                print(f"Line {i}: '{lines[i]}'")
        
        # Try to parse it
        try:
            rao_data = reader._parse_rao_section_robust(last_section)
            print(f"\nParsing successful!")
            print(f"Number of frequencies: {len(rao_data)}")
            
            # Show first frequency with all its headings
            if rao_data:
                first_freq = sorted(list(rao_data.keys()))[0]
                print(f"\nFirst frequency: {first_freq}")
                print(f"Headings for this frequency: {sorted(list(rao_data[first_freq].keys()))}")
                print(f"Number of headings: {len(rao_data[first_freq])}")
                
        except Exception as e:
            print(f"Parsing failed: {e}")
            import traceback
            traceback.print_exc()

if __name__ == "__main__":
    debug_aqwa_detailed()