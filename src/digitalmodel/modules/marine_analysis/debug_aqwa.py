"""Debug AQWA parsing."""

import re
from src.digitalmodel.modules.marine_analysis.aqwa_reader import AQWAReader

def debug_aqwa():
    """Debug AQWA file parsing."""
    
    # Read one of the RAO sections
    import os
    aqwa_file = os.path.join(os.path.dirname(__file__), "tests/modules/rao_analysis/NO_DAMP_FST1_L015.LIS")
    
    # Read the file and find the patterns
    with open(aqwa_file, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    
    # Create reader
    reader = AQWAReader()
    
    # Find all RAO sections
    print("Looking for RAO sections...")
    sections = reader._find_all_rao_sections(content)
    print(f"Found {len(sections)} RAO sections")
    
    if sections:
        print("\nAnalyzing last section...")
        last_section = sections[-1]
        
        # Show first 1000 characters of last section
        print("First 1000 chars of last section:")
        print(last_section[:1000])
        print("...")
        
        # Try to parse it
        try:
            rao_data = reader._parse_rao_section(last_section)
            print(f"\nParsing successful!")
            print(f"Frequencies found: {list(rao_data.keys())}")
            if rao_data:
                first_freq = list(rao_data.keys())[0]
                print(f"Headings for freq {first_freq}: {list(rao_data[first_freq].keys())}")
                
                # Show sample data
                first_heading = list(rao_data[first_freq].keys())[0]
                print(f"Sample data for freq {first_freq}, heading {first_heading}:")
                print(rao_data[first_freq][first_heading])
                
        except Exception as e:
            print(f"Parsing failed: {e}")
            import traceback
            traceback.print_exc()

if __name__ == "__main__":
    debug_aqwa()