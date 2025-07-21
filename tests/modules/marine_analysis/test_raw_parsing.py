"""Test raw parsing to see all headings."""

from src.digitalmodel.modules.marine_analysis.aqwa_reader_v2 import AQWAReaderV2
import os

def test_raw_parsing():
    """Test raw parsing results."""
    
    # Test file path
    aqwa_file = os.path.join(os.path.dirname(__file__), "tests/modules/rao_analysis/NO_DAMP_FST1_L015.LIS")
    
    # Read file content
    with open(aqwa_file, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    
    # Create reader
    reader = AQWAReaderV2()
    
    # Find RAO sections
    sections = reader._find_displacement_rao_sections(content)
    print(f"Found {len(sections)} RAO sections")
    
    if sections:
        # Parse the last section
        last_section = sections[-1]
        raw_data = reader._parse_rao_section_robust(last_section)
        
        print(f"\nRaw parsing results:")
        print(f"Number of frequencies: {len(raw_data)}")
        
        # Show first few frequencies and their headings
        for i, freq in enumerate(sorted(list(raw_data.keys()))[:5]):
            headings = sorted(list(raw_data[freq].keys()))
            print(f"\nFrequency {freq}: {len(headings)} headings")
            print(f"  Headings: {headings}")

if __name__ == "__main__":
    test_raw_parsing()