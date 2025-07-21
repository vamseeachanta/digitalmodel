"""Clean test of parsing to isolate the issue."""

from src.digitalmodel.modules.marine_analysis.aqwa_reader_fixed import AQWAReaderFixed
import os

def test_clean():
    """Test clean parsing."""
    
    # Read the actual file
    aqwa_file = os.path.join(os.path.dirname(__file__), "tests/modules/rao_analysis/NO_DAMP_FST1_L015.LIS")
    
    with open(aqwa_file, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    
    # Create parser
    parser = AQWAReaderFixed()
    
    # Find RAO sections
    sections = parser._find_displacement_rao_sections(content)
    print(f"Found {len(sections)} RAO sections")
    
    if sections:
        # Check the last section
        last_section = sections[-1]
        
        # Parse it
        raw_data = parser._parse_rao_section_fixed(last_section)
        
        print(f"\nParsing results:")
        print(f"Number of frequencies: {len(raw_data)}")
        
        # Show frequency details
        for freq in sorted(list(raw_data.keys()))[:5]:  # First 5 frequencies
            headings = sorted(list(raw_data[freq].keys()))
            print(f"\nFrequency {freq}:")
            print(f"  Headings: {headings}")
            
            # Check for expected headings
            expected = [-180, -135, -90, -45, 0, 45, 90, 135, 180]
            missing = [h for h in expected if h not in headings]
            if missing:
                print(f"  Missing: {missing}")

if __name__ == "__main__":
    test_clean()