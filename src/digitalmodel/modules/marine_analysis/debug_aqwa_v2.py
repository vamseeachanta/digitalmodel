"""Debug AQWA V2 parsing in detail."""

from src.digitalmodel.modules.marine_analysis.aqwa_reader_v2 import AQWAReaderV2

def debug_parsing_v2():
    """Debug the V2 parser step by step."""
    
    parser = AQWAReaderV2()
    
    # Test file reading
    file_path = "tests/modules/rao_analysis/NO_DAMP_FST1_L015.LIS"
    
    try:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
        
        print("File read successfully")
        
        # Find displacement sections
        sections = parser._find_displacement_rao_sections(content)
        print(f"Found {len(sections)} RAO sections")
        
        if sections:
            last_section = sections[-1]
            print(f"Last section length: {len(last_section)} characters")
            
            # Show first part of last section
            lines = last_section.split('\n')
            print(f"First 15 lines of last section:")
            for i, line in enumerate(lines[:15]):
                print(f"{i:2d}: '{line}'")
            
            # Try parsing
            try:
                rao_data = parser._parse_rao_section_robust(last_section)
                print(f"\nParsing successful!")
                print(f"Found frequencies: {list(rao_data.keys())}")
                
                if rao_data:
                    first_freq = list(rao_data.keys())[0]
                    print(f"First frequency {first_freq} has headings: {list(rao_data[first_freq].keys())}")
                    
                    if rao_data[first_freq]:
                        first_heading = list(rao_data[first_freq].keys())[0]
                        print(f"First data point: {rao_data[first_freq][first_heading]}")
                else:
                    print("No RAO data extracted!")
                    
            except Exception as e:
                print(f"Parsing failed: {e}")
                import traceback
                traceback.print_exc()
        
    except Exception as e:
        print(f"File read failed: {e}")

if __name__ == "__main__":
    debug_parsing_v2()