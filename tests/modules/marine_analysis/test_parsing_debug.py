"""Debug the parsing to see what's happening with the missing headings."""

from src.digitalmodel.modules.marine_analysis.aqwa_reader_v2 import AQWAReaderV2
import os

def debug_parsing():
    """Debug the parsing with detailed output."""
    
    # Create a simple test case
    test_content = """
                                    R.A.O.S-VARIATION WITH WAVE DIRECTION
                                    -------------------------------------
 PERIOD   FREQ   DIRECTION          X                 Y                 Z                RX                RY                RZ
 ------   -----  --------- -----------------------------------------------------------------------------------------------------------
 (SECS)  (RAD/S) (DEGREES)    AMP      PHASE    AMP      PHASE    AMP      PHASE    AMP      PHASE    AMP      PHASE    AMP      PHASE
 -------------------------------------------------------------------------------------------------------------------------------------

   40.00   0.157   -180.00    1.0099  -88.22    0.0000  -33.08    0.9912   -0.03    0.0014   47.25    0.1455   92.26    0.0011  -88.75
                   -135.00    0.7173  -88.71    0.7069  -88.97    0.9957   -0.03    0.2032  -73.56    0.1032   92.38    0.1195  120.73
                    -90.00    0.0006   -0.25    1.0040  -89.99    1.0001   -0.01    0.2789  -89.79    0.0028 -179.62    0.1484   90.01
                    -45.00    0.7173   88.73    0.7068  -91.01    0.9955    0.01    0.2041 -105.80    0.1032  -92.38    0.1210   59.67
                      0.00    1.0100   88.22    0.0000   34.39    0.9911    0.03    0.0014  -47.25    0.1455  -92.26    0.0011   88.76
                     45.00    0.7174   88.71    0.7069   88.97    0.9957    0.02    0.2032   73.56    0.1032  -92.38    0.1196 -120.73
                     90.00    0.0006    1.13    1.0040   89.99    1.0001    0.01    0.2789   89.79    0.0028  179.64    0.1484  -90.02
                    135.00    0.7173  -88.73    0.7068   91.01    0.9955   -0.01    0.2041  105.79    0.1032   92.38    0.1210  -59.67
                    180.00    1.0099  -88.22    0.0000  -32.43    0.9912   -0.03    0.0014   47.25    0.1455   92.26    0.0011  -88.75
"""
    
    # Create reader
    reader = AQWAReaderV2()
    
    # Parse with debug
    lines = test_content.split('\n')
    
    # Find header
    header_idx = -1
    for i, line in enumerate(lines):
        if reader.header_pattern.search(line):
            header_idx = i
            print(f"Header found at line {i}")
            break
    
    # Process data lines
    if header_idx != -1:
        data_start = header_idx + 4
        current_freq = None
        parsed_count = 0
        
        for line_idx in range(data_start, len(lines)):
            line = lines[line_idx].strip()
            
            if not line:
                continue
                
            parts = line.split()
            if len(parts) < 3:
                continue
                
            print(f"\nLine {line_idx}: {len(parts)} parts")
            print(f"  First 3 parts: {parts[:3] if len(parts) >= 3 else parts}")
            
            try:
                if len(parts) >= 15:  # Full line
                    period = float(parts[0])
                    freq = float(parts[1])
                    direction = float(parts[2])
                    current_freq = freq
                    print(f"  Full line: freq={freq}, direction={direction}")
                    parsed_count += 1
                elif len(parts) >= 13 and current_freq is not None:  # Continuation
                    direction = float(parts[0])
                    print(f"  Continuation line: direction={direction} for freq={current_freq}")
                    parsed_count += 1
            except ValueError as e:
                print(f"  Error parsing: {e}")
        
        print(f"\nTotal lines parsed: {parsed_count}")

if __name__ == "__main__":
    debug_parsing()