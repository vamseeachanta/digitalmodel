"""Detailed debug of the parser."""

import re

def debug_parser_detailed():
    """Debug parser with detailed output."""
    
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
"""
    
    lines = test_content.split('\n')
    
    # Find header
    header_pattern = re.compile(r'PERIOD\s+FREQ\s+DIRECTION\s+X\s+Y\s+Z\s+RX\s+RY\s+RZ')
    header_idx = -1
    for i, line in enumerate(lines):
        if header_pattern.search(line):
            header_idx = i
            break
    
    print(f"Header found at line {header_idx}")
    
    # Process data lines
    data_start = header_idx + 4
    current_freq = None
    found_headings = {}
    
    for line_idx in range(data_start, min(data_start + 10, len(lines))):
        line = lines[line_idx]
        
        if not line or line.strip().startswith('-'):
            continue
            
        print(f"\nProcessing line {line_idx}: '{line[:60]}...'")
        
        # Check for full line
        period_str = line[0:8].strip()
        freq_str = line[8:16].strip()
        
        if period_str and freq_str:
            try:
                period = float(period_str)
                freq = float(freq_str)
                direction_str = line[16:27].strip()
                direction = float(direction_str)
                
                print(f"  FULL LINE: period={period}, freq={freq}, dir={direction}")
                current_freq = freq
                
                if freq not in found_headings:
                    found_headings[freq] = []
                found_headings[freq].append(direction)
                
            except ValueError as e:
                print(f"  Error parsing full line: {e}")
        else:
            # Continuation line
            print(f"  CONTINUATION LINE")
            line_stripped = line.strip()
            if line_stripped and current_freq is not None:
                parts = line_stripped.split()
                print(f"  Parts: {parts[:3]}")
                if parts:
                    try:
                        direction = float(parts[0])
                        print(f"  Direction: {direction}")
                        
                        if current_freq in found_headings:
                            found_headings[current_freq].append(direction)
                    except ValueError as e:
                        print(f"  Error parsing direction: {e}")
    
    print("\n\nSummary:")
    for freq, headings in found_headings.items():
        print(f"Frequency {freq}: {sorted(headings)}")

if __name__ == "__main__":
    debug_parser_detailed()