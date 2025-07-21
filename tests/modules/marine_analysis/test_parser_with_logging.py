"""Test parser with logging to understand the issue."""

import re
from typing import Dict, List, Any

class DebugAQWAParser:
    """Debug version of AQWA parser with logging."""
    
    def __init__(self):
        self.header_pattern = re.compile(
            r'PERIOD\s+FREQ\s+DIRECTION\s+X\s+Y\s+Z\s+RX\s+RY\s+RZ'
        )
    
    def parse_section(self, section: str) -> Dict:
        """Parse with detailed logging."""
        lines = section.split('\n')
        
        # Find header
        header_idx = -1
        for i, line in enumerate(lines):
            if self.header_pattern.search(line):
                header_idx = i
                break
        
        if header_idx == -1:
            print("No header found!")
            return {}
        
        data_start = header_idx + 4
        rao_data = {}
        current_freq = None
        
        for line_idx in range(data_start, min(data_start + 20, len(lines))):
            line = lines[line_idx].strip()
            
            if not line or line.startswith('-'):
                continue
            
            parts = line.split()
            if len(parts) < 3:
                continue
            
            print(f"\nLine {line_idx}: '{line[:60]}...'")
            print(f"  Parts count: {len(parts)}")
            
            try:
                if len(parts) >= 15:
                    # This should be a full line
                    period = float(parts[0])
                    freq = float(parts[1])
                    direction = float(parts[2])
                    current_freq = freq
                    
                    print(f"  FULL LINE: period={period}, freq={freq}, dir={direction}")
                    
                    if freq not in rao_data:
                        rao_data[freq] = {}
                    rao_data[freq][direction] = f"data for {direction}"
                    
                elif len(parts) >= 13 and current_freq is not None:
                    # This should be a continuation line
                    # But let's check if it could be mistaken for a full line
                    val1 = float(parts[0])
                    val2 = float(parts[1]) 
                    val3 = float(parts[2])
                    
                    print(f"  CONTINUATION? val1={val1}, val2={val2}, val3={val3}")
                    print(f"  Current freq: {current_freq}")
                    
                    # The first value should be the direction
                    direction = val1
                    
                    # Store under current frequency
                    rao_data[current_freq][direction] = f"data for {direction}"
                    print(f"  Stored as continuation: dir={direction} under freq={current_freq}")
                    
            except Exception as e:
                print(f"  Error: {e}")
        
        # Summary
        print("\n\nSUMMARY:")
        for freq in sorted(rao_data.keys()):
            dirs = sorted(rao_data[freq].keys())
            print(f"Frequency {freq}: {len(dirs)} directions = {dirs}")
        
        return rao_data


# Test with actual data
test_data = """
                                    R.A.O.S-VARIATION WITH WAVE DIRECTION
                                    -------------------------------------
 PERIOD   FREQ   DIRECTION          X                 Y                 Z                RX                RY                RZ
 ------   -----  --------- -----------------------------------------------------------------------------------------------------------
 (SECS)  (RAD/S) (DEGREES)    AMP      PHASE    AMP      PHASE    AMP      PHASE    AMP      PHASE    AMP      PHASE    AMP      PHASE
 -------------------------------------------------------------------------------------------------------------------------------------

    4.00   1.571   -180.00    0.0088   36.77    0.0000  -88.09    0.0019  -94.72    0.0000  -75.57    0.0030   99.03    0.0000   -4.76
                   -135.00    0.0062   82.23    0.0045   72.45    0.0021  -42.31    0.0089   45.98    0.0025  135.60    0.0054   39.99
                    -90.00    0.0008  -13.33    0.0776   46.90    0.0333 -118.40    0.0146  -19.86    0.0095 -178.98    0.0272 -148.97
                    -45.00    0.0056  148.70    0.0034  -78.37    0.0015  162.18    0.0039   95.42    0.0067   77.17    0.0050   82.42
                      0.00    0.0146   65.01    0.0000   57.68    0.0014   88.26    0.0000  115.45    0.0046  106.25    0.0000  121.18
                     45.00    0.0056  147.49    0.0035   99.31    0.0015  158.90    0.0038  -86.30    0.0069   75.57    0.0050  -99.46
                     90.00    0.0008  -17.87    0.0780 -135.17    0.0330 -120.44    0.0141  151.02    0.0094  178.50    0.0274   29.13
                    135.00    0.0062   80.52    0.0046 -109.65    0.0021  -45.51    0.0088 -136.89    0.0025  134.12    0.0054 -141.47
                    180.00    0.0088   36.77    0.0000  -88.10    0.0019  -94.72    0.0000  -75.60    0.0030   99.03    0.0000   -4.77
"""

parser = DebugAQWAParser()
parser.parse_section(test_data)