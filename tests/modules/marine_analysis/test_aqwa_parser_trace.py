"""Test AQWA parser with detailed trace."""

from src.digitalmodel.modules.marine_analysis.aqwa_reader_v2 import AQWAReaderV2

def test_parser_with_trace():
    """Test the parser and trace what's happening."""
    
    # Simple test data that mimics AQWA format
    test_content = """
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
    
    # Create reader and parse
    reader = AQWAReaderV2()
    
    # Parse with instrumentation
    rao_data = reader._parse_rao_section_robust(test_content)
    
    print("Parsing results:")
    print(f"Number of frequencies: {len(rao_data)}")
    
    for freq in sorted(rao_data.keys()):
        headings = sorted(list(rao_data[freq].keys()))
        print(f"\nFrequency {freq}:")
        print(f"  Headings: {headings}")
        print(f"  Number of headings: {len(headings)}")

if __name__ == "__main__":
    test_parser_with_trace()