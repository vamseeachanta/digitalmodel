"""Debug the fixed-width parser."""

from src.digitalmodel.modules.marine_analysis.aqwa_reader_fixed import AQWAReaderFixed

def debug_fixed_parser():
    """Debug the fixed parser with test data."""
    
    # Test data with all headings
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
    
    # Create parser
    parser = AQWAReaderFixed()
    
    # Parse the test data
    raw_data = parser._parse_rao_section_fixed(test_content)
    
    print("Parsing results:")
    for freq in sorted(raw_data.keys()):
        headings = sorted(list(raw_data[freq].keys()))
        print(f"\nFrequency {freq}:")
        print(f"  Headings: {headings}")
        print(f"  Number of headings: {len(headings)}")
        
        # Show a sample value
        if headings:
            h = headings[0]
            surge_amp = raw_data[freq][h]['surge']['amplitude']
            surge_phase = raw_data[freq][h]['surge']['phase']
            print(f"  Sample (heading {h}, surge): amp={surge_amp}, phase={surge_phase}")

if __name__ == "__main__":
    debug_fixed_parser()