"""Test actual line parts from the file."""

def test_actual_parts():
    """Test parsing actual lines from the file."""
    
    # Actual lines from the AQWA file
    test_lines = [
        "    4.00   1.571   -180.00    0.0088   36.77    0.0000  -88.09    0.0019  -94.72    0.0000  -75.57    0.0030   99.03    0.0000   -4.76",
        "                   -135.00    0.0062   82.23    0.0045   72.45    0.0021  -42.31    0.0089   45.98    0.0025  135.60    0.0054   39.99",
        "                    -90.00    0.0008  -13.33    0.0776   46.90    0.0333 -118.40    0.0146  -19.86    0.0095 -178.98    0.0272 -148.97"
    ]
    
    for i, line in enumerate(test_lines):
        parts = line.split()
        print(f"Line {i}: {len(parts)} parts")
        print(f"  Parts: {parts}")
        
        # Check if first three could be period, freq, direction
        if len(parts) >= 3:
            try:
                val1 = float(parts[0])
                val2 = float(parts[1])
                val3 = float(parts[2])
                print(f"  First three as floats: {val1}, {val2}, {val3}")
                
                # Check ranges
                if 0 < val1 < 100:  # Could be period
                    print(f"    {val1} could be a period")
                if 0 < val2 < 10:  # Could be frequency
                    print(f"    {val2} could be a frequency")
                if -180 <= val3 <= 180:  # Could be direction
                    print(f"    {val3} could be a direction")
                    
            except ValueError:
                print("  First value not a number")
        print()

if __name__ == "__main__":
    test_actual_parts()