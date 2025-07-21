"""Debug specific line parsing."""

def debug_line_parsing():
    # Test lines from the AQWA file
    test_lines = [
        "    4.00   1.571   -180.00    0.0088   36.77    0.0000  -88.09    0.0019  -94.72    0.0000  -75.57    0.0030   99.03    0.0000   -4.76",
        "                   -135.00    0.0062   82.23    0.0045   72.45    0.0021  -42.31    0.0089   45.98    0.0025  135.60    0.0054   39.99",
        "                    -90.00    0.0008  -13.33    0.0776   46.90    0.0333 -118.40    0.0146  -19.86    0.0095 -178.98    0.0272 -148.97"
    ]
    
    for i, line in enumerate(test_lines):
        print(f"\nLine {i}: '{line}'")
        parts = line.split()
        print(f"Number of parts: {len(parts)}")
        print(f"Parts: {parts}")
        
        # Check if it's a continuation line
        if len(parts) >= 13:
            try:
                # Try to parse first value as float
                first_val = float(parts[0])
                print(f"First value as float: {first_val}")
                
                # Check if it could be a direction
                if -180 <= first_val <= 180:
                    print("Could be a direction value!")
            except ValueError:
                print("First value is not a number")

if __name__ == "__main__":
    debug_line_parsing()