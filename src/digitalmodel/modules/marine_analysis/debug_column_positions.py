"""Debug column positions for continuation lines."""

def debug_columns():
    """Debug the exact column positions."""
    
    # Continuation lines from the test data
    cont_lines = [
        "                   -135.00    0.7173  -88.71    0.7069  -88.97    0.9957   -0.03    0.2032  -73.56    0.1032   92.38    0.1195  120.73",
        "                    -90.00    0.0006   -0.25    1.0040  -89.99    1.0001   -0.01    0.2789  -89.79    0.0028 -179.62    0.1484   90.01",
        "                    -45.00    0.7173   88.73    0.7068  -91.01    0.9955    0.01    0.2041 -105.80    0.1032  -92.38    0.1210   59.67",
        "                      0.00    1.0100   88.22    0.0000   34.39    0.9911    0.03    0.0014  -47.25    0.1455  -92.26    0.0011   88.76"
    ]
    
    print("Column ruler:")
    print("0         1         2         3         4         5         6         7         8         9         10        11        12        13")
    print("0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890")
    
    for i, line in enumerate(cont_lines):
        print(f"\nLine {i+1}:")
        print(line)
        
        # Check different column ranges for direction
        ranges = [
            (16, 27),  # Original assumption
            (19, 27),  # Current code
            (20, 27),  # Alternative
            (0, 27),   # First 27 chars
        ]
        
        for start, end in ranges:
            value = line[start:end].strip()
            print(f"  Columns {start}-{end}: '{value}'")
            
        # Find where the direction actually starts
        stripped = line.strip()
        direction = stripped.split()[0]
        actual_start = line.find(direction)
        print(f"  Actual direction '{direction}' starts at column {actual_start}")

if __name__ == "__main__":
    debug_columns()