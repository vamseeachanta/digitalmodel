"""Fix line processing to include all lines."""

test_data = """   40.00   0.157   -180.00    1.0099  -88.22    0.0000  -33.08    0.9912   -0.03    0.0014   47.25    0.1455   92.26    0.0011  -88.75
                   -135.00    0.7173  -88.71    0.7069  -88.97    0.9957   -0.03    0.2032  -73.56    0.1032   92.38    0.1195  120.73
                    -90.00    0.0006   -0.25    1.0040  -89.99    1.0001   -0.01    0.2789  -89.79    0.0028 -179.62    0.1484   90.01
                    -45.00    0.7173   88.73    0.7068  -91.01    0.9955    0.01    0.2041 -105.80    0.1032  -92.38    0.1210   59.67
                      0.00    1.0100   88.22    0.0000   34.39    0.9911    0.03    0.0014  -47.25    0.1455  -92.26    0.0011   88.76
                     45.00    0.7174   88.71    0.7069   88.97    0.9957    0.02    0.2032   73.56    0.1032  -92.38    0.1196 -120.73
                     90.00    0.0006    1.13    1.0040   89.99    1.0001    0.01    0.2789   89.79    0.0028  179.64    0.1484  -90.02
                    135.00    0.7173  -88.73    0.7068   91.01    0.9955   -0.01    0.2041  105.79    0.1032   92.38    0.1210  -59.67
                    180.00    1.0099  -88.22    0.0000  -32.43    0.9912   -0.03    0.0014   47.25    0.1455   92.26    0.0011  -88.75"""

lines = test_data.split('\n')
print(f"Total lines: {len(lines)}")

for i, line in enumerate(lines):
    print(f"\nLine {i}: length={len(line)}")
    print(f"  Raw: '{line[:50]}...'")
    print(f"  First 27 chars: '{line[:27]}'")
    
    # Check if it's a full line
    if len(line) >= 16:
        period_str = line[0:8].strip()
        freq_str = line[8:16].strip()
        print(f"  Period field (0-8): '{period_str}'")
        print(f"  Freq field (8-16): '{freq_str}'")
        
        if period_str and freq_str:
            print("  -> This is a FULL line")
        else:
            print("  -> This is a CONTINUATION line")
            # Get first value
            stripped = line.strip()
            if stripped:
                first_val = stripped.split()[0]
                print(f"     First value: '{first_val}'")