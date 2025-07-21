"""Analyze AQWA format to determine column positions."""

test_lines = [
    "    4.00   1.571   -180.00    0.0216 -143.23    0.0000   91.91    0.0047   85.28    0.0000  104.43    0.0075  -80.97    0.0001  175.24",
    "                   -135.00    0.0154  -97.77    0.0110 -107.55    0.0052  137.69    0.0219 -134.02    0.0061  -44.40    0.0132 -140.01",
    "                    -90.00    0.0019  166.67    0.1915 -133.10    0.0822   61.60    0.0359  160.14    0.0235    1.02    0.0670   31.03",
]

# Print with column positions
print("Column positions (1-based):")
print("1234567890" * 14)
for line in test_lines:
    print(line)
    
print("\n\nAnalyzing field positions:")
for i, line in enumerate(test_lines):
    print(f"\nLine {i+1}: length = {len(line)}")
    
    if i == 0:  # Full line
        # Extract fields by position
        period = line[0:8].strip()
        freq = line[8:16].strip()
        direction = line[16:27].strip()
        
        print(f"  Period (1-8): '{period}'")
        print(f"  Freq (9-16): '{freq}'")
        print(f"  Direction (17-27): '{direction}'")
        
        # The rest are amp/phase pairs
        rest = line[27:]
        print(f"  Rest of line starting at 28: '{rest}'")
        
    else:  # Continuation line
        # Direction starts around column 20
        direction = line[19:27].strip()
        print(f"  Direction (20-27): '{direction}'")
        
        # The rest are amp/phase pairs
        rest = line[27:]
        print(f"  Rest of line starting at 28: '{rest}'")
        
# Now analyze the amp/phase pairs
print("\n\nAnalyzing amp/phase pairs:")
# Starting from position 27, we have 6 DOF * 2 values = 12 values
line = test_lines[0]
data_part = line[27:]
print(f"Data part: '{data_part}'")
print(f"Data length: {len(data_part)}")

# Each value appears to be about 9 characters wide
pos = 0
values = []
while pos < len(data_part):
    val = data_part[pos:pos+9].strip()
    if val:
        values.append(val)
    pos += 9
    
print(f"Found {len(values)} values: {values}")