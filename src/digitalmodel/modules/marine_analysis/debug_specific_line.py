"""Debug specific line parsing."""

from src.digitalmodel.modules.marine_analysis.aqwa_reader_v2 import AQWAReaderV2

def test_specific_line():
    """Test parsing of a specific data line."""
    
    parser = AQWAReaderV2()
    
    # Test line from the debug output
    test_line = "    4.00   1.571   -180.00    0.0088   36.77    0.0000  -88.09    0.0019  -94.72    0.0000  -75.57    0.0030   99.03    0.0000   -4.76"
    
    parts = test_line.split()
    print(f"Parts count: {len(parts)}")
    print(f"Parts: {parts}")
    
    if len(parts) >= 15:
        period = float(parts[0])
        freq = float(parts[1])
        direction = float(parts[2])
        print(f"Period: {period}, Freq: {freq}, Direction: {direction}")
        
        dof_values = parser._extract_dof_from_parts(parts[3:])
        print(f"DOF values: {dof_values}")
        
        # Expected values for first line:
        # X: 0.0088, 36.77 (surge)
        # Y: 0.0000, -88.09 (sway)  
        # Z: 0.0019, -94.72 (heave)
        # RX: 0.0000, -75.57 (roll)
        # RY: 0.0030, 99.03 (pitch)
        # RZ: 0.0000, -4.76 (yaw)
        
        expected = {
            'surge': {'amplitude': 0.0088, 'phase': 36.77},
            'sway': {'amplitude': 0.0000, 'phase': -88.09},
            'heave': {'amplitude': 0.0019, 'phase': -94.72},
            'roll': {'amplitude': 0.0000, 'phase': -75.57},
            'pitch': {'amplitude': 0.0030, 'phase': 99.03},
            'yaw': {'amplitude': 0.0000, 'phase': -4.76}
        }
        
        print(f"Expected: {expected}")
        
        # Check if they match
        for dof in expected:
            parsed = dof_values[dof]
            exp = expected[dof]
            print(f"{dof}: parsed={parsed} expected={exp} match={parsed == exp}")

if __name__ == "__main__":
    test_specific_line()