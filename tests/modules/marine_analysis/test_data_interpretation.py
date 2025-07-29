"""Test the data interpretation functionality for AQWA parser.

This test verifies that the enhanced parser correctly interprets abbreviated
data format where wave direction rows repeat the period and frequency from
the first row.
"""

import pytest
import numpy as np
from pathlib import Path
import tempfile
import os

from src.digitalmodel.modules.marine_analysis.aqwa_enhanced_parser import AQWAEnhancedParser


class TestDataInterpretation:
    """Test class for data interpretation functionality."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.parser = AQWAEnhancedParser()
    
    def test_expand_abbreviated_data(self):
        """Test the expansion of abbreviated data format."""
        # Create sample abbreviated data as specified in requirements
        abbreviated_data = """   40.00   0.157   -180.00    1.0099  -88.22    0.0000  -33.08    0.9912   -0.03    0.0014   47.25    0.1455   92.26    0.0011  -88.75
                   -135.00    0.7173  -88.71    0.7069  -88.97    0.9957   -0.03    0.2032  -73.56    0.1032   92.38    0.1195  120.73
                    -90.00    0.0006   -0.25    1.0040  -89.99    1.0001   -0.01    0.2789  -89.79    0.0028 -179.62    0.1484   90.01
                    -45.00    0.7173   88.73    0.7068  -91.01    0.9955    0.01    0.2041 -105.80    0.1032  -92.38    0.1210   59.67
                      0.00    1.0100   88.22    0.0000   34.39    0.9911    0.03    0.0014  -47.25    0.1455  -92.26    0.0011   88.76
                     45.00    0.7174   88.71    0.7069   88.97    0.9957    0.02    0.2032   73.56    0.1032  -92.38    0.1196 -120.73
                     90.00    0.0006    1.13    1.0040   89.99    1.0001    0.01    0.2789   89.79    0.0028  179.64    0.1484  -90.02
                    135.00    0.7173  -88.73    0.7068   91.01    0.9955   -0.01    0.2041  105.79    0.1032   92.38    0.1210  -59.67
                    180.00    1.0099  -88.22    0.0000  -32.43    0.9912   -0.03    0.0014   47.25    0.1455   92.26    0.0011  -88.75"""
        
        # Expected expanded data
        expected_data = """   40.00   0.157   -180.00    1.0099  -88.22    0.0000  -33.08    0.9912   -0.03    0.0014   47.25    0.1455   92.26    0.0011  -88.75
   40.00   0.157   -135.00    0.7173  -88.71    0.7069  -88.97    0.9957   -0.03    0.2032  -73.56    0.1032   92.38    0.1195  120.73
   40.00   0.157    -90.00    0.0006   -0.25    1.0040  -89.99    1.0001   -0.01    0.2789  -89.79    0.0028 -179.62    0.1484   90.01
   40.00   0.157    -45.00    0.7173   88.73    0.7068  -91.01    0.9955    0.01    0.2041 -105.80    0.1032  -92.38    0.1210   59.67
   40.00   0.157      0.00    1.0100   88.22    0.0000   34.39    0.9911    0.03    0.0014  -47.25    0.1455  -92.26    0.0011   88.76
   40.00   0.157     45.00    0.7174   88.71    0.7069   88.97    0.9957    0.02    0.2032   73.56    0.1032  -92.38    0.1196 -120.73
   40.00   0.157     90.00    0.0006    1.13    1.0040   89.99    1.0001    0.01    0.2789   89.79    0.0028  179.64    0.1484  -90.02
   40.00   0.157    135.00    0.7173  -88.73    0.7068   91.01    0.9955   -0.01    0.2041  105.79    0.1032   92.38    0.1210  -59.67
   40.00   0.157    180.00    1.0099  -88.22    0.0000  -32.43    0.9912   -0.03    0.0014   47.25    0.1455   92.26    0.0011  -88.75"""
        
        # Expand the abbreviated data
        expanded = self.parser.expand_abbreviated_data(abbreviated_data)
        
        # Verify the expansion matches expected format
        expanded_lines = expanded.strip().split('\n')
        expected_lines = expected_data.strip().split('\n')
        
        assert len(expanded_lines) == len(expected_lines)
        
        # Check that each line has the correct period and frequency values
        for i, (expanded_line, expected_line) in enumerate(zip(expanded_lines, expected_lines)):
            expanded_parts = expanded_line.split()
            expected_parts = expected_line.split()
            
            # First line should be unchanged
            if i == 0:
                assert expanded_parts[0] == expected_parts[0]  # period
                assert expanded_parts[1] == expected_parts[1]  # frequency
                assert expanded_parts[2] == expected_parts[2]  # direction
            else:
                # Continuation lines should have period and frequency from first line
                assert float(expanded_parts[0]) == 40.00  # period
                assert float(expanded_parts[1]) == 0.157  # frequency
    
    def test_interpret_continuation_line(self):
        """Test interpretation of a single continuation line."""
        # Test line from the requirements
        continuation_line = "                   -135.00    0.7173  -88.71    0.7069  -88.97    0.9957   -0.03    0.2032  -73.56    0.1032   92.38    0.1195  120.73"
        period = 40.00
        freq = 0.157
        
        result = self.parser._interpret_continuation_line(continuation_line, period, freq)
        
        assert result is not None
        direction, dof_values = result
        
        # Check direction
        assert direction == -135.00
        
        # Check that DOF values are extracted
        assert 'surge' in dof_values
        assert 'amplitude' in dof_values['surge']
        assert 'phase' in dof_values['surge']
        
        # Check first DOF values (surge)
        assert abs(dof_values['surge']['amplitude'] - 0.7173) < 1e-6
        assert abs(dof_values['surge']['phase'] - (-88.71)) < 1e-6
    
    def test_parse_with_mock_data(self):
        """Test parsing with mock AQWA data including the interpretation feature."""
        # Create a mock AQWA file with abbreviated format
        mock_content = """
R.A.O.S-VARIATION WITH WAVE DIRECTION

         PERIOD    FREQ      DIRECTION         X                Y                Z               RX               RY               RZ
         (SECS)  (RAD/SEC)  (DEGREES)     AMP    PHASE       AMP    PHASE       AMP    PHASE       AMP    PHASE       AMP    PHASE       AMP    PHASE
       -------------------------------------------------------------------------------------------------------------------------------------------
   40.00   0.157   -180.00    1.0099  -88.22    0.0000  -33.08    0.9912   -0.03    0.0014   47.25    0.1455   92.26    0.0011  -88.75
                   -135.00    0.7173  -88.71    0.7069  -88.97    0.9957   -0.03    0.2032  -73.56    0.1032   92.38    0.1195  120.73
                    -90.00    0.0006   -0.25    1.0040  -89.99    1.0001   -0.01    0.2789  -89.79    0.0028 -179.62    0.1484   90.01
                      0.00    1.0100   88.22    0.0000   34.39    0.9911    0.03    0.0014  -47.25    0.1455  -92.26    0.0011   88.76
                    180.00    1.0099  -88.22    0.0000  -32.43    0.9912   -0.03    0.0014   47.25    0.1455   92.26    0.0011  -88.75
"""
        
        # Create a temporary file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.lis', delete=False) as f:
            f.write(mock_content)
            temp_file_path = f.name
        
        try:
            # Parse the file
            result = self.parser.parse_lis_file(temp_file_path)
            
            # Check that the results contain expected data
            assert 'frequencies' in result
            assert 'headings' in result
            assert 'raos' in result
            
            # Check that we have the expected frequency
            assert len(result['frequencies']) == 1
            assert abs(result['frequencies'][0] - 0.157) < 1e-6
            
            # Check that we have all expected headings (including from continuation lines)
            expected_headings = [-180.0, -135.0, -90.0, 0.0, 180.0]
            assert len(result['headings']) == len(expected_headings)
            for expected_heading in expected_headings:
                assert expected_heading in result['headings']
            
            # Check RAO data structure
            assert 'surge' in result['raos']
            assert 'amplitude' in result['raos']['surge']
            assert 'phase' in result['raos']['surge']
            
        finally:
            # Clean up temp file
            os.unlink(temp_file_path)
    
    def test_data_interpretation_with_multiple_frequencies(self):
        """Test data interpretation with multiple frequency blocks."""
        # Create mock data with multiple frequency blocks
        mock_content = """
R.A.O.S-VARIATION WITH WAVE DIRECTION

         PERIOD    FREQ      DIRECTION         X                Y                Z               RX               RY               RZ
         (SECS)  (RAD/SEC)  (DEGREES)     AMP    PHASE       AMP    PHASE       AMP    PHASE       AMP    PHASE       AMP    PHASE       AMP    PHASE
       -------------------------------------------------------------------------------------------------------------------------------------------
   40.00   0.157   -180.00    1.0099  -88.22    0.0000  -33.08    0.9912   -0.03    0.0014   47.25    0.1455   92.26    0.0011  -88.75
                   -135.00    0.7173  -88.71    0.7069  -88.97    0.9957   -0.03    0.2032  -73.56    0.1032   92.38    0.1195  120.73
                      0.00    1.0100   88.22    0.0000   34.39    0.9911    0.03    0.0014  -47.25    0.1455  -92.26    0.0011   88.76
   20.00   0.314   -180.00    2.0099  -78.22    1.0000  -23.08    1.9912   -0.13    0.1014   57.25    1.1455  102.26    0.1011  -78.75
                   -135.00    1.7173  -78.71    1.7069  -78.97    1.9957   -0.13    1.2032  -63.56    1.1032  102.38    1.1195  130.73
                      0.00    2.0100   78.22    1.0000   44.39    1.9911    0.13    0.1014  -37.25    1.1455  -82.26    0.1011   98.76
"""
        
        # Create a temporary file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.lis', delete=False) as f:
            f.write(mock_content)
            temp_file_path = f.name
        
        try:
            # Parse the file
            result = self.parser.parse_lis_file(temp_file_path)
            
            # Check that we have multiple frequencies
            assert len(result['frequencies']) == 2
            assert abs(result['frequencies'][0] - 0.157) < 1e-6
            assert abs(result['frequencies'][1] - 0.314) < 1e-6
            
            # Check that each frequency has the expected headings
            expected_headings = [-180.0, -135.0, 0.0]
            assert len(result['headings']) == len(expected_headings)
            
        finally:
            # Clean up temp file
            os.unlink(temp_file_path)


if __name__ == "__main__":
    pytest.main([__file__])