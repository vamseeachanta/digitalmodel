"""
Unit tests for CSV Parser module
"""

import unittest
import tempfile
import csv
from pathlib import Path
import sys
import os

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from csv_parser import CSVParser, MooringLineTarget, FenderProperties


class TestMooringLineTarget(unittest.TestCase):
    """Test the MooringLineTarget data class"""
    
    def test_has_tension_target(self):
        """Test tension target detection"""
        target = MooringLineTarget(name="Line01")
        self.assertFalse(target.has_tension_target())
        
        target.target_tension = 220.0
        self.assertTrue(target.has_tension_target())
        
        target.target_tension = 0
        self.assertFalse(target.has_tension_target())
    
    def test_has_length_target(self):
        """Test length target detection"""
        target = MooringLineTarget(name="Line01")
        self.assertFalse(target.has_length_target())
        
        target.target_length = 11.0
        self.assertTrue(target.has_length_target())
        
        target.target_length = 0
        self.assertFalse(target.has_length_target())
    
    def test_get_governing_ea(self):
        """Test governing EA selection"""
        target = MooringLineTarget(name="Line01")
        
        # No EA values should raise error
        with self.assertRaises(ValueError):
            target.get_governing_ea()
        
        # Single EA value
        target.line_ea = [1290.0]
        self.assertEqual(target.get_governing_ea(), 1290.0)
        
        # Multiple EA values, section 1
        target.line_ea = [1290.0, 50000.0]
        target.section_to_modify = 1
        self.assertEqual(target.get_governing_ea(), 1290.0)
        
        # Multiple EA values, section 2
        target.section_to_modify = 2
        self.assertEqual(target.get_governing_ea(), 50000.0)
        
        # Section exceeds available - use minimum
        target.section_to_modify = 3
        self.assertEqual(target.get_governing_ea(), 1290.0)


class TestFenderProperties(unittest.TestCase):
    """Test the FenderProperties data class"""
    
    def test_get_force_at_deflection(self):
        """Test force interpolation"""
        fender = FenderProperties(name="Fender1")
        
        # Empty curve returns 0
        self.assertEqual(fender.get_force_at_deflection(1.0), 0.0)
        
        # Simple linear curve
        fender.force_deflection_curve = [
            (0.0, 0.0),
            (1.0, 100.0),
            (2.0, 300.0)
        ]
        
        # Exact points
        self.assertEqual(fender.get_force_at_deflection(0.0), 0.0)
        self.assertEqual(fender.get_force_at_deflection(1.0), 100.0)
        self.assertEqual(fender.get_force_at_deflection(2.0), 300.0)
        
        # Interpolation
        self.assertEqual(fender.get_force_at_deflection(0.5), 50.0)
        self.assertEqual(fender.get_force_at_deflection(1.5), 200.0)
        
        # Outside range
        self.assertEqual(fender.get_force_at_deflection(-1.0), 0.0)
        self.assertEqual(fender.get_force_at_deflection(3.0), 300.0)


class TestCSVParser(unittest.TestCase):
    """Test the CSV Parser"""
    
    def setUp(self):
        """Create temporary test files"""
        self.temp_dir = tempfile.mkdtemp()
        self.temp_path = Path(self.temp_dir)
    
    def tearDown(self):
        """Clean up temporary files"""
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)
    
    def create_mooring_csv(self, filename, data):
        """Helper to create CSV files"""
        file_path = self.temp_path / filename
        with open(file_path, 'w', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=data[0].keys())
            writer.writeheader()
            writer.writerows(data)
        return file_path
    
    def test_parse_simple_mooring_targets(self):
        """Test parsing simple mooring targets"""
        data = [
            {
                'ObjectName': 'Line01',
                'target_tension': '220',
                'line_length': '[11.0, None]',
                'line_EA': '[1290.0, 50000.0]',
                'section_to_be_modified': '1'
            },
            {
                'ObjectName': 'Line02',
                'target_tension': '200',
                'line_length': '[12.0, None]',
                'line_EA': '[1290.0, 50000.0]',
                'section_to_be_modified': '1'
            }
        ]
        
        file_path = self.create_mooring_csv('test_mooring.csv', data)
        parser = CSVParser(file_path)
        targets = parser.parse_mooring_targets()
        
        self.assertEqual(len(targets), 2)
        self.assertIn('Line01', targets)
        self.assertIn('Line02', targets)
        
        line01 = targets['Line01']
        self.assertEqual(line01.target_tension, 220.0)
        self.assertEqual(line01.target_length, 11.0)
        self.assertEqual(line01.line_ea, [1290.0, 50000.0])
        self.assertEqual(line01.section_to_modify, 1)
    
    def test_parse_null_values(self):
        """Test handling of NULL values"""
        data = [
            {
                'ObjectName': 'Line01',
                'target_tension': 'NULL',
                'line_length': '[11.0, None]',
                'line_EA': '[1290.0, 50000.0]',
                'section_to_be_modified': '1'
            }
        ]
        
        file_path = self.create_mooring_csv('test_null.csv', data)
        parser = CSVParser(file_path)
        targets = parser.parse_mooring_targets()
        
        line01 = targets['Line01']
        self.assertIsNone(line01.target_tension)
        self.assertEqual(line01.target_length, 11.0)
    
    def test_parse_scientific_notation(self):
        """Test parsing scientific notation in EA values"""
        data = [
            {
                'ObjectName': 'Line01',
                'target_tension': '220',
                'line_length': '[11.0, None]',
                'line_EA': '[1.29e3, 5.0e4]',
                'section_to_be_modified': '1'
            }
        ]
        
        file_path = self.create_mooring_csv('test_scientific.csv', data)
        parser = CSVParser(file_path)
        targets = parser.parse_mooring_targets()
        
        line01 = targets['Line01']
        self.assertEqual(line01.line_ea, [1290.0, 50000.0])
    
    def test_parse_fender_properties(self):
        """Test parsing fender properties"""
        data = [
            {
                'ObjectName': 'Fender1',
                'target_force': 'NULL',
                'fender_properties': '[[0.0, 0.0], [1.0, 100.0], [2.0, 300.0]]'
            }
        ]
        
        file_path = self.create_mooring_csv('test_fender.csv', data)
        parser = CSVParser(file_path)
        fenders = parser.parse_fender_properties()
        
        self.assertEqual(len(fenders), 1)
        self.assertIn('Fender1', fenders)
        
        fender1 = fenders['Fender1']
        self.assertIsNone(fender1.target_force)
        self.assertEqual(len(fender1.force_deflection_curve), 3)
        self.assertEqual(fender1.force_deflection_curve[0], (0.0, 0.0))
    
    def test_validate_targets(self):
        """Test target validation"""
        data = [
            {
                'ObjectName': 'Line01',
                'target_tension': '220',
                'line_length': '[11.0, None]',
                'line_EA': '',  # Missing EA
                'section_to_be_modified': '1'
            },
            {
                'ObjectName': 'Line02',
                'target_tension': 'NULL',  # No target
                'line_length': '',
                'line_EA': '[1290.0]',
                'section_to_be_modified': '1'
            },
            {
                'ObjectName': 'Line03',
                'target_tension': '200',
                'line_length': '[12.0]',  # Both targets
                'line_EA': '[1290.0]',
                'section_to_be_modified': '2'  # Invalid section
            }
        ]
        
        file_path = self.create_mooring_csv('test_validate.csv', data)
        parser = CSVParser(file_path)
        targets = parser.parse_mooring_targets()
        warnings = parser.validate_targets(targets)
        
        # Should have warnings for:
        # - Line01: Missing EA
        # - Line02: No target
        # - Line03: Both targets specified
        # - Line03: Invalid section
        # Note: Line03 may generate 2 warnings (both targets + invalid section)
        self.assertGreaterEqual(len(warnings), 4)
        
        # Check specific warnings
        warning_texts = ' '.join(warnings)
        self.assertIn('Line01', warning_texts)
        self.assertIn('EA', warning_texts)
        self.assertIn('Line02', warning_texts)
        self.assertIn('No target', warning_texts)
        self.assertIn('Line03', warning_texts)
        self.assertIn('Both tension and length', warning_texts)
    
    def test_file_not_found(self):
        """Test handling of missing files"""
        with self.assertRaises(FileNotFoundError):
            CSVParser(self.temp_path / 'nonexistent.csv')
    
    def test_malformed_csv(self):
        """Test handling of malformed CSV data"""
        data = [
            {
                'ObjectName': 'Line01',
                'target_tension': 'not_a_number',
                'line_length': 'invalid_json',
                'line_EA': '[broken',
                'section_to_be_modified': 'abc'
            }
        ]
        
        file_path = self.create_mooring_csv('test_malformed.csv', data)
        parser = CSVParser(file_path)
        
        # Should handle gracefully without crashing
        targets = parser.parse_mooring_targets()
        self.assertIn('Line01', targets)
        
        line01 = targets['Line01']
        # Invalid values should be None or default
        self.assertIsNone(line01.target_tension)
        self.assertEqual(line01.section_to_modify, 1)  # Default value


class TestRealDataIntegration(unittest.TestCase):
    """Test with real project data if available"""
    
    def test_real_csv_file(self):
        """Test parsing actual project CSV file"""
        real_file = Path(r"D:\1522\ctr7\orcaflex\rev_a08\base_files\fsts_lngc_pretension\180km3_l000_pb_target_mooring_pretension.csv")
        
        if not real_file.exists():
            self.skipTest("Real data file not available")
        
        parser = CSVParser(real_file)
        targets = parser.parse_mooring_targets()
        
        # Check expected structure
        self.assertEqual(len(targets), 18)  # 18 mooring lines
        
        # Check Line01 properties
        line01 = targets.get('Line01')
        self.assertIsNotNone(line01)
        self.assertEqual(line01.target_tension, 220.0)
        self.assertEqual(line01.line_ea, [1290.0, 50000.0])
        
        # Validate all targets
        warnings = parser.validate_targets(targets)
        # All lines have both tension and length in the real file
        for warning in warnings:
            self.assertIn("Both tension and length", warning)


if __name__ == '__main__':
    # Run tests with verbose output
    unittest.main(verbosity=2)