"""
Integration tests for the mooring tension iteration system
"""

import unittest
import tempfile
from pathlib import Path
import csv
import yaml
import numpy as np
import sys
import os

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from csv_parser import CSVParser
from length_calculator import LengthCalculator
from test_iteration_simulation import IterationSimulator


class TestEndToEndWorkflow(unittest.TestCase):
    """Test complete workflow from CSV to includefile"""
    
    def setUp(self):
        """Set up test environment"""
        self.temp_dir = tempfile.mkdtemp()
        self.temp_path = Path(self.temp_dir)
        
        # Create test CSV
        self.create_test_csv()
    
    def tearDown(self):
        """Clean up"""
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)
    
    def create_test_csv(self):
        """Create a test CSV file"""
        csv_data = [
            {
                'ObjectName': 'Line01',
                'target_tension': '100',
                'line_length': '[10.0, None]',
                'line_EA': '[1000.0, 50000.0]',
                'section_to_be_modified': '1'
            },
            {
                'ObjectName': 'Line02',
                'target_tension': '150',
                'line_length': '[12.0, None]',
                'line_EA': '[1200.0, 50000.0]',
                'section_to_be_modified': '1'
            },
            {
                'ObjectName': 'Line03',
                'target_tension': '200',
                'line_length': '[15.0, None]',
                'line_EA': '[1500.0, 50000.0]',
                'section_to_be_modified': '1'
            }
        ]
        
        self.csv_file = self.temp_path / 'test_targets.csv'
        with open(self.csv_file, 'w', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=csv_data[0].keys())
            writer.writeheader()
            writer.writerows(csv_data)
    
    def test_complete_workflow(self):
        """Test CSV → Parse → Calculate → Includefile"""
        # Step 1: Parse CSV
        parser = CSVParser(self.csv_file)
        targets = parser.parse_mooring_targets()
        
        self.assertEqual(len(targets), 3)
        
        # Step 2: Validate targets
        warnings = parser.validate_targets(targets)
        # Should warn about both tension and length specified
        self.assertEqual(len(warnings), 3)
        
        # Step 3: Create calculator
        calculator = LengthCalculator(targets)
        
        # Step 4: Simulate current tensions (with errors)
        current_tensions = {
            'Line01': 110.0,  # 10% over
            'Line02': 135.0,  # 10% under
            'Line03': 220.0   # 10% over
        }
        
        # Step 5: Calculate adjustments
        adjustments = calculator.calculate_adjustments(current_tensions)
        
        self.assertEqual(len(adjustments), 3)
        
        # Step 6: Check convergence
        converged, unconverged = calculator.check_convergence(tolerance=0.01)
        self.assertFalse(converged)  # All have 10% error
        
        # Step 7: Generate includefile
        includefile_path = self.temp_path / 'iteration_1.yml'
        calculator.generate_includefile(includefile_path)
        
        self.assertTrue(includefile_path.exists())
        
        # Step 8: Verify includefile content
        with open(includefile_path, 'r') as f:
            include_data = yaml.safe_load(f)
        
        self.assertEqual(include_data['iteration'], 1)
        self.assertIn('Line01', include_data['lines'])
        self.assertIn('Line02', include_data['lines'])
        self.assertIn('Line03', include_data['lines'])
        
        # Check that adjustments are in correct direction
        line01_length = include_data['lines']['Line01']['UnstretchedLength'][0]
        self.assertGreater(line01_length, 10.0)  # Should be lengthened
        
        line02_length = include_data['lines']['Line02']['UnstretchedLength'][0]
        self.assertLess(line02_length, 12.0)  # Should be shortened


class TestIterationConvergence(unittest.TestCase):
    """Test iteration convergence behavior"""
    
    def test_simple_convergence(self):
        """Test convergence with simple linear response"""
        # Create simple targets using mock object
        class MockTarget:
            def __init__(self):
                self.name = 'Line01'
                self.target_tension = 100.0
                self.line_lengths = [10.0]
                self.line_ea = [1000.0]
                self.section_to_modify = 1
            
            def has_tension_target(self):
                return True
            
            def get_governing_ea(self):
                return 1000.0
        
        targets = {
            'Line01': MockTarget()
        }
        
        calculator = LengthCalculator(targets)
        
        # Simulate iterations with decreasing error
        errors = [50.0, 25.0, 12.5, 6.25, 3.125, 1.5, 0.75]
        
        for i, error_percent in enumerate(errors):
            current_tension = 100.0 * (1 + error_percent/100)
            current_tensions = {'Line01': current_tension}
            
            adjustments = calculator.calculate_adjustments(current_tensions)
            converged, _ = calculator.check_convergence(tolerance=0.01)
            
            if i < 6:  # First 6 iterations should not converge
                self.assertFalse(converged)
            else:  # Last iteration should converge
                self.assertTrue(converged)
    
    def test_damping_effect_on_convergence(self):
        """Test how damping affects convergence"""
        # Create test CSV
        with tempfile.TemporaryDirectory() as temp_dir:
            temp_path = Path(temp_dir)
            csv_file = temp_path / 'test.csv'
            
            # Create minimal CSV
            with open(csv_file, 'w') as f:
                f.write("ObjectName,target_tension,line_length,line_EA,section_to_be_modified\n")
                f.write('Line01,100,"[10.0]","[1000.0]",1\n')
                f.write('Line02,150,"[12.0]","[1200.0]",1\n')
            
            # Test with different damping factors
            damping_factors = [1.0, 0.8, 0.5]
            convergence_iterations = []
            
            for damping in damping_factors:
                simulator = IterationSimulator(csv_file)
                
                # Mock the OrcaFlex response for consistent testing
                def mock_response(lengths):
                    # Simple linear response
                    tensions = {}
                    for name, length in lengths.items():
                        target = simulator.targets[name]
                        ea = target.get_governing_ea()
                        nominal_length = 10.0 if name == 'Line01' else 12.0
                        target_tension = target.target_tension
                        
                        # T = T_target - EA * strain
                        strain = (length - nominal_length) / nominal_length
                        tensions[name] = target_tension - ea * strain * 0.1
                    return tensions
                
                simulator.simulate_orcaflex_response = mock_response
                
                # Run simulation
                converged = simulator.run_iteration(
                    max_iterations=20,
                    tolerance=0.01,
                    damping=damping
                )
                
                if converged:
                    convergence_iterations.append(len(simulator.error_history))
                else:
                    convergence_iterations.append(None)
            
            # Higher damping should converge (though possibly slower)
            # No damping (1.0) might oscillate
            self.assertIsNotNone(convergence_iterations[1])  # 0.8 should converge
            self.assertIsNotNone(convergence_iterations[2])  # 0.5 should converge


class TestRealDataIntegration(unittest.TestCase):
    """Integration tests with real project data"""
    
    def test_real_data_workflow(self):
        """Test with actual project CSV if available"""
        real_csv = Path(r"D:\1522\ctr7\orcaflex\rev_a08\base_files\fsts_lngc_pretension\180km3_l000_pb_target_mooring_pretension.csv")
        
        if not real_csv.exists():
            self.skipTest("Real data file not available")
        
        # Parse real data
        parser = CSVParser(real_csv)
        targets = parser.parse_mooring_targets()
        
        # Create calculator
        calculator = LengthCalculator(targets)
        
        # Simulate realistic tensions (5% error)
        current_tensions = {}
        for name, target in targets.items():
            if target.has_tension_target():
                # Add random 5% error
                error = np.random.uniform(-0.05, 0.05)
                current_tensions[name] = target.target_tension * (1 + error)
        
        # Calculate adjustments
        adjustments = calculator.calculate_adjustments(current_tensions)
        
        # Should have adjustments for all 18 lines
        self.assertEqual(len(adjustments), 18)
        
        # Generate report
        report = calculator.generate_report()
        self.assertIn("Iteration 1", report)
        self.assertIn("Line01", report)
        
        # Generate includefile
        with tempfile.TemporaryDirectory() as temp_dir:
            includefile = Path(temp_dir) / 'real_data_include.yml'
            calculator.generate_includefile(includefile)
            
            self.assertTrue(includefile.exists())
            
            # Verify structure
            with open(includefile, 'r') as f:
                data = yaml.safe_load(f)
            
            self.assertEqual(len(data['lines']), 18)
            
            # Check a few lines
            self.assertIn('Line01', data['lines'])
            self.assertIn('Line18', data['lines'])
    
    def test_multiple_iteration_cycles(self):
        """Test multiple iteration cycles with real data structure"""
        real_csv = Path(r"D:\1522\ctr7\orcaflex\rev_a08\base_files\fsts_lngc_pretension\180km3_l000_pb_target_mooring_pretension.csv")
        
        if not real_csv.exists():
            self.skipTest("Real data file not available")
        
        parser = CSVParser(real_csv)
        targets = parser.parse_mooring_targets()
        calculator = LengthCalculator(targets)
        
        # Track convergence over iterations
        max_errors = []
        
        # Start with 10% error
        current_tensions = {}
        for name, target in targets.items():
            if target.has_tension_target():
                current_tensions[name] = target.target_tension * 1.1
        
        # Run 5 iterations with decreasing error
        for i in range(5):
            # Calculate adjustments
            adjustments = calculator.calculate_adjustments(current_tensions)
            
            # Check convergence
            converged, unconverged = calculator.check_convergence(tolerance=0.01)
            
            # Track max error
            max_error = max(abs(adj.tension_error_percent) for adj in adjustments.values())
            max_errors.append(max_error)
            
            # Simulate tension response (reduce error by 50% each iteration)
            for name in current_tensions:
                target_tension = targets[name].target_tension
                current = current_tensions[name]
                # Move 50% closer to target
                current_tensions[name] = current + 0.5 * (target_tension - current)
        
        # Errors should decrease
        for i in range(1, len(max_errors)):
            self.assertLess(max_errors[i], max_errors[i-1])
        
        # Should converge by iteration 5
        self.assertLess(max_errors[-1], 1.0)


class TestErrorHandling(unittest.TestCase):
    """Test error handling and edge cases"""
    
    def test_empty_csv(self):
        """Test handling of empty CSV"""
        with tempfile.TemporaryDirectory() as temp_dir:
            empty_csv = Path(temp_dir) / 'empty.csv'
            with open(empty_csv, 'w') as f:
                f.write("ObjectName,target_tension,line_length,line_EA,section_to_be_modified\n")
            
            parser = CSVParser(empty_csv)
            targets = parser.parse_mooring_targets()
            
            self.assertEqual(len(targets), 0)
            
            # Calculator should handle empty targets
            calculator = LengthCalculator(targets)
            adjustments = calculator.calculate_adjustments({})
            self.assertEqual(len(adjustments), 0)
    
    def test_missing_tensions(self):
        """Test handling when current tensions are missing"""
        # Create mock target object
        class MockTarget:
            def __init__(self):
                self.name = 'Line01'
                self.target_tension = 100.0
                self.line_lengths = [10.0]
                self.line_ea = [1000.0]
                self.section_to_modify = 1
            
            def has_tension_target(self):
                return True
            
            def get_governing_ea(self):
                return 1000.0
        
        targets = {
            'Line01': MockTarget()
        }
        
        calculator = LengthCalculator(targets)
        
        # Empty current tensions
        adjustments = calculator.calculate_adjustments({})
        
        # Should handle missing tension as 0
        self.assertEqual(len(adjustments), 1)
        adj = adjustments['Line01']
        self.assertEqual(adj.current_tension, 0.0)
    
    def test_extreme_values(self):
        """Test handling of extreme values"""
        # Create mock target object
        class MockTarget:
            def __init__(self):
                self.name = 'Line01'
                self.target_tension = 100.0
                self.line_lengths = [10.0]
                self.line_ea = [1000.0]
                self.section_to_modify = 1
            
            def has_tension_target(self):
                return True
            
            def get_governing_ea(self):
                return 1000.0
        
        targets = {
            'Line01': MockTarget()
        }
        
        calculator = LengthCalculator(targets)
        
        # Extremely high tension
        adjustments = calculator.calculate_adjustments({'Line01': 10000.0})
        # Should not produce negative length
        self.assertGreater(adjustments['Line01'].new_length, 0)
        
        # Zero tension
        calculator2 = LengthCalculator(targets)
        adjustments2 = calculator2.calculate_adjustments({'Line01': 0.0})
        self.assertIsNotNone(adjustments2['Line01'])
        
        # Negative tension (shouldn't happen but test anyway)
        calculator3 = LengthCalculator(targets)
        adjustments3 = calculator3.calculate_adjustments({'Line01': -50.0})
        self.assertIsNotNone(adjustments3['Line01'])


if __name__ == '__main__':
    # Run tests with verbose output
    unittest.main(verbosity=2)