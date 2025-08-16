"""
Unit tests for Length Calculator module
"""

import unittest
import tempfile
from pathlib import Path
import yaml
import sys
import os

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from csv_parser import MooringLineTarget
from length_calculator import LengthCalculator, LineLengthAdjustment


class TestLineLengthAdjustment(unittest.TestCase):
    """Test the LineLengthAdjustment data class"""
    
    def test_is_converged(self):
        """Test convergence checking"""
        adj = LineLengthAdjustment(
            line_name="Line01",
            current_tension=220.0,
            target_tension=220.0,
            tension_error=0.0,
            tension_error_percent=0.0,
            current_length=11.0,
            length_adjustment=0.0,
            new_length=11.0,
            ea_used=1290.0
        )
        
        # Perfect convergence
        self.assertTrue(adj.is_converged(tolerance=0.01))
        
        # Within 1% tolerance
        adj.tension_error_percent = 0.5
        self.assertTrue(adj.is_converged(tolerance=0.01))
        
        # Outside 1% tolerance
        adj.tension_error_percent = 1.5
        self.assertFalse(adj.is_converged(tolerance=0.01))
        
        # Negative error
        adj.tension_error_percent = -0.8
        self.assertTrue(adj.is_converged(tolerance=0.01))
        
        adj.tension_error_percent = -1.2
        self.assertFalse(adj.is_converged(tolerance=0.01))


class TestLengthCalculator(unittest.TestCase):
    """Test the Length Calculator"""
    
    def setUp(self):
        """Create test targets"""
        self.targets = {
            'Line01': MooringLineTarget(
                name='Line01',
                target_tension=220.0,
                line_lengths=[11.0],
                line_ea=[1290.0, 50000.0],
                section_to_modify=1
            ),
            'Line02': MooringLineTarget(
                name='Line02',
                target_tension=200.0,
                line_lengths=[12.0],
                line_ea=[1500.0],
                section_to_modify=1
            ),
            'Line03': MooringLineTarget(
                name='Line03',
                target_length=15.0,  # Length target instead of tension
                line_lengths=[15.0],
                line_ea=[2000.0],
                section_to_modify=1
            )
        }
        
        self.calculator = LengthCalculator(self.targets)
    
    def test_calculate_adjustments_basic(self):
        """Test basic length adjustment calculation"""
        current_tensions = {
            'Line01': 242.0,  # 10% over target
            'Line02': 180.0,  # 10% under target
            'Line03': 150.0   # Has length target, not tension
        }
        
        adjustments = self.calculator.calculate_adjustments(current_tensions)
        
        # Check Line01 (over-tensioned, needs lengthening)
        adj1 = adjustments['Line01']
        self.assertEqual(adj1.current_tension, 242.0)
        self.assertEqual(adj1.target_tension, 220.0)
        self.assertEqual(adj1.tension_error, 22.0)
        self.assertAlmostEqual(adj1.tension_error_percent, 10.0, places=1)
        # Length adjustment should be positive (adding length)
        self.assertGreater(adj1.length_adjustment, 0)
        # New length should be greater than current
        self.assertGreater(adj1.new_length, adj1.current_length)
        
        # Check Line02 (under-tensioned, needs shortening)
        adj2 = adjustments['Line02']
        self.assertEqual(adj2.current_tension, 180.0)
        self.assertEqual(adj2.tension_error, -20.0)
        # Length adjustment should be negative (reducing length)
        self.assertLess(adj2.length_adjustment, 0)
        # New length should be less than current
        self.assertLess(adj2.new_length, adj2.current_length)
        
        # Line03 should not be in adjustments (no tension target)
        self.assertNotIn('Line03', adjustments)
    
    def test_calculate_adjustments_with_damping(self):
        """Test damping factor effect"""
        current_tensions = {'Line01': 242.0}
        
        # Full adjustment (no damping)
        adj_full = self.calculator.calculate_adjustments(
            current_tensions, damping_factor=1.0
        )
        full_adjustment = adj_full['Line01'].length_adjustment
        
        # Half adjustment (50% damping)
        calculator2 = LengthCalculator(self.targets)
        adj_half = calculator2.calculate_adjustments(
            current_tensions, damping_factor=0.5
        )
        half_adjustment = adj_half['Line01'].length_adjustment
        
        # Half adjustment should be 50% of full
        self.assertAlmostEqual(half_adjustment, full_adjustment * 0.5, places=5)
    
    def test_calculate_adjustments_with_custom_lengths(self):
        """Test providing custom current lengths"""
        current_tensions = {'Line01': 242.0}
        current_lengths = {'Line01': 20.0}  # Different from default
        
        adjustments = self.calculator.calculate_adjustments(
            current_tensions, current_lengths=current_lengths
        )
        
        adj = adjustments['Line01']
        self.assertEqual(adj.current_length, 20.0)
        # Adjustment magnitude should scale with length
        # ΔL = L/EA × ΔT
        expected_adjustment = (20.0 / 1290.0) * 22.0
        self.assertAlmostEqual(adj.length_adjustment, expected_adjustment, places=5)
    
    def test_safety_limits(self):
        """Test safety limits for extremely small lengths"""
        current_tensions = {'Line01': 10000.0}  # Extremely high tension
        current_lengths = {'Line01': 1.0}  # Short line
        
        adjustments = self.calculator.calculate_adjustments(
            current_tensions, current_lengths=current_lengths
        )
        
        adj = adjustments['Line01']
        # Should limit to minimum 0.1m
        self.assertGreaterEqual(adj.new_length, 0.1)
    
    def test_check_convergence(self):
        """Test convergence checking"""
        # No adjustments yet
        converged, unconverged = self.calculator.check_convergence()
        self.assertFalse(converged)
        
        # Calculate adjustments with mixed convergence
        current_tensions = {
            'Line01': 221.0,  # Within 1% tolerance
            'Line02': 180.0   # 10% error
        }
        
        self.calculator.calculate_adjustments(current_tensions)
        converged, unconverged = self.calculator.check_convergence(tolerance=0.01)
        
        self.assertFalse(converged)
        self.assertEqual(len(unconverged), 1)
        self.assertIn('Line02', unconverged[0])
        
        # All within tolerance
        current_tensions = {
            'Line01': 221.0,  # Within 1%
            'Line02': 199.0   # Within 1%
        }
        
        calculator2 = LengthCalculator(self.targets)
        calculator2.calculate_adjustments(current_tensions)
        converged, unconverged = calculator2.check_convergence(tolerance=0.01)
        
        self.assertTrue(converged)
        self.assertEqual(len(unconverged), 0)
    
    def test_generate_includefile(self):
        """Test includefile generation"""
        current_tensions = {
            'Line01': 242.0,
            'Line02': 180.0
        }
        
        self.calculator.calculate_adjustments(current_tensions)
        
        # Generate includefile
        with tempfile.TemporaryDirectory() as temp_dir:
            output_path = Path(temp_dir) / 'test_include.yml'
            result_path = self.calculator.generate_includefile(output_path)
            
            self.assertTrue(result_path.exists())
            
            # Load and check content
            with open(result_path, 'r') as f:
                data = yaml.safe_load(f)
            
            self.assertIn('iteration', data)
            self.assertEqual(data['iteration'], 1)
            self.assertIn('lines', data)
            self.assertIn('Line01', data['lines'])
            self.assertIn('Line02', data['lines'])
            
            # Check line data
            line01_data = data['lines']['Line01']
            self.assertIn('UnstretchedLength', line01_data)
            self.assertIn('_comment', line01_data)
            self.assertIsInstance(line01_data['UnstretchedLength'], list)
    
    def test_generate_report(self):
        """Test report generation"""
        # Empty report
        report = self.calculator.generate_report()
        self.assertIn("No adjustments", report)
        
        # Calculate adjustments
        current_tensions = {
            'Line01': 242.0,
            'Line02': 180.0
        }
        
        self.calculator.calculate_adjustments(current_tensions)
        report = self.calculator.generate_report()
        
        # Check report content
        self.assertIn("Iteration 1", report)
        self.assertIn("Convergence:", report)
        self.assertIn("Max error:", report)
        self.assertIn("Line01", report)
        self.assertIn("Line02", report)
        self.assertIn("242.0", report)  # Current tension
        self.assertIn("220.0", report)  # Target tension
    
    def test_iteration_counting(self):
        """Test iteration counter"""
        self.assertEqual(self.calculator.iteration_count, 0)
        
        current_tensions = {'Line01': 242.0}
        self.calculator.calculate_adjustments(current_tensions)
        self.assertEqual(self.calculator.iteration_count, 1)
        
        self.calculator.calculate_adjustments(current_tensions)
        self.assertEqual(self.calculator.iteration_count, 2)
    
    def test_missing_ea_values(self):
        """Test handling of missing EA values"""
        # Create target without EA
        targets = {
            'Line01': MooringLineTarget(
                name='Line01',
                target_tension=220.0,
                line_lengths=[11.0],
                line_ea=[],  # No EA values
                section_to_modify=1
            )
        }
        
        calculator = LengthCalculator(targets)
        current_tensions = {'Line01': 242.0}
        
        # Should handle gracefully
        adjustments = calculator.calculate_adjustments(current_tensions)
        self.assertEqual(len(adjustments), 0)  # No adjustment possible without EA


class TestLengthCalculatorFormula(unittest.TestCase):
    """Test the physics/math of the length calculation"""
    
    def test_formula_correctness(self):
        """Verify the ΔL = L/EA × (T_current - T_target) formula"""
        targets = {
            'TestLine': MooringLineTarget(
                name='TestLine',
                target_tension=100.0,  # kN
                line_lengths=[10.0],   # m
                line_ea=[1000.0],      # kN
                section_to_modify=1
            )
        }
        
        calculator = LengthCalculator(targets)
        
        # Test case: 10% over-tension
        current_tensions = {'TestLine': 110.0}  # 10 kN over
        current_lengths = {'TestLine': 10.0}
        
        adjustments = calculator.calculate_adjustments(
            current_tensions, 
            current_lengths=current_lengths,
            damping_factor=1.0  # No damping
        )
        
        adj = adjustments['TestLine']
        
        # Manual calculation: ΔL = L/EA × (T_current - T_target)
        # ΔL = 10/1000 × (110 - 100) = 10/1000 × 10 = 0.1
        expected_adjustment = 0.1
        
        self.assertAlmostEqual(adj.length_adjustment, expected_adjustment, places=5)
        self.assertAlmostEqual(adj.new_length, 10.0 + expected_adjustment, places=5)
    
    def test_sign_convention(self):
        """Test sign convention for adjustments"""
        targets = {
            'TestLine': MooringLineTarget(
                name='TestLine',
                target_tension=100.0,
                line_lengths=[10.0],
                line_ea=[1000.0],
                section_to_modify=1
            )
        }
        
        calculator = LengthCalculator(targets)
        current_lengths = {'TestLine': 10.0}
        
        # Over-tensioned: needs lengthening (positive adjustment)
        over_tensions = {'TestLine': 110.0}
        adj_over = calculator.calculate_adjustments(over_tensions, current_lengths)
        self.assertGreater(adj_over['TestLine'].length_adjustment, 0)
        self.assertGreater(adj_over['TestLine'].new_length, 10.0)
        
        # Under-tensioned: needs shortening (negative adjustment)
        calculator2 = LengthCalculator(targets)
        under_tensions = {'TestLine': 90.0}
        adj_under = calculator2.calculate_adjustments(under_tensions, current_lengths)
        self.assertLess(adj_under['TestLine'].length_adjustment, 0)
        self.assertLess(adj_under['TestLine'].new_length, 10.0)


if __name__ == '__main__':
    # Run tests with verbose output
    unittest.main(verbosity=2)