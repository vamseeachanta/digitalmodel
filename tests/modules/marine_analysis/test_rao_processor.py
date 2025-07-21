"""Test suite for RAO data processing module."""

import unittest
import numpy as np
import pandas as pd
from pathlib import Path
import os

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from src.digitalmodel.modules.marine_analysis import (
    RAODataProcessor, RAOData, AQWAReader, OrcaFlexReader,
    RAODataValidators, ValidationReport
)


class TestRAODataProcessing(unittest.TestCase):
    """Comprehensive test suite for RAO data processing."""
    
    @classmethod
    def setUpClass(cls):
        """Set up test fixtures."""
        cls.test_data_dir = Path("tests/modules/rao_analysis")
        cls.aqwa_file = cls.test_data_dir / "NO_DAMP_FST1_L015.LIS"
        cls.orcaflex_file = cls.test_data_dir / "SS_Off_0_8P.yml"
        
    def setUp(self):
        """Set up each test."""
        self.processor = RAODataProcessor()
    
    def test_aqwa_lis_import_success(self):
        """Test successful AQWA .lis file import."""
        # Check if test file exists
        if not self.aqwa_file.exists():
            self.skipTest(f"Test file {self.aqwa_file} not found")
        
        # Import RAO data
        rao_data = self.processor.import_aqwa_lis_file(str(self.aqwa_file))
        
        # Validate data structure
        self.assertIsInstance(rao_data, RAOData)
        self.assertGreater(len(rao_data.frequencies), 0)
        self.assertGreater(len(rao_data.headings), 0)
        
        # Check all DOFs present
        required_dofs = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        for dof in required_dofs:
            self.assertIn(dof, rao_data.raos)
            self.assertIn('amplitude', rao_data.raos[dof])
            self.assertIn('phase', rao_data.raos[dof])
        
        # Test DataFrame output
        df_dict = self.processor.get_rao_dataframes(rao_data)
        self.assertIn('amplitude', df_dict)
        self.assertIn('phase', df_dict)
        self.assertIn('verification_info', df_dict)
        
        # Validate DataFrame structure
        amp_df = df_dict['amplitude']
        phase_df = df_dict['phase']
        
        self.assertIsInstance(amp_df.columns, pd.MultiIndex)
        self.assertEqual(amp_df.columns.names, ['DOF', 'Heading'])
        self.assertEqual(phase_df.columns.names, ['DOF', 'Heading'])
        self.assertEqual(amp_df.index.name, 'Frequency (rad/s)')
        
        # Validate all DOFs and headings present
        dofs_in_df = amp_df.columns.get_level_values('DOF').unique()
        for dof in required_dofs:
            self.assertIn(dof, dofs_in_df)
        
        # Validate verification info present
        self.assertTrue(df_dict['verification_info']['requires_manual_verification'])
        self.assertEqual(len(df_dict['verification_info']['verification_checklist']), 4)
    
    def test_orcaflex_yml_import_success(self):
        """Test successful OrcaFlex YAML file import."""
        # Check if test file exists
        if not self.orcaflex_file.exists():
            self.skipTest(f"Test file {self.orcaflex_file} not found")
        
        # Import RAO data
        rao_data = self.processor.import_orcaflex_yml_file(str(self.orcaflex_file))
        
        # Validate data structure
        self.assertIsInstance(rao_data, RAOData)
        self.assertGreater(len(rao_data.frequencies), 0)
        self.assertGreater(len(rao_data.headings), 0)
        
        # Check vessel name was extracted
        self.assertNotEqual(rao_data.vessel_name, '')
        
        # Test DataFrame conversion
        df_dict = self.processor.get_rao_dataframes(rao_data)
        self.assertIn('amplitude', df_dict)
        self.assertIn('phase', df_dict)
    
    def test_manual_verification_workflow(self):
        """Test manual verification workflow and metadata."""
        # Create sample RAO data for testing
        frequencies = np.linspace(0.1, 2.0, 20)
        headings = np.arange(0, 360, 15)
        
        rao_data = RAOData(
            frequencies=frequencies,
            headings=headings,
            raos={
                dof: {
                    'amplitude': np.random.rand(len(frequencies), len(headings)),
                    'phase': np.random.uniform(-180, 180, (len(frequencies), len(headings)))
                }
                for dof in ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
            },
            source_file="test_file.yml"
        )
        
        # Get DataFrames with verification info
        df_dict = self.processor.get_rao_dataframes(rao_data)
        
        # Create verification report
        verification_report = self.processor.create_verification_report(df_dict)
        
        # Test verification report contains required checks
        self.assertIsInstance(verification_report.amplitude_check_required, bool)
        self.assertIsInstance(verification_report.phase_continuity_check_required, bool)
        self.assertIsNotNone(verification_report.spot_check_locations)
        self.assertIsNotNone(verification_report.symmetry_pairs)
        
        # Test manual verification helper functions
        spot_checks = self.processor.generate_spot_check_values(
            df_dict['amplitude'], n_samples=10
        )
        self.assertEqual(len(spot_checks), 10)
        
        # Check spot check structure
        for check in spot_checks:
            self.assertIn('frequency', check)
            self.assertIn('dof', check)
            self.assertIn('heading', check)
            self.assertIn('value', check)
            self.assertIn('location_in_source', check)
        
        # Test phase continuity check
        phase_issues = self.processor.check_phase_continuity(df_dict['phase'])
        self.assertIsInstance(phase_issues, list)
    
    def test_data_validation_quality_checks(self):
        """Test comprehensive data validation."""
        # Create data with known issues
        frequencies = np.array([0.5, 1.0, 1.5])  # Missing low frequencies
        headings = np.arange(0, 180, 30)  # Incomplete heading coverage
        
        rao_data = RAOData(
            frequencies=frequencies,
            headings=headings,
            raos={
                'surge': {
                    'amplitude': np.ones((3, 6)) * 10.0,  # Excessive amplitude
                    'phase': np.zeros((3, 6))
                },
                'sway': {
                    'amplitude': np.ones((3, 6)),
                    'phase': np.zeros((3, 6))
                },
                # Missing other DOFs
            }
        )
        
        validator = RAODataValidators()
        report = validator.validate_rao_data(rao_data)
        
        # Should detect quality issues
        self.assertFalse(report.is_valid)
        self.assertGreater(len(report.errors), 0)
        self.assertGreater(len(report.warnings), 0)
        
        # Check specific issues were detected
        error_messages = ' '.join(report.errors).lower()
        warning_messages = ' '.join(report.warnings).lower()
        
        self.assertIn('missing rao data', error_messages)  # Missing DOFs
        self.assertIn('frequency', warning_messages)  # Frequency range issue
        self.assertIn('heading', warning_messages)  # Heading coverage issue
    
    def test_interpolation_functionality(self):
        """Test RAO interpolation."""
        # Create simple RAO data
        source_freq = np.array([0.1, 0.5, 1.0, 1.5, 2.0])
        source_head = np.arange(0, 360, 30)
        
        rao_data = RAOData(
            frequencies=source_freq,
            headings=source_head,
            raos={
                dof: {
                    'amplitude': np.random.rand(len(source_freq), len(source_head)),
                    'phase': np.random.uniform(-180, 180, (len(source_freq), len(source_head)))
                }
                for dof in ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
            }
        )
        
        # Define target grids
        target_freq = np.linspace(0.1, 2.0, 10)
        target_head = np.arange(0, 360, 15)
        
        # Interpolate
        interp_data = self.processor.interpolate_rao_data(
            rao_data, target_freq, target_head
        )
        
        # Validate interpolated data
        self.assertEqual(len(interp_data.frequencies), len(target_freq))
        self.assertEqual(len(interp_data.headings), len(target_head))
        
        # Check data shape
        for dof in interp_data.raos:
            amp_shape = interp_data.raos[dof]['amplitude'].shape
            self.assertEqual(amp_shape, (len(target_freq), len(target_head)))
    
    def test_phase_continuity_detection(self):
        """Test phase discontinuity detection."""
        # Create data with phase jump
        frequencies = np.linspace(0.1, 2.0, 10)
        headings = np.array([0, 90, 180, 270])
        
        # Create phase data with discontinuity
        phase_data = np.zeros((len(frequencies), len(headings)))
        phase_data[5:, 0] = 170  # Jump from 0 to 170 degrees
        
        df = pd.DataFrame(
            phase_data,
            index=pd.Index(frequencies, name='Frequency (rad/s)'),
            columns=pd.MultiIndex.from_tuples(
                [('surge', f'{h}deg') for h in headings],
                names=['DOF', 'Heading']
            )
        )
        
        # Check phase continuity
        discontinuities = self.processor.check_phase_continuity(df, threshold=90.0)
        
        # Should detect the discontinuity
        self.assertGreater(len(discontinuities), 0)
        
        # Verify discontinuity details
        for disc in discontinuities:
            self.assertIn('dof', disc)
            self.assertIn('heading', disc)
            self.assertIn('frequency_index', disc)
            self.assertIn('phase_jump', disc)
            self.assertTrue(disc['requires_manual_check'])


if __name__ == '__main__':
    unittest.main()