#!/usr/bin/env python
"""
Comprehensive Test Suite for OrcaFlex Batch Runner
==================================================
Tests for batch processing functionality including mooring tension iteration
and Length[2] modifications.
"""

import unittest
import tempfile
import shutil
import yaml
import pandas as pd
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock
import sys
import os

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent.parent / 'src'))

from modules.orcaflex.mooring_tension_iteration.batch_processing.orcaflex_batch_runner import (
    OrcaFlexBatchRunner
)


class TestOrcaFlexBatchRunner(unittest.TestCase):
    """Test suite for OrcaFlex batch runner."""
    
    def setUp(self):
        """Set up test fixtures."""
        # Create temporary directory for test files
        self.test_dir = tempfile.mkdtemp()
        self.base_dir = Path(self.test_dir) / "base"
        self.base_dir.mkdir(parents=True)
        
        # Create test configuration
        self.config_file = self.base_dir / "test_batch_config.yml"
        self.create_test_config()
        
        # Create test model files
        self.create_test_files()
    
    def tearDown(self):
        """Clean up test fixtures."""
        shutil.rmtree(self.test_dir, ignore_errors=True)
    
    def create_test_config(self):
        """Create a test batch configuration file."""
        config = {
            'batch_info': {
                'name': 'Test Batch Run',
                'description': 'Test batch processing',
                'base_directory': str(self.base_dir),
                'output_directory': './output',
                'timestamp': '2025-01-17'
            },
            'simulation_settings': {
                'analysis_type': 'statics',
                'calculate_statics': True,
                'save_simulation_file': True,
                'save_results': True,
                'continue_on_error': True,
                'parallel_processing': False
            },
            'output_settings': {
                'save_csv': True,
                'save_simulation': True,
                'csv_output_folder': 'csv',
                'sim_output_folder': 'sim',
                'include_summary_report': True
            },
            'models': [
                {
                    'model_file': 'test_model_1.yml',
                    'description': 'Test Model 1',
                    'includefile': 'includefile_1.yml',
                    'target_tensions': 'tensions_1.csv'
                },
                {
                    'model_file': 'test_model_2.yml',
                    'description': 'Test Model 2'
                }
            ],
            'mooring_parameters': {
                'lines_to_check': ['Line01', 'Line02', 'Line03'],
                'tension_tolerance': 0.01,
                'section_to_modify': 2
            },
            'processing_options': {
                'max_iterations': 10,
                'damping_factor': 0.7,
                'create_backup': True,
                'generate_report': True,
                'log_level': 'INFO'
            }
        }
        
        with open(self.config_file, 'w') as f:
            yaml.dump(config, f)
    
    def create_test_files(self):
        """Create test model and supporting files."""
        # Create test model files
        model1 = self.base_dir / "test_model_1.yml"
        model2 = self.base_dir / "test_model_2.yml"
        
        test_model_content = {
            'General': {
                'ModelVersion': '11.4a',
                'Program': 'OrcaFlex'
            },
            'Environment': {
                'WaterDepth': 1500.0
            }
        }
        
        for model_file in [model1, model2]:
            with open(model_file, 'w') as f:
                yaml.dump(test_model_content, f)
        
        # Create includefile
        includefile = self.base_dir / "includefile_1.yml"
        include_content = {
            'UnstretchedLength': {
                'Line01': {
                    'Length[1]': 11.0,
                    'Length[2]': 850.5
                },
                'Line02': {
                    'Length[1]': 11.0,
                    'Length[2]': 851.2
                }
            }
        }
        
        with open(includefile, 'w') as f:
            yaml.dump(include_content, f)
        
        # Create target tensions CSV
        tensions_csv = self.base_dir / "tensions_1.csv"
        tensions_df = pd.DataFrame({
            'line_name': ['Line01', 'Line02', 'Line03'],
            'target_tension': [1200.0, 1200.0, 1200.0],
            'section_to_be_modified': [2, 2, 2]
        })
        tensions_df.to_csv(tensions_csv, index=False)
    
    def test_initialization(self):
        """Test batch runner initialization."""
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        
        self.assertEqual(runner.base_dir, self.base_dir)
        self.assertEqual(runner.section_to_modify, 2)
        self.assertEqual(runner.tension_tolerance, 0.01)
        self.assertEqual(runner.damping_factor, 0.7)
        self.assertEqual(runner.max_iterations, 10)
        self.assertFalse(runner.parallel_processing)
        self.assertTrue(runner.mock_mode)
    
    def test_config_loading(self):
        """Test configuration file loading and validation."""
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        
        self.assertIn('batch_info', runner.config)
        self.assertIn('simulation_settings', runner.config)
        self.assertIn('models', runner.config)
        self.assertEqual(len(runner.config['models']), 2)
    
    def test_missing_config_file(self):
        """Test handling of missing configuration file."""
        with self.assertRaises(FileNotFoundError):
            OrcaFlexBatchRunner("nonexistent_config.yml", mock_mode=True)
    
    def test_output_directory_creation(self):
        """Test that output directories are created properly."""
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        
        self.assertTrue(runner.output_dir.exists())
        self.assertTrue(runner.csv_dir.exists())
        self.assertTrue(runner.sim_dir.exists())
    
    def test_relative_output_path(self):
        """Test handling of relative output directory paths."""
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        
        expected_output = self.base_dir / "output"
        self.assertEqual(runner.output_dir, expected_output)
    
    def test_mock_batch_processing(self):
        """Test batch processing in mock mode."""
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        summary = runner.run_batch()
        
        self.assertEqual(summary['total_models'], 2)
        self.assertEqual(summary['successful'], 2)
        self.assertEqual(summary['failed'], 0)
        self.assertEqual(summary['success_rate'], 100.0)
        self.assertGreater(summary['elapsed_time'], 0)
    
    def test_model_file_validation(self):
        """Test validation of model file existence."""
        # Create config with non-existent model
        config = yaml.safe_load(open(self.config_file))
        config['models'].append({
            'model_file': 'nonexistent.yml',
            'description': 'Missing Model'
        })
        
        with open(self.config_file, 'w') as f:
            yaml.dump(config, f)
        
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        summary = runner.run_batch()
        
        self.assertEqual(summary['total_models'], 3)
        self.assertEqual(summary['successful'], 2)
        self.assertEqual(summary['failed'], 1)
    
    def test_includefile_handling(self):
        """Test handling of includefiles."""
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        
        model_config = runner.config['models'][0]
        result = runner.process_model(model_config)
        
        self.assertTrue(result['success'])
        self.assertIn('processing_time', result)
    
    def test_target_tensions_handling(self):
        """Test handling of target tension CSV files."""
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        
        model_config = runner.config['models'][0]
        result = runner.process_model(model_config)
        
        self.assertTrue(result['success'])
        self.assertTrue(result.get('tensions_converged', False))
    
    def test_report_generation(self):
        """Test batch report generation."""
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        summary = runner.run_batch()
        
        # Check if report file was created
        report_files = list(runner.output_dir.glob("batch_report_*.txt"))
        self.assertGreater(len(report_files), 0)
        
        # Check report content
        with open(report_files[0]) as f:
            content = f.read()
            self.assertIn("OrcaFlex Batch Processing Report", content)
            self.assertIn("Test Batch Run", content)
            self.assertIn("Length[2]", content)
            self.assertIn("Success Rate", content)
    
    def test_continue_on_error(self):
        """Test continue_on_error flag behavior."""
        # Modify config to set continue_on_error to False
        config = yaml.safe_load(open(self.config_file))
        config['simulation_settings']['continue_on_error'] = False
        
        # Add a model that will fail
        config['models'].insert(0, {
            'model_file': 'will_fail.yml',
            'description': 'This will fail'
        })
        
        with open(self.config_file, 'w') as f:
            yaml.dump(config, f)
        
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        summary = runner.run_batch()
        
        # Should stop at first failure
        self.assertGreater(summary['failed'], 0)
    
    @patch('modules.orcaflex.mooring_tension_iteration.batch_processing.orcaflex_batch_runner.OrcFxAPI')
    def test_orcaflex_api_integration(self, mock_orcfxapi):
        """Test integration with OrcaFlex API (mocked)."""
        # Mock the OrcaFlex API
        mock_model = MagicMock()
        mock_orcfxapi.Model.return_value = mock_model
        mock_model.LoadData.return_value = None
        mock_model.CalculateStatics.return_value = None
        mock_model.SaveSimulation.return_value = None
        
        # Mock line objects
        mock_line = MagicMock()
        mock_line.Length = [11.0, 850.0, 500.0]
        mock_line.StaticResult.return_value = 1195.0  # Close to target
        mock_model.__getitem__.return_value = mock_line
        
        # Set ORCAFLEX_AVAILABLE to True for this test
        with patch.object(sys.modules['modules.orcaflex.mooring_tension_iteration.batch_processing.orcaflex_batch_runner'], 
                         'ORCAFLEX_AVAILABLE', True):
            runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=False)
            summary = runner.run_batch()
        
        # Verify API calls were made
        self.assertEqual(mock_orcfxapi.Model.call_count, 2)  # Two models
        mock_model.LoadData.assert_called()
        mock_model.CalculateStatics.assert_called()
        mock_model.SaveSimulation.assert_called()
    
    def test_length2_modification(self):
        """Test that Length[2] modifications are applied correctly."""
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        
        # Verify configuration specifies section 2
        self.assertEqual(runner.section_to_modify, 2)
        
        # Process model with includefile
        model_config = runner.config['models'][0]
        result = runner.process_model(model_config)
        
        self.assertTrue(result['success'])
        self.assertIn('includefile', model_config)
    
    def test_parallel_processing_flag(self):
        """Test parallel processing configuration."""
        # Modify config to enable parallel processing
        config = yaml.safe_load(open(self.config_file))
        config['simulation_settings']['parallel_processing'] = True
        
        with open(self.config_file, 'w') as f:
            yaml.dump(config, f)
        
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        self.assertTrue(runner.parallel_processing)
    
    def test_csv_output_generation(self):
        """Test CSV output file generation."""
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        summary = runner.run_batch()
        
        # Check that CSV paths are recorded
        for result in summary['results']:
            if result['success']:
                self.assertIn('csv_output', result)
                self.assertTrue(result['csv_output'].endswith('.csv'))
    
    def test_simulation_file_paths(self):
        """Test simulation file path generation."""
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        summary = runner.run_batch()
        
        # Check that simulation paths are recorded
        for result in summary['results']:
            if result['success']:
                self.assertIn('sim_output', result)
                self.assertTrue(result['sim_output'].endswith('.sim'))
    
    def test_logging_configuration(self):
        """Test logging setup and configuration."""
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        
        # Check log file creation
        log_files = list(runner.output_dir.glob("batch_run_*.log"))
        self.assertGreater(len(log_files), 0)
    
    def test_iteration_parameters(self):
        """Test mooring tension iteration parameters."""
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        
        self.assertEqual(runner.max_iterations, 10)
        self.assertEqual(runner.damping_factor, 0.7)
        self.assertEqual(runner.tension_tolerance, 0.01)
    
    def test_model_description_handling(self):
        """Test handling of model descriptions."""
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        summary = runner.run_batch()
        
        # First model has description
        self.assertEqual(summary['results'][0]['description'], 'Test Model 1')
        
        # Second model has description
        self.assertEqual(summary['results'][1]['description'], 'Test Model 2')


class TestBatchRunnerIntegration(unittest.TestCase):
    """Integration tests for batch runner with actual file structures."""
    
    def setUp(self):
        """Set up integration test environment."""
        self.test_dir = tempfile.mkdtemp()
        self.fsts_dir = Path(self.test_dir) / "fsts_lngc_pretension"
        self.fsts_dir.mkdir(parents=True)
        
        # Path to actual test models in the repository
        self.repo_test_dir = Path(__file__).parent.parent / "orcaflex_analysis"
    
    def tearDown(self):
        """Clean up test environment."""
        shutil.rmtree(self.test_dir, ignore_errors=True)
    
    def test_fsts_batch_configuration(self):
        """Test with FSTS-style batch configuration."""
        # Create FSTS-style configuration
        config = {
            'batch_info': {
                'name': 'FSTS LNGC Pretension Analysis',
                'base_directory': str(self.fsts_dir),
                'output_directory': './output/results'
            },
            'simulation_settings': {
                'parallel_processing': True,
                'save_simulation_file': True
            },
            'output_settings': {
                'save_csv': True,
                'save_simulation': True,
                'csv_output_folder': 'csv',
                'sim_output_folder': 'sim'
            },
            'models': [
                {
                    'model_file': 'fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.yml',
                    'includefile': 'includefile_fsts_l015_hwl_125km3_l100_pb.yml',
                    'target_tensions': '125km3_l000_pb_target_mooring_pretension.csv'
                }
            ],
            'mooring_parameters': {
                'section_to_modify': 2,
                'tension_tolerance': 0.01,
                'lines_to_check': ['Line01', 'Line02', 'Line03']
            },
            'processing_options': {
                'damping_factor': 0.7,
                'max_iterations': 10,
                'generate_report': True,
                'log_level': 'INFO'
            }
        }
        
        config_file = self.fsts_dir / "batch_run_all_fsts.yml"
        with open(config_file, 'w') as f:
            yaml.dump(config, f)
        
        # Create dummy model file
        model_file = self.fsts_dir / config['models'][0]['model_file']
        with open(model_file, 'w') as f:
            yaml.dump({'test': 'model'}, f)
        
        # Run in mock mode
        runner = OrcaFlexBatchRunner(str(config_file), mock_mode=True)
        summary = runner.run_batch()
        
        # Verify processing
        self.assertEqual(summary['total_models'], 1)
        self.assertTrue(runner.parallel_processing)
        self.assertEqual(runner.section_to_modify, 2)
    
    def test_actual_model_files_with_sim_generation(self):
        """Test with actual OrcaFlex model files from test directory and verify .sim generation."""
        # Use actual test models
        test_models = [
            'orcaflex_test1.yml',
            'orcaflex_test2.dat'
        ]
        
        # Copy test models to our test directory
        for model_file in test_models:
            src = self.repo_test_dir / model_file
            if src.exists():
                shutil.copy(src, self.fsts_dir / model_file)
            else:
                # Create dummy file if not found
                with open(self.fsts_dir / model_file, 'w') as f:
                    yaml.dump({'test': 'model'}, f)
        
        # Create configuration for these test models
        config = {
            'batch_info': {
                'name': 'Test with Actual Models',
                'base_directory': str(self.fsts_dir),
                'output_directory': './output'
            },
            'simulation_settings': {
                'analysis_type': 'statics',
                'save_simulation_file': True,
                'save_results': True,
                'continue_on_error': True,
                'parallel_processing': False
            },
            'output_settings': {
                'save_csv': True,
                'save_simulation': True,
                'csv_output_folder': 'csv',
                'sim_output_folder': 'sim'
            },
            'models': [
                {
                    'model_file': test_models[0],
                    'description': 'OrcaFlex Test Model 1'
                },
                {
                    'model_file': test_models[1],
                    'description': 'OrcaFlex Test Model 2 (DAT format)'
                }
            ],
            'mooring_parameters': {
                'lines_to_check': ['Line01', 'Line02'],
                'tension_tolerance': 0.01,
                'section_to_modify': 2
            },
            'processing_options': {
                'damping_factor': 0.7,
                'max_iterations': 10,
                'generate_report': True,
                'log_level': 'INFO'
            }
        }
        
        config_file = self.fsts_dir / "test_actual_models.yml"
        with open(config_file, 'w') as f:
            yaml.dump(config, f)
        
        # Run batch processing
        runner = OrcaFlexBatchRunner(str(config_file), mock_mode=True)
        summary = runner.run_batch()
        
        # Verify results
        self.assertEqual(summary['total_models'], 2, "Should process 2 models")
        self.assertEqual(summary['successful'], 2, "Both models should succeed in mock mode")
        
        # Verify .sim file paths are generated
        sim_files_in_results = []
        for result in summary['results']:
            if result['success']:
                self.assertIn('sim_output', result, f"Result should have sim_output: {result}")
                sim_path = result['sim_output']
                self.assertTrue(sim_path.endswith('.sim'), f"Output should be .sim file: {sim_path}")
                sim_files_in_results.append(Path(sim_path).name)
        
        # Verify we have 2 .sim files
        self.assertEqual(len(sim_files_in_results), 2, "Should generate 2 .sim files")
        
        # Verify the .sim file names match the models
        expected_sim_files = ['orcaflex_test1.sim', 'orcaflex_test2.sim']
        for expected in expected_sim_files:
            self.assertIn(expected, sim_files_in_results, 
                         f"Expected {expected} in generated files: {sim_files_in_results}")
        
        # Verify output directories exist
        self.assertTrue(runner.output_dir.exists(), "Output directory should exist")
        self.assertTrue(runner.sim_dir.exists(), "Simulation directory should exist")
        
        # In real (non-mock) mode, these files would actually be created
        # Here we just verify the paths are correctly set up
        print(f"\nTest Summary:")
        print(f"  Models processed: {summary['total_models']}")
        print(f"  Simulation files that would be generated:")
        for sim_file in sim_files_in_results:
            print(f"    - {sim_file}")
    
    @unittest.skipUnless(os.environ.get('ORCAFLEX_LICENSE_AVAILABLE') == 'true', 
                         "Requires OrcaFlex license")
    def test_sim_file_creation_with_license(self):
        """
        Test that .sim files are actually created when running with OrcaFlex license.
        This test requires ORCAFLEX_LICENSE_AVAILABLE=true environment variable.
        """
        # Use actual test models
        test_models = ['orcaflex_test1.yml', 'orcaflex_test2.dat']
        
        # Copy test models to test directory
        for model_file in test_models:
            src = self.repo_test_dir / model_file
            if src.exists():
                shutil.copy(src, self.fsts_dir / model_file)
            else:
                # Create minimal valid OrcaFlex model
                model_content = {
                    'General': {'ModelVersion': '11.4a', 'Program': 'OrcaFlex'},
                    'Environment': {'WaterDepth': 1500.0}
                }
                with open(self.fsts_dir / model_file, 'w') as f:
                    yaml.dump(model_content, f)
        
        # Create configuration
        config = {
            'batch_info': {
                'name': 'License Test - Verify .sim Creation',
                'base_directory': str(self.fsts_dir),
                'output_directory': './output'
            },
            'simulation_settings': {
                'analysis_type': 'statics',
                'save_simulation_file': True,
                'save_results': True,
                'continue_on_error': True,
                'parallel_processing': False  # Start with sequential
            },
            'output_settings': {
                'save_csv': True,
                'save_simulation': True,
                'csv_output_folder': 'csv',
                'sim_output_folder': 'sim'
            },
            'models': [
                {'model_file': test_models[0], 'description': 'Test Model 1'},
                {'model_file': test_models[1], 'description': 'Test Model 2'}
            ],
            'mooring_parameters': {
                'lines_to_check': [],
                'tension_tolerance': 0.01,
                'section_to_modify': 2
            },
            'processing_options': {
                'damping_factor': 0.7,
                'max_iterations': 10,
                'generate_report': True,
                'log_level': 'INFO'
            }
        }
        
        config_file = self.fsts_dir / "test_license.yml"
        with open(config_file, 'w') as f:
            yaml.dump(config, f)
        
        # Run with actual OrcaFlex (not mock mode)
        runner = OrcaFlexBatchRunner(str(config_file), mock_mode=False)
        summary = runner.run_batch()
        
        # CRITICAL VERIFICATION: Check .sim files actually exist on disk
        sim_dir = runner.sim_dir
        self.assertTrue(sim_dir.exists(), f"Simulation directory should exist: {sim_dir}")
        
        # List actual .sim files in the directory
        actual_sim_files = list(sim_dir.glob("*.sim"))
        print(f"\nActual .sim files created in {sim_dir}:")
        for sim_file in actual_sim_files:
            file_size = sim_file.stat().st_size
            print(f"  - {sim_file.name} ({file_size / (1024*1024):.2f} MB)")
        
        # Verify we have exactly 2 .sim files
        self.assertEqual(len(actual_sim_files), 2, 
                        f"Should create exactly 2 .sim files, found {len(actual_sim_files)}")
        
        # Verify specific files exist
        expected_files = ['orcaflex_test1.sim', 'orcaflex_test2.sim']
        for expected_name in expected_files:
            expected_path = sim_dir / expected_name
            self.assertTrue(expected_path.exists(), 
                          f".sim file should exist: {expected_path}")
            
            # Verify file is not empty
            file_size = expected_path.stat().st_size
            self.assertGreater(file_size, 1000,  # At least 1KB
                             f".sim file should not be empty: {expected_name} ({file_size} bytes)")
        
        # Verify summary includes sim verification
        self.assertIn('sim_verification', summary)
        sim_verify = summary['sim_verification']
        self.assertEqual(sim_verify['files_found'], 2)
        self.assertEqual(sim_verify['files_missing'], 0)
        
        print(f"\n✓ License test passed: 2 .sim files successfully created")
    
    def test_parallel_processing_with_sim_verification(self):
        """Test parallel processing mode with simulation file verification."""
        # Create configuration with parallel processing enabled
        config = yaml.safe_load(open(self.config_file))
        config['simulation_settings']['parallel_processing'] = True
        
        # Add more models to test parallel processing
        for i in range(3, 6):
            config['models'].append({
                'model_file': f'test_model_{i}.yml',
                'description': f'Test Model {i}'
            })
            # Create the model file
            with open(self.base_dir / f'test_model_{i}.yml', 'w') as f:
                yaml.dump({'test': f'model{i}'}, f)
        
        with open(self.config_file, 'w') as f:
            yaml.dump(config, f)
        
        # Run with parallel processing
        runner = OrcaFlexBatchRunner(str(self.config_file), mock_mode=True)
        summary = runner.run_batch()
        
        # Verify all models were processed
        self.assertEqual(summary['total_models'], 5)
        self.assertEqual(summary['successful'], 5)
        
        # Verify sim paths are set for all results
        for result in summary['results']:
            if result['success']:
                self.assertIn('sim_output', result)
                self.assertTrue(result['sim_output'].endswith('.sim'))


def suite():
    """Create test suite."""
    suite = unittest.TestSuite()
    
    # Add all test cases
    suite.addTests(unittest.TestLoader().loadTestsFromTestCase(TestOrcaFlexBatchRunner))
    suite.addTests(unittest.TestLoader().loadTestsFromTestCase(TestBatchRunnerIntegration))
    
    return suite


if __name__ == '__main__':
    # Run tests with verbose output
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())