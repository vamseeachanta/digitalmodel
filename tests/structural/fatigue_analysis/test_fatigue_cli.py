#!/usr/bin/env python3
"""
Test Suite for Fatigue Analysis CLI

Tests the command-line interface for the fatigue analysis module.
"""

import pytest
import sys
import os
from pathlib import Path
import tempfile
import json
import pandas as pd
from unittest.mock import patch, MagicMock, call
from io import StringIO

# Add module path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from digitalmodel.fatigue_analysis.__main__ import (
    parse_arguments,
    validate_inputs,
    setup_logging,
    run_analysis
)


class TestArgumentParsing:
    """Test command line argument parsing"""
    
    def test_default_arguments(self):
        """Test default argument values"""
        with patch('sys.argv', ['prog']):
            args = parse_arguments()
            
            assert args.input_directory is None
            assert args.output_directory == 'output'
            assert args.struts == '1,2,3,4,5,6,7,8'
            assert args.sn_curve == 'ABS_E_AIR'
            assert args.scf == 1.0
            assert args.design_life == 20.0
            assert args.timesteps == 1000
            assert not args.sample
            assert not args.dry_run
            assert not args.verbose
    
    def test_input_directory_arguments(self):
        """Test input directory argument variations"""
        test_cases = [
            (['prog', '--input-directory', '/path/to/data'], '/path/to/data'),
            (['prog', '--directory', '/path/to/data'], '/path/to/data'),
            (['prog', '-d', '/path/to/data'], '/path/to/data'),
        ]
        
        for argv, expected in test_cases:
            with patch('sys.argv', argv):
                args = parse_arguments()
                assert args.input_directory == expected
    
    def test_output_directory_arguments(self):
        """Test output directory argument variations"""
        test_cases = [
            (['prog', '--output-directory', '/path/to/output'], '/path/to/output'),
            (['prog', '--output', '/path/to/output'], '/path/to/output'),
            (['prog', '-o', '/path/to/output'], '/path/to/output'),
        ]
        
        for argv, expected in test_cases:
            with patch('sys.argv', argv):
                args = parse_arguments()
                assert args.output_directory == expected
    
    def test_configuration_selection(self):
        """Test configuration selection arguments"""
        with patch('sys.argv', ['prog', '--configs', 'fsts_l015,fsts_l095']):
            args = parse_arguments()
            assert args.configs == 'fsts_l015,fsts_l095'
    
    def test_strut_selection(self):
        """Test strut selection arguments"""
        with patch('sys.argv', ['prog', '--struts', '1,2,3']):
            args = parse_arguments()
            assert args.struts == '1,2,3'
    
    def test_sn_curve_selection(self):
        """Test S-N curve selection"""
        curves = ['ABS_E_AIR', 'ABS_F_AIR', 'DNV_D_AIR', 'DNV_C_SEAWATER']
        
        for curve in curves:
            with patch('sys.argv', ['prog', '--sn-curve', curve]):
                args = parse_arguments()
                assert args.sn_curve == curve
    
    def test_sample_mode(self):
        """Test sample data mode"""
        with patch('sys.argv', ['prog', '--sample', '--timesteps', '500']):
            args = parse_arguments()
            assert args.sample is True
            assert args.timesteps == 500
    
    def test_processing_options(self):
        """Test processing control options"""
        with patch('sys.argv', ['prog', '--parallel', '4', '--dry-run', '--verbose']):
            args = parse_arguments()
            assert args.parallel == 4
            assert args.dry_run is True
            assert args.verbose is True
    
    def test_fatigue_parameters(self):
        """Test fatigue analysis parameters"""
        with patch('sys.argv', ['prog', '--scf', '1.5', '--design-life', '30']):
            args = parse_arguments()
            assert args.scf == 1.5
            assert args.design_life == 30.0


class TestInputValidation:
    """Test input validation"""
    
    def test_valid_inputs(self, tmp_path):
        """Test validation with valid inputs"""
        # Create mock directory
        data_dir = tmp_path / "data"
        data_dir.mkdir()
        
        args = MagicMock()
        args.input_directory = str(data_dir)
        args.conditions = None
        args.weights = None
        args.stress_table = None
        args.struts = "1,2,3"
        args.sample = False
        
        with patch('digitalmodel.fatigue_analysis.__main__.logging') as mock_logging:
            result = validate_inputs(args)
            assert result is True
            mock_logging.error.assert_not_called()
    
    def test_missing_input_directory(self):
        """Test validation with missing input directory"""
        args = MagicMock()
        args.input_directory = "/nonexistent/path"
        args.conditions = None
        args.weights = None
        args.stress_table = None
        args.struts = "1,2,3"
        args.sample = False
        
        with patch('digitalmodel.fatigue_analysis.__main__.logging') as mock_logging:
            result = validate_inputs(args)
            assert result is False
            mock_logging.error.assert_called()
    
    def test_invalid_strut_numbers(self):
        """Test validation with invalid strut numbers"""
        args = MagicMock()
        args.input_directory = None
        args.sample = True
        args.conditions = None
        args.weights = None
        args.stress_table = None
        args.struts = "1,9,10"  # Invalid: > 8
        
        with patch('digitalmodel.fatigue_analysis.__main__.logging') as mock_logging:
            result = validate_inputs(args)
            assert result is False
            assert any("between 1 and 8" in str(call) for call in mock_logging.error.call_args_list)
    
    def test_missing_optional_files(self, tmp_path):
        """Test validation with missing optional files"""
        args = MagicMock()
        args.input_directory = None
        args.sample = True
        args.conditions = str(tmp_path / "missing_conditions.csv")
        args.weights = str(tmp_path / "missing_weights.csv")
        args.stress_table = str(tmp_path / "missing_stress.csv")
        args.struts = "1,2"
        
        with patch('digitalmodel.fatigue_analysis.__main__.logging') as mock_logging:
            result = validate_inputs(args)
            assert result is False
            assert mock_logging.error.call_count >= 3  # Three missing files


class TestLogging:
    """Test logging configuration"""
    
    def test_normal_logging(self):
        """Test normal logging setup"""
        with patch('digitalmodel.fatigue_analysis.__main__.logging') as mock_logging:
            setup_logging(verbose=False)
            mock_logging.basicConfig.assert_called_once()
            config_call = mock_logging.basicConfig.call_args
            assert config_call[1]['level'] == mock_logging.INFO
    
    def test_verbose_logging(self):
        """Test verbose logging setup"""
        with patch('digitalmodel.fatigue_analysis.__main__.logging') as mock_logging:
            setup_logging(verbose=True)
            mock_logging.basicConfig.assert_called_once()
            config_call = mock_logging.basicConfig.call_args
            assert config_call[1]['level'] == mock_logging.DEBUG


class TestAnalysisExecution:
    """Test analysis execution"""
    
    def test_sample_mode_execution(self, tmp_path):
        """Test execution in sample mode"""
        args = MagicMock()
        args.sample = True
        args.input_directory = None
        args.output_directory = str(tmp_path / "output")
        args.configs = None
        args.struts = "1,2"
        args.conditions = None
        args.weights = None
        args.stress_table = None
        args.sn_curve = 'ABS_E_AIR'
        args.scf = 1.0
        args.design_life = 20.0
        args.timesteps = 100
        args.dry_run = False
        args.verbose = False
        args.export_intermediate = False
        
        with patch('digitalmodel.fatigue_analysis.__main__.IntegratedFatigueProcessor') as MockProcessor:
            with patch('digitalmodel.fatigue_analysis.__main__.ProductionDataHandler') as MockHandler:
                with patch('digitalmodel.fatigue_analysis.__main__.main') as mock_main:
                    mock_main.return_value = ([], {})
                    
                    result = run_analysis(args)
                    
                    # Should use sample_data path
                    MockHandler.assert_called_with(
                        base_path="sample_data",
                        sample_timesteps=100
                    )
    
    def test_production_mode_execution(self, tmp_path):
        """Test execution with production data"""
        data_dir = tmp_path / "data"
        data_dir.mkdir()
        
        args = MagicMock()
        args.sample = False
        args.input_directory = str(data_dir)
        args.output_directory = str(tmp_path / "output")
        args.configs = "fsts_l015"
        args.struts = "1"
        args.conditions = None
        args.weights = None
        args.stress_table = None
        args.sn_curve = 'DNV_D_AIR'
        args.scf = 1.2
        args.design_life = 25.0
        args.timesteps = 1000
        args.dry_run = False
        args.verbose = False
        args.export_intermediate = False
        
        with patch('digitalmodel.fatigue_analysis.__main__.IntegratedFatigueProcessor') as MockProcessor:
            with patch('digitalmodel.fatigue_analysis.__main__.ProductionDataHandler') as MockHandler:
                with patch('digitalmodel.fatigue_analysis.__main__.main') as mock_main:
                    mock_main.return_value = ([{"test": "result"}], {"summary": "data"})
                    
                    result = run_analysis(args)
                    
                    # Should use provided data directory
                    MockHandler.assert_called_with(
                        base_path=str(data_dir),
                        sample_timesteps=1000
                    )
                    
                    assert result is True
    
    def test_dry_run_mode(self, tmp_path):
        """Test dry run preview mode"""
        args = MagicMock()
        args.sample = True
        args.input_directory = None
        args.output_directory = str(tmp_path / "output")
        args.configs = None
        args.struts = "1,2,3,4,5,6,7,8"
        args.conditions = None
        args.weights = None
        args.stress_table = None
        args.sn_curve = 'ABS_E_AIR'
        args.scf = 1.0
        args.design_life = 20.0
        args.timesteps = 1000
        args.dry_run = True
        args.verbose = False
        args.export_intermediate = False
        
        with patch('digitalmodel.fatigue_analysis.__main__.IntegratedFatigueProcessor') as MockProcessor:
            with patch('digitalmodel.fatigue_analysis.__main__.ProductionDataHandler') as MockHandler:
                mock_handler = MagicMock()
                mock_handler.configurations = {
                    'config1': MagicMock(description="Test Config", weight=50.0)
                }
                MockHandler.return_value = mock_handler
                
                with patch('digitalmodel.fatigue_analysis.__main__.logging') as mock_logging:
                    result = run_analysis(args)
                    
                    # Should not run actual analysis in dry-run
                    assert result is True
                    # Should log preview information
                    assert any("DRY RUN" in str(call) for call in mock_logging.info.call_args_list)
                    assert any("without --dry-run" in str(call) for call in mock_logging.info.call_args_list)
    
    def test_custom_conditions_loading(self, tmp_path):
        """Test loading custom fatigue conditions"""
        # Create conditions file
        conditions_file = tmp_path / "conditions.csv"
        df = pd.DataFrame({
            'Row': [1],
            'Wind Speed (m/s)': [10.0],
            'Wind Dir (°)': [45],
            'Hs (m)': [2.0],
            'Tp (s)': [8.0],
            'Wave Dir (°)': [90],
            'Occurrence (%)': [5.0]
        })
        df.to_csv(conditions_file, index=False)
        
        args = MagicMock()
        args.sample = True
        args.input_directory = None
        args.output_directory = str(tmp_path / "output")
        args.configs = None
        args.struts = "1"
        args.conditions = str(conditions_file)
        args.weights = None
        args.stress_table = None
        args.sn_curve = 'ABS_E_AIR'
        args.scf = 1.0
        args.design_life = 20.0
        args.timesteps = 100
        args.dry_run = False
        args.verbose = False
        args.export_intermediate = False
        
        with patch('digitalmodel.fatigue_analysis.__main__.IntegratedFatigueProcessor') as MockProcessor:
            with patch('digitalmodel.fatigue_analysis.__main__.ProductionDataHandler') as MockHandler:
                with patch('digitalmodel.fatigue_analysis.__main__.main') as mock_main:
                    mock_processor = MagicMock()
                    MockProcessor.return_value = mock_processor
                    mock_main.return_value = ([], {})
                    
                    result = run_analysis(args)
                    
                    # Should load and set custom conditions
                    assert len(mock_processor.fatigue_conditions) > 0


class TestEndToEnd:
    """End-to-end integration tests"""
    
    @pytest.mark.integration
    def test_help_display(self, capsys):
        """Test help message display"""
        with patch('sys.argv', ['prog', '--help']):
            with pytest.raises(SystemExit) as exc_info:
                parse_arguments()
            
            assert exc_info.value.code == 0
            captured = capsys.readouterr()
            assert "Reference Seastate Scaling Fatigue Analysis" in captured.out
            assert "--input-directory" in captured.out
            assert "--sn-curve" in captured.out
            assert "Examples:" in captured.out
    
    @pytest.mark.integration
    def test_complete_cli_flow(self, tmp_path, monkeypatch):
        """Test complete CLI execution flow"""
        # Create minimal data structure
        data_dir = tmp_path / "test_data"
        data_dir.mkdir()
        output_dir = tmp_path / "output"
        
        # Mock sys.argv
        test_args = [
            'prog',
            '--input-directory', str(data_dir),
            '--output-directory', str(output_dir),
            '--struts', '1',
            '--timesteps', '10',
            '--dry-run'
        ]
        
        with patch('sys.argv', test_args):
            # Mock the main function to avoid actual processing
            with patch('digitalmodel.fatigue_analysis.__main__.run_analysis') as mock_run:
                mock_run.return_value = True
                
                # Import and run main
                from digitalmodel.fatigue_analysis.__main__ import main
                
                with pytest.raises(SystemExit) as exc_info:
                    main()
                
                assert exc_info.value.code == 0
                mock_run.assert_called_once()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])