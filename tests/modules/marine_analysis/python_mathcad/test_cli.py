"""
Tests for CLI interface of the passing ship forces module.
"""

import pytest
import sys
import os
from pathlib import Path
from unittest.mock import patch, MagicMock
import argparse
import json
import csv
from io import StringIO

# Add the module path to sys.path for imports
module_path = Path(__file__).parent.parent.parent.parent / "src"
if str(module_path) not in sys.path:
    sys.path.insert(0, str(module_path))

try:
    from digitalmodel.modules.marine_analysis.python_code_passing_ship.cli import (
        create_parser,
        main,
        process_single,
        process_batch,
        export_results
    )
except ImportError:
    # If import fails, create mock functions for testing
    import sys
    sys.exit("CLI module not properly configured yet")


class TestCLIArgumentParser:
    """Test CLI argument parsing functionality."""
    
    def test_create_parser_basic(self):
        """Test basic parser creation."""
        parser = create_parser()
        assert isinstance(parser, argparse.ArgumentParser)
        
        # Test help
        with pytest.raises(SystemExit):
            parser.parse_args(['--help'])
    
    def test_single_calculation_args(self):
        """Test single calculation argument parsing."""
        parser = create_parser()
        
        # Test with config file
        args = parser.parse_args(['--config', 'test.yaml'])
        assert args.config == 'test.yaml'
        assert args.batch is False
        
        # Test with output directory
        args = parser.parse_args([
            '--config', 'test.yaml',
            '--output-directory', 'results'
        ])
        assert args.output_directory == 'results'
    
    def test_batch_processing_args(self):
        """Test batch processing argument parsing."""
        parser = create_parser()
        
        # Test batch mode
        args = parser.parse_args([
            '--batch',
            '--input-directory', 'configs',
            '--pattern', '*.yaml'
        ])
        assert args.batch is True
        assert args.input_directory == 'configs'
        assert args.pattern == '*.yaml'
        
        # Test parallel processing
        args = parser.parse_args([
            '--batch',
            '--input-directory', 'configs',
            '--parallel', '4'
        ])
        assert args.parallel == 4
    
    def test_standard_parameter_names(self):
        """Test that standard repository parameter names are supported."""
        parser = create_parser()
        
        # Test all standard parameters
        args = parser.parse_args([
            '--input-directory', 'input_dir',
            '--output-directory', 'output_dir',
            '--pattern', '*.yml',
            '--recursive',
            '--parallel', '8',
            '--config', 'config.yaml',
            '--verbose',
            '--dry-run'
        ])
        
        assert args.input_directory == 'input_dir'
        assert args.output_directory == 'output_dir'
        assert args.pattern == '*.yml'
        assert args.recursive is True
        assert args.parallel == 8
        assert args.config == 'config.yaml'
        assert args.verbose is True
        assert args.dry_run is True
    
    def test_parameter_aliases(self):
        """Test parameter aliases for backward compatibility."""
        parser = create_parser()
        
        # Test short forms
        args = parser.parse_args([
            '-d', 'input_dir',
            '-o', 'output_dir',
            '-v'
        ])
        assert args.input_directory == 'input_dir'
        assert args.output_directory == 'output_dir'
        assert args.verbose is True
        
        # Test alternative forms
        args = parser.parse_args([
            '--directory', 'input_dir',
            '--output', 'output_dir'
        ])
        assert args.input_directory == 'input_dir'
        assert args.output_directory == 'output_dir'
    
    def test_export_format_args(self):
        """Test export format arguments."""
        parser = create_parser()
        
        # Test JSON export
        args = parser.parse_args([
            '--config', 'test.yaml',
            '--export-json', 'results.json'
        ])
        assert args.export_json == 'results.json'
        
        # Test CSV export
        args = parser.parse_args([
            '--config', 'test.yaml',
            '--export-csv', 'results.csv'
        ])
        assert args.export_csv == 'results.csv'
        
        # Test both formats
        args = parser.parse_args([
            '--config', 'test.yaml',
            '--export-json', 'results.json',
            '--export-csv', 'results.csv'
        ])
        assert args.export_json == 'results.json'
        assert args.export_csv == 'results.csv'
    
    def test_visualization_args(self):
        """Test visualization-related arguments."""
        parser = create_parser()
        
        # Test plot generation
        args = parser.parse_args([
            '--config', 'test.yaml',
            '--plot',
            '--plot-format', 'png'
        ])
        assert args.plot is True
        assert args.plot_format == 'png'
        
        # Test interactive mode
        args = parser.parse_args([
            '--config', 'test.yaml',
            '--interactive'
        ])
        assert args.interactive is True
    
    def test_invalid_arguments(self):
        """Test invalid argument combinations."""
        parser = create_parser()
        
        # Test mutually exclusive options (if any)
        # This would depend on implementation details
        
        # Test invalid parallel value
        with pytest.raises(SystemExit):
            parser.parse_args(['--parallel', '0'])
        
        with pytest.raises(SystemExit):
            parser.parse_args(['--parallel', 'abc'])


class TestCLIProcessing:
    """Test CLI processing functions."""
    
    @patch('digitalmodel.modules.marine_analysis.python_code_passing_ship.cli.PassingShipCalculator')
    def test_process_single(self, mock_calculator):
        """Test single calculation processing."""
        # Setup mock
        mock_calc_instance = MagicMock()
        mock_calc_instance.calculate.return_value = {
            'surge_force': 1000.0,
            'sway_force': 500.0,
            'yaw_moment': 2000.0
        }
        mock_calculator.from_config.return_value = mock_calc_instance
        
        # Test processing
        result = process_single('test.yaml', verbose=True)
        
        assert result is not None
        assert 'surge_force' in result
        mock_calculator.from_config.assert_called_once_with('test.yaml')
    
    @patch('digitalmodel.modules.marine_analysis.python_code_passing_ship.cli.PassingShipCalculator')
    @patch('os.listdir')
    def test_process_batch(self, mock_listdir, mock_calculator):
        """Test batch processing."""
        # Setup mocks
        mock_listdir.return_value = ['config1.yaml', 'config2.yaml', 'other.txt']
        
        mock_calc_instance = MagicMock()
        mock_calc_instance.calculate.return_value = {
            'surge_force': 1000.0,
            'sway_force': 500.0,
            'yaw_moment': 2000.0
        }
        mock_calculator.from_config.return_value = mock_calc_instance
        
        # Test batch processing
        results = process_batch(
            input_directory='configs',
            pattern='*.yaml',
            parallel=2,
            verbose=True
        )
        
        assert len(results) == 2
        assert all('surge_force' in r for r in results.values())
    
    def test_export_results_json(self):
        """Test JSON export functionality."""
        results = {
            'surge_force': 1000.0,
            'sway_force': 500.0,
            'yaw_moment': 2000.0,
            'metadata': {
                'vessel_1': 'Tanker',
                'vessel_2': 'Supply',
                'separation': 50.0
            }
        }
        
        # Test JSON export
        output = StringIO()
        export_results(results, format='json', file=output)
        output.seek(0)
        
        exported = json.load(output)
        assert exported['surge_force'] == 1000.0
        assert exported['metadata']['vessel_1'] == 'Tanker'
    
    def test_export_results_csv(self):
        """Test CSV export functionality."""
        results = {
            'surge_force': 1000.0,
            'sway_force': 500.0,
            'yaw_moment': 2000.0,
            'separation': 50.0,
            'water_depth': 100.0
        }
        
        # Test CSV export
        output = StringIO()
        export_results(results, format='csv', file=output)
        output.seek(0)
        
        reader = csv.DictReader(output)
        row = next(reader)
        assert float(row['surge_force']) == 1000.0
        assert float(row['sway_force']) == 500.0


class TestCLIMain:
    """Test main CLI entry point."""
    
    @patch('sys.argv', ['cli.py', '--help'])
    def test_help_message(self, capsys):
        """Test help message display."""
        with pytest.raises(SystemExit) as exc_info:
            main()
        
        assert exc_info.value.code == 0
        captured = capsys.readouterr()
        assert 'Passing Ship Forces Calculator' in captured.out
    
    @patch('sys.argv', ['cli.py', '--config', 'test.yaml'])
    @patch('digitalmodel.modules.marine_analysis.python_code_passing_ship.cli.process_single')
    def test_main_single_calculation(self, mock_process):
        """Test main function with single calculation."""
        mock_process.return_value = {'surge_force': 1000.0}
        
        result = main()
        
        mock_process.assert_called_once()
        assert result == 0
    
    @patch('sys.argv', ['cli.py', '--batch', '--input-directory', 'configs'])
    @patch('digitalmodel.modules.marine_analysis.python_code_passing_ship.cli.process_batch')
    def test_main_batch_processing(self, mock_process):
        """Test main function with batch processing."""
        mock_process.return_value = {
            'config1.yaml': {'surge_force': 1000.0},
            'config2.yaml': {'surge_force': 2000.0}
        }
        
        result = main()
        
        mock_process.assert_called_once()
        assert result == 0
    
    @patch('sys.argv', ['cli.py', '--config', 'test.yaml', '--dry-run'])
    def test_main_dry_run(self, capsys):
        """Test dry run mode."""
        result = main()
        
        captured = capsys.readouterr()
        assert 'DRY RUN' in captured.out
        assert result == 0
    
    @patch('sys.argv', ['cli.py', '--config', 'nonexistent.yaml'])
    @patch('digitalmodel.modules.marine_analysis.python_code_passing_ship.cli.process_single')
    def test_main_error_handling(self, mock_process):
        """Test error handling in main."""
        mock_process.side_effect = FileNotFoundError("Config file not found")
        
        result = main()
        
        assert result == 1


class TestCLIIntegration:
    """Integration tests for CLI functionality."""
    
    def test_module_entry_point(self):
        """Test module can be run with python -m."""
        # This test verifies the __main__.py file works correctly
        import subprocess
        
        result = subprocess.run(
            [
                "C:/Users/Sk Samdan/Desktop/github/digitalmodel/.venv/Scripts/python.exe",
                "-m",
                "digitalmodel.modules.marine_analysis.python_code_passing_ship",
                "--help"
            ],
            capture_output=True,
            text=True
        )
        
        # Check that help is displayed without errors
        assert result.returncode in [0, 2]  # 0 for success, 2 for help
        assert 'Passing Ship Forces' in result.stdout or 'Passing Ship Forces' in result.stderr
    
    def test_cli_with_real_config(self, tmp_path):
        """Test CLI with a real configuration file."""
        # Create a test config
        config_file = tmp_path / "test_config.yaml"
        config_file.write_text("""
vessel_1:
  length: 300.0
  beam: 50.0
  draft: 15.0

vessel_2:
  length: 200.0
  beam: 30.0
  draft: 10.0

environment:
  water_depth: 100.0
  separation_distance: 50.0
  passing_speed: 10.0
""")
        
        # Run CLI
        import subprocess
        result = subprocess.run(
            [
                "C:/Users/Sk Samdan/Desktop/github/digitalmodel/.venv/Scripts/python.exe",
                "-m",
                "digitalmodel.modules.marine_analysis.python_code_passing_ship",
                "--config", str(config_file),
                "--export-json", str(tmp_path / "results.json")
            ],
            capture_output=True,
            text=True
        )
        
        # Check results
        assert result.returncode == 0
        assert (tmp_path / "results.json").exists()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])