"""
ABOUTME: Integration tests for signal_analysis CLI
ABOUTME: Tests rainflow counting, FFT/spectral analysis, and signal filtering via CLI
"""

import pytest
import json
import numpy as np
import pandas as pd
from pathlib import Path
from unittest.mock import patch, MagicMock

from digitalmodel.modules.signal_analysis.cli import cli
from tests.modules.cli.conftest import assert_cli_success, assert_cli_failure, assert_json_output, assert_output_contains


class TestRainflowCommand:
    """Tests for the 'rainflow' command"""

    @patch('digitalmodel.modules.signal_analysis.cli.RainflowCounter')
    def test_basic_rainflow_counting(self, mock_counter, cli_runner, temp_output_dir):
        """Test basic rainflow cycle counting"""
        # Create test input file
        input_file = temp_output_dir / "signal.csv"
        signal_data = np.sin(np.linspace(0, 10*np.pi, 100))
        pd.DataFrame({'signal': signal_data}).to_csv(input_file, index=False)

        # Mock rainflow counter
        mock_counter_instance = MagicMock()
        mock_counter.return_value = mock_counter_instance

        # Mock cycles
        mock_cycles = [
            {'range': 1.5, 'mean': 0.2, 'count': 1.0},
            {'range': 1.2, 'mean': 0.1, 'count': 1.0},
            {'range': 0.8, 'mean': 0.0, 'count': 2.0},
        ]
        mock_counter_instance.count_cycles.return_value = mock_cycles

        # Mock statistics
        mock_stats = {
            'total_cycles': 4.0,
            'max_range': 1.5,
            'mean_range': 1.17,
            'std_range': 0.35
        }
        mock_counter_instance.get_statistics.return_value = mock_stats

        result = cli_runner.invoke(cli, [
            'rainflow',
            str(input_file),
            '--column', 'signal'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'Rainflow Cycle Counting Results',
            'Total Cycles:',
            '4.0',
            'Max Range:',
            '1.50',
            'Mean Range:'
        )

    @patch('digitalmodel.modules.signal_analysis.cli.RainflowCounter')
    def test_rainflow_astm_method(self, mock_counter, cli_runner, temp_output_dir):
        """Test rainflow with ASTM method"""
        input_file = temp_output_dir / "signal.txt"
        np.savetxt(input_file, np.random.randn(200))

        mock_counter_instance = MagicMock()
        mock_counter.return_value = mock_counter_instance

        mock_cycles = [{'range': 2.0, 'mean': 0.0, 'count': 1.0}]
        mock_counter_instance.count_cycles.return_value = mock_cycles

        mock_stats = {
            'total_cycles': 25.0,
            'max_range': 2.5,
            'mean_range': 1.2,
            'std_range': 0.4
        }
        mock_counter_instance.get_statistics.return_value = mock_stats

        result = cli_runner.invoke(cli, [
            'rainflow',
            str(input_file),
            '--method', 'astm'
        ])

        assert_cli_success(result)
        assert 'ASTM' in result.output

    @patch('digitalmodel.modules.signal_analysis.cli.RainflowCounter')
    def test_rainflow_with_json_output(self, mock_counter, cli_runner, temp_output_dir):
        """Test rainflow counting with JSON output"""
        input_file = temp_output_dir / "signal.csv"
        signal_data = np.sin(np.linspace(0, 10*np.pi, 100))
        pd.DataFrame({'data': signal_data}).to_csv(input_file, index=False)

        mock_counter_instance = MagicMock()
        mock_counter.return_value = mock_counter_instance

        mock_cycles = [
            {'range': 1.5, 'mean': 0.2, 'count': 1.0},
            {'range': 1.2, 'mean': 0.1, 'count': 1.0},
        ]
        mock_counter_instance.count_cycles.return_value = mock_cycles

        mock_stats = {
            'total_cycles': 10.0,
            'max_range': 1.8,
            'mean_range': 1.1,
            'std_range': 0.3
        }
        mock_counter_instance.get_statistics.return_value = mock_stats

        output_file = temp_output_dir / "rainflow_results.json"

        result = cli_runner.invoke(cli, [
            'rainflow',
            str(input_file),
            '--column', 'data',
            '--output', str(output_file)
        ])

        assert_cli_success(result)

        # Validate JSON output
        data = assert_json_output(output_file, [
            'method',
            'statistics',
            'cycles'
        ])

        assert data['statistics']['total_cycles'] == 10.0
        assert len(data['cycles']) > 0

    def test_rainflow_missing_input_file(self, cli_runner):
        """Test error for non-existent input file"""
        result = cli_runner.invoke(cli, [
            'rainflow',
            '/nonexistent/file.csv'
        ])

        assert_cli_failure(result)


class TestFFTCommand:
    """Tests for the 'fft' command"""

    @patch('digitalmodel.modules.signal_analysis.cli.SpectralAnalyzer')
    def test_basic_fft_analysis(self, mock_analyzer, cli_runner, temp_output_dir):
        """Test basic FFT spectral analysis"""
        # Create test signal
        input_file = temp_output_dir / "signal.csv"
        t = np.linspace(0, 1, 1000)
        signal = np.sin(2*np.pi*5*t) + 0.5*np.sin(2*np.pi*12*t)
        pd.DataFrame({'signal': signal}).to_csv(input_file, index=False)

        # Mock analyzer
        mock_analyzer_instance = MagicMock()
        mock_analyzer.return_value = mock_analyzer_instance

        # Mock spectrum
        mock_spectrum = {
            'frequencies': np.linspace(0, 500, 1000),
            'amplitudes': np.random.rand(1000)
        }
        mock_analyzer_instance.compute_spectrum.return_value = mock_spectrum

        # Mock peaks
        mock_peaks = [
            {'frequency': 5.0, 'amplitude': 0.5},
            {'frequency': 12.0, 'amplitude': 0.25},
            {'frequency': 24.0, 'amplitude': 0.1},
        ]
        mock_analyzer_instance.find_peaks.return_value = mock_peaks

        result = cli_runner.invoke(cli, [
            'fft',
            str(input_file),
            '--column', 'signal',
            '--sampling-rate', '1000'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'FFT/Spectral Analysis Results',
            'Sampling Rate:',
            '1000',
            'Dominant Frequencies:',
            '5.0000 Hz'
        )

    @patch('digitalmodel.modules.signal_analysis.cli.SpectralAnalyzer')
    def test_fft_welch_method(self, mock_analyzer, cli_runner, temp_output_dir):
        """Test FFT with Welch method"""
        input_file = temp_output_dir / "signal.txt"
        np.savetxt(input_file, np.random.randn(500))

        mock_analyzer_instance = MagicMock()
        mock_analyzer.return_value = mock_analyzer_instance

        mock_spectrum = {
            'frequencies': np.linspace(0, 250, 500),
            'amplitudes': np.random.rand(500)
        }
        mock_analyzer_instance.compute_spectrum.return_value = mock_spectrum

        mock_peaks = [
            {'frequency': 10.5, 'amplitude': 0.8},
            {'frequency': 25.0, 'amplitude': 0.4},
        ]
        mock_analyzer_instance.find_peaks.return_value = mock_peaks

        result = cli_runner.invoke(cli, [
            'fft',
            str(input_file),
            '--sampling-rate', '500',
            '--method', 'welch'
        ])

        assert_cli_success(result)
        assert 'WELCH' in result.output

    @patch('digitalmodel.modules.signal_analysis.cli.SpectralAnalyzer')
    def test_fft_peak_identification(self, mock_analyzer, cli_runner, temp_output_dir):
        """Test FFT with custom number of peaks"""
        input_file = temp_output_dir / "signal.csv"
        pd.DataFrame({'data': np.random.randn(1000)}).to_csv(input_file, index=False)

        mock_analyzer_instance = MagicMock()
        mock_analyzer.return_value = mock_analyzer_instance

        mock_spectrum = {
            'frequencies': np.linspace(0, 500, 1000),
            'amplitudes': np.random.rand(1000)
        }
        mock_analyzer_instance.compute_spectrum.return_value = mock_spectrum

        # 10 peaks
        mock_peaks = [
            {'frequency': float(i*5), 'amplitude': float(1.0 / (i+1))}
            for i in range(10)
        ]
        mock_analyzer_instance.find_peaks.return_value = mock_peaks

        result = cli_runner.invoke(cli, [
            'fft',
            str(input_file),
            '--column', 'data',
            '--sampling-rate', '1000',
            '--n-peaks', '10'
        ])

        assert_cli_success(result)

    @patch('digitalmodel.modules.signal_analysis.cli.SpectralAnalyzer')
    def test_fft_with_json_output(self, mock_analyzer, cli_runner, temp_output_dir):
        """Test FFT with JSON output"""
        input_file = temp_output_dir / "signal.csv"
        pd.DataFrame({'signal': np.random.randn(500)}).to_csv(input_file, index=False)

        mock_analyzer_instance = MagicMock()
        mock_analyzer.return_value = mock_analyzer_instance

        mock_spectrum = {
            'frequencies': np.linspace(0, 500, 1000),
            'amplitudes': np.random.rand(1000)
        }
        mock_analyzer_instance.compute_spectrum.return_value = mock_spectrum

        mock_peaks = [
            {'frequency': 15.5, 'amplitude': 0.6},
            {'frequency': 30.0, 'amplitude': 0.3},
        ]
        mock_analyzer_instance.find_peaks.return_value = mock_peaks

        output_file = temp_output_dir / "fft_results.json"

        result = cli_runner.invoke(cli, [
            'fft',
            str(input_file),
            '--column', 'signal',
            '--sampling-rate', '1000',
            '--output', str(output_file)
        ])

        assert_cli_success(result)

        # Validate JSON
        data = assert_json_output(output_file, [
            'method',
            'sampling_rate',
            'signal_length',
            'peaks',
            'spectrum'
        ])

        assert data['sampling_rate'] == 1000
        assert len(data['peaks']) > 0


class TestPSDCommand:
    """Tests for the 'psd' command"""

    @patch('digitalmodel.modules.signal_analysis.cli.SpectralAnalyzer')
    def test_basic_psd_computation(self, mock_analyzer, cli_runner, temp_output_dir):
        """Test basic power spectral density computation"""
        input_file = temp_output_dir / "signal.csv"
        signal = np.random.randn(1000)
        pd.DataFrame({'signal': signal}).to_csv(input_file, index=False)

        mock_analyzer_instance = MagicMock()
        mock_analyzer.return_value = mock_analyzer_instance

        mock_psd = {
            'frequencies': np.linspace(0, 500, 1000),
            'psd': np.random.rand(1000)
        }
        mock_analyzer_instance.compute_psd.return_value = mock_psd

        result = cli_runner.invoke(cli, [
            'psd',
            str(input_file),
            '--column', 'signal',
            '--sampling-rate', '1000'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'Power Spectral Density Results',
            'Sampling Rate:',
            '1000',
            'Total Power:',
            'Peak Frequency:'
        )

    @patch('digitalmodel.modules.signal_analysis.cli.SpectralAnalyzer')
    def test_psd_with_json_output(self, mock_analyzer, cli_runner, temp_output_dir):
        """Test PSD computation with JSON output"""
        input_file = temp_output_dir / "signal.txt"
        np.savetxt(input_file, np.random.randn(500))

        mock_analyzer_instance = MagicMock()
        mock_analyzer.return_value = mock_analyzer_instance

        mock_psd = {
            'frequencies': np.linspace(0, 250, 500),
            'psd': np.random.rand(500)
        }
        mock_analyzer_instance.compute_psd.return_value = mock_psd

        output_file = temp_output_dir / "psd_results.json"

        result = cli_runner.invoke(cli, [
            'psd',
            str(input_file),
            '--sampling-rate', '500',
            '--output', str(output_file)
        ])

        assert_cli_success(result)

        # Validate JSON
        data = assert_json_output(output_file, [
            'sampling_rate',
            'total_power',
            'peak_frequency',
            'peak_psd',
            'psd'
        ])

        assert data['sampling_rate'] == 500

    def test_psd_missing_sampling_rate(self, cli_runner, temp_output_dir):
        """Test error for missing required sampling rate"""
        input_file = temp_output_dir / "signal.csv"
        pd.DataFrame({'signal': np.random.randn(100)}).to_csv(input_file, index=False)

        result = cli_runner.invoke(cli, [
            'psd',
            str(input_file)
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output or 'required' in result.output.lower()


class TestFilterCommand:
    """Tests for the 'filter' command"""

    @patch('digitalmodel.modules.signal_analysis.cli.TimeSeriesProcessor')
    def test_lowpass_filter(self, mock_processor, cli_runner, temp_output_dir):
        """Test lowpass filter application"""
        # Create input file
        input_file = temp_output_dir / "signal.csv"
        signal = np.random.randn(500)
        pd.DataFrame({'signal': signal}).to_csv(input_file, index=False)

        # Mock processor
        mock_processor_instance = MagicMock()
        mock_processor.return_value = mock_processor_instance

        # Mock filtered signal
        filtered_signal = signal * 0.8  # Simulated filtered signal
        mock_processor_instance.filter_signal.return_value = filtered_signal

        output_file = temp_output_dir / "filtered.csv"

        result = cli_runner.invoke(cli, [
            'filter',
            str(input_file),
            str(output_file),
            '--column', 'signal',
            '--sampling-rate', '1000',
            '--filter-type', 'lowpass',
            '--cutoff', '50'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'Signal Filtering Complete',
            'lowpass',
            '50.0 Hz',
            'Filtered signal saved'
        )

        # Check output file was created
        assert output_file.exists()

    @patch('digitalmodel.modules.signal_analysis.cli.TimeSeriesProcessor')
    def test_filter_output_creation(self, mock_processor, cli_runner, temp_output_dir):
        """Test that filter creates output file"""
        input_file = temp_output_dir / "signal.txt"
        np.savetxt(input_file, np.random.randn(300))

        mock_processor_instance = MagicMock()
        mock_processor.return_value = mock_processor_instance

        filtered = np.random.randn(300)
        mock_processor_instance.filter_signal.return_value = filtered

        output_file = temp_output_dir / "filtered.txt"

        result = cli_runner.invoke(cli, [
            'filter',
            str(input_file),
            str(output_file),
            '--sampling-rate', '500',
            '--filter-type', 'highpass',
            '--cutoff', '20',
            '--order', '6'
        ])

        assert_cli_success(result)
        assert output_file.exists()


class TestCLIHelp:
    """Tests for CLI help and documentation"""

    def test_main_help(self, cli_runner):
        """Test main CLI help message"""
        result = cli_runner.invoke(cli, ['--help'])

        assert_cli_success(result)
        assert_output_contains(result,
            'Signal Analysis Tools',
            'rainflow',
            'fft',
            'psd',
            'filter'
        )

    def test_version(self, cli_runner):
        """Test version output"""
        result = cli_runner.invoke(cli, ['--version'])

        assert_cli_success(result)
        assert 'signal-analysis' in result.output.lower()
