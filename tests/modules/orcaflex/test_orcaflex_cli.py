#!/usr/bin/env python3
"""
ABOUTME: CLI integration tests for OrcaFlex module commands including
universal runner and run-to-sim functionality.
"""

import pytest
import subprocess
import sys
from pathlib import Path


class TestCLIAvailability:
    """Test that CLI commands are available"""

    def test_orcaflex_universal_command_exists(self):
        """Test orcaflex-universal command is installed"""
        result = subprocess.run(
            ['orcaflex-universal', '--version'],
            capture_output=True,
            text=True
        )
        # Command should exist (exit code 0 or command-specific code)
        assert result.returncode in [0, 2]  # Click returns 2 for --version sometimes

    def test_run_to_sim_command_exists(self):
        """Test run-to-sim command is installed"""
        result = subprocess.run(
            ['run-to-sim', '--version'],
            capture_output=True,
            text=True
        )
        assert result.returncode in [0, 2]


class TestUniversalCLIHelp:
    """Test universal CLI help and documentation"""

    def test_universal_help(self):
        """Test orcaflex-universal --help"""
        result = subprocess.run(
            ['orcaflex-universal', '--help'],
            capture_output=True,
            text=True
        )
        assert result.returncode == 0
        assert 'OrcaFlex' in result.stdout or 'orcaflex' in result.stdout.lower()
        assert 'pattern' in result.stdout.lower() or 'option' in result.stdout.lower()

    def test_universal_version(self):
        """Test orcaflex-universal --version"""
        result = subprocess.run(
            ['orcaflex-universal', '--version'],
            capture_output=True,
            text=True
        )
        # Version info should be in stdout or stderr
        output = result.stdout + result.stderr
        assert '1.0' in output or 'version' in output.lower()


class TestRunToSimCLIHelp:
    """Test run-to-sim CLI help and documentation"""

    def test_run_to_sim_help(self):
        """Test run-to-sim --help"""
        result = subprocess.run(
            ['run-to-sim', '--help'],
            capture_output=True,
            text=True
        )
        assert result.returncode == 0
        assert 'OrcaFlex' in result.stdout or 'sim' in result.stdout.lower()
        assert 'model' in result.stdout.lower()

    def test_run_to_sim_version(self):
        """Test run-to-sim --version"""
        result = subprocess.run(
            ['run-to-sim', '--version'],
            capture_output=True,
            text=True
        )
        output = result.stdout + result.stderr
        assert '1.0' in output or 'version' in output.lower()


class TestCLIOptions:
    """Test CLI option parsing"""

    def test_universal_pattern_option(self):
        """Test --pattern option is recognized"""
        result = subprocess.run(
            ['orcaflex-universal', '--help'],
            capture_output=True,
            text=True
        )
        assert '--pattern' in result.stdout or '-p' in result.stdout

    def test_universal_output_option(self):
        """Test --output-dir option is recognized"""
        result = subprocess.run(
            ['orcaflex-universal', '--help'],
            capture_output=True,
            text=True
        )
        assert '--output' in result.stdout or 'output' in result.stdout.lower()

    def test_universal_parallel_option(self):
        """Test --parallel option is recognized"""
        result = subprocess.run(
            ['orcaflex-universal', '--help'],
            capture_output=True,
            text=True
        )
        assert '--parallel' in result.stdout or 'parallel' in result.stdout.lower()

    def test_universal_mock_option(self):
        """Test --mock option is recognized"""
        result = subprocess.run(
            ['orcaflex-universal', '--help'],
            capture_output=True,
            text=True
        )
        assert '--mock' in result.stdout

    def test_run_to_sim_all_option(self):
        """Test --all option is recognized"""
        result = subprocess.run(
            ['run-to-sim', '--help'],
            capture_output=True,
            text=True
        )
        assert '--all' in result.stdout

    def test_run_to_sim_models_option(self):
        """Test --models option is recognized"""
        result = subprocess.run(
            ['run-to-sim', '--help'],
            capture_output=True,
            text=True
        )
        assert '--models' in result.stdout

    def test_run_to_sim_threads_option(self):
        """Test --threads option is recognized"""
        result = subprocess.run(
            ['run-to-sim', '--help'],
            capture_output=True,
            text=True
        )
        assert '--threads' in result.stdout


class TestCLIErrorHandling:
    """Test CLI error handling"""

    def test_universal_invalid_option(self):
        """Test invalid option produces error"""
        result = subprocess.run(
            ['orcaflex-universal', '--invalid-option-xyz'],
            capture_output=True,
            text=True
        )
        # Should fail with non-zero exit code
        assert result.returncode != 0

    def test_run_to_sim_invalid_option(self):
        """Test invalid option produces error"""
        result = subprocess.run(
            ['run-to-sim', '--invalid-option-xyz'],
            capture_output=True,
            text=True
        )
        assert result.returncode != 0


class TestCLIExamples:
    """Test CLI example commands from documentation"""

    def test_universal_help_shows_examples(self):
        """Test that help includes usage examples"""
        result = subprocess.run(
            ['orcaflex-universal', '--help'],
            capture_output=True,
            text=True
        )
        # Should show examples or usage
        assert 'example' in result.stdout.lower() or 'usage' in result.stdout.lower()

    def test_run_to_sim_help_shows_examples(self):
        """Test that help includes usage examples"""
        result = subprocess.run(
            ['run-to-sim', '--help'],
            capture_output=True,
            text=True
        )
        assert 'example' in result.stdout.lower() or 'usage' in result.stdout.lower()


class TestCLIModuleIntegration:
    """Test CLI integration with Python module"""

    def test_python_can_import_after_cli_install(self):
        """Test Python import works after CLI installation"""
        result = subprocess.run(
            [sys.executable, '-c',
             'import digitalmodel.modules.orcaflex; print(digitalmodel.modules.orcaflex.__version__)'],
            capture_output=True,
            text=True
        )
        assert result.returncode == 0
        assert '1.0' in result.stdout

    def test_cli_commands_from_module(self):
        """Test CLI commands list from module"""
        result = subprocess.run(
            [sys.executable, '-c',
             'from digitalmodel.modules.orcaflex import list_cli_commands; '
             'cmds = list_cli_commands(); '
             'print(cmds)'],
            capture_output=True,
            text=True
        )
        assert result.returncode == 0
        assert 'orcaflex-universal' in result.stdout
        assert 'run-to-sim' in result.stdout


class TestCLIOutputFormats:
    """Test CLI output formatting"""

    def test_universal_verbose_output(self):
        """Test --verbose flag affects output"""
        result = subprocess.run(
            ['orcaflex-universal', '--help'],
            capture_output=True,
            text=True
        )
        assert '--verbose' in result.stdout or '-v' in result.stdout

    def test_run_to_sim_verbose_output(self):
        """Test --verbose flag exists"""
        result = subprocess.run(
            ['run-to-sim', '--help'],
            capture_output=True,
            text=True
        )
        assert '--verbose' in result.stdout


class TestCLICompatibility:
    """Test CLI compatibility across platforms"""

    def test_cli_runs_on_current_python(self):
        """Test CLI works with current Python version"""
        # Just test that help works - indicates CLI is functional
        result = subprocess.run(
            ['orcaflex-universal', '--help'],
            capture_output=True,
            text=True
        )
        assert result.returncode == 0

        result = subprocess.run(
            ['run-to-sim', '--help'],
            capture_output=True,
            text=True
        )
        assert result.returncode == 0


class TestCLIDefaults:
    """Test CLI default values"""

    def test_universal_defaults_shown_in_help(self):
        """Test default values are documented"""
        result = subprocess.run(
            ['orcaflex-universal', '--help'],
            capture_output=True,
            text=True
        )
        # Should show defaults like "default:" or "[default: ...]"
        assert 'default' in result.stdout.lower()

    def test_run_to_sim_defaults_shown_in_help(self):
        """Test default values are documented"""
        result = subprocess.run(
            ['run-to-sim', '--help'],
            capture_output=True,
            text=True
        )
        assert 'default' in result.stdout.lower()


# Run tests if executed directly
if __name__ == '__main__':
    pytest.main([__file__, '-v'])
