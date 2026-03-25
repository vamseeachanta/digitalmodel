"""
ABOUTME: Pytest fixtures and utilities for CLI integration tests
ABOUTME: Provides Click test runners, temporary directories, and mock data
"""

import pytest
import json
import tempfile
from pathlib import Path
from click.testing import CliRunner


@pytest.fixture
def cli_runner():
    """
    Fixture providing Click CLI test runner.

    Usage:
        def test_my_command(cli_runner):
            result = cli_runner.invoke(my_cli, ['--option', 'value'])
            assert result.exit_code == 0
    """
    return CliRunner()


@pytest.fixture
def temp_output_dir(tmp_path):
    """
    Fixture providing temporary output directory for CLI test results.

    Returns:
        Path: Temporary directory path that is automatically cleaned up
    """
    output_dir = tmp_path / "cli_output"
    output_dir.mkdir(exist_ok=True)
    return output_dir


@pytest.fixture
def temp_input_file(tmp_path):
    """
    Fixture providing temporary input file path.

    Returns:
        Path: Temporary file path for test input data
    """
    return tmp_path / "input.json"


@pytest.fixture
def sample_stress_state():
    """Sample stress state for structural analysis tests."""
    return {
        'sigma_x': 150.0,
        'sigma_y': 100.0,
        'sigma_z': 50.0,
        'tau_xy': 30.0,
        'tau_xz': 20.0,
        'tau_yz': 10.0
    }


@pytest.fixture
def sample_plate_geometry():
    """Sample plate geometry for buckling tests."""
    return {
        'length': 1000.0,  # mm
        'width': 500.0,    # mm
        'thickness': 10.0  # mm
    }


@pytest.fixture
def sample_material():
    """Sample material properties."""
    return {
        'name': 'S355',
        'yield_strength': 355.0,  # MPa
        'elastic_modulus': 210000.0,  # MPa
        'poisson_ratio': 0.3
    }


def assert_cli_success(result):
    """
    Assert that CLI command executed successfully.

    Args:
        result: CliRunner result object

    Raises:
        AssertionError: If command failed
    """
    if result.exit_code != 0:
        raise AssertionError(
            f"CLI command failed with exit code {result.exit_code}\n"
            f"Output: {result.output}\n"
            f"Exception: {result.exception}"
        )
    assert result.exit_code == 0


def assert_cli_failure(result, expected_exit_code=None):
    """
    Assert that CLI command failed as expected.

    Args:
        result: CliRunner result object
        expected_exit_code: Expected non-zero exit code (optional)

    Raises:
        AssertionError: If command succeeded unexpectedly
    """
    assert result.exit_code != 0, "CLI command should have failed but succeeded"

    if expected_exit_code is not None:
        assert result.exit_code == expected_exit_code, (
            f"Expected exit code {expected_exit_code}, got {result.exit_code}"
        )


def assert_json_output(output_file, expected_keys):
    """
    Assert that JSON output file exists and contains expected keys.

    Args:
        output_file: Path to output JSON file
        expected_keys: List of expected top-level keys

    Raises:
        AssertionError: If file missing or keys not found
    """
    assert output_file.exists(), f"Output file not created: {output_file}"

    with open(output_file) as f:
        data = json.load(f)

    for key in expected_keys:
        assert key in data, f"Expected key '{key}' not found in output"

    return data


def assert_output_contains(result, *strings):
    """
    Assert that CLI output contains all specified strings.

    Args:
        result: CliRunner result object
        *strings: Strings that should appear in output

    Raises:
        AssertionError: If any string not found
    """
    for string in strings:
        assert string in result.output, (
            f"Expected string '{string}' not found in output:\n{result.output}"
        )
