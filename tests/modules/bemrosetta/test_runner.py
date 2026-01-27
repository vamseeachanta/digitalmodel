"""
Tests for BEMRosetta runner module.

TDD: Tests for BEMRosetta executable detection and execution wrapper.
"""

import subprocess
from pathlib import Path
from unittest.mock import patch, MagicMock

import pytest


class TestBEMRosettaRunner:
    """Tests for BEMRosettaRunner class."""

    def test_initialization_without_path(self):
        """Test runner initializes without explicit path."""
        from digitalmodel.modules.bemrosetta.core.runner import BEMRosettaRunner

        # Should not raise, even if executable not found
        runner = BEMRosettaRunner()
        assert runner is not None

    def test_initialization_with_explicit_path(self):
        """Test runner initializes with explicit executable path."""
        from digitalmodel.modules.bemrosetta.core.runner import BEMRosettaRunner

        fake_path = Path("D:/software/BEMRosetta/BEMRosetta_cl.exe")
        runner = BEMRosettaRunner(executable_path=fake_path)

        # The path should be stored (even if file doesn't exist)
        assert runner.executable_path == fake_path

    def test_is_available_returns_bool(self):
        """Test is_available returns boolean."""
        from digitalmodel.modules.bemrosetta.core.runner import BEMRosettaRunner

        runner = BEMRosettaRunner()
        result = runner.is_available()

        assert isinstance(result, bool)

    def test_is_available_false_when_no_executable(self):
        """Test is_available returns False when no executable found."""
        from digitalmodel.modules.bemrosetta.core.runner import BEMRosettaRunner

        # Use a path that definitely doesn't exist
        runner = BEMRosettaRunner(executable_path=Path("/nonexistent/path/bemrosetta.exe"))

        assert runner.is_available() is False

    @patch("subprocess.run")
    def test_is_available_true_with_valid_executable(self, mock_run):
        """Test is_available returns True with valid executable path."""
        from digitalmodel.modules.bemrosetta.core.runner import BEMRosettaRunner

        # Create a mock for an existing file
        with patch.object(Path, "exists", return_value=True):
            runner = BEMRosettaRunner.__new__(BEMRosettaRunner)
            runner._executable_path = Path("/valid/path/BEMRosetta_cl.exe")
            runner._version = None
            runner._searched_paths = []

            assert runner.is_available() is True

    def test_executable_path_property(self):
        """Test executable_path property returns correct value."""
        from digitalmodel.modules.bemrosetta.core.runner import BEMRosettaRunner

        runner = BEMRosettaRunner()

        # Property should return Path or None
        result = runner.executable_path
        assert result is None or isinstance(result, Path)

    def test_version_property(self):
        """Test version property returns string or None."""
        from digitalmodel.modules.bemrosetta.core.runner import BEMRosettaRunner

        runner = BEMRosettaRunner()

        # Property should return string or None
        result = runner.version
        assert result is None or isinstance(result, str)

    def test_get_info_returns_dict(self):
        """Test get_info returns dictionary with expected keys."""
        from digitalmodel.modules.bemrosetta.core.runner import BEMRosettaRunner

        runner = BEMRosettaRunner()
        info = runner.get_info()

        assert isinstance(info, dict)
        assert "available" in info
        assert "executable_path" in info
        assert "version" in info
        assert "searched_paths" in info

    @patch("subprocess.run")
    def test_run_raises_when_not_available(self, mock_run):
        """Test run raises ExecutableNotFoundError when not available."""
        from digitalmodel.modules.bemrosetta.core.runner import BEMRosettaRunner
        from digitalmodel.modules.bemrosetta.core.exceptions import ExecutableNotFoundError

        runner = BEMRosettaRunner(executable_path=Path("/nonexistent/path"))

        with pytest.raises(ExecutableNotFoundError):
            runner.run(["--version"])

    @patch("subprocess.run")
    def test_run_executes_command(self, mock_run):
        """Test run executes command when executable available."""
        from digitalmodel.modules.bemrosetta.core.runner import BEMRosettaRunner

        mock_run.return_value = subprocess.CompletedProcess(
            args=["bemrosetta", "--version"],
            returncode=0,
            stdout="BEMRosetta v1.0",
            stderr=""
        )

        # Mock an available executable
        with patch.object(Path, "exists", return_value=True):
            runner = BEMRosettaRunner.__new__(BEMRosettaRunner)
            runner._executable_path = Path("/valid/BEMRosetta_cl.exe")
            runner._version = None
            runner._searched_paths = []

            result = runner.run(["--version"])

            assert result.returncode == 0
            mock_run.assert_called_once()

    @patch("subprocess.run")
    def test_run_passes_arguments(self, mock_run):
        """Test run passes arguments correctly to subprocess."""
        from digitalmodel.modules.bemrosetta.core.runner import BEMRosettaRunner

        mock_run.return_value = subprocess.CompletedProcess(
            args=[], returncode=0, stdout="", stderr=""
        )

        with patch.object(Path, "exists", return_value=True):
            runner = BEMRosettaRunner.__new__(BEMRosettaRunner)
            runner._executable_path = Path("/valid/BEMRosetta_cl.exe")
            runner._version = None
            runner._searched_paths = []

            runner.run(["-i", "input.lis", "-o", "output.yml"])

            call_args = mock_run.call_args
            cmd = call_args[0][0]
            assert "-i" in cmd
            assert "input.lis" in cmd
            assert "-o" in cmd
            assert "output.yml" in cmd

    @patch("subprocess.run")
    def test_run_respects_timeout(self, mock_run):
        """Test run passes timeout to subprocess."""
        from digitalmodel.modules.bemrosetta.core.runner import BEMRosettaRunner

        mock_run.return_value = subprocess.CompletedProcess(
            args=[], returncode=0, stdout="", stderr=""
        )

        with patch.object(Path, "exists", return_value=True):
            runner = BEMRosettaRunner.__new__(BEMRosettaRunner)
            runner._executable_path = Path("/valid/BEMRosetta_cl.exe")
            runner._version = None
            runner._searched_paths = []

            runner.run(["--version"], timeout=600)

            call_kwargs = mock_run.call_args[1]
            assert call_kwargs["timeout"] == 600


class TestGetRunner:
    """Tests for get_runner singleton function."""

    def test_returns_bemrosetta_runner(self):
        """Test get_runner returns BEMRosettaRunner instance."""
        from digitalmodel.modules.bemrosetta.core.runner import (
            get_runner,
            BEMRosettaRunner,
        )

        # Reset singleton for testing
        import digitalmodel.modules.bemrosetta.core.runner as runner_module
        runner_module._runner_instance = None

        runner = get_runner()
        assert isinstance(runner, BEMRosettaRunner)

    def test_returns_same_instance(self):
        """Test get_runner returns same instance on multiple calls (singleton)."""
        from digitalmodel.modules.bemrosetta.core.runner import get_runner

        # Reset singleton for testing
        import digitalmodel.modules.bemrosetta.core.runner as runner_module
        runner_module._runner_instance = None

        runner1 = get_runner()
        runner2 = get_runner()

        assert runner1 is runner2


class TestIsBemRosettaAvailable:
    """Tests for is_bemrosetta_available convenience function."""

    def test_returns_bool(self):
        """Test is_bemrosetta_available returns boolean."""
        from digitalmodel.modules.bemrosetta.core.runner import is_bemrosetta_available

        result = is_bemrosetta_available()
        assert isinstance(result, bool)

    def test_calls_runner_is_available(self):
        """Test is_bemrosetta_available uses runner's is_available."""
        from digitalmodel.modules.bemrosetta.core import runner as runner_module

        # Reset singleton
        runner_module._runner_instance = None

        # Create mock runner
        mock_runner = MagicMock()
        mock_runner.is_available.return_value = True

        with patch.object(runner_module, "get_runner", return_value=mock_runner):
            result = runner_module.is_bemrosetta_available()

            assert result is True
            mock_runner.is_available.assert_called_once()


class TestEnvironmentVariableSupport:
    """Tests for BEMROSETTA_PATH environment variable support."""

    @patch.dict("os.environ", {"BEMROSETTA_PATH": "/custom/path/BEMRosetta_cl.exe"})
    @patch.object(Path, "exists", return_value=True)
    def test_uses_environment_variable(self, mock_exists):
        """Test runner uses BEMROSETTA_PATH environment variable."""
        from digitalmodel.modules.bemrosetta.core.runner import BEMRosettaRunner

        # Reset singleton
        import digitalmodel.modules.bemrosetta.core.runner as runner_module
        runner_module._runner_instance = None

        # Create fresh runner to pick up env var
        with patch("subprocess.run"):  # Prevent version detection
            runner = BEMRosettaRunner()

        # If env var is set and file exists, should use that path
        if runner.executable_path:
            assert "custom" in str(runner.executable_path) or runner._searched_paths[0].startswith("$BEMROSETTA_PATH")


class TestPathSearchOrder:
    """Tests for executable search order."""

    def test_searched_paths_recorded(self):
        """Test that searched paths are recorded for debugging."""
        from digitalmodel.modules.bemrosetta.core.runner import BEMRosettaRunner

        # Reset singleton
        import digitalmodel.modules.bemrosetta.core.runner as runner_module
        runner_module._runner_instance = None

        runner = BEMRosettaRunner()
        info = runner.get_info()

        # Should have searched at least one location
        assert "searched_paths" in info
        assert isinstance(info["searched_paths"], list)
