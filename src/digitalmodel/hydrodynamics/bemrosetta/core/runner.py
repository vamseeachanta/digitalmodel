"""
BEMRosetta Executable Runner

Handles detection and execution of the BEMRosetta command-line tool.
Provides graceful degradation when executable is not available.
"""

import logging
import os
import shutil
import subprocess
from pathlib import Path
from typing import Any

from .exceptions import ExecutableNotFoundError

logger = logging.getLogger(__name__)

# Default executable names by platform
EXECUTABLE_NAMES = ["BEMRosetta_cl.exe", "BEMRosetta_cl", "bemrosetta_cl"]

# Environment variable for custom path
ENV_VAR_NAME = "BEMROSETTA_PATH"

# Cached runner instance
_runner_instance: "BEMRosettaRunner | None" = None


def is_bemrosetta_available() -> bool:
    """Check if BEMRosetta executable is available.

    Searches for the executable in:
    1. BEMROSETTA_PATH environment variable
    2. System PATH
    3. Common installation directories

    Returns:
        True if executable is found and accessible.
    """
    try:
        runner = get_runner()
        return runner.is_available()
    except ExecutableNotFoundError:
        return False


def get_runner() -> "BEMRosettaRunner":
    """Get or create the BEMRosetta runner instance.

    Returns:
        BEMRosettaRunner instance.

    Raises:
        ExecutableNotFoundError: If executable cannot be found.
    """
    global _runner_instance

    if _runner_instance is None:
        _runner_instance = BEMRosettaRunner()

    return _runner_instance


class BEMRosettaRunner:
    """Manages BEMRosetta executable detection and execution.

    Attributes:
        executable_path: Path to the BEMRosetta executable (None if not found).
        version: Version string if detected.
    """

    def __init__(self, executable_path: Path | str | None = None):
        """Initialize runner, optionally with explicit path.

        Args:
            executable_path: Optional explicit path to executable.
        """
        self._executable_path: Path | None = None
        self._version: str | None = None
        self._searched_paths: list[str] = []

        if executable_path:
            self._set_executable(Path(executable_path))
        else:
            self._find_executable()

    @property
    def executable_path(self) -> Path | None:
        """Path to BEMRosetta executable, or None if not found."""
        return self._executable_path

    @property
    def version(self) -> str | None:
        """BEMRosetta version string, or None if not detected."""
        return self._version

    def is_available(self) -> bool:
        """Check if executable is available and functional.

        Returns:
            True if executable exists and can be executed.
        """
        if self._executable_path is None:
            return False

        if not self._executable_path.exists():
            return False

        return True

    def run(
        self,
        args: list[str],
        cwd: Path | str | None = None,
        timeout: int = 300,
    ) -> subprocess.CompletedProcess:
        """Execute BEMRosetta with given arguments.

        Args:
            args: Command-line arguments.
            cwd: Working directory for execution.
            timeout: Timeout in seconds.

        Returns:
            CompletedProcess with stdout, stderr, and return code.

        Raises:
            ExecutableNotFoundError: If executable is not available.
            subprocess.TimeoutExpired: If execution times out.
            subprocess.SubprocessError: If execution fails.
        """
        if not self.is_available():
            raise ExecutableNotFoundError(
                "BEMRosetta executable not available",
                searched_paths=self._searched_paths,
            )

        cmd = [str(self._executable_path)] + args
        logger.debug(f"Running BEMRosetta: {' '.join(cmd)}")

        result = subprocess.run(
            cmd,
            cwd=cwd,
            capture_output=True,
            text=True,
            timeout=timeout,
        )

        if result.returncode != 0:
            logger.warning(
                f"BEMRosetta returned non-zero: {result.returncode}\n"
                f"stderr: {result.stderr}"
            )

        return result

    def get_info(self) -> dict[str, Any]:
        """Get information about the BEMRosetta installation.

        Returns:
            Dict with executable path, version, and availability.
        """
        return {
            "available": self.is_available(),
            "executable_path": str(self._executable_path) if self._executable_path else None,
            "version": self._version,
            "searched_paths": self._searched_paths,
        }

    def _find_executable(self) -> None:
        """Search for BEMRosetta executable in standard locations."""
        # Check environment variable first
        env_path = os.environ.get(ENV_VAR_NAME)
        if env_path:
            path = Path(env_path)
            self._searched_paths.append(f"${ENV_VAR_NAME}={env_path}")
            if path.exists():
                self._set_executable(path)
                return

        # Check system PATH
        for name in EXECUTABLE_NAMES:
            found = shutil.which(name)
            self._searched_paths.append(f"PATH:{name}")
            if found:
                self._set_executable(Path(found))
                return

        # Check common installation directories
        common_paths = self._get_common_paths()
        for path in common_paths:
            self._searched_paths.append(str(path))
            if path.exists():
                self._set_executable(path)
                return

        logger.debug(
            f"BEMRosetta executable not found. Searched: {self._searched_paths}"
        )

    def _get_common_paths(self) -> list[Path]:
        """Get list of common installation paths to check.

        Returns:
            List of paths to check for executable.
        """
        paths = []

        # Windows common paths
        if os.name == "nt":
            program_files = os.environ.get("ProgramFiles", "C:\\Program Files")
            program_files_x86 = os.environ.get(
                "ProgramFiles(x86)", "C:\\Program Files (x86)"
            )
            local_app_data = os.environ.get(
                "LOCALAPPDATA", os.path.expanduser("~\\AppData\\Local")
            )

            paths.extend([
                Path(program_files) / "BEMRosetta" / "BEMRosetta_cl.exe",
                Path(program_files_x86) / "BEMRosetta" / "BEMRosetta_cl.exe",
                Path(local_app_data) / "BEMRosetta" / "BEMRosetta_cl.exe",
            ])

        # Linux/macOS common paths
        else:
            paths.extend([
                Path("/usr/local/bin/BEMRosetta_cl"),
                Path("/opt/BEMRosetta/BEMRosetta_cl"),
                Path.home() / ".local" / "bin" / "BEMRosetta_cl",
            ])

        return paths

    def _set_executable(self, path: Path) -> None:
        """Set executable path and detect version.

        Args:
            path: Path to executable.
        """
        self._executable_path = path
        logger.info(f"Found BEMRosetta at: {path}")
        self._detect_version()

    def _detect_version(self) -> None:
        """Attempt to detect BEMRosetta version."""
        if not self._executable_path or not self._executable_path.exists():
            return

        try:
            result = subprocess.run(
                [str(self._executable_path), "--version"],
                capture_output=True,
                text=True,
                timeout=10,
            )
            if result.returncode == 0 and result.stdout:
                self._version = result.stdout.strip().split("\n")[0]
                logger.debug(f"BEMRosetta version: {self._version}")
        except (subprocess.SubprocessError, OSError) as e:
            logger.debug(f"Could not detect BEMRosetta version: {e}")
