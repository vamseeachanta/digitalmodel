"""
ABOUTME: Shared fixtures for Blender automation tests
Provides mock subprocess helpers and pytest markers for Blender availability.
"""

import subprocess
from unittest.mock import MagicMock, patch

import pytest


# ---------------------------------------------------------------------------
# Subprocess mock helpers
# ---------------------------------------------------------------------------

def mock_blender_run(args, **kwargs):
    """Simulate a successful `blender` subprocess call."""
    cmd = args if isinstance(args, list) else [args]
    stdout = ""
    if "--version" in cmd:
        stdout = "Blender 4.3.0\n  build date: 2025-01-01"
    elif "--python-expr" in cmd:
        idx = cmd.index("--python-expr") + 1
        expr = cmd[idx] if idx < len(cmd) else ""
        for line in expr.splitlines():
            stripped = line.strip()
            if stripped.startswith("print("):
                content = stripped[len("print("):-1].strip("'\"")
                stdout += content + "\n"
    result = MagicMock()
    result.stdout = stdout
    result.stderr = ""
    result.returncode = 0
    return result


def mock_blender_run_error(args, **kwargs):
    """Simulate a failing `blender` subprocess call."""
    cmd = args if isinstance(args, list) else [args]
    if "--version" in cmd:
        return mock_blender_run(args, **kwargs)
    raise subprocess.CalledProcessError(
        returncode=1, cmd=cmd, output="", stderr="Error in script"
    )


# ---------------------------------------------------------------------------
# Blender availability detection (module-level, runs once)
# ---------------------------------------------------------------------------

_real_blender_available = False
try:
    subprocess.run(
        ["blender", "--version"],
        capture_output=True, text=True, check=True, timeout=10,
    )
    _real_blender_available = True
except Exception:
    pass

requires_blender = pytest.mark.skipif(
    not _real_blender_available,
    reason="Blender not installed on this machine",
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def mock_subprocess():
    """Patch subprocess.run with the mock Blender simulator."""
    with patch("subprocess.run", side_effect=mock_blender_run):
        yield
