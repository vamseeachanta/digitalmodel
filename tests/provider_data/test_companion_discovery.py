"""An explicitly configured companion library cannot silently skip its tests."""

import os
from pathlib import Path
import subprocess
import sys


def test_explicit_companion_root_requires_pilot_data(tmp_path):
    env = os.environ.copy()
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(name, None)
    env["WORKSPACE_HUB_ROOT"] = str(tmp_path / "missing-companion")
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "tests/provider_data/test_irs_15k_yaml.py",
            "-m",
            "not solver",
            "-q",
        ],
        cwd=Path(__file__).resolve().parents[2],
        env=env,
        capture_output=True,
        text=True,
        timeout=30,
    )
    output = result.stdout + result.stderr
    assert result.returncode == 1, output
    assert "WORKSPACE_HUB_ROOT is set but companion IRS YAML is missing" in output
    assert "5 errors" in output
    assert "skipped" not in output
