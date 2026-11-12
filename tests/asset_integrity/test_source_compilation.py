# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Keep asset-integrity sources importable under strict warning policies."""

import os
from pathlib import Path
import subprocess
import sys


def test_asset_integrity_compiles_without_syntax_warnings(tmp_path):
    package = (
        Path(__file__).resolve().parents[2]
        / "src"
        / "digitalmodel"
        / "asset_integrity"
    )
    result = subprocess.run(
        [
            sys.executable,
            "-W",
            "error::SyntaxWarning",
            "-W",
            "error::DeprecationWarning",
            "-m",
            "compileall",
            "-q",
            "-f",
            str(package),
        ],
        env={**os.environ, "PYTHONPYCACHEPREFIX": str(tmp_path / "pycache")},
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stdout + result.stderr
