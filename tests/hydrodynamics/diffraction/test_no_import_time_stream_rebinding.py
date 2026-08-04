"""Importing a benchmark script must not rebind the process's standard streams.

Regression for digitalmodel#1633.

`scripts/benchmark/validate_owd_vs_spec.py` fixed Windows console encoding by
rebinding at module scope::

    if sys.platform == "win32":
        sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")
        sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding="utf-8")

Rebinding creates a *new* wrapper over the same underlying buffer. When that
wrapper is garbage-collected it closes the buffer, taking the real stream with
it -- so everything later in the process writes to a closed file. Under pytest
this surfaces as ``ValueError: I/O operation on closed file`` / ``lost
sys.stderr`` during capture teardown, and the whole suite cannot complete.
`test_validate_owd_vs_spec_semantics.py` calls `_load_validate_module()` in
every test, so the module is executed repeatedly and the damage compounds.

The fix is in-place reconfiguration, which mutates the existing objects and
closes nothing::

    sys.stdout.reconfigure(encoding="utf-8")

The check runs in a subprocess with ``sys.platform`` forced to ``"win32"`` so it
exercises the guarded branch on any host -- otherwise it would be vacuously
green on Linux, where the branch never executes.
"""

from __future__ import annotations

import os
import subprocess
import sys
import textwrap
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
SCRIPTS = [
    REPO_ROOT / "scripts" / "benchmark" / "validate_owd_vs_spec.py",
]

_DRIVER = textwrap.dedent(
    """
    import importlib.util
    import sys

    # Force the win32-guarded branch regardless of the host platform.
    sys.platform = "win32"

    before = (id(sys.stdout), id(sys.stderr))

    spec = importlib.util.spec_from_file_location("_probe_target", sys.argv[1])
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)

    after = (id(sys.stdout), id(sys.stderr))

    # Write via the ORIGINAL stream object: if the module rebound and the old
    # buffer was closed, using sys.stdout here could mask the failure.
    sys.__stdout__.write("SAME" if before == after else "REBOUND")
    """
)


@pytest.mark.parametrize("script", SCRIPTS, ids=lambda p: p.name)
def test_import_does_not_rebind_std_streams(script: Path, tmp_path: Path) -> None:
    assert script.exists(), f"target script missing: {script}"

    driver = tmp_path / "probe.py"
    driver.write_text(_DRIVER, encoding="utf-8")

    env = os.environ.copy()
    env["PYTHONPATH"] = os.pathsep.join(
        [str(REPO_ROOT / "src"), env.get("PYTHONPATH", "")]
    ).rstrip(os.pathsep)

    result = subprocess.run(
        [sys.executable, str(driver), str(script)],
        capture_output=True,
        text=True,
        timeout=120,
        cwd=str(REPO_ROOT),
        env=env,
    )

    assert result.returncode == 0, (
        f"probe failed (rc={result.returncode})\n"
        f"stdout: {result.stdout!r}\nstderr: {result.stderr!r}"
    )
    assert "REBOUND" not in result.stdout, (
        f"{script.name} rebinds sys.stdout/sys.stderr at import time. "
        "Use sys.stdout.reconfigure(encoding='utf-8') instead of assigning a "
        "new TextIOWrapper -- rebinding closes the underlying buffer when the "
        "wrapper is collected and destroys the stream for the whole process."
    )
    assert "SAME" in result.stdout, f"probe produced no verdict: {result.stdout!r}"
