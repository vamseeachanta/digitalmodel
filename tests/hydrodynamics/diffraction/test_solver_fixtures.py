"""Verify solver fixture infrastructure works correctly.

Per D-17: pytest fixtures provide .owr/.xlsx reference data so solver-free
tests can consume real results without needing OrcFxAPI installed.

These tests verify the fixture infrastructure, NOT the fixture contents
(which require solver execution from Plan 03).
"""
from pathlib import Path
import pytest


def test_solver_fixtures_dir_exists(solver_fixtures_dir: Path) -> None:
    """The solver fixtures directory exists."""
    assert solver_fixtures_dir.exists(), \
        f"Solver fixtures dir missing: {solver_fixtures_dir}"
    assert solver_fixtures_dir.is_dir()


def test_l00_fixture_skips_when_missing(l00_owr_path: Path) -> None:
    """L00 fixture skips gracefully when .owr file not yet committed."""
    # If we reach this line, the fixture exists (Plan 03 has run)
    assert l00_owr_path.suffix == ".owr"


def test_l01_fixture_skips_when_missing(l01_owr_path: Path) -> None:
    """L01 fixture skips gracefully when .owr file not yet committed."""
    assert l01_owr_path.suffix == ".owr"


@pytest.mark.solver
def test_solver_marker_deselection() -> None:
    """This test is marked @solver and should be skipped with -m 'not solver'."""
    # This test body doesn't matter -- the marker is what we're testing.
    # When running with -m "not solver", this test should not appear.
    pass
