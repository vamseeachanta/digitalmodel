"""Verify solver fixture infrastructure works correctly.

Per D-17: pytest fixtures provide .owr/.xlsx reference data so solver-free
tests can consume real results without needing OrcFxAPI installed.

These tests verify the fixture infrastructure, NOT the fixture contents
(which require solver execution from Plan 03).
"""
from pathlib import Path
import pytest


REPO_ROOT = Path(__file__).parent.parent.parent.parent


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


def test_l00_xlsx_fixture_skips_when_missing(l00_xlsx_path: Path) -> None:
    """L00 Excel fixture skips gracefully when .xlsx file not yet committed."""
    assert l00_xlsx_path.suffix == ".xlsx"


def test_l01_xlsx_fixture_skips_when_missing(l01_xlsx_path: Path) -> None:
    """L01 Excel fixture skips gracefully when .xlsx file not yet committed."""
    assert l01_xlsx_path.suffix == ".xlsx"


def test_solver_fixture_owr_paths_are_not_gitignored() -> None:
    """Solver fixture .owr files must be committable as permanent references."""
    gitignore = (REPO_ROOT / ".gitignore").read_text(encoding="utf-8")
    assert "!tests/fixtures/solver/*.owr" in gitignore


def test_solver_fixture_binary_types_marked_in_gitattributes() -> None:
    """Binary solver fixture types remain marked binary for git handling."""
    gitattributes = (REPO_ROOT / ".gitattributes").read_text(encoding="utf-8")
    assert "*.owr binary" in gitattributes
    assert "*.xlsx binary" in gitattributes


@pytest.mark.solver
def test_solver_marker_deselection() -> None:
    """This test is marked @solver and should be skipped with -m 'not solver'."""
    # This test body doesn't matter -- the marker is what we're testing.
    # When running with -m "not solver", this test should not appear.
    pass
