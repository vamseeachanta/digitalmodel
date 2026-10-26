# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Publish a portable copy of the canonical wellbore screening workflow.

Run from the repository root:
    python scripts/capabilities/build_wellbore_workflow_page.py

The explorer links to this Markdown page. Its freshness is checked against
examples/workflows/wellbore-stability/README.md by the explorer tests.
"""

from pathlib import Path

_REPO = Path(__file__).resolve().parents[2]
_SOURCE = _REPO / "examples/workflows/wellbore-stability/README.md"
_OUTPUT = _REPO / "docs/api/structural/wellbore-stability.md"


def main() -> None:
    """Copy the workflow without requiring checkout support for symlinks."""
    workflow = _SOURCE.read_bytes()
    _OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    if _OUTPUT.is_symlink():
        _OUTPUT.unlink()
    _OUTPUT.write_bytes(workflow)


if __name__ == "__main__":
    main()
