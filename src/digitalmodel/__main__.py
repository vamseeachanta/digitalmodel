"""Python package for digital model

Usage:
------

Run an analysis from an input file (the engine contract)::

    python -m digitalmodel <input.yml>

or invoke any of the package's console-script CLIs through the module
entry point (#713)::

    python -m digitalmodel diffraction run-orcawave spec.yml --dry-run
    python -m digitalmodel aqwa --help

Contact:
--------

More information is available at:

- https://pypi.org/project/digitalmodel/
- https://github.com/vamseeachanta/digitalmodel


Version:
--------

- digitalmodel v0.0.9
"""

import sys
from importlib import metadata
from pathlib import Path

from digitalmodel.engine import engine

# Entry points that would re-enter this module rather than dispatch to a CLI.
_SELF_NAMES = {"digital_model", "digitalmodel"}


def _resolve_cli(name: str):
    """Return this distribution's console-script entry point called ``name``.

    Looks up ``[project.scripts]`` via package metadata so the routing table
    cannot drift from pyproject.toml. Returns None for unknown names, for
    this module's own entry point, or when distribution metadata is
    unavailable — callers then fall through to the engine contract.
    """
    if name in _SELF_NAMES:
        return None
    try:
        entry_points = metadata.distribution("digitalmodel").entry_points
    except metadata.PackageNotFoundError:
        return None
    for entry_point in entry_points:
        if entry_point.group == "console_scripts" and entry_point.name == name:
            return entry_point
    return None


def main():
    # An existing file always wins: `python -m digitalmodel <input.yml>` is
    # the engine contract and must stay unchanged (durable-workflow callers).
    if len(sys.argv) > 1 and not Path(sys.argv[1]).exists():
        entry_point = _resolve_cli(sys.argv[1])
        if entry_point is not None:
            sys.argv = sys.argv[1:]
            sys.exit(entry_point.load()())
    engine()


if __name__ == "__main__":
    main()
