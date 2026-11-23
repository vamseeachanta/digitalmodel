#!/usr/bin/env python
"""CLI shim for :mod:`digitalmodel.solvers.openfoam.wave_cut`."""

from digitalmodel.solvers.openfoam.wave_cut import main


if __name__ == "__main__":
    raise SystemExit(main())
