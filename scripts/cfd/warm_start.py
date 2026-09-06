#!/usr/bin/env python3
"""Portable CLI wrapper for digitalmodel's OpenFOAM warm-start safety tool."""
from __future__ import annotations

import os
import sys
from pathlib import Path

for candidate in (Path(__file__).resolve().parents[2] / "src",
                  Path(__file__).resolve().parents[2] / "dm_src",
                  Path(os.environ["DM_CFD_SRC"]) if os.environ.get("DM_CFD_SRC") else None):
    if candidate is not None and (candidate / "digitalmodel").is_dir():
        sys.path.insert(0, str(candidate)); break

try:
    from digitalmodel.solvers.openfoam.warm_start import main
except ImportError as exc:  # pragma: no cover
    sys.exit(f"warm_start: cannot import digitalmodel ({exc}); set DM_CFD_SRC")

if __name__ == "__main__":
    raise SystemExit(main())
