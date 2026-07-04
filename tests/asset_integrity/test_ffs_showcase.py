# ABOUTME: Light test for the FFS showcase data-build function — asserts non-empty
# ABOUTME: grids with finite safe-pressure values (no HTML rendering in the test).
"""Test the FFS showcase driver's data-build function.

Imports ``build_showcase_data`` from the demo driver and checks that it returns
non-empty corroded-pipe grids with finite safe-pressure values for at least one
preset+method combination. Deliberately light: it does not render the HTML.
"""

import importlib.util
import math
from pathlib import Path

_DRIVER = (
    Path(__file__).resolve().parents[2]
    / "examples" / "demos" / "asset_integrity" / "ffs_showcase.py"
)


def _load_driver():
    spec = importlib.util.spec_from_file_location("ffs_showcase_driver", _DRIVER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_build_showcase_data_returns_finite_grids():
    data = _load_driver().build_showcase_data()
    cor = data["corroded"]

    assert cor["grids"], "expected at least one (preset, method) grid"
    assert cor["presets"] and cor["methods"]

    # At least one preset+method grid must have finite, positive safe pressures.
    finite_grid_found = False
    for combo, grid in cor["grids"].items():
        safe = grid["safe"]
        assert safe, f"empty safe-pressure grid for {combo}"
        flat = [v for row in safe for v in row]
        assert all(math.isfinite(v) for v in flat), f"non-finite safe pressure in {combo}"
        if any(v > 0 for v in flat):
            finite_grid_found = True
            assert grid["maop"] > 0
            assert grid["ratio"] and len(grid["ratio"]) == len(safe)
    assert finite_grid_found, "no grid had positive finite safe pressures"
