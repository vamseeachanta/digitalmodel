# ABOUTME: Tests the issue #1915 wall-thickness, tension, and moment data sweep.
# ABOUTME: Verifies tidy rows, capacity-derived axes, and JSON-only output.

import importlib.util
import math
import sys
from pathlib import Path

from digitalmodel.structural.analysis.wall_thickness import DesignCode


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT = REPO_ROOT / "scripts" / "capabilities" / "build_wall_thickness_3d.py"


def _load_module():
    spec = importlib.util.spec_from_file_location("build_wall_thickness_3d", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def test_single_grid_point_emits_one_flat_row_per_design_code():
    generator = _load_module()

    study = generator.build_study(
        wall_thickness_mm=(18.0,),
        effective_tension_n=(0.0,),
        bending_moment_nm=(0.0,),
    )

    assert len(study["rows"]) == len(DesignCode) == 9
    assert {row["code"] for row in study["rows"]} == {
        code.value for code in DesignCode
    }
    for row in study["rows"]:
        assert row["wall_thickness_mm"] == 18.0
        assert row["effective_tension_n"] == 0.0
        assert row["bending_moment_nm"] == 0.0
        assert row["governing_check"]
        assert isinstance(row["utilisation"], float)
        assert all(not isinstance(value, (dict, list)) for value in row.values())


def test_default_metadata_records_capacity_derived_ranges_and_grid():
    generator = _load_module()

    meta = generator.build_metadata()
    basis = meta["load_range_basis"]

    expected_area_m2 = math.pi / 4 * (0.3239**2 - (0.3239 - 2 * 0.018) ** 2)
    expected_yield_tension_n = 448.0e6 * expected_area_m2
    expected_yield_moment_nm = (
        math.pi / 4 * 448.0e6 * (0.3239 - 0.018) ** 2 * 0.018
    )
    assert basis["reference_wall_thickness_mm"] == 18.0
    assert basis["reference_area_m2"] == expected_area_m2
    assert basis["tension"]["yield_capacity_n"] == expected_yield_tension_n
    assert basis["tension"]["maximum_n"] == 4_649_766
    assert basis["bending_moment"]["yield_capacity_nm"] == expected_yield_moment_nm
    assert basis["bending_moment"]["maximum_nm"] == 355_591

    grid = meta["grid"]
    assert grid["wall_thickness_mm"]["values"] == [
        8.0 + 0.5 * index for index in range(41)
    ]
    assert grid["effective_tension_n"]["values"][0] == 0.0
    assert grid["effective_tension_n"]["values"][-1] == 4_649_766
    assert grid["bending_moment_nm"]["values"][0] == 0.0
    assert grid["bending_moment_nm"]["values"][-1] == 355_591
    assert grid["row_count"] == 41 * 31 * 31 * 9 == 354_609
    assert meta["units"]["wall_thickness"] == "mm"
