# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Construct two geometric scales; native achieved wall values remain unmeasured."""

import dataclasses
import json
import math

import pytest

from digitalmodel.solvers.openfoam.hull_case import HullCaseConfig, build_hull_case
from digitalmodel.solvers.openfoam.hull_double_body import DoubleBodyCaseConfig, build_double_body_case
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest
from digitalmodel.solvers.openfoam.near_wall_acceptance import (
    NearWallCriteria, WallMeasurement, assess_near_wall,
)
from digitalmodel.solvers.openfoam.validation.ship_resistance import (
    ShipResistanceConfig, build_ship_resistance_case,
)

from .conftest import scaled_manifest_dict


@pytest.mark.parametrize("kind", ["hull", "double_body", "ship"])
def test_both_scales_are_pending_and_measurement_differences_are_gated(kind, stl_file, tmp_path):
    # Scales from issue 2025 acceptance; U=2 from existing synthetic hull tests.
    for scale in (1, 10):
        name = f"{kind}_{scale}"
        if kind == "ship":
            reference = ShipResistanceConfig()
            config = dataclasses.replace(
                reference, name=name, lpp=reference.lpp * scale,
                loa=reference.loa * scale, beam=reference.beam * scale,
                draft=reference.draft * scale,
                wetted_surface=reference.wetted_surface * scale**2,
                reynolds=reference.reynolds * scale,
            )
            case = build_ship_resistance_case(config, tmp_path)
            pending = json.loads((case / "near_wall_acceptance.json").read_text(encoding="utf-8"))
        else:
            cls, builder = ((HullCaseConfig, build_hull_case) if kind == "hull"
                            else (DoubleBodyCaseConfig, build_double_body_case))
            config = cls(
                manifest=HullManifest.from_dict(scaled_manifest_dict(scale)),
                stl_path=stl_file, velocity=2 * math.sqrt(scale), ranks=8, name=name,
            )
            case = builder(config, tmp_path)
            pending = json.loads((case / "case_provenance.json").read_text(encoding="utf-8"))["mesh"]["near_wall_acceptance"]
        assert pending["status"] == "NOT_EVALUATED"
        assert not pending["passed"]
        assert "relativeSizes true;" in (case / "system/snappyHexMeshDict").read_text(encoding="utf-8")

    # Invented observations exercise the gate, not a claim of achieved CFD y+.
    criteria = NearWallCriteria(50, 0.2, 0.9, 0.05, "synthetic test basis")
    small = WallMeasurement(40, 60, 9, 10, "hull", "synthetic small-case evidence")
    large = dataclasses.replace(small, y_plus_max=600, source="synthetic large-case evidence")
    report = assess_near_wall({"small": small, "large": large}, criteria)
    assert not report["passed"]
    assert not report["cases"]["large"]["y_plus_passed"]
