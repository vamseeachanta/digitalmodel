"""Run a synthetic Part 9 Level 1 readiness example, without curve assumptions.

Run from the repository root with PYTHONPATH=src and execute this file with
Python. Output is strict JSON on stdout; no files are written.

Method locator: API 579-1/ASME FFS-1 (2021), 9.4.2. The public contents
supplies section titles only. All dimensions, material properties and loads
below are synthetic demonstration inputs, not values from the standard.
Expected answer: NOT_EVALUATED and ESCALATE, because no qualified Level 1
curves, regeneration assumptions or digitized validation points are supplied.
"""

import json

from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter
from digitalmodel.asset_integrity.assessment.part9_level1 import Part9Level1Case


def worked_example() -> dict:
    """Show the inputs, method locator, missing evidence and resulting action."""
    case = Part9Level1Case(
        geometry="cylinder",
        reference_flaw="quarter_thickness",
        thickness_mm=20.0,
        flaw_depth_mm=5.0,
        flaw_length_mm=40.0,
        inside_radius_mm=500.0,
        membrane_stress_mpa=100.0,
        bending_stress_mpa=0.0,
        assessment_temperature_c=20.0,
        yield_strength_mpa=300.0,
        ultimate_strength_mpa=450.0,
        fracture_toughness_mpa_sqrt_m=80.0,
    )
    return {"input_basis": "synthetic demonstration", **FFSRouter.route_crack_like_flaw(case)}


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2, allow_nan=False))
