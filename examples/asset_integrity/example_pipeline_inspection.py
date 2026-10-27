# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic pipeline inspection-planning arithmetic, executable offline.

Run from the repository root:
    PYTHONPATH=src python examples/asset_integrity/example_pipeline_inspection.py

Input basis S1 (invented for this example): previous wall 12 mm, current
wall 11 mm, elapsed time 4 yr, assumed required wall 8 mm, supplied cap 3 yr.
Hand calculation: rate = (12 - 11) / 4 = 0.25 mm/yr;
thickness life = (11 - 8) / 0.25 = 12 yr. The existing planner convention
gives min(12 / 2, 3) = 3 yr. Basis S2 substitutes a current wall of 7 mm:
the wall is below the assumed requirement, so no positive life is claimed.

All numbers are synthetic assumptions, including the simulated readings;
none is a field measurement or a value from a standard. The supplied cap
has no standard basis. See docs/domains/asset-integrity/pipeline-inspection-example.md.
"""

from __future__ import annotations

import json
from dataclasses import asdict

from digitalmodel.asset_integrity.inspection_planning import (
    corrosion_rate_mm_yr,
    plan_inspection,
)


def build_example(*, max_interval_years: float) -> dict:
    """Return synthetic cases using a required, caller-supplied interval cap."""
    cases = {}
    # S1 and S2 from the synthetic input basis in the module docstring.
    for name, current, basis in (
        ("above_required_wall", 11.0, "S1"),
        ("below_required_wall", 7.0, "S2"),
    ):
        values = {
            "previous_mm": 12.0,
            "current_mm": current,
            "years_between": 4.0,
            "required_mm": 8.0,
            "max_interval_years": max_interval_years,
        }
        inputs = {
            key: {
                "value": value,
                "unit": "yr" if "years" in key else "mm",
                "measured": False,
                "assumed": True,
                "basis": (
                    "Synthetic caller-supplied cap; not standard-derived"
                    if key == "max_interval_years"
                    else f"Synthetic input basis {basis}; no field measurement"
                ),
            }
            for key, value in values.items()
        }
        rate = corrosion_rate_mm_yr(
            previous_mm=values["previous_mm"],
            current_mm=values["current_mm"],
            years_between=values["years_between"],
        )
        plan = plan_inspection(
            current_mm=values["current_mm"],
            required_mm=values["required_mm"],
            corrosion_rate=rate,
            code_max_interval_years=max_interval_years,
        )
        cases[name] = {
            "inputs": inputs,
            "plan": asdict(plan),
            "way_forward": (
                "Obtain a qualified integrity assessment before selecting an interval."
                if plan.screening_status == "pass"
                else "No positive thickness life or deferred inspection is claimed; "
                "obtain immediate qualified assessment for repair, re-rating or retirement."
            ),
        }

    return {
        "scope": (
            "This arithmetic example is a narrow integrity-management aid and "
            "does not establish DNV-RP-F116 compliance or a qualified inspection interval."
        ),
        "planner_convention": (
            "The existing planner uses min(remaining_life / 2, supplied cap). "
            "The cap is not standard-derived; code_max_interval_years is the function "
            "parameter name, and the returned standard label identifies the existing "
            "planner, not pipeline applicability or a standards assessment."
        ),
        "cases": cases,
    }


if __name__ == "__main__":
    # S1 supplied cap, an arbitrary synthetic assumption rather than a code limit.
    print(json.dumps(build_example(max_interval_years=3.0), indent=2))
