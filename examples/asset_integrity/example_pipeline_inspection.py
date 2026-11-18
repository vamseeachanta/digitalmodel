# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Public worked-example arithmetic for thickness-based inspection planning.

Run from the repository root:
    PYTHONPATH=src python examples/asset_integrity/example_pipeline_inspection.py

Numerical basis: SOURCE, section "A worked example". Published readings are
7.3 mm in 2023 and 6.4 mm in 2026, with a required wall of 4.5 mm.
The article suggests a 3-year schedule; its 3.8-year long-term alternative
is reused only as a counterfactual ceiling, never as a qualified interval.
The boundary control reuses the required wall as current wall, not a reading.

Method references: API 570, Piping Inspection Code, 5th edition (2024),
7.1.2 Corrosion Rate Calculations; 7.2 Remaining Life Calculations;
6.3.3 Thickness Measurement Inspection. Numerical values come from the public
article, not a licensed publication. Standard applicability is not established.
See docs/domains/asset-integrity/pipeline-inspection-example.md.
"""

from __future__ import annotations

import json
from dataclasses import asdict

from digitalmodel.asset_integrity.inspection_planning import (
    corrosion_rate_mm_yr,
    plan_inspection,
)

SOURCE = {
    "url": (
        "https://highpointsolutions.com.au/knowledge/"
        "how-often-should-ultrasonic-thickness-testing-be-done/"
    ),
    "title": "How often should ultrasonic thickness testing be done?",
    "published": "2026-09-09",
    "section": "A worked example",
    "checked": "2026-10-27",
    "readiness": "Public worked-example reproduction only; no equipment qualification",
}


def _inputs(*, current_mm: float, cap: float, boundary: bool) -> dict:
    """Attach the published basis or explicit transformation to every input."""
    values = {
        "previous_mm": (7.3, "Published 2023 reading"),
        "current_mm": (
            current_mm,
            "Boundary control: reuse published required wall; not a reading"
            if boundary else "Published 2026 reading",
        ),
        "years_between": (3.0, "Published dates: 2026 minus 2023"),
        "required_mm": (4.5, "Published assessment threshold; design basis not supplied"),
        "max_interval_years": (
            cap,
            "Published suggested schedule; not a code maximum"
            if cap == 3.0 else
            "Published long-term alternative reused as a counterfactual ceiling; "
            "not a selected interval or code maximum",
        ),
    }
    return {
        key: {
            "value": value,
            "unit": "yr" if "years" in key else "mm",
            "measured": False,
            "assumed": True,
            "basis": basis,
            "source_url": SOURCE["url"],
            "source_section": SOURCE["section"],
        }
        for key, (value, basis) in values.items()
    }


def _case(*, current_mm: float, cap: float, boundary: bool) -> dict:
    """Run the unchanged planner and present its criterion beside the result."""
    inputs = _inputs(current_mm=current_mm, cap=cap, boundary=boundary)
    rate = corrosion_rate_mm_yr(
        previous_mm=inputs["previous_mm"]["value"],
        current_mm=current_mm,
        years_between=inputs["years_between"]["value"],
    )
    required = inputs["required_mm"]["value"]
    plan = plan_inspection(
        current_mm=current_mm, required_mm=required,
        corrosion_rate=rate, code_max_interval_years=cap,
    )
    return {
        "case_kind": "boundary_control" if boundary else "published_worked_example",
        "inputs": inputs,
        "plan": {
            key: round(value, 3) if isinstance(value, float) else value
            for key, value in asdict(plan).items()
        },
        "screening_basis": (
            "Positive-life criterion: current wall > required wall; "
            f"current wall {current_mm:.3f} mm; required wall {required:.3f} mm; "
            f"corrosion rate {rate:.3f} mm/yr. "
            "Interval criterion: min(remaining_life / 2, supplied cap); "
            f"half-life {plan.half_life_interval_years:.3f} yr; supplied cap {cap:.3f} yr. "
            "Basis: cited public worked example and the stated boundary transformation."
        ),
        "way_forward": (
            "Obtain a qualified integrity assessment before selecting an interval."
            if plan.screening_status == "pass" else
            "No positive thickness life or deferred inspection is claimed; "
            "obtain immediate qualified assessment for repair, re-rating or retirement."
        ),
    }


def build_example(*, max_interval_years: float) -> dict:
    """Return the public example and boundary control with a cited ceiling."""
    if (
        isinstance(max_interval_years, bool)
        or not isinstance(max_interval_years, (int, float))
        or max_interval_years not in (3.0, 3.8)
    ):
        raise ValueError("max_interval_years must select a cited ceiling: 3.0 or 3.8 yr")
    return {
        "source": dict(SOURCE),
        "scope": (
            "This arithmetic example is a narrow integrity-management aid and "
            "does not establish DNV-RP-F116 compliance or a qualified inspection interval."
        ),
        "planner_convention": (
            "The existing planner uses min(remaining_life / 2, supplied cap). "
            "The supplied cap is not standard-derived: 3.0 yr is the public article's "
            "suggested schedule; 3.8 yr is a counterfactual comparison ceiling. "
            "code_max_interval_years and the returned standard label are unchanged "
            "planner fields, not an assessment of pipeline applicability. "
            "Computed results are displayed to three decimals; inputs retain precision."
        ),
        "cases": {
            "above_required_wall": _case(
                current_mm=6.4, cap=max_interval_years, boundary=False,
            ),
            "at_required_wall": _case(
                current_mm=4.5, cap=max_interval_years, boundary=True,
            ),
        },
    }


if __name__ == "__main__":
    # The published suggested schedule, not a standard-derived maximum.
    print(json.dumps(build_example(max_interval_years=3.0), indent=2, allow_nan=False))
