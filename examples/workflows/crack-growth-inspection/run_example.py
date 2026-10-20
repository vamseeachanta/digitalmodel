# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Print a compact, reproducible crack-growth and inspection example as JSON.

Run with PYTHONPATH=src python examples/workflows/crack-growth-inspection/run_example.py.
No files are written. All inspection-policy inputs are synthetic illustrations.
"""

import json
from dataclasses import asdict

from digitalmodel.asset_integrity.crack_life import (
    plan_crack_from_fad,
    screen_leak_before_break,
)
from digitalmodel.asset_integrity.inspection_planning import plan_crack_inspection
from digitalmodel.fatigue.crack_growth import stress_intensity_factor
from digitalmodel.fatigue.threshold_growth import GrowthLaw, integrate_growth


def build_example():
    """MIT Fatigue (2001), Table 1 and Problem 6; independent integral in docs."""
    # Published problem: steel C=1e-11, m=3, range=950 MPa,
    # central infinite-plate half-lengths 0.1 and 2.5 mm, geometry factor 1.
    # No threshold or R correction is specified in that problem: zero
    # threshold and gamma=1 explicitly recover its Paris-only equation.
    law = GrowthLaw(
        1e-11,
        3,
        0,
        0,
        1,
        "MIT Fatigue (2001), Table 1 and Problem 6; Paris-only illustration",
    )
    growth = integrate_growth(
        law,
        lambda a: stress_intensity_factor(950, a, Y=1),
        initial_m=0.0001,
        limit_m=0.0025,
        steps=4000,
    )
    # Synthetic inspection policy: 1000 cycles/year, 2-year cap and divisor 2.
    # These inputs do not assert a code interval or operating history.
    plan = plan_crack_inspection(
        growth,
        cycles_per_year=1000,
        code_max_interval_years=2,
        life_divisor=2,
        policy_basis="Synthetic inspection policy, not a code requirement",
    )
    # Separate synthetic surface-flaw example: all geometry, strength,
    # toughness, loading and policy values are illustrative required inputs.
    fad = plan_crack_from_fad(
        fad_inputs=dict(
            length_mm=60,
            t_mm=20,
            sigma_m_mpa=100,
            sigma_y_mpa=350,
            sigma_u_mpa=500,
            k_mat=100,
        ),
        law=GrowthLaw(1e-11, 3, 0, 0, 1, "Synthetic surface-flaw material inputs"),
        delta_k=lambda a: stress_intensity_factor(100, a, Y=1),
        initial_depth_mm=1,
        steps=1000,
        cycles_per_year=100_000,
        code_max_interval_years=3,
        life_divisor=2,
        policy_basis="Synthetic inspection policy, not a code requirement",
    )
    return {
        "published_source": "https://web.mit.edu/course/3/3.11/www/modules/fatigue.pdf",
        "published_inputs": {
            "initial_half_length_m": 0.0001,
            "critical_half_length_m": 0.0025,
            "stress_range_mpa": 950,
            "law": asdict(law),
        },
        "published_life_cycles": growth.life_cycles,
        "inspection": asdict(plan),
        "synthetic_fad_example": {
            "limit_mm": fad.limiting_depth_mm,
            "limit_basis": fad.critical_depth_basis,
            "inspection": asdict(fad.inspection),
        },
        "lbb": asdict(screen_leak_before_break(through_wall=True)),
    }


if __name__ == "__main__":
    print(json.dumps(build_example(), indent=2, allow_nan=False))
