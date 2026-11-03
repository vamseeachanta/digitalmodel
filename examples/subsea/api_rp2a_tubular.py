# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Illustrative elastic tubular demand calculation, not a code-qualified design.

Run from the repository root:
    PYTHONPATH=src python examples/subsea/api_rp2a_tubular.py

All inputs are invented for the hand calculation, not material-grade data.
D=10 mm, t=1 mm gives d=8 mm, A=9*pi mm², I=369*pi/4 mm⁴,
Z=369*pi/20 mm³ by circular integration. N=90*pi N and M=369*pi/2 N mm
give axial and peak bending stresses of 10 MPa each.
Sources: MIT, Stress-Strain Curves (2001), equation 1, and MIT,
Stresses in Beams (2000), equations 4 and 7; see the calculation module.
"""

import json
import math

from digitalmodel.subsea.structural_checks import elastic_tubular_demands


def main() -> None:
    """Print demands and the explicitly incomplete acceptance disposition."""
    result = elastic_tubular_demands(
        diameter=10,
        thickness=1,
        yield_strength=100,  # Illustrative input; no API 5L material grade assumed.
        axial_force=90 * math.pi,
        bending_moment=369 * math.pi / 2,
        force_unit="N",
        moment_unit="N mm",
    )
    print(json.dumps({
        "area_mm2": round(result.area, 3),
        "second_moment_mm4": round(result.second_moment, 3),
        "section_modulus_mm3": round(result.section_modulus, 3),
        "stress_mpa": {
            check.name: (round(check.calculated_stress, 3)
                         if check.calculated_stress is not None else None)
            for check in result.checks
        },
        "status": result.status,
        "passes": result.passes,
        "basis": "Illustrative elastic demands; code capacities not established",
    }, indent=2))


if __name__ == "__main__":
    main()
