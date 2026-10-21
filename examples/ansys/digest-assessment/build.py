# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Assess a synthetic supported-load digest without launching a solver.

Run with the source tree on PYTHONPATH. All numbers below are illustrative
inputs, not properties or acceptance criteria for a physical component.
Hand answer: 60/100 = 0.6; (300-300, 400-400) = (0, 0) N.
"""

import json
from dataclasses import asdict

from digitalmodel.ansys.screening_validation import (
    ScreeningAssessment,
    ScreeningCriteria,
    assess_supported_digest,
)


def worked_example() -> ScreeningAssessment:
    """Return content dispositions for the documented independent hand case."""
    # Synthetic case defined in docs/domains/ansys/digest-assessment.html,
    # revision 0 (2026); all limits and tolerances are demonstration inputs.
    criteria = ScreeningCriteria(
        allowable_mpa=100.0,
        yield_strength_mpa=200.0,
        expected_applied_fx_n=300.0,
        expected_applied_fy_n=400.0,
        force_tolerance_n=1.0,
        stress_tolerance_mpa=0.0,
        unity_tolerance=0.0,
        source="Synthetic hand case; digest-assessment.html revision 0 (2026)",
    )
    csv_text = (
        "max_seqv_mpa,60,allowable_mpa,100,uc,0.6\n"
        "applied_fx_n,300,applied_fy_n,400\n"
        "reaction_fx_n,-300,reaction_fy_n,-400\n"
        "force_residual_n,0\n"
    )
    return assess_supported_digest(csv_text, criteria)


if __name__ == "__main__":
    result = worked_example()
    print(
        json.dumps(
            {
                "content_status": result.content_status,
                "native_qualification": result.native_qualification,
                "criteria_source": result.criteria_source,
                "checks": {
                    name: asdict(check) for name, check in result.checks.items()
                },
            },
            indent=2,
        )
    )
