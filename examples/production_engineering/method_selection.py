# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Run a synthetic undersaturated, single-phase oil selection example offline.

Input basis and hand calculation:
docs/domains/production/method-selection-example.md.
Selection and arithmetic do not establish engineering qualification.
"""

from dataclasses import dataclass

from digitalmodel.production_engineering.ipr_models import (
    LinearIpr,
    ReservoirConditions,
)
from digitalmodel.production_engineering.method_selector import (
    SelectionAssessment,
    assess_ipr_selection,
)


@dataclass(frozen=True)
class ExampleResult:
    """Return the supplied inputs, selection assessment and calculated rate."""

    reservoir: ReservoirConditions
    flowing_pressure_psi: float
    assessment: SelectionAssessment
    flow_rate_bopd: float


def run_example() -> ExampleResult:
    """Print and return the fixed synthetic case from the usage page.

    Returns:
        Inputs and outputs for the explicitly single-phase pressure interval.

    Raises:
        RuntimeError: The selector does not choose the required linear model.
    """
    # Synthetic inputs defined in the usage page, Synthetic inputs table.
    reservoir = ReservoirConditions(
        reservoir_pressure_psi=3000.0,
        bubble_point_psi=2000.0,
        productivity_index_bopd_psi=2.0,
    )
    flowing_pressure_psi = 2500.0
    assessment = assess_ipr_selection(
        {
            "reservoir_pressure_psi": reservoir.reservoir_pressure_psi,
            "bubble_point_psi": reservoir.bubble_point_psi,
            "minimum_pwf_psi": flowing_pressure_psi,
            "multirate_test_available": False,
        },
        {"phase": "oil", "solution_gas_drive": False, "significant_skin": False},
    )
    if assessment.method != "linear":
        raise RuntimeError("This single-phase example requires the linear selection")

    model = LinearIpr(reservoir, reservoir.productivity_index_bopd_psi)
    result = ExampleResult(
        reservoir=reservoir,
        flowing_pressure_psi=flowing_pressure_psi,
        assessment=assessment,
        flow_rate_bopd=model.flow_rate(flowing_pressure_psi),
    )
    print("Scenario: synthetic undersaturated, single-phase oil")
    print(f"Selected method: {assessment.method}")
    print(f"Rationale: {assessment.rationale}")
    print(f"Flow rate: {result.flow_rate_bopd:.1f} bopd")
    print(f"Qualification: {assessment.qualification_status}")
    print("Sources:")
    for source in assessment.sources:
        print(f"  - {source}")
    print("Open checks:")
    for check in assessment.open_checks:
        print(f"  - {check}")
    return result


if __name__ == "__main__":
    run_example()
