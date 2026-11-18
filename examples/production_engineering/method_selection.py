# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Run a sourced undersaturated, single-phase oil selection example offline.

Basis: Production Engineering II, 2017-2018 course text, Problem (1-4),
pp. 40-41; equations (1.1) and (1.4), pp. 16-17. Source link and calculation:
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
    """Print and return the published teaching case from the usage page.

    Returns:
        Inputs and outputs for the explicitly single-phase pressure interval.

    Raises:
        RuntimeError: The selector does not choose the required linear model.
    """
    # Course text, Problem (1-4), pp. 40-41; all pressures share a gauge basis.
    # J = 250 / (3000 - 2500) = 0.5 bopd/psi, Eq. (1.1), p. 16.
    reservoir = ReservoirConditions(
        reservoir_pressure_psi=3000.0,
        bubble_point_psi=2130.0,
        productivity_index_bopd_psi=0.5,
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
    print("Scenario: sourced teaching example, undersaturated single-phase oil")
    print("Input basis: Production Engineering II (2017-2018), Problem (1-4), pp. 40-41")
    print(
        f"Pr = {reservoir.reservoir_pressure_psi:.3f} psig; "
        f"Pwf = {flowing_pressure_psi:.3f} psig; "
        f"Pb = {reservoir.bubble_point_psi:.3f} psig"
    )
    print("Criterion: Pr >= Pwf >= Pb; no multirate test or significant skin")
    print(f"Productivity index: {reservoir.productivity_index_bopd_psi:.3f} bopd/psi")
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
