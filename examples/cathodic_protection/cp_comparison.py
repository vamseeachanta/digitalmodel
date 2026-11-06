# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run an offline synthetic cost comparison with PYTHONPATH=src.

    python examples/cathodic_protection/cp_comparison.py

All amounts, years and the rate are synthetic assumptions, not market prices.
Independent arithmetic at a ten percent annual effective rate:
ICCP = 1000 + 110/1.1 + (121+242)/1.21 = 1400;
SACP = 1200 + 22/1.1 + (24.2+121)/1.21 = 1340.
The difference is 60 currency units. Neither alternative is design-qualified.
"""

from digitalmodel.cathodic_protection.cp_comparison import (
    AlternativeInput,
    AnnualCost,
    ComparisonInput,
    compare_systems,
)


def main() -> None:
    """Print category totals, NPV, signed difference and the synthetic basis."""
    inputs = ComparisonInput(
        design_life_years=2,
        discount_rate=0.1,
        provenance=(
            "Synthetic common monetary basis; no escalation, tax or salvage; "
            "capital at time zero, annual payments at year end."
        ),
        iccp=AlternativeInput(
            currency="USD",
            initial_cost=1000,
            annual_costs=(
                AnnualCost(year=1, operating_cost=110, replacement_cost=0),
                AnnualCost(year=2, operating_cost=121, replacement_cost=242),
            ),
            provenance="Synthetic already-sized ICCP; assumed capital and annual costs.",
        ),
        sacp=AlternativeInput(
            currency="USD",
            initial_cost=1200,
            annual_costs=(
                AnnualCost(year=1, operating_cost=22, replacement_cost=0),
                AnnualCost(year=2, operating_cost=24.2, replacement_cost=121),
            ),
            provenance="Synthetic already-sized SACP; assumed capital and annual costs.",
        ),
    )
    result = compare_systems(inputs)
    # Round presentation to one decimal place; retain full precision in the API.
    for label, alternative in (("ICCP", result.iccp), ("SACP", result.sacp)):
        print(f"{label} ({result.currency}):")
        for kind in ("undiscounted", "npv"):
            totals = getattr(alternative, kind)
            print(
                f"  {kind}: initial {totals.initial_cost:.1f}, "
                f"operating {totals.operating_cost:.1f}, "
                f"replacement {totals.replacement_cost:.1f}, "
                f"total {totals.total_cost:.1f}"
            )
    print(f"NPV difference (ICCP minus SACP): {result.npv_difference_iccp_minus_sacp:.1f}")
    print(f"Financial basis: {result.input_basis.provenance}")
    print(f"ICCP basis: {result.input_basis.iccp.provenance}")
    print(f"SACP basis: {result.input_basis.sacp.provenance}")


if __name__ == "__main__":
    main()
