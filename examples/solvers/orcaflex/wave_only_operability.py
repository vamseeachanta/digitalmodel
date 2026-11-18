# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Demonstrate wave-only operability and rejection of unsupported criteria.

Run from the repository root with PYTHONPATH=src:
    python3 examples/solvers/orcaflex/wave_only_operability.py

All inputs are synthetic demonstration choices, not site data or design limits.
Independent hand calculation: accepted cells sum to 0.1 + 0.3 = 0.4, giving
40.0% operability and 60.0% non-operability. Addition/complement rules:
OpenStax, Introductory Statistics 2e (2023), section 3.3:
https://openstax.org/books/introductory-statistics-2e/pages/3-3-two-basic-rules-of-probability
"""

import json

from pydantic import ValidationError

from digitalmodel.orcaflex.weather_window import OperabilityInput, ScatterDiagram


def worked_example() -> dict:
    """Return the wave-only answer and unevaluated wind/current criteria."""
    # Synthetic inputs: see the independent hand calculation in the docstring.
    scatter = ScatterDiagram(
        hs_bins=[1.0, 2.0],
        tp_bins=[6.0, 8.0],
        occurrences=[[0.1, 0.2], [0.3, 0.4]],
    )
    wave_inputs = {"hs_limit": 2.0, "tp_limit": 6.0, "scatter": scatter}
    result = {
        "basis": "synthetic demonstration; wave-only assessment",
        "inputs": {
            "scatter": scatter.model_dump(),
            "hs_limit_m": 2.0,
            "tp_limit_s": 6.0,
        },
        "wave_only": OperabilityInput(**wave_inputs).calculate_operability(),
        "unsupported_criteria": {},
    }
    for field in ("wind_limit_mps", "current_limit_mps"):
        # The 1 m/s limit is a caller-selected synthetic test input.
        try:
            OperabilityInput(**wave_inputs, **{field: 1.0})
        except ValidationError as exc:
            result["unsupported_criteria"][field] = {
                "limit_mps": 1.0,
                "status": "NOT_EVALUATED",
                "reason": exc.errors(include_url=False, include_input=False)[0]["msg"],
            }
    return result


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2))
