# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline protocol example: preserve evidence through CSV and JSON readback.

All numeric tokens are synthetic inputs, not engineering reference values.
The raw zero is intentionally identical across different computation states.
Expected answers follow the explicit states; no solver or unit conversion runs.
"""

from digitalmodel.data_systems.solver_reference_export import SolverReferenceExporter


SUPPORTED_UNITS = {"force": {"N"}}  # Caller-supplied spelling; no conversion.


def worked_example() -> SolverReferenceExporter:
    """Return the six-condition example after CSV and JSON readback."""
    common = {
        "quantity": "force",
        "source_unit": "N",
        "source_id": "synthetic-source",
        "revision": "synthetic-revision",
    }
    rows = [
        {
            **common,
            "condition_id": "computed",
            "value": 0,
            "value_state": "computed",
            "warnings": [],
        },
        {
            **common,
            "condition_id": "pending",
            "value": 0,
            "value_state": "uncomputed",
            "warnings": None,
        },
        {
            **common,
            "condition_id": "cached",
            "value": 0,
            "value_state": "cached_unqualified",
            "warnings": ["Applicability unknown, cached output"],
        },
        {
            **common,
            "condition_id": "no-output",
            "value": None,
            "value_state": "computed",
            "execution": {"exit_code": 0, "instance_handle": "opaque"},
        },
        {
            **common,
            "condition_id": "invalid",
            "value": "NaN",
            "value_state": "computed",
        },
        {**common, "condition_id": "unknown", "value": 0},
    ]
    exporter = SolverReferenceExporter(rows, supported_units=SUPPORTED_UNITS)
    csv_readback = SolverReferenceExporter.from_csv(
        exporter.to_csv(), supported_units=SUPPORTED_UNITS
    )
    return SolverReferenceExporter.from_json(
        csv_readback.to_json(), supported_units=SUPPORTED_UNITS
    )


if __name__ == "__main__":
    print(worked_example().to_json(), end="")
