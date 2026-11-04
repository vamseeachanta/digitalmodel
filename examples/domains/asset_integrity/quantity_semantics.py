"""Worked synthetic quantity example; no engineering acceptance rule is implied.

Inputs are assumed, based on previous experience, for illustration only.
Hand calculation: capacity/demand = 60/80 = 0.75. For the explicitly selected
linear scaling rule, a limit of 80 MPa becomes 80 * 0.75 / 1 = 60 MPa.
All bands and the allowable are caller inputs, not standard requirements.
Run with the source package on the Python path; the result is printed as JSON.
"""

import json

from digitalmodel.asset_integrity.assessment.ffs_decision import (
    DecisionBands,
    decide,
    scaled_limit,
)
from digitalmodel.asset_integrity.assessment.ffs_report import (
    FFSReport,
    ReportMethodology,
)
from digitalmodel.units import Q_


def worked_example():
    """Return the decision and an offline report for the stated hand calculation."""
    # Synthetic scenario definition in this module's docstring, not code limits.
    capacity_mpa, demand_mpa = 60, 80
    allowable_ratio = 1
    bands = DecisionBands(monitor_band=0.1, derate_floor=0, repair_life_yr=2)
    decision = decide(
        capacity_mpa / demand_mpa,
        allowable_ratio,
        5,
        "pipeline",
        margin_label="capacity/demand",
        bands=bands,
        derate=scaled_limit(
            Q_(demand_mpa, "MPa"),
            "[pressure]",
            label="axial stress limit",
            reference="illustrative linear scaling; no code qualification",
            quantity="axial_stress",
            action="REDUCE AXIAL STRESS",
        ),
    ).to_dict()
    report = FFSReport.generate_screening_html(
        "illustrative-component",
        "Axial membrane screening analysis",
        decision,
        {
            "Inputs": {
                "capacity (MPa)": capacity_mpa,
                "demand (MPa)": demand_mpa,
                "allowable capacity/demand": allowable_ratio,
            },
            "Answer": {
                "margin (dimensionless)": decision["margin"],
                "axial stress limit (MPa)": decision["derated"],
            },
        },
        ["Synthetic inputs; combined loading and code qualification NOT_EVALUATED."],
        methodology=ReportMethodology(
            "Caller-selected capacity/demand comparison and linear scaling",
            "Illustrative arithmetic only; no stress-method qualification.",
            ("Hand calculation in quantity_semantics.py module docstring",),
        ),
    )
    return decision, report


if __name__ == "__main__":
    decision, _ = worked_example()
    print(json.dumps(decision, indent=2))
