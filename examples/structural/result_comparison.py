# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run with PYTHONPATH=src python examples/structural/result_comparison.py.

All inputs and tolerances are synthetic choices for demonstrating arithmetic,
not design criteria. Independent hand calculations:
reaction: -12 - (-10) = -2 N, within the chosen 2 N absolute tolerance;
displacement: 0.014 - 0.010 = 0.004 m, exceeds the chosen 0.003 m tolerance;
member end action: 11 - 10 = 1 N, within the chosen 1 N tolerance.
Relative tolerances are zero, so each bound equals its absolute tolerance.
Agreement alone establishes no standard compliance or physical model validity.
"""

from digitalmodel.structural.result_comparison import (
    ReferenceMetadata,
    ResultComparison,
    ResultKey,
    ResultValue,
    Tolerance,
)


def main() -> None:
    """Compare extracted synthetic values without invoking any solver."""
    metadata = ReferenceMetadata(
        model_id="synthetic-frame",
        load_case="static-case",
        state="undeformed-static",
        reference="common-origin-and-node-member-map",
        convention="right-handed; member i-to-j; positive-end-actions",
    )
    reaction = ResultKey("node-a", "force", "fy", "node")
    displacement = ResultKey("node-b", "translation", "uy", "node")
    member = ResultKey("member-a", "force", "axial", "i")
    comparison = ResultComparison()
    comparison.add_result(
        "reference",
        metadata=metadata,
        reactions={reaction: ResultValue(-10, "N", "global")},
        displacements={displacement: ResultValue(0.010, "m", "global")},
        member_forces={member: ResultValue(10, "N", "member-local")},
    )
    comparison.add_result(
        "candidate",
        metadata=metadata,
        reactions={reaction: ResultValue(-12, "N", "global")},
        displacements={displacement: ResultValue(0.014, "m", "global")},
        member_forces={member: ResultValue(11, "N", "member-local")},
    )
    cases = (
        ("reactions", comparison.compare_reactions, reaction, Tolerance(2, 0, "N")),
        ("displacements", comparison.compare_deflections, displacement,
         Tolerance(0.003, 0, "m")),
        ("member_forces", comparison.compare_member_forces, member, Tolerance(1, 0, "N")),
    )
    for label, compare, key, tolerance in cases:
        report = compare("reference", "candidate", tolerances={key: tolerance})
        print(f"{label}: {report.status}")
        for row in report.differences:
            print(
                f"  {row.key.identifier} {row.key.component} at {row.key.location}: "
                f"signed={row.signed_difference:.1e}, "
                f"absolute={row.absolute_difference:.1e}, "
                f"limit={row.allowed_difference:.1e} {row.unit}"
            )


if __name__ == "__main__":
    main()
