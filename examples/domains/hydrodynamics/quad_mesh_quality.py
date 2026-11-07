# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Worked quad quality example; run from the repository with PYTHONPATH=src.

Inputs, hand derivations and the result table are in
docs/domains/hull_library/quad-mesh-quality.md, 'Executable worked example'.
Shape-function basis: Introduction to Solid Mechanics, Quadrilateral Elements,
undated online edition, accessed 2026-10-22 (S2 in the method note).
Metric definitions: Coreform Cubit 2026.8, Quadrilateral Metrics (S1).
All dimensions and limits below are synthetic declared geometric inputs.
"""

import json

from digitalmodel.hydrodynamics.quad_mesh_quality import (
    QuadMeshQualityReport,
    QuadQualityCriteria,
    assess_quad_mesh,
)

CASE_NAMES = (
    "rectangle", "skew_rhombus", "flat_triangle", "folded_quad", "warped_quad",
)


def build_report() -> QuadMeshQualityReport:
    """All edge ratios pass; angles, mapping and planarity expose the defects."""
    # Synthetic coordinates; the method note derives each map and minimum J.
    cases = (
        ((0, 0, 0), (2, 0, 0), (2, 1, 0), (0, 1, 0)),
        ((0, 0, 0), (1, 0, 0), (1.6, 0.8, 0), (0.6, 0.8, 0)),
        ((0, 0, 0), (2, 0, 0), (4, 0, 0), (0, 1, 0)),
        ((0, 0, 0), (2, 0, 0), (0, 1, 0), (1, 1, 0)),
        ((0, 0, 0), (1, 0, 0), (1, 1, 1), (0, 1, 0)),
    )
    nodes = [point for case in cases for point in case]
    # Four perimeter nodes per bilinear element, following S2's shape functions.
    panels = [list(range(start, start + 4)) for start in range(0, len(nodes), 4)]
    return assess_quad_mesh(
        nodes, panels,
        # Exact planarity and these bounds are demonstration inputs, not
        # solver limits. Strict J>0 follows the S2-derived geometric criterion.
        planarity_tolerance=0,
        criteria=QuadQualityCriteria(
            basis="Synthetic demonstration: edge ratio <=5, angles 60..120 degrees, J>0",
            max_edge_ratio=5, min_angle_deg=60, max_angle_deg=120, min_jacobian=0,
        ),
    )


def main() -> None:
    """Write full-precision JSON to stdout for storage or downstream tooling."""
    payload = build_report().to_dict()
    payload["case_names"] = CASE_NAMES
    print(json.dumps(payload, indent=2, allow_nan=False))


if __name__ == "__main__":
    main()
