# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run with PYTHONPATH=src python examples/openfoam/wake_preflight.py.

All inputs are synthetic. From linear deep-water dispersion,
lambda=2*pi*U**2/g; choosing g=2*pi gives wavelengths of 1 and 4 m at
speeds of 1 and 2 m/s. Two requested wavelengths and a 45-degree half angle
are illustrative caller criteria, not engineering recommendations.
This preflight writes no case and runs no mesher or solver.
"""

import json
import math

from digitalmodel.solvers.openfoam.hull_domain import (
    HullWakeExtentError,
    build_hull_domain,
    preflight_wake_extent,
)
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest


def example_results() -> list[dict]:
    """Return a pass and a refusal for the same synthetic hull and boxes."""
    hull = HullManifest.from_dict({
        "source_file": "synthetic_hull.stl",
        "source_sha256": "0" * 64,
        "units_in": "m",
        "scale_to_m": 1.0,
        "orientation": {"x": "forward", "y": "port", "z": "up"},
        "origin": "aft_perpendicular_keel",
        "lpp_m": 6.0,
        "beam_m": 0.9,
        "draft_m": 0.3,
        "wetted_surface_m2": 7.2,
        "displacement_m3": 0.72,
        "watertight": True,
        "n_triangles": 12,
        "bbox_min_m": [-0.05, -0.45, 0.0],
        "bbox_max_m": [6.15, 0.45, 0.55],
    })
    domain = build_hull_domain(hull)
    boxes = [((-5.0, -10.0, -0.1), (7.0, 0.0, 1.0))]
    results = []
    for speed in (1.0, 2.0):
        try:
            report = preflight_wake_extent(
                hull, domain, boxes, speed_m_s=speed, gravity_m_s2=2 * math.pi,
                required_wake_wavelengths=2.0, wedge_half_angle_deg=45.0,
            )
        except HullWakeExtentError as error:
            if error.report is None:
                raise
            report = error.report
        results.append(report.to_provenance())
    return results


if __name__ == "__main__":
    print(json.dumps(example_results(), indent=2, allow_nan=False))
