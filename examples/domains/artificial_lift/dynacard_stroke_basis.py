# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Run a sourced lost-motion example and audit every synthetic mode's scales.

Run with the repository's src directory on PYTHONPATH. Printed extrema are
rounded for display; disjoint pairs are computed from unrounded values.
"""

import json

import numpy as np

from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    generate_normal_card,
    generate_tubing_movement_card,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.generator_audit import (
    audit_generator_scale_ranges,
)


def worked_example() -> dict:
    """Return the SWPSC (2017), Fig. 1 kinematic subtraction and scale audit."""
    # SWPSC, Over-travel Can Occur On Both The Upstroke And Down Stroke (2017),
    # Definition of Over Travel and Under Travel, Fig. 1: surface 45.4 in,
    # rod stretch 30.7 in, dynamic overtravel 12.2 in, tubing stretch 8.3 in.
    # The reconstructed otherwise matched anchored travel is 26.9 in.
    anchored = 45.4 - 30.7 + 12.2
    tubing_loss = 8.3
    card = generate_tubing_movement_card(
        anchored_stroke_in=anchored,
        lost_motion_in=tubing_loss,
    )
    healthy = generate_normal_card(stroke_in=18.6)  # Same source's pump travel.
    # Sample count is an example workload, not an engineering acceptance limit.
    audit = audit_generator_scale_ranges(samples_per_mode=32)
    for features in audit["ranges"].values():
        for bounds in features.values():
            for endpoint in bounds:
                bounds[endpoint] = round(bounds[endpoint], 1)
    return {
        "anchored_travel_in": round(anchored, 1),
        "specified_tubing_loss_in": tubing_loss,
        "pump_travel_in": round(float(np.ptp(card.position)), 1),
        "other_installation_healthy_travel_in": round(
            float(np.ptp(healthy.position)), 1
        ),
        "basis": "SWPSC (2017), Definition of Over Travel and Under Travel, Fig. 1",
        "limits": "Kinematic comparison only; field envelope and diagnosis NOT_EVALUATED",
        "scale_audit": audit,
    }


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2))
