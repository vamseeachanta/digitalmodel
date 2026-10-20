# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Worked scoring arithmetic; no field cards or acquired labels are implied.

Run with PYTHONPATH=src python examples/domains/artificial_lift/dynacard_benchmark_demo.py.
"""

import json
from statistics import NormalDist

from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark import (
    BenchmarkRunner,
    score_predictions,
)


def worked_example() -> dict:
    """Score four invented labels and predictions against hand arithmetic.

    NIST/SEMATECH e-Handbook 7.2.4.1, Wilson score formula (accessed 2026-10-19):
    choose z=2 as an example input, n=4 and p=2/4. The denominator is 2,
    centre is 1/2 and radius is sqrt(1/8); accuracy is 50.0%, bounds are
    14.6% and 85.4%. Neither z nor the four-card threshold is a recommendation.
    """
    labels = ["NORMAL", "NORMAL", "GAS_LOCK", "GAS_LOCK"]
    predictions = ["NORMAL", "GAS_LOCK", "GAS_LOCK", "NORMAL"]
    return {
        "evidence_kind": "invented arithmetic example; not field evidence",
        "labels": labels,
        "predictions": predictions,
        "score": score_predictions(
            labels, predictions, minimum_count=4,
            confidence=2 * NormalDist().cdf(2) - 1,
        ),
        "field_status": BenchmarkRunner().run().sections["real_card_accuracy"]["status"],
    }


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2, allow_nan=False))
