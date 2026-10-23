# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Invented cards demonstrate intake; field-data accuracy remains unevaluated.

Input: two normal-labelled unit squares and one gas-labelled square. Method:
fixed predictions NORMAL, GAS_LOCK, GAS_INTERFERENCE. Hand answer: one of two
normal cards correct, normal recall = 1/2 = 50.0 %. The gas class has only one
card and is below this example's chosen reporting count of two. Coordinates,
permissions and predictions are invented, with no engineering limits implied.
Temporary files use the caller's temporary-directory environment and are retained.
"""

import csv
import hashlib
import json
from pathlib import Path
from tempfile import mkdtemp

from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.field_dataset import (
    DataPermission,
    acquisition_status,
    evaluate_export,
    load_three_column_export,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.test_set_builder import (
    LabellingProvenance,
)


def run_example() -> dict:
    """Exercise the full adapter with invented input and independent predictions."""
    folder = mkdtemp(prefix="field-card-example-")
    path = Path(folder) / "invented-cards.csv"
    with path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.writer(stream)
        writer.writerow(["loads", "positions", "type"])
        for label in ("healthy", "healthy", "gas"):
            writer.writerow(["[0,0,1,1]", "[0,1,1,0]", label])
    dataset = load_three_column_export(
        path,
        # Self-computed only for this invented example; real intake requires
        # a digest independently confirmed by the data supplier.
        expected_sha256=hashlib.sha256(path.read_bytes()).hexdigest(),
        permission=DataPermission(
            written_agreement=True,
            benchmark_allowed=True,
            evidence_reference="invented-example-terms",
            licence="MIT",
            attribution="Invented unit-square example under repository licence",
        ),
        # The adapter's real/recorded branch is exercised with invented data.
        # The result is kept separate from the field-data status below.
        provenance=LabellingProvenance(
            source="invented unit squares; no field accuracy evidence",
            labeller="fixed example dictionary",
            date="2026-10-19",
            licence="MIT",
            kind="real",
            acquisition="recorded",
        ),
        label_map={"healthy": "Normal", "gas": "Gas interference"},
        codebook_reference="invented-example-dictionary",
        card_space="pump",
        units={"position": "in", "load": "lbf"},
    )
    predictions = iter(["NORMAL", "GAS_LOCK", "GAS_INTERFERENCE"])
    # Software demonstration policy, not a sample-adequacy recommendation.
    # Confidence is an explicit example input, not an acceptance criterion.
    result = evaluate_export(
        dataset,
        minimum_count=2,
        confidence=0.95,
        ml_classifier=lambda card: next(predictions),
        heuristic_classifier=lambda card: "NORMAL",
    )
    normal = result["methods"]["ml"]["per_class"]["NORMAL"]
    gas = result["methods"]["ml"]["per_class"]["GAS_INTERFERENCE"]
    return {
        "field_data": acquisition_status(),
        "invented_example": {
            "basis": "Invented cards and fixed predictions; not field evidence",
            "normal_correct": normal["correct"],
            "normal_count": normal["count"],
            "normal_recall_percent": round(normal["accuracy"] * 100, 1),
            "gas_count": gas["count"],
            "gas_status": gas["status"],
        },
    }


if __name__ == "__main__":
    print(json.dumps(run_example(), indent=2, allow_nan=False))
