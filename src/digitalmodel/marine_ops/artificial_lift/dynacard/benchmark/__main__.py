# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Score an authorised labelled pump-card corpus without training or network use."""

from __future__ import annotations

import argparse
import hashlib
import json
from collections.abc import Callable
from pathlib import Path

from ..diagnostics import MODEL_PATH, classify_with_model
from ..models import CardData
from .runner import BenchmarkRunner
from .test_set_builder import load_labelled_set


def benchmark_document(
    real_set: Path | None, *, minimum_count: int, confidence: float,
) -> dict:
    """Return independent scores and digests identifying the scored inputs.

    Reporting settings are required inputs: no universal sample-size acceptance
    limit is inferred. The existing loader requires provenance and units. Its
    declarations must be checked against acquisition and labelling records by
    the caller; schema validation cannot authenticate field measurements.
    """
    payload = real_set.read_bytes() if real_set is not None else None
    dataset = load_labelled_set(json.loads(payload)) if payload is not None else None
    artifact = {
        "status": "Not Evaluated", "training_domain": "synthetic",
        "method": "shipped numerical ML classifier; independent legacy threshold baseline",
    }
    classifier: Callable[[CardData], str] | None = None
    if dataset is not None and dataset.cards:
        # The training domain is documented in training.py, not inferred from
        # the exported CV number. Never present synthetic CV as a field score.
        model_bytes = MODEL_PATH.read_bytes()
        model = json.loads(model_bytes)

        def predict_card(card: CardData) -> str:
            # Reuse production inference against the exact bytes identified
            # below, independently of any process-level classifier cache.
            return classify_with_model(card, model)

        classifier = predict_card
        artifact.update(
            status="Evaluated", sha256=hashlib.sha256(model_bytes).hexdigest(),
            model_version=model["model_version"],
        )
    result = BenchmarkRunner(
        real_set=dataset, minimum_count=minimum_count, confidence=confidence,
        ml_classifier=classifier,
    ).run().to_dict()
    return {
        "sections": result["sections"],
        "input_sha256": hashlib.sha256(payload).hexdigest() if payload is not None else None,
        "classifier_artifact": artifact,
        "limitations": [
            "Provenance declarations require independent review; the loader cannot authenticate labels.",
            "Wilson intervals assume independent binomial observations; repeated well cards may be correlated.",
            "No classifier confidence or operational acceptance criterion is reported.",
        ],
    }


def main() -> None:
    """Write a JSON benchmark record to standard output."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--real-set", type=Path, help="Authorised labelled-set JSON document")
    parser.add_argument("--minimum-count", type=int, required=True,
                        help="Reporting threshold for overall and per-class estimates")
    parser.add_argument("--confidence", type=float, required=True,
                        help="Two-sided Wilson interval coverage, strictly between zero and one")
    args = parser.parse_args()
    try:
        result = benchmark_document(
            args.real_set, minimum_count=args.minimum_count, confidence=args.confidence,
        )
    except (OSError, ValueError, TypeError, KeyError) as exc:
        parser.exit(status=2, message=f"Benchmark input or model error: {exc}\n")
    print(json.dumps(result, indent=2, allow_nan=False))


if __name__ == "__main__":
    main()
