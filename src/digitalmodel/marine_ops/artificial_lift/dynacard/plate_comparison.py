# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Compare caller-supplied synthetic shapes with the pinned CC0 workbook.

Coordinate evidence: Mendeley Data (2020), DOI 10.17632/hnfwm9mjs6.1,
retained workbook and manifest under testdata/mendeley_plate/.
Method: the repository's test_card_generator_shapes.py, prepare/loop_distance,
and benchmark/measurements.py, loop_distance. The uniform arc-length grid size
is a numerical convention inherited from that method, not a physical constant
or acceptance limit. Euclidean norm and distance: Mathematics for Machine
Learning (2020), equations (3.4), (3.21), public copy dated 2024-01-15,
https://mml-book.github.io/book/mml-book.pdf.
No engineering equation or calibrated limit is introduced.
"""

from __future__ import annotations

from collections.abc import Mapping, Sequence
from pathlib import Path
from statistics import median
from typing import Any

from .benchmark.measurements import loop_distance
from .benchmark.test_set_builder import validate_card
from .card_generators import ALL_GENERATORS
from .diagnostic_plate import _SOURCE_SHA256, load_diagnostic_plate
from .models import CardData


def compare_diagnostic_plate(
    workbook: str | Path,
    synthetic_cards: Mapping[str, Sequence[CardData]],
) -> dict[str, Any]:
    """Measure each literal load mapping, retaining every unscored curve.

    Args:
        workbook: Original workbook accepted by the digest-pinned loader.
        synthetic_cards: Current generator mode to caller-declared synthetic
            cards. Labels are supplied by the caller, never inferred here.
            Empty sample lists remain explicitly unevaluated.

    Returns:
        JSON-compatible evidence with unrounded distances and medians. Nearest
        load labels describe geometry only; ambiguous labels may be nearest
        without establishing a fault equivalence. Power curves are excluded.
        Unscored supplied counts retain every supplied mode without a literal
        load exemplar, including explicitly empty sample lists.

    Raises:
        ValueError: If the source bytes, mode names or card arrays are invalid.
    """
    curves = load_diagnostic_plate(workbook)
    unknown = set(synthetic_cards) - set(ALL_GENERATORS)
    if unknown:
        raise ValueError(f"Unknown generator mode: {', '.join(sorted(unknown))}")
    for samples in synthetic_cards.values():
        for card in samples:
            validate_card(card)

    loads = [curve for curve in curves if curve.ordinate == "load"]
    literal_modes = {curve.fault_mode for curve in loads if curve.fault_mode}
    rows = []
    for curve in curves:
        row: dict[str, Any] = {
            "label": curve.label,
            "block_index": curve.block_index,
            "ordinate": curve.ordinate,
            "fault_mode": curve.fault_mode,
            "status": "NOT_EVALUATED",
            "count": 0,
            "distances": [],
            "median_distance": None,
            "nearest_load_label": None,
            "median_distances_by_label": {},
        }
        if curve.ordinate != "load":
            row["reason"] = "Power curve is not a load-card exemplar"
        elif curve.fault_mode is None:
            row["reason"] = "Source label does not establish a literal generator mode"
        else:
            samples = synthetic_cards.get(curve.fault_mode, ())
            row["count"] = len(samples)
            row["reason"] = "No synthetic samples supplied"
            if samples:
                distances = {
                    reference.label: [
                        loop_distance(card.position, card.load,
                                      reference.position, reference.values)
                        for card in samples
                    ]
                    for reference in loads
                }
                medians = {label: median(values)
                           for label, values in distances.items()}
                row.update(
                    status="MEASURED",
                    reason="Geometry only; no acceptance limit supplied",
                    distances=distances[curve.label],
                    median_distance=medians[curve.label],
                    nearest_load_label=min(medians, key=medians.__getitem__),
                    median_distances_by_label=medians,
                )
        rows.append(row)

    return {
        "source_doi": "10.17632/hnfwm9mjs6.1",
        # The loader verified this digest against the bytes used for the curves.
        "source_sha256": _SOURCE_SHA256,
        "input_kind": "caller-declared synthetic cards",
        "metric": (
            "Per-axis normalisation, uniform arc-length resampling; minimum mean "
            "Euclidean distance over discrete cyclic shifts and both directions "
            "(benchmark.measurements.loop_distance)"
        ),
        "curves": rows,
        "modes_without_literal_exemplar": sorted(set(ALL_GENERATORS) - literal_modes),
        "unscored_supplied_counts": {
            mode: len(synthetic_cards[mode])
            for mode in sorted(set(synthetic_cards) - literal_modes)
        },
        "acceptance": {
            "status": "NOT_EVALUATED",
            "reason": "The source provides no shape-distance acceptance limit",
        },
        "field_accuracy": {
            "status": "NOT_EVALUATED",
            "reason": "A diagnostic plate is not a labelled field-card corpus",
        },
    }
