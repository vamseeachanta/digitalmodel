# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run a wholly illustrative intake without claiming measured field accuracy.

All dimensions, rates and samples below are chosen example inputs, not field
measurements or design recommendations. Hand answer: three synthetic cards,
zero real cards, zero eligible wells, and no evaluated real-card accuracy.
Run from the repository root with PYTHONPATH=src and the package dependencies.
"""

import copy
import json

from digitalmodel.marine_ops.artificial_lift.dynacard.intake import (
    prepare_intake,
    read_card_csv,
)


def run_example() -> dict:
    """Project samples, count coverage, and demonstrate the field-data gate."""
    samples = read_card_csv("time_s,position_in,load_lbf\n0,0,0\n1,1,0\n2,1,1\n3,0,1\n")
    base = {
        "well_id": "example-well-a",
        "card_id": "example-before",
        "kind": "synthetic",
        "acquisition": "generated",
        "card_space": "pump",
        **samples,
        "relative_day": 0,
        "stage": "before",
        "event_id": "example-change",
        "context": {
            "rod_string": [{"diameter_in": 1, "length_ft": 10}],
            "pump_diameter_in": 1,
            "pump_setting_depth_ft": 10,
            "stroke_in": 1,
            "spm": 1,
            "production_bpd": 0,
        },
        "label": {
            "mode": "FLUID_POUND",
            "basis": "workover_report",
            "evidence_ref": "example-outcome",
            "reviewer_ref": "example-review",
            "reviewed_on": "2026-10-19",
        },
        "rights": {
            "analysis": True,
            "publication": True,
            "evidence_ref": "example-permission",
        },
        "original_retained": True,
    }
    after = copy.deepcopy(base)
    after.update(
        card_id="example-after", stage="after", relative_day=1, load=[0, 0, 2, 2]
    )
    after["label"]["mode"] = "NORMAL"
    other = copy.deepcopy(base)
    other.update(
        well_id="example-well-b",
        card_id="example-other",
        stage="standalone",
        load=[0, 0, 3, 3],
    )
    # This public example key is deliberately non-secret. Real intake requires
    # a custodian-supplied private, unpredictable key retained outside releases.
    prepared = prepare_intake([base, after, other], alias_key=b"public-example-only")
    try:
        prepared.benchmark_set(prepared.card_ids())
    except ValueError:
        rejected = True
    else:
        rejected = False
    return {
        "summary": prepared.summary(),
        "projected_cards": len(prepared.publication_document()["cards"]),
        "synthetic_benchmark_rejected": rejected,
    }


if __name__ == "__main__":
    print(json.dumps(run_example(), indent=2))
