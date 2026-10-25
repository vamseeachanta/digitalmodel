# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Local intake of a permitted three-column labelled pump-card export.

Source schema: open monograph (2025), section 3.1 and Appendix A; see
docs/domains/artificial_lift/labelled-field-dataset.md for source links.
No download, contact, licence inference, unit conversion or model fitting occurs.
Permission evidence is a caller attestation requiring review outside this code.
"""

from __future__ import annotations

import csv
import hashlib
import io
import json
import math
from dataclasses import dataclass
from pathlib import Path
from typing import Callable

from ..card_generators import ALL_GENERATORS
from ..models import CardData
from .measurements import real_accuracy, score_predictions
from .test_set_builder import LabellingProvenance, LabelledSet, load_labelled_set

PUBLICATION = "https://doi.org/10.3390/s21134546"

# Sensors (2021), 21, 4546, Table 1. These are printed inventory counts,
# never received-file counts, minimum sample sizes or accuracy evidence.
_INVENTORY = (
    ("Normal", 10282, "NORMAL", "Same named operating condition"),
    ("Gas interference", 260, "GAS_INTERFERENCE", "Same named operating condition"),
    ("Fluid pound", 38298, "FLUID_POUND", "Same named operating condition"),
    ("Traveling valve leak", 67, "VALVE_LEAK_TV", "Same valve and leakage mechanism"),
    ("Standing valve leak", 172, "VALVE_LEAK_SV", "Same valve and leakage mechanism"),
    ("Rod parted", 29, "ROD_PARTING", "Same named failure mechanism"),
    ("Gas lock", 6, "GAS_LOCK", "Same named operating condition"),
    (
        "Tubing anchor malfunction",
        30,
        None,
        "Anchor fault alone does not establish tubing movement",
    ),
    (
        "Sensor fault - rotated card",
        895,
        None,
        "Sensor faults are outside the pump-mode taxonomy",
    ),
    (
        "Sensor fault - line card",
        73,
        None,
        "Sensor faults are outside the pump-mode taxonomy",
    ),
)


@dataclass(frozen=True)
class DataPermission:
    """Attestation that written terms permit this benchmark and its intake.

    The evidence reference locates the privately retained agreement. This object
    is neither an agreement nor proof that the signatory can grant these rights.
    It does not grant training, model publication or raw-data redistribution.
    """

    written_agreement: bool
    benchmark_allowed: bool
    evidence_reference: str
    licence: str
    attribution: str

    def validate(self) -> None:
        """Require explicit permission and substantive evidence before file I/O."""
        if self.written_agreement is not True or self.benchmark_allowed is not True:
            raise ValueError("Written benchmark permission is required before intake")
        for value in (self.evidence_reference, self.licence, self.attribution):
            if not isinstance(value, str) or not value.strip():
                raise ValueError(
                    "Permission evidence, licence and attribution are required"
                )


@dataclass(kw_only=True)
class FieldDataset(LabelledSet):
    """Mapped cards with the intake's byte identity and privately held use terms."""

    source_sha256: str
    permission: DataPermission
    codebook_reference: str


def published_crosswalk() -> list[dict]:
    """Return a fresh proposed semantic crosswalk, without integer type guesses."""
    return [
        {
            "external_label": label,
            "published_count": count,
            "mode": mode,
            "reason": reason,
            "source": PUBLICATION,
            "source_location": "Table 1",
        }
        for label, count, mode, reason in _INVENTORY
    ]


def acquisition_status() -> dict:
    """Distinguish the publication inventory from a still-unreceived dataset."""
    mapped = {row["mode"] for row in published_crosswalk() if row["mode"]}
    return {
        "status": "NOT_EVALUATED",
        "reason": "No field dataset or agreed use terms supplied",
        "received_count": 0,
        "published_table_total": sum(row[1] for row in _INVENTORY),
        "published_well_count": 38,  # Sensors (2021), section 1.
        "crosswalk": published_crosswalk(),
        "unmapped_modes": sorted(set(ALL_GENERATORS) - mapped),
    }


def _array(cell: str) -> list[float]:
    values = json.loads(cell)
    if not isinstance(values, list) or any(
        isinstance(value, bool)
        or not isinstance(value, (int, float))
        or not math.isfinite(value)
        for value in values
    ):
        raise ValueError("Array must contain finite JSON numbers")
    return values


def load_three_column_export(
    path: str | Path,
    *,
    expected_sha256: str,
    permission: DataPermission,
    provenance: LabellingProvenance,
    label_map: dict[str, str],
    codebook_reference: str,
    card_space: str,
    units: dict[str, str],
    columns: tuple[str, str, str] = ("cargas", "posicoes", "tipo"),
) -> FieldDataset:
    """Load supplier-confirmed bytes and explicit codes into the benchmark set.

    Arrays are JSON in CSV cells. Select columns in load/position/type order;
    defaults match the cited export: cargas/posicoes/tipo. Arbitrary CSV
    column order is supported. All other columns are rejected.
    Unknown type codes and unsupported published classes remain unscored.
    Physical axis units and pump-card space are required by the existing model.
    No sample is transformed, discarded silently or relabelled by its shape.

    Raw data and written permission evidence must remain outside public source
    control. Return provenance may be private; inspect reports before publishing.
    """
    if not isinstance(permission, DataPermission):
        raise ValueError("DataPermission is required before intake")
    permission.validate()
    if (
        not isinstance(provenance, LabellingProvenance)
        or provenance.kind != "real"
        or provenance.acquisition != "recorded"
        or provenance.licence != permission.licence
    ):
        raise ValueError(
            "Recorded real-card provenance must match the permission licence"
        )
    if card_space != "pump":
        raise ValueError(
            "Only pump cards are supported; surface inversion is NOT_EVALUATED"
        )
    if units != {"position": "in", "load": "lbf"}:
        raise ValueError(
            "Explicit physical units in/lbf required; normalized scoring is NOT_EVALUATED"
        )
    if not isinstance(codebook_reference, str) or not codebook_reference.strip():
        raise ValueError("An export-specific codebook reference is required")
    # The three fields follow monograph (2025), section 3.1, Appendix A.
    if (
        len(columns) != 3
        or any(not isinstance(c, str) or not c.strip() for c in columns)
        or len(set(columns)) != len(columns)
    ):
        raise ValueError("Select three unique columns in load/position/type order")
    by_label = {row["external_label"]: row for row in published_crosswalk()}
    if not isinstance(label_map, dict):
        raise ValueError("A string code dictionary is required")
    crosswalk = []
    for code, label in label_map.items():
        if not isinstance(code, str) or not code.strip():
            raise ValueError("Type codes must be nonempty strings")
        if not isinstance(label, str) or label not in by_label:
            raise ValueError("Code dictionary values must name a published class")
        row = by_label[label]
        crosswalk.append(
            {
                "external_label": code,
                "mode": row["mode"],
                "reason": f"{label}: {row['reason']}; codebook: {codebook_reference}",
            }
        )
    # SHA-256 hexadecimal width derives from the digest API, not a data limit.
    width = hashlib.sha256().digest_size * 2
    if (
        not isinstance(expected_sha256, str)
        or len(expected_sha256) != width
        or any(c not in "0123456789abcdef" for c in expected_sha256)
    ):
        raise ValueError(
            "Expected digest must be a lowercase SHA-256 hexadecimal string"
        )
    content = Path(path).read_bytes()
    if hashlib.sha256(content).hexdigest() != expected_sha256:
        raise ValueError("Import digest mismatch; no records loaded")
    try:
        reader = csv.DictReader(io.StringIO(content.decode("utf-8-sig")), strict=True)
        if (
            reader.fieldnames is None
            or len(reader.fieldnames) != len(columns)
            or set(reader.fieldnames) != set(columns)
        ):
            raise ValueError("Export must contain exactly the selected three columns")
        records = []
        for record_index, row in enumerate(reader, start=2):
            try:
                if None in row or any(row[c] is None for c in columns):
                    raise ValueError("Row width does not match columns")
                loads, positions, code = (row[c] for c in columns)
                record = {
                    "load": _array(loads),
                    "position": _array(positions),
                    "label": code,
                    "provenance": provenance.__dict__,
                }
                # Validate unmapped records too, using the existing card contract.
                load_labelled_set(
                    {
                        "card_space": card_space,
                        "units": units,
                        "crosswalk": crosswalk,
                        "cards": [record],
                    }
                )
                records.append(record)
            except (ValueError, TypeError, OverflowError, RecursionError):
                # Do not expose supplied card contents through exception chains.
                raise ValueError(
                    f"Invalid export row {record_index}; no records loaded"
                ) from None
    except (UnicodeDecodeError, csv.Error):
        raise ValueError("Invalid UTF-8 CSV export; no records loaded") from None
    dataset = load_labelled_set(
        {
            "card_space": card_space,
            "units": units,
            "crosswalk": crosswalk,
            "cards": records,
        }
    )
    return FieldDataset(
        cards=dataset.cards,
        crosswalk=dataset.crosswalk,
        unmapped_labels=dataset.unmapped_labels,
        card_space=card_space,
        units=dict(units),
        source_sha256=expected_sha256,
        permission=permission,
        codebook_reference=codebook_reference,
    )


def evaluate_export(
    dataset: FieldDataset | None,
    *,
    minimum_count: int,
    confidence: float,
    ml_classifier: Callable[[CardData], str] | None = None,
    heuristic_classifier: Callable[[CardData], str] | None = None,
) -> dict:
    """Measure supplied predictions; require explicit reporting policy and models.

    Per-class accuracy is recall (correct/actual-class count), as in monograph
    (2025), equation (2.8). Intervals reuse the existing score implementation;
    its statistical source is documented in measurements.py. No well-level
    independence, training separation or representativeness is established here.
    """
    score_predictions([], [], minimum_count=minimum_count, confidence=confidence)
    report = acquisition_status()
    if dataset is None:
        return report
    if not isinstance(dataset, FieldDataset):
        raise ValueError("A permission-gated FieldDataset is required")
    dataset.permission.validate()
    report.update(
        source_sha256=dataset.source_sha256,
        permission_reference=dataset.permission.evidence_reference,
        licence=dataset.permission.licence,
        attribution=dataset.permission.attribution,
        codebook_reference=dataset.codebook_reference,
    )
    report["received_count"] = len(dataset.cards) + sum(
        dataset.unmapped_labels.values()
    )
    report["unrepresented_modes"] = sorted(
        set(ALL_GENERATORS) - {c.label for c in dataset.cards}
    )
    report["unmapped_labels"] = dict(dataset.unmapped_labels)
    if ml_classifier is None or heuristic_classifier is None:
        report["reason"] = "Both numerical classifier callbacks are required"
        return report
    measured = real_accuracy(
        dataset,
        ml_classifier,
        heuristic_classifier,
        minimum_count=minimum_count,
        confidence=confidence,
    )
    report.update(measured)
    report["status"] = "EVALUATED" if dataset.cards else "NOT_EVALUATED"
    report["count"] = len(dataset.cards)
    report["published_crosswalk"] = published_crosswalk()
    report["reason"] = (
        "Supplied cards only; generalization and training independence are NOT_EVALUATED"
        if dataset.cards
        else "No mapped cards available to score"
    )
    return report
