# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Provenance-aware pump cards and an explicit labelled-set crosswalk.

The loader accepts already authorised, labelled pump cards in inches and lbf.
It neither downloads data nor decides whether the supplied licence grants use.
Surface cards require inversion before loading. External labels are mapped only
by the supplied table; ambiguous and absent mappings remain unscored.
"""

from __future__ import annotations

import json
import math
from dataclasses import dataclass, field
from datetime import date as calendar_date
from pathlib import Path

from ..card_generators import ALL_GENERATORS, SYNTHETIC_LABEL_EXCLUSIONS
from ..models import CardData

_HOLDOUT_SEED_FLOOR = 1000


@dataclass(frozen=True)
class LabellingProvenance:
    """Required evidence for a label; kind separates real and synthetic cards."""

    source: str
    labeller: str
    date: str
    licence: str
    kind: str
    acquisition: str
    publication: str | None = None
    figure_or_page: str | None = None
    digitising_method: str | None = None

    def __post_init__(self) -> None:
        """Reject incomplete provenance rather than inventing it."""
        for value in (self.source, self.labeller, self.date, self.licence):
            if not isinstance(value, str) or not value.strip():
                raise ValueError("Provenance fields must be nonempty strings")
        calendar_date.fromisoformat(self.date)
        if self.kind not in {"real", "synthetic", "plate"}:
            raise ValueError("Provenance kind must be real, synthetic or plate")
        if self.acquisition not in {"recorded", "digitised_graphic", "generated"}:
            raise ValueError("Acquisition must be recorded, digitised_graphic or generated")
        if self.kind == "real" and self.acquisition == "generated":
            raise ValueError("Real-card acquisition cannot be generated")
        if self.acquisition == "digitised_graphic":
            for value in (self.publication, self.figure_or_page, self.digitising_method):
                if not isinstance(value, str) or not value.strip():
                    raise ValueError(
                        "Digitised graphics require a publication or URL, figure or page, "
                        "and digitising method"
                    )


def validate_card(card: CardData) -> None:
    """Require paired finite samples, preserving their traversal order."""
    if len(card.position) != len(card.load) or len(card.position) < 3:
        raise ValueError("A card requires at least three paired samples")
    if not all(math.isfinite(v) for v in (*card.position, *card.load)):
        raise ValueError("Card samples must be finite")


@dataclass
class LabelledCard:
    """A pump card with a current mode and explicit label provenance."""

    card: CardData
    label: str
    provenance: LabellingProvenance
    seed: int | None = None
    external_label: str | None = None

    def __post_init__(self) -> None:
        """Validate both label and numerical evidence."""
        if self.label not in ALL_GENERATORS:
            raise ValueError("Label is not a current diagnostic mode")
        if not isinstance(self.provenance, LabellingProvenance):
            raise TypeError("LabellingProvenance is required")
        validate_card(self.card)


@dataclass
class LabelledSet:
    """Mapped cards and a table retaining all unmapped external labels."""

    cards: list[LabelledCard]
    crosswalk: list[dict]
    unmapped_labels: dict[str, int] = field(default_factory=dict)
    card_space: str = "pump"
    units: dict[str, str] = field(default_factory=lambda: {"position": "in", "load": "lbf"})


def load_labelled_set(document: dict | str | Path) -> LabelledSet:
    """Load a JSON document or in-memory mapping, without implicit aliases.

    Required keys are card_space, units, crosswalk and cards. Each crosswalk
    row supplies external_label, mode (or null) and reason. Each card supplies
    position, load, label and provenance (source, labeller, date, licence,
    kind, acquisition). Digitised graphics additionally require publication
    (citation or URL), figure_or_page and digitising_method. All records are
    validated, including those with unmapped labels.
    """
    if isinstance(document, (str, Path)):
        document = json.loads(Path(document).read_text(encoding="utf-8"))
    if document.get("card_space") != "pump":
        raise ValueError("Only pump cards can be classified by this loader")
    if document.get("units") != {"position": "in", "load": "lbf"}:
        raise ValueError("Explicit position=in and load=lbf units are required")
    rows = document.get("crosswalk")
    records = document.get("cards")
    if not isinstance(rows, list) or not isinstance(records, list):
        raise ValueError("Crosswalk and cards must be lists")
    mapping = {}
    for row in rows:
        if "mode" not in row:
            raise ValueError("Crosswalk rows require an explicit mode or null")
        external, mode = row.get("external_label"), row.get("mode")
        if not isinstance(external, str) or not external.strip() or external in mapping:
            raise ValueError("External labels must be nonempty and unique")
        if mode is not None and mode not in ALL_GENERATORS:
            raise ValueError("Crosswalk mode is not a current diagnostic mode")
        if not isinstance(row.get("reason"), str) or not row["reason"].strip():
            raise ValueError("Each crosswalk row requires a mapping reason")
        mapping[external] = mode
    cards, unmapped = [], {}
    for record in records:
        try:
            provenance = LabellingProvenance(**record["provenance"])
            card = CardData(position=record["position"], load=record["load"])
            external = record["label"]
        except KeyError as exc:
            raise ValueError("Incomplete labelled-card record") from exc
        validate_card(card)
        if not isinstance(external, str) or not external.strip():
            raise ValueError("External card label must be nonempty")
        mode = mapping.get(external)
        if mode is None:
            unmapped[external] = unmapped.get(external, 0) + 1
        else:
            cards.append(LabelledCard(card, mode, provenance, external_label=external))
    return LabelledSet(cards=cards, crosswalk=[dict(row) for row in rows], unmapped_labels=unmapped)


def build_hold_out_test_set(
    samples_per_mode: int = 3,
    base_seed: int = _HOLDOUT_SEED_FLOOR,
) -> list[LabelledCard]:
    """Generate balanced synthetic pump cards with explicit generator labels.

    PLUNGER_UNDERTRAVEL is excluded because its defaults duplicate NORMAL;
    see SYNTHETIC_LABEL_EXCLUSIONS for the condition for restoring the label.

    The seed floor preserves the original builder convention, not a proof of
    independence from an arbitrary training run. The caller must compare the
    seeds with that run's recorded training seeds before claiming a hold-out.
    """
    if not isinstance(samples_per_mode, int) or samples_per_mode < 1:
        raise ValueError("samples_per_mode must be a positive integer")
    if not isinstance(base_seed, int) or base_seed < _HOLDOUT_SEED_FLOOR:
        raise ValueError("base_seed must be an integer >= 1000")
    provenance = LabellingProvenance(
        source="repository card_generators", labeller="generator mode key",
        date="2026-10-14", licence="MIT", kind="synthetic", acquisition="generated",
    )
    return [
        LabelledCard(generator(seed=seed), mode, provenance, seed=seed)
        for mode, generator in ALL_GENERATORS.items()
        if mode not in SYNTHETIC_LABEL_EXCLUSIONS
        for seed in range(base_seed, base_seed + samples_per_mode)
    ]
