# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Read pipe-delimited controller surface-card exports without inferring units.

Schema source: the 2020 exports referenced by issue #1873, inspected 2026-10-19;
see docs/domains/artificial_lift/scada-export.md for revision and rights notes.
The correction on #1873 (2026-07-28) rules out independent solver validation.
No restricted data, rod properties, physical coefficients or tolerances are used.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import sys
from collections import Counter
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import TYPE_CHECKING, Iterable, Sequence, TextIO

if TYPE_CHECKING:
    from .models import CardData

_REQUIRED = {"Card Time", "Card Type", "Surface Positions", "Surface Loads"}


@dataclass(frozen=True)
class ScadaSurfaceCard:
    """One ordered surface trace in undeclared export units.

    Card type is acquisition metadata, never a fault label. The source timestamp
    is retained verbatim, including its precision and timezone. Downhole arrays
    are assessed but never promoted to a reference card.
    """

    timestamp: str
    card_type: str
    position: tuple[float, ...]
    load: tuple[float, ...]
    stroke_period: float | None
    spm: float | None
    stroke_length: float | None
    downhole_status: str

    @property
    def surface_stroke(self) -> float:
        """Return max(position) - min(position), a descriptive range."""
        # Definition of range; no engineering coefficient or acceptance bound.
        return max(self.position) - min(self.position)

    @property
    def load_range(self) -> float:
        """Return max(load) - min(load) in the original export units."""
        return max(self.load) - min(self.load)

    @property
    def stroke_ratio(self) -> float | None:
        """Return observed surface span / declared surface stroke length.

        This is not a downhole/surface stroke ratio. No ratio is reported when
        the optional header stroke length is absent.
        """
        return None if self.stroke_length is None else (
            self.surface_stroke / self.stroke_length
        )

    def to_surface_card(self, *, position_unit: str, load_unit: str) -> CardData:
        """Adapt only after the caller establishes inches and pounds-force.

        These are the existing dynacard model's native units (models.py,
        RodSection and PumpProperties). This method performs no conversion,
        supplies no rod/pump configuration and invokes no solver.
        """
        if (position_unit, load_unit) != ("in", "lbf"):
            raise ValueError("Explicit native units required: in and lbf")
        from .models import CardData

        return CardData(
            position=list(self.position), load=list(self.load),
            timestamp=self.timestamp,
        )


def _numbers(text: str, field: str) -> tuple[float, ...]:
    """Read comma-separated finite samples; never fill missing samples."""
    try:
        values = tuple(float(part) for part in text.split(","))
    except ValueError:
        raise ValueError(f"{field}: expected comma-separated numbers") from None
    if not all(math.isfinite(value) for value in values):
        raise ValueError(f"{field}: expected finite samples")
    return values


def _pair(position: str, load: str) -> tuple[tuple[float, ...], tuple[float, ...]]:
    x, y = _numbers(position, "positions"), _numbers(load, "loads")
    # Structural minimum: a trace needs at least a pair of samples to span a
    # range. This is not an engineering-quality or closed-loop acceptance test.
    if len(x) != len(y) or len(x) < 2:
        raise ValueError("surface or downhole trace: unequal or insufficient samples")
    return x, y


def _downhole_status(
    position: str, load: str,
    surface_position: tuple[float, ...], surface_load: tuple[float, ...],
) -> str:
    if not position.strip() and not load.strip():
        return "MISSING"
    try:
        x, y = _pair(position, load)
    except ValueError:
        return "INVALID"
    # Exact numeric equality also catches copies with different text formatting.
    if x == y:
        return "COPIED"
    if x == surface_position and y == surface_load:
        return "SURFACE_COPY"
    return "UNVERIFIED"


def _positive_optional(row: dict[str, str], field: str) -> float | None:
    text = row.get(field, "").strip()
    if not text:
        return None
    try:
        value = float(text)
    except ValueError:
        raise ValueError(f"{field}: expected a positive finite number") from None
    # Positive period, rate and declared stroke are their mathematical domain,
    # not field acceptance limits. No consistency tolerance is assumed.
    if not math.isfinite(value) or value <= 0:
        raise ValueError(f"{field}: expected a positive finite number")
    return value


def _read_row(row: dict[str, str]) -> ScadaSurfaceCard:
    timestamp = row["Card Time"].strip()
    try:
        parsed = datetime.fromisoformat(timestamp)
    except ValueError:
        raise ValueError("Card Time: expected ISO timestamp with timezone") from None
    if parsed.tzinfo is None:
        raise ValueError("Card Time: timezone required")
    card_type = row["Card Type"].strip()
    if not card_type:
        raise ValueError("Card Type: acquisition type required")
    x, y = _pair(row["Surface Positions"], row["Surface Loads"])
    card = ScadaSurfaceCard(
        timestamp=timestamp, card_type=card_type, position=x, load=y,
        stroke_period=_positive_optional(row, "Stroke Period"),
        spm=_positive_optional(row, "SPM"),
        stroke_length=_positive_optional(row, "Stroke Length"),
        downhole_status=_downhole_status(
            row.get("Downhole Positions", ""), row.get("Downhole Loads", ""),
            x, y,
        ),
    )
    metrics = (card.surface_stroke, card.load_range, card.stroke_ratio)
    if any(value is not None and not math.isfinite(value) for value in metrics):
        raise ValueError("surface metrics: numeric range exceeded")
    return card


def read_scada_export(stream: TextIO) -> list[ScadaSurfaceCard]:
    """Read a text stream, rejecting malformed headers, rows or surface traces.

    Unknown columns are ignored. Sample order, repeated points and acquisition
    order are preserved; there is no closure, sorting, deduplication, smoothing
    or resampling. Error messages count records (header = 1), including quoted
    multiline cells, and contain no source values.
    """
    reader = csv.DictReader(stream, delimiter="|", strict=True)
    index = 0
    try:
        fields = reader.fieldnames
        if (not fields or len(set(fields)) != len(fields)
                or not _REQUIRED.issubset(fields)):
            raise ValueError("export header: missing or duplicate required columns")
        cards = []
        index = 1
        for index, row in enumerate(reader, start=2):
            try:
                if None in row or any(value is None for value in row.values()):
                    raise ValueError("column count differs from header")
                cards.append(_read_row(row))
            except ValueError as error:
                raise ValueError(f"row {index}: {error}") from None
        return cards
    except csv.Error:
        raise ValueError(
            f"row {index + 1}: malformed delimited text"
        ) from None


def load_scada_export(path: str | Path) -> list[ScadaSurfaceCard]:
    """Load a UTF-8 export, allowing a byte-order mark."""
    with Path(path).open(encoding="utf-8-sig", newline="") as stream:
        return read_scada_export(stream)


def summarize_cards(cards: Sequence[ScadaSurfaceCard]) -> dict:
    """Inventory each card and retain distributions instead of a mean alone.

    Ranges and ratios are descriptive arithmetic, with no acceptance limits.
    Units, independent inversion and diagnoses cannot be established here.
    """
    records = [
        {
            "point_count": len(card.position),
            "surface_stroke": card.surface_stroke,
            "surface_load_range": card.load_range,
            "surface_to_declared_stroke_ratio": card.stroke_ratio,
            "downhole_status": card.downhole_status,
        }
        for card in cards
    ]

    def distribution(values: Iterable[float | None]) -> dict:
        present = [value for value in values if value is not None]
        return {
            "values": present, "min": min(present) if present else None,
            "max": max(present) if present else None,
        }

    return {
        "card_count": len(cards), "units": "UNDECLARED",
        "solver_validation": "NOT_EVALUATED",
        "solver_validation_reason": (
            "No independently verified paired downhole loads or complete "
            "rod/pump configuration; acquisition type is not a diagnosis."
        ),
        "card_type_counts": dict(Counter(card.card_type for card in cards)),
        "point_count_counts": dict(Counter(len(card.position) for card in cards)),
        "downhole_status_counts": dict(Counter(card.downhole_status for card in cards)),
        "cards": records,
        "distributions": {
            "surface_stroke": distribution(card.surface_stroke for card in cards),
            "surface_load_range": distribution(card.load_range for card in cards),
            "stroke_ratio": distribution(card.stroke_ratio for card in cards),
        },
    }


def main(argv: Sequence[str] | None = None) -> int:
    """Print an inventory of local exports, without downloading source data."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("exports", nargs="+", type=Path)
    args = parser.parse_args(argv)
    try:
        cards = [card for path in args.exports for card in load_scada_export(path)]
    except (OSError, UnicodeError) as error:
        # Do not echo file paths or source content in command-line failures.
        print(f"Cannot read export: {type(error).__name__}", file=sys.stderr)
        return 2
    except ValueError as error:
        print(f"Cannot read export: {error}", file=sys.stderr)
        return 2
    print(json.dumps(summarize_cards(cards), indent=2, allow_nan=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
