# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Capture offline evidence for an event supplied by the caller.

Supply an oldest-first sequence of ``(identifier, CardData)`` pairs and a
positive retention count. Card and event timestamps must be ISO extended
timestamps with seconds and a Z or hours/minutes offset. Fractional seconds are
compared exactly, including precision finer than a microsecond. Identifiers
must be unique and card times strictly increasing;
ties are rejected rather than choosing which tied card survives retention.
The latest eligible cards are retained in their original order, including a
card exactly at the event. Empty eligible histories produce empty evidence.

Arrays become tuples and optional JSON metadata becomes recursively immutable.
Use ``to_dict`` for a detached JSON-compatible document, or ``to_json`` and
``AlarmEvidence.from_json`` for storage and validated restoration. Metadata
accepts string-keyed objects, lists/tuples and finite JSON scalar values.
No file is written and no alarm condition or equipment action is inferred.
"""

from __future__ import annotations

import json
import math
import re
from collections.abc import Mapping, Sequence
from dataclasses import dataclass, field
from datetime import datetime
from fractions import Fraction
from types import MappingProxyType
from typing import Any

from .models import CardData


# Field widths are ISO extended calendar/time syntax, not engineering limits.
_TIME_FORMAT = re.compile(
    r"(?P<whole>\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2})"
    r"(?:[.,](?P<fraction>\d+))?(?P<offset>Z|[+-](?:[01]\d|2[0-3]):[0-5]\d)"
)
_Instant = tuple[datetime, Fraction]


def _text(value: str, name: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} must be nonblank text")


def _timestamp(value: str, name: str) -> _Instant:
    _text(value, name)
    match = _TIME_FORMAT.fullmatch(value)
    if match is None:
        raise ValueError(f"{name} must be an offset-aware ISO timestamp with seconds")
    try:
        instant = datetime.fromisoformat(match["whole"] + match["offset"])
    except ValueError as exc:
        raise ValueError(f"{name} must be an offset-aware ISO timestamp") from exc
    if instant.utcoffset() is None:
        raise ValueError(f"{name} must be an offset-aware ISO timestamp")
    # Preserve decimal fractions exactly; datetime truncates beyond microseconds.
    return instant, Fraction("0." + (match["fraction"] or "0"))


def _retention(value: int) -> None:
    # A count must be a positive integer; booleans are not retention choices.
    if isinstance(value, bool) or not isinstance(value, int) or value <= 0:
        raise ValueError("retention_count must be a positive integer")


def _freeze(value: Any) -> Any:
    if isinstance(value, Mapping):
        if any(not isinstance(key, str) for key in value):
            raise ValueError("metadata object keys must be strings")
        return MappingProxyType({key: _freeze(item) for key, item in value.items()})
    if isinstance(value, (list, tuple)):
        return tuple(_freeze(item) for item in value)
    if value is None or type(value) in (str, bool, int):
        return value
    if type(value) is float and math.isfinite(value):
        return value
    raise ValueError("metadata must contain only finite JSON values")


def _thaw(value: Any) -> Any:
    if isinstance(value, Mapping):
        return {key: _thaw(item) for key, item in value.items()}
    if isinstance(value, tuple):
        return [_thaw(item) for item in value]
    return value


@dataclass(frozen=True, slots=True)
class EvidenceCard:
    """An identified card with detached, immutable trace arrays."""

    card_id: str
    timestamp: str
    position: tuple[float, ...]
    load: tuple[float, ...]

    def __post_init__(self) -> None:
        """Validate structural consistency without applying engineering limits."""
        _text(self.card_id, "card_id")
        _timestamp(self.timestamp, "card timestamp")
        for name in ("position", "load"):
            trace = getattr(self, name)
            if not isinstance(trace, (list, tuple)) or any(
                type(value) not in (int, float) for value in trace
            ):
                raise ValueError(f"{name} must be a finite numeric array")
            try:
                values = tuple(float(value) for value in trace)
            except (TypeError, ValueError, OverflowError) as exc:
                raise ValueError(f"{name} must be a finite numeric array") from exc
            if not all(math.isfinite(value) for value in values):
                raise ValueError(f"{name} must be a finite numeric array")
            object.__setattr__(self, name, values)
        if not self.position or len(self.position) != len(self.load):
            raise ValueError("card arrays must be nonempty and equal in length")

    def to_dict(self) -> dict[str, Any]:
        """Return a detached document in the source array order."""
        return {
            "card_id": self.card_id,
            "timestamp": self.timestamp,
            "position": list(self.position),
            "load": list(self.load),
        }


def _ordered_times(cards: tuple[EvidenceCard, ...]) -> list[_Instant]:
    identifiers: set[str] = set()
    times: list[_Instant] = []
    for card in cards:
        if not isinstance(card, EvidenceCard):
            raise ValueError("cards must contain EvidenceCard snapshots")
        if card.card_id in identifiers:
            raise ValueError("card identifiers must be unique")
        identifiers.add(card.card_id)
        instant = _timestamp(card.timestamp, "card timestamp")
        if times and instant <= times[-1]:
            raise ValueError("card order must have strictly increasing timestamps")
        times.append(instant)
    return times


@dataclass(frozen=True, slots=True)
class AlarmEvidence:
    """Immutable event envelope; serialization returns detached JSON values."""

    event_timestamp: str
    reason: str
    retention_count: int
    cards: tuple[EvidenceCard, ...]
    metadata: Mapping[str, Any] = field(default_factory=dict)

    def __post_init__(self) -> None:
        """Enforce the same evidence invariants on construction and restore."""
        _retention(self.retention_count)
        event = _timestamp(self.event_timestamp, "event_timestamp")
        _text(self.reason, "reason")
        if not isinstance(self.cards, Sequence):
            raise ValueError("cards must be an ordered sequence")
        snapshots = tuple(self.cards)
        times = _ordered_times(snapshots)
        if len(snapshots) > self.retention_count:
            raise ValueError("evidence exceeds retention_count")
        if any(instant > event for instant in times):
            raise ValueError("evidence contains a card after the event")
        if not isinstance(self.metadata, Mapping):
            raise ValueError("metadata must be a JSON object")
        try:
            metadata = _freeze(self.metadata)
        except RecursionError as exc:
            raise ValueError("metadata must be an acyclic JSON object") from exc
        object.__setattr__(self, "cards", snapshots)
        object.__setattr__(self, "metadata", metadata)

    def to_dict(self) -> dict[str, Any]:
        """Return a mutable copy suitable for JSON serialization."""
        return {
            "event_timestamp": self.event_timestamp,
            "reason": self.reason,
            "retention_count": self.retention_count,
            "cards": [card.to_dict() for card in self.cards],
            "metadata": _thaw(self.metadata),
        }

    def to_json(self) -> str:
        """Serialize the event, retention choice, ordered cards and metadata."""
        return json.dumps(self.to_dict(), allow_nan=False)

    @classmethod
    def from_json(cls, document: str) -> AlarmEvidence:
        """Restore immutable evidence, rejecting invalid event envelopes."""
        try:
            values = json.loads(document)
            cards = values.pop("cards")
            if not isinstance(cards, list) or any(
                not isinstance(card, dict) for card in cards
            ):
                raise ValueError("cards must be a JSON array of card objects")
            snapshots = tuple(EvidenceCard(**card) for card in cards)
            return cls(cards=snapshots, **values)
        except (AttributeError, KeyError, TypeError) as exc:
            raise ValueError("invalid alarm evidence document") from exc


def capture_alarm_evidence(
    cards: Sequence[tuple[str, CardData]],
    *,
    retention_count: int,
    event_timestamp: str,
    reason: str,
    metadata: Mapping[str, Any] | None = None,
) -> AlarmEvidence:
    """Retain the latest eligible cards from an explicit oldest-first history.

    The complete supplied history must have unique nonblank identifiers and
    strictly increasing offset-aware timestamps, including post-event cards.
    Input is validated without sorting or modifying it. Optional metadata is
    event-level context; no additional fields are inferred from ``CardData``.
    """
    _retention(retention_count)
    event = _timestamp(event_timestamp, "event_timestamp")
    _text(reason, "reason")
    if not isinstance(cards, Sequence) or isinstance(cards, (str, bytes)):
        raise ValueError("cards must be an ordered sequence of identifier/card pairs")
    snapshots = []
    for entry in cards:
        try:
            identifier, card = entry
        except (TypeError, ValueError) as exc:
            raise ValueError("each entry must be an identifier/CardData pair") from exc
        if not isinstance(card, CardData):
            raise ValueError("each entry must contain CardData")
        if card.timestamp is None:
            raise ValueError("card timestamp must be an offset-aware ISO timestamp")
        snapshots.append(EvidenceCard(identifier, card.timestamp, card.position, card.load))
    ordered = tuple(snapshots)
    times = _ordered_times(ordered)
    eligible = tuple(card for card, instant in zip(ordered, times) if instant <= event)
    return AlarmEvidence(
        event_timestamp=event_timestamp,
        reason=reason,
        retention_count=retention_count,
        cards=eligible[-retention_count:],
        metadata={} if metadata is None else metadata,
    )
