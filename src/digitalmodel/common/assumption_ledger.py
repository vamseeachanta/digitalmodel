"""Assumption provenance ledger for inverse spec resolution.

Domain-neutral: shared by the diffraction resolver (OrcaWave/AQWA) and the
OrcaFlex inverse resolver. Records every value an inverse resolver *assumed*
(rather than received) so it can be surfaced for review -- the "no silent
assumptions" contract (digitalmodel#622).
"""

from __future__ import annotations

import json
from enum import Enum
from typing import Any, Iterator, Optional

from pydantic import BaseModel, Field


class AssumptionSource(str, Enum):
    """Source category for a resolved input value."""

    USER_SUPPLIED = "user_supplied"
    ASSUMED_DEFAULT = "assumed_default"
    ESTIMATED_FROM_DATA = "estimated_from_data"
    DATABASE_LOOKUP = "database_lookup"


class Confidence(str, Enum):
    """Confidence in an assumed value."""

    HIGH = "high"
    MEDIUM = "medium"
    LOW = "low"


class AssumptionRecord(BaseModel):
    """One non-user-supplied value used to complete a spec."""

    field: str
    value: Any
    source: AssumptionSource
    basis: str
    reference: Optional[str] = None
    confidence: Confidence
    impact: int = Field(default=0)


class AssumptionLedger:
    """Collection of non-user-supplied assumption records.

    User-supplied values are intentionally not stored. A fully detailed
    resolver run therefore produces a literally empty ledger.
    """

    def __init__(self, records: list[AssumptionRecord] | None = None) -> None:
        self._records = list(records or [])

    def record(
        self,
        field: str,
        value: Any,
        source: AssumptionSource,
        basis: str,
        confidence: Confidence,
        *,
        reference: str | None = None,
        impact: int = 0,
    ) -> None:
        """Store a non-user-supplied assumption record."""
        if source == AssumptionSource.USER_SUPPLIED:
            return
        self._records.append(
            AssumptionRecord(
                field=field,
                value=value,
                source=source,
                basis=basis,
                reference=reference,
                confidence=confidence,
                impact=impact,
            )
        )

    @property
    def records(self) -> list[AssumptionRecord]:
        """Return records in insertion order."""
        return list(self._records)

    def rows(self) -> list[AssumptionRecord]:
        """Return records sorted for human review by impact then confidence."""
        confidence_rank = {
            Confidence.LOW: 0,
            Confidence.MEDIUM: 1,
            Confidence.HIGH: 2,
        }
        return sorted(
            self._records,
            key=lambda r: (-r.impact, confidence_rank[r.confidence], r.field),
        )

    def to_dict(self) -> dict[str, Any]:
        """Serialise to a machine-readable dictionary."""
        return {"records": [record.model_dump(mode="json") for record in self._records]}

    def to_json(self) -> str:
        """Serialise to JSON."""
        return json.dumps(self.to_dict(), sort_keys=True)

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "AssumptionLedger":
        """Rebuild a ledger from ``to_dict`` output."""
        return cls(
            [AssumptionRecord.model_validate(item) for item in data.get("records", [])]
        )

    @classmethod
    def from_json(cls, data: str) -> "AssumptionLedger":
        """Rebuild a ledger from ``to_json`` output."""
        return cls.from_dict(json.loads(data))

    def __len__(self) -> int:
        return len(self._records)

    def __bool__(self) -> bool:
        return bool(self._records)

    def __iter__(self) -> Iterator[AssumptionRecord]:
        return iter(self._records)


__all__ = [
    "AssumptionLedger",
    "AssumptionRecord",
    "AssumptionSource",
    "Confidence",
]
