"""Immutable records and validation for the geotechnical parameter database."""

from dataclasses import dataclass, fields
from types import MappingProxyType
from typing import Mapping

LICENCES = frozenset({"BY", "NS", "CO"})
GRADES = frozenset({"A", "B", "C"})
REDISTRIBUTION = frozenset({"yes", "cite-only", "no", "not-stated"})


def required(entry: Mapping, names: tuple[str, ...]) -> None:
    """Reject absent, non-string or empty required cells without defaults."""
    for name in names:
        if not isinstance(entry.get(name), str) or not entry[name].strip():
            raise ValueError(f"required field {name!r} is missing or empty")


def freeze(value):
    """Detach nested containers from caller data and make them read-only."""
    if isinstance(value, Mapping):
        return MappingProxyType({key: freeze(item) for key, item in value.items()})
    if isinstance(value, (list, tuple)):
        return tuple(freeze(item) for item in value)
    return value


@dataclass(frozen=True)
class Source:
    source_id: str
    citation: str
    access: str
    reuse_terms: str
    redistribution: str
    read_status: str

    @classmethod
    def from_entry(cls, entry: Mapping) -> "Source":
        names = tuple(f.name for f in fields(cls))
        required(entry, names)
        if entry["redistribution"] not in REDISTRIBUTION:
            raise ValueError("invalid source redistribution status")
        return cls(**{name: entry[name] for name in names})


@dataclass(frozen=True)
class ParameterRow:
    row_id: str
    soil_unit: str
    parameter: str
    value_or_range: str
    units_as_printed: str
    test_or_derivation_method: str
    site_or_region: str
    depth_range: str
    source_id: str
    locator: str
    licence: str
    confidence_grade: str
    source: Source

    @classmethod
    def from_entry(cls, entry: Mapping, sources: Mapping) -> "ParameterRow":
        names = tuple(f.name for f in fields(cls) if f.name != "source")
        required(entry, names)
        if entry["licence"] not in LICENCES:
            raise ValueError("invalid licence code")
        if entry["confidence_grade"] not in GRADES:
            raise ValueError("invalid confidence grade")
        if entry["source_id"] not in sources:
            raise ValueError(f"unknown source {entry['source_id']!r}")
        source = sources[entry["source_id"]]
        if source.read_status != "read":
            raise ValueError(f"source {source.source_id!r} is not read")
        return cls(**{name: entry[name] for name in names}, source=source)


@dataclass(frozen=True)
class UnitValue:
    """A single value or bounds, always accompanied by its unit."""

    unit: str
    value: float | None = None
    minimum: float | None = None
    maximum: float | None = None


@dataclass(frozen=True)
class Quantity:
    """A printed quantity with provenance and explicit numeric availability."""

    row: ParameterRow
    name: str
    printed_value: str
    printed_units: str
    qualifier: str
    printed: UnitValue | None
    si: UnitValue | None
    unavailable_reason: str

    @property
    def parent_row_id(self) -> str:
        return self.row.row_id

    @property
    def source(self) -> Source:
        return self.row.source

    def in_printed_units(self) -> UnitValue:
        if self.printed is None:
            raise ValueError(f"numeric access unavailable: {self.unavailable_reason}")
        return self.printed

    def in_si(self) -> UnitValue:
        if self.si is None:
            raise ValueError(f"SI unavailable: {self.unavailable_reason}")
        return self.si
