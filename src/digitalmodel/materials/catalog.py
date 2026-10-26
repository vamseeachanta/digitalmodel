# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Validated caller-owned material data, with a source sidecar for every row.

No engineering rows are bundled. A catalogue is active only inside use_catalog;
ContextVar isolation prevents one concurrent calculation changing another's data.
"""
from __future__ import annotations

import json
import math
import re
from collections.abc import Callable, Iterator, Mapping
from contextlib import contextmanager
from contextvars import ContextVar
from dataclasses import dataclass
from datetime import date
from hashlib import sha256
from numbers import Real
from pathlib import Path
from types import MappingProxyType
from typing import Any, Generic, TypeVar, TYPE_CHECKING
from urllib.parse import urlparse


if TYPE_CHECKING:
    from .grades import MaterialGrade


def positive(value: object, field: str) -> None:
    """Reject nonnumeric, nonfinite and nonpositive engineering inputs."""
    if (isinstance(value, bool) or not isinstance(value, Real)
            or not math.isfinite(value) or value <= 0):
        raise ValueError(f"{field} must be a finite positive number")


def text(value: object, field: str) -> None:
    """Require trimmed identifiers and metadata."""
    if not isinstance(value, str) or not value.strip() or value != value.strip():
        raise ValueError(f"{field} must be a nonempty trimmed string")


def schedule_key(schedule: str) -> str:
    """Normalize label spelling without supplying a dimensional value."""
    text(schedule, "schedule")
    key = schedule.upper()
    if key.isdigit():
        return f"SCH {key}"
    if key.startswith("SCH") and key != "SCH" and " " not in key:
        return f"SCH {key[3:]}"
    return key


_STANDARD = re.compile(
    r"\b(?:API\s*(?:SPEC\s*)?5L|ASME\s*B36|ISO\s*3183|IACS\s*UR\s*W11|"
    r"EN\s*10025|ASTM\s*A(?:312|790))\b", re.IGNORECASE
)
_STANDARD_PUBLISHERS = re.compile(
    r"\b(?:API|ASME|ISO|IACS|ASTM|CEN|American Petroleum Institute|"
    r"American Society of Mechanical Engineers|International Organization for "
    r"Standardization)\b", re.IGNORECASE
)


@dataclass(frozen=True)
class Source:
    """Per-row nonstandard source identity; metadata is not qualification."""

    title: str
    publisher: str
    revision: str
    date: str
    kind: str
    url: str = ""
    catalogue_number: str = ""

    def __post_init__(self) -> None:
        for field in ("title", "publisher", "revision", "date", "kind"):
            text(getattr(self, field), field)
        if self.kind not in {"manufacturer", "public_paper", "design_basis", "synthetic"}:
            raise ValueError("source kind must identify a nonstandard source")
        # A manufacturer's catalogue or paper can name a product specification
        # in its exact title. The publisher and kind identify the actual source.
        if (_STANDARD_PUBLISHERS.search(self.publisher)
                or (_STANDARD.search(self.title)
                    and self.kind not in {"manufacturer", "public_paper"})):
            raise ValueError("standards-attributed source labels are prohibited")
        if not self.url and not self.catalogue_number:
            raise ValueError("source needs a URL or catalogue_number")
        if self.catalogue_number:
            text(self.catalogue_number, "catalogue_number")
            if _STANDARD.search(self.catalogue_number):
                raise ValueError("standards-attributed source labels are prohibited")
        if self.url:
            parsed = urlparse(self.url)
            if parsed.scheme not in ("http", "https") or not parsed.netloc:
                raise ValueError("source URL must be public HTTP or HTTPS")
        date.fromisoformat(self.date)

    @property
    def citation(self) -> str:
        return f"{self.title}; {self.publisher}; {self.revision}; {self.date}; {self.url or self.catalogue_number}"


@dataclass(frozen=True)
class Dimension:
    """Caller-supplied OD and optional schedule wall, all in inches."""

    row_id: str
    nps: float
    od_in: float
    wt_in: float
    schedule: str
    source: Source

    def __post_init__(self) -> None:
        for field in ("row_id", "schedule"):
            text(getattr(self, field), field)
        object.__setattr__(self, "schedule", schedule_key(self.schedule))
        for field in ("nps", "od_in", "wt_in"):
            positive(getattr(self, field), field)
        if self.wt_in >= self.od_in / 2:
            raise ValueError("wt_in must be less than od_in/2")
        if not isinstance(self.source, Source):
            raise ValueError("dimension source must be a validated Source")


@dataclass(frozen=True)
class Catalog:
    """Immutable validated inputs and digest of the supplied data and sidecar."""

    grades: tuple[MaterialGrade, ...]
    dimensions: tuple[Dimension, ...]
    digest: str
    locator: str

    def __post_init__(self) -> None:
        from .grades import MaterialGrade

        if not isinstance(self.grades, tuple) or not isinstance(self.dimensions, tuple):
            raise ValueError("catalogue rows must be immutable tuples")
        if not all(isinstance(g, MaterialGrade) for g in self.grades):
            raise ValueError("catalogue grades must be validated MaterialGrade records")
        if not all(isinstance(d, Dimension) for d in self.dimensions):
            raise ValueError("catalogue dimensions must be validated Dimension records")
        identities = [g.name.casefold() for g in self.grades]
        aliases = [alias.casefold() for g in self.grades for alias in (g.name, *g.aliases)]
        if len(identities) != len(set(identities)) or len(aliases) != len(set(aliases)):
            raise ValueError("duplicate or ambiguous grade identity")
        row_ids = [row.row_id for row in (*self.grades, *self.dimensions)]
        if len(row_ids) != len(set(row_ids)):
            raise ValueError("duplicate row identity")
        keys = [(d.nps, d.schedule.strip().upper()) for d in self.dimensions]
        if len(keys) != len(set(keys)):
            raise ValueError("duplicate dimension identity")
        ods = {}
        for row in self.dimensions:
            if row.nps in ods and ods[row.nps] != row.od_in:
                raise ValueError("inconsistent OD for NPS")
            ods[row.nps] = row.od_in
        if not re.fullmatch(r"[a-f0-9]{64}", self.digest):
            raise ValueError("catalogue digest must be SHA-256")
        text(self.locator, "locator")
        if Path(self.locator).is_absolute() or ".." in Path(self.locator).parts:
            raise ValueError("catalogue locator must be relative")

    @property
    def synthetic(self) -> bool:
        return any(row.source.kind == "synthetic" for row in (*self.grades, *self.dimensions))


_ACTIVE: ContextVar[Catalog | None] = ContextVar("material_catalog", default=None)


def current_catalog() -> Catalog:
    catalog = _ACTIVE.get()
    if catalog is None:
        raise ValueError("a validated caller-supplied material catalogue is required")
    return catalog


@contextmanager
def use_catalog(catalog: Catalog | None, *, allow_synthetic: bool = False) -> Iterator[Catalog | None]:
    """Scope inputs to a calculation; synthetic rows need explicit test opt-in."""
    if catalog is not None and not isinstance(catalog, Catalog):
        raise ValueError("catalog must be a validated Catalog")
    if catalog is not None and catalog.synthetic and not allow_synthetic:
        raise ValueError("synthetic catalogue is restricted to explicit test use")
    token = _ACTIVE.set(catalog)
    try:
        yield catalog
    finally:
        _ACTIVE.reset(token)


def _unique(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("duplicate data or source key")
        result[key] = value
    return result


def load_catalog(path: str | Path) -> Catalog:
    """Read JSON data and adjacent sources.yml; reject orphan/missing sources.

    sources.yml uses JSON syntax (a YAML subset) so loading is dependency-free
    and duplicate keys cannot silently overwrite row provenance.
    """
    from .grades import MaterialGrade

    path = Path(path)
    payload = path.read_bytes()
    sidecar = path.with_name("sources.yml").read_bytes()
    data = json.loads(payload, object_pairs_hook=_unique)
    sources = json.loads(sidecar, object_pairs_hook=_unique)
    if set(data) != {"grades", "dimensions"} or not isinstance(sources, dict):
        raise ValueError("catalogue needs grades, dimensions and row sources")
    seen = set()

    def source(row: dict[str, Any]) -> tuple[str, Source]:
        row_id = row.pop("row_id")
        text(row_id, "row_id")
        if row_id in seen or row_id not in sources:
            raise ValueError("duplicate row or missing row source")
        seen.add(row_id)
        return row_id, Source(**sources[row_id])

    grades: list[MaterialGrade] = []
    dimensions: list[Dimension] = []
    for raw in data["grades"]:
        row = dict(raw)
        row_id, provenance = source(row)
        row["aliases"] = tuple(row.get("aliases", ()))
        grades.append(MaterialGrade(**row, source=provenance, row_id=row_id))
    for raw in data["dimensions"]:
        row = dict(raw)
        row_id, provenance = source(row)
        dimensions.append(Dimension(**row, source=provenance, row_id=row_id))
    if seen != set(sources):
        raise ValueError("orphan row source")
    digest = sha256(payload + b"\0" + sidecar).hexdigest()
    return Catalog(tuple(grades), tuple(dimensions), digest, path.name)


K = TypeVar("K")
V = TypeVar("V")


class CatalogView(Mapping[K, V], Generic[K, V]):
    """Lazy read-only adapter: imports never capture a default material table."""

    def __init__(self, project: Callable[[Catalog], Mapping[K, V]]) -> None:
        self._project = project

    def __getitem__(self, key: K) -> V:
        return self._project(current_catalog())[key]

    def __iter__(self) -> Iterator[K]:
        return iter(self._project(current_catalog()))

    def __len__(self) -> int:
        return len(self._project(current_catalog()))

    def snapshot(self) -> Mapping[K, V]:
        return MappingProxyType(dict(self._project(current_catalog())))
