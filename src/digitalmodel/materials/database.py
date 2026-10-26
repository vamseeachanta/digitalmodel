# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Indexed snapshot of caller-supplied grades and their source metadata.

The digest covers the input data and its adjacent sources.yml, not software
text. No historical software revision is presented as a material data source.
"""
from __future__ import annotations

from collections.abc import Iterable, Iterator, Mapping
from hashlib import sha256
from types import MappingProxyType

from . import grades
from .catalog import Catalog
from .database_schema import MaterialRecord, RepositoryProvenance, Strengths

_LINK_FIELDS = ("sn_curve_ids", "standards_registry_id", "structural_shape_id")


class MaterialDatabase(Mapping[str, MaterialRecord]):
    """Read-only stable-key index with a grades.py-compatible convenience query.

    Direct db[record.key] lookup uses a dictionary, independent of grade scans.
    This slice accepts only null foreign keys: the inspected grade source
    establishes neither grade-to-detail associations nor registry/shape links.
    """

    def __init__(
        self, records: Iterable[MaterialRecord], *, source_digest_matches: bool = False,
        grade_rows: tuple[grades.MaterialGrade, ...] = ()
    ) -> None:
        indexed: dict[str, MaterialRecord] = {}
        by_grade: dict[tuple[str, str | None], MaterialRecord] = {}
        for record in records:
            if not isinstance(record, MaterialRecord):
                raise ValueError("database entries must be MaterialRecord objects")
            if record.key in indexed:
                raise ValueError(f"duplicate material key: {record.key}")
            for field in _LINK_FIELDS:
                if getattr(record, field) is not None:
                    raise ValueError(f"{field}: no grade association is established in this slice")
            identity = (record.designation, record.product_level)
            if identity in by_grade:
                raise ValueError(f"ambiguous grade lookup: {identity}")
            indexed[record.key] = record
            by_grade[identity] = record
        self._grade_rows = grade_rows
        self._source_digest_matches = source_digest_matches
        self._records = MappingProxyType(indexed)
        self._by_grade = MappingProxyType(by_grade)

    @property
    def source_digest_matches(self) -> bool:
        """Whether records were projected from the validated input snapshot."""
        return self._source_digest_matches

    def __getitem__(self, key: str) -> MaterialRecord:
        return self._records[key]

    def __iter__(self) -> Iterator[str]:
        return iter(self._records)

    def __len__(self) -> int:
        return len(self._records)

    def lookup(self, name: str, *, psl: str = "PSL2") -> MaterialRecord:
        """Match grades.smts_mpa, rejecting unsupported API product levels."""
        key = name.strip().casefold()
        grade = next((g for g in self._grade_rows
                      if key in (g.name.casefold(), *(a.casefold() for a in g.aliases))), None)
        if grade is None:
            raise KeyError(f"Unknown material grade: {name!r}")
        level = None
        if grade.standard == "API 5L":
            level = psl.upper()
            if level not in ("PSL1", "PSL2"):
                raise ValueError("PSL must be PSL1 or PSL2")
            strength = grade.smts_psl1_mpa if level == "PSL1" else grade.smts_mpa
            if strength is None:
                raise ValueError(f"{grade.name}: {level} strength was not supplied")
        return self._by_grade[(grade.name, level)]


def _record(grade: grades.MaterialGrade, level: str | None, catalog: Catalog) -> MaterialRecord:
    tensile = grade.smts_psl1_mpa if level == "PSL1" else grade.smts_mpa
    if tensile is None:
        raise ValueError("the requested tensile strength was not supplied")
    return MaterialRecord(
        designation=grade.name,
        specification=grade.standard,
        edition=grade.specification_edition,
        product_level=level,
        strengths=Strengths(
            smys_mpa=grade.smys_mpa,
            smts_mpa=tensile,
        ),
        provenance=RepositoryProvenance(
            citation=grade.source.citation,
            repository_path=catalog.locator,
            repository_revision=None,
            sha256=catalog.digest,
            retrieved_on_utc=grade.source.date,
            original_source_terms="caller-supplied; qualification remains required",
        ),
        notes=grade.notes,
    )


def normalised_source_digest(source: bytes) -> str:
    """Hash UTF-8-decoded text after replacing CRLF and lone CR with LF."""
    text = source.decode("utf-8").replace("\r\n", "\n").replace("\r", "\n")
    return sha256(text.encode("utf-8")).hexdigest()


def load_database() -> MaterialDatabase:
    """Snapshot the active caller catalogue, retaining source identities."""
    from .catalog import current_catalog

    catalog = current_catalog()
    records = []
    for grade in catalog.grades:
        if grade.standard == "API 5L":
            levels = tuple(level for level, strength in (
                ("PSL1", grade.smts_psl1_mpa), ("PSL2", grade.smts_mpa)
            ) if strength is not None)
        else:
            levels = (None,)
        records.extend(_record(grade, level, catalog) for level in levels)
    return MaterialDatabase(records, source_digest_matches=True, grade_rows=catalog.grades)
