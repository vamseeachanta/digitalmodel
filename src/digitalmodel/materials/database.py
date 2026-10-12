"""Indexed projection of the existing reconciled grades, with pinned provenance.

No dimensional values, independent grade matrix or external source fetch is
introduced. Compatibility lookup delegates name handling to grades.get.
"""
from __future__ import annotations

import re
from collections.abc import Iterable, Iterator, Mapping
from hashlib import sha256
from pathlib import Path
from types import MappingProxyType

from . import grades
from .database_schema import MaterialRecord, RepositoryProvenance, Strengths

# Inspected source bytes, not a standards-derived numerical dataset.
_SOURCE_REVISION = "f21a12af5349c1656137a7b9daa091ec8f968738"
_SOURCE_SHA256 = "3cc745f599faefdb3c6b08218c383026445662796faa23fba0806691de4017ab"
_RETRIEVED_ON_UTC = "2026-10-11"
_SOURCE_PATH = "src/digitalmodel/materials/grades.py"
_LINK_FIELDS = ("sn_curve_ids", "standards_registry_id", "structural_shape_id")


class MaterialDatabase(Mapping[str, MaterialRecord]):
    """Read-only stable-key index with a grades.py-compatible convenience query.

    Direct db[record.key] lookup uses a dictionary, independent of grade scans.
    This slice accepts only null foreign keys: the inspected grade source
    establishes neither grade-to-detail associations nor registry/shape links.
    """

    def __init__(self, records: Iterable[MaterialRecord]) -> None:
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
        self._records = MappingProxyType(indexed)
        self._by_grade = MappingProxyType(by_grade)

    def __getitem__(self, key: str) -> MaterialRecord:
        return self._records[key]

    def __iter__(self) -> Iterator[str]:
        return iter(self._records)

    def __len__(self) -> int:
        return len(self._records)

    def lookup(self, name: str, *, psl: str = "PSL2") -> MaterialRecord:
        """Match grades.smts_mpa selection, including its PSL1 fallback.

        Missing PSL1 data returns the stored PSL2 record, without asserting
        PSL1 product qualification. Other PSL strings select the canonical
        value, exactly as grades.smts_mpa does. Non-API levels remain None.
        """
        grade = grades.get(name)
        level = None
        if grade.standard == "API 5L":
            level = "PSL1" if psl.upper() == "PSL1" and grade.smts_psl1_mpa is not None else "PSL2"
        return self._by_grade[(grade.name, level)]


def _edition(citation: str) -> str | None:
    """Retain only the edition already present in the grade citation."""
    match = re.search(r"\(([^()]*\bed\.)\)", citation)
    return match.group(1) if match else None


def _record(grade: grades.MaterialGrade, level: str | None) -> MaterialRecord:
    return MaterialRecord(
        designation=grade.name,
        specification=grade.standard,
        edition=_edition(grade.source),
        product_level=level,
        strengths=Strengths(
            smys_mpa=grades.smys_mpa(grade.name),
            smts_mpa=grades.smts_mpa(grade.name, psl=level or "PSL2"),
        ),
        provenance=RepositoryProvenance(
            citation=grade.source,
            repository_path=_SOURCE_PATH,
            repository_revision=_SOURCE_REVISION,
            sha256=_SOURCE_SHA256,
            retrieved_on_utc=_RETRIEVED_ON_UTC,
        ),
        notes=grade.notes,
    )


def load_database() -> MaterialDatabase:
    """Build from grades.py reconciliation; reject uninspected source drift.

    A changed grade source requires refreshed provenance after inspection;
    unchanged source bytes remain traceable even on a later repository commit.
    Original standards and reuse terms are not re-verified by this loader.
    """
    digest = sha256(Path(grades.__file__).read_bytes()).hexdigest()
    if digest != _SOURCE_SHA256:
        raise ValueError("grades.py source digest changed; inspect and refresh database provenance")
    records = []
    for grade in grades.all_grades():
        if grade.standard == "API 5L":
            levels = ("PSL1", "PSL2") if grade.smts_psl1_mpa is not None else ("PSL2",)
        else:
            levels = (None,)
        records.extend(_record(grade, level) for level in levels)
    return MaterialDatabase(records)
