"""Indexed projection of the existing reconciled grades, with pinned provenance.

No dimensional values, independent grade matrix or external source fetch is
introduced. Compatibility lookup delegates name handling to grades.get.
The provenance digest hashes the grade module bytes decoded as UTF-8, with
every CRLF and lone CR replaced by LF, then encoded as UTF-8.
"""
from __future__ import annotations

import re
from collections.abc import Iterable, Iterator, Mapping
from hashlib import sha256
from pathlib import Path
from types import MappingProxyType

from . import grades
from .database_schema import MaterialRecord, RepositoryProvenance, Strengths

# Inspected source text, not a standards-derived numerical dataset.
_SOURCE_REVISION = "8036237f20d2bc62e4e8dd3178fc2036b9ed4f40"
# SHA-256 of UTF-8-decoded grade text, CRLF and lone CR replaced by LF,
# then encoded as UTF-8.
_SOURCE_SHA256 = "184ea635cbed6ff811500e79114db4df3ffaa6fb275f799a7b40084f2663553f"
_RETRIEVED_ON_UTC = "2026-10-18"
_SOURCE_PATH = "src/digitalmodel/materials/grades.py"
_LINK_FIELDS = ("sn_curve_ids", "standards_registry_id", "structural_shape_id")


class MaterialDatabase(Mapping[str, MaterialRecord]):
    """Read-only stable-key index with a grades.py-compatible convenience query.

    Direct db[record.key] lookup uses a dictionary, independent of grade scans.
    This slice accepts only null foreign keys: the inspected grade source
    establishes neither grade-to-detail associations nor registry/shape links.
    """

    def __init__(
        self, records: Iterable[MaterialRecord], *, source_digest_matches: bool = False
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
        self._source_digest_matches = source_digest_matches
        self._records = MappingProxyType(indexed)
        self._by_grade = MappingProxyType(by_grade)

    @property
    def source_digest_matches(self) -> bool:
        """Whether the loaded source matched the inspected normalised digest.

        False on a loader result means the grade source changed since inspection
        and the records carry stale pinned provenance. Directly constructed
        indexes default to False because no source comparison is established.
        """
        return self._source_digest_matches

    def __getitem__(self, key: str) -> MaterialRecord:
        return self._records[key]

    def __iter__(self) -> Iterator[str]:
        return iter(self._records)

    def __len__(self) -> int:
        return len(self._records)

    def lookup(self, name: str, *, psl: str = "PSL2") -> MaterialRecord:
        """Match grades.smts_mpa, rejecting unsupported API product levels."""
        grade = grades.get(name)
        level = None
        if grade.standard == "API 5L":
            grades.smts_mpa(grade.name, psl=psl)  # Validate against the cited table.
            level = psl.upper()
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


def normalised_source_digest(source: bytes) -> str:
    """Hash UTF-8-decoded text after replacing CRLF and lone CR with LF."""
    text = source.decode("utf-8").replace("\r\n", "\n").replace("\r", "\n")
    return sha256(text.encode("utf-8")).hexdigest()


def load_database() -> MaterialDatabase:
    """Build from grades.py reconciliation, including when its digest drifts.

    A False source_digest_matches flag means the grade source changed since
    inspection and the records carry stale pinned provenance. Drift is reported
    by the flag and a test, without a loader error. Original standards and reuse
    terms are not re-verified by this loader.
    """
    digest = normalised_source_digest(Path(grades.__file__).read_bytes())
    records = []
    for grade in grades.all_grades():
        if grade.standard == "API 5L":
            levels = ("PSL1", "PSL2") if grade.smts_psl1_mpa is not None else ("PSL2",)
        else:
            levels = (None,)
        records.extend(_record(grade, level) for level in levels)
    return MaterialDatabase(records, source_digest_matches=digest == _SOURCE_SHA256)
