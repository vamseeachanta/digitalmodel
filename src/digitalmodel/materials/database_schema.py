"""Validated, immutable materials metadata; no independent strength table.

The frozen-dataclass convention follows materials.grades and materials.line_pipe.
Unknown editions and unestablished foreign keys are represented by None.
"""
from __future__ import annotations

import math
import re
from dataclasses import dataclass
from datetime import date
from numbers import Real


def _text(value: object, field: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{field} must be a non-empty string")


@dataclass(frozen=True)
class Strengths:
    """Specified minima in the canonical grade module's MPa convention."""

    smys_mpa: float
    smts_mpa: float
    units: str = "MPa"

    def __post_init__(self) -> None:
        for field in ("smys_mpa", "smts_mpa"):
            value = getattr(self, field)
            if (isinstance(value, bool) or not isinstance(value, Real)
                    or not math.isfinite(value) or value <= 0):
                raise ValueError(f"{field} must be a finite positive number")
        if self.smts_mpa < self.smys_mpa:
            raise ValueError("smts_mpa must be at least smys_mpa")
        if self.units != "MPa":
            raise ValueError("strength units must be MPa, as in grades.py")


@dataclass(frozen=True)
class RepositoryProvenance:
    """Traceability to inspected repository bytes, without original verification.

    The repository licence describes software reuse. It does not establish
    redistribution rights for the original specification or qualify its values.
    """

    citation: str
    repository_path: str
    repository_revision: str
    sha256: str
    retrieved_on_utc: str
    repository_terms: str = "MIT License"
    repository_terms_locator: str = "LICENSE"
    original_source_terms: str = "not re-verified in this job"

    def __post_init__(self) -> None:
        for field in self.__dataclass_fields__:
            _text(getattr(self, field), field)
        if not re.fullmatch(r"[a-f0-9]{40}", self.repository_revision):
            raise ValueError("repository_revision must be a full Git commit hash")
        if not re.fullmatch(r"[a-f0-9]{64}", self.sha256):
            raise ValueError("sha256 must be a lowercase SHA-256 digest")
        try:
            date.fromisoformat(self.retrieved_on_utc)
        except ValueError as exc:
            raise ValueError("retrieved_on_utc must be an ISO calendar date") from exc
        if (self.repository_path.startswith(("/", "\\"))
                or "\\" in self.repository_path
                or any(p in ("..", "", ".") for p in self.repository_path.split("/"))):
            raise ValueError("repository_path must be a repository-relative locator")


@dataclass(frozen=True)
class MaterialRecord:
    """A reconciled grade and stored product level, with explicit nullable links.

    sn_curve_ids targets fatigue.sn_library.SNCurveRecord.curve_id.
    standards_registry_id targets the future standards-registry crosswalk.
    structural_shape_id names the structural-shapes catalogue target; linkage
    remains unresolved in this slice. See database_gaps.html.
    """

    designation: str
    specification: str
    edition: str | None
    product_level: str | None
    strengths: Strengths
    provenance: RepositoryProvenance
    sn_curve_ids: tuple[str, ...] | None = None
    standards_registry_id: str | None = None
    structural_shape_id: str | None = None
    notes: str = ""

    def __post_init__(self) -> None:
        for field in ("designation", "specification", "edition"):
            value = getattr(self, field)
            if field == "edition" and value is None:
                continue
            _text(value, field)
            if "|" in value or value != value.strip():
                raise ValueError(f"{field} must be trimmed and contain no key separator")
        levels = ("PSL1", "PSL2") if self.specification == "API 5L" else (None,)
        if self.product_level not in levels:
            raise ValueError(f"product_level must be one of {levels} for {self.specification}")
        if not isinstance(self.strengths, Strengths):
            raise ValueError("strengths must be a validated Strengths record")
        if not isinstance(self.provenance, RepositoryProvenance):
            raise ValueError("provenance must be a RepositoryProvenance record")
        if not isinstance(self.notes, str):
            raise ValueError("notes must be a string")

    @property
    def key(self) -> str:
        """Stable identity; edition-unspecified is a key token, not an edition."""
        return "|".join((
            self.specification, self.edition or "edition-unspecified",
            self.designation, self.product_level or "level-not-applicable",
        ))
