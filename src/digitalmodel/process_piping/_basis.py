"""Packaged public-reference records, validated by the existing citation schema.

These records contain source locators and scope, never standards text/tables.
Calculation-time resolution is bounded to this package, including in a wheel.
"""

from pathlib import Path
import math

from digitalmodel.citations.schema import Citation, validate_citation

_BASIS_ROOT = Path(__file__).parent / "basis"
_RECORDS = {
    "pressure": (
        "asme-b31-3",
        "ASME",
        "2024",
        "304.1.1; 304.1.2(a), eq. (3a); 304.1.2(b)",
    ),
    "hydro": ("asme-b31-3", "ASME", "2024", "345.4.2, eq. (24); 345.2.1(a)"),
    "bolting": (
        "asme-pcc-1",
        "ASME",
        "2019",
        "Sections 8, 11, 12; Appendices O and P; separate BPVC VIII-1 Appendix 2 load screen",
    ),
    "hydraulics": (
        "pipe-flow",
        "Public engineering references",
        "2026-11-02",
        "Continuity; Darcy-Weisbach; Swamee-Jain 1976; integrated isothermal momentum",
    ),
    "velocity": (
        "ashrae-fundamentals",
        "ASHRAE",
        "2017",
        "Chapter 22, Table 22, selected water-service upper bounds",
    ),
    "erosion": ("api-rp-14e", "API", "1991", "Fifth edition, 2.5(a), eq. (2.14)"),
}


def citations(*keys: str) -> tuple[Citation, ...]:
    """Resolve every requested source record or fail before calculation."""
    result = []
    for key in keys:
        code, publisher, revision, section = _RECORDS[key]
        citation = Citation(
            code_id=code,
            publisher=publisher,
            revision=revision,
            section=section,
            wiki_path=f"wikis/{code}.md",
            source_sibling="generic",
            note="Public-reference locator only; no reproduced standard tables.",
        )
        validate_citation(citation, repo_root=_BASIS_ROOT)
        result.append(citation)
    return tuple(result)


def positive(**values: float) -> None:
    """Reject missing, Boolean, nonnumeric, nonfinite and nonpositive inputs."""
    for name, value in values.items():
        if (
            isinstance(value, bool)
            or not isinstance(value, (int, float))
            or not math.isfinite(value)
            or value <= 0
        ):
            raise ValueError(f"{name} must be a finite positive number")


def nonnegative(**values: float) -> None:
    """Validate finite nonnegative scalars."""
    for name, value in values.items():
        if (
            isinstance(value, bool)
            or not isinstance(value, (int, float))
            or not math.isfinite(value)
            or value < 0
        ):
            raise ValueError(f"{name} must be a finite nonnegative number")
