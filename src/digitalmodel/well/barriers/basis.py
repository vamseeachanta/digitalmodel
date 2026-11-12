"""Reference citation sidecars and bounded input checks.

Source records are shipped with this package. Resolution uses this package's
explicit directory, never a sibling repository or a machine-specific wiki.
"""

from __future__ import annotations

import math
from pathlib import Path

from digitalmodel.citations import Citation, validate_citation


def citation(source: str) -> Citation:
    """Resolve a basis record; missing or mismatched metadata fails closed."""
    records = {
        "norsok": ("NORSOK-D010", "Standards Norway", "4 (2013)",
                   "Well barrier element acceptance criteria: Cement plug",
                   "NORSOK D-010, Well integrity in drilling and well operations"),
        "pds": ("PDS-DATA-HANDBOOK", "SINTEF", "2021",
                "section 4.4.1", "PDS Data Handbook, 2021 edition"),
        "nureg": ("NUREG-0492", "Nuclear Regulatory Commission", "1981",
                  "chapter VII; chapter XI pages 12-13 and equation XI-24",
                  "https://www.nrc.gov/regulations-legislation/nureg-series-"
                  "publications/publications-prepared-by-nrc-staff/sr0492"),
    }
    code, publisher, revision, section, note = records[source]
    item = Citation(code, publisher, revision, section,
                    f"wikis/well_integrity/{source}.md", note=note,
                    source_sibling="generic")
    validate_citation(item, repo_root=Path(__file__).with_name("basis"))
    return item


def number(name: str, value: float, *, positive: bool = False) -> None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite number")
    if not math.isfinite(value) or value < 0 or (positive and value == 0):
        raise ValueError(f"{name} must be finite and {'positive' if positive else 'nonnegative'}")


def text(name: str, value: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} must be a nonempty string")


def boolean(name: str, value: bool) -> None:
    if type(value) is not bool:
        raise ValueError(f"{name} must be boolean")


def positive_integer(name: str, value: int) -> None:
    if type(value) is not int or value <= 0:
        raise ValueError(f"{name} must be a positive integer")
