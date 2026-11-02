"""Public-source citation sidecars and bounded input checks.

Source records are shipped with this package. Resolution uses this package's
explicit directory, never a sibling repository or a machine-specific wiki.
"""

from __future__ import annotations

import math
from pathlib import Path

from digitalmodel.citations import Citation, validate_citation


def citation(source: str) -> Citation:
    """Resolve a public basis record; missing or mismatched metadata fails closed."""
    records = {
        "norsok": ("D010-PUBLIC-SEMINAR", "Offshore Norge", "2013",
                   "slides 8-9, 12-14; D-010 rev.4 chapter 9 and EAC table 24",
                   "https://www.offshorenorge.no/globalassets/dokumenter/drift/"
                   "presentasjonerarrangementer/plug--abandonment-seminar-2013/"
                   "3---norsok--garry-brewster.pdf"),
        "pds": ("PDS-PUBLIC-EXCERPT", "SINTEF", "2021",
                "section 4.4.1, printed page 99",
                "https://pds-forum.com/wp-content/uploads/2023/01/"
                "PDS-Data-Handbook-2021-example-pages-reduced-2023-01-26.pdf"),
        "nureg": ("NUREG-0492", "US NRC", "1981",
                  "chapter VII; chapter XI pages 12-13 and equation XI-24",
                  "https://www.nrc.gov/regulations-legislation/nureg-series-"
                  "publications/publications-prepared-by-nrc-staff/sr0492"),
    }
    code, publisher, revision, section, url = records[source]
    item = Citation(code, publisher, revision, section,
                    f"wikis/well_integrity/{source}.md", note=url,
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
