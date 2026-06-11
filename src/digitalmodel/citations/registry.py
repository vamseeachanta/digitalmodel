"""Registry of citation-emitting getters for standards-derived constants.

Pilot scope (#2481 D1): mooring design factors from DNV-OS-E301 and OCIMF-MEG4.

Each getter returns a CitedValue whose citation is validated before return
(fail-closed per #2481 D2).
"""
from __future__ import annotations

from enum import Enum
from pathlib import Path
from typing import Final, Optional

from digitalmodel.citations.schema import (
    Citation,
    CitedValue,
    validate_citation,
)


class MooringCondition(str, Enum):
    INTACT_QUASI_STATIC = "intact-quasi-static"
    DAMAGED_QUASI_STATIC = "damaged-quasi-static"


_DNV_OS_E301_CITATION_TEMPLATE: Final = {
    "code_id": "DNV-OS-E301",
    "publisher": "DNV",
    "revision": "2021-07",
    "wiki_path": "wikis/engineering/wiki/standards/dnv-os-e301.md",
}

_DNV_RP_F103_CITATION_TEMPLATE: Final = {
    "code_id": "dnv-rp-f103",
    "publisher": "DNV",
    "revision": "2010",
    "wiki_path": "wikis/engineering-standards/wiki/standards/dnv-rp-f103.md",
}

# DNV-OS-E301 Section 2.2 design load factors for mooring systems.
# Values are industry-standard and also appear in API RP 2SK Table C-1.
_MOORING_SAFETY_FACTORS: Final[dict[MooringCondition, tuple[float, str]]] = {
    MooringCondition.INTACT_QUASI_STATIC: (1.67, "Section 2.2.3 (intact, quasi-static)"),
    MooringCondition.DAMAGED_QUASI_STATIC: (1.25, "Section 2.2.3 (damaged, quasi-static)"),
}


def get_mooring_safety_factor(
    condition: MooringCondition, *, repo_root: Optional[Path] = None
) -> CitedValue:
    """Return the DNV-OS-E301 mooring safety factor for the given condition.

    Fail-closed: raises CitationResolutionError if the cited wiki page is missing
    or its frontmatter no longer matches the citation.

    When repo_root is None, defers to the resolver (LLM_WIKI_PATH precedence chain).
    """
    if condition not in _MOORING_SAFETY_FACTORS:
        raise ValueError(f"Unknown mooring condition: {condition!r}")
    value, section = _MOORING_SAFETY_FACTORS[condition]
    citation = Citation(
        section=section,
        note=f"Design factor for {condition.value} condition",
        **_DNV_OS_E301_CITATION_TEMPLATE,
    )
    validate_citation(citation, repo_root=repo_root)
    return CitedValue(value=value, citation=citation, units="dimensionless")


def get_dnv_f103_reference(
    section: str, *, note: str = "", repo_root: Optional[Path] = None
) -> CitedValue:
    """Return a validated DNV-RP-F103 reference citation.

    Use for F103 calculations whose numeric values come from multiple standard
    locations instead of one scalar constant. The value is a sentinel; the
    citation is the payload.
    """
    citation = Citation(
        section=section,
        note=note,
        **_DNV_RP_F103_CITATION_TEMPLATE,
    )
    validate_citation(citation, repo_root=repo_root)
    return CitedValue(value=1.0, citation=citation, units="reference")
